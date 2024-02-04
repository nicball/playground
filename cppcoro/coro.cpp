#include <exception>
#include <memory>
#include <coroutine>
#include <functional>
#include <optional>
#include <cassert>
#include <iostream>
#include <queue>

class scheduler;

template <class T, class Scheduler = scheduler>
class coro {

public:

  class promise_type {

  public:

    template <class... Args>
    promise_type(Scheduler s, Args&&...): sched{s} {}

    promise_type() = default;

    coro<T, Scheduler> get_return_object() { return { std::coroutine_handle<promise_type>::from_promise(*this) }; }

    void return_value(T value) { this->result = std::move(value); }

    std::suspend_always initial_suspend() { return {}; }

    auto final_suspend() noexcept {

      struct final_awaiter {
        bool await_ready() noexcept { return false; }
        void await_suspend(std::coroutine_handle<promise_type> handle) noexcept {
          if (this->p->continuation) {
            this->p->sched.wake(this->p->continuation);
          }
        }
        void await_resume() noexcept {}
        promise_type* p;
      };

      return final_awaiter { this };
    }

    void unhandled_exception() { this->ex = std::current_exception(); }

  private:

    friend struct coro::coro_awaiter;
    template <class> friend class suspend;
    std::optional<T> result = std::nullopt;
    std::coroutine_handle<promise_type> continuation = {};
    std::exception_ptr ex = {};
    Scheduler sched = {};

  };

  ~coro() {
    // TODO: coroutine life cycle
  }

  auto operator co_await() {
    return coro_awaiter { this };
  }

private:

  friend class scheduler;

  struct coro_awaiter {

    coro* subcoro;

    bool await_ready() { return false; }

    bool await_suspend(std::coroutine_handle<promise_type> k) {
      auto hdl = this->subcoro->handle;
      hdl.resume();
      if (hdl.done()) {
        return false;
      }
      else {
        hdl.promise().continuation = k;
        return true;
      }
    }

    T await_resume() {
      auto& p = this->subcoro->handle.promise();
      if (p.ex) std::rethrow_exception(p.ex);
      assert(p.result.has_value());
      return std::move(*p.result);
    }

  };

  coro(std::coroutine_handle<promise_type> h): handle{h} {}

  std::coroutine_handle<promise_type> handle;

};

template <class> class promise;

template <class T>
class suspend {

public:

  template <class F>
  suspend(F&& f): register_promise{std::forward<F>(f)} {}

  bool await_ready() { return false; }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    this->register_promise(promise<T>{this, [handle]() { handle.promise().sched.wake(handle); }});
  }

  T await_resume() {
    if (this->ex) std::rethrow_exception(this->ex);
    assert(this->result);
    return std::move(*this->result);
  }

private:

  template <class U>
  friend class promise;
  std::function<void(promise<T>)> register_promise;
  std::optional<T> result = std::nullopt;
  std::exception_ptr ex = {};

};

template <class T>
class promise {

public:

  template <class F>
  promise(suspend<T>* s, F&& f): susp{s}, wake{std::forward<F>(f)} {}

  void fulfill(T value) { 
    this->susp->result = value;
    this->wake();
  }

  template <class U>
  void reject(U error) {
    try { throw error; }
    catch (...) { this->susp->ex = std::current_exception(); }
    this->wake();
  }

private:

  suspend<T>* susp;
  std::function<void()> wake;

};

class scheduler {

public:

  void wake(std::coroutine_handle<> h) {
    this->work_queue->push(h);
  }

  template <class T, class S>
  void wake(coro<T, S> c) {
    this->wake(c.handle);
  }

  void run() {
    auto& q = *this->work_queue;
    while (!q.empty()) {
      q.front().resume();
      q.pop();
    }
  }

private:

  std::shared_ptr<std::queue<std::coroutine_handle<>>> work_queue = std::make_shared<std::queue<std::coroutine_handle<>>>();

};

coro<int> complete_sync(scheduler s) {
  co_return 1;
}

coro<int> f(scheduler s) {
  co_await complete_sync(s);
  int i = co_await suspend<int>([s](promise<int> k) mutable -> void {
    s.wake(([](promise<int> k) -> coro<int> {
      std::cout << "sending" << std::endl;
      k.fulfill(5);
      co_return 0;
    })(k));
  });
  std::cout << i << std::endl;
  co_return 1;
}

int main() {
  scheduler s;
  s.wake(f(s));
  s.run();
  return 0;
}
