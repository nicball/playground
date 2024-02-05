#include <exception>
#include <memory>
#include <coroutine>
#include <functional>
#include <optional>
#include <cassert>
#include <iostream>
#include <queue>

class scheduler;

template <class T>
class coro {

private:

  class coro_promise {

  private:

    friend struct coro::coro_awaiter;
    std::optional<T> result = std::nullopt;
    std::coroutine_handle<coro_promise> continuation = {};
    std::exception_ptr ex = {};

  public:

    coro_promise() = default;

    coro get_return_object() { return { std::coroutine_handle<coro_promise>::from_promise(*this) }; }

    void return_value(T value) { this->result = std::move(value); }

    std::suspend_always initial_suspend() { return {}; }

    auto final_suspend() noexcept {

      struct final_awaiter {
        bool await_ready() noexcept { return false; }
        std::coroutine_handle<> await_suspend(std::coroutine_handle<coro_promise> handle) noexcept {
          if (this->p->continuation)
            return this->p->continuation;
          else
            return std::noop_coroutine();
        }
        void await_resume() noexcept {}
        coro_promise* p;
      };

      return final_awaiter { this };
    }

    void unhandled_exception() { this->ex = std::current_exception(); }

  };

  class coro_awaiter {

  private:

    coro* subcoro;

  public:

    coro_awaiter(coro* s): subcoro{s} {}

    bool await_ready() { return false; }

    std::coroutine_handle<> await_suspend(std::coroutine_handle<coro_promise> k) {
      auto hdl = this->subcoro->handle;
      hdl.promise().continuation = k;
      return hdl;
    }

    T await_resume() {
      auto& p = this->subcoro->handle.promise();
      if (p.ex) std::rethrow_exception(p.ex);
      assert(p.result.has_value());
      return std::move(*p.result);
    }

  };

  coro(std::coroutine_handle<coro_promise> h): handle{h} {}

  std::coroutine_handle<coro_promise> handle;

public:

  using promise_type = coro_promise;

  ~coro() {
    // TODO: coroutine life cycle
  }

  coro_awaiter operator co_await() {
    return {this};
  }

  std::coroutine_handle<> get_handle() { return this->handle; }

};

template <class> class promise;

template <class T>
class suspend {

private:

  friend class promise<T>;
  std::function<void(promise<T>)> register_promise;
  std::optional<T> result = std::nullopt;
  std::exception_ptr ex = {};
  std::function<void(std::coroutine_handle<>)> wake;

public:

  template <class Scheduler, class F>
  suspend(Scheduler s, F&& f): wake{[s](std::coroutine_handle<> h) { s.wake(h); }}, register_promise{std::forward<F>(f)} {}

  bool await_ready() { return false; }

  void await_suspend(std::coroutine_handle<> handle) {
    this->register_promise(promise<T>{this, handle});
  }

  T await_resume() {
    if (this->ex) std::rethrow_exception(this->ex);
    assert(this->result);
    return std::move(*this->result);
  }

};

template <class T>
class promise {

private:

  suspend<T>* susp;
  std::coroutine_handle<> handle;

public:

  promise(suspend<T>* s, std::coroutine_handle<> h): susp{s}, handle{h} {}

  void fulfill(T value) { 
    this->susp->result = value;
    this->susp->wake(this->handle);
  }

  template <class U>
  void reject(U error) {
    try { throw error; }
    catch (...) { this->susp->ex = std::current_exception(); }
    this->susp->wake(this->handle);
  }

};

class scheduler {

private:

  std::shared_ptr<std::queue<std::coroutine_handle<>>> work_queue = std::make_shared<std::queue<std::coroutine_handle<>>>();

public:

  void wake(std::coroutine_handle<> h) const {
    this->work_queue->push(h);
  }

  template <class T>
  void wake(coro<T> c) const {
    this->wake(c.get_handle());
  }

  void run() const {
    auto& q = *this->work_queue;
    while (!q.empty()) {
      q.front().resume();
      q.pop();
    }
  }

};

coro<int> complete_sync() {
  co_return 1;
}

coro<int> f(scheduler s) {
  co_await complete_sync();
  int i = co_await suspend<int>(s, [s](promise<int> k) -> void {
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
