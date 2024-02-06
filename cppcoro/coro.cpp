#include <exception>
#include <memory>
#include <coroutine>
#include <functional>
#include <optional>
#include <cassert>
#include <iostream>
#include <queue>
#include <vector>
#include <deque>

class scheduler;

template <class T>
class coro {

private:

  class coro_promise {

  private:

    friend class coro;
    friend struct coro::coro_awaiter;
    std::optional<T> result = std::nullopt;
    std::coroutine_handle<coro_promise> continuation = {};
    std::exception_ptr ex = {};
    scheduler* sched = nullptr;

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

    scheduler* get_scheduler() const { assert(this->sched); return this->sched; }

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

  void set_scheduler(scheduler* s) { this->handle.promise().sched = s; }

};

class scheduler {

private:

  std::shared_ptr<std::queue<std::coroutine_handle<>>> work_queue = std::make_shared<std::queue<std::coroutine_handle<>>>();

public:

  void wake(std::coroutine_handle<> h) {
    this->work_queue->push(h);
  }

  template <class T>
  void wake(coro<T> c) {
    c.set_scheduler(this);
    this->wake(c.get_handle());
  }

  void run() {
    auto& q = *this->work_queue;
    while (!q.empty()) {
      q.front().resume();
      q.pop();
    }
  }

};

template <class> class promise;

template <class T>
class suspend {

private:

  friend class promise<T>;
  std::function<void(scheduler*, promise<T>)> register_promise;
  std::optional<T> result = std::nullopt;
  std::exception_ptr ex = {};

public:

  template <class F>
  suspend(F&& f): register_promise{std::forward<F>(f)} {}

  bool await_ready() { return false; }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    scheduler* s = handle.promise().get_scheduler();
    this->register_promise(s, promise<T>{this, handle, s});
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
  scheduler* sched;

public:

  promise(suspend<T>* s, std::coroutine_handle<> h, scheduler* sc): susp{s}, handle{h}, sched{sc} {}

  void fulfill(T value) { 
    this->susp->result = value;
    this->sched->wake(this->handle);
  }

  template <class U>
  void reject(U error) {
    try { throw error; }
    catch (...) { this->susp->ex = std::current_exception(); }
    this->sched->wake(this->handle);
  }

};

coro<int> complete_sync() {
  co_return 1;
}

coro<int> f() {
  co_await complete_sync();
  int i = co_await suspend<int>([](scheduler* s, promise<int> k) -> void {
    s->wake(([](promise<int> k) -> coro<int> {
      std::cout << "sending" << std::endl;
      k.fulfill(5);
      co_return 0;
    })(k));
  });
  std::cout << i << std::endl;
  co_return 1;
}

template <class T>
class broadcast_state {

private:

  std::vector<T> buffer;
  int write_end = 0;
  std::deque<std::coroutine_handle<>> wait_list = {};
  scheduler* sched;

public:

  broadcast_state(scheduler* s, int size): sched{s}, buffer(size) {}

  int append(T value) {
    int index = this->write_end++;
    this->buffer[index % (int)this->buffer.size()] = std::move(value);
    std::deque<std::coroutine_handle<>> wl = {};
    std::swap(wl, this->wait_list);
    for (auto h : wl) this->sched->wake(h);
    return index;
  }

  T read(int index) {
    return this->buffer[index % (int)this->buffer.size()];
  }

  std::optional<int> next_available(int index) {
    if (this->write_end == 0 || this->write_end == index) return std::nullopt;
    assert(0 <= index && index < this->write_end);
    int oldest = std::max(0, this->write_end - (int)this->buffer.size() + 1);
    return std::max(oldest, index);
  }

  void wait(std::coroutine_handle<> handle) {
    this->wait_list.push_back(handle);
  }
};

template <class T>
class broadcast {

private:

  struct broadcast_awaiter {

    broadcast* b;

    bool await_ready() {
      return static_cast<bool>(this->b->state->next_available(this->b->read_end));
    }

    void await_suspend(std::coroutine_handle<> handle) {
      this->b->state->wait(handle);
    }

    T await_resume() {
      this->b->read_end = *this->b->state->next_available(this->b->read_end);
      return this->b->state->read(this->b->read_end++);
    }

  };

  std::shared_ptr<broadcast_state<T>> state;
  int read_end = 0;

public:

  broadcast(scheduler* s, int size): state{std::make_shared<broadcast_state<T>>(s, size)} {}

  broadcast_awaiter recv() {
    return {this};
  }

  void send(T value) {
    this->state->append(std::move(value));
  }

};

struct unit {};

coro<unit> broadcaster(broadcast<int> b) {
  for (int i = 0; i < 10; ++i) {
    b.send(i);
    std::cout << "sent " << i << std::endl;
  }
  co_return {};
};

coro<unit> audience(broadcast<int> b) {
  std::cout << "started receiving" << std::endl;
  for (;;) std::cout << "got " << co_await b.recv() << std::endl;
  co_return {};
}

int main() {
  scheduler s;
  s.wake(f());
  broadcast<int> b{&s, 5};
  s.wake(broadcaster(b));
  s.wake(audience(b));
  s.wake(audience(b));
  s.run();
  return 0;
}
