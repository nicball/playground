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
#include <set>

struct unit {};

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
      return *p.result;
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

struct wakable {
  std::coroutine_handle<> handle;
  scheduler* sched;
  wakable(std::coroutine_handle<> h, scheduler* s): handle{h}, sched{s} {}
  void wake() { this->sched->wake(this->handle); }
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
    this->register_promise(s, promise<T>{this, {handle, s}});
  }

  T await_resume() {
    if (this->ex) std::rethrow_exception(this->ex);
    assert(this->result);
    return *this->result;
  }

};

template <class T>
class promise {

private:

  suspend<T>* susp;
  wakable handle;

public:

  promise(suspend<T>* s, wakable w): susp{s}, handle{w} {}

  void fulfill(T value) { 
    this->susp->result = value;
    this->handle.wake();
  }

  template <class U>
  void reject(U error) {
    try { throw error; }
    catch (...) { this->susp->ex = std::current_exception(); }
    this->handle.wake();
  }

};

template <class T>
class channel {

private:
  std::vector<T> buffer = {};
  std::size_t size;
  std::size_t read_end = 0;
  std::size_t write_end = 0;
  std::deque<wakable> writers = {};
  std::deque<wakable> readers = {};

  class write_awaiter {
  private:
    channel* chan;
    T value;
  public:
    write_awaiter(channel* c, T v): chan{c}, value{std::move(v)} {}
    bool await_ready() {
      return this->chan->write_end - this->chan->read_end < this->chan->size;
    }
    template <class P>
    void await_suspend(std::coroutine_handle<P> handle) {
      this->chan->writers.push_back({handle, handle.promise().get_scheduler()});
    }
    void await_resume() {
      std::size_t index = this->chan->write_end % this->chan->size;
      assert(index <= this->chan->buffer.size());
      if (index == this->chan->buffer.size())
        this->chan->buffer.push_back(std::move(this->value));
      else
        this->chan->buffer[index] = std::move(this->value);
      if (this->chan->write_end == this->chan->read_end && !this->chan->readers.empty()) {
        this->chan->readers.front().wake();
        this->chan->readers.pop_front();
      }
      ++this->chan->write_end;
    }
  };

  class read_awaiter {
  private:
    channel* chan;
  public:
    read_awaiter(channel* c): chan{c} {}
    bool await_ready() {
      return this->chan->write_end - this->chan->read_end > 0;
    }
    template <class P>
    void await_suspend(std::coroutine_handle<P> handle) {
      this->chan->readers.push_back({handle, handle.promise().get_scheduler()});
    }
    T await_resume() {
      T res = std::move(this->chan->buffer[this->chan->read_end % this->chan->size]);
      if (this->chan->write_end - this->chan->read_end == this->chan->size && !this->chan->writers.empty()) {
        this->chan->writers.front().wake();
        this->chan->writers.pop_front();
      }
      ++this->chan->read_end;
      return res;
    }
  };

public:

  channel(std::size_t s): size{s} {
    this->buffer.reserve(size);
  }

  write_awaiter write(T value) {
    return {this, std::move(value)};
  }

  read_awaiter read() {
    return {this};
  }

};

template <class T>
class broadcast {

private:

  channel<T>* input;
  std::set<channel<T>*> receivers;

public:

  class receiver {
  private:
    std::shared_ptr<channel<T>> chan;
  public:
    receiver(std::shared_ptr<channel<T>> c): chan{std::move(c)} {}
    auto read() { return this->chan->read(); }
  };

  broadcast(channel<T>* i): input{i} {}

  coro<unit> run() {
    for (;;) {
      T v = co_await this->input->read();
      for (auto r : this->receivers) co_await r->write(v);
    }
  }

  receiver get_receiver(std::size_t size) {
    std::shared_ptr<channel<T>> c{new channel<T>(size), [this](auto p) {
      this->receivers.erase(p);
      delete p;
    }};
    this->receivers.insert(c.get());
    return {c};
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

coro<unit> broadcaster(channel<int>* p) {
  for (int i = 0; i < 10; ++i) {
    co_await p->write(i);
    std::cout << "sent " << i << std::endl;
  }
  co_return {};
};

coro<unit> audience(int id, broadcast<int>::receiver s) {
  std::cout << "started receiving" << std::endl;
  for (;;) std::cout << id  << " got " << co_await s.read() << std::endl;
  co_return {};
}

int main() {
  scheduler s;
  // s.wake(f());
  channel<int> c{5};
  broadcast<int> b{&c};
  s.wake(b.run());
  s.wake(broadcaster(&c));
  s.wake(audience(1, b.get_receiver(2)));
  s.wake(audience(2, b.get_receiver(2)));
  s.run();
  return 0;
}
