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

  class coro_awaiter;

  class coro_promise {

  private:

    friend class coro;
    friend class coro::coro_awaiter;
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
          std::coroutine_handle<> res;
          if (this->p->continuation)
            res = this->p->continuation;
          else
            res = std::noop_coroutine();
          handle.destroy();
          return res;
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

class channel_half_closed_exception : public std::exception {
public:
  const char* what() const noexcept override {
    return "co_awaiting a channel that's half closed";
  }
};

template <class T>
class channel {

private:

  class write_awaiter;
  class read_awaiter;

  struct write_awaiter_handle {
    wakable coro;
    write_awaiter* awaiter;
  };

  struct read_awaiter_handle {
    wakable coro;
    read_awaiter* awaiter;
  };

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
      std::cerr << "suspended while writing " << this->chan << std::endl;
      this->chan->writers.push_back({{handle, handle.promise().get_scheduler()}, this});
    }
    void await_resume() {
      std::cerr << "resumed from writing " << this->chan << std::endl;
      if (this->chan->reader_count == 0) throw channel_half_closed_exception{};
      std::size_t index = this->chan->write_end % this->chan->size;
      assert(index <= this->chan->buffer.size());
      if (index == this->chan->buffer.size())
        this->chan->buffer.push_back(std::move(this->value));
      else
        this->chan->buffer[index] = std::move(this->value);
      if (!this->chan->readers.empty()) {
        this->chan->readers.front().coro.wake();
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
      this->chan->readers.push_back({{handle, handle.promise().get_scheduler()}, this});
    }
    T await_resume() {
      std::cerr << "resumed from reading " << this->chan << std::endl;
      if (this->chan->writer_count == 0) throw channel_half_closed_exception{};
      T res = std::move(this->chan->buffer[this->chan->read_end % this->chan->size]);
      if (!this->chan->writers.empty()) {
        this->chan->writers.front().coro.wake();
        this->chan->writers.pop_front();
      }
      ++this->chan->read_end;
      return res;
    }
  };

  std::vector<T> buffer = {};
  std::size_t size;
  std::size_t read_end = 0;
  std::size_t write_end = 0;
  std::deque<write_awaiter_handle> writers = {};
  std::deque<read_awaiter_handle> readers = {};
  std::size_t reader_count = 0;
  std::size_t writer_count = 0;

public:

  class reader {
  private:
    channel* chan;
  public:
    reader(channel* c): chan{c} {
      ++this->chan->reader_count;
    }
    ~reader() {
      --this->chan->reader_count;
    }
    reader(const reader& other): reader{other.chan} {}
    reader& operator=(const reader& other) = delete;
    read_awaiter read() const { return {this->chan}; }
    bool operator<(const reader& o) const { return this->chan < o.chan; }
    channel* get_channel() const { return this->chan; }
  };

  class writer {
  private:
    channel* chan;
  public:
    writer(channel* c): chan{c} {
      ++this->chan->writer_count;
    }
    ~writer() {
      --this->chan->writer_count;
    }
    writer(const writer& other): writer{other.chan} {}
    writer& operator=(const writer& other) = delete;
    write_awaiter write(T value) const { return {this->chan, std::move(value)}; }
    bool operator<(const writer& o) const { return this->chan < o.chan; }
    channel* get_channel() const { return this->chan; }
  };

  channel(std::size_t s): size{s} {
    this->buffer.reserve(this->size);
  }

  // ~channel() {
  //   assert(this->readers.size() == 0);
  //   assert(this->writers.size() == 0);
  // }

  reader get_reader() { return {this}; }

  writer get_writer() { return {this}; }

};

template <class T>
class broadcast {

private:

  channel<T>::reader input;
  std::set<typename channel<T>::writer> receivers;

public:

  broadcast(typename channel<T>::reader i): input{i} {}

  coro<unit> run() {
    for (;;) {
      T v = co_await this->input.read();
      for (auto i = this->receivers.begin(); i != this->receivers.end(); ) {
        auto& r = *i;
        auto curr = i++;
        try {
          std::cerr << "broadcasting " << v << " to " << r.get_channel() << std::endl;
          co_await r.write(v);
        }
        catch (channel_half_closed_exception) {
          std::cerr << "broadcast: forgetting " << r.get_channel() << std::endl;
          this->receivers.erase(curr);
        }
      }
    }
  }

  void subscribe(channel<T>::writer w) {
    receivers.insert(w);
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

coro<unit> broadcaster(typename channel<int>::writer p) {
  for (int i = 0; i < 10; ++i) {
    co_await p.write(i);
    std::cout << "sent " << i << std::endl;
  }
  co_return {};
};

coro<unit> audience(int id, typename channel<int>::reader s, int len) {
  std::cout << "started receiving" << std::endl;
  for (int i = 0; i < len; ++i)
    std::cout << id  << " got " << co_await s.read() << std::endl;
  co_return {};
}

int main() {
  scheduler s;
  // s.wake(f());
  channel<int> c{5};
  broadcast<int> b{c.get_reader()};
  channel<int> a1{2}, a2{2};
  b.subscribe(a1.get_writer());
  b.subscribe(a2.get_writer());
  s.wake(b.run());
  s.wake(broadcaster(c.get_writer()));
  s.wake(audience(1, a1.get_reader(), 3));
  s.wake(audience(2, a2.get_reader(), 9));
  s.run();
  return 0;
}
