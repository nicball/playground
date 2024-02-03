#include <cassert>
#include <optional>
#include <coroutine>
#include <exception>
#include <iostream>

template <class T>
class optional_promise {
public:
  struct delayed_optional {
    std::optional<T> opt = std::nullopt;
    std::exception_ptr ex = {};
    delayed_optional(optional_promise* p) {
      p->dopt = this;
    }
    operator std::optional<T>() {
      std::cout << "conversion" << std::endl;
      if (this->ex) std::rethrow_exception(std::move(ex));
      else return std::move(this->opt);
    }
    ~delayed_optional() { std::cout << "~delayed_optional()" << std::endl; }
  };
  delayed_optional get_return_object() { return {this}; }
  template <class U>
  auto await_transform(std::optional<U> o) {
    struct awaiter {
      std::optional<U> o;
      bool await_ready() { return o.has_value(); }
      void await_suspend(std::coroutine_handle<> handle) {
        handle.destroy();
      }
      U await_resume() { return *this->o; }
    };
    return awaiter { std::move(o) };
  }
  auto await_transform(std::nullopt_t n) {
    return this->await_transform<int>(n);
  }
  std::suspend_never initial_suspend() { std::cout << "initial suspend" << std::endl; return {}; }
  void unhandled_exception() { std::cout << "unhandled exception" << std::endl; this->dopt->ex = std::current_exception(); }
  std::suspend_never final_suspend() noexcept { std::cout << "final suspend" << std::endl; return {}; }
  void return_value(std::optional<T> v) { dopt->opt = std::move(v); }
  ~optional_promise() { std::cout << "~optional_promise()" << std::endl; }
private:
  delayed_optional* dopt = nullptr;
};

template <class T, class... Args>
struct std::coroutine_traits<std::optional<T>, Args...> {
  using promise_type = optional_promise<T>;
};

std::optional<int> f() {
  //int x = co_await std::nullopt;
  throw 42;
  co_return co_await std::optional{42};
}

int main() {
  try {
    assert(*f() == 42);
  }
  catch (int i) {
    assert(i == 42);
    std::cout << "caught 42" << std::endl;
  }
  return 0;
}
