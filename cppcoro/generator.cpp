#include <coroutine>
#include <iostream>
#include <optional>
#include <type_traits>

template <class T>
class generator {
public:
  class promise_type {
  public:
    friend class generator;
    template <class U>
    U&& await_transform(U&& u) {
      if constexpr (std::is_same_v<std::remove_reference_t<U>, std::suspend_always>)
        std::cout << "awaited suspend_always" << std::endl;
      else
        std::cout << "awaited something else" << std::endl;
      return static_cast<U&&>(u);
    }
    template <class U>
    std::suspend_always yield_value(U&& value) {
      this->value = &value;
      this->started = true;
      return {};
    }
    // awaiter await_transform(awaiter a) { return a; }
    generator get_return_object() { return generator{this}; }
    std::suspend_always initial_suspend() { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() { this->ex = std::current_exception(); }
  private:
    bool started = false;
    T* value = nullptr;
    std::exception_ptr ex = {};
  };
  generator(promise_type* p): promise{p}, handle{std::coroutine_handle<promise_type>::from_promise(*p)} {}
  ~generator() { this->handle.destroy(); }
  std::optional<T> get() {
    if (this->promise->started && this->handle.done())
      return std::nullopt;
    else {
      this->handle.resume();
      if (this->promise->ex) std::rethrow_exception(this->promise->ex);
      if (this->handle.done())
        return std::nullopt;
      else
        return *this->promise->value;
    }
  }
private:
  promise_type* promise;
  std::coroutine_handle<> handle;
};

int main() {
  auto g = ([]() -> generator<int> {
    for (int i = 0; i < 5; ++i) {
      co_yield i;
      if (i == 3) throw 5;
    }
  })();
  try {
    for (auto i = g.get(); i.has_value(); i = g.get()) std::cout << *i << std::endl;
  }
  catch (int i) {
    std::cout << "caught " << i << std::endl;
  }
  return 0;
}
