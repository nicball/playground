#include <queue>
#include <condition_variable>
#include <mutex>

template <size_t N, class T>
class channel {
  std::queue<T> data;
  std::condition_variable empty;
  std::condition_variable full;
  std::mutex mutex;

public:
  channel() = default;
  T take() {
    std::unique_lock lock(mutex);
    full.wait(lock, [this] { return data.size() > 0; });
    T result = std::move(data.front());
    data.pop();
    lock.unlock();
    empty.notify_one();
    return result;
  }
  template <class... U>
  void put(U&&... value) {
    std::unique_lock lock(mutex);
    empty.wait(lock, [this] { return data.size() < N; });
    data.emplace(std::forward<U>(value)...);
    lock.unlock();
    full.notify_one();
  }
};
