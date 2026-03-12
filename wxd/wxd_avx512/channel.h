#include <queue>
#include <condition_variable>
#include <mutex>
#include <limits>
#include <string>
#include "log.h"

template <class T, size_t N = std::numeric_limits<size_t>::max()>
class channel {
public:
  struct perf_counters {
    int num_take;
    int num_take_wait;
    int num_put;
    int num_put_wait;
  };
  channel(std::string name): name{std::move(name)} {}
  channel() = default;
  ~channel() {
    if (!name.empty()) LOG_PROFILING("channel %s: blocking on take %.2f%%, blocking on put %.2f%%\n", name.data(), (float)pc.num_take_wait / pc.num_take * 100, (float)pc.num_put_wait / pc.num_put * 100);
  }
  T take() {
    std::unique_lock lock(mutex);
    ++pc.num_take;
    while (data.size() == 0) {
      ++pc.num_take_wait;
      full.wait(lock);
      ++pc.num_take;
    }
    T result = std::move(data.front());
    data.pop();
    lock.unlock();
    empty.notify_one();
    return result;
  }
  template <class... U>
  void put(U&&... value) {
    std::unique_lock lock(mutex);
    ++pc.num_put;
    while (data.size() == N) {
      ++pc.num_put_wait;
      empty.wait(lock);
      ++pc.num_put;
    }
    data.emplace(std::forward<U>(value)...);
    lock.unlock();
    full.notify_one();
  }
private:
  std::queue<T> data;
  std::condition_variable empty;
  std::condition_variable full;
  std::mutex mutex;
  std::string name;
  perf_counters pc = { 0, 0, 0, 0 };
};
