#include <algorithm>
#include <chrono>
#include <functional>
#include <iostream>
#include <iterator>
#include <random>
#include <type_traits>
#include <vector>

namespace chrono = std::chrono;
template <class T>
using iterator_category = typename std::iterator_traits<T>::iterator_category;
template <class T>
using value_type = typename std::iterator_traits<T>::value_type;

template <class T>
std::ostream& operator << (std::ostream& os, const std::vector<T>& vec) {
    bool comma = false;
    int k = 0;
    os << '[';
    for (auto& v : vec) {
        if (comma)
            os << ", ";
        if (k >= 64) {
            os << "...";
            break;
        }
        os << v;
        comma = true;
        ++k;
    }
    os << ']';
    return os;
}

template <class RI, class Cmp>
void merge(RI begin, RI mid, RI end, Cmp cmp) {
    std::vector<value_type<RI>> sorted(end - begin);
    int i = 0, j = 0;
    for (auto& k : sorted) {
        if (begin + i == mid)
            k = mid[j++];
        else if (mid + j == end)
            k = begin[i++];
        else if (cmp(begin[i], mid[j]))
            k = begin[i++];
        else
            k = mid[j++];
    }
    std::copy(sorted.begin(), sorted.end(), begin);
}

template <class RI, class Cmp = std::less<value_type<RI>>>
void merge_sort(RI begin, RI end, Cmp cmp = Cmp{}) {
    static_assert(std::is_same<iterator_category<RI>,
                               std::random_access_iterator_tag>::value,
                  "merge_sort() requires random access iterators.");
    if (begin < end - 1) {
        RI mid = begin + (end - begin) / 2;
        merge_sort(begin, mid, cmp);
        merge_sort(mid, end, cmp);
        merge(begin, mid, end, cmp);
    }
}

template <class RI, class Cmp = std::less<value_type<RI>>>
void quick_sort(RI begin, RI end, Cmp cmp = Cmp{}) {
    static_assert(std::is_same<iterator_category<RI>,
                               std::random_access_iterator_tag>::value,
                  "quick_sort() requires random access iterators.");
    if (begin < end-1) {
        auto& pivot = *begin;
        RI mid = begin + 1;
        for (RI i = mid; i != end; ++i) {
            if (cmp(*i, pivot)) {
                std::swap(*i, *mid);
                ++mid;
            }
        }
        std::swap(pivot, *(mid-1));
        quick_sort(begin, mid-1);
        quick_sort(mid, end);
    }
}

template <class Func>
void benchmark(Func sort, int length) {
    std::vector<int> v(length);
    auto rand = std::bind(std::uniform_int_distribution<>(-1000000, 1000000),
                          std::mt19937{std::random_device{}()});
    std::generate(v.begin(), v.end(), rand);
    std::cout << v << std::endl;
    auto before = chrono::high_resolution_clock::now();
    sort(v.begin(), v.end(), {});
    auto after = chrono::high_resolution_clock::now();
    for (auto i = v.begin(); i != v.end()-1; ++i) {
        if (*i > *(i+1)) {
            std::cout << "Sort failed." << std::endl;
            return;
        }
    }
    std::cout << v << std::endl
              << "n = " << length << std::endl
              << "time = " << chrono::duration_cast<chrono::milliseconds>(after - before).count()
                  << " ms" << std::endl;
    std::cout << std::endl;
}

int main() {
    std::cout << "Testing merge_sort:" << std::endl;
    for (int n : { 16, 32, 64, 1000, 10000, 100000 }) {
        benchmark(merge_sort<std::vector<int>::iterator>, n);
    }
    std::cout << "====================================" << std::endl;
    
    std::cout << "Testing quick_sort:" << std::endl;
    for (int n : { 16, 32, 64, 1000, 10000, 100000 }) {
        benchmark(quick_sort<std::vector<int>::iterator>, n);
    }
}
