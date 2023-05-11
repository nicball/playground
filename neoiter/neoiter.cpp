#include <memory>
#include <functional>
#include <iostream>
#include <type_traits>
#include <vector>
#include <optional>
#include <exception>

template <class F, class It>
class Map {
    F mapper;
    It wrapped;
public:
    Map(F f, It it): mapper{std::move(f)}, wrapped{std::move(it)} {}
    auto next() { return mapper(wrapped.next()); }
    auto has_next() -> bool { return wrapped.has_next(); }
};

template <class F, class It>
class Filter {
    using E = decltype((std::declval<It>().next()));
    F predicate;
    It wrapped;
    std::optional<E> next_value;

    auto try_next() -> bool {
        while (wrapped.has_next() && !next_value.has_value()) {
            auto v = wrapped.next();
            if (predicate(v)) {
                next_value = std::move(v);
                return true;
            }
        }
        return false;
    }
    auto move_value() -> E {
        auto v = std::move(next_value.value());
        next_value.reset();
        return v;
    }
public:
    Filter(F f, It it): predicate{std::move(f)}, wrapped{std::move(it)} {}
    auto next() -> E {
        if (next_value.has_value()) return move_value();
        else {
            if (try_next()) return move_value();
            throw std::runtime_error{"next() called on depleted iterator!"};
        }
    }
    auto has_next() -> bool {
        if (next_value.has_value()) return true;
        else return try_next();
    }
};

template <class It>
class CopyIterator {
    It iterator;
    It end;
public:
    CopyIterator(It begin, It end): iterator{std::move(begin)}, end{std::move(end)} {}
    auto next() { auto v = *iterator; ++iterator; return v; }
    auto has_next() -> bool { return iterator != end; }
};

template <class It>
class MoveIterator {
    It iterator;
    It end;
public:
    MoveIterator(It begin, It end): iterator{std::move(begin)}, end{std::move(end)} {}
    auto next() { auto v = std::move(*iterator); ++iterator; return v; }
    auto has_next() -> bool { return iterator != end; }
};

template <class It>
class RefIterator {
    It iterator;
    It end;
public:
    RefIterator(It begin, It end): iterator{std::move(begin)}, end{std::move(end)} {}
    auto next() { auto&& v = *iterator; ++iterator; return std::ref(v); }
    auto has_next() -> bool { return iterator != end; }
};

template <class It, class F>
auto consume(F action, It it) -> void {
    while (it.has_next()) action(it.next());
}

template <class It, class F, class T>
auto foldl(F f, T z, It xs) -> T {
    while (xs.has_next()) z = f(std::move(z), xs.next());
    return z;
}

int main() {
    {
        auto v = std::vector<int>{1, 2, 3, 4, 5, 6, 7};
        consume(
            [](auto&& i) { std::cout << " " << i; },
            Map{[](auto&& i) { return i + 1; },
                CopyIterator{v.begin(), v.end()}});
        std::cout << std::endl;
    }

    {
        auto v = std::vector<std::unique_ptr<int>>{};
        for (auto&& i : {1, 2, 3, 4, 5, 6, 7}) v.emplace_back(new auto{i});
        consume(
            [](auto&& i) { std::cout << " " << *i; },
            Map{[](auto&& i) { ++*i; return std::move(i); },
                MoveIterator{v.begin(), v.end()}});
        std::cout << std::endl;
    }

    {
        auto v = std::vector<int>{1, 2, 3, 4, 5, 6, 7};
        consume(
            [](auto&& i) { ++i.get(); },
            RefIterator{v.begin(), v.end()});
        for (int i : v) std::cout << " " << i;
        std::cout << std::endl;
    }

    {
        auto v = std::vector<int>{1, 2, 3, 4, 5, 6, 7};
        consume(
            [](auto&& i) { std::cout << " " << i; },
            Filter{[](auto&& i) { return i % 2; },
                CopyIterator{v.begin(), v.end()}});
        std::cout << std::endl;
    }

    {
        auto v = std::vector<int>{1, 2, 3, 4, 5, 6, 7};
        std::cout << foldl([](auto&& x, auto&& y) { return x + y; },
                           0,
                           CopyIterator{v.begin(), v.end()});
    }

    return 0;
}
