#ifndef UNIT_H_
#define UNIT_H_

#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <initializer_list>
#include <string>
#include <sstream>

template <int Kg = 0, int M = 0, int S = 0, int A = 0, int K = 0, int Mol = 0, int Cd = 0>
class Unit {
    double value = 0;

public:
    Unit() = default;
    Unit(double d): value{d} {}
    double to_d() const {
        return value;
    }
    Unit& operator += (const Unit& r) {
        value += r.value;
        return *this;
    }
    Unit& operator -= (const Unit& r) {
        value -= r.value;
        return *this;
    }
    Unit& operator *= (double r) {
        value *= r;
        return *this;
    }
    Unit& operator /= (double r) {
        value /= r;
        return *this;
    }
};

template <int... Us>
inline Unit<Us...> operator + (Unit<Us...> l, Unit<Us...> r) {
    return l += r;
}

template <int... Us>
inline Unit<Us...> operator - (Unit<Us...> l, Unit<Us...> r) {
    return l -= r;
}

template <int... Us>
inline Unit<Us...> operator * (Unit<Us...> l, double r) {
    return l *= r;
}

template <int... Us>
inline Unit<Us...> operator / (Unit<Us...> l, double r) {
    return l /= r;
}

template <int... Us>
inline Unit<Us...> operator * (double l, Unit<Us...> r) {
    return r *= l;
}

template <int... Us>
inline Unit<Us...> operator / (double l, Unit<Us...> r) {
    return l / r.to_d();
}

template <int Kg1, int Kg2, int M1, int M2, int S1, int S2,
          int A1, int A2, int K1, int K2, int Mol1, int Mol2, int Cd1, int Cd2>
inline Unit<Kg1+Kg2, M1+M2, S1+S2, A1+A2, K1+K2, Mol1+Mol2, Cd1+Cd2>
operator * (Unit<Kg1, M1, S1, A1, K1, Mol1, Cd1> l, Unit<Kg2, M2, S2, A2, K2, Mol2, Cd2> r) {
    return {l.to_d() * r.to_d()};
}

template <int Kg1, int Kg2, int M1, int M2, int S1, int S2,
          int A1, int A2, int K1, int K2, int Mol1, int Mol2, int Cd1, int Cd2>
inline Unit<Kg1-Kg2, M1-M2, S1-S2, A1-A2, K1-K2, Mol1-Mol2, Cd1-Cd2>
operator / (Unit<Kg1, M1, S1, A1, K1, Mol1, Cd1> l, Unit<Kg2, M2, S2, A2, K2, Mol2, Cd2> r) {
    return {l.to_d() / r.to_d()};
}

template <unsigned int N, class T>
struct Vector {
    std::array<T, N> vec;

    Vector() = default;
    Vector(std::initializer_list<T> li) {
        assert(li.size() == N);
        std::copy(li.begin(), li.end(), vec.begin());
    }
    ~Vector() = default;

    T& operator [] (unsigned int n) {
        return vec[n];
    }
    const T& operator [] (unsigned int n) const {
        return vec[n];
    }

    Vector& operator += (const Vector& r) {
        for (int i = 0; i < N; i++)
            vec[i] += r.vec[i];
        return *this;
    }
    Vector& operator -= (const Vector& r) {
        for (int i = 0; i < N; i++)
            vec[i] -= r.vec[i];
        return *this;
    }

    typename std::array<T, N>::iterator begin() {
        return vec.begin();
    }
    typename std::array<T, N>::iterator end() {
        return vec.end();
    }
    typename std::array<T, N>::const_iterator begin() const {
        return vec.cbegin();
    }
    typename std::array<T, N>::const_iterator end() const {
        return vec.cend();
    }

    double to_d() const {
        double re = 0;
        for (auto& i : vec)
            re += i.to_d() * i.to_d();
        return std::sqrt(re);
    }

    T length() const {
        return T{to_d()};
    }
};

template <unsigned int N, class T>
inline Vector<N, T> operator + (Vector<N, T> l, const Vector<N, T>& r) {
    return l += r;
}

template <unsigned int N, class T>
inline Vector<N, T> operator - (Vector<N, T> l, const Vector<N, T>&  r) {
    return l -= r;
}

template <unsigned int N, class T, class U>
inline Vector<N, decltype(T{} * U{})> operator * (const Vector<N, T>& v, const U& s) {
    Vector<N, decltype(T{} * U{})> re;
    for (int i = 0; i < N; i++)
        re[i] = v[i] * s;
    return re;
}

template <unsigned int N, class T, class U>
inline Vector<N, decltype(T{} * U{})> operator * (const U& s, const Vector<N, T>& v) {
    Vector<N, decltype(T{} * U{})> re;
    for (int i = 0; i < N; i++)
        re[i] = v[i] * s;
    return re;
}

template <unsigned int N, class T, class U>
inline decltype(T{} * U{}) operator * (const Vector<N, T>& l, const Vector<N, U>& r) {
    decltype(T{} * U{}) re{};
    for (int i = 0; i < N; i++)
        re += l[i] * r[i];
    return re;
}

template <unsigned int N, class T, class U>
inline Vector<N, decltype(T{} / U{})> operator / (const Vector<N, T>& v, const U& s) {
    Vector<N, decltype(T{} / U{})> re;
    for (int i = 0; i < N; i++)
        re[i] = v[i] / s;
    return re;
}

using Acceleration = Unit<0, 1, -2>;
using Angel = Unit<>;
using Area = Unit<0, 2>;
using Density = Unit<1, -3>;
using Displacement = Unit<0, 1, 0>;
using ElectricalCapacitance = Unit<-1, -2, 4, 2>;
using ElectricalResistance = Unit<-1, -2, 4, 2>;
using ElectricCharge = Unit<0, 0, 1, 1>;
using EnergyDensity = Unit<1, -1, -2>;
using Energy = Unit<1, 2, -2>;
using Force = Unit<1, 1, -2>;
using Frequency = Unit<0, 0, -1>;
using HeatCapacity = Unit<1, 2, -2, 0, -1>;
using MagneticFieldStrenth = Unit<1, 0, -2, -1>;
using MagneticFlux = Unit<1, 2, -2, -1>;
using Mass = Unit<1, 0, 0>;
using MolarEnergy = Unit<1, 2, -2, 0, 0, -1>;
using MolarMass = Unit<1, 0, 0, 0, 0, -1>;
using MolarVolume = Unit<0, 3, 0, 0, 0, -1>;
using Power = Unit<1, 2, -3>;
using Pressure = Unit<1, -1, -2>;
using SpecificHeatCapacity = Unit<0, 2, -2, 0, -1>;
using Time = Unit<0, 0, 1>;
using Velocity = Unit<0, 1, -1>;
using Voltage = Unit<1, 2, -3, 1>;
using Volume = Unit<0, 3>;

using Displacement3D = Vector<3, Displacement>;
using Velocity3D = Vector<3, Velocity>;
using Acceleration3D = Vector<3, Acceleration>;
using Force3D = Vector<3, Force>;
using Pressure3D = Vector<3, Pressure>;

template <int Kg = 0, int M = 0, int S = 0, int A = 0, int K = 0, int Mol = 0, int Cd = 0>
inline std::string unitName() {
    std::ostringstream os;
    if (Kg) { os << "kg"; if (Kg != 1) os << "^" << Kg; }
    if (M) { os << "m"; if (M != 1) os << "^" << M; }
    if (S) { os << "s"; if (S != 1) os << "^" << S; }
    if (A) { os << "A"; if (A != 1) os << "^" << A; }
    if (K) { os << "K"; if (K != 1) os << "^" << K; }
    if (Mol) { os << "mol"; if (Mol != 1) os << "^" << Mol; }
    if (Cd) { os << "cd"; if (Cd != 1) os << "^" << Cd; }
    return os.str();
}

template<> inline std::string unitName<0, -1>() {
    return "Hz";
}

template<> inline std::string unitName<1, 1, -2>() {
    return "N";
}

template<> inline std::string unitName<1, -1, -2>() {
    return "Pa";
}

template<> inline std::string unitName<1, 2, -2>() {
    return "J";
}

template<> inline std::string unitName<1, 2, -3>() {
    return "W";
}

template<> inline std::string unitName<0, 0, 1, 1>() {
    return "C";
}

template<> inline std::string unitName<1, 2, -3, -1>() {
    return "V";
}

template<> inline std::string unitName<-1, -2, 4, 2>() {
    return "F";
}

template<> inline std::string unitName<1, 2, -2, 1>() {
    return "Wb";
}

template<> inline std::string unitName<1, 0, -2, 1>() {
    return "T";
}

template <int... Us>
inline std::ostream& operator << (std::ostream& os, Unit<Us...> x) {
    os << x.to_d() << unitName<Us...>();
    return os;
}

template <unsigned int N, int... Us>
inline std::ostream& operator << (std::ostream& os, const Vector<N, Unit<Us...>>& xs) {
    os << "(";
    bool comma = false;
    for (auto i : xs) {
        if (comma)
            os << ", ";
        os << i.to_d();
        comma = true;
    }
    os << ")" << unitName<Us...>();
    return os;
}

#endif
