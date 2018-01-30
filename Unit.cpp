#include <iostream>
#include "Unit.hpp"

template <class T>
inline decltype(T{} * T{} * T{}) cube(T d) { return d * d * d; }

int main() {
    Energy e = Force3D{3, 4, 0} * Displacement3D{6, 8, 0} * 5;
    Unit<-1, 3, -2> G = 6.754e-11;
    Displacement3D R = {0, 6371e3, 0};
    Mass M = 5.98e24;
    Force3D Fg = G * Mass{1} * M * R / cube(R.length());
    std::cout << e << std::endl;
    std::cout << Fg << std::endl;
}
