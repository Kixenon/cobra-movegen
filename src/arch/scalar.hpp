#ifndef ARCH_SCALAR_HPP
#define ARCH_SCALAR_HPP

#include "base.hpp"
#include <cstddef>

namespace Cobra::Arch {

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {};

} // namespace Cobra::Arch

#endif