#pragma once

#include "base.hpp"

#include <cstddef>

namespace Cobra::Arch {

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using BitboardBase<T, N>::operator=;
};

} // namespace Cobra::Arch