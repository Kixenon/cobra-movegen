#pragma once

#include "base.hpp"

#include <cstddef>

namespace Cobra::Arch {

template <typename T>
struct ScalarBackend {
    using Block = T;
    static constexpr size_t lanes = 1;

    static constexpr Block splat(const T value) {
        return value;
    }

    static constexpr Block load(const T* values) {
        return *values;
    }

    static constexpr void store(T* values, const Block value) {
        *values = value;
    }

    template <size_t lane>
    static constexpr Block set_lane([[maybe_unused]] const Block value, const T scalar) {
        static_assert(lane == 0);
        return scalar;
    }

    template <int offset>
    static constexpr Block ext(const Block a, [[maybe_unused]] const Block b) {
        static_assert(offset == 0);
        return a;
    }

    template <int bits>
    static constexpr Block shift(const Block value) {
        if constexpr (bits >= 0)
            return static_cast<Block>(value << bits);
        else
            return static_cast<Block>(value >> -bits);
    }

    static constexpr T reduce_or(const Block value) {
        return value;
    }
};

template <typename T, size_t N>
using Bitboard = BitboardBase<T, N, ScalarBackend<T>>;

} // namespace Cobra::Arch