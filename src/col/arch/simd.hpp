#pragma once

#include "base.hpp"

#include <cstddef>
#include <experimental/simd>

namespace Cobra::Arch {

template <typename T>
struct SimdBackend {
    using Block = std::experimental::fixed_size_simd<T, 8>;
    static constexpr size_t lanes = 8;
    static constexpr bool hasClz = false;

    static Block splat(const T value) {
        return Block(value);
    }

    template <int bits>
    static Block shift(const Block value) {
        if constexpr (bits >= 0)
            return value << bits;
        else
            return value >> -bits;
    }

    template <size_t lane>
    static Block set_lane(Block value, const T scalar) {
        value[lane] = scalar;
        return value;
    }

    template <int offset>
    static Block ext(const Block a, const Block b) {
        Block result;
        #pragma unroll
        for (size_t i = 0; i < lanes; ++i)
            result[i] = i + offset < lanes ? a[i + offset] : b[i + offset - lanes];
        return result;
    }

    static Block load(const T* values) {
        return Block(values, std::experimental::element_aligned);
    }

    static void store(T* values, const Block value) {
        value.copy_to(values, std::experimental::element_aligned);
    }
};

template <typename T, size_t N>
using Bitboard = BitboardBase<T, N, SimdBackend<T>>;

} // namespace Cobra::Arch