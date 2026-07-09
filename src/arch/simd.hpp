#ifndef ARCH_SIMD_HPP
#define ARCH_SIMD_HPP

#include "base.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <experimental/simd>
#include <utility>

namespace Cobra::Arch {

namespace detail {

constexpr size_t simd_lanes = 8;

template <typename T>
using simd_block_t = std::experimental::fixed_size_simd<T, simd_lanes>;

template <typename T, size_t N>
constexpr simd_block_t<T> load_block(const std::array<T, N>& data, size_t block) {
    return simd_block_t<T>(data.data() + (block * simd_lanes), std::experimental::element_aligned);
}

template <typename T, size_t N>
constexpr void store_block(std::array<T, N>& data, size_t block, const simd_block_t<T>& val) {
    val.copy_to(data.data() + (block * simd_lanes), std::experimental::element_aligned);
}

} // namespace detail

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using BitboardBase<T, N>::data;

    constexpr Bitboard operator~() const {
        if consteval {
            Bitboard r{};
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((r[i] = static_cast<T>(~data[i])), ...);
            }(std::make_index_sequence<N>());
            return r;
        }

        Bitboard r{};
        constexpr size_t blocks = N / detail::simd_lanes;
        constexpr size_t tail_start = blocks * detail::simd_lanes;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(r.data, i, ~detail::load_block<T>(data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((r.data[tail_start + i] = static_cast<T>(~data[tail_start + i])), ...);
        }(std::make_index_sequence<N - tail_start>());
        return r;
    }

    constexpr Bitboard& operator|=(const Bitboard& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] |= other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }

        constexpr size_t blocks = N / detail::simd_lanes;
        constexpr size_t tail_start = blocks * detail::simd_lanes;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::load_block<T>(data, i) | detail::load_block<T>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] |= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator&=(const Bitboard& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] &= other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }

        constexpr size_t blocks = N / detail::simd_lanes;
        constexpr size_t tail_start = blocks * detail::simd_lanes;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::load_block<T>(data, i) & detail::load_block<T>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] &= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator^=(const Bitboard& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] ^= other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }

        constexpr size_t blocks = N / detail::simd_lanes;
        constexpr size_t tail_start = blocks * detail::simd_lanes;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::load_block<T>(data, i) ^ detail::load_block<T>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] ^= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator<<=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            Bitboard in = *this;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] = static_cast<T>(in[i] << bits)), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }

        constexpr size_t blocks = N / detail::simd_lanes;
        constexpr size_t tail_start = blocks * detail::simd_lanes;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::load_block<T>(data, i) << bits)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] = static_cast<T>(data[tail_start + i] << bits)), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator>>=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            Bitboard in = *this;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] = static_cast<T>(in[i] >> bits)), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }

        constexpr size_t blocks = N / detail::simd_lanes;
        constexpr size_t tail_start = blocks * detail::simd_lanes;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::load_block<T>(data, i) >> bits)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] = static_cast<T>(data[tail_start + i] >> bits)), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }
};

} // namespace Cobra::Arch

#endif
