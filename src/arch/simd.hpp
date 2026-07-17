#ifndef ARCH_SIMD_HPP
#define ARCH_SIMD_HPP

#include "base.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <experimental/simd>
#include <utility>

namespace Cobra::Arch {

namespace detail {

template <size_t N>
inline constexpr size_t simd_lanes = N >= 4 ? 4 : 2;

template <size_t N>
using simd_block_t = std::experimental::fixed_size_simd<uint64_t, simd_lanes<N>>;

template <size_t N>
inline simd_block_t<N> load_block(const std::array<uint64_t, N>& data, size_t block) {
    return simd_block_t<N>(data.data() + (block * simd_lanes<N>), std::experimental::element_aligned);
}

template <size_t N>
inline void store_block(std::array<uint64_t, N>& data, size_t block, const simd_block_t<N>& val) {
    val.copy_to(data.data() + (block * simd_lanes<N>), std::experimental::element_aligned);
}

} // namespace detail

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using BitboardBase<T, N>::data;
    using BitboardBase<T, N>::operator=;

    constexpr Bitboard operator~() const {
        if consteval {
            Bitboard r{};
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((r[i] = static_cast<T>(~data[i])), ...);
            }(std::make_index_sequence<N>());
            return r;
        }

        Bitboard r{};
        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(r.data, i, ~detail::load_block<N>(data, i))), ...);
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

        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(data, i, detail::load_block<N>(data, i) | detail::load_block<N>(other.data, i))), ...);
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

        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(data, i, detail::load_block<N>(data, i) & detail::load_block<N>(other.data, i))), ...);
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

        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(data, i, detail::load_block<N>(data, i) ^ detail::load_block<N>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] ^= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator+=(const Bitboard& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] += other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }

        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(data, i, detail::load_block<N>(data, i) + detail::load_block<N>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] += other[tail_start + i]), ...);
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

        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(data, i, detail::load_block<N>(data, i) << bits)), ...);
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

        constexpr size_t blocks = N / detail::simd_lanes<N>;
        constexpr size_t tail_start = blocks * detail::simd_lanes<N>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<N>(data, i, detail::load_block<N>(data, i) >> bits)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] = static_cast<T>(data[tail_start + i] >> bits)), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard operator|(const Bitboard& o) const {
        Bitboard r = *this;
        r |= o;
        return r;
    }

    constexpr Bitboard operator&(const Bitboard& o) const {
        Bitboard r = *this;
        r &= o;
        return r;
    }

    constexpr Bitboard operator^(const Bitboard& o) const {
        Bitboard r = *this;
        r ^= o;
        return r;
    }

    constexpr Bitboard operator+(const Bitboard& o) const {
        Bitboard r = *this;
        r += o;
        return r;
    }

    constexpr Bitboard operator<<(int bits) const {
        Bitboard r = *this;
        r <<= bits;
        return r;
    }

    constexpr Bitboard operator>>(int bits) const {
        Bitboard r = *this;
        r >>= bits;
        return r;
    }
};

} // namespace Cobra::Arch

#endif