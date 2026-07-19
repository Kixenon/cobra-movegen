#pragma once

#include "base.hpp"

#include <arm_neon.h>
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

namespace Cobra::Arch {

namespace detail {

template <typename T>
struct NeonVec;

template <>
struct NeonVec<uint16_t> {
    using type = uint16x8_t;
    static constexpr size_t lanes = 8;
};

template <>
struct NeonVec<uint32_t> {
    using type = uint32x4_t;
    static constexpr size_t lanes = 4;
};

template <>
struct NeonVec<uint64_t> {
    using type = uint64x2_t;
    static constexpr size_t lanes = 2;
};

template <typename T>
using neon_t = typename NeonVec<T>::type;

template <typename T>
constexpr size_t neon_lanes = NeonVec<T>::lanes;

template <typename T, size_t N>
constexpr neon_t<T> load_block(const std::array<T, N>& data, size_t block) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vld1q_u16(data.data() + (block * neon_lanes<T>));
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vld1q_u32(data.data() + (block * neon_lanes<T>));
    else
        return vld1q_u64(data.data() + (block * neon_lanes<T>));
}

template <typename T, size_t N>
constexpr void store_block(std::array<T, N>& data, size_t block, neon_t<T> val) {
    if constexpr (std::is_same_v<T, uint16_t>)
        vst1q_u16(data.data() + (block * neon_lanes<T>), val);
    else if constexpr (std::is_same_v<T, uint32_t>)
        vst1q_u32(data.data() + (block * neon_lanes<T>), val);
    else
        vst1q_u64(data.data() + (block * neon_lanes<T>), val);
}

template <typename T>
constexpr neon_t<T> neon_not(neon_t<T> v) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vmvnq_u16(v);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vmvnq_u32(v);
    else
        return veorq_u64(v, vdupq_n_u64(~uint64_t{0}));
}

template <typename T>
constexpr neon_t<T> neon_or(neon_t<T> a, neon_t<T> b) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vorrq_u16(a, b);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vorrq_u32(a, b);
    else
        return vorrq_u64(a, b);
}

template <typename T>
constexpr neon_t<T> neon_and(neon_t<T> a, neon_t<T> b) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vandq_u16(a, b);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vandq_u32(a, b);
    else
        return vandq_u64(a, b);
}

template <typename T>
constexpr neon_t<T> neon_xor(neon_t<T> a, neon_t<T> b) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return veorq_u16(a, b);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return veorq_u32(a, b);
    else
        return veorq_u64(a, b);
}

template <typename T>
constexpr neon_t<T> neon_shl(neon_t<T> v, int bits) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vshlq_u16(v, vdupq_n_s16(static_cast<int16_t>(bits)));
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vshlq_u32(v, vdupq_n_s32(static_cast<int32_t>(bits)));
    else
        return vshlq_u64(v, vdupq_n_s64(static_cast<int64_t>(bits)));
}

template <typename T>
constexpr neon_t<T> neon_shr(neon_t<T> v, int bits) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vshlq_u16(v, vdupq_n_s16(static_cast<int16_t>(-bits)));
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vshlq_u32(v, vdupq_n_s32(static_cast<int32_t>(-bits)));
    else
        return vshlq_u64(v, vdupq_n_s64(static_cast<int64_t>(-bits)));
}

} // namespace detail

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using BitboardBase<T, N>::data;

    constexpr Bitboard operator~() const {
        if consteval {
            Bitboard r;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((r[i] = static_cast<T>(~data[i])), ...);
            }(std::make_index_sequence<N>());
            return r;
        }

        Bitboard r;
        constexpr size_t blocks = N / detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(r.data, i, detail::neon_not<T>(detail::load_block<T>(data, i)))), ...);
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

        constexpr size_t blocks = N / detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::neon_or<T>(detail::load_block<T>(data, i), detail::load_block<T>(other.data, i)))), ...);
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

        constexpr size_t blocks = N / detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::neon_and<T>(detail::load_block<T>(data, i), detail::load_block<T>(other.data, i)))), ...);
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

        constexpr size_t blocks = N / detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::neon_xor<T>(detail::load_block<T>(data, i), detail::load_block<T>(other.data, i)))), ...);
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

        constexpr size_t blocks = N / detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::neon_shl<T>(detail::load_block<T>(data, i), bits))), ...);
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

        constexpr size_t blocks = N / detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((detail::store_block<T>(data, i, detail::neon_shr<T>(detail::load_block<T>(data, i), bits))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] = static_cast<T>(data[tail_start + i] >> bits)), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }
};

} // namespace Cobra::Arch