#pragma once

#include "base.hpp"

#include <arm_neon.h>
#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <utility>

namespace Cobra::Arch {

namespace Detail {

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
using neon_t = NeonVec<T>::type;

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
constexpr neon_t<T> neon_clz(neon_t<T> v) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vclzq_u16(v);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vclzq_u32(v);
    else
        return vclzq_u64(v);
}

template <typename T>
constexpr neon_t<T> neon_sub(neon_t<T> a, neon_t<T> b) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vsubq_u16(a, b);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vsubq_u32(a, b);
    else
        return vsubq_u64(a, b);
}

template <typename T>
constexpr neon_t<T> neon_shift_one(neon_t<T> bits) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vshlq_u16(vdupq_n_u16(1), vreinterpretq_s16_u16(bits));
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vshlq_u32(vdupq_n_u32(1), vreinterpretq_s32_u32(bits));
    else
        return vshlq_u64(vdupq_n_u64(1), vreinterpretq_s64_u64(bits));
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

} // namespace Detail

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using BitboardBase<T, N>::data;

    constexpr Bitboard top_ray(const T hMask) const {
        if constexpr (std::is_same_v<T, uint64_t>)
            return Bitboard{BitboardBase<T, N>::top_ray(hMask)};
        else {
            if consteval {
                return Bitboard{BitboardBase<T, N>::top_ray(hMask)};
            }

            constexpr size_t lanes = Detail::neon_lanes<T>;
            constexpr size_t blocks = N / lanes;
            constexpr size_t tail_start = blocks * lanes;
            Bitboard result{};

            const auto mask = [&] {
                if constexpr (std::is_same_v<T, uint16_t>)
                    return vdupq_n_u16(hMask);
                else
                    return vdupq_n_u32(hMask);
            }();
            const auto digits = [&] {
                if constexpr (std::is_same_v<T, uint16_t>)
                    return vdupq_n_u16(16);
                else
                    return vdupq_n_u32(32);
            }();
            const auto one = [&] {
                if constexpr (std::is_same_v<T, uint16_t>)
                    return vdupq_n_u16(1);
                else
                    return vdupq_n_u32(1);
            }();

            [&]<size_t... i>(std::index_sequence<i...>) {
                ([&] {
                    const auto blocked = Detail::neon_and<T>(Detail::neon_not<T>(Detail::load_block<T>(data, i)), mask);
                    const auto width = Detail::neon_sub<T>(digits, Detail::neon_clz<T>(blocked));
                    const auto fill = Detail::neon_sub<T>(Detail::neon_shift_one<T>(width), one);
                    Detail::store_block<T>(result.data, i, Detail::neon_xor<T>(mask, fill));
                }(), ...);
            }(std::make_index_sequence<blocks>());

            [&]<size_t... i>(std::index_sequence<i...>) {
                ([&] {
                    const int width = std::bit_width(static_cast<T>(hMask & ~data[tail_start + i]));
                    const T fill = width == std::numeric_limits<T>::digits
                        ? static_cast<T>(~T{})
                        : static_cast<T>((static_cast<T>(1) << width) - 1);
                    result.data[tail_start + i] = static_cast<T>(hMask ^ fill);
                }(), ...);
            }(std::make_index_sequence<N - tail_start>());
            return result;
        }
    }

    constexpr Bitboard operator~() const {
        if consteval {
            return Bitboard{BitboardBase<T, N>::operator~()};
        }

        Bitboard r;
        constexpr size_t blocks = N / Detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * Detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(r.data, i, Detail::neon_not<T>(Detail::load_block<T>(data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((r.data[tail_start + i] = static_cast<T>(~data[tail_start + i])), ...);
        }(std::make_index_sequence<N - tail_start>());
        return r;
    }

    constexpr Bitboard& operator|=(const Bitboard& other) {
        if consteval {
            BitboardBase<T, N>::operator|=(other);
            return *this;
        }

        constexpr size_t blocks = N / Detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * Detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(data, i, Detail::neon_or<T>(Detail::load_block<T>(data, i), Detail::load_block<T>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] |= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator&=(const Bitboard& other) {
        if consteval {
            BitboardBase<T, N>::operator&=(other);
            return *this;
        }

        constexpr size_t blocks = N / Detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * Detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(data, i, Detail::neon_and<T>(Detail::load_block<T>(data, i), Detail::load_block<T>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] &= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator^=(const Bitboard& other) {
        if consteval {
            BitboardBase<T, N>::operator^=(other);
            return *this;
        }

        constexpr size_t blocks = N / Detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * Detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(data, i, Detail::neon_xor<T>(Detail::load_block<T>(data, i), Detail::load_block<T>(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] ^= other[tail_start + i]), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator<<=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            BitboardBase<T, N>::operator<<=(bits);
            return *this;
        }

        constexpr size_t blocks = N / Detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * Detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(data, i, Detail::neon_shl<T>(Detail::load_block<T>(data, i), bits)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] = static_cast<T>(data[tail_start + i] << bits)), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }

    constexpr Bitboard& operator>>=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            BitboardBase<T, N>::operator>>=(bits);
            return *this;
        }

        constexpr size_t blocks = N / Detail::neon_lanes<T>;
        constexpr size_t tail_start = blocks * Detail::neon_lanes<T>;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(data, i, Detail::neon_shr<T>(Detail::load_block<T>(data, i), bits)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tail_start + i] = static_cast<T>(data[tail_start + i] >> bits)), ...);
        }(std::make_index_sequence<N - tail_start>());
        return *this;
    }
};

} // namespace Cobra::Arch