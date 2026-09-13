#pragma once

#include "base.hpp"

#include <arm_neon.h>
#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace Cobra::Arch {

template <typename T>
struct NeonBackend {
    static_assert(std::is_same_v<T, uint16_t> || std::is_same_v<T, uint32_t> || std::is_same_v<T, uint64_t>);

    struct NeonT {
        using Native = std::conditional_t<std::is_same_v<T, uint16_t>, uint16x8_t, std::conditional_t<std::is_same_v<T, uint32_t>, uint32x4_t, uint64x2_t>>;
        Native value;

        static constexpr NeonT splat(const T value) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vdupq_n_u16(value)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vdupq_n_u32(value)};
            else
                return {vdupq_n_u64(value)};
        }

        friend constexpr NeonT operator~(const NeonT v) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vmvnq_u16(v.value)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vmvnq_u32(v.value)};
            else
                return {veorq_u64(v.value, vdupq_n_u64(~uint64_t{0}))};
        }

        friend constexpr NeonT operator|(const NeonT a, const NeonT b) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vorrq_u16(a.value, b.value)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vorrq_u32(a.value, b.value)};
            else
                return {vorrq_u64(a.value, b.value)};
        }

        friend constexpr NeonT operator&(const NeonT a, const NeonT b) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vandq_u16(a.value, b.value)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vandq_u32(a.value, b.value)};
            else
                return {vandq_u64(a.value, b.value)};
        }

        friend constexpr NeonT operator^(const NeonT a, const NeonT b) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {veorq_u16(a.value, b.value)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {veorq_u32(a.value, b.value)};
            else
                return {veorq_u64(a.value, b.value)};
        }

        friend constexpr NeonT operator-(const NeonT a, const NeonT b) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vsubq_u16(a.value, b.value)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vsubq_u32(a.value, b.value)};
            else
                return {vsubq_u64(a.value, b.value)};
        }

        friend constexpr NeonT operator<<(const NeonT v, const int bits) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vshlq_u16(v.value, vdupq_n_s16(static_cast<int16_t>(bits)))};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vshlq_u32(v.value, vdupq_n_s32(static_cast<int32_t>(bits)))};
            else
                return {vshlq_u64(v.value, vdupq_n_s64(static_cast<int64_t>(bits)))};
        }

        friend constexpr NeonT operator>>(const NeonT v, const int bits) {
            return v << -bits;
        }

        friend constexpr NeonT operator<<(const NeonT v, const NeonT bits) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vshlq_u16(v.value, vreinterpretq_s16_u16(bits.value))};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vshlq_u32(v.value, vreinterpretq_s32_u32(bits.value))};
            else
                return {vshlq_u64(v.value, vreinterpretq_s64_u64(bits.value))};
        }

        template <size_t lane>
        static constexpr NeonT set_lane(const NeonT v, const T value) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vsetq_lane_u16(value, v.value, lane)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vsetq_lane_u32(value, v.value, lane)};
            else
                return {vsetq_lane_u64(value, v.value, lane)};
        }

        template <int offset>
        static constexpr NeonT ext(const NeonT a, const NeonT b) {
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vextq_u16(a.value, b.value, offset)};
            else if constexpr (std::is_same_v<T, uint32_t>)
                return {vextq_u32(a.value, b.value, offset)};
            else
                return {vextq_u64(a.value, b.value, offset)};
        }

        static constexpr NeonT clz(const NeonT v) {
            static_assert(std::is_same_v<T, uint16_t> || std::is_same_v<T, uint32_t>);
            if constexpr (std::is_same_v<T, uint16_t>)
                return {vclzq_u16(v.value)};
            else
                return {vclzq_u32(v.value)};
        }
    };

    using Block = NeonT;
    static constexpr size_t lanes = sizeof(typename NeonT::Native) / sizeof(T);

    static constexpr Block splat(const T value) {
        return Block::splat(value);
    }

    static constexpr Block load(const T* values) {
        if constexpr (std::is_same_v<T, uint16_t>)
            return {vld1q_u16(values)};
        else if constexpr (std::is_same_v<T, uint32_t>)
            return {vld1q_u32(values)};
        else
            return {vld1q_u64(values)};
    }

    static constexpr void store(T* values, const Block val) {
        if constexpr (std::is_same_v<T, uint16_t>)
            vst1q_u16(values, val.value);
        else if constexpr (std::is_same_v<T, uint32_t>)
            vst1q_u32(values, val.value);
        else
            vst1q_u64(values, val.value);
    }

    template <size_t lane>
    static constexpr Block set_lane(const Block value, const T scalar) {
        return Block::template set_lane<lane>(value, scalar);
    }

    template <int offset>
    static constexpr Block ext(const Block a, const Block b) {
        return Block::template ext<offset>(a, b);
    }

    template <int bits>
    static constexpr Block shift(const Block value) {
        if constexpr (bits >= 0)
            return value << bits;
        else
            return value >> -bits;
    }

    template <typename U = T>
    requires (std::is_same_v<U, uint16_t> || std::is_same_v<U, uint32_t>)
    static constexpr Block clz(const Block value) {
        return Block::clz(value);
    }

    static constexpr T reduce_or(Block value) {
        if constexpr (std::is_same_v<T, uint16_t>) {
            value.value = vorrq_u16(value.value, vextq_u16(value.value, value.value, 4));
            value.value = vorrq_u16(value.value, vextq_u16(value.value, value.value, 2));
            value.value = vorrq_u16(value.value, vextq_u16(value.value, value.value, 1));
            return vgetq_lane_u16(value.value, 0);
        } else if constexpr (std::is_same_v<T, uint32_t>) {
            value.value = vorrq_u32(value.value, vextq_u32(value.value, value.value, 2));
            value.value = vorrq_u32(value.value, vextq_u32(value.value, value.value, 1));
            return vgetq_lane_u32(value.value, 0);
        } else {
            value.value = vorrq_u64(value.value, vextq_u64(value.value, value.value, 1));
            return vgetq_lane_u64(value.value, 0);
        }
    }
};

template <typename T, size_t N>
using Bitboard = BitboardBase<T, N, NeonBackend<T>>;

} // namespace Cobra::Arch