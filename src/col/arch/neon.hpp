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
    static_assert(std::is_same_v<T, uint16_t> || std::is_same_v<T, uint32_t>);
    if constexpr (std::is_same_v<T, uint16_t>)
        return vclzq_u16(v);
    else
        return vclzq_u32(v);
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

template <typename T>
constexpr neon_t<T> neon_dup(const T value) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vdupq_n_u16(value);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vdupq_n_u32(value);
    else
        return vdupq_n_u64(value);
}

template <typename T, size_t lane>
constexpr neon_t<T> neon_set_lane(neon_t<T> v, T value) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vsetq_lane_u16(value, v, lane);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vsetq_lane_u32(value, v, lane);
    else
        return vsetq_lane_u64(value, v, lane);
}

template <typename T, int offset>
constexpr neon_t<T> neon_ext(neon_t<T> a, neon_t<T> b) {
    if constexpr (std::is_same_v<T, uint16_t>)
        return vextq_u16(a, b, offset);
    else if constexpr (std::is_same_v<T, uint32_t>)
        return vextq_u32(a, b, offset);
    else
        return vextq_u64(a, b, offset);
}

template <typename T, int dy>
constexpr neon_t<T> neon_vertical_shift(neon_t<T> v) {
    if constexpr (dy > 0)
        return neon_shl<T>(v, dy);
    else if constexpr (dy < 0)
        return neon_shr<T>(v, -dy);
    else
        return v;
}

} // namespace Detail

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using Base = BitboardBase<T, N>;
    using Base::data;

private:
    static constexpr size_t lanes = Detail::neon_lanes<T>;
    static constexpr size_t blocks = N / lanes;
    static constexpr size_t tail_start = blocks * lanes;

    template <size_t block>
    static constexpr auto load_padded(const Bitboard& source) {
        if constexpr (block < blocks)
            return Detail::load_block<T>(source.data, block);
        else if constexpr (block * lanes < N) {
            auto value = Detail::neon_dup<T>(0);
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((value = Detail::neon_set_lane<T, i>(value, source.data[(block * lanes) + i])), ...);
            }(std::make_index_sequence<N - (block * lanes)>());
            return value;
        } else
            return Detail::neon_dup<T>(0);
    }

    template <int dx, int dy, size_t block>
    static constexpr auto load_shifted_block(const Bitboard& source) {
        if constexpr (dx > 0) {
            constexpr int q = dx / static_cast<int>(lanes);
            constexpr int rem = dx % static_cast<int>(lanes);
            if constexpr (static_cast<int>(block) < q)
                return Detail::neon_dup<T>(0);
            else if constexpr (rem == 0)
                return Detail::neon_vertical_shift<T, dy>(load_padded<block - q>(source));
            else {
                constexpr int current = static_cast<int>(block) - q;
                const auto before = current > 0 ? load_padded<static_cast<size_t>(current - 1)>(source) : Detail::neon_dup<T>(0);
                const auto after = load_padded<static_cast<size_t>(current)>(source);
                return Detail::neon_vertical_shift<T, dy>(
                    Detail::neon_ext<T, static_cast<int>(lanes) - rem>(before, after));
            }
        } else if constexpr (dx < 0) {
            constexpr int adx = -dx;
            constexpr int q = adx / static_cast<int>(lanes);
            constexpr int rem = adx % static_cast<int>(lanes);
            if constexpr (rem == 0)
                return Detail::neon_vertical_shift<T, dy>(load_padded<block + q>(source));
            else {
                constexpr int current = static_cast<int>(block) + q;
                const auto before = load_padded<static_cast<size_t>(current)>(source);
                const auto after = load_padded<static_cast<size_t>(current + 1)>(source);
                return Detail::neon_vertical_shift<T, dy>(
                    Detail::neon_ext<T, rem>(before, after));
            }
        } else
            return Detail::neon_vertical_shift<T, dy>(load_padded<block>(source));
    }

public:
    template <int dx, int dy = 0>
    constexpr Bitboard shift() const {
        static_assert(dx > -static_cast<int>(N) && dx < static_cast<int>(N));
        static_assert(dy > -std::numeric_limits<T>::digits && dy < std::numeric_limits<T>::digits);

        if consteval {
            return Bitboard{Base::template shift<dx, dy>()};
        }

        Bitboard result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            (Detail::store_block<T>(result.data, i, load_shifted_block<dx, dy, i>(*this)), ...);
        }(std::make_index_sequence<blocks>());

        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[tail_start + i] = [&] {
                constexpr int source = static_cast<int>(tail_start + i) - dx;
                if constexpr (source >= 0 && source < static_cast<int>(N)) {
                    if constexpr (dy > 0)
                        return static_cast<T>(data[source] << dy);
                    else if constexpr (dy < 0)
                        return static_cast<T>(data[source] >> -dy);
                    else
                        return data[source];
                }
                return T{};
            }()), ...);
        }(std::make_index_sequence<N - tail_start>());

        return result;
    }

    template <int dx0, int dy0, int dx1, int dy1, int dx2, int dy2>
    constexpr Bitboard and_not_shifts(const Bitboard& occupied) const {
        if consteval {
            return Bitboard{Base::template and_not_shifts<dx0, dy0, dx1, dy1, dx2, dy2>(occupied)};
        }

        Bitboard result{};

        [&]<size_t... i>(std::index_sequence<i...>) {
            ([&] {
                const auto blocked = Detail::neon_or<T>(
                    Detail::neon_or<T>(Detail::load_block<T>(occupied.data, i), load_shifted_block<dx0, dy0, i>(occupied)),
                    Detail::neon_or<T>(load_shifted_block<dx1, dy1, i>(occupied), load_shifted_block<dx2, dy2, i>(occupied)));
                const auto available = Detail::neon_and<T>(Detail::load_block<T>(data, i), Detail::neon_not<T>(blocked));
                Detail::store_block<T>(result.data, i, available);
            }(), ...);
        }(std::make_index_sequence<blocks>());

        [&]<size_t... i>(std::index_sequence<i...>) {
            ([&] {
                constexpr int x = static_cast<int>(tail_start + i);
                T blocked = occupied.data[x];
                auto add = [&](const int dx, const int dy) {
                    const int source = x - dx;
                    if (source >= 0 && source < static_cast<int>(N)) {
                        if (dy > 0)
                            blocked |= static_cast<T>(occupied.data[static_cast<size_t>(source)] << dy);
                        else if (dy < 0)
                            blocked |= static_cast<T>(occupied.data[static_cast<size_t>(source)] >> -dy);
                        else
                            blocked |= occupied.data[static_cast<size_t>(source)];
                    }
                };
                add(dx0, dy0);
                add(dx1, dy1);
                add(dx2, dy2);
                result.data[x] = static_cast<T>(data[x] & ~blocked);
            }(), ...);
        }(std::make_index_sequence<N - tail_start>());

        return result;
    }

    constexpr Bitboard top_ray(const T hMask) const {
        if constexpr (std::is_same_v<T, uint64_t>)
            return Bitboard{Base::top_ray(hMask)};
        else {
            if consteval {
                return Bitboard{Base::top_ray(hMask)};
            }

            Bitboard result{};

            const auto mask = Detail::neon_dup<T>(hMask);
            const auto digits = Detail::neon_dup<T>(std::numeric_limits<T>::digits);
            const auto one = Detail::neon_dup<T>(1);

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
                    const T fill = width == std::numeric_limits<T>::digits ? static_cast<T>(~T{}) : static_cast<T>((static_cast<T>(1) << width) - 1);
                    result.data[tail_start + i] = static_cast<T>(hMask ^ fill);
                }(), ...);
            }(std::make_index_sequence<N - tail_start>());
            return result;
        }
    }

    constexpr Bitboard operator~() const {
        if consteval {
            return Bitboard{Base::operator~()};
        }

        Bitboard r;
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
            Base::operator|=(other);
            return *this;
        }

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
            Base::operator&=(other);
            return *this;
        }

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
            Base::operator^=(other);
            return *this;
        }

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
            Base::operator<<=(bits);
            return *this;
        }

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
            Base::operator>>=(bits);
            return *this;
        }

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