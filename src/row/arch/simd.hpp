#pragma once

#include "base.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <experimental/simd>
#include <utility>

namespace Cobra::Arch {

template <typename T, size_t N>
struct Bitboard : BitboardBase<T, N> {
    using Base = BitboardBase<T, N>;
    using Base::data;
    using Base::operator=;

private:
    using SimdBlock = std::experimental::fixed_size_simd<uint64_t, lanes>;
    static constexpr size_t lanes = N >= 4 ? 4 : 2;
    static constexpr size_t blocks = N / lanes;
    static constexpr size_t tailStart = blocks * lanes;

    static SimdBlock load_block(const std::array<uint64_t, N>& values, size_t block) {
        return SimdBlock(values.data() + (block * lanes), std::experimental::element_aligned);
    }

    static void store_block(std::array<uint64_t, N>& values, size_t block, const SimdBlock& val) {
        val.copy_to(values.data() + (block * lanes), std::experimental::element_aligned);
    }

public:
    constexpr Bitboard operator~() const {
        if consteval {
            return Bitboard{Base::operator~()};
        }

        Bitboard r;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(r.data, i, ~load_block(data, i)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((r.data[tailStart + i] = static_cast<T>(~data[tailStart + i])), ...);
        }(std::make_index_sequence<N - tailStart>());
        return r;
    }

    constexpr Bitboard& operator|=(const Bitboard& other) {
        if consteval {
            Base::operator|=(other);
            return *this;
        }

        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, load_block(data, i) | load_block(other.data, i)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] |= other[tailStart + i]), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

    constexpr Bitboard& operator&=(const Bitboard& other) {
        if consteval {
            Base::operator&=(other);
            return *this;
        }

        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, load_block(data, i) & load_block(other.data, i)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] &= other[tailStart + i]), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

    constexpr Bitboard& operator^=(const Bitboard& other) {
        if consteval {
            Base::operator^=(other);
            return *this;
        }

        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, load_block(data, i) ^ load_block(other.data, i)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] ^= other[tailStart + i]), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

    constexpr Bitboard& operator+=(const Bitboard& other) {
        if consteval {
            Base::operator+=(other);
            return *this;
        }

        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, load_block(data, i) + load_block(other.data, i)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] += other[tailStart + i]), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

    constexpr Bitboard& operator<<=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            Base::operator<<=(bits);
            return *this;
        }

        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, load_block(data, i) << bits), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] = static_cast<T>(data[tailStart + i] << bits)), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

    constexpr Bitboard& operator>>=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            Base::operator>>=(bits);
            return *this;
        }

        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, load_block(data, i) >> bits), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] = static_cast<T>(data[tailStart + i] >> bits)), ...);
        }(std::make_index_sequence<N - tailStart>());
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