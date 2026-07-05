#ifndef ARCH_SCALAR_HPP
#define ARCH_SCALAR_HPP

#include <array>
#include <cassert>
#include <cstddef>
#include <utility>

namespace Cobra::Arch {

template <typename T, size_t N>
struct Bitboard {
    std::array<T, N> data{};

    constexpr T& operator[](size_t i) {
        return data[i];
    }

    constexpr const T& operator[](size_t i) const {
        return data[i];
    }

    constexpr Bitboard operator~() const {
        Bitboard result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result[i] = ~data[i]), ...);
        }(std::make_index_sequence<N>());
        return result;
    }

    constexpr Bitboard& operator|=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] |= other.data[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator|(const Bitboard& other) const {
        Bitboard result = *this;
        result |= other;
        return result;
    }

    constexpr Bitboard& operator&=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] &= other.data[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator&(const Bitboard& other) const {
        Bitboard result = *this;
        result &= other;
        return result;
    }

    constexpr Bitboard& operator^=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] ^= other.data[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator^(const Bitboard& other) const {
        Bitboard result = *this;
        result ^= other;
        return result;
    }

    constexpr Bitboard& operator+=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] += other.data[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator+(const Bitboard& other) const {
        Bitboard result = *this;
        result += other;
        return result;
    }

    constexpr Bitboard& operator<<=(const int bits) {
        assert(bits >= 0 && bits < 64);
        Bitboard input = *this;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] = input.data[i] << bits), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator<<(const int bits) const {
        Bitboard result = *this;
        result <<= bits;
        return result;
    }

    constexpr Bitboard& operator>>=(const int bits) {
        assert(bits >= 0 && bits < 64);
        Bitboard input = *this;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] = input.data[i] >> bits), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator>>(const int bits) const {
        Bitboard result = *this;
        result >>= bits;
        return result;
    }
};

} // namespace Cobra::Arch

#endif