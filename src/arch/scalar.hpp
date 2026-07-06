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
            ((result[i] = static_cast<T>(~data[i])), ...);
        }(std::make_index_sequence<N>());
        return result;
    }

    constexpr Bitboard& operator|=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] |= other[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator|(const Bitboard& other) const {
        Bitboard r = *this;
        r |= other;
        return r;
    }

    constexpr Bitboard& operator&=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] &= other[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator&(const Bitboard& other) const {
        Bitboard r = *this;
        r &= other;
        return r;
    }

    constexpr Bitboard& operator^=(const Bitboard& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] ^= other[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator^(const Bitboard& other) const {
        Bitboard r = *this;
        r ^= other;
        return r;
    }

    constexpr Bitboard& operator<<=(const int bits) {
        assert(bits >= 0 && bits < sizeof(T) * 8);
        Bitboard input = *this;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] = static_cast<T>(input[i] << bits)), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator<<(const int bits) const {
        Bitboard r = *this;
        r <<= bits;
        return r;
    }

    constexpr Bitboard& operator>>=(const int bits) {
        assert(bits >= 0 && bits < sizeof(T) * 8);
        Bitboard input = *this;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] = static_cast<T>(input[i] >> bits)), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr Bitboard operator>>(const int bits) const {
        Bitboard r = *this;
        r >>= bits;
        return r;
    }

    constexpr bool any() const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return (data[i] || ...);
        }(std::make_index_sequence<N>());
    }

    constexpr bool operator==(const Bitboard& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] == other[i]) && ...);
        }(std::make_index_sequence<N>());
    }

    constexpr bool operator!=(const Bitboard& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] != other[i]) || ...);
        }(std::make_index_sequence<N>());
    }
};

} // namespace Cobra::Arch

#endif