#pragma once

#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <limits>
#include <utility>

namespace Cobra::Arch {

template <typename T, size_t N>
struct BitboardBase {
    std::array<T, N> data;

    constexpr T& operator[](size_t i) {
        return data[i];
    }

    constexpr const T& operator[](size_t i) const {
        return data[i];
    }

    constexpr BitboardBase operator~() const {
        BitboardBase result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result[i] = static_cast<T>(~data[i])), ...);
        }(std::make_index_sequence<N>());
        return result;
    }

    constexpr BitboardBase& operator|=(const BitboardBase& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] |= other[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr BitboardBase& operator&=(const BitboardBase& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] &= other[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr BitboardBase& operator^=(const BitboardBase& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] ^= other[i]), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr BitboardBase& operator<<=(const int bits) {
        assert(bits >= 0 && bits < sizeof(T) * 8);
        BitboardBase input = *this;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] = static_cast<T>(input[i] << bits)), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    constexpr BitboardBase& operator>>=(const int bits) {
        assert(bits >= 0 && bits < sizeof(T) * 8);
        BitboardBase input = *this;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] = static_cast<T>(input[i] >> bits)), ...);
        }(std::make_index_sequence<N>());
        return *this;
    }

    template <int dx, int dy = 0>
    constexpr BitboardBase shift() const {
        static_assert(dx > -static_cast<int>(N) && dx < static_cast<int>(N));
        static_assert(dy > -std::numeric_limits<T>::digits && dy < std::numeric_limits<T>::digits);
        BitboardBase result{};
        if constexpr (dx >= 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result[i + dx] = data[i]), ...);
            }(std::make_index_sequence<N - dx>());
        else {
            constexpr int adx = -dx;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result[i] = data[i + adx]), ...);
            }(std::make_index_sequence<N - adx>());
        }
        if constexpr (dy > 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result[i] = static_cast<T>(result[i] << dy)), ...);
            }(std::make_index_sequence<N>());
        else if constexpr (dy < 0) {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result[i] = static_cast<T>(result[i] >> -dy)), ...);
            }(std::make_index_sequence<N>());
        }
        return result;
    }

    template <int dx0, int dy0, int dx1, int dy1, int dx2, int dy2>
    constexpr BitboardBase and_not_shifts(const BitboardBase& occupied) const {
        BitboardBase blocked = occupied;
        blocked |= occupied.template shift<dx0, dy0>();
        blocked |= occupied.template shift<dx1, dy1>();
        blocked |= occupied.template shift<dx2, dy2>();
        BitboardBase result = *this;
        result &= ~blocked;
        return result;
    }

    constexpr BitboardBase top_ray(const T hMask) const {
        BitboardBase result{};
        constexpr int digits = std::numeric_limits<T>::digits;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ([&] {
                const int width = std::bit_width(static_cast<T>(hMask & ~data[i]));
                const T fill = width == digits
                    ? static_cast<T>(~T{})
                    : static_cast<T>((static_cast<T>(1) << width) - 1);
                result[i] = static_cast<T>(hMask ^ fill);
            }(), ...);
        }(std::make_index_sequence<N>());
        return result;
    }

    constexpr bool any() const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return (data[i] || ...);
        }(std::make_index_sequence<N>());
    }

    constexpr bool operator==(const BitboardBase& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] == other[i]) && ...);
        }(std::make_index_sequence<N>());
    }

    constexpr bool operator!=(const BitboardBase& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] != other[i]) || ...);
        }(std::make_index_sequence<N>());
    }
};

} // namespace Cobra::Arch