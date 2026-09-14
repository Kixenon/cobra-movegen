#pragma once

#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <limits>
#include <utility>

namespace Cobra::Arch {

template <typename T, size_t N, typename Backend>
struct BitboardBase {
    using Block = Backend::Block;
    std::array<T, N> data;

protected:
    static constexpr size_t lanes = Backend::lanes;
    static constexpr size_t blocks = N / lanes;
    static constexpr size_t tailStart = blocks * lanes;

    static constexpr Block load_block(const std::array<T, N>& values, const size_t block) {
        return Backend::load(values.data() + (block * lanes));
    }

    static constexpr void store_block(std::array<T, N>& values, const size_t block, const Block value) {
        Backend::store(values.data() + (block * lanes), value);
    }

    template <size_t block>
    static constexpr Block load_padded(const BitboardBase& source) {
        if constexpr (block < blocks)
            return load_block(source.data, block);
        else if constexpr (block * lanes < N) {
            auto value = Backend::splat(T{});
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((value = Backend::template set_lane<i>(value, source.data[(block * lanes) + i])), ...);
            }(std::make_index_sequence<N - (block * lanes)>());
            return value;
        } else
            return Backend::splat(T{});
    }

    template <int dx, int dy, size_t block>
    static constexpr Block load_shifted_block(const BitboardBase& source) {
        constexpr auto vs = [](const Block value) {
            return Backend::template shift<dy>(value);
        };

        if constexpr (dx > 0) {
            constexpr int q = dx / static_cast<int>(lanes);
            constexpr int rem = dx % static_cast<int>(lanes);
            if constexpr (static_cast<int>(block) < q)
                return Backend::splat(T{});
            else if constexpr (rem == 0)
                return vs(load_padded<block - q>(source));
            else {
                constexpr int current = static_cast<int>(block) - q;
                const auto before = [&] {
                    if constexpr (current > 0)
                        return load_padded<static_cast<size_t>(current - 1)>(source);
                    else
                        return Backend::splat(T{});
                }();
                const auto after = load_padded<static_cast<size_t>(current)>(source);
                return vs(Backend::template ext<static_cast<int>(lanes) - rem>(before, after));
            }
        } else if constexpr (dx < 0) {
            constexpr int adx = -dx;
            constexpr int q = adx / static_cast<int>(lanes);
            constexpr int rem = adx % static_cast<int>(lanes);
            if constexpr (rem == 0)
                return vs(load_padded<block + q>(source));
            else {
                constexpr int current = static_cast<int>(block) + q;
                const auto before = load_padded<static_cast<size_t>(current)>(source);
                const auto after = load_padded<static_cast<size_t>(current + 1)>(source);
                return vs(Backend::template ext<rem>(before, after));
            }
        } else
            return vs(load_padded<block>(source));
    }

    template <int dx, int dy, size_t index>
    static constexpr T shifted_value(const BitboardBase& source) {
        constexpr int sourceIndex = static_cast<int>(index) - dx;
        if constexpr (sourceIndex >= 0 && sourceIndex < static_cast<int>(N)) {
            if constexpr (dy > 0)
                return static_cast<T>(source.data[static_cast<size_t>(sourceIndex)] << dy);
            else if constexpr (dy < 0)
                return static_cast<T>(source.data[static_cast<size_t>(sourceIndex)] >> -dy);
            else
                return source.data[static_cast<size_t>(sourceIndex)];
        } else
            return T{};
    }

    template <int dx, int dy>
    constexpr BitboardBase scalar_shift() const {
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
        else if constexpr (dy < 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result[i] = static_cast<T>(result[i] >> -dy)), ...);
            }(std::make_index_sequence<N>());
        return result;
    }

    template <size_t index>
    constexpr T top_ray_value(const T hMask) const {
        constexpr int digits = std::numeric_limits<T>::digits;
        const int width = std::bit_width(static_cast<T>(hMask & ~data[index]));
        const T fill = width == digits ? static_cast<T>(~T{}) : static_cast<T>((static_cast<T>(1) << width) - 1);
        return static_cast<T>(hMask ^ fill);
    }

    constexpr BitboardBase scalar_top_ray(const T hMask) const {
        BitboardBase result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result[i] = top_ray_value<i>(hMask)), ...);
        }(std::make_index_sequence<N>());
        return result;
    }

    template <typename Fn>
    constexpr BitboardBase map(Fn&& fn) const {
        BitboardBase result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(result.data, i, fn(load_block(data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[tailStart + i] = static_cast<T>(fn(data[tailStart + i]))), ...);
        }(std::make_index_sequence<N - tailStart>());
        return result;
    }

    template <typename Fn>
    constexpr BitboardBase& map_assign(Fn&& fn) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, fn(load_block(data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] = static_cast<T>(fn(data[tailStart + i]))), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

    template <typename Fn>
    constexpr BitboardBase& zip_assign(const BitboardBase& other, Fn&& fn) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(data, i, fn(load_block(data, i), load_block(other.data, i))), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[tailStart + i] = static_cast<T>(fn(data[tailStart + i], other.data[tailStart + i]))), ...);
        }(std::make_index_sequence<N - tailStart>());
        return *this;
    }

public:
    constexpr T& operator[](size_t i) {
        return data[i];
    }

    constexpr const T& operator[](size_t i) const {
        return data[i];
    }

    constexpr BitboardBase operator~() const {
        if consteval {
            BitboardBase result{};
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result[i] = static_cast<T>(~data[i])), ...);
            }(std::make_index_sequence<N>());
            return result;
        }
        return map([](const auto value) { return ~value; });
    }

    constexpr BitboardBase& operator|=(const BitboardBase& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] |= other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }
        return zip_assign(other, [](const auto a, const auto b) { return a | b; });
    }

    constexpr BitboardBase& operator&=(const BitboardBase& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] &= other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }
        return zip_assign(other, [](const auto a, const auto b) { return a & b; });
    }

    constexpr BitboardBase& operator^=(const BitboardBase& other) {
        if consteval {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] ^= other[i]), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }
        return zip_assign(other, [](const auto a, const auto b) { return a ^ b; });
    }

    constexpr BitboardBase& operator<<=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            BitboardBase input = *this;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] = static_cast<T>(input[i] << bits)), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }
        return map_assign([bits](const auto value) { return value << bits; });
    }

    constexpr BitboardBase& operator>>=(const int bits) {
        assert(bits >= 0 && bits < static_cast<int>(sizeof(T) * 8));
        if consteval {
            BitboardBase input = *this;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] = static_cast<T>(input[i] >> bits)), ...);
            }(std::make_index_sequence<N>());
            return *this;
        }
        return map_assign([bits](const auto value) { return value >> bits; });
    }

    template <int dx, int dy = 0>
    constexpr BitboardBase shift() const {
        static_assert(dx > -static_cast<int>(N) && dx < static_cast<int>(N));
        static_assert(dy > -std::numeric_limits<T>::digits && dy < std::numeric_limits<T>::digits);

        if consteval {
            return scalar_shift<dx, dy>();
        }

        BitboardBase result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            (store_block(result.data, i, load_shifted_block<dx, dy, i>(*this)), ...);
        }(std::make_index_sequence<blocks>());
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[tailStart + i] = shifted_value<dx, dy, tailStart + i>(*this)), ...);
        }(std::make_index_sequence<N - tailStart>());
        return result;
    }

    constexpr BitboardBase top_ray(const T hMask) const {
        if constexpr (requires { Backend::clz(std::declval<Block>()); }) {
            if consteval {
                return scalar_top_ray(hMask);
            }

            BitboardBase result{};
            const auto mask = Backend::splat(hMask);
            const auto digits = Backend::splat(std::numeric_limits<T>::digits);
            const auto one = Backend::splat(1);

            [&]<size_t... i>(std::index_sequence<i...>) {
                ([&] {
                    const auto blocked = ~load_block(data, i) & mask;
                    const auto width = digits - Backend::clz(blocked);
                    const auto fill = (one << width) - one;
                    store_block(result.data, i, mask ^ fill);
                }(), ...);
            }(std::make_index_sequence<blocks>());

            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result.data[tailStart + i] = top_ray_value<tailStart + i>(hMask)), ...);
            }(std::make_index_sequence<N - tailStart>());
            return result;
        } else
            return scalar_top_ray(hMask);
    }

    constexpr T reduce_or() const {
        if consteval {
            return [&]<size_t... i>(std::index_sequence<i...>) {
                return static_cast<T>((data[i] | ...));
            }(std::make_index_sequence<N>());
        }

        if constexpr (blocks > 0) {
            auto result = load_block(data, 0);
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result = result | load_block(data, i + 1)), ...);
            }(std::make_index_sequence<blocks - 1>());

            T scalar = Backend::reduce_or(result);
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((scalar = static_cast<T>(scalar | data[tailStart + i])), ...);
            }(std::make_index_sequence<N - tailStart>());
            return scalar;
        } else {
            return [&]<size_t... i>(std::index_sequence<i...>) {
                return static_cast<T>((data[i] | ...));
            }(std::make_index_sequence<N>());
        }
    }

    constexpr int popcount() const {
        if consteval {
            return [&]<size_t... i>(std::index_sequence<i...>) {
                return (std::popcount(data[i]) + ...);
            }(std::make_index_sequence<N>());
        }

        if constexpr (requires { Backend::popcount(std::declval<Block>()); }) {
            int result = 0;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result += Backend::popcount(load_block(data, i))), ...);
            }(std::make_index_sequence<blocks>());
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result += std::popcount(data[tailStart + i])), ...);
            }(std::make_index_sequence<N - tailStart>());
            return result;
        } else
            return [&]<size_t... i>(std::index_sequence<i...>) {
                return (std::popcount(data[i]) + ...);
            }(std::make_index_sequence<N>());
    }

    constexpr bool any() const {
        if consteval {
            return [&]<size_t... i>(std::index_sequence<i...>) {
                return (data[i] || ...);
            }(std::make_index_sequence<N>());
        }

        if constexpr (requires { Backend::any(std::declval<Block>()); }) {
            bool result = false;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result = result || Backend::any(load_block(data, i))), ...);
            }(std::make_index_sequence<blocks>());
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((result = result || (data[tailStart + i] != 0)), ...);
            }(std::make_index_sequence<N - tailStart>());
            return result;
        } else
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