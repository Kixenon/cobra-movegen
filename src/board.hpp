#ifndef BOARD_HPP
#define BOARD_HPP

#include "header.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <type_traits>
#include <utility>

namespace Cobra {

namespace BoardBase {

constexpr std::array Y = {6, 12, 18, 24};
constexpr int H = ROW_NB;

constexpr bool is_ok_h(const int h) {
    return std::ranges::contains(Y, h) || h == H;
}

constexpr int height_target(const int h) {
    #pragma unroll
    for (const auto i : Y)
        if (h < i)
            return i;
    return H;
}

template <typename Fn>
constexpr auto route(const int h, Fn&& fn) {
    assert(is_ok_h(h));

    switch (h) {
        case Y[0]: return fn.template operator()<Y[0]>();
        case Y[1]: return fn.template operator()<Y[1]>();
        case Y[2]: return fn.template operator()<Y[2]>();
        case Y[3]: return fn.template operator()<Y[3]>();
        default: return fn.template operator()<H>();
        // Somehow using the below is quite a lot slower?
        // case H: return fn.template operator()<H>();
        // default: std::unreachable();
    }
}

} // namespace BoardBase

template <int Height = BoardBase::H>
struct Board {
    static constexpr int H = Height;
    static constexpr int W = COL_NB;
    static_assert(BoardBase::is_ok_h(H));

    using T = std::conditional_t<
        H <= 8, uint8_t,
        std::conditional_t<
            H <= 16, uint16_t,
            std::conditional_t<
                H <= 32, uint32_t,
                uint64_t
            >
        >
    >;

    static constexpr T Tmask = (H >= static_cast<int>(sizeof(T) * 8))
        ? static_cast<T>(-1)
        : (static_cast<T>(1) << H) - static_cast<T>(1);

    T data[W];

    static constexpr bool is_ok_y_local(const int y) {
        return y >= 0 && y < H;
    }

    constexpr void set(const int x, const int y) {
        assert(is_ok_x(x) && is_ok_y_local(y));
        data[x] |= static_cast<T>(1) << y;
    }

    constexpr bool get(const int x, const int y) const {
        assert(is_ok_x(x) && is_ok_y_local(y));
        return data[x] & (static_cast<T>(1) << y);
    }

    static consteval Board all() {
        Board b{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((b.data[i] = Tmask), ...);
        }(std::make_index_sequence<W>());
        return b;
    }

    template <int x>
    static consteval Board col_mask() {
        Board b{};
        b.data[x] = Tmask;
        return b;
    }

    template <int dx>
    static consteval Board shift_mask() {
        Board b{};
        if constexpr (dx > 0) {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((b.data[i + dx] = Tmask), ...);
            }(std::make_index_sequence<W - dx>());
        } else if constexpr (dx < 0) {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((b.data[i] = Tmask), ...);
            }(std::make_index_sequence<W + dx>());
        } else
            b = all();
        return b;
    }

    template <int dx, int dy>
    constexpr Board& shift() {
        if constexpr (dx == 0 && dy == 0)
            return *this;

        if constexpr (dy > 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] <<= dy), ...);
            }(std::make_index_sequence<W>());
        else if constexpr (dy < 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] >>= -dy), ...);
            }(std::make_index_sequence<W>());

        if constexpr (dx > 0) {
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[W - 1 - i] = data[W - 1 - i - dx]), ...);
            }(std::make_index_sequence<W - dx>());
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] = T{0}), ...);
            }(std::make_index_sequence<dx>());
        } else if constexpr (dx < 0) {
            constexpr int adx = -dx;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] = data[i + adx]), ...);
            }(std::make_index_sequence<W - adx>());
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i + W - adx] = T{0}), ...);
            }(std::make_index_sequence<adx>());
        }

        if constexpr (dx != 0) {
            const auto mask = shift_mask<dx>();
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] &= mask.data[i]), ...);
            }(std::make_index_sequence<W>());
        }
        if constexpr (dy != 0 && dx == 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((data[i] &= Tmask), ...);
            }(std::make_index_sequence<W>());

        return *this;
    }

    template <int dx, int dy>
    constexpr Board shifted() const {
        Board result = *this;
        result.shift<dx, dy>();
        return result;
    }

    constexpr bool any() const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return (data[i] || ...);
        }(std::make_index_sequence<W>());
    }

    constexpr bool operator==(const Board& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] == other.data[i]) && ...);
        }(std::make_index_sequence<W>());
    }

    constexpr bool operator!=(const Board& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] != other.data[i]) || ...);
        }(std::make_index_sequence<W>());
    }

    constexpr Board operator~() const {
        Board result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = Tmask & ~data[i]), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr Board& operator|=(const Board& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] |= other.data[i]), ...);
        }(std::make_index_sequence<W>());
        return *this;
    }

    constexpr Board operator|(const Board& other) const {
        Board result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = data[i] | other.data[i]), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr Board& operator&=(const Board& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] &= other.data[i]), ...);
        }(std::make_index_sequence<W>());
        return *this;
    }

    constexpr Board operator&(const Board& other) const {
        Board result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = data[i] & other.data[i]), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr Board& operator^=(const Board& other) {
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((data[i] ^= other.data[i]), ...);
        }(std::make_index_sequence<W>());
        return *this;
    }

    constexpr Board operator^(const Board& other) const {
        Board result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = data[i] ^ other.data[i]), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr Board line_clears() const {
        const T full = static_cast<T>([&]<size_t... i>(std::index_sequence<i...>) {
            return (data[i] & ...);
        }(std::make_index_sequence<W>()));

        Board result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = full), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr void clear_lines(const Board& lines) {
        const T line_bits = lines.data[0];

        [&]<size_t... c>(std::index_sequence<c...>) {
            (([&]{
                constexpr int col = static_cast<int>(c);
                T src = data[col] & ~line_bits;
                T result = 0;
                int dest = 0;

                [&]<size_t... row>(std::index_sequence<row...>) {
                    (([&]{
                        constexpr int r = static_cast<int>(row);
                        if (!(line_bits & (static_cast<T>(1) << r))) {
                            result |= ((src >> r) & static_cast<T>(1)) << dest;
                            ++dest;
                        }
                    }()), ...);
                }(std::make_index_sequence<H>());

                data[col] = result;
            }()), ...);
        }(std::make_index_sequence<W>());
    }

    template <Piece p, Rotation r>
    auto do_move(const int x, const int y) {
        static_assert(p.is_ok() && r.is_ok());
        constexpr PieceCoordinates pc = piece_table<p, r>();

        set(x, y);
        set(x + pc[0].x, y + pc[0].y);
        set(x + pc[1].x, y + pc[1].y);
        set(x + pc[2].x, y + pc[2].y);

        const auto clears = line_clears();
        if (clears.any())
            clear_lines(clears);

        return clears;
    }

    std::string to_string() const {
        constexpr int lines = std::min(20, H);
        constexpr size_t lenR = (W * 8) + 6;
        constexpr size_t lenH = (W * 4) + 4;
        constexpr size_t lenT = (lines * lenR) + lenH;

        std::string output;
        output.reserve(lenT);

        std::string header;
        header.reserve(lenH);
        header = "\n +";
        for (int i = 0; i < W; ++i)
            header += "---+";
        header += "\n";

        output += header;
        for (int y = lines - 1; y >= 0; --y) {
            for (int x = 0; x < W; ++x)
                output += (get(x, y) ? " | #" : " |  ");
            output += " |";
            output += header;
        }

        return output;
    }

    template <int H1>
    constexpr Board<H1> cast_height() const {
        static_assert(BoardBase::is_ok_h(H1));

        Board<H1> result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = static_cast<typename Board<H1>::T>(data[i])), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr int max_y() const {
        int best = 0;
        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                const int idx = std::bit_width(data[i]);
                best = std::max(idx, best);
            }()), ...);
        }(std::make_index_sequence<W>());
        return best;
    }

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result += std::popcount(data[i])), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    template <typename Fn>
    void for_each_set_bit(Fn&& fn) const {
        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                constexpr int x = static_cast<int>(i);
                T bits = data[i];
                while (bits) {
                    const int y = std::countr_zero(bits);
                    fn(x, y);
                    bits &= bits - 1;
                }
            }()), ...);
        }(std::make_index_sequence<W>());
    }
};

} // namespace Cobra

#endif