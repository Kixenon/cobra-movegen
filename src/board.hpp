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

constexpr std::array Y = {16, 32};
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

    using T = std::conditional_t<H <= 16, uint16_t, std::conditional_t<H <= 32, uint32_t, uint64_t>>;

    static constexpr T bb_low(const int i) {
        return static_cast<T>((1ULL << i) - 1);
    }

    static constexpr T Tall = bb_low(H);

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

    template <int dx, int dy>
    constexpr Board shift() const {
        if constexpr (dx >= W || dx <= -W || dy >= H || dy <= -H)
            return {};

        if constexpr (dx == 0 && dy == 0)
            return *this;

        Board b{};

        if constexpr (dx >= 0) {
            [&]<size_t... i>(std::index_sequence<i...>) {
                if constexpr (dy > 0)
                    ((b.data[i + dx] = static_cast<T>(data[i] << dy)), ...);
                else if constexpr (dy < 0)
                    ((b.data[i + dx] = static_cast<T>(data[i] >> -dy)), ...);
                else
                    ((b.data[i + dx] = data[i]), ...);
            }(std::make_index_sequence<W - dx>());
        } else {
            constexpr int adx = -dx;
            [&]<size_t... i>(std::index_sequence<i...>) {
                if constexpr (dy > 0)
                    ((b.data[i] = static_cast<T>(data[i + adx] << dy)), ...);
                else if constexpr (dy < 0)
                    ((b.data[i] = static_cast<T>(data[i + adx] >> -dy)), ...);
                else
                    ((b.data[i] = data[i + adx]), ...);
            }(std::make_index_sequence<W - adx>());
        }

        return b;
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
            ((result.data[i] = Tall & ~data[i]), ...);
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

    constexpr T line_clears() const {
        return static_cast<T>([&]<size_t... i>(std::index_sequence<i...>) {
            return (data[i] & ...);
        }(std::make_index_sequence<W>()));
    }

    constexpr void clear_lines(const T lines) {
        assert(lines);
        [&]<size_t... xs>(std::index_sequence<xs...>) {
            (([&]{
                T src = data[xs] & ~lines;
                T result = 0;
                int dest = 0;

                [&]<size_t... ys>(std::index_sequence<ys...>) {
                    (([&]{
                        if (!(lines & (static_cast<T>(1) << ys))) {
                            result |= ((src >> ys) & static_cast<T>(1)) << dest;
                            ++dest;
                        }
                    }()), ...);
                }(std::make_index_sequence<H>());

                data[xs] = result;
            }()), ...);
        }(std::make_index_sequence<W>());

        // Possible alternative, slightly slower in testing but might be noise
        // do {
        //     const T mask = static_cast<T>(~((lines & -lines) - 1));
        //     [&]<size_t... xs>(std::index_sequence<xs...>) {
        //         ((data[xs] = static_cast<T>(data[xs] ^ ((data[xs] ^ (data[xs] >> 1)) & mask))), ...);
        //     }(std::make_index_sequence<W>());
        // } while ((lines = static_cast<T>((lines & (lines - 1)) >> 1)));
    }

    template <Piece p, Rotation r>
    T do_move(const int x, const int y) {
        static_assert(p.is_ok() && r.is_ok());
        constexpr PieceCoordinates pc = piece_table<p, r>();

        set(x, y);
        set(x + pc[0].x, y + pc[0].y);
        set(x + pc[1].x, y + pc[1].y);
        set(x + pc[2].x, y + pc[2].y);

        const auto clears = line_clears();
        if (clears)
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
            ((result.data[i] = static_cast<Board<H1>::T>(data[i])), ...);
        }(std::make_index_sequence<W>());
        return result;
    }

    constexpr int max_y() const {
        T tmp = 0;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((tmp |= data[i]), ...);
        }(std::make_index_sequence<W>());
        return std::bit_width(tmp);
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
                    fn(x, std::countr_zero(bits));
                    bits &= bits - 1;
                }
            }()), ...);
        }(std::make_index_sequence<W>());
    }
};

} // namespace Cobra

#endif