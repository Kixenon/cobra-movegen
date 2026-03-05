#ifndef BOARD_HPP
#define BOARD_HPP

#include "header.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <utility>

namespace Cobra2 {

struct BoardBase {
    static constexpr int W = COL_NB;

    using T = uint64_t;
    static constexpr int Tbits = std::numeric_limits<T>::digits;
    static constexpr int Tlines = Tbits / W;
    static constexpr T Tall = static_cast<T>(-1) >> (Tbits - (Tlines * W));

    static constexpr std::array Y = {6, 12, 18, 24};
    static constexpr int H = ROW_NB;

    static constexpr bool is_ok_h(const int h) {
        return std::ranges::contains(Y, h) || h == H;
    }

    static constexpr int height_target(const int h) {
        #pragma unroll Y.size()
        for (const auto i : Y)
            if (h < i)
                return i;
        return H;
    }

    template <typename Fn>
    static constexpr auto route(const int h, Fn&& fn) {
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
};

template <int Height = ROW_NB>
struct Board : public BoardBase {
    static constexpr int H = Height;
    static constexpr int Tn = ((H - 1) / Tlines) + 1;

    using Bitboard [[gnu::vector_size(Tn * sizeof(T))]] = T;
    // static_assert(sizeof(Bitboard) == Tn * sizeof(T));

    Bitboard data;

    static constexpr int height_target(const int h) = delete;
    static constexpr auto route(const int h) = delete;

    static constexpr bool is_ok_y_local(const int y) {
        return y >= 0 && y < H;
    }

    template <int x, int y>
    constexpr void set() {
        static_assert(is_ok_x(x) && is_ok_y_local(y));
        data[y / Tlines] |= static_cast<T>(1) << ((y % Tlines) * W + x);
    }

    void set(const int x, const int y) {
        assert(is_ok_x(x) && is_ok_y_local(y));
        data[y / Tlines] |= static_cast<T>(1) << ((y % Tlines) * W + x);
    }

    template <int x, int y>
    constexpr bool get() const {
        static_assert(is_ok_x(x) && is_ok_y_local(y));
        return data[y / Tlines] & (static_cast<T>(1) << ((y % Tlines) * W + x));
    }

    constexpr bool get(const int x, const int y) const {
        assert(is_ok_x(x) && is_ok_y_local(y));
        return data[y / Tlines] & (static_cast<T>(1) << ((y % Tlines) * W + x));
    }

    // static constexpr Bitboard all() {
    static consteval Bitboard all() {
        Bitboard b{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((b[i] = Tall), ...);
        }(std::make_index_sequence<Tn>());
        return b;
    };

    template <int x>
    // static constexpr Bitboard col_mask() {
    static consteval Bitboard col_mask() {
        Board b{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            (b.set<x, i>(), ...);
        }(std::make_index_sequence<H>());
        return b.data;
    }

    template <int dx>
    // static constexpr Bitboard shift_mask() {
    static consteval Bitboard shift_mask() {
        Board b{};
        if constexpr (dx > 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                (b.set<i / Tlines, i % Tlines>(), ...);
            }(std::make_index_sequence<dx * Tlines>());
        else if constexpr (dx < 0)
            [&]<size_t... i>(std::index_sequence<i...>) {
                (b.set<W - 1 - (i / Tlines), i % Tlines>(), ...);
            }(std::make_index_sequence<-dx * Tlines>());

        [&]<size_t... i>(std::index_sequence<i...>) {
            ((b.data[i + 1] = b.data[0]), ...);
        }(std::make_index_sequence<Tn - 1>());

        return (~b).data;
    }

    template <int dx, int dy>
    constexpr Board& shift() {
        if constexpr (dx == 0 && dy == 0)
            return *this;

        constexpr auto split_helper = [&]<int removed, bool right>(Bitboard bb) {
            Bitboard result;
            [&]<size_t... i>(std::index_sequence<i...>) {
                ((
                    result[i] = [&]{
                        constexpr size_t index = right ? i - removed : i + removed;
                        if constexpr (index >= Tn)
                            return static_cast<T>(0);
                        else
                            return bb[index];
                    }()
                ), ...);
            }(std::make_index_sequence<Tn>());
            return result;
        };

        constexpr auto shift_helper = [&]<int ddx, int ddy>(Bitboard bb) {
            if constexpr (ddy == Tlines || ddy == -Tlines)
                bb = Bitboard{};
            else if constexpr (ddy < 0)
                bb >>= -ddy * W;
            else if constexpr (ddy > 0)
                bb <<= ddy * W;

            if constexpr (ddx > 0)
                bb <<= ddx;
            else if constexpr (ddx < 0)
                bb >>= -ddx;

            return bb;
        };

        if constexpr (dy == 0) {
            if constexpr (dx > 0)
                data <<= dx;
            else if constexpr (dx < 0)
                data >>= -dx;
        } else if constexpr (dy > 0) {
            constexpr int pad = (dy - 1) / Tlines;
            constexpr int shift = ((dy - 1) % Tlines) + 1;
            auto unmoved = split_helper.template operator()<pad, true>(shift_helper.template operator()<dx, shift>(data));
            auto moved = split_helper.template operator()<pad + 1, true>(shift_helper.template operator()<dx, shift - Tlines>(data));
            data = (unmoved | moved); // & all();
        } else if constexpr (dy < 0) {
            constexpr int pad = (-dy - 1) / Tlines;
            constexpr int shift = ((-dy - 1) % Tlines) + 1;
            auto unmoved = split_helper.template operator()<pad, false>(shift_helper.template operator()<dx, -shift>(data));
            auto moved = split_helper.template operator()<pad + 1, false>(shift_helper.template operator()<dx, Tlines - shift>(data));
            data = (unmoved | moved); // & all();
        }

        if constexpr (dx != 0)
            data &= shift_mask<dx>();
        else if constexpr (dy != 0)
            data &= all();

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
        }(std::make_index_sequence<Tn>());

        // No measurable difference in speed
        // T temp{};
        // [&]<size_t... i>(std::index_sequence<i...>) {
        //     ((temp |= data[i]), ...);
        // }(std::make_index_sequence<Tn>());
        // return temp;
    }

    constexpr bool operator==(const Board& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] == other.data[i]) && ...);
        }(std::make_index_sequence<Tn>());
    }

    constexpr bool operator!=(const Board& other) const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return ((data[i] != other.data[i]) || ...);
        }(std::make_index_sequence<Tn>());
    }

    constexpr Board operator~() const {
        return Board{.data = all() & ~data};
    }

    constexpr Board& operator|=(const Board& other) {
        data |= other.data;
        return *this;
    }

    constexpr Board operator|(const Board& other) const {
        return Board{.data = data | other.data};
    }

    constexpr Board& operator&=(const Board& other) {
        data &= other.data;
        return *this;
    }

    constexpr Board operator&(const Board& other) const {
        return Board{.data = data & other.data};
    }

    constexpr Board& operator^=(const Board& other) {
        data ^= other.data;
        return *this;
    }

    constexpr Board operator^(const Board& other) const {
        return Board{.data = data ^ other.data};
    }

    constexpr Board line_clears() const {
        return Board{.data = data & ((data & ~col_mask<W - 1>()) + col_mask<0>()) & col_mask<W - 1>()};
    }

    constexpr void clear_lines(Board lines) {
        assert(lines.any());
        assert(!Board{.data = lines.data & ~col_mask<W - 1>()}.any());

        lines.data >>= (W - 1);
        // lines.shifted<-(W - 1), 0>();
        int prefix[Tn]{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((prefix[i + 1] = prefix[i] + std::popcount(lines.data[i])), ...);
        }(std::make_index_sequence<Tn - 1>());

        Bitboard cleared{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                const T ld = data[i];
                const T ll = lines.data[i];
                T packed = 0;
                int dest = 0;

                [&]<size_t... r>(std::index_sequence<r...>) {
                    (([&]{
                        constexpr int src = r * W;
                        constexpr T rowMask = (static_cast<T>(1) << W) - 1;
                        if (((ll >> src) & 1) == 0) {
                            packed |= ((ld >> src) & rowMask) << dest;
                            dest += W;
                        }
                    }()), ...);
                }(std::make_index_sequence<Tlines>());

                cleared[i] = packed;
            }()), ...);
        }(std::make_index_sequence<Tn>());

        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                constexpr int dest = static_cast<int>(i);
                T result = 0;

                [&]<size_t... j>(std::index_sequence<j...>) {
                    (([&]{
                        constexpr int src = static_cast<int>(j);

                        if constexpr (src >= dest) {
                            const int relative = ((src - dest) * Tlines) - prefix[src];
                            if (relative >= 0 && relative < Tlines)
                                result |= cleared[src] << (relative * W);
                            else if (relative < 0 && relative > -Tlines)
                                result |= cleared[src] >> (-relative * W);
                        }
                    }()), ...);
                }(std::make_index_sequence<Tn>());

                data[dest] = result & Tall;
            }()), ...);
        }(std::make_index_sequence<Tn>());
    }

    template <Piece p, Rotation r>
    void do_move(const int x, const int y) {
        static_assert(p.is_ok() && r.is_ok());
        constexpr PieceCoordinates pc = piece_table<p, r>();

        set(x, y);
        set(x + pc[0].x, y + pc[0].y);
        set(x + pc[1].x, y + pc[1].y);
        set(x + pc[2].x, y + pc[2].y);

        const auto clears = line_clears();
        if (clears.any())
            clear_lines(clears);
    }

    // std::string to_string() const {
    //     constexpr int lines = std::min(20, H);
    //     constexpr size_t lenR = (W * 8) + 6;
    //     constexpr size_t lenH = (W * 4) + 4;

    //     std::string output;
    //     output.reserve((lines * lenR) + lenH);

    //     std::string header = "\n +";
    //     for (int i = 0; i < W; ++i) header += "---+";
    //     header += "\n";

    //     output += header;
    //     auto output_row = [&]<int y>() {
    //         [&]<size_t... x>(std::index_sequence<x...>) {
    //             ((output += (get<x, y>()? " | #" : " |  ")), ...);
    //         }(std::make_index_sequence<W>());
    //         output += " |";
    //         output += header;
    //     };
    //     [&]<size_t... y>(std::index_sequence<y...>) {
    //         (output_row.template operator()<lines - y - 1>(), ...);
    //     }(std::make_index_sequence<lines>());
    //     return output;
    // }

    // std::string to_string(const Move& move) const {
    //     std::string output = to_string();
    //     constexpr int lines = std::min(20, H);
    //     constexpr size_t lenR = (W * 8) + 6;
    //     constexpr size_t lenH = (W * 4) + 4;

    //     const PieceCoordinates pc = piece_table(move.piece, move.rotation);
    //     for (size_t i = 0; i < 4; ++i) {
    //         const int inverseY = lines - (i == 0 ? move.y : pc[i - 1].y + move.y) - 1;
    //         if (inverseY < 0)
    //             continue;
    //         const size_t idx = static_cast<size_t>((inverseY * lenR) + ((i == 0 ? move.x : pc[i - 1].x + move.x) * 4) + lenH + 3);
    //         if (output[idx] == ' ')
    //             output[idx] = '.';
    //     }
    //     return output;
    // }

    template <int H1>
    constexpr Board<H1> cast_height() const {
        static_assert(is_ok_h(H1));

        Board<H1> result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = data[i]), ...);
        }(std::make_index_sequence<std::min(Board<H1>::Tn, Tn)>());
        return result;
    }

    constexpr int max_y() const {
        #pragma unroll Tn
        for (int lane = Tn - 1; lane >= 0; --lane) {
            const T bits = data[static_cast<size_t>(lane)];
            if (!bits)
                continue;

            const int idx = (Tbits - 1) - std::countl_zero(bits);
            const int y = (lane * Tlines) + (idx / W);
            assert(y < H);
            return y;
        }
        return 0;
    }

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result += std::popcount(data[i])), ...);
        }(std::make_index_sequence<Tn>());
        return result;
    }

    template <typename Fn>
    void for_each_set_bit(Fn&& fn) const {
        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                T bits = data[i];
                while (bits) {
                    const int idx = std::countr_zero(bits);
                    const int y = (static_cast<int>(i) * Tlines) + (idx / W);
                    const int x = idx % W;
                    fn(x, y);
                    bits &= bits - 1;
                }
            }()), ...);
        }(std::make_index_sequence<Tn>());
    }
};

} // namespace Cobra2

#endif