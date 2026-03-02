#ifndef BOARD_HPP
#define BOARD_HPP

#include "header.hpp"

#include <algorithm>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <utility>

namespace Cobra2 {

template <int Height = ROW_NB>
struct Board {
    static constexpr int W = COL_NB;
    static constexpr int H = Height;

    using U = uint64_t;
    static constexpr int Tbits = std::numeric_limits<U>::digits;
    static constexpr int Tlines = Tbits / W;
    static constexpr int Tn = ((H - 1) / Tlines) + 1;
    static constexpr U Tall = static_cast<U>(-1) >> (Tbits - (Tlines * W));

    using Bitboard [[gnu::vector_size(Tn * sizeof(U))]] = U;
    // static_assert(sizeof(Bitboard) == Tn * sizeof(U));

    Bitboard data;

    // static constexpr Bitboard all() {
    static consteval Bitboard all() {
        Bitboard b{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((b[i] = Tall), ...);
        }(std::make_index_sequence<Tn>());
        return b;
    };

    static constexpr bool is_ok_y_local(const int y) {
        return y >= 0 && y < H;
    }

    template <int x, int y>
    constexpr void set() {
        static_assert(is_ok_x(x) && is_ok_y_local(y));
        data[y / Tlines] |= static_cast<U>(1) << ((y % Tlines) * W + x);
    }

    void set(const int x, const int y) {
        assert(is_ok_x(x) && is_ok_y_local(y));
        data[y / Tlines] |= static_cast<U>(1) << ((y % Tlines) * W + x);
    }

    template <int x, int y>
    constexpr bool get() const {
        static_assert(is_ok_x(x) && is_ok_y_local(y));
        return data[y / Tlines] & (static_cast<U>(1) << ((y % Tlines) * W + x));
    }

    constexpr bool get(const int x, const int y) const {
        assert(is_ok_x(x) && is_ok_y_local(y));
        return data[y / Tlines] & (static_cast<U>(1) << ((y % Tlines) * W + x));
    }

    template <int x>
    // static constexpr Bitboard one_mask() {
    static consteval Bitboard one_mask() {
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
        if constexpr (dx > 0) {
            [&]<size_t... idx>(std::index_sequence<idx...>) {
                (b.set<idx / H, idx % H>(), ...);
            }(std::make_index_sequence<dx * H>());
        } else if constexpr (dx < 0) {
            [&]<size_t... idx>(std::index_sequence<idx...>) {
                (b.set<W - 1 - (idx / H), idx % H>(), ...);
            }(std::make_index_sequence<-dx * H>());
        }
        return (~b).data;
    }

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result += std::popcount(data[i])), ...);
        }(std::make_index_sequence<Tn>());
        return result;
    }

    template<typename Fn>
    void for_each_set_bit(Fn&& fn) const {
        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                U bits = data[i];
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

    constexpr int max_y() const {
        for (int lane = Tn - 1; lane >= 0; --lane) {
            const U bits = data[static_cast<size_t>(lane)];
            if (!bits)
                continue;

            const int idx = (Tbits - 1) - std::countl_zero(bits);
            const int y = (lane * Tlines) + (idx / W);
            assert(y < H);
            return y;
        }

        return 0;
    }

    template<int OtherH>
    constexpr Board<OtherH> cast_height() const {
        Board<OtherH> result{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((result.data[i] = data[i]), ...);
        }(std::make_index_sequence<std::min(Board<OtherH>::Tn, Tn)>());
        return result;
    }

    // constexpr Board populate() const {
    //     return Board{.data = (one_mask<W - 1>() - data) ^ one_mask<W - 1>()};
    // }

    // constexpr Board remove_ones_after_zero() const {
    //     Bitboard b = data | ~all();
    //     bool found = false;
    //     [&]<size_t... i>(std::index_sequence<i...>) {
    //         (([&]{
    //             const size_t idx = Tn - 1 - i;
    //             if (found) {
    //                 b[idx] = 0;
    //             } else {
    //                 const int ones = std::countl_one(b[idx]);
    //                 if (ones < Tbits) {
    //                     found = true;
    //                     b[idx] &= ~((~static_cast<U>(0)) >> ones);
    //                 }
    //             }
    //         }()), ...);
    //     }(std::make_index_sequence<Tn>());
    //     return Board{.data = b & all()};

    //     // Board b{.data = all() & ~data};
    //     // b |= b.template shifted<0, -1>();
    //     // b |= b.template shifted<0, -2>();
    //     // b |= b.template shifted<0, -4>();
    //     // if constexpr (H >= 8)
    //     //     b |= b.template shifted<0, -8>();
    //     // if constexpr (H >= 16)
    //     //     b |= b.template shifted<0, -16>();
    //     // // if constexpr (H >= 32)
    //     // //     b |= b.template shifted<0, -32>();

    //     // return Board{.data = all() & ~b.data};
    // }

    template<int dx, int dy>
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
                            return static_cast<U>(0);
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

    template<int dx, int dy>
    constexpr Board shifted() const {
        Board result = *this;
        result.shift<dx, dy>();
        return result;
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

    constexpr bool any() const {
        return [&]<size_t... i>(std::index_sequence<i...>) {
            return (data[i] || ...);
        }(std::make_index_sequence<Tn>());

        // No measurable difference in speed
        // U temp{};
        // [&]<size_t... i>(std::index_sequence<i...>) {
        //     ((temp |= data[i]), ...);
        // }(std::make_index_sequence<Tn>());
        // return temp;
    }

    constexpr Board operator~() const {
        return Board{.data = all() & ~data};
    }

    constexpr Board& operator|=(const Board& other) {
        data |= other.data;
        return *this;
    }

    constexpr Board& operator&=(const Board& other) {
        data &= other.data;
        return *this;
    }

    constexpr Board& operator^=(const Board& other) {
        data ^= other.data;
        return *this;
    }

    constexpr Board operator|(const Board& other) const {
        return Board{.data = data | other.data};
    }

    constexpr Board operator&(const Board& other) const {
        return Board{.data = data & other.data};
    }

    constexpr Board operator^(const Board& other) const {
        return Board{.data = data ^ other.data};
    }

    constexpr void place(const Move& move) {
        const PieceCoordinates pc = piece_table(move.piece, move.rotation);
        set(move.x, move.y);
        set(move.x + pc[0].x, move.y + pc[0].y);
        set(move.x + pc[1].x, move.y + pc[1].y);
        set(move.x + pc[2].x, move.y + pc[2].y);
    }

    constexpr Board line_clears() const {
        return Board{.data = data & ((data & ~one_mask<W - 1>()) + one_mask<0>()) & one_mask<W - 1>()};
    }

    constexpr void clear_lines(Board lines) {
        assert(lines.any());
        assert(!Board{.data = lines.data & ~one_mask<W - 1>()}.any());

        lines.data >>= (W - 1);
        // lines.shifted<-(W - 1), 0>();
        int prefix[Tn]{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            ((prefix[i + 1] = prefix[i] + std::popcount(lines.data[i])), ...);
        }(std::make_index_sequence<Tn - 1>());

        Bitboard cleared{};
        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                const U ld = data[i];
                const U ll = lines.data[i];
                U packed = 0;
                int dest = 0;

                [&]<size_t... r>(std::index_sequence<r...>) {
                    (([&]{
                        constexpr int src = r * W;
                        constexpr U rowMask = (static_cast<U>(1) << W) - 1;
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
                U result = 0;

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

    template<Piece p, Rotation r>
    void do_move(const int x, const int y) {
        static_assert(is_ok(p) && is_ok(r));
        constexpr PieceCoordinates pc = piece_table(p, r);

        set(x, y);
        set(x + pc[0].x, y + pc[0].y);
        set(x + pc[1].x, y + pc[1].y);
        set(x + pc[2].x, y + pc[2].y);

        const auto clears = line_clears();
        if (clears.any())
            clear_lines(clears);
    }

    template<Piece p>
    void do_move(Board<>& b, const Move& move) {
        assert(is_ok(move.rotation));
        switch(move.rotation) {
            case NORTH: return b.do_move<p, NORTH>(move.x, move.y);
            case EAST:  return b.do_move<p, EAST>(move.x, move.y);
            case SOUTH: return b.do_move<p, SOUTH>(move.x, move.y);
            case WEST:  return b.do_move<p, WEST>(move.x, move.y);
            default: __builtin_unreachable();
        }
    }

    void do_move(const Move& move) {
        assert(is_ok(move.piece));
        switch(move.piece) {
            case I: return do_move<I>(*this, move);
            case O: return do_move<O>(*this, move);
            case T: return do_move<T>(*this, move);
            case L: return do_move<L>(*this, move);
            case J: return do_move<J>(*this, move);
            case S: return do_move<S>(*this, move);
            case Z: return do_move<Z>(*this, move);
            default: __builtin_unreachable();
        }
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
};

} // namespace Cobra2

#endif