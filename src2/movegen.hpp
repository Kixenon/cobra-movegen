#ifndef MOVEGEN_HPP
#define MOVEGEN_HPP

#include "header.hpp"
#include "board.hpp"
#include "gen.hpp"

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>

namespace Cobra2 {

struct MoveList;

MoveList generate(const Board<>& b, Piece p);

struct MoveList {
    Gen::SmearedBoard<Board<>, ROTATION_NB> board;

    constexpr MoveList() = default;
    MoveList(const Board<>& b, const Piece p) : board(generate(b, p).board) {}

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((result += board[rs].popcount()), ...);
        }(std::make_index_sequence<std::tuple_size_v<decltype(board)>>{});
        return result;
    }

    template<typename Fn>
    void for_each_move(const Piece p, Fn&& fn) const {
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((board[rs].for_each_set_bit([&](int x, int y) {
                fn(Move{p, static_cast<Rotation>(rs), x, y});
            })), ...);
        }(std::make_index_sequence<ROTATION_NB>{});
    }
};

namespace Movegen {

// Temporary
constexpr int SPAWN_X = 4;
constexpr int SPAWN_Y = 19;
constexpr int THRESHOLD = 3;
constexpr int H1 = 6;
constexpr int H2 = 12;
constexpr int H3 = 24;

// Stats
// size_t counter = 0;
// size_t counterA = 0;
// size_t counterB = 0;

// size_t counterS = 0;
// size_t counterSA = 0;
// size_t counterF = 0;

// size_t counterL = 0;
// size_t counterLI = 0;

template <typename BoardT, Piece p>
MoveList generate(const BoardT& b, const int y) {
    static_assert(is_ok(p));
    assert(y < BoardT::H - THRESHOLD);

    // ++counter;

    constexpr int sSize = p == O ? 1 : ROTATION_NB;
    constexpr int cSize = Gen::canonical_size<p>();

    using cSB = Gen::SmearedBoard<BoardT, cSize>;
    using sSB = Gen::SmearedBoard<BoardT, sSize>;

    const cSB usable = Gen::usable_map<BoardT, p>(b);
    sSB search;

    const cSB candidates = Gen::landable_map<cSB, p>(usable);
    cSB moves{};

    std::bitset<cSize> remaining;
    std::bitset<sSize> done = 0;
    // unsigned int done = 0;
    // unsigned int remaining = (1u << sSize) - 1;

    // if (y > SPAWN_Y - THRESHOLD) [[unlikely]] {
    if (BoardT::H > SPAWN_Y && y > SPAWN_Y - THRESHOLD) [[unlikely]] {
        // ++counterS;
        constexpr int spawnY = std::min(SPAWN_Y, BoardT::H - 1);
        if (!usable[NORTH].template get<SPAWN_X, spawnY>())
            return {};

        // ++counterSA;
        search = {};
        search[NORTH].template set<SPAWN_X, spawnY>();

        remaining.set();

    } else {
        // ++counterF;
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            (([&]{
                constexpr Rotation r = static_cast<Rotation>(rs);
                constexpr Rotation rc = Gen::canonical_r<p>(r);
                BoardT surface = ~usable[rc];
                surface |= surface.template shifted<0, -1>();
                surface |= surface.template shifted<0, -2>();

                if constexpr (BoardT::H - THRESHOLD >= 4)
                    surface |= surface.template shifted<0, -4>();
                if constexpr (BoardT::H - THRESHOLD >= 8)
                    surface |= surface.template shifted<0, -8>();
                if constexpr (BoardT::H - THRESHOLD >= 16)
                    surface |= surface.template shifted<0, -16>();
                // if constexpr (BoardT::H - THRESHOLD >= 32) // Shouldn't be needed since it will be routed to slow init
                //     surface |= surface.template shifted<0, -32>();

                BoardT sky = ~surface;
                sky |= (sky.template shifted<-1, 0>() | sky.template shifted<1, 0>()) & usable[rc]; // Quick tucks
                sky |= (sky.template shifted<-1, 0>() | sky.template shifted<1, 0>()) & usable[rc];

                // const BoardT sky = [&]{
                //     const BoardT current = usable[rc] & usable[rc].template shifted<0, -1>();
                //     const BoardT covered = usable[rc] & ~current;
                //     const BoardT starts = covered & current.template shifted<-1, 0>();
                //     const BoardT ends = covered & current.template shifted<1, 0>();
                //     const BoardT heads = covered & ~covered.template shifted<-1, 0>();
                //     const BoardT expandable = starts | BoardT{.data = ends.data + (covered & ~heads).data};

                //     const BoardT whole = expandable | ~heads;
                //     const BoardT clears = whole.line_clears().template shift<-(BoardT::W - 1), 0>();
                //     // const BoardT wholes = clears.populate() | ~((~BoardT{}).template shift<0, -3>());
                //     const BoardT wholes = clears.populate() | (~BoardT{}).template shift<0, BoardT::H - 3>();
                //     // BoardT wholes = (~BoardT{}).template shift<0, BoardT::H - 3>();

                //     return wholes.remove_ones_after_zero() & usable[rc];
                // }();
                // asm volatile("" : : "m"(wholes) : "memory");

                search[r] = sky;
            }()), ...);
        }(std::make_index_sequence<cSize>{});

        if constexpr (Gen::group3(p)) {
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                (([&]{
                    constexpr Rotation r = static_cast<Rotation>(rs);
                    constexpr Rotation r1 = Gen::rotate<Gen::Direction::CW>(r);
                    constexpr Rotation r2 = Gen::rotate<Gen::Direction::CCW>(r);
                    static_assert(r == Gen::canonical_r<p>(r));

                    search[r] |= (search[r1] | search[r2]) & usable[r];
                }()), ...);
            }(std::make_index_sequence<cSize>{});
        }

        if constexpr (Gen::group2(p)) {
            search[SOUTH] = search[NORTH];
            search[WEST] = search[EAST];
        }

        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((remaining[rs] = (moves[rs] = search[rs] & candidates[rs]) != candidates[rs]), ...);
        }(std::make_index_sequence<cSize>{});

        if (!remaining.any()) {
            // ++counterA;
            // done.set();
            goto output;
        }
    }

    {
        sSB unsearched;
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((unsearched[rs] = ~search[rs] & usable[Gen::canonical_r<p>(static_cast<Rotation>(rs))]), ...);
        }(std::make_index_sequence<sSize>{});

        while (!done.all()) {
            // ++counterL;
            auto bfs = [&]<Rotation r>{
                if (done[r])
                    return;
                done.set(r);

                // ++counterLI;

                assert(r < sSize);
                assert(search[r].any());

                const Rotation rc = Gen::canonical_r<p>(r);

                // Shifts
                {
                    while (true) {
                        const BoardT temp = ( search[r].template shifted<-1,  0>()
                                            | search[r].template shifted< 1,  0>()
                                            | search[r].template shifted< 0, -1>() ) & unsearched[r];

                        if (!temp.any())
                            break;

                        search[r] |= temp;
                        unsearched[r] ^= temp;
                    }
                }

                // Harddrop
                {
                    remaining[rc] = (moves[rc] |= search[r] & candidates[rc]) != candidates[rc];
                    if (!remaining.any()) {
                        // ++counterB;
                        done.set();
                        return;
                    }
                }

                // Rotates
                if constexpr (p != O) {
                    constexpr size_t kickIndex = (p == I) ? 1 : 0;

                    auto process_rotation = [&]<Gen::Direction d, const auto& kickTable>() {
                        constexpr Rotation r1 = Gen::rotate<d>(r);
                        constexpr Rotation r1c = Gen::canonical_r<p>(r1);
                        constexpr auto off = Gen::canonical_offset<p>(r) - Gen::canonical_offset<p>(r1);

                        BoardT temp = search[r];
                        BoardT result{};

                        [&]<size_t... i>(std::index_sequence<i...>) {
                            ([&]{
                                constexpr auto kick = kickTable[r][i] + off;
                                result |= temp.template shifted<kick.x, kick.y>();
                                if constexpr (i != sizeof...(i) - 1)
                                    temp &= ~(usable[r1c].template shifted<-kick.x, -kick.y>());
                            }(), ...);
                        }(std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<decltype(kickTable[r])>>>{});

                        result &= unsearched[r1];
                        if (result.any()) {
                            search[r1] |= result;
                            unsearched[r1] &= ~result;
                            // unsearched[r1] ^= result; // Why is this 20% slower?
                            done.reset(r1);

                            remaining[r1c] = (moves[r1c] |= result & candidates[r1c]) != candidates[r1c];
                        }
                    };
                    process_rotation.template operator()<Gen::Direction::CW, Gen::kicks[kickIndex][Gen::Direction::CW]>();
                    process_rotation.template operator()<Gen::Direction::CCW, Gen::kicks[kickIndex][Gen::Direction::CCW]>();

                    if (!remaining.any()) {
                        // ++counterB;
                        done.set();
                        return;
                    }
                }

                search[r] = BoardT{};
            };
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                (bfs.template operator()<static_cast<Rotation>(rs)>(), ...);
            }(std::make_index_sequence<sSize>{});
        }
    }

    output:

    MoveList result{};
    [&]<size_t... rs>(std::index_sequence<rs...>) {
        ((result.board[rs] = moves[rs].template cast_height<Board<>::H>()), ...);
    }(std::make_index_sequence<cSize>{});
    return result;
}

template <int H, Piece p>
MoveList generate(const Board<>& b, const int y) {
    const auto small = b.template cast_height<H>();
    return generate<Board<H>, p>(small, y);
}

template <Piece p>
MoveList generate(const Board<>& b) {
    const int y = b.max_y();
    const int target = [&]{
        const int y1 = y + THRESHOLD;
        if (y1 < H1)
            return H1;
        if (y1 < H2)
            return H2;
        if (y1 < H3)
            return H3;
        return Board<>::H;
    }();

    switch (target) {
        case H1: return generate<H1, p>(b, y);
        case H2: return generate<H2, p>(b, y);
        case H3: return generate<H3, p>(b, y);
        default: return generate<Board<>, p>(b, y);
    }
}

} // namespace Movegen

inline MoveList generate(const Board<>& b, Piece p) {
    switch(p) {
        case I: return Movegen::generate<I>(b);
        case O: return Movegen::generate<O>(b);
        case T: return Movegen::generate<T>(b);
        case L: return Movegen::generate<L>(b);
        case J: return Movegen::generate<J>(b);
        case S: return Movegen::generate<S>(b);
        case Z: return Movegen::generate<Z>(b);
        default: __builtin_unreachable();
    }
}

} // namespace Cobra2

#endif