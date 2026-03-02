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

template <Piece p>
class MoveList {
private:
    static constexpr auto cSize = Gen::canonical_size<p>();

    // Temporary
    static constexpr int SPAWN_X = 4;
    static constexpr int SPAWN_Y = 19;
    static constexpr int T_CAST = p == Piece::I || p == Piece::T ? 3 : 2 - (p == Piece::O);
    static constexpr int T_SPAWN = p == Piece::I ? 3 : 2 - (p == Piece::O);

    template <typename BoardT>
    void generate(const BoardT& b, const int y) {
        static_assert(p.is_ok());

        constexpr auto sSize = p == Piece::O ? 1 : Rotation::size;

        using cSB = Gen::SmearedBoard<BoardT, cSize>;
        using sSB = Gen::SmearedBoard<BoardT, sSize>;

        const cSB usable = Gen::usable_map<BoardT, p>(b);
        const cSB candidates = Gen::landable_map<cSB, p>(usable);
        cSB moves{};
        sSB search;

        std::bitset<cSize> remaining;
        std::bitset<sSize> done = 0;

        constexpr auto ceiling = BoardT::H - T_CAST;
        assert(y < ceiling);

        if (BoardT::H > SPAWN_Y && y > SPAWN_Y - T_SPAWN) [[unlikely]] {
            if (!usable[Rotation::NORTH].template get<SPAWN_X, SPAWN_Y>())
                return;

            search = {};
            search[Rotation::NORTH].template set<SPAWN_X, SPAWN_Y>();

            remaining.set();

        } else {
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                (([&]{
                    constexpr Rotation r(rs);
                    constexpr Rotation rc = Gen::canonical_r<p>(r);
                    BoardT surface = ~usable[rc];
                    surface |= surface.template shifted<0, -1>();
                    surface |= surface.template shifted<0, -2>();

                    if constexpr (ceiling >= 4)
                        surface |= surface.template shifted<0, -4>();
                    if constexpr (ceiling >= 8)
                        surface |= surface.template shifted<0, -8>();
                    if constexpr (ceiling >= 16)
                        surface |= surface.template shifted<0, -16>();
                    // if constexpr (ceiling >= 32) // Shouldn't be needed since it will be routed to slow init
                    //     surface |= surface.template shifted<0, -32>();

                    search[r] = ~surface;
                    search[r] |= (search[r].template shifted<-1, 0>() | search[r].template shifted<1, 0>()) & usable[rc]; // Quick tucks
                    search[r] |= (search[r].template shifted<-1, 0>() | search[r].template shifted<1, 0>()) & usable[rc];
                }()), ...);
            }(std::make_index_sequence<cSize>{});

            if constexpr (Gen::group3(p)) {
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    (([&]{
                        constexpr Rotation r(rs);
                        constexpr Rotation r1 = Gen::rotate<Gen::Direction::CW>(r);
                        constexpr Rotation r2 = Gen::rotate<Gen::Direction::CCW>(r);
                        static_assert(r == Gen::canonical_r<p>(r));

                        search[r] |= (search[r1] | search[r2]) & usable[r];
                    }()), ...);
                }(std::make_index_sequence<cSize>{});
            }

            if constexpr (Gen::group2(p)) {
                search[Rotation::SOUTH] = search[Rotation::NORTH];
                search[Rotation::WEST] = search[Rotation::EAST];
            }

            [&]<size_t... rs>(std::index_sequence<rs...>) {
                ((remaining[rs] = (moves[rs] = search[rs] & candidates[rs]) != candidates[rs]), ...);
            }(std::make_index_sequence<cSize>{});

            if (!remaining.any()) {
                // done.set();
                goto output;
            }
        }

        {
            sSB unsearched;
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                ((unsearched[rs] = ~search[rs] & usable[Gen::canonical_r<p>(Rotation(rs))]), ...);
            }(std::make_index_sequence<sSize>{});

            while (!done.all()) {
                auto bfs = [&]<Rotation r>{
                    if (done[r])
                        return;
                    done.set(r);

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
                            done.set();
                            return;
                        }
                    }

                    // Rotates
                    if constexpr (p != Piece::O) {
                        constexpr size_t kickIndex = (p == Piece::I) ? 1 : 0;

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
                            done.set();
                            return;
                        }
                    }

                    search[r] = BoardT{};
                };
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    (bfs.template operator()<Rotation(rs)>(), ...);
                }(std::make_index_sequence<sSize>{});
            }
        }

        output:

        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((list[rs] = moves[rs].template cast_height<Board<>::H>()), ...);
        }(std::make_index_sequence<cSize>{});
    }

    void generate(const Board<>& b) {
        static constexpr int H1 = 6;
        static constexpr int H2 = 12;
        static constexpr int H3 = 18;
        static constexpr int H4 = 24;

        const int y = b.max_y();
        const int target = [&]{
            const int y1 = y + T_CAST;
            if (y1 < H1)
                return H1;
            if (y1 < H2)
                return H2;
            if (y1 < H3)
                return H3;
            if (y1 < H4)
                return H4;
            return Board<>::H;
        }();

        auto helper = [&]<int H>(const Board<>& b) {
            const auto small = b.template cast_height<H>();
            return generate<Board<H>>(small, y);
        };

        switch (target) {
            case H1: return helper.template operator()<H1>(b);
            case H2: return helper.template operator()<H2>(b);
            case H3: return helper.template operator()<H3>(b);
            case H4: return helper.template operator()<H4>(b);
            default: return generate<Board<>>(b, y);
        }
    }

public:

    Gen::SmearedBoard<Board<>, cSize> list{};

    constexpr MoveList() = default;
    MoveList(const Board<>& b) { generate(b); }

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((result += list[rs].popcount()), ...);
        }(std::make_index_sequence<cSize>{});
        return result;
    }

    template <typename Fn>
    void for_each_move(Fn&& fn) const {
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((list[rs].for_each_set_bit([&](int x, int y) {
                fn.template operator()<Rotation(rs)>(x, y);
            })), ...);
        }(std::make_index_sequence<cSize>{});
    }
};

} // namespace Cobra2

#endif