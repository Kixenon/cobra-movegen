#ifndef MOVEGEN_HPP
#define MOVEGEN_HPP

#include "header.hpp"
#include "board.hpp"
#include "gen.hpp"
#include "ruleset.hpp"

#include <array>
#include <bitset>
#include <cassert>
#include <concepts>
#include <cstddef>
#include <utility>

namespace Cobra2 {

template <typename RulesT, Piece p, typename BoardT>
requires Ruleset<RulesT> && std::derived_from<BoardT, BoardBase>
class MoveList {
private:
    static constexpr auto cSize = Gen::canonical_size<p>();
    static constexpr auto sSize = Gen::search_size<p>();
    using CSB = Gen::SmearedBoard<BoardT, cSize>;
    using SSB = Gen::SmearedBoard<BoardT, sSize>;

    CSB moves{};

    void generate(const BoardT& b, [[maybe_unused]] const int y) {
        static_assert(p.is_ok());
        constexpr int SPAWN_Y = RulesT::SPAWN_Y;
        constexpr auto ceiling = BoardT::H - p.h_gen();
        assert(y < ceiling);

        const CSB usable = Gen::usable_map<BoardT, p>(b);
        const CSB candidates = Gen::landable_map<CSB, p>(usable);
        SSB search;

        std::bitset<cSize> remaining;
        std::bitset<sSize> done;
        // 500 iq syntax
        do {
            // Slow init
            if constexpr (BoardT::H > SPAWN_Y)
                if (y > SPAWN_Y - p.h_spawn()) {
                    if (!usable[Rotation::NORTH].template get<Gen::SPAWN_X, SPAWN_Y>())
                        return;

                    search = {};
                    search[Rotation::NORTH].template set<Gen::SPAWN_X, SPAWN_Y>();

                    remaining.set();
                    done.set();
                    done.reset(Rotation::NORTH);

                    break;
                }

            // Fast init
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                (([&]{
                    constexpr Rotation r(rs);
                    constexpr Rotation rc = Gen::canonical_r<p>(r);
                    BoardT surface = ~usable[rc];
                    [&]<size_t... i>(std::index_sequence<i...>) {
                        ([&]<int shift>{
                            if constexpr (ceiling >= shift)
                                surface |= surface.template shifted<0, -shift>();
                        }.template operator()<1 << i>(), ...);
                    }(std::make_index_sequence<5>{}); // 1 - 16. 32 isn't needed since that will be routed to slow init

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

            [&]<size_t... rs>(std::index_sequence<rs...>) {
                ((remaining[rs] = (moves[rs] = search[rs] & candidates[rs]) != candidates[rs]), ...);
            }(std::make_index_sequence<cSize>{});

            if (!remaining.any())
                return;

            if constexpr (Gen::group2(p)) {
                search[Rotation::SOUTH] = search[Rotation::NORTH];
                search[Rotation::WEST] = search[Rotation::EAST];
            }
        } while (false);

        // BFS
        {
            SSB unsearched;
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

                        remaining[rc] = (moves[rc] |= search[r] & candidates[rc]) != candidates[rc];
                        if (!remaining.any()) {
                            done.set();
                            return;
                        }
                    }

                    // Rotates
                    if constexpr (p != Piece::O) {
                        constexpr size_t kickIndex = Gen::kick_index<RulesT, p>();
                        constexpr size_t kick180Index = Gen::kick180_index<p>();

                        auto rotate = [&]<Gen::Direction d, const auto& kickTable>{
                            constexpr Rotation r1 = Gen::rotate<d>(r);
                            constexpr Rotation r1c = Gen::canonical_r<p>(r1);
                            constexpr auto off = Gen::canonical_offset<p>(r) - Gen::canonical_offset<p>(r1);
                            constexpr size_t kickSize = Gen::kick_size<RulesT, d, kickTable[r].size()>();

                            BoardT temp = search[r];
                            BoardT result{};

                            [&]<size_t... i>(std::index_sequence<i...>) {
                                ([&]{
                                    constexpr auto kick = kickTable[r][i] + off;
                                    result |= temp.template shifted<kick.x, kick.y>();
                                    if constexpr (i != sizeof...(i) - 1)
                                        temp &= ~(usable[r1c].template shifted<-kick.x, -kick.y>());
                                }(), ...);
                            }(std::make_index_sequence<kickSize>{});

                            result &= unsearched[r1];
                            if (result.any()) {
                                search[r1] |= result;
                                unsearched[r1] &= ~result;
                                // unsearched[r1] ^= result; // Why is this 20% slower?
                                done.reset(r1);

                                remaining[r1c] = (moves[r1c] |= result & candidates[r1c]) != candidates[r1c];
                            }
                        };
                        rotate.template operator()<Gen::Direction::CW, Gen::kicks[kickIndex][Gen::Direction::CW]>();
                        rotate.template operator()<Gen::Direction::CCW, Gen::kicks[kickIndex][Gen::Direction::CCW]>();
                        if constexpr (RulesT::ENABLE_180)
                            rotate.template operator()<Gen::Direction::FLIP, Gen::kicks180[kick180Index]>();

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
    }

public:
    MoveList(const BoardT& b, const int y) { generate(b, y); }

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((result += moves[rs].popcount()), ...);
        }(std::make_index_sequence<cSize>{});
        return result;
    }

    template <typename Fn>
    void for_each_move(Fn&& fn) const {
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((moves[rs].for_each_set_bit([&](int x, int y) {
                fn.template operator()<Rotation(rs)>(x, y);
            })), ...);
        }(std::make_index_sequence<cSize>{});
    }
};

} // namespace Cobra2

#endif