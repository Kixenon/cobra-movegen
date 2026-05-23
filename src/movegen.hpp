#ifndef MOVEGEN_HPP
#define MOVEGEN_HPP

#include "header.hpp"
#include "gen.hpp"
#include "ruleset.hpp"

#include <array>
#include <bit>
#include <bitset>
#include <cassert>
#include <cstddef>
#include <utility>

namespace Cobra {

template <typename RulesT, Piece p, typename BoardT>
requires Ruleset<RulesT>
class MoveList {
private:
    static constexpr bool checkTspin = (p == Piece::T) && (RulesT::SPINS == Policy::SpinRule::TSPIN);
    static constexpr auto sMul = checkTspin ? SpinType::size : 1;
    static constexpr auto cSize = Gen::canonical_size<p>();
    static constexpr auto sSize = Gen::search_size<p>();
    using CSB = Gen::SmearedBoard<BoardT, cSize>;
    using SSB = Gen::SmearedBoard<BoardT, sSize>;

    std::array<CSB, sMul> moves{};

    void generate(const BoardT& b, [[maybe_unused]] const int y, [[maybe_unused]] const int force) {
        static_assert(p.is_ok());
        assert(y < BoardT::H - p.h_gen());

        const auto usable = Gen::usable_map<BoardT, p>(b);
        const auto candidates = Gen::landable_map<CSB, p>(usable);

        SSB search;

        std::bitset<cSize> remaining;
        std::bitset<sSize> done;
        // 500 iq syntax
        do {
            // Slow init
            if constexpr (BoardT::H > RulesT::SPAWN_Y)
                if (y > RulesT::SPAWN_Y - p.h_spawn()) {
                    assert(force >= 0);
                    const int threshold = std::min(RulesT::SPAWN_Y + force + 1, BoardT::H);
                    const int spawn = [&]{
                        int s = RulesT::SPAWN_Y;
                        for (; s < threshold && !usable[Rotation::NORTH].get(Gen::SPAWN_X, s); ++s);
                        return s;
                    }();

                    if (spawn == threshold)
                        return;

                    assert(spawn < BoardT::H);
                    search = {};
                    search[Rotation::NORTH].set(Gen::SPAWN_X, spawn);

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

                    [&]<size_t... xs>(std::index_sequence<xs...>) {
                        (([&]{
                            using CT = BoardT::T;
                            const CT blocked = static_cast<CT>(BoardT::Tall & ~usable[rc].data[xs]);
                            const int inv = BoardT::H - std::bit_width(blocked);
                            const CT fill = BoardT::Tall >> inv;
                            search[r].data[xs] = static_cast<CT>(BoardT::Tall ^ fill);
                        }()), ...);
                    }(std::make_index_sequence<BoardT::W>());

                    search[r] |= (search[r].template shift<-1, 0>() | search[r].template shift<1, 0>()) & usable[rc]; // Quick tucks
                    search[r] |= (search[r].template shift<-1, 0>() | search[r].template shift<1, 0>()) & usable[rc];
                }()), ...);
            }(std::make_index_sequence<cSize>());

            if constexpr (Gen::group3(p)) {
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    (([&]{
                        constexpr Rotation r(rs);
                        constexpr Rotation r1 = Gen::rotate<Gen::Direction::CW>(r);
                        constexpr Rotation r2 = Gen::rotate<Gen::Direction::CCW>(r);
                        static_assert(r == Gen::canonical_r<p>(r));

                        search[r] |= (search[r1] | search[r2]) & usable[r];
                    }()), ...);
                }(std::make_index_sequence<cSize>());
            }

            [&]<size_t... rs>(std::index_sequence<rs...>) {
                ((remaining[rs] = (moves[0][rs] = search[rs] & candidates[rs]) != candidates[rs]), ...);
            }(std::make_index_sequence<cSize>());

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
            }(std::make_index_sequence<sSize>());

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
                            const BoardT temp = ( search[r].template shift<-1,  0>()
                                                | search[r].template shift< 1,  0>()
                                                | search[r].template shift< 0, -1>() ) & unsearched[r];

                            if (!temp.any())
                                break;

                            search[r] |= temp;
                            unsearched[r] ^= temp;
                        }

                        remaining[rc] = (moves[0][rc] |= search[r] & candidates[rc]) != candidates[rc];
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
                                    result |= temp.template shift<kick.x, kick.y>();
                                    if constexpr (i != sizeof...(i) - 1)
                                        temp &= ~(usable[r1c].template shift<-kick.x, -kick.y>());
                                }(), ...);
                            }(std::make_index_sequence<kickSize>());

                            result &= unsearched[r1];
                            if (result.any()) {
                                search[r1] |= result;
                                unsearched[r1] &= ~result;
                                // unsearched[r1] ^= result; // Why is this 20% slower?
                                done.reset(r1);

                                remaining[r1c] = (moves[0][r1c] |= result & candidates[r1c]) != candidates[r1c];
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
                }(std::make_index_sequence<sSize>());
            }
        }
    }

    void generate_tspins(const BoardT& b, const int y, const int force) requires checkTspin {
        static_assert(cSize == Rotation::size);
        static_assert(sSize == Rotation::size);

        SSB corners;
        corners[Rotation::NORTH] = ~(~b).template shift< 1, -1>();
        corners[Rotation::EAST ] = ~(~b).template shift<-1, -1>();
        corners[Rotation::SOUTH] = ~(~b).template shift<-1,  1>();
        corners[Rotation::WEST ] = ~(~b).template shift< 1,  1>();

        const BoardT spins = (
            (corners[0] & corners[1] & (corners[2] | corners[3])) |
            (corners[2] & corners[3] & (corners[0] | corners[1]))
        );

        const auto usable = Gen::usable_map<BoardT, p>(b);
        const auto candidates = Gen::landable_map<CSB, p>(usable);

        if (![&]<size_t... rs>(std::index_sequence<rs...>) {
            return (spins & (candidates[rs] | ...)).any();
        }(std::make_index_sequence<sSize>()))
            return generate(b, y, force);

        SSB spinMap;
        [&]<size_t... rs>(std::index_sequence<rs...>) {
            ((spinMap[rs] = spins & corners[rs] & corners[Gen::rotate<Gen::Direction::CW>(Rotation(rs))]), ...);
        }(std::make_index_sequence<sSize>());

        SSB search;
        std::array<SSB, SpinType::size> spinReach{};

        std::bitset<sSize> done;
        do {
            // Slow init
            if constexpr (BoardT::H > RulesT::SPAWN_Y)
                if (y > RulesT::SPAWN_Y - p.h_spawn()) {
                    assert(force >= 0);
                    const int threshold = std::min(RulesT::SPAWN_Y + force + 1, BoardT::H);
                    const int spawn = [&] {
                        int s = RulesT::SPAWN_Y;
                        for (; s < threshold && !usable[Rotation::NORTH].get(Gen::SPAWN_X, s); ++s);
                        return s;
                    }();

                    if (spawn == threshold)
                        return;

                    assert(spawn < BoardT::H);
                    search = {};
                    search[Rotation::NORTH].set(Gen::SPAWN_X, spawn);
                    spinReach[SpinType::NONE][Rotation::NORTH].set(Gen::SPAWN_X, spawn);

                    done.set();
                    done.reset(Rotation::NORTH);

                    break;
                }

            // Fast init
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                (([&] {
                    constexpr Rotation r(rs);
                    constexpr Rotation rc = Gen::canonical_r<p>(r);

                    [&]<size_t... xs>(std::index_sequence<xs...>) {
                        (([&]{
                            using CT = BoardT::T;
                            const CT blocked = static_cast<CT>(BoardT::Tall & ~usable[rc].data[xs]);
                            const int inv = BoardT::H - std::bit_width(blocked);
                            const CT fill = BoardT::Tall >> inv;
                            search[r].data[xs] = static_cast<CT>(BoardT::Tall ^ fill);
                        }()), ...);
                    }(std::make_index_sequence<BoardT::W>());

                    search[r] |= (search[r].template shift<-1, 0>() | search[r].template shift<1, 0>()) & usable[rc];
                    search[r] |= (search[r].template shift<-1, 0>() | search[r].template shift<1, 0>()) & usable[rc];

                    spinReach[SpinType::NONE][rs] = search[r];
                }()), ...);
            }(std::make_index_sequence<cSize>());
        } while (false);

        // BFS
        {
            SSB unsearched;
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                ((unsearched[rs] = ~search[rs] & usable[rs]), ...);
            }(std::make_index_sequence<sSize>());

            while (!done.all()) {
                auto bfs = [&]<Rotation r> {
                    if (done[r])
                        return;
                    done.set(r);

                    static_assert(r == Gen::canonical_r<p>(r));
                    assert(search[r].any());

                    // Shifts / softdrops
                    {
                        assert((search[r] & usable[r]) == search[r]);
                        while (true) {
                            BoardT temp = ( search[r].template shift<-1,  0>()
                                          | search[r].template shift< 1,  0>()
                                          | search[r].template shift< 0, -1>() );

                            spinReach[SpinType::NONE][r] |= temp;
                            temp &= unsearched[r];

                            if (!temp.any())
                                break;

                            search[r] |= temp;
                            unsearched[r] ^= temp;
                        }
                    }

                    // Rotates
                    if constexpr (p != Piece::O) {
                        constexpr size_t kickIndex = Gen::kick_index<RulesT, p>();
                        constexpr size_t kick180Index = Gen::kick180_index<p>();

                        auto rotate = [&]<Gen::Direction d, const auto& kickTable> {
                            constexpr Rotation r1 = Gen::rotate<d>(r);
                            constexpr auto off = Gen::canonical_offset<p>(r) - Gen::canonical_offset<p>(r1);
                            constexpr size_t kickSize = Gen::kick_size<RulesT, d, kickTable[r].size()>();

                            BoardT temp = search[r];
                            BoardT result{};

                            [&]<size_t... i>(std::index_sequence<i...>) {
                                ([&] {
                                    constexpr auto kick = kickTable[r][i] + off;
                                    const BoardT m = temp.template shift<kick.x, kick.y>();
                                    result |= m;

                                    const BoardT spun = m & spins;
                                    spinReach[SpinType::NONE][r1] |= m ^ spun;
                                    if constexpr (i >= 4)
                                        spinReach[SpinType::FULL][r1] |= spun;
                                    else {
                                        spinReach[SpinType::MINI][r1] |= spun & ~spinMap[r1];
                                        spinReach[SpinType::FULL][r1] |= spun & spinMap[r1];
                                    }

                                    if constexpr (i != sizeof...(i) - 1)
                                        temp &= ~(usable[r1].template shift<-kick.x, -kick.y>());
                                }(), ...);
                            }(std::make_index_sequence<kickSize>());

                            result &= unsearched[r1];
                            if (result.any()) {
                                search[r1] |= result;
                                unsearched[r1] &= ~result;
                                done.reset(r1);
                            }
                        };

                        rotate.template operator()<Gen::Direction::CW, Gen::kicks[kickIndex][Gen::Direction::CW]>();
                        rotate.template operator()<Gen::Direction::CCW, Gen::kicks[kickIndex][Gen::Direction::CCW]>();
                        if constexpr (RulesT::ENABLE_180)
                            rotate.template operator()<Gen::Direction::FLIP, Gen::kicks180[kick180Index]>();
                    }

                    search[r] = BoardT{};
                };
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    (bfs.template operator()<Rotation(rs)>(), ...);
                }(std::make_index_sequence<sSize>());
            }
        }

        [&]<size_t... ss>(std::index_sequence<ss...>) {
            (([&] {
                constexpr auto s = SpinType(ss);
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    ((moves[s][rs] = candidates[rs] & spinReach[s][rs]), ...);
                }(std::make_index_sequence<cSize>());
            }()), ...);
        }(std::make_index_sequence<sMul>());
    }

public:
    MoveList(const BoardT b, const int y, const int force = 0) {
        if constexpr (checkTspin)
            generate_tspins(b, y, force);
        else
            generate(b, y, force);
    }

    constexpr int popcount() const {
        int result = 0;
        [&]<size_t... ss>(std::index_sequence<ss...>) {
            (([&] {
                constexpr auto s = SpinType(ss);
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    ((result += moves[s][rs].popcount()), ...);
                }(std::make_index_sequence<cSize>());
            }()), ...);
        }(std::make_index_sequence<sMul>());
        return result;
    }

    template <typename Fn>
    void for_each_move(Fn&& fn) const {
        [&]<size_t... ss>(std::index_sequence<ss...>) {
            (([&] {
                constexpr auto s = SpinType(ss);
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    ((moves[s][rs].for_each_set_bit([&](int x, int y) {
                        fn.template operator()<Rotation(rs)>(x, y, s);
                    })), ...);
                }(std::make_index_sequence<cSize>());
            }()), ...);
        }(std::make_index_sequence<sMul>());
    }
};

} // namespace Cobra

#endif