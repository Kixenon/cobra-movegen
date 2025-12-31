#include "board.hpp"
#include "gen.hpp"
#include "header.hpp"
#include "movegen.hpp"
#include "ruleset.hpp"

#include <cassert>
#include <cstddef>
#include <utility>

namespace Cobra {

template<Piece p1>
Move* generate(const Gen::CollisionMap<p1 == TSPIN ? T : p1>& cm, Move* moves, const bool slow, const bool force, [[maybe_unused]] const Bitboard (*spinMap)[1 + ROTATION_NB] = nullptr) {
    constexpr Piece p = p1 == TSPIN ? T : p1;
    constexpr bool checkSpin = p1 == TSPIN;
    constexpr int canonicalSize = Gen::canonical_size<p>();
    constexpr int searchSize = p == O ? 1 : ROTATION_NB;
    static_assert(is_ok(p));

    int total = 0;
    Bitboard remaining = 0;
    Bitboard toSearch[COL_NB][searchSize] = {};
    Bitboard searched[COL_NB][searchSize];
    Bitboard moveSet[COL_NB][canonicalSize] = {};
    Bitboard spinSet[COL_NB][ROTATION_NB][checkSpin ? SPIN_NB : 0] = {};

    auto remaining_index = [](int x, Rotation r) { return bb(x * ROTATION_NB + r); };

    for (int x = 0; x < COL_NB; ++x)
        for (int r = 0; r < canonicalSize; ++r) {
            searched[x][r] = cm[x][r];
            if constexpr (Gen::group2(p))
                searched[x][r + 2] = searched[x][r];
        }

    if (slow) {
        constexpr int spawnCol = 4;
        const Bitboard spawn = [&]{
            if (force) {
                const Bitboard s = ~cm[spawnCol][NORTH] & (~0ULL << ACTIVE_RULES.spawnRow);
                return s & -s;
            }
            return ~cm[spawnCol][NORTH] & bb(ACTIVE_RULES.spawnRow);
        }();
        if (!spawn)
            return moves;

        toSearch[spawnCol][NORTH] = spawn;
        remaining |= remaining_index(spawnCol, NORTH);

        if constexpr (checkSpin)
            spinSet[spawnCol][NORTH][NO_SPIN] = spawn;
    } else {
        auto init = [&]<int x>{
            auto process = [&]<Rotation r>{
                if constexpr (!Gen::in_bounds<p, r>(x))
                    return;

                assert(cm[x][r] != ~0ULL);
                const int y = bitlen(cm[x][r]);
                const Bitboard surface = bb_low(ACTIVE_RULES.spawnRow) & ~bb_low(y);

                searched[x][r] |= toSearch[x][r] = surface;
                remaining |= remaining_index(x, r);

                if constexpr (Gen::group2(p)) {
                    constexpr Rotation r1 = Gen::rotate<Gen::FLIP>(r);
                    searched[x][r1] |= toSearch[x][r1] = surface;
                    remaining |= remaining_index(x, r1);
                }

                if constexpr (checkSpin)
                    spinSet[x][r][NO_SPIN] = surface;
                else {
                    *moves++ = Move(p, r, x, y);
                    total += popcount(~cm[x][r] & ((cm[x][r] << 1) | 1)) - 1;
                }
            };

            [&]<size_t... rs>(std::index_sequence<rs...>) {
                (process.template operator()<static_cast<Rotation>(rs)>(), ...);
            }(std::make_index_sequence<canonicalSize>());
        };

        [&]<size_t... xs>(std::index_sequence<xs...>) {
            (init.template operator()<xs>(), ...);
        }(std::make_index_sequence<COL_NB>());

        if constexpr (!checkSpin)
            if (!total)
                return moves;
    }

    while (remaining) {
        const int index = ctz(remaining);
        const int x = index >> 2;
        const Rotation r = static_cast<Rotation>(index & 3);

        assert(is_ok_x(x));
        assert(is_ok(r));
        assert(toSearch[x][r]);
        assert((toSearch[x][r] & ~cm[x][Gen::canonical_r<p>(r)]) == toSearch[x][r]);

        // Softdrops
        {
            if constexpr (checkSpin) {
                Bitboard m = (toSearch[x][r] >> 1) & ~cm[x][r];
                while ((m & toSearch[x][r]) != m) {
                    toSearch[x][r] |= m;
                    m |= (m >> 1) & ~cm[x][r];
                }
                spinSet[x][r][NO_SPIN] |= m;
            } else {
                Bitboard m = (toSearch[x][r] >> 1) & ~toSearch[x][r] & ~searched[x][r];
                // if (m) {
                //     const Bitboard m1 = __builtin_bitreverse64(m);
                //     const Bitboard f = __builtin_bitreverse64(searched[x][r]);
                //     toSearch[x][r] |= __builtin_bitreverse64(((f & (~f + m1)) - m1));
                // }
                // Alternative if no/slow bit reverse function (x86 arch):
                while (m) {
                    toSearch[x][r] |= m;
                    m = (m >> 1) & ~searched[x][r];
                }
            }
        }

        // Harddrops
        {
            if constexpr (checkSpin)
                moveSet[x][r] |= toSearch[x][r] & ((cm[x][r] << 1) | 1);
            else {
                const Rotation r1 = Gen::canonical_r<p>(r);
                Bitboard m = toSearch[x][r] & ((cm[x][r1] << 1) | 1) & ~searched[x][r] & ~moveSet[x][r1];
                if (m) {
                    assert(is_ok(r1));
                    assert(!(m & cm[x][r1]));
                    assert(((m >> 1) & cm[x][r1]) == (m >> 1));

                    moveSet[x][r1] |= m;
                    total -= popcount(m);
                    while (m) {
                        *moves++ = Move(p, r1, x, ctz(m));
                        m &= m - 1;
                    }
                    if (!total)
                        return moves;
                }
            }
        }

        // Shift
        {
            auto shift = [&](int x1) {
                const Bitboard m = toSearch[x][r] & ~searched[x1][r];
                if (m) {
                    toSearch[x1][r] |= m;
                    remaining |= remaining_index(x1, r);
                    if constexpr (checkSpin)
                        spinSet[x1][r][NO_SPIN] |= m;
                }
            };
            if (x > 0)
                shift(x - 1);
            if (x < 9)
                shift(x + 1);
        }

        // Rotate
        if constexpr (p != O) {
            auto process = [&]<auto kicksRot>(Rotation r1) {
                const auto& kicks = kicksRot[r];
                const Rotation rc = Gen::canonical_r<p>(r1);

                const Coordinates src = Gen::canonical_offset<p>(r);
                const Coordinates tgt = Gen::canonical_offset<p>(r1);
                const int ddx = src.x - tgt.x;
                const int ddy = src.y - tgt.y;

                const size_t N = (!ACTIVE_RULES.srsPlus && kicks.size() == 6) ? 2 : kicks.size();

                Bitboard current = toSearch[x][r];

                for (size_t i = 0; i < N && current; ++i) {
                    const int x1 = x + kicks[i].x + ddx;
                    if (!is_ok_x(x1))
                        continue;

                    constexpr int threshold = 3;
                    const int dy = kicks[i].y + ddy;
                    const int y1 = threshold + dy;

                    Bitboard m = ((current << y1) >> threshold) & ~cm[x1][rc];
                    current ^= (m << threshold) >> y1;

                    if constexpr (checkSpin) {
                        const Bitboard spins = m & spinMap[x1][0];

                        spinSet[x1][r1][NO_SPIN] |= m ^ spins;

                        if (spins) {
                            if (i >= 4)
                                spinSet[x1][r1][FULL] |= spins;
                            else {
                                spinSet[x1][r1][MINI] |= spins & ~spinMap[x1][1 + r1];
                                spinSet[x1][r1][FULL] |= spins & spinMap[x1][1 + r1];
                            }
                        }
                    }

                    if ((m &= ~searched[x1][r1])) {
                        toSearch[x1][r1] |= m;
                        remaining |= remaining_index(x1, r1);
                    }
                }
            };

            if (ACTIVE_RULES.srsPlus) {
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CW]>(Gen::rotate<Gen::Direction::CW>(r));
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CCW]>(Gen::rotate<Gen::Direction::CCW>(r));
            }
            else {
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CW]>(Gen::rotate<Gen::Direction::CW>(r));
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CCW]>(Gen::rotate<Gen::Direction::CCW>(r));
            }
            if (ACTIVE_RULES.enable180)
                process.template operator()<Gen::kicks180[p == I]>(Gen::rotate<Gen::Direction::FLIP>(r));
        }

        searched[x][r] |= toSearch[x][r];
        toSearch[x][r] = 0;
        remaining ^= bb(index);
    }

    if constexpr (checkSpin)
        for (int x = 0; x < COL_NB; ++x)
            for (const Rotation r : allRotations) {
                if (!moveSet[x][r])
                    continue;

                for (const auto s : {NO_SPIN, MINI, FULL}) {
                    Bitboard current = moveSet[x][r] & spinSet[x][r][s];
                    while (current) {
                        *moves++ = Move(s == NO_SPIN ? T : TSPIN, r, x, ctz(current), s == FULL);
                        current &= current - 1;
                    }
                }
            }

    return moves;
}

Move* generate(const Board& b, Move* moves, const Piece p, const bool force) {
    assert(ACTIVE_RULES.spawnRow > 0);
    const bool slow = [&]{
        Bitboard m = b[0];
        for (int i = 1; i < COL_NB; ++i)
            m |= b[i];
        return bitlen(m) > ACTIVE_RULES.spawnRow - 3;
    }();

    switch(p) {
        case I: return generate<I>(Gen::CollisionMap<I>(b), moves, slow, force);
        case O: return generate<O>(Gen::CollisionMap<O>(b), moves, slow, force);
        case T:
            if (ACTIVE_RULES.enableTspin) {
                const Gen::CollisionMap<T> cm(b);
                bool checkSpin = false;
                Bitboard spinMap[COL_NB][1 + ROTATION_NB] = {};
                auto init = [&]<int x>{
                    const Bitboard corners[] = {
                        x > 0 ? b[x - 1] >> 1 : ~0ULL,
                        x < 9 ? b[x + 1] >> 1 : ~0ULL,
                        x < 9 ? (b[x + 1] << 1 | 1) : ~0ULL,
                        x > 0 ? (b[x - 1] << 1 | 1) : ~0ULL
                    };

                    const Bitboard spins = (
                        (corners[0] & corners[1] & (corners[2] | corners[3])) |
                        (corners[2] & corners[3] & (corners[0] | corners[1]))
                    );

                    spinMap[x][0] = spins;
                    if (spins) {
                        auto process = [&]<Rotation r>{
                            if (Gen::in_bounds<T, r>(x)) {
                                spinMap[x][1 + r] = spins & corners[r] & corners[Gen::rotate<Gen::Direction::CW>(r)];
                                checkSpin |= spins & ~cm[x][r] & ((cm[x][r] << 1) | 1);
                            }
                        };

                        [&]<size_t... rs>(std::index_sequence<rs...>) {
                            (process.template operator()<static_cast<Rotation>(rs)>(), ...);
                        }(std::make_index_sequence<ROTATION_NB>());
                    }
                };

                [&]<size_t... xs>(std::index_sequence<xs...>) {
                    (init.template operator()<xs>(), ...);
                }(std::make_index_sequence<COL_NB>());

                if (checkSpin)
                    return generate<TSPIN>(cm, moves, slow, force, spinMap);
                return generate<T>(cm, moves, slow, force);
            } else
                return generate<T>(Gen::CollisionMap<T>(b), moves, slow, force);
        case L: return generate<L>(Gen::CollisionMap<L>(b), moves, slow, force);
        case J: return generate<J>(Gen::CollisionMap<J>(b), moves, slow, force);
        case S: return generate<S>(Gen::CollisionMap<S>(b), moves, slow, force);
        case Z: return generate<Z>(Gen::CollisionMap<Z>(b), moves, slow, force);
        default: __builtin_unreachable();
    }
}

} // namespace Cobra