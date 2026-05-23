#ifndef MOVEGEN_H
#define MOVEGEN_H

#include "board.hpp"
#include "gen.hpp"
#include "header.hpp"
#include "ruleset.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <utility>

namespace Cobra {

constexpr int MAX_MOVES = 256;

template <Ruleset R>
class MoveList {
private:
    Move moves[MAX_MOVES];
    const Move* const last;

    bool no_duplicates() const {
        for (const Move* m = begin(); m < end() - 1; ++m)
            for (const Move* n = m + 1; n < end(); ++n)
                if (*m == *n)
                    return false;
        return true;
    }

    bool all_valid(const Board& b) const {
        for (const Move* m = begin(); m != end(); ++m) {
            if (!is_ok(*m))
                return false;

            const PieceCoordinates pc = m->cells();
            Coordinates off(m->x(), m->y());
            if (b.obstructed(off) ||
                b.obstructed(pc[0] + off) ||
                b.obstructed(pc[1] + off) ||
                b.obstructed(pc[2] + off))
                return false;

            --off.y;
            if (!b.obstructed(off) &&
                !b.obstructed(pc[0] + off) &&
                !b.obstructed(pc[1] + off) &&
                !b.obstructed(pc[2] + off))
                return false;
        }
        return true;
    }

public:
    MoveList(const Board& b, Piece p) : last(generate<R>(b, moves, p, false)) {
        assert(size() < MAX_MOVES);
        assert(no_duplicates());
        assert(all_valid(b));
    }

    MoveList(const Board& b, Piece p, Piece hold, bool force = false) :
        last([&]{
            Move* l = generate<R>(b, moves, p, force);
            return (l != moves && p != hold) ? generate<R>(b, l, hold, force) : l;
        }()) {
        assert(size() < MAX_MOVES);
        assert(no_duplicates());
        assert(all_valid(b));
    }

    size_t size() const {
        return static_cast<size_t>(last - moves);
    }

    bool empty() const {
        return last == moves;
    }

    bool contains(const Move& m) const {
        return std::any_of(begin(), end(), [m](const Move& move) { return move == m; });
    }

    const Move* begin() const {
        return moves;
    }

    const Move* end() const {
        return last;
    }

};

template <Ruleset R, Piece p1>
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
    Bitboard moveSet[COL_NB][checkSpin ? 0 : canonicalSize] = {};
    Bitboard spinSet[COL_NB][ROTATION_NB][checkSpin ? SPIN_NB : 0] = {};

    auto remaining_index = [](int x, Rotation r) { return bb(x * ROTATION_NB + r); };

    [&]<size_t... xs>(std::index_sequence<xs...>) {
        ([&]<int x>() {
            [&]<size_t... rs>(std::index_sequence<rs...>) {
                ([&]{
                    searched[x][rs] = cm[x][rs];
                    if constexpr (Gen::group2(p))
                        searched[x][rs + 2] = searched[x][rs];
                }(), ...);
            }(std::make_index_sequence<canonicalSize>());
        }.template operator()<xs>(), ...);
    }(std::make_index_sequence<COL_NB>());

    if (slow) {
        const Bitboard spawn = [&]{
            if (force) {
                const Bitboard s = ~cm[Gen::SPAWN_COL][NORTH] & (~0ULL << R::SPAWN_Y);
                return s & -s;
            }
            return ~cm[Gen::SPAWN_COL][NORTH] & bb(R::SPAWN_Y);
        }();
        if (!spawn)
            return moves;

        toSearch[Gen::SPAWN_COL][NORTH] = spawn;
        remaining |= remaining_index(Gen::SPAWN_COL, NORTH);

        if constexpr (checkSpin)
            spinSet[Gen::SPAWN_COL][NORTH][NO_SPIN] = spawn;
    } else {
        auto init = [&]<int x>{
            auto process = [&]<Rotation r>{
                if constexpr (!Gen::in_bounds<p, r>(x))
                    return;

                assert(cm[x][r] != ~0ULL);
                const int y = bitlen(cm[x][r]);
                const Bitboard surface = bb_low(R::SPAWN_Y) & ~bb_low(y);

                searched[x][r] |= toSearch[x][r] = surface;
                remaining |= remaining_index(x, r);

                if constexpr (Gen::group2(p)) {
                    constexpr Rotation r1 = Gen::rotate<Gen::FLIP>(r);
                    if constexpr (r1 == SOUTH)
                        searched[x][r1] |= toSearch[x][r1] = surface & (surface >> 1);
                    else
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
                while (m) {
                    toSearch[x][r] |= m;
                    m = (m >> 1) & ~searched[x][r];
                }
            }
        }

        // Harddrops
        {
            if constexpr (!checkSpin) {
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
                }
                if constexpr (checkSpin)
                    spinSet[x1][r][NO_SPIN] |= toSearch[x][r];
            };
            if (x > 0)
                shift(x - 1);
            if (x < COL_NB - 1)
                shift(x + 1);
        }

        // Rotate
        if constexpr (p != O) {
            auto process = [&]<auto kicksRot, Gen::Direction d>() {
                const auto& kicks = kicksRot[r];
                const Rotation r1 = Gen::rotate<d>(r);
                const Rotation rc = Gen::canonical_r<p>(r1);
                const Coordinates off = Gen::canonical_offset<p>(r) - Gen::canonical_offset<p>(r1);
                const size_t N = (R::KICKS != Policy::KickRule::SRS_PLUS && kicks.size() == 6) ? 2 : kicks.size();

                Bitboard current = toSearch[x][r];

                for (size_t i = 0; i < N && current; ++i) {
                    const int x1 = x + kicks[i].x + off.x;
                    if (!is_ok_x(x1))
                        continue;

                    constexpr int threshold = 3;
                    const int y1 = threshold + kicks[i].y + off.y;

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

            if (R::KICKS == Policy::KickRule::SRS_PLUS) {
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CW], Gen::Direction::CW>();
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CCW], Gen::Direction::CCW>();
            }
            else {
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CW], Gen::Direction::CW>();
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CCW], Gen::Direction::CCW>();
            }
            if (R::ENABLE_180)
                process.template operator()<Gen::kicks180[p == I], Gen::Direction::FLIP>();
        }

        searched[x][r] |= toSearch[x][r];
        toSearch[x][r] = 0;
        remaining ^= bb(index);
    }

    if constexpr (checkSpin)
        for (int x = 0; x < COL_NB; ++x)
            for (const Rotation r : allRotations) {
                const auto ms = ~cm[x][r] & ((cm[x][r] << 1) | 1);

                for (const auto s : {NO_SPIN, MINI, FULL}) {
                    Bitboard current = ms & spinSet[x][r][s];
                    while (current) {
                        *moves++ = Move(s == NO_SPIN ? T : TSPIN, r, x, ctz(current), s == FULL);
                        current &= current - 1;
                    }
                }
            }

    return moves;
}

template <Ruleset R, Piece p>
Move* generate16(const Board& b, Move* moves) {
    static_assert(is_ok(p));

    constexpr int canonicalSize = Gen::canonical_size<p>();
    constexpr int searchSize = p == O ? 1 : ROTATION_NB;
    constexpr Bitboard canonicalMask = canonicalSize == 4 ? ~0ULL : canonicalSize == 2 ? 0xFFFFFFFFULL : 0xFFFFULL;
    constexpr Bitboard searchMask = searchSize == 4 ? ~0ULL : 0xFFFFULL;
    constexpr Bitboard sMask = 0x7FFF7FFF7FFF7FFFULL;
    constexpr Bitboard fMask = 0x1000100010001ULL;

    const Gen::CollisionMap16<p> cm(b);

    int total = 0;
    unsigned remaining = 0;
    Bitboard toSearch[COL_NB] = {};
    Bitboard searched[COL_NB];
    Bitboard moveSet[COL_NB] = {};

    // Fast init
    auto init = [&]<int x>() {
        Bitboard surface = searched[x] = cm[x]; // Include cm in searched to save some instructions later
        surface |= (surface >> 1) & 0x7FFF7FFF7FFF7FFFULL;
        surface |= (surface >> 2) & 0x3FFF3FFF3FFF3FFFULL;
        surface |= (surface >> 4) & 0x0FFF0FFF0FFF0FFFULL;
        surface |= (surface >> 8) & 0x00FF00FF00FF00FFULL;

        const Bitboard s = ~surface;
        searched[x] |= toSearch[x] = s;
        if (s)
            remaining |= (1 << x);

        moveSet[x] = ~surface & ((surface << 1) | fMask) & canonicalMask;
        assert(popcount(moveSet[x]) <= searchSize);

        Bitboard m = moveSet[x];
        total += popcount(~cm[x] & ((cm[x] << 1) | fMask) & canonicalMask) - popcount(m);

        while (m) {
            const int y = ctz(m);
            const Rotation r = static_cast<Rotation>(y / 16);
            *moves++ = Move(p, r, x, y % 16);
            m &= m - 1;
        }
    };

    [&]<size_t... xs>(std::index_sequence<xs...>) {
        (init.template operator()<xs>(), ...);
    }(std::make_index_sequence<COL_NB>());

    if (!total)
        return moves;

    while (remaining) {
        const int x = ctz(remaining);
        remaining &= remaining - 1;

        assert(is_ok_x(x));
        assert(toSearch[x]);
        assert((toSearch[x] & ~cm[x]) == toSearch[x]);

        Bitboard current = toSearch[x];
        toSearch[x] = 0;

        // Softdrops
        {
            Bitboard m = (current >> 1) & ~searched[x] & sMask;
            while (m) {
                current |= m;
                m = (m >> 1) & sMask & ~searched[x];
            }
        }

        // Harddrops
        {
            Bitboard m = current & ((cm[x] << 1) | fMask) & searchMask;

            if constexpr (Gen::group2(p))
                m = (m | (m >> 32)) & canonicalMask;

            m &= ~moveSet[x];

            if (m) {
                moveSet[x] |= m;
                total -= popcount(m);

                while (m) {
                    const int y = ctz(m);
                    const Rotation r = static_cast<Rotation>(y / 16);
                    *moves++ = Move(p, r, x, y % 16);
                    m &= m - 1;
                }

                if (!total)
                    return moves;
            }
        }

        // Shift
        {
            auto shift = [&](int x1) {
                const Bitboard m = current & ~searched[x1];
                if (m) {
                    toSearch[x1] |= m;
                    remaining |= (1 << x1);
                }
            };
            if (x > 0)
                shift(x - 1);
            if (x < COL_NB - 1)
                shift(x + 1);
        }

        // Rotate (Very Ugly)
        if constexpr (p != O) {
            auto process = [&]<auto kicksRot, Gen::Direction d>() {
                [&]<size_t... rs>(std::index_sequence<rs...>) {
                    auto try_rotate = [&]<Rotation r>() {
                        constexpr int shiftSrc = r * 16;
                        Bitboard src = (current >> shiftSrc) & 0xFFFFULL;

                        if (!src) return;

                        constexpr Rotation r1 = Gen::rotate<d>(r);
                        constexpr int shiftDest = r1 * 16;
                        constexpr Coordinates off = Gen::canonical_offset<p>(r) - Gen::canonical_offset<p>(r1);
                        const auto& kicks = kicksRot[r];
                        const size_t N = (R::KICKS != Policy::KickRule::SRS_PLUS && kicks.size() == 6) ? 2 : kicks.size();

                        for (size_t i = 0; i < N && src; ++i) {
                            const int x1 = x + kicks[i].x + off.x;
                            if (!is_ok_x(x1))
                                continue;

                            constexpr int threshold = 3;
                            const int shiftVal = threshold + kicks[i].y + off.y;

                            Bitboard m = (src << shiftVal) >> threshold;
                            m &= ~(cm[x1] >> shiftDest) & 0xFFFFULL;
                            src ^= (m << threshold) >> shiftVal;

                            Bitboard visited = searched[x1];
                            if (x1 == x)
                                visited |= current;
                            m &= ~(visited >> shiftDest);

                            if (m) {
                                toSearch[x1] |= (m << shiftDest);
                                remaining |= (1 << x1);
                            }
                        }
                    };
                    (try_rotate.template operator()<static_cast<Rotation>(rs)>(), ...);
                }(std::make_index_sequence<ROTATION_NB>{});
            };

            if (R::KICKS == Policy::KickRule::SRS_PLUS) {
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CW], Gen::Direction::CW>();
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CCW], Gen::Direction::CCW>();
            } else {
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CW], Gen::Direction::CW>();
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CCW], Gen::Direction::CCW>();
            }
            if (R::ENABLE_180)
                process.template operator()<Gen::kicks180[p == I], Gen::Direction::FLIP>();
        }

        searched[x] |= current;
    }

    return moves;
}

template <Ruleset R>
Move* generate16(const Board& b, Move* moves, const Piece p) {
    switch(p) {
        case I: return generate16<R, I>(b, moves);
        case O: return generate16<R, O>(b, moves);
        case T: return generate16<R, T>(b, moves);
        case L: return generate16<R, L>(b, moves);
        case J: return generate16<R, J>(b, moves);
        case S: return generate16<R, S>(b, moves);
        case Z: return generate16<R, Z>(b, moves);
        default: __builtin_unreachable();
    }
}

template <Ruleset R>
Move* generate(const Board& b, Move* moves, const Piece p, const bool force) {
    static_assert(R::SPAWN_Y > 0);
    const int h = [&]{
        Bitboard m = b[0];
        for (int i = 1; i < COL_NB; ++i)
            m |= b[i];
        return bitlen(m);
    }();

    const bool slow = h > R::SPAWN_Y - 3;
    const bool low = !slow && h <= 13;

    if (low && (p != T || R::SPINS != Policy::SpinRule::TSPIN))
        return generate16<R>(b, moves, p);

    switch(p) {
        case I: return generate<R, I>(Gen::CollisionMap<I>(b), moves, slow, force);
        case O: return generate<R, O>(Gen::CollisionMap<O>(b), moves, slow, force);
        case T:
            if (R::SPINS == Policy::SpinRule::TSPIN) {
                const Gen::CollisionMap<T> cm(b);
                bool checkSpin = false;
                Bitboard spinMap[COL_NB][1 + ROTATION_NB] = {};
                auto init = [&]<int x>{
                    const Bitboard corners[] = {
                        x > 0 ? b[x - 1] >> 1 : ~0ULL,
                        x < COL_NB - 1 ? b[x + 1] >> 1 : ~0ULL,
                        x < COL_NB - 1 ? (b[x + 1] << 1 | 1) : ~0ULL,
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
                    return generate<R, TSPIN>(cm, moves, slow, force, spinMap);
                return low ? generate16<R>(b, moves, p) : generate<R, T>(cm, moves, slow, force);
            } else
                return generate<R, T>(Gen::CollisionMap<T>(b), moves, slow, force);
        case L: return generate<R, L>(Gen::CollisionMap<L>(b), moves, slow, force);
        case J: return generate<R, J>(Gen::CollisionMap<J>(b), moves, slow, force);
        case S: return generate<R, S>(Gen::CollisionMap<S>(b), moves, slow, force);
        case Z: return generate<R, Z>(Gen::CollisionMap<Z>(b), moves, slow, force);
        default: __builtin_unreachable();
    }
}

} // namespace Cobra

#endif