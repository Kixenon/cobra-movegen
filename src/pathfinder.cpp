#include "pathfinder.hpp"
#include "board.hpp"
#include "gen.hpp"
#include "header.hpp"
#include "ruleset.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <limits>
#include <vector>

namespace Cobra {

namespace PathFinder {

struct PathNode {
    Input input;
    uint16_t prev;
};

struct GhostMove {
    Rotation r;
    int8_t x;
    int8_t y;
    uint16_t i;
    SpinType s;

    GhostMove(Rotation r, int x, int y, SpinType s, uint16_t i = root_index()) :
        r(r), x(static_cast<int8_t>(x)), y(static_cast<int8_t>(y)), i(i), s(s) {}

    bool operator==(const GhostMove& other) const { return r == other.r && x == other.x && y == other.y && s == other.s; }

    static constexpr uint16_t root_index() { return std::numeric_limits<uint16_t>::max(); }
};

template <Piece p>
Inputs get_input(const Board& b, const Move& target, const bool useFinesse, const bool force) {
    static_assert(is_ok(p));
    assert(p == T || target.spin() == NO_SPIN);

    constexpr bool canSpin = p == T;
    const Gen::CollisionMap<p> cm(b);

    const int spawn = [&]{
        if (force) {
            const Bitboard s = ~cm[Gen::SPAWN_COL][NORTH] & (~0ULL << ACTIVE_RULES.spawnRow);
            return s ? ctz(s) : 64;
        }
        return (cm[Gen::SPAWN_COL][NORTH] & bb(ACTIVE_RULES.spawnRow)) ? 64 : ACTIVE_RULES.spawnRow;
    }();
    if (spawn == 64)
        return {};

    Bitboard searched[canSpin ? SPIN_NB : 1][COL_NB][ROTATION_NB] = {};
    std::deque<GhostMove> queue;
    std::vector<PathNode> vec;
    vec.reserve(256);

    const GhostMove t(target.rotation(), target.x(), target.y(), target.spin());
    queue.push_back({NORTH, Gen::SPAWN_COL, spawn, NO_SPIN});

    while (!queue.empty()) {
        GhostMove m = queue.front();
        const Rotation rc = Gen::canonical_r<p>(m.r);

        auto update = [&]<Input input>{
            assert(is_ok_x(m.x));
            assert(is_ok_y(m.y));
            assert(canSpin || m.s == NO_SPIN);
            auto& entry = searched[m.s][m.x][m.r];

            if (!(entry & bb(m.y))) {
                entry |= bb(m.y);
                vec.push_back({input, m.i});
                queue.push_back({m.r, m.x, m.y, m.s, static_cast<uint16_t>(vec.size() - 1)});
            }
        };

        // Harddrop
        {
            const int drop = clz( ~(~cm[m.x][rc] << (63 - m.y)) ) - 1;
            if (drop) {
                m.y -= drop;
                if constexpr (p == T)
                    m.s = NO_SPIN;
            }

            m.r = Gen::canonical_r<p>(m.r);

            if (m == t) {
                Inputs inputs;
                for (uint16_t index = m.i; index != GhostMove::root_index(); index = vec[index].prev)
                    inputs += vec[index].input;
                inputs.reverse();
                return inputs += HARD_DROP;
            }
        }

        if constexpr (p == T)
            queue.front().s = NO_SPIN;

        // Rotation
        if constexpr (p != O) {
            auto process = [&]<auto kicksRot, Gen::Direction d>() {
                constexpr Input input = d == Gen::CW ? ROTATE_CW : d == Gen::CCW ? ROTATE_CCW : ROTATE_FLIP;
                const auto& kicks = kicksRot[queue.front().r];
                const Rotation r1 = Gen::rotate<d>(queue.front().r);
                const Rotation rc1 = Gen::canonical_r<p>(r1);
                const Coordinates off = Gen::canonical_offset<p>(queue.front().r) - Gen::canonical_offset<p>(r1);
                const size_t N = (!ACTIVE_RULES.srsPlus && kicks.size() == 6) ? 2 : kicks.size();

                m = queue.front();
                m.r = r1;

                for (size_t i = 0; i < N; ++i) {
                    m.x = queue.front().x + kicks[i].x + off.x;
                    m.y = queue.front().y + kicks[i].y + off.y;

                    if (!is_ok_x(m.x) || m.y < 0 || (cm[m.x][rc1] & bb(m.y)))
                        continue;

                    if constexpr (p == T) {
                        if (ACTIVE_RULES.enableTspin) {
                            const bool corner[] = {
                                b.obstructed(m.x - 1, m.y + 1),
                                b.obstructed(m.x + 1, m.y + 1),
                                b.obstructed(m.x + 1, m.y - 1),
                                b.obstructed(m.x - 1, m.y - 1)
                            };
                            if (corner[0] + corner[1] + corner[2] + corner[3] >= 3)
                                m.s = (i >= 4 || (corner[m.r] && corner[Gen::rotate<Gen::Direction::CW>(m.r)])) ? FULL : MINI;
                            else
                                m.s = NO_SPIN;
                        }
                    }
                    update.template operator()<input>();
                    break;
                }
            };

            if (ACTIVE_RULES.srsPlus) {
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CW], Gen::Direction::CW>();
                process.template operator()<Gen::kicks[(p == I) * 2][Gen::Direction::CCW], Gen::Direction::CCW>();
            } else {
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CW], Gen::Direction::CW>();
                process.template operator()<Gen::kicks[p == I][Gen::Direction::CCW], Gen::Direction::CCW>();
            }

            if (ACTIVE_RULES.enable180)
                process.template operator()<Gen::kicks180[p == I], Gen::Direction::FLIP>();
        }

        // Shift
        {
            auto process = [&]<int8_t dx, Input input>{
                static_assert(dx == 1 || dx == -1);
                assert(m == queue.front());
                m.x += dx;
                if (is_ok_x(m.x) && !(cm[m.x][rc] & bb(m.y))) {
                    update.template operator()<input>();
                    if (useFinesse) {
                        const int8_t x = m.x;
                        m.x += dx;
                        while (is_ok_x(m.x) && !(cm[m.x][rc] & bb(m.y)))
                            m.x += dx;
                        m.x -= dx;
                        if (m.x != x)
                            update.template operator()<input == SHIFT_LEFT ? DAS_LEFT : DAS_RIGHT>();
                        m.x = x;
                    }
                }
                m.x -= dx;
                assert(m == queue.front());
            };
            m = queue.front();
            process.template operator()<-1, SHIFT_LEFT>();
            process.template operator()<1, SHIFT_RIGHT>();
        }

        // Softdrop
        m.y -= 1;
        if (m.y >= 0 && !(cm[m.x][rc] & bb(m.y)))
            update.template operator()<SOFT_DROP>();

        queue.pop_front();
    }

    return {};
}

Inputs get_input(const Board& b, const Move& target, const bool useFinesse, const bool force) {
    assert(is_ok(target));
    switch (target.piece()) {
        case I: return get_input<I>(b, target, useFinesse, force);
        case O: return get_input<O>(b, target, useFinesse, force);
        case T: return get_input<T>(b, target, useFinesse, force);
        case L: return get_input<L>(b, target, useFinesse, force);
        case J: return get_input<J>(b, target, useFinesse, force);
        case S: return get_input<S>(b, target, useFinesse, force);
        case Z: return get_input<Z>(b, target, useFinesse, force);
        default: __builtin_unreachable();
    }
}

} // namespace PathFinder

} // namespace Cobra