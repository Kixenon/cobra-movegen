#ifndef GEN_HPP
#define GEN_HPP

#include "header.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <utility>

namespace Cobra2 {

namespace Gen {

constexpr int SPAWN_COL = 4;

template<Piece p, Rotation r>
consteval bool in_bounds(const int x) {
    static_assert(is_ok(p));
    static_assert(is_ok(r));
    constexpr PieceCoordinates pc = piece_table(p, r);
    return is_ok_x(x) && is_ok_x(pc[0].x + x) && is_ok_x(pc[1].x + x) && is_ok_x(pc[2].x + x);
}

consteval bool group2(const Piece p) {
    return p == I || p == S || p == Z;
}

consteval bool group3(const Piece p) {
    return p == T || p == L || p == J;
}

template<Piece p>
consteval int canonical_size() {
    if constexpr (p == O)
        return 1;
    if constexpr (group2(p))
        return 2;
    return 4; // L, J, T
}

template<Piece p>
consteval Rotation canonical_r(const Rotation r){
    if constexpr (p == O)
        return NORTH;
    if constexpr (group2(p))
        return static_cast<Rotation>(r & 1);
    return r; // L, J, T
}

template<Piece p>
consteval Coordinates canonical_offset(const Rotation r) {
    if constexpr (p == I) {
        if (r == SOUTH)
            return {1, 0};
        if (r == WEST)
            return {0, -1};
    }
    if constexpr (p == S || p == Z) {
        if (r == SOUTH)
            return {0, 1};
        if (r == WEST)
            return {1, 0};
    }
    return {0, 0};
}

template<typename T, size_t N>
using SmearedBoard = std::array<T, N>;

template <typename BoardT, Piece p>
constexpr auto usable_map(const BoardT& b) {
    static_assert(is_ok(p));

    SmearedBoard<BoardT, canonical_size<p>()> result;

    auto init = [&]<Rotation r>{
        constexpr PieceCoordinates pc = piece_table(p, r);
        const BoardT temp = ~b;
        result[r] = temp;

        [&]<size_t... i>(std::index_sequence<i...>) {
            (([&]{
                if constexpr (pc[i].y > 0) // Don't kick against ceiling
                    result[r] &= (~b.template shifted<0, -pc[i].y>()).template shift<-pc[i].x, 0>();
                else
                    result[r] &= temp.template shifted<-pc[i].x, -pc[i].y>();
            }()), ...);
        }(std::make_index_sequence<3>());
    };
    [&]<size_t... rs>(std::index_sequence<rs...>) {
        ((init.template operator()<static_cast<Rotation>(rs)>()), ...);
    }(std::make_index_sequence<result.size()>{});

    return result;
}

template <typename SB, Piece p>
constexpr SB landable_map(const SB& sb) {
    SB result{};

    [&]<size_t... rs>(std::index_sequence<rs...>) {
        ((result[rs] = sb[rs] & ~sb[rs].template shifted<0, 1>()), ...);
    }(std::make_index_sequence<result.size()>{});

    return result;
}

enum Direction {
    CW, CCW, FLIP, Direction_NB = 2
};

template<Direction d>
constexpr Rotation rotate(const Rotation r) {
    switch(d) {
        case CW: return static_cast<Rotation>((static_cast<int>(r) + 1) & 3);
        case CCW: return static_cast<Rotation>((static_cast<int>(r) + 3) & 3);
        case FLIP: return static_cast<Rotation>((static_cast<int>(r) + 2) & 3);
        default: __builtin_unreachable();
    }
}

template<size_t N>
using Offsets = std::array<Coordinates, N>;

template<size_t N>
using OffsetsRot = std::array<Offsets<N>, ROTATION_NB>;

#define e Coordinates
constexpr std::array<std::array<OffsetsRot<5>, Direction_NB>, 3> kicks = {{
    {{ // LJSZT
        {{ // CW
            {e( 0,  0), e(-1,  0), e(-1,  1), e( 0, -2), e(-1, -2)},
            {e( 0,  0), e( 1,  0), e( 1, -1), e( 0,  2), e( 1,  2)},
            {e( 0,  0), e( 1,  0), e( 1,  1), e( 0, -2), e( 1, -2)},
            {e( 0,  0), e(-1,  0), e(-1, -1), e( 0,  2), e(-1,  2)}
        }},
        {{ // CCW
            {e( 0,  0), e( 1,  0), e( 1,  1), e( 0, -2), e( 1, -2)},
            {e( 0,  0), e( 1,  0), e( 1, -1), e( 0,  2), e( 1,  2)},
            {e( 0,  0), e(-1,  0), e(-1,  1), e( 0, -2), e(-1, -2)},
            {e( 0,  0), e(-1,  0), e(-1, -1), e( 0,  2), e(-1,  2)}
        }}
    }},
    {{ // I SRS
        {{ // CW
            {e( 1,  0), e(-1,  0), e( 2,  0), e(-1, -1), e( 2,  2)},
            {e( 0, -1), e(-1, -1), e( 2, -1), e(-1,  1), e( 2, -2)},
            {e(-1,  0), e( 1,  0), e(-2,  0), e( 1,  1), e(-2, -2)},
            {e( 0,  1), e( 1,  1), e(-2,  1), e( 1, -1), e(-2,  2)}
        }},
        {{ // CCW
            {e( 0, -1), e(-1, -1), e( 2, -1), e(-1,  1), e( 2, -2)},
            {e(-1,  0), e( 1,  0), e(-2,  0), e( 1,  1), e(-2, -2)},
            {e( 0,  1), e( 1,  1), e(-2,  1), e( 1, -1), e(-2,  2)},
            {e( 1,  0), e(-1,  0), e( 2,  0), e(-1, -1), e( 2,  2)}
        }}
    }},
    {{ // I SRS+
        {{ // CW
            {e( 1,  0), e( 2,  0), e(-1,  0), e(-1, -1), e( 2,  2)},
            {e( 0, -1), e(-1, -1), e( 2, -1), e(-1,  1), e( 2, -2)},
            {e(-1,  0), e( 1,  0), e(-2,  0), e( 1,  1), e(-2, -2)},
            {e( 0,  1), e( 1,  1), e(-2,  1), e( 1, -1), e(-2,  2)}
        }},
        {{ // CCW
            {e( 0, -1), e(-1, -1), e( 2, -1), e( 2, -2), e(-1,  1)},
            {e(-1,  0), e(-2,  0), e( 1,  0), e(-2, -2), e( 1,  1)},
            {e( 0,  1), e(-2,  1), e( 1,  1), e(-2,  2), e( 1, -1)},
            {e( 1,  0), e( 2,  0), e(-1,  0), e( 2,  2), e(-1, -1)}
        }}
    }}
}};

constexpr std::array<OffsetsRot<6>, 2> kicks180 = {{
    {{ // LJSZT
        {e( 0,  0), e( 0,  1), e( 1,  1), e(-1,  1), e( 1,  0), e(-1,  0)},
        {e( 0,  0), e( 1,  0), e( 1,  2), e( 1,  1), e( 0,  2), e( 0,  1)},
        {e( 0,  0), e( 0, -1), e(-1, -1), e( 1, -1), e(-1,  0), e( 1,  0)},
        {e( 0,  0), e(-1,  0), e(-1,  2), e(-1,  1), e( 0,  2), e( 0,  1)}
    }},
    {{ // I
        {e( 1, -1), e( 1,  0), e( 2,  0), e( 0,  0), e( 2, -1), e( 0, -1)},
        {e(-1, -1), e( 0, -1), e( 0,  1), e( 0,  0), e(-1,  1), e(-1,  0)},
        {e(-1,  1), e(-1,  0), e(-2,  0), e( 0,  0), e(-2,  1), e( 0,  1)},
        {e( 1,  1), e( 0,  1), e( 0,  3), e( 0,  2), e( 1,  3), e( 1,  2)}
    }}
}};
#undef e

} // namespace Gen

} // namespace Cobra2

#endif