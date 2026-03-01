#ifndef HEADER_HPP
#define HEADER_HPP

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace Cobra2 {

/*----------------------------------------------------------------------------*/
// Types

constexpr int COL_NB = 10;
constexpr int ROW_NB = 48;

enum Piece {
    I, O, T, L, J, S, Z, PIECE_NB = 7, NO_PIECE = 7
};

enum Rotation {
    NORTH, EAST, SOUTH, WEST, ROTATION_NB = 4
};

// enum SpinType {
//     NO_SPIN, MINI, FULL, SPIN_NB = 3
// };

constexpr Piece allPieces[] = {
    I, O, T, L, J, S, Z
};

constexpr Rotation allRotations[] = {
    NORTH, EAST, SOUTH, WEST
};

/*----------------------------------------------------------------------------*/
// Debug functions

constexpr bool is_ok(const Piece p) {
    return p >= I && p < PIECE_NB;
}
constexpr bool is_ok(const Rotation r) {
    return r >= NORTH && r < ROTATION_NB;
}
// constexpr bool is_ok(const SpinType s) {
//     return s >= NO_SPIN && s < SPIN_NB;
// }
constexpr bool is_ok_x(const int x) {
    return x >= 0 && x < COL_NB;
}
constexpr bool is_ok_y(const int y) {
    return y >= 0 && y < ROW_NB;
}

/*----------------------------------------------------------------------------*/
// Types

struct Coordinates {
    int8_t x, y;

    Coordinates() = default;
    constexpr Coordinates(int x, int y) : x(static_cast<int8_t>(x)), y(static_cast<int8_t>(y)) {}

    constexpr Coordinates operator+(const Coordinates& c) const {
        return Coordinates(x + c.x, y + c.y);
    }
    constexpr Coordinates operator-(const Coordinates& c) const {
        return Coordinates(x - c.x, y - c.y);
    }
};

using PieceCoordinates = std::array<Coordinates, 3>; // Only 3 offset needed since one is always at (0, 0)

struct Move {
    Piece piece;
    Rotation rotation;
    int x, y;
    // SpinType spin;

    static constexpr Move none() {
        return Move{.piece = NO_PIECE, .rotation = NORTH, .x = 0, .y = 0};
    }
};

/*----------------------------------------------------------------------------*/
// Functions

constexpr bool operator!(const Piece& p) { return p == NO_PIECE; }

constexpr PieceCoordinates piece_table(const Piece p, const Rotation r) {
    assert(is_ok(p));
    assert(is_ok(r));

    using C = Coordinates;
    constexpr auto make_piece = [](const Piece p) {
        switch(p) {
            case I: return PieceCoordinates{C(-1, 0), C( 1, 0), C( 2, 0)}; // ––––
            case O: return PieceCoordinates{C( 1, 0), C( 0, 1), C( 1, 1)}; // ::
            case T: return PieceCoordinates{C(-1, 0), C( 1, 0), C( 0, 1)}; // _|_
            case L: return PieceCoordinates{C(-1, 0), C( 1, 0), C( 1, 1)}; // __|
            case J: return PieceCoordinates{C(-1, 0), C( 1, 0), C(-1, 1)}; // ––;
            case S: return PieceCoordinates{C(-1, 0), C( 0, 1), C( 1, 1)}; // S
            case Z: return PieceCoordinates{C(-1, 1), C( 0, 1), C( 1, 0)}; // Z
            default: __builtin_unreachable();
        }
    };

    constexpr auto rotate = [](const Rotation r, const Coordinates c) {
        switch(r) {
            case NORTH: return c;
            case EAST:  return C(c.y, -c.x);
            case SOUTH: return C(-c.x, -c.y);
            case WEST:  return C(-c.y, c.x);
            default: __builtin_unreachable();
        }
    };

    const auto cells = make_piece(p);
    return PieceCoordinates{rotate(r, cells[0]), rotate(r, cells[1]), rotate(r, cells[2])};
}

} // namespace Cobra2

#endif