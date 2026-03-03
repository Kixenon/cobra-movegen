#ifndef HEADER_HPP
#define HEADER_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

namespace Cobra2 {

/*----------------------------------------------------------------------------*/
// Types

constexpr int COL_NB = 10;
constexpr int ROW_NB = 48;

struct Piece {
    enum Type {
        I, O, T, L, J, S, Z, NO_PIECE
    };

    static constexpr std::array all = {I, O, T, L, J, S, Z};

    Type value;

    constexpr Piece(Type v) : value(v) {}

    constexpr operator size_t() const {
        return static_cast<size_t>(value);
    }
    constexpr bool operator!() const {
        return value == NO_PIECE;
    }

    constexpr bool is_ok() const {
        return std::ranges::contains(all, *this);
    }

    consteval int h_gen() const {
        return value == I || value == T ? 3 : 2 - (value == O);
    }
    consteval int h_spawn() const {
        return value == I ? 3 : 2 - (value == O);
    }
    consteval int h_place() const {
        return 3 + (value == I);
    }

    template <typename Fn>
    constexpr auto route(Fn&& fn) const {
        switch (value) {
            case I: return fn.template operator()<I>();
            case O: return fn.template operator()<O>();
            case T: return fn.template operator()<T>();
            case L: return fn.template operator()<L>();
            case J: return fn.template operator()<J>();
            case S: return fn.template operator()<S>();
            case Z: return fn.template operator()<Z>();
            default: std::unreachable();
        }
    }
};

struct Rotation {
    enum Type {
        NORTH, EAST, SOUTH, WEST
    };

    static constexpr std::array all = {NORTH, EAST, SOUTH, WEST};
    static constexpr size_t size = all.size();

    Type value;

    constexpr Rotation(Type v) : value(v) {}
    explicit constexpr Rotation(size_t v) : value(static_cast<Type>(v)) {}

    constexpr operator size_t() const {
        return static_cast<size_t>(value);
    }

    constexpr bool is_ok() const {
        return std::ranges::contains(all, *this);
    }
};

/*----------------------------------------------------------------------------*/
// Debug functions

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
        return Move{.piece = Piece::NO_PIECE, .rotation = Rotation::NORTH, .x = 0, .y = 0};
    }
};

/*----------------------------------------------------------------------------*/
// Functions

template <Piece p, Rotation r>
consteval PieceCoordinates piece_table() {
    assert(p.is_ok());
    assert(r.is_ok());

    using C = Coordinates;
    constexpr auto cells = []{
        switch (p) {
            case Piece::I: return PieceCoordinates{C(-1, 0), C( 1, 0), C( 2, 0)}; // ––––
            case Piece::O: return PieceCoordinates{C( 1, 0), C( 0, 1), C( 1, 1)}; // ::
            case Piece::T: return PieceCoordinates{C(-1, 0), C( 1, 0), C( 0, 1)}; // _|_
            case Piece::L: return PieceCoordinates{C(-1, 0), C( 1, 0), C( 1, 1)}; // __|
            case Piece::J: return PieceCoordinates{C(-1, 0), C( 1, 0), C(-1, 1)}; // ––;
            case Piece::S: return PieceCoordinates{C(-1, 0), C( 0, 1), C( 1, 1)}; // S
            case Piece::Z: return PieceCoordinates{C(-1, 1), C( 0, 1), C( 1, 0)}; // Z
            default: std::unreachable();
        }
    }();

    constexpr auto rotate = [](const Coordinates c) {
        switch (r) {
            case Rotation::NORTH: return c;
            case Rotation::EAST:  return C(c.y, -c.x);
            case Rotation::SOUTH: return C(-c.x, -c.y);
            case Rotation::WEST:  return C(-c.y, c.x);
            default: std::unreachable();
        }
    };

    return PieceCoordinates{rotate(cells[0]), rotate(cells[1]), rotate(cells[2])};
}

} // namespace Cobra2

#endif