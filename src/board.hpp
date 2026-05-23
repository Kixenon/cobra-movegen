#ifndef BOARD_H
#define BOARD_H

#include "header.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <string>
#include <utility>

namespace Cobra {

class Board {
private:
    Bitboard col[COL_NB];

public:
    bool occupied(const int x, const int y) const {
        return col[x] & bb(y);
    }

    bool occupied(const Coordinates& c) const {
        return occupied(c.x, c.y);
    }

    bool obstructed(const int x, const int y) const {
        return !is_ok_x(x) || !is_ok_y(y) || occupied(x, y);
    }

    bool obstructed(const Coordinates& c) const {
        return obstructed(c.x, c.y);
    }

    bool obstructed(const Move& move) const {
        const PieceCoordinates pc = move.cells();
        const Coordinates off(move.x(), move.y());
        return obstructed(off)
            || obstructed(pc[0] + off)
            || obstructed(pc[1] + off)
            || obstructed(pc[2] + off);
    }

    constexpr Bitboard& operator[](const int x) const {
        assert(is_ok_x(x));
        return const_cast<Bitboard&>(col[x]);
    }

    bool empty() const {
        return std::all_of(std::begin(col), std::end(col), [](Bitboard b) { return b == 0; });
    }

    Bitboard line_clears() const {
        Bitboard result = ~0ULL;
        [&]<size_t... xs>(std::index_sequence<xs...>) {
            ((result &= col[xs]), ...);
        }(std::make_index_sequence<COL_NB>{});
        return result;
    }

    void clear() {
        __builtin_memset(col, 0, sizeof(col));
    }

    void clear_lines(Bitboard l) {
        assert(l);
        do {
            const Bitboard mask = ~((l & -l) - 1);
            [&]<size_t... xs>(std::index_sequence<xs...>) {
                ((col[xs] = col[xs] ^ ((col[xs] ^ (col[xs] >> 1)) & mask)), ...);
            }(std::make_index_sequence<COL_NB>{});
        } while ((l = (l & (l - 1)) >> 1));
    }

    void place(const Move& move) {
        const PieceCoordinates pc = move.cells();
        const int x = move.x();
        const int y = move.y();
        col[x] |= bb(y);
        col[pc[0].x + x] |= bb(pc[0].y + y);
        col[pc[1].x + x] |= bb(pc[1].y + y);
        col[pc[2].x + x] |= bb(pc[2].y + y);
    }

    void spawn_garbage(const int lines, const int x) {
        assert(is_ok_x(x));
        assert(lines > 0);
        [&]<size_t... xs>(std::index_sequence<xs...>) {
            ((col[xs] = ~(~col[xs] << lines)), ...);
        }(std::make_index_sequence<COL_NB>{});
        col[x] = (col[x] >> lines) << lines;
    }

    int do_move(const Move& move) {
        assert(is_ok(move));
        assert(!obstructed(move));

        place(move);
        const Bitboard clears = line_clears();
        if (!clears)
            return 0;

        clear_lines(clears);
        return popcount(clears);
    }

    std::string to_string() const {
        constexpr int lines = 20;
        std::string output;
        output.reserve((lines + 1) * 86 + 44);
        output += "\n +---+---+---+---+---+---+---+---+---+---+\n";
        for (int y = lines; y >= 0; --y) {
            for (const auto& c : col) {
                output += " | ";
                output += (c & bb(y) ? '#' : ' ');
            }
            output += " |\n +---+---+---+---+---+---+---+---+---+---+\n";
        }
        return output;
    }

    std::string to_string(const Move& move) const {
        std::string output = to_string();
        if (!obstructed(move)) {
            constexpr int lines = 20;
            const PieceCoordinates pc = move.cells();
            const int x = move.x();
            const int y = move.y();
            for (size_t i = 0; i < 4; ++i) {
                const int inverseY = lines - (i == 0 ? y : pc[i - 1].y + y);
                if (inverseY < 0) // Moves above printable area
                    continue;
                output[static_cast<size_t>(inverseY * 86 + (i == 0 ? x : pc[i - 1].x + x) * 4 + 47)] = '.';
            }
        }
        return output;
    }
};

} // namespace Cobra

#endif // BOARD_H