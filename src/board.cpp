#include "board.hpp"
#include "header.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <string>

namespace Cobra {

bool Board::obstructed(const Move& move) const {
    const PieceCoordinates pc = move.cells();
    const Coordinates off(move.x(), move.y());
    return obstructed(off)
        || obstructed(pc[0] + off)
        || obstructed(pc[1] + off)
        || obstructed(pc[2] + off);
}

bool Board::empty() const {
    return std::all_of(std::begin(col), std::end(col), [](Bitboard b) { return b == 0; });
}

Bitboard Board::line_clears() const {
    Bitboard result = col[0];
    for (int x = 1; x < COL_NB && result; ++x)
        result &= col[x];
    return result;
}

void Board::clear() {
    __builtin_memset(col, 0, sizeof(col));
}

void Board::clear_lines(Bitboard l) {
    assert(l);
    do {
        const Bitboard mask = ~((l & -l) - 1);
        for (auto& c : col)
            c = c ^ ((c ^ (c >> 1)) & mask);
    } while ((l = (l & (l - 1)) >> 1));
}

void Board::place(const Move& move) {
    const PieceCoordinates pc = move.cells();
    const int x = move.x();
    const int y = move.y();
    col[x] |= bb(y);
    col[pc[0].x + x] |= bb(pc[0].y + y);
    col[pc[1].x + x] |= bb(pc[1].y + y);
    col[pc[2].x + x] |= bb(pc[2].y + y);
}

void Board::spawn_garbage(const int lines, const int x) {
    assert(is_ok_x(x));
    assert(lines > 0);
    for (auto& c : col)
        c = ~(~c << lines);
    col[x] = (col[x] >> lines) << lines;
}

int Board::do_move(const Move& move) {
    assert(is_ok(move));
    assert(!obstructed(move));

    place(move);
    const Bitboard clears = line_clears();
    if (!clears)
        return 0;

    clear_lines(clears);
    return popcount(clears);
}

std::string Board::to_string() const {
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

std::string Board::to_string(const Move& move) const {
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

void State::init() {
    board.clear();
    hold = NO_PIECE;
    b2b = combo = 0;
}

MoveInfo State::do_move(const Move& move) {
    assert(is_ok(move));

    const int clearCount = board.do_move(move);
    if (!clearCount)
        return MoveInfo{move.piece(), NO_SPIN, 0, 0, combo = 0, false};

    const SpinType spin = move.spin();

    return MoveInfo{
        move.piece(),
        spin,
        clearCount,
        b2b = (spin || clearCount == 4) ? b2b + 1 : 0,
        ++combo,
        board.empty()
    };
}

} // namespace Cobra