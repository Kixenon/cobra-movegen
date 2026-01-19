#include "../src/board.hpp"
#include "../src/header.hpp"
#include "../src/movegen.hpp"

#include <cassert>
#include <chrono>
#include <cstdint>
#include <iostream>

using namespace Cobra;

// From https://github.com/facebook/folly/blob/7a3f5e4e81bc83a07036e2d1d99d6a5bf5932a48/folly/lang/Hint-inl.h#L107
// Apache License 2.0
template <class Tp>
inline void do_not_optimize(const Tp &value) {
    asm volatile("" : : "m"(value) : "memory");
}

uint64_t perft(Board& b, const Piece* next, unsigned depth) {
    assert(is_ok(*next));
    assert(depth > 0);

    if (depth == 1) {
        const MoveList moves(b, *next);
        do_not_optimize(moves);
        return static_cast<uint64_t>(moves.size());
    }

    uint64_t nodes = 0;
    for (const Move& move : MoveList(b, *next)) {
        Board nextBoard = b;
        nextBoard.do_move(move);
        nodes += perft(nextBoard, next + 1, depth - 1);
    }

    return nodes;
}

int main() {
    // Depth should be <= the queue size, but that is left to the user
    constexpr unsigned depth = 7;
    const Piece queue[] = {I, O, L, J, S, Z, T};
    Board board;
    board.clear();

    const auto start = std::chrono::high_resolution_clock::now();

    const uint64_t nodes = perft(board, queue, depth);

    const auto end = std::chrono::high_resolution_clock::now();
    const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

    std::cout << "Depth: " << depth
              << " Nodes: " << nodes
              << " Time: " << dt << "ms"
              << " NPS: " << (nodes * 1000) / static_cast<uint64_t>(dt + 1) << std::endl;

    return 0;
}