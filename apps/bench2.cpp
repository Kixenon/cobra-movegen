#include "../src2/board.hpp"
#include "../src2/header.hpp"
#include "../src2/movegen.hpp"

#include <array>
#include <cassert>
#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string_view>
#include <utility>
#include <vector>

using namespace Cobra2;

namespace {

// From https://github.com/facebook/folly/blob/7a3f5e4e81bc83a07036e2d1d99d6a5bf5932a48/folly/lang/Hint-inl.h#L107
// Apache License 2.0
template <class Tp>
inline void do_not_optimize(const Tp &value) {
    asm volatile("" : : "m"(value) : "memory");
}

uint64_t perft(Board<>& b, const Piece* next, unsigned depth) {
    assert(is_ok(*next));
    assert(depth > 0);

    if (depth == 1) {
        const MoveList moves(b, *next);
        do_not_optimize(moves);
        return static_cast<uint64_t>(moves.popcount());
    }

    uint64_t nodes = 0;
    const MoveList moves(b, *next);
    moves.for_each_move(*next,[&](const Move& move) {
        Board nextBoard = b;
        nextBoard.do_move(move);
        nodes += perft(nextBoard, next + 1, depth - 1);
    });

    return nodes;
}

Piece char_to_piece(const char c) {
    switch (c) {
        case 'I': return I;
        case 'O': return O;
        case 'L': return L;
        case 'J': return J;
        case 'S': return S;
        case 'Z': return Z;
        case 'T': return T;
        default: return PIECE_NB;
    }
}

std::vector<Piece> parse_queue(std::string_view s) {
    std::vector<Piece> q;
    q.reserve(s.size());
    for (const auto& c : s) {
        const Piece p = char_to_piece(c);
        if (!is_ok(p)) {
            std::cerr << "Invalid piece in queue: " << c << std::endl;
            std::exit(1);
        }
        q.push_back(p);
    }
    return q;
}

std::pair<uint64_t, int64_t> benchmark(std::string_view pieces) {
    Board b{};
    const std::vector<Piece> queue = parse_queue(pieces);
    const auto start = std::chrono::high_resolution_clock::now();
    const uint64_t result = perft(b, queue.data(), static_cast<unsigned>(queue.size()));
    const auto end = std::chrono::high_resolution_clock::now();
    const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    return {result, dt};
}

void test() {
    constexpr std::array data = {
        std::pair{"IIIIII", 33325345U},
        std::pair{"IOLJSZT", 2647076135U},
        std::pair{"TIOLJSZ", 2785677550U},
        std::pair{"ZTIOLJS", 2741273038U},
        std::pair{"SZTIOLJ", 2740055656U},
        std::pair{"JSZTIOL", 2801460686U},
        std::pair{"LJSZTIO", 2852978763U},
        std::pair{"OLJSZTI", 2689379684U},
    };

    int64_t totalTime = 0;
    const auto start = std::chrono::high_resolution_clock::now();
    for (const auto &[pieces, expected] : data) {
        const auto [nodes, time] = benchmark(pieces);
        totalTime += time;
        std::cout << "Testing piece: " << pieces
                  << ", expected: " << expected
                  << ", got: " << nodes
                  << ", time: " << time << "ms" << std::endl;
    }
    const auto end = std::chrono::high_resolution_clock::now();
    const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    std::cout << "\nTotal test time: " << totalTime << "ms | " << dt << "ms" << std::endl;
}

} // namespace

int main(int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <pieces or 'test'>" << std::endl;
        return 1;
    }

    if (std::strcmp(argv[1], "test") == 0)
        test();
    else {
        const auto [nodes, time] = benchmark(argv[1]);
        std::cout << "Depth: " << std::strlen(argv[1])
                  << " Nodes: " << nodes
                  << " Time: " << time << "ms"
                  << " NPS: " << (nodes * 1000) / static_cast<uint64_t>(time + 1) << std::endl;
    }

    // std::cout << "counter: " << Movegen::counter
    //           << "\ncounterA: " << Movegen::counterA
    //           << "\ncounterB: " << Movegen::counterB
    //           << "\ncounterS: " << Movegen::counterS
    //           << "\ncounterSA: " << Movegen::counterSA
    //           << "\ncounterF: " << Movegen::counterF
    //           << "\ncounterL: " << Movegen::counterL
    //           << "\ncounterLI: " << Movegen::counterLI
    //           << std::endl;
    return 0;
}