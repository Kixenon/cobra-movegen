#ifdef COBRA_COL_MAJOR
#include "../src/col/board.hpp"
#include "../src/col/movegen.hpp"
#else
#include "../src/row/board.hpp"
#include "../src/row/movegen.hpp"
#endif
#include "../src/header.hpp"
#include "../src/ruleset.hpp"

#include <algorithm>
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

using namespace Cobra;

namespace {

struct Rules : RulesetBase {
    static constexpr Policy::KickRule KICKS = Policy::KickRule::SRS;
    static constexpr Policy::SpinRule SPINS = Policy::SpinRule::NONE;
    static constexpr int SPAWN_Y = 19;
    static constexpr bool ENABLE_180 = false;
};
// struct Rules : RulesetBase {
//     static constexpr Policy::KickRule KICKS = Policy::KickRule::SRS_PLUS;
//     static constexpr Policy::SpinRule SPINS = Policy::SpinRule::TSPIN;
//     static constexpr int SPAWN_Y = 5;
//     static constexpr bool ENABLE_180 = true;
// };

uint64_t perft(Board<>& b, const Piece* next, unsigned depth) {
    assert(next->is_ok());
    assert(depth > 0);

    if (depth == 1)
        return next->route([&]<Piece p>{
            const int h = b.max_y();
            const int h1 = BoardBase::height_target(h + p.h_gen());
            return BoardBase::route(h1, [&]<int H>{
                auto b1 = b.template cast_height<H>();
                return static_cast<uint64_t>(MoveList<Rules, p, Board<H>>(b1, h).popcount());
            });
        });

    uint64_t nodes = 0;
    next->route([&]<Piece p>{
        const int h = b.max_y();
        const int h1 = BoardBase::height_target(h + p.h_gen());
        const int h2 = BoardBase::height_target(h + p.h_place());
        assert(h1 <= h2);

        // Experimentally, routing h1 + h1/h2 > h2 > h1 + h2
        BoardBase::route(h1, [&]<int H1>{
            assert(Board<H1>::is_ok_y_local(h));
            auto b1 = b.template cast_height<H1>();
            MoveList<Rules, p, Board<H1>>(b1, h).for_each_move([&]<Rotation r>(const int x, const int y, [[maybe_unused]] const SpinType s) {
                // Difficult to abstract this into a single routing lambda without hurting performance
                // The compiler becomes blind to this easy-to-predict branch, and forcing inline breaks more things
                // if (H1 == h2) { // <- This is consistently slower (~2% nps) even though H1 IS h1
                if (h1 == h2) {
                    auto b2 = b1;
                    b2.template do_move<p, r>(x, y);
                    Board nextBoard = b2.template cast_height<BoardBase::H>();
                    nodes += perft(nextBoard, next + 1, depth - 1);
                } else {
                    assert(h1 < h2);
                    BoardBase::route(h2, [&]<int H2>{
                        assert(Board<H2>::is_ok_y_local(h));
                        auto b2 = b1.template cast_height<H2>();
                        b2.template do_move<p, r>(x, y);
                        Board nextBoard = b2.template cast_height<BoardBase::H>();
                        nodes += perft(nextBoard, next + 1, depth - 1);
                    });
                }
            });
        });
    });

    return nodes;
}

Piece char_to_piece(const char c) {
    switch (c) {
        case 'I': return Piece::I;
        case 'O': return Piece::O;
        case 'L': return Piece::L;
        case 'J': return Piece::J;
        case 'S': return Piece::S;
        case 'Z': return Piece::Z;
        case 'T': return Piece::T;
        default: return Piece::NO_PIECE;
    }
}

std::vector<Piece> parse_queue(std::string_view s) {
    std::vector<Piece> q;
    q.reserve(s.size());
    for (const auto& c : s) {
        const Piece p = char_to_piece(c);
        if (!p.is_ok()) {
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
    const unsigned depth = static_cast<unsigned>(queue.size());
    const auto start = std::chrono::high_resolution_clock::now();
    const uint64_t result = perft(b, queue.data(), depth);
    const auto end = std::chrono::high_resolution_clock::now();
    const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    return {result, dt};
}

void test() {
    // (SRS, no tspin, spawn y = 19, no 180)
    constexpr std::array data = {
        std::pair{"IIIIII",  33325345ULL},
        std::pair{"IOLJSZT", 2647076135ULL},
        std::pair{"TIOLJSZ", 2785677550ULL},
        std::pair{"ZTIOLJS", 2741273038ULL},
        std::pair{"SZTIOLJ", 2740055656ULL},
        std::pair{"JSZTIOL", 2801460686ULL},
        std::pair{"LJSZTIO", 2852978763ULL},
        std::pair{"OLJSZTI", 2689379684ULL},
    };

    // (SRS+, tspin, spawn y = 5, 180)
    // constexpr std::array data = {
    //     std::pair{"IIIIII", 12955903ULL},
    //     std::pair{"IOLJSZT", 788332817ULL},
    //     std::pair{"TIOLJSZ", 908982457ULL},
    //     std::pair{"ZTIOLJS", 884231722ULL},
    //     std::pair{"SZTIOLJ", 822485640ULL},
    //     std::pair{"JSZTIOL", 735332135ULL},
    //     std::pair{"LJSZTIO", 648149538ULL},
    //     std::pair{"OLJSZTI", 721407228ULL},
    // };

    uint64_t totalNodes = 0;
    uint64_t totalTime = 0;
    const auto start = std::chrono::high_resolution_clock::now();
    for (const auto& [pieces, expected] : data) {
        const auto [nodes, time] = benchmark(pieces);
        totalNodes += nodes;
        totalTime += static_cast<uint64_t>(time);
        std::cout << "Testing piece: " << pieces
                  << ", expected: " << expected
                  << ", got: " << nodes
                  << ", time: " << time << "ms" << std::endl;
    }
    const auto end = std::chrono::high_resolution_clock::now();
    const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    std::cout << "\nTotal test time: " << totalTime << "ms | " << dt << "ms\n";
    std::cout << "Total nodes per second: " << (totalNodes * 1000) / std::max(totalTime, static_cast<uint64_t>(1)) << std::endl;
}

} // namespace

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <queue or \"test\">" << std::endl;
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

    return 0;
}