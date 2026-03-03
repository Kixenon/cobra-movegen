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

uint64_t perft(Board<>& b, const Piece* next, unsigned depth) {
    assert(next->is_ok());
    assert(depth > 0);

    if (depth == 1)
        return next->route([&]<Piece p>{
            const int h = b.max_y();
            const int h1 = BoardBase::height_target(h + p.h_gen());
            return BoardBase::route(h1, [&]<int H>{
                Board<H> b1 = b.template cast_height<H>();
                return static_cast<uint64_t>(MoveList<p, Board<H>>(b1, h).popcount());
            });
        });

    uint64_t nodes = 0;
    next->route([&]<Piece p>{
        const int h = b.max_y();
        const int h1 = BoardBase::height_target(h + p.h_gen());
        const int h2 = BoardBase::height_target(h + p.h_place());
        assert(h1 <= h2);

        BoardBase::route(h1, [&]<int H1>{
            assert(Board<H1>::is_ok_y_local(h));
            auto b1 = b.template cast_height<H1>();
            MoveList<p, Board<H1>>(b1, h).for_each_move([&]<Rotation r>(const int x, const int y) {
                // Difficult to abstract this into a single routing lambda without hurting performance
                // The compiler becomes blind to this easy-to-predict branch, and forcing inline breaks more things
                if (h1 == h2) {
                    Board<H1> b2 = b1;
                    b2.template do_move<p, r>(x, y);
                    Board nextBoard = b2.template cast_height<BoardBase::H>();
                    nodes += perft(nextBoard, next + 1, depth - 1);
                } else {
                    assert(h1 < h2);
                    BoardBase::route(h2, [&]<int H2>{
                        assert(Board<H2>::is_ok_y_local(h));
                        Board<H2> b2 = b1.template cast_height<H2>();
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
    for (const auto& [pieces, expected] : data) {
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

int main(int argc, char* argv[]) {
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

    return 0;
}