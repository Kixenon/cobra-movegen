#ifndef PATHFINDER_H
#define PATHFINDER_H

#include "board.hpp"
#include "header.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace Cobra {

namespace PathFinder {

constexpr int MAX_INPUTS = 64;

enum Input : uint8_t {
    NO_INPUT, SHIFT_LEFT, SHIFT_RIGHT, DAS_LEFT, DAS_RIGHT, ROTATE_CW, ROTATE_CCW, ROTATE_FLIP, SOFT_DROP, HARD_DROP
};

class Inputs {
private:
    Input inputs[MAX_INPUTS];
    size_t length = 0;

public:
    void reverse() { std::reverse(inputs, inputs + length); }
    Inputs& operator+=(const Input input) {
        assert(input);
        assert(size() < MAX_INPUTS);
        inputs[length++] = input;
        return *this;
    }
    size_t size() const { return length; }
    const Input* begin() const { return inputs; }
    const Input* end() const { return inputs + length; }
};

Inputs get_input(const Board& b, const Move& target, bool useFinesse, bool force = false);

} // namespace PathFinder

} // namespace Cobra

#endif