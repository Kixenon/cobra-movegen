#ifndef RULESET_HPP
#define RULESET_HPP

#include "header.hpp"

#include <concepts>

namespace Cobra {

namespace Policy {

enum class KickRule {
    UNSPECIFIED,
    SRS,
    SRS_PLUS
};

enum class SpinRule {
    NONE,
    TSPIN
};

} // namespace Policy

struct RulesetBase {
    static constexpr Policy::KickRule KICKS = Policy::KickRule::UNSPECIFIED;
    static constexpr Policy::SpinRule SPINS = Policy::SpinRule::NONE;
    static constexpr int SPAWN_Y = -1;
    static constexpr bool ENABLE_180 = false;
};

template <typename R>
concept Ruleset = requires {
    { R::KICKS } -> std::convertible_to<Policy::KickRule>;
    { R::SPINS } -> std::convertible_to<Policy::SpinRule>;
    { R::SPAWN_Y } -> std::convertible_to<int>;
    { R::ENABLE_180 } -> std::convertible_to<bool>;
} && (R::KICKS != Policy::KickRule::UNSPECIFIED)
  && (R::SPAWN_Y >= 0 && R::SPAWN_Y < ROW_NB);

} // namespace Cobra

#endif