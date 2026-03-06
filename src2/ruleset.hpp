#ifndef RULESET_HPP
#define RULESET_HPP

#include <concepts>
#include <cstdint>

namespace Cobra2 {

namespace Policy {

enum class KickRule : uint8_t {
    UNSPECIFIED,
    SRS,
    SRS_PLUS
};

} // namespace Policy

struct RulesetBase {
    static constexpr bool ENABLE_180 = false;
    static constexpr Policy::KickRule KICKS = Policy::KickRule::UNSPECIFIED;
    static constexpr int SPAWN_Y = -1;
};

template <typename R>
concept Ruleset = requires {
    { R::ENABLE_180 } -> std::convertible_to<bool>;
    { R::KICKS } -> std::convertible_to<Policy::KickRule>;
    { R::SPAWN_Y } -> std::convertible_to<int>;
} && (R::KICKS != Policy::KickRule::UNSPECIFIED)
  && (R::SPAWN_Y >= 0);

} // namespace Cobra2

#endif