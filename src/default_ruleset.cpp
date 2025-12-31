#include "ruleset.hpp"

namespace Cobra {

// Defaults to TETR.IO S1
extern const __attribute__((weak)) Rules ACTIVE_RULES = {
    .enable180 = true,
    .enableTspin = true,
    .srsPlus = true,
    .spawnRow = 21
};

} // namespace Cobra