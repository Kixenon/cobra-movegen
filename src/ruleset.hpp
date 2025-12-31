#ifndef RULESET_H
#define RULESET_H

namespace Cobra {

struct Rules {
    bool enable180;
    bool enableTspin;
    bool srsPlus;
    int spawnRow;
};

extern const Rules ACTIVE_RULES;

} // namespace Cobra

#endif