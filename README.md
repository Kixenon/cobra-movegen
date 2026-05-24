<div align="center">
    <h2>Cobra Movegen</h2>

    A high-performance bitboard-based move generator for tetromino stackers.
</div>

## Overview

This is the column-major version of [cobra-movegen](https://github.com/Kixenon/cobra-movegen). Although slightly slower, it may be more suitable for bot usage.

This project was originally derived from [Cobra](https://www.youtube.com/@cobra-tetris), but has since gone through major changes, with some reference from [fast-reachability](https://github.com/ImpleLee/fast-reachability/tree/avx512-perft).

Currently, it runs perft from an empty position, though this can be modified as needed in apps/bench.cpp.

## Features

- SRS/SRS+ rotation system
- 180 spins
- T-spin detection (full and mini)
- Configurable piece spawn (rising spawns, clutch clear)
- Full movegen (non-infinite SDF)
- Canonical, deduplicated moves
- Move input pathfinding with finesse
- Header-only library

Cobra Movegen achieves high single-thread performance through data-level parallelism (register-friendly wide operations), compile-time template specialization, and aggressive search-space pruning.

Benchmarked on an M2 MacBook Pro (results may vary):
```bash
./bin/bench IOLJSZT
Depth: 7 Nodes: 2647076135 Time: 8109ms NPS: 326396564
```

## Usage

- Requires C++23
```bash
make help # Shows build information
make bench && ./bin/bench <queue (e.g. "IOLJSZT") or "test">
```

If you wish to use this as a submodule, simply include headers in `src`.

Remember to define the rules! More information can be found in `src/ruleset.hpp`
```cpp
struct Rules : RulesetBase {
    static constexpr Policy::KickRule KICKS = Policy::KickRule::SRS;
    static constexpr Policy::SpinRule SPINS = Policy::SpinRule::NONE;
    static constexpr int SPAWN_Y = 19;
    static constexpr bool ENABLE_180 = false;
};
```

## Links

- [YouTube Channel](https://www.youtube.com/@cobra-tetris)
- [TETR.IO Profile](https://ch.tetr.io/u/cobra)

## Credits

- **Kixenon** - Developer and maintainer
- Special thanks to **ImpleLee** and **Opyu**

## License

This project is open source and available under the Apache 2.0 License.