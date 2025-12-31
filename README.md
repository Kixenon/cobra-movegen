<div align="center">
    <h2>Cobra Movegen</h2>

    A high-performance move generation implementation for tetromino stackers.
</div>

## Overview

This project is derived from [Cobra](https://www.youtube.com/@cobra-tetris).

Currently it runs perft from an empty position. (This may be modified as needed in apps/bench.cpp)

## Features:
- SRS/SRS+ rotation system
- 180 spins
- Tspin detections (Full and mini)
- Full movegen (Non-infinite SDF)
- Canonical, deduplicated moves
- Optimized for single-thread performance and speed

## Building

- Requires c++20
```bash
make help # Shows build information
make -j build
```

## Usage

If you wish to use this as a submodule, simply include `src`.

Remember to define `ACTIVE_RULES`!
```cpp
extern const Rules ACTIVE_RULES = {
    .enable180 = true,
    .enableTspin = true,
    .srsPlus = true,
    .spawnRow = 21
};
```

## Links

- [YouTube Channel](https://www.youtube.com/@cobra-tetris)
- [TETR.IO Profile](https://ch.tetr.io/u/cobra)

## Credits

- **Kixenon** - Developer and maintainer
- Special thanks to **Opyu**

## License

This project is open source and available under the Apache 2.0 License.