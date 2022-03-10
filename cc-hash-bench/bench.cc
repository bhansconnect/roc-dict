#include <chrono>
#include <cstdint>
#include <iostream>

#include "wyhash.h"

// This is not a super robust benchmark. Just copying what we do in roc.

int main() {
  uint64_t iters;
  std::cin >> iters;

  uint64_t seed = 0x1234567890ABCDEF;
  int64_t hash = 0xFEDCBA0987654321;
  auto start = std::chrono::high_resolution_clock::now();
  for (int64_t i = 0; i < iters; ++i) {
    hash = wyhash(&hash, 8, seed, _wyp);
  }
  std::cout << hash << '\n';
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(stop - start)
          .count();

  std::cout << "runtime: " << duration / 1000.0 << "ms" << '\n';
}