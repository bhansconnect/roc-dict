#include <chrono>
#include <cstdint>
#include <iostream>

#include "absl/hash/internal/low_level_hash.h"

// This is not a super robust benchmark. Just copying what we do in roc.

int main() {
  int64_t iters;
  std::cin >> iters;

  uint64_t seed = 0x1234567890ABCDEF;
  uint64_t salt[5] = {
      0x243F6A8885A308D3, 0x13198A2E03707344, 0xA4093822299F31D0,
      0x082EFA98EC4E6C89, 0x452821E638D01377,
  };
  int64_t hash = 0xFEDCBA0987654321;
  auto start = std::chrono::high_resolution_clock::now();
  if (iters > 0) {
    for (int64_t i = 0; i < iters; ++i) {
      // We directly call absl low level hash to ensure it does the same thing
      // as the roc version.
      hash = absl::hash_internal::LowLevelHash(&hash, 8, seed, salt);
    }
  }
  std::cout << hash << '\n';
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(stop - start)
          .count();

  std::cout << "runtime: " << duration / 1000.0 << "ms" << '\n';
}