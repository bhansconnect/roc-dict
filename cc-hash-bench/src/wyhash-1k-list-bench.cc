#include <chrono>
#include <cstdint>
#include <iostream>

#include "wyhash.h"

// This is not a super robust benchmark. Just copying what we do in roc.
constexpr int SIZE = 1024;

int main() {
  uint64_t iters;
  std::cin >> iters;

  uint64_t seed = 0x1234567890ABCDEF;
  uint8_t data[SIZE];
  std::fill(data, data + SIZE, 0x77);
  int64_t hash = 0;
  auto start = std::chrono::high_resolution_clock::now();
  if (iters > 0) {
    for (int64_t remaining = iters; remaining > 0; --remaining) {
      data[0] = remaining & 0xFF;
      int64_t data_hash = wyhash(&data, SIZE, seed, _wyp);
      hash = wyhash64(hash, data_hash);
    }
  }
  std::cout << hash << '\n';
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(stop - start)
          .count();

  std::cout << "runtime: " << duration / 1000.0 << "ms" << '\n';
}