#include <chrono>
#include <cstdint>
#include <iostream>

#include "absl/container/flat_hash_map.h"

// This is not a super robust benchmark. Just copying what we do in roc.

int main() {
  uint64_t iters;
  std::cin >> iters;

  auto start = std::chrono::high_resolution_clock::now();
  absl::flat_hash_map<uint64_t, int64_t> map;
  int64_t accum = 0;
  if (iters > 0) {
    for (int64_t remaining = iters; remaining > 0; --remaining) {
      map.insert({static_cast<uint64_t>(remaining), remaining});
    }
    for (int64_t remaining = iters; remaining > 0; --remaining) {
      accum += map.erase(static_cast<uint64_t>(remaining));
    }
  }
  std::cout << accum << '\n';
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(stop - start)
          .count();

  std::cout << "runtime: " << duration / 1000.0 << "ms" << '\n';
}