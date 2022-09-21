#include <chrono>
#include <cstdint>
#include <iostream>

#include "absl/container/flat_hash_map.h"

// This is not a super robust benchmark. Just copying what we do in roc.

template <typename T>
struct HashFloodKey {
  HashFloodKey(T value) : value(value) {}

  T value;

  template <typename H>
  friend H AbslHashValue(H h, const HashFloodKey& key) {
    // This doesn't actaully hash anything. So all keys should have the same has
    // value. This should lead to worst case hash flooding.
    // return h;
    return H::combine(std::move(h), key.value);
  }

  friend bool operator==(const HashFloodKey& lhs, const HashFloodKey& rhs) {
    return lhs.value == rhs.value;
  }
};

int main() {
  uint64_t iters;
  std::cin >> iters;

  auto start = std::chrono::high_resolution_clock::now();
  absl::flat_hash_map<HashFloodKey<uint64_t>, int64_t> map;
  int64_t accum = 0;
  if (iters > 0) {
    for (int64_t remaining = iters; remaining > 0; --remaining) {
      map.insert({{static_cast<uint64_t>(remaining)}, remaining});
    }
    for (int64_t remaining = iters; remaining > 0; --remaining) {
      accum += map.erase({static_cast<uint64_t>(remaining)});
    }
  }
  std::cout << accum << '\n';
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::microseconds>(stop - start)
          .count();

  std::cout << "runtime: " << duration / 1000.0 << "ms" << '\n';
}