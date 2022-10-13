#include <cstdint>
#include <cstdio>
#include <string>

#include "wyhash.h"

// This is not a super robust benchmark. Just copying what we do in roc.

int main() {
  uint64_t seed = 0x526F6352616E643F;
  uint64_t state = seed;
  // std::string data =
  //     "1234567890123456789012345678901234567890123456789012345678901234567890";
  uint8_t data[5] = {8, 82, 3, 8, 24};
  // uint64_t data = 0xA736;
  uint64_t accum = 0;
  for (int i = 0; i < 5; ++i) {
    uint64_t x = _wymix(state, wyhash(&data[i], 1, seed, _wyp));
    uint64_t next_accum = accum + x;
    if (next_accum < accum) {
      ++next_accum;
    }
    accum = next_accum;
  }
  uint64_t hash = wyhash(&accum, 8, seed, _wyp);
  uint64_t new_state = _wymix(state, hash);
  printf(
      "old_state: 0x%016llX, seed: 0x%016llX, hash: "
      "0x%016llX, new_state: "
      "0x%016llX\n",
      state, seed, hash, new_state);
}