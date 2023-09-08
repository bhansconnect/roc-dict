#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>

#include "wyhash.h"

// This is not a super robust benchmark. Just copying what we do in roc.
const uint64_t seed = 0x526F6352616E643F;

uint64_t hash_bytes(uint8_t* data, size_t len) {
  uint64_t state = wyhash(data, len, seed, _wyp);
  return _wymix(seed, state);
}

uint64_t hash_unordered_bytes(std::vector<uint8_t> data) {
  uint64_t state = seed;
  uint64_t accum = 0;
  for (size_t i = 0; i < data.size(); ++i) {
    uint64_t x = _wymix(state, wyhash(&data[i], 1, seed, _wyp));
    uint64_t next_accum = accum + x;
    if (next_accum < accum) {
      ++next_accum;
    }
    accum = next_accum;
  }
  return hash_bytes((uint8_t*)&accum, 8);
}

int main() {
  std::vector<uint8_t> data;
  printf("[] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {0x42};
  printf("[0x42] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {0xFF, 0xFF};
  printf("[0xFF, 0xFF] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {0x36, 0xA7};
  printf("[0x36, 0xA7] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {0x00, 0x00, 0x00, 0x00};
  printf("[0x00, 0x00, 0x00, 0x00] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {0xA9, 0x2F, 0xEE, 0x21};
  printf("[0xA9, 0x2F, 0xEE, 0x21] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {0x5D, 0x66, 0xB1, 0x8F, 0x68, 0x44, 0xC7, 0x03, 0xE1, 0xDD, 0x23, 0x34, 0xBB, 0x9A, 0x42, 0xA7};
  printf("[0x5D, 0x66, 0xB1, 0x8F, 0x68, 0x44, 0xC7, 0x03, 0xE1, 0xDD, 0x23, 0x34, 0xBB, 0x9A, 0x42, 0xA7] -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  std::string str_data = "abcdefghijklmnopqrstuvwxyz";
  printf("\"abcdefghijklmnopqrstuvwxyz\" -> 0x%016llX\n", hash_bytes((uint8_t*)str_data.data(), str_data.size()));

  str_data = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  printf("\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\" -> 0x%016llX\n", hash_bytes((uint8_t*)str_data.data(), str_data.size()));

  str_data = "1234567890123456789012345678901234567890123456789012345678901234567890";
  printf("\"1234567890123456789012345678901234567890123456789012345678901234567890\" -> 0x%016llX\n", hash_bytes((uint8_t*)str_data.data(), str_data.size()));

  data.clear();
  data.resize(100, 0x77);
  printf("[0x77] * 100 -> 0x%016llX\n", hash_bytes(data.data(), data.size()));

  data = {8, 82, 3, 8, 24};
  printf("[8, 82, 3, 8, 24] unordered -> 0x%016llX\n", hash_unordered_bytes(data));
}