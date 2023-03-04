#include <algorithm>
#include <atomic>
#include <cassert>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <random>
#include <string>
#include <thread>
#include <vector>

#include "wyhash.h"

bool ctrlc = false;

void signal_callback_handler(int) {
  ctrlc = true;
  signal(SIGINT, SIG_DFL);
}

struct StringGen {
  StringGen(uint8_t first_start, uint8_t first_end, uint8_t start, uint8_t end)
      : first_start(first_start), first_end(first_end), start(start), end(end) {
    assert(start <= end);
    assert(first_start <= first_end);
    assert(start <= first_start);
    assert(first_end <= end);
    data.push_back(first_start);
  }

  std::string data;
  uint8_t first_start;
  uint8_t first_end;
  uint8_t start;
  uint8_t end;
};

std::string& next(StringGen& sg) {
  int size = sg.data.size();
  for (int i = size - 1; i >= 0; --i) {
    ++sg.data[i];
    if (i == 0 && sg.data[0] > sg.first_end) {
      // The first dimensions has a limited range.
      sg.data[0] = sg.first_start;
      sg.data.push_back(sg.start);
    } else if (sg.data[i] > sg.end) {
      sg.data[i] = sg.start;
    } else {
      break;
    }
  }
  return sg.data;
}

int main() {
  const uint64_t seed = 0x526F6352616E643F;
  const uint64_t state = seed;

  uint32_t slots_log_2 = 0;
  fprintf(stderr, "input slots (in power of 2) of target dict: ");
  scanf("%u", &slots_log_2);
  uint32_t str_size = 0;
  fprintf(stderr, "size of string to use as a key: ");
  scanf("%u", &str_size);
  const size_t slots = 1 << slots_log_2;
  const size_t mask = slots - 1;
  // Each slot contains 8 items.
  // The max capaicity is 7/8 * slots * 8;
  const size_t max_cap = 7 * slots;
  fprintf(stderr, "\nLooking for collisions for dicts with %zu slots.\n",
          slots);
  fprintf(stderr, "Its max capacity before rehash is %zu elements.\n", max_cap);

  signal(SIGINT, signal_callback_handler);

  std::atomic_size_t index = 0;
  std::vector<std::string> out{max_cap};
  const uint8_t base_char = ' ';
  const uint8_t min_char = base_char + 1;
  const uint8_t max_char = 'Z';
  const uint8_t char_diff = max_char - min_char;

  uint32_t thread_count = std::thread::hardware_concurrency();
  bool complete = false;
  std::vector<std::thread> threads{thread_count};
  for (uint32_t i = 0; i < thread_count; ++i) {
    std::thread t(
        [&](int id) {
          auto data_gen = StringGen{
              static_cast<uint8_t>(min_char + (id * char_diff) / thread_count),
              static_cast<uint8_t>(min_char +
                                   (((id + 1) * char_diff) / thread_count) - 1),
              min_char,
              max_char,
          };
          std::string padded_data{};
          while (!complete) {
            // Prepadding makes equality cost more by ensure the size is the
            // same. Note: if the value is too small, it will crash.
            padded_data.resize(str_size, base_char);
            const auto data = next(data_gen);
            assert(data.size() <= padded_data.size());
            const auto offset = padded_data.size() - data.size();
            for (size_t i = 0; i < data.size(); ++i) {
              padded_data[offset + i] = data[i];
            }
            uint64_t hash =
                wyhash(padded_data.c_str(), padded_data.size(), seed, _wyp);
            uint64_t new_state = _wymix(state, hash);
            uint64_t h1 = new_state >> 7;
            uint64_t h2 = new_state & 0b01111111;
            uint64_t slot = h1 & mask;
            if (slot == 0 && h2 == 0) {
              // Only find items that collide with slot 0.
              size_t tmp = index++;
              if (tmp < max_cap) {
                out[tmp] = padded_data;
              } else {
                complete = true;
                // Avoid incrementing past max cap.
                index.store(max_cap);
              }
            }
            if (ctrlc) {
              break;
            }
          }
        },
        i);
    threads[i] = std::move(t);
  }
  for (uint32_t i = 0; i < thread_count; ++i) {
    threads[i].join();
  }
  fprintf(stderr, "Found %zu collisions for slot 0.\n", index.load());
  std::shuffle(out.begin(), out.end(),
               std::default_random_engine{static_cast<uint32_t>(std::rand())});
  for (auto& data : out) {
    printf("%s\n", data.c_str());
  }
  fflush(stderr);
  exit(0);
}
