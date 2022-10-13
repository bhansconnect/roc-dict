project(
  'cc-hash-bench',
  'cpp',
  version : '0.1',
  default_options : [
    'warning_level=2',
    'cpp_std=c++20',
  ]
)

absl_proj = subproject('abseil-cpp')
absl_container_dep = absl_proj.get_variable('absl_container_dep')
absl_hash_dep = absl_proj.get_variable('absl_hash_dep')

executable(
  'wyhash-tests',
  'src/wyhash-tests.cc',
  dependencies: [],
  link_with: [],
)

executable(
  'wyhash-u64-bench',
  'src/wyhash-u64-bench.cc',
  dependencies: [],
  link_with: [],
)

executable(
  'wyhash-1k-list-bench',
  'src/wyhash-1k-list-bench.cc',
  dependencies: [],
  link_with: [],
)

executable(
  'abslhash-u64-bench',
  'src/abslhash-u64-bench.cc',
  dependencies: [absl_hash_dep],
  link_with: [],
)

executable(
  'abslhash-1k-list-bench',
  'src/abslhash-1k-list-bench.cc',
  dependencies: [absl_hash_dep],
  link_with: [],
)

executable(
  'hashdict-insert-remove-bench',
  'src/hashdict-insert-remove-bench.cc',
  dependencies: [absl_container_dep, absl_hash_dep],
  link_with: [],
)
