#!/bin/sh

clang++ bench.cc -o bench -O3 -std=c++17 -stdlib=libc++ -g
clang++ bench-1k-list.cc -o bench-1k-list -O3 -std=c++17 -stdlib=libc++ -g
