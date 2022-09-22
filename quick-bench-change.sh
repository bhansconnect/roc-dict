#!/bin/sh

rm ./examples/hashdict-insert-remove-bench-base ./examples/hashdict-insert-remove-bench-change

git stash
roc build --optimize ./examples/hashdict-insert-remove-bench.roc
mv ./examples/hashdict-insert-remove-bench ./examples/hashdict-insert-remove-bench-base

git stash pop
roc build --optimize ./examples/hashdict-insert-remove-bench.roc
mv ./examples/hashdict-insert-remove-bench ./examples/hashdict-insert-remove-bench-change

hyperfine -w 10 -L version base,change "echo 1000000 | ./examples/hashdict-insert-remove-bench-{version}"