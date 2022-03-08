#!/bin/sh
set -e

example="$1"
example_roc="./examples/$example.roc"
if [ -f "$example_roc" ]; then
	./roc/target/release/roc --optimize $example_roc
else
	echo "$example is not an example!"
fi
