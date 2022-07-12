#!/bin/sh
set -e

example="$1"
shift 1
args="$@"
example_roc="examples/$example.roc"
if [ -f "$example_roc" ]; then
	roc $example_roc $args
else
	echo "$example is not an example!"
fi
