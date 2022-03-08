#!/bin/sh
set -e

nix-shell \
	--command 'cd roc && cargo build --release' \
	./roc/shell.nix
