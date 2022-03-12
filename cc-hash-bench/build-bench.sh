#!/bin/sh

SCRIPT_DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

(cd $SCRIPT_DIR && \
    ([ -d build ] || meson setup build --buildtype release) && \
    meson compile -C build)