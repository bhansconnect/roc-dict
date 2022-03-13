#!/bin/bash

SCRIPT_DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

(cd $SCRIPT_DIR && \
    ([ -d build ] || meson setup build --buildtype release) && \
    ninja -C build)
