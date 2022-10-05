#!/bin/bash

roc-debug build --optimize wyhash-1k-list-bench.roc
roc-debug build --optimize wyhash-1k-list-stateful-bench.roc
roc-debug build --optimize wyhash-1k-list-with-copy-bench.roc
hyperfine -L version "","-with-copy","-stateful" "./wyhash-1k-list{version}-bench <<<1000000"