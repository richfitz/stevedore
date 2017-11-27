#!/usr/bin/env bash
set -e
for d in `ls -d */`; do
    docker build --rm --tag richfitz/${d%%/} $d
done
