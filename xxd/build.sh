#!/usr/bin/env bash
if [ ! -d bin ]; then
    mkdir -p bin
fi

odin build . -vet \
             -o:aggressive \
             -no-bounds-check \
             -disable-assert \
             -microarch:native \
             -out:bin/xxd

