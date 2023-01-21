#!/usr/bin/env bash
make -r all

if [ $? -ne 0 ]; then
    exit $?
fi

/Applications/GSPlus.app/Contents/MacOS/gsplus
