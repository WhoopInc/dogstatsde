#!/bin/sh -ex

if [ $REBAR_VSN -eq 2 ]; then
    ./vendor/rebar get-deps
elif [ $REBAR_VSN -eq 3 ]; then
    echo no-op
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
