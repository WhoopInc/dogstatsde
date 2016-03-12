#!/bin/sh -ex

if [ $REBAR_VSN -eq 2 ]; then
    ./vendor/rebar compile
    ./vendor/rebar eunit
elif [ $REBAR_VSN -eq 3 ]; then
    ./vendor/rebar3 eunit
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
