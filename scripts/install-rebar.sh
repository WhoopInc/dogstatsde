#!/bin/sh -ex

mkdir vendor
if [ $REBAR_VSN -eq 2 ]; then
    if [ x$(which rebar)x != xx ]; then
        ln -s $(which rebar) vendor/rebar
    else
        curl -L https://github.com/rebar/rebar/wiki/rebar -o vendor/rebar
        chmod a+x vendor/rebar
    fi
elif [ $REBAR_VSN -eq 3 ]; then
    if [ x$(which rebar3)x != xx ]; then
        ln -s $(which rebar3) vendor/rebar3
    else
        curl -L https://s3.amazonaws.com/rebar3/rebar3 -o vendor/rebar3
        chmod a+x vendor/rebar3
    fi
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
