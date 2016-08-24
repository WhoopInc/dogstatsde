#!/bin/bash -ex

if [[ $TRAVIS_TAG && ($TRAVIS_OTP_RELEASE != $MAIN_OTP || $REBAR_VSN -ne 3) ]]; then
    echo Skip installation for non-primary test of a tag-build
    exit 0
fi

if [ $ELIXIR_VSN ]; then
    ./scripts/kiex list known
    ./scripts/kiex install $ELIXIR_VSN
    exit 0
fi

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
