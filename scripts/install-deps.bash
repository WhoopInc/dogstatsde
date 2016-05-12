#!/bin/bash -ex

if [[ $TRAVIS_TAG && $TRAVIS_OTP_RELEASE != $MAIN_OTP ]]; then
    echo Skip deps for non-primary tests of a tag-build
    exit 0
fi

if [ $REBAR_VSN -eq 2 ]; then
    ./vendor/rebar get-deps
elif [ $REBAR_VSN -eq 3 ]; then
    echo no-op
elif [ $ELIXIR_VSN ]; then
    source "$HOME/.kiex/scripts/kiex"
    mix deps.get
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
