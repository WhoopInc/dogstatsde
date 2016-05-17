#!/bin/bash -ex

if [[ $TRAVIS_TAG && ($TRAVIS_OTP_RELEASE != $MAIN_OTP || $REBAR_VSN -ne 3) ]]; then
    echo Skip deps for non-primary tests of a tag-build
    exit 0
fi

if [ $REBAR_VSN -eq 2 ]; then
    ./vendor/rebar get-deps
elif [ $REBAR_VSN -eq 3 ]; then
    echo no-op
elif [ $TRAVIS_ELIXIR_VERSION ]; then
    source "$HOME/.kiex/scripts/kiex"
    mix local.hex --force
    mix deps.get
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
