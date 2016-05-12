#!/bin/bash -ex

if [[ $TRAVIS_TAG && $TRAVIS_OTP_RELEASE != $MAIN_OTP ]]; then
    echo Skip tests for non-primary tests of a tag-build
    exit 0
fi

if [ $REBAR_VSN -eq 2 ]; then
    ./vendor/rebar compile
    ./vendor/rebar eunit
elif [ $REBAR_VSN -eq 3 ]; then
    ./vendor/rebar3 eunit
elif [ $ELIXIR_VSN ]; then
    source "$HOME/.kiex/scripts/kiex"
    mix compile
    mix test
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
