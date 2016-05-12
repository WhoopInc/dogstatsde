#!/bin/bash -ex

if [[ $TRAVIS_OTP_RELEASE != $MAIN_OTP ]]; then
    echo Skipping dialyzer for non-primary build
    exit 0
fi

if [ $REBAR_VSN -eq 2 ]; then
    echo "No dialyzer run under rebar2"
elif [ $REBAR_VSN -eq 3 ]; then
    ./vendor/rebar3 dialyzer
else
    echo Unknown rebar version requested: $REBAR_VSN
    exit 1
fi
