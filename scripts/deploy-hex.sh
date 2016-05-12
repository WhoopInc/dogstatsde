#!/bin/sh -e


mkdir -p ~/.hex
mkdir -p ~/.config/rebar3

openssl aes-256-cbc -K $encrypted_fffbebb689b3_key -iv $encrypted_fffbebb689b3_iv -in scripts/hex.config.enc -out ~/.hex/hex.config -d

set -x

echo '{plugins, [rebar3_hex]}.' > ~/.config/rebar3/rebar.config

vi -e -c '%s/{vsn, *.*}/{vsn, "'${TRAVIS_TAG}'"}/g|w|q' src/dogstatsd.app.src

echo 'Y' | ./vendor/rebar hex publish
