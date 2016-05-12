#!/bin/bash -ex

echo Create directories
mkdir -p ~/.hex
mkdir -p ~/.config/rebar3

echo Decrypt secrets
set +x
openssl aes-256-cbc -K $encrypted_fffbebb689b3_key -iv $encrypted_fffbebb689b3_iv -in scripts/hex.config.enc -out ~/.hex/hex.config -d
set -x

echo Create global config
echo '{plugins, [rebar3_hex]}.' > ~/.config/rebar3/rebar.config

echo Edit version tag in app.src
vi -e -c '%s/{vsn, *.*}/{vsn, "'${TRAVIS_TAG}'"}/g|w|q' src/dogstatsd.app.src

echo Publish to Hex
echo 'Y' | ./vendor/rebar3 hex publish

echo Done
