#!/usr/bin/env bash

set -ex

## Add the rebar3 hex plugin to global
mkdir -p ~/.config/rebar3
echo '{plugins, [{rebar3_hex, "6.11.2"}]}.' > ~/.config/rebar3/rebar.config

rebar3 hex publish --yes --repo=hexpm
