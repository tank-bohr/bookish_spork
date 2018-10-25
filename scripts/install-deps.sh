#!/bin/bash

set -ex

DEPS_BIN="$HOME/.deps-bin"
REBAR3_URL="https://s3.amazonaws.com/rebar3/rebar3"
REBAR3_BIN="$DEPS_BIN/rebar3"
ELVIS_GIT_REPO_URL="https://github.com/inaka/elvis.git"
ELVIS_DIR="$HOME/.elvis"
ELVIS_BIN="$DEPS_BIN/elvis"

if [ -z "$DEPS_BIN" ]; then
  echo "Environment variable DEPS_BIN must be set"
  return 1
fi

if [ ! -d "$DEPS_BIN" ]; then
  mkdir -p $DEPS_BIN
fi

# Install codecov
sudo pip install codecov

# Install the latest rebar3
if [ -x "$REBAR3_BIN" ]
then
  echo "rebar3 is already installed"
  $REBAR3_BIN version
else
  echo "Installing rebar3"
  wget $REBAR3_URL -O $REBAR3_BIN
  chmod +x $REBAR3_BIN
fi

# Install elvis
if [ -x "$ELVIS_BIN" ]
then
  echo "Elvis is already installed"
  $ELVIS_BIN --version
else
  echo "Installing elvis..."
  git clone $ELVIS_GIT_REPO_URL $ELVIS_DIR
  cd $ELVIS_DIR
  $REBAR3_BIN escriptize
  mkdir bin
  mv _build/default/bin/elvis $DEPS_BIN
  rm -rf $ELVIS_DIR
fi

echo "Done!"
