#!/bin/bash

ELVIS_DIR="$HOME/.elvis"
ELVIS_BIN="$ELVIS_DIR/bin/elvis"
GITREPO_URL="https://github.com/inaka/elvis.git"

if [ -x "$ELVIS_BIN" ]
then
  echo "Elvis already installed"
  exit 0
else
  echo "Installing elvis..."
fi

git clone $GITREPO_URL $ELVIS_DIR
cd $ELVIS_DIR
rebar3 escriptize
mkdir bin
mv _build/default/bin/elvis ./bin
