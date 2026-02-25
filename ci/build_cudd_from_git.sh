#!/usr/bin/env bash
set -euxo pipefail

PREFIX="${1:?usage: build_cudd_from_git.sh <prefix>}"

# Skip if already installed in prefix
if [[ -f "$PREFIX/lib/libcudd.a" || -f "$PREFIX/lib/libcudd.dylib" ]]; then
  echo "CUDD already present in $PREFIX"
  exit 0
fi

: "${CUDD_REPO:?set CUDD_REPO}"
: "${CUDD_REF:?set CUDD_REF}"
CUDD_SHA="${CUDD_SHA:-}"

mkdir -p "$PREFIX" "$PWD/.tmp"
cd "$PWD/.tmp"

rm -rf cudd-src
git clone --depth 1 --branch "$CUDD_REF" "$CUDD_REPO" cudd-src
cd cudd-src

if [[ -n "$CUDD_SHA" ]]; then
  test "$(git rev-parse HEAD)" = "$CUDD_SHA"
fi

./configure --prefix="$PREFIX" --enable-silent-rules
make -j2
make install
