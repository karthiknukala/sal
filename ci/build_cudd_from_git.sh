#!/usr/bin/env bash
set -euo pipefail

PREFIX="${1:?usage: build_cudd_from_git.sh <prefix>}"

: "${CUDD_REPO:=https://github.com/cuddorg/cudd.git}"
: "${CUDD_REF:=3.0.0}"
: "${CUDD_SHA:=}"   # optional: set to verify exact commit

# If already installed, skip.
if [[ -f "$PREFIX/lib/libcudd.a" && -f "$PREFIX/include/cudd.h" ]]; then
  echo "CUDD already installed in $PREFIX"
  exit 0
fi

ROOT="$(pwd)"
WORK="${ROOT}/.tmp/cudd-src"

rm -rf "$WORK"
mkdir -p "$PREFIX" "${ROOT}/.tmp"

echo "Cloning CUDD: $CUDD_REPO @ $CUDD_REF"
git clone --depth 1 --branch "$CUDD_REF" "$CUDD_REPO" "$WORK"
cd "$WORK"

if [[ -n "$CUDD_SHA" ]]; then
  HEAD_SHA="$(git rev-parse HEAD)"
  if [[ "$HEAD_SHA" != "$CUDD_SHA" ]]; then
    echo "ERROR: CUDD_SHA mismatch. Expected $CUDD_SHA got $HEAD_SHA"
    exit 1
  fi
fi

# Build & install
# (CUDD ships a configure script; no autoreconf needed for releases.)
./configure --prefix="$PREFIX" --enable-silent-rules

JOBS="${JOBS:-}"
if [[ -z "$JOBS" ]]; then
  if command -v nproc >/dev/null 2>&1; then
    JOBS="$(nproc)"
  elif command -v sysctl >/dev/null 2>&1; then
    JOBS="$(sysctl -n hw.ncpu)"
  else
    JOBS="2"
  fi
fi

make -j"$JOBS"
make install

# --- IMPORTANT ---
# CUDD's install often only installs cudd/cudd.h.
# SAL may include headers from util/, st/, mtr/, epd/, dddmp/, etc.
# Copy all public headers into the prefix include directory.
mkdir -p "$PREFIX/include"

for d in cudd util st mtr epd dddmp cplusplus; do
  if [[ -d "$WORK/$d" ]]; then
    find "$WORK/$d" -maxdepth 1 -type f -name '*.h' -exec cp -f {} "$PREFIX/include/" \; || true
  fi
done

# Optional: install the generated config.h too (sometimes useful)
if [[ -f "$WORK/config.h" ]]; then
  cp -f "$WORK/config.h" "$PREFIX/include/cudd_config.h" || true
fi

# Optional: provide a minimal pkg-config file for libcudd
mkdir -p "$PREFIX/lib/pkgconfig"
cat > "$PREFIX/lib/pkgconfig/cudd.pc" <<EOF
prefix=${PREFIX}
exec_prefix=\${prefix}
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: cudd
Description: CUDD BDD package
Version: ${CUDD_REF}
Cflags: -I\${includedir}
Libs: -L\${libdir} -lcudd
EOF

echo "CUDD installed to $PREFIX"
