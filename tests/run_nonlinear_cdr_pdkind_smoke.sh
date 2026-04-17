#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BIN_DIR="$ROOT_DIR/bin"
EXAMPLES_DIR="$ROOT_DIR/examples/nonlinear"
SAL_CDR="$BIN_DIR/sal-cdr"
SAL_WFC="$BIN_DIR/sal-wfc"

if [[ -z "${YICES2_MCSAT_COMMAND:-}" ]]; then
  echo "Set YICES2_MCSAT_COMMAND to an MCSAT-enabled Yices2 executable."
  exit 2
fi

normalize_yices2_command() {
  local cmd="$1"
  case "$cmd" in
    */yices_smt2|*/yices-smt2)
      local dir
      dir="$(dirname "$cmd")"
      if [[ -x "$dir/yices" ]]; then
        printf '%s\n' "$dir/yices"
        return 0
      fi
      ;;
  esac
  printf '%s\n' "$cmd"
}

YICES2_COMMAND="$(normalize_yices2_command "$YICES2_MCSAT_COMMAND")"

tmp_home="$(mktemp -d "${TMPDIR:-/tmp}/sal-cdr-pdkind-smoke.XXXXXX")"
cleanup() {
  rm -rf "$tmp_home"
}
trap cleanup EXIT

printf '(sal/set-yices2-command! "%s")\n' "$YICES2_COMMAND" >"$tmp_home/.salrc"

run_case() {
  local label="$1"
  shift
  local output

  echo "[INFO] $label"
  output=$(HOME="$tmp_home" "$SAL_CDR" -i "$@" 2>&1)
  printf '%s\n' "$output"

  if grep -qiE 'parse error|syntax error|segmentation fault|trace/breakpoint trap' <<<"$output"; then
    echo "[ERROR] Unexpected parser/runtime failure in: $label" >&2
    exit 1
  fi

  if ! grep -Eq '^(valid\.|invalid\.|unknown\.)$' <<<"$output"; then
    echo "[ERROR] Missing final SAL status in: $label" >&2
    exit 1
  fi
}

cd "$EXAMPLES_DIR"

HOME="$tmp_home" "$SAL_WFC" nonlinear_square.sal >/dev/null
HOME="$tmp_home" "$SAL_WFC" nonlinear_product.sal >/dev/null

run_case "sal-cdr -i nonlinear_square nonnegative" --solver=yices2 --to=2 nonlinear_square nonnegative
run_case "sal-cdr -i nonlinear_square bad_bound" --solver=yices2 --to=2 nonlinear_square bad_bound
run_case "sal-cdr -i nonlinear_product positive" --solver=yices2 --to=2 nonlinear_product positive
run_case "sal-cdr -i nonlinear_product bad_bound" --solver=yices2 --to=2 nonlinear_product bad_bound
