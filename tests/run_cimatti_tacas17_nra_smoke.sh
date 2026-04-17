#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SUITE_DIR="$ROOT_DIR/examples/cimatti-tacas17-nra/sal"
GENERATOR="$ROOT_DIR/examples/cimatti-tacas17-nra/generate_sal_from_vmt.py"
VMT_DIR="$ROOT_DIR/examples/cimatti-tacas17-nra/vmt"

python3 "$GENERATOR" --src-dir "$VMT_DIR" --dst-dir "$SUITE_DIR" --clean

while IFS= read -r sal_file; do
  "$ROOT_DIR/bin/sal-wfc" "$sal_file" >/dev/null
done < <(find "$SUITE_DIR" -name '*.sal' | sort)

echo "Syntax check passed for $(find "$SUITE_DIR" -name '*.sal' | wc -l | tr -d ' ') translated benchmarks."

if [[ -z "${YICES2_MCSAT_COMMAND:-}" ]]; then
  echo "Set YICES2_MCSAT_COMMAND to also run sal-inf-bmc smoke cases."
  exit 0
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

tmp_home="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_home"
}
trap cleanup EXIT

printf '(sal/set-yices2-command! "%s")\n' "$YICES2_COMMAND" >"$tmp_home/.salrc"

echo "Running sal-inf-bmc on example_4..."
HOME="$tmp_home" "$ROOT_DIR/bin/sal-inf-bmc" -s yices2 -d 6 \
  "$SUITE_DIR/example_4.sal" property

echo
echo "Running sal-inf-bmc on keymaera_nonlinear1..."
HOME="$tmp_home" "$ROOT_DIR/bin/sal-inf-bmc" -s yices2 -d 3 \
  "$SUITE_DIR/keymaera_nonlinear1.sal" property

echo
echo "Running sal-cdr on example_4..."
HOME="$tmp_home" "$ROOT_DIR/bin/sal-cdr" --solver=yices2 --to=6 \
  "$SUITE_DIR/example_4.sal" property

echo
echo "Running sal-cdr -i on keymaera_nonlinear1..."
HOME="$tmp_home" "$ROOT_DIR/bin/sal-cdr" -i --solver=yices2 --to=6 \
  "$SUITE_DIR/keymaera_nonlinear1.sal" property
