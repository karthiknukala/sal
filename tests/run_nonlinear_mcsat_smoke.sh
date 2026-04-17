#!/bin/bash
#
# Optional smoke test for the sal-inf-bmc + Yices2/MCSAT nonlinear path.
#
# Usage:
#   YICES2_MCSAT_COMMAND=/path/to/yices ./tests/run_nonlinear_mcsat_smoke.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SAL_DIR="$(dirname "$SCRIPT_DIR")"
BIN_DIR="$SAL_DIR/bin"
EXAMPLES_DIR="$SAL_DIR/examples/nonlinear"
SALENV_SAFE="$BIN_DIR/salenv-safe"

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

TMP_HOME="$(mktemp -d "${TMPDIR:-/tmp}/sal-yices2-mcsat.XXXXXX")"
trap 'rm -rf "$TMP_HOME"' EXIT

printf '%s\n' "(sal/set-yices2-command! \"$YICES2_COMMAND\")" > "$TMP_HOME/.salrc"

echo "[INFO] Using temporary HOME: $TMP_HOME"
echo "[INFO] Using Yices2 command: $YICES2_COMMAND"

cd "$EXAMPLES_DIR"

HOME="$TMP_HOME" "$SALENV_SAFE" nonlinear_square "$SAL_DIR/src/sal-wfc-front-end.scm"
HOME="$TMP_HOME" "$SALENV_SAFE" nonlinear_product "$SAL_DIR/src/sal-wfc-front-end.scm"

HOME="$TMP_HOME" "$SALENV_SAFE" -s yices2 -d 2 nonlinear_square nonnegative "$SAL_DIR/src/sal-inf-bmc-front-end.scm"
HOME="$TMP_HOME" "$SALENV_SAFE" -s yices2 -d 1 nonlinear_square bad_bound "$SAL_DIR/src/sal-inf-bmc-front-end.scm"
HOME="$TMP_HOME" "$SALENV_SAFE" -s yices2 -d 2 nonlinear_product positive "$SAL_DIR/src/sal-inf-bmc-front-end.scm"
HOME="$TMP_HOME" "$SALENV_SAFE" -s yices2 -d 1 nonlinear_product bad_bound "$SAL_DIR/src/sal-inf-bmc-front-end.scm"
