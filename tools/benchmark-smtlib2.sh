#!/bin/sh

set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
SAL_INF_BMC="$ROOT/bin/sal-inf-bmc"

benchmark_list() {
  cat <<EOF
inf_bakery invalid|$ROOT/examples/inf-bakery|inf_bakery invalid
inf_bakery mutex (k-induction)|$ROOT/examples/inf-bakery|-d 3 -l aux1 -l aux2 -i inf_bakery mutex
waterlevel prop|$ROOT/examples/hybrid|waterlevel prop
gasburner prop3|$ROOT/examples/hybrid|gasburner prop3
pursuit safety|$ROOT/examples/hybrid|pursuit safety
EOF
}

solver_available() {
  case "$1" in
    yices2) command -v yices-smt2 >/dev/null 2>&1 ;;
    z3) command -v z3 >/dev/null 2>&1 ;;
    *) return 1 ;;
  esac
}

normalize_result() {
  output_file=$1
  if grep -q '^Counterexample:' "$output_file"; then
    printf '%s\n' "counterexample"
  elif grep -q '^proved\.$' "$output_file"; then
    printf '%s\n' "proved"
  elif grep -q '^no counterexample between depths:' "$output_file"; then
    printf '%s\n' "no counterexample"
  else
    printf '%s\n' "unexpected"
  fi
}

run_once() {
  solver=$1
  workdir=$2
  shift 2
  output_file=$(mktemp)
  time_file=$(mktemp)
  status=0
  (
    cd "$workdir"
    SAL_SMTLIB2_PROFILE="$solver" /usr/bin/time -p "$SAL_INF_BMC" -s smtlib2 "$@" >"$output_file" 2>"$time_file"
  ) || status=$?
  real_time=$(awk '/^real / { print $2 }' "$time_file")
  result=$(normalize_result "$output_file")
  rm -f "$output_file" "$time_file"
  if [ "$status" -ne 0 ]; then
    printf 'error\t%s\n' "$status"
  else
    printf '%s\t%s\n' "$real_time" "$result"
  fi
}

median_of_three() {
  printf '%s\n%s\n%s\n' "$1" "$2" "$3" | sort -n | sed -n '2p'
}

printf '| Benchmark | Yices2 | Z3 |\n'
printf '| --- | --- | --- |\n'

benchmark_list | while IFS='|' read -r label workdir args; do
  yices_cell='N/A'
  z3_cell='N/A'

  for solver in yices2 z3; do
    if ! solver_available "$solver"; then
      continue
    fi

    # Warm the relevant solver/profile before collecting timings.
    # shellcheck disable=SC2086
    run_once "$solver" "$workdir" $args >/dev/null

    # shellcheck disable=SC2086
    r1=$(run_once "$solver" "$workdir" $args)
    # shellcheck disable=SC2086
    r2=$(run_once "$solver" "$workdir" $args)
    # shellcheck disable=SC2086
    r3=$(run_once "$solver" "$workdir" $args)

    t1=$(printf '%s\n' "$r1" | awk -F '\t' '{ print $1 }')
    t2=$(printf '%s\n' "$r2" | awk -F '\t' '{ print $1 }')
    t3=$(printf '%s\n' "$r3" | awk -F '\t' '{ print $1 }')
    s1=$(printf '%s\n' "$r1" | awk -F '\t' '{ print $2 }')
    s2=$(printf '%s\n' "$r2" | awk -F '\t' '{ print $2 }')
    s3=$(printf '%s\n' "$r3" | awk -F '\t' '{ print $2 }')

    if [ "$t1" = error ] || [ "$t2" = error ] || [ "$t3" = error ]; then
      cell='error'
    elif [ "$s1" != "$s2" ] || [ "$s1" != "$s3" ]; then
      cell='inconsistent'
    else
      median=$(median_of_three "$t1" "$t2" "$t3")
      cell=$(printf '%s (%ss)' "$s1" "$median")
    fi

    case "$solver" in
      yices2) yices_cell=$cell ;;
      z3) z3_cell=$cell ;;
    esac
  done

  printf '| %s | %s | %s |\n' "$label" "$yices_cell" "$z3_cell"
done
