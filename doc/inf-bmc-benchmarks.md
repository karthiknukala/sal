# `sal-inf-bmc` Example Benchmark Matrix

Date: 2026-04-08

This report records the broader `sal-inf-bmc` benchmark sweep over the
documented `examples/` queries, including both the native Yices-language
backends and the new SMT-LIB2 backend.

## Scope and Method

- Source set: every documented `sal-inf-bmc` command found under `examples/`.
- Normalization:
  - strip the documented solver selection (`-s ...`)
  - strip verbosity flags (`-v`, `--verbose`)
  - deduplicate by working directory plus normalized argument list
- Total normalized queries: 58
- Timeout: 60 seconds per solver/query pair
- Timing method: one timed run per solver/query pair
- Preserved options: all example-local options such as `--enable-ate`, `-i`,
  `-ice`, lemma lists, and depth bounds

The harness used for this sweep is
[tools/benchmark-inf-bmc-examples.py](/Users/e35480/projects/misc/sal/sal-3.3/tools/benchmark-inf-bmc-examples.py#L1).

## Solver Configurations

| Column | SAL invocation pattern | External solver command actually used | Version / path | Notes |
| --- | --- | --- | --- | --- |
| `Yices2` | `sal-inf-bmc -s yices2 ...` | `/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices --mode=one-shot --mcsat` | Yices 2.7.0, build date 2026-03-04 | Selected via `~/.salrc` using `(sal/set-yices2-command! ...)`. This is the native Yices-language frontend, not SMT-LIB2. |
| `SMTLIB2/Yices2` | `SAL_SMTLIB2_PROFILE=yices2 sal-inf-bmc -s smtlib2 ...` | `yices-smt2` | `/opt/homebrew/bin/yices-smt2`, Yices 2.7.0, build date 2025-07-28 | This is the SMT-LIB2 frontend using the built-in `yices2` profile from [smtlib2-interface.scm](/Users/e35480/projects/misc/sal/sal-3.3/src/smtlib2-interface.scm#L25). |
| `SMTLIB2/Z3` | `SAL_SMTLIB2_PROFILE=z3 sal-inf-bmc -s smtlib2 ...` | `z3 -smt2 pp.decimal=true pp.decimal_precision=20` | `/Users/e35480/.pyenv/shims/z3`, Z3 4.15.4 | SMT-LIB2 frontend using the built-in `z3` profile. |
| `ICS` | `sal-inf-bmc -s ics ...` | `~/projects/misc/ics-2.1.experimental/bin/arm-apple-darwin24.6.0/ics` | ICS 2.1.experimental, Fri Mar 6 18:19:26 PST 2026 | Selected via `~/.salrc` using `(sal/set-ics-command! ...)`. |
| `Yices 1.0` | `sal-inf-bmc -s yices ...` | `/Users/e35480/yices-1.0.40/bin/yices` | 1.0.40 | Selected via `~/.salrc` using `(sal/set-yices-command! ...)`. |

## Important Caveats

### Native `yices2` issues on `queue`, `stack`, and `pipeline`

- `queue` and `stack`:
  - native `yices2` fails before solving with
    `Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type.`
  - the immediate source is
    [sat-yices2-context.scm](/Users/e35480/projects/misc/sal/sal-3.3/src/sat-yices2-context.scm#L81),
    where `sal-type/display-yices2` rejects `<sal-subtype>` outright
  - the reported source location points into
    [sal-prelude.scm](/Users/e35480/projects/misc/sal/sal-3.3/src/sal-prelude.scm#L212),
    where `int-pred?` and the subtype-based integer tower are defined
  - these examples, especially with `--enable-ate`, trigger subtype-typed
    terms that the native Yices2 translator does not support

- `pipeline`:
  - native `yices2` gets through translation and solver execution, then fails
    while parsing the returned model:
    `Error: Unexpected output from Yices 2: parse error (unexpected token 'RP').`
  - in the observed run, the solver command completed successfully with exit
    code 0:
    `/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices --mode=one-shot --mcsat ...`
  - this indicates a frontend/parser mismatch in the legacy SAL
    Yices2-language interface, not a `yices` solver failure

- Additional debugging caveat:
  - when SAL verbosity is above 3, the native Yices2 interface appends
    `--verbose` in [yices2-interface.scm](/Users/e35480/projects/misc/sal/sal-3.3/src/yices2-interface.scm#L246)
  - the local `yices` binary used here does not accept that flag
  - this did not affect the benchmark matrix, which was run at default SAL verbosity

### Why `SMTLIB2/Yices2` used a non-MCSAT `yices-smt2`

This was not an intentional attempt to handicap the SMT-LIB2 backend. The
SMT-LIB2 `yices2` profile is currently defined as plain `yices-smt2` in
[smtlib2-interface.scm](/Users/e35480/projects/misc/sal/sal-3.3/src/smtlib2-interface.scm#L25),
so the benchmark used the `yices-smt2` binary that was actually on `PATH`:
`/opt/homebrew/bin/yices-smt2`.

On this machine, that binary reports:

- `Yices 2.7.0`
- build date `2025-07-28`
- no MCSAT support for `QF_AUFNIRA`

That is why the four nonlinear rows fail for `SMTLIB2/Yices2` with
messages of the form:

- `logic QF_AUFNIRA is not supported since yices was not built with mcsat`

By contrast, the native `-s yices2` column used a different local binary,
`/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices`,
and SAL invoked it with `--mcsat`.

If we want the SMT-LIB2/Yices2 nonlinear rows to be apples-to-apples with
native `yices2`, we should point the SMT-LIB2 backend at an MCSAT-capable
SMT-LIB2 command via either:

- `SAL_SMTLIB2_COMMAND=...`
- `(sal/set-smtlib2-command! "...")`

### Shared or example-level caveats

- The 8 `examples/lsal/{queue,stack}` rows fail identically for every solver
  with:
  - `Error: Type "procedure" expected, "bbool" provided`
  - these are not backend-specific regressions
- The 5 `unexpected` rows are known k-induction/example-level outcomes rather
  than SMTLIB2-specific bugs
- The only successful-solver disagreements are:
  - `examples/queue :: -d 6 queue th1`
  - `examples/stack :: -d 6 stack th1`
  In both rows, `ICS` reports `counterexample` while the other successful
  solvers report `no counterexample`. These are the non-`--enable-ate` cases
  where ICS is already known to be weaker.

## Summary

| Solver | Successful rows | Errors | Timeouts | Unexpected |
| --- | ---: | ---: | ---: | ---: |
| `Yices2` | 36 | 17 | 0 | 5 |
| `SMTLIB2/Yices2` | 40 | 12 | 1 | 5 |
| `SMTLIB2/Z3` | 45 | 8 | 0 | 5 |
| `ICS` | 37 | 9 | 7 | 5 |
| `Yices 1.0` | 40 | 12 | 1 | 5 |

## Full Matrix

| Query | Yices2 | SMTLIB2/Yices2 | SMTLIB2/Z3 | ICS | Yices 1.0 |
| --- | --- | --- | --- | --- | --- |
| examples :: -d 0 activeSuspension3 reachable | no counterexample (0.67s) | no counterexample (0.52s) | no counterexample (0.96s) | no counterexample (0.58s) | no counterexample (0.62s) |
| examples :: -d 1 activeSuspension3 reachable | counterexample (0.73s) | counterexample (1.30s) | counterexample (2.83s) | timeout | counterexample (54.26s) |
| examples :: -d 0 activeSuspension3 reachable1 | no counterexample (0.84s) | no counterexample (0.89s) | no counterexample (1.66s) | no counterexample (0.88s) | no counterexample (0.85s) |
| examples :: -d 1 activeSuspension3 reachable1 | no counterexample (0.86s) | no counterexample (1.03s) | no counterexample (2.04s) | no counterexample (57.01s) | no counterexample (0.78s) |
| examples :: -d 2 activeSuspension3 reachable1 | counterexample (1.67s) | timeout | counterexample (15.19s) | timeout | timeout |
| examples :: -d 0 activeSuspension3 reachable2 | no counterexample (1.52s) | no counterexample (1.62s) | no counterexample (3.79s) | no counterexample (1.64s) | no counterexample (2.28s) |
| examples :: -d 1 activeSuspension3 reachable2 | no counterexample (1.58s) | no counterexample (1.75s) | no counterexample (3.26s) | timeout | no counterexample (1.95s) |
| examples :: -d 2 activeSuspension3 reachable2 | no counterexample (2.07s) | no counterexample (1.87s) | no counterexample (6.30s) | timeout | no counterexample (15.98s) |
| examples :: -d 3 activeSuspension3 reachable2 | no counterexample (11.30s) | no counterexample (12.40s) | no counterexample (22.03s) | timeout | no counterexample (6.67s) |
| examples/fischer :: fischer mutual_exclusion | no counterexample (3.06s) | no counterexample (1.02s) | no counterexample (2.41s) | no counterexample (2.96s) | no counterexample (1.55s) |
| examples/fischer :: -i -d 1 fischer time_aux1 | proved (1.05s) | proved (0.90s) | proved (2.43s) | proved (0.88s) | proved (1.26s) |
| examples/fischer :: -i -d 1 fischer time_aux2 | proved (1.18s) | proved (0.88s) | proved (2.50s) | proved (0.84s) | proved (1.45s) |
| examples/fischer :: -i -d 1 -l time_aux2 fischer time_aux3 | proved (1.04s) | proved (0.98s) | proved (5.53s) | proved (2.42s) | proved (2.55s) |
| examples/fischer :: -i -d 1 -l time_aux3 -l time_aux1 fischer logical_aux1 | proved (1.50s) | proved (0.71s) | proved (1.95s) | proved (0.71s) | proved (0.98s) |
| examples/fischer :: -i -d 0 -l logical_aux1 fischer mutual_exclusion | proved (0.85s) | proved (0.74s) | proved (1.88s) | proved (0.79s) | proved (0.95s) |
| examples/fischer :: -i -d 3 -l time_aux2 fischer logical_aux1 | unexpected: The context is unsat. No model. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. |
| examples/fischer :: -i -d 5 -l time_aux2 fischer mutex | unexpected: The context is unsat. No model. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. |
| examples/fischer :: -i -d 0 -l mutex fischer mutual_exclusion | proved (0.69s) | proved (1.01s) | proved (1.79s) | proved (0.66s) | proved (0.91s) |
| examples/fischer :: -i -d 9 -l time_aux2 fischer mutual_exclusion | unexpected: The context is unsat. No model. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. |
| examples/hybrid :: waterlevel prop | no counterexample (0.65s) | no counterexample (0.56s) | no counterexample (1.19s) | no counterexample (0.57s) | no counterexample (0.80s) |
| examples/hybrid :: gasburner prop3 | no counterexample (0.57s) | no counterexample (0.50s) | no counterexample (1.10s) | no counterexample (0.49s) | no counterexample (0.62s) |
| examples/hybrid :: pursuit safety | no counterexample (1.13s) | no counterexample (0.64s) | no counterexample (1.40s) | no counterexample (0.74s) | no counterexample (0.91s) |
| examples/inf-bakery :: inf_bakery mutex | no counterexample (0.77s) | no counterexample (0.55s) | no counterexample (1.09s) | no counterexample (1.00s) | no counterexample (0.71s) |
| examples/inf-bakery :: inf_bakery invalid | counterexample (0.55s) | counterexample (0.52s) | counterexample (1.16s) | counterexample (0.55s) | counterexample (0.93s) |
| examples/inf-bakery :: -d 1 -i inf_bakery aux1 | proved (0.78s) | proved (0.64s) | proved (1.60s) | proved (0.70s) | proved (0.97s) |
| examples/inf-bakery :: -d 1 -i inf_bakery aux2 | proved (0.82s) | proved (0.80s) | proved (1.88s) | proved (0.57s) | proved (0.85s) |
| examples/inf-bakery :: -d 3 -l aux1 -l aux2 -i inf_bakery mutex | proved (0.68s) | proved (0.69s) | proved (2.24s) | proved (0.72s) | proved (1.10s) |
| examples/inf-bakery :: -d 7 -i inf_bakery mutex | unexpected: The context is unsat. No model. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. |
| examples/lsal/inf-bakery :: inf-bakery mutex | no counterexample (0.73s) | no counterexample (0.65s) | no counterexample (1.63s) | no counterexample (1.18s) | no counterexample (0.73s) |
| examples/lsal/inf-bakery :: inf-bakery invalid | counterexample (0.51s) | counterexample (0.51s) | counterexample (1.06s) | counterexample (0.52s) | counterexample (0.89s) |
| examples/lsal/inf-bakery :: -d 1 -i inf-bakery aux1 | proved (0.79s) | proved (1.29s) | proved (2.66s) | proved (0.77s) | proved (1.01s) |
| examples/lsal/inf-bakery :: -d 1 -i inf-bakery aux2 | proved (0.72s) | proved (0.60s) | proved (2.19s) | proved (0.95s) | proved (1.72s) |
| examples/lsal/inf-bakery :: -d 3 -l aux1 -l aux2 -i inf-bakery mutex | proved (1.04s) | proved (0.88s) | proved (4.86s) | proved (1.73s) | proved (1.41s) |
| examples/lsal/inf-bakery :: -d 7 -i inf-bakery mutex | unexpected: The context is unsat. No model. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. | unexpected: k-induction rule failed, please try to increase the depth. |
| examples/lsal/queue :: -d 6 --enable-ate queue th1 | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/queue :: -d 6 queue th1 | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/queue :: -d 6 --enable-ate queue th2 | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/queue :: -d 6 --enable-ate queue invalid | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/stack :: -d 6 --enable-ate stack th1 | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/stack :: -d 6 stack th1 | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/stack :: -d 6 --enable-ate stack th2 | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/lsal/stack :: -d 6 --enable-ate stack invalid | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided | error: Error: Type "procedure" expected, "bbool" provided |
| examples/nonlinear :: -d 2 nonlinear_square nonnegative | no counterexample (0.88s) | error: Error: Unexpected SMT-LIB2 solver output: ((error logic QF_AUFNIRA is not supported since yices was not built with mcsat | no counterexample (1.27s) | error: Error: An incompleteness was detected in the decision procedure (or a bug in the translator). The counterexample produce | error: This statement should be included in your `.salrc' file in your home directory. |
| examples/nonlinear :: -d 1 nonlinear_square bad_bound | counterexample (0.62s) | error: Error: Unexpected SMT-LIB2 solver output: ((error logic QF_AUFNIRA is not supported since yices was not built with mcsat | counterexample (1.31s) | counterexample (0.75s) | error: This statement should be included in your `.salrc' file in your home directory. |
| examples/nonlinear :: -d 2 nonlinear_product positive | no counterexample (0.57s) | error: Error: Unexpected SMT-LIB2 solver output: ((error logic QF_AUFNIRA is not supported since yices was not built with mcsat | no counterexample (1.59s) | timeout | error: This statement should be included in your `.salrc' file in your home directory. |
| examples/nonlinear :: -d 1 nonlinear_product bad_bound | counterexample (0.67s) | error: Error: Unexpected SMT-LIB2 solver output: ((error logic QF_AUFNIRA is not supported since yices was not built with mcsat | counterexample (1.53s) | timeout | error: This statement should be included in your `.salrc' file in your home directory. |
| examples/pipeline :: --enable-ate -d 4 pipeline equivalent | error: Error: Unexpected output from Yices 2: parse error (unexpected token `RP'). Please contact support. | counterexample (1.05s) | counterexample (1.76s) | counterexample (1.00s) | counterexample (1.26s) |
| examples/queue :: -d 6 --enable-ate queue th1 | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | no counterexample (0.73s) | no counterexample (1.90s) | no counterexample (1.17s) | no counterexample (1.31s) |
| examples/queue :: -d 6 queue th1 | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | no counterexample (0.56s) | no counterexample (1.48s) | counterexample (0.74s) | no counterexample (0.99s) |
| examples/queue :: -d 6 --enable-ate queue th2 | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | no counterexample (0.85s) | no counterexample (1.53s) | no counterexample (0.89s) | no counterexample (1.50s) |
| examples/queue :: -d 6 --enable-ate queue invalid | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | counterexample (0.80s) | counterexample (1.59s) | counterexample (0.78s) | counterexample (1.12s) |
| examples/skdmxa :: -i -ice -d 1 -l lemma1 -l lemma2 --assertion=inf_skdmxa{2,2}!mutex | proved (1.65s) | proved (1.26s) | proved (2.60s) | proved (1.73s) | proved (1.83s) |
| examples/skdmxa :: -i -ice -d 1 --assertion=inf_skdmxa{2,2}!lemma1 | proved (1.49s) | proved (1.71s) | proved (2.75s) | proved (1.99s) | proved (2.18s) |
| examples/skdmxa :: -i -ice -d 1 -l lemma1 --assertion=inf_skdmxa{2,2}!lemma2 | proved (1.60s) | proved (1.27s) | proved (2.57s) | proved (1.71s) | proved (2.07s) |
| examples/stack :: -d 6 --enable-ate stack th1 | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | no counterexample (0.90s) | no counterexample (1.33s) | no counterexample (1.07s) | no counterexample (1.12s) |
| examples/stack :: -d 6 stack th1 | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | no counterexample (0.62s) | no counterexample (1.32s) | counterexample (1.15s) | no counterexample (0.98s) |
| examples/stack :: -d 6 --enable-ate stack th2 | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | no counterexample (0.83s) | no counterexample (1.63s) | no counterexample (0.89s) | no counterexample (1.12s) |
| examples/stack :: -d 6 --enable-ate stack invalid | error: Error: [Context: prelude, line(212), column(20)]: Can't be converted to a Yices 2 type. | counterexample (0.80s) | counterexample (1.74s) | counterexample (1.05s) | counterexample (1.19s) |
