# SAL Test Suite

This directory contains the comprehensive test suite for SAL (Symbolic Analysis Laboratory).

## Contents

- `TEST_CATALOGUE.md` - Complete catalogue of all SAL features, tools, options, edge cases, and test scenarios
- `run_tests.sh` - Automated test runner script
- `run_nonlinear_mcsat_smoke.sh` - Optional smoke test for `sal-inf-bmc` + Yices2/MCSAT nonlinear examples
- `run_cimatti_tacas17_nra_smoke.sh` - Regenerates and syntax-checks the 114 translated TACAS'17 NRA benchmarks, with optional `sal-inf-bmc` spot checks
- `run_cimatti_tacas17_nra_kind_bench.py` - Runs `sal-inf-bmc -i` over the TACAS'17 NRA suite and prints paper-style family summaries
- `README.md` - This file

## Running Tests

### Quick Tests (Sanity Check)
```bash
./run_tests.sh --quick
```
Runs a quick sanity check to verify basic SAL functionality:
- Well-formedness checking
- Basic BMC and SMC tests
- Deadlock checking

### Full Test Suite
```bash
./run_tests.sh --full
```
Runs the comprehensive test suite including:
- All quick tests
- Path finder tests
- Explicit model checker tests
- Infinite-state BMC tests
- LTL to Büchi translation
- All available SAT/SMT solvers

### Solver Comparison Tests
```bash
./run_tests.sh --solver-test
```
Tests all available SAT and SMT solvers:
- Yices, Kissat, MiniSat, Lingeling, etc.
- ICS, Yices2, CVC Lite, etc.

### Native PDKIND Regression Tests

Build and run the checker from the repository root:

```bash
make YICES2_PREFIX=/path/to/mcsat-enabled-yices
bin/sal-cdr --statistics model.sal theorem
bin/sal-cdr --max-depth=10 model.sal theorem
```

When switching the configured Yices2 installation, rebuild its interface before
rebuilding SAL:

```bash
make -B YICES2_PREFIX=/path/to/mcsat-enabled-yices yices2-ffi
make YICES2_PREFIX=/path/to/mcsat-enabled-yices
```

The native build supports Darwin and Linux and isolates SAL's GMP allocation
hooks from Yices2. It requires shared MCSAT-enabled Yices2 and libpoly libraries;
static-only solver libraries and dynamic GMP overrides are rejected. Installed
`sal-cdr` needs those configured shared runtimes. Portability of the generated
installation archive is unverified.

`--max-depth` bounds the induction depth; exhausting the bound reports
`unknown`. The default, zero, leaves the depth unlimited.

Run the regression mode from this directory:

```bash
./run_tests.sh --cdr
```

Requires `sal-cdr` built against an MCSAT-enabled Yices2 library. This mode
generates 93 SAL models in a temporary directory, checks their well-formedness,
and compares `sal-cdr`'s semantic results with independently specified answers.
Forty finite transition graphs use breadth-first reachability as an oracle;
their counterexamples are also checked against the actual graph edges.

The remaining cases cover induction strengthening, nonlinear Cauchy-Schwarz,
deadlocks and empty initial sets, scalar and dependent types, signed integer
division/modulo, nested `LET` scope, array/record flattening, and inputs used in
initialization, properties, or primed transition constraints. Trace checks
require exact rational/algebraic values and consistent global constants. A
solver error or timeout fails the regression. `unknown` is accepted only
for the deliberately bounded `limited_strengthen` case (`--max-depth=1`); it
never counts as a proof.

Each invocation preserves the generated models, commands, checker output, and
full traces. The final line prints their directory. Set `SAL_CDR_LOG_DIR` to
choose a destination and `SAL_CDR_TIMEOUT` to change the per-command timeout
(20 seconds by default):

```bash
SAL_CDR_LOG_DIR=/tmp/sal-cdr-checks SAL_CDR_TIMEOUT=30 ./run_tests.sh --cdr
```

### Nonlinear Yices2/MCSAT Smoke Test
```bash
YICES2_MCSAT_COMMAND=/path/to/yices ./run_nonlinear_mcsat_smoke.sh
```
Runs the small nonlinear `sal-inf-bmc` examples in `examples/nonlinear/`
against an MCSAT-enabled Yices2 executable configured through a temporary
`.salrc`.

### TACAS'17 NRA Benchmark Smoke Test
```bash
./run_cimatti_tacas17_nra_smoke.sh
YICES2_MCSAT_COMMAND=/path/to/yices ./run_cimatti_tacas17_nra_smoke.sh
```
Regenerates the SAL translations for the 114-source `benchmarks/vmt/nra`
suite under `examples/cimatti-tacas17-nra/`, syntax-checks every generated
SAL file, and optionally runs two representative `sal-inf-bmc` cases against
an MCSAT-capable Yices2 binary.

### TACAS'17 NRA k-Induction Comparison Harness
```bash
python3 ./run_cimatti_tacas17_nra_kind_bench.py \
  --yices2 /path/to/yices \
  --known-only \
  --timeout-s 20 \
  --max-k 32
```
Reads `examples/cimatti-tacas17-nra/benchmark_metadata.tsv`, runs
`sal-inf-bmc -i -it` on the selected benchmarks, writes a per-benchmark TSV,
and prints family summaries that can be compared with the 2018/2021 paper
tables checked in under `examples/cimatti-tacas17-nra/`.

## Test Catalogue

See `TEST_CATALOGUE.md` for:

1. **Tool-specific tests** - Detailed test cases for each SAL tool
2. **SAT/SMT solver integrations** - Tests for all supported solvers
3. **Language features** - Tests for SAL language constructs
4. **Property types** - Safety, liveness, LTL, and CTL properties
5. **Code transformations** - CSE, simplification, slicing
6. **BDD options** - Reordering methods, caching, etc.
7. **Edge cases** - Error handling, capacity limits, invalid inputs
8. **Parametric contexts** - Scaling tests with different parameters

## Manual Testing

For interactive testing, use the SAL environment:
```bash
../bin/salenv
```

Then in the REPL:
```scheme
(sal/load-context! "peterson")
(sal/bmc! "peterson!mutex" :depth 10)
(sal/smc! "peterson!mutex")
(exit)
```

## Adding New Tests

To add new tests:

1. Add test cases to `TEST_CATALOGUE.md` with documentation
2. Add automated test function to `run_tests.sh`
3. Create any necessary test input files in `examples/`

## Requirements

- SAL 3.3 compiled and installed
- Bigloo 4.x runtime
- External solvers (optional):
  - Kissat
  - MiniSat
  - Lingeling
  - Yices2

## Exit Codes

- `0` - All tests passed
- `1` - One or more tests failed

## Output

The test runner provides colored output:
- 🟢 `[PASS]` - Test passed
- 🔴 `[FAIL]` - Test failed
- 🟡 `[SKIP]` - Test skipped (e.g., solver not installed)
- 🔵 `[INFO]` - Informational message

## Troubleshooting

### Tests fail with "command not found"
Ensure SAL is properly built:
```bash
cd ..
./configure
make
```

### External solver tests skip
Install the required solvers and ensure they're in your PATH:
```bash
brew install kissat minisat  # macOS
```

### Out of memory errors
Some tests with large state spaces may require more memory. Try:
```bash
ulimit -v unlimited
./run_tests.sh --full
```
