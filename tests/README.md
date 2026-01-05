# SAL Test Suite

This directory contains the comprehensive test suite for SAL (Symbolic Analysis Laboratory).

## Contents

- `TEST_CATALOGUE.md` - Complete catalogue of all SAL features, tools, options, edge cases, and test scenarios
- `run_tests.sh` - Automated test runner script
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

