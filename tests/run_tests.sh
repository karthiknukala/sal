#!/bin/bash
#
# SAL Test Suite Runner
# Runs comprehensive tests for all SAL tools and features
#
# Usage: ./run_tests.sh [--quick|--full|--solver-test|--cdr|--help]
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SAL_DIR="$(dirname "$SCRIPT_DIR")"
BIN_DIR="$SAL_DIR/bin"
EXAMPLES_DIR="$SAL_DIR/examples"

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Default test mode
TEST_MODE="quick"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --quick)
            TEST_MODE="quick"
            shift
            ;;
        --full)
            TEST_MODE="full"
            shift
            ;;
        --cdr)
            TEST_MODE="cdr"
            shift
            ;;
        --solver-test)
            TEST_MODE="solver"
            shift
            ;;
        --help|-h)
            echo "SAL Test Suite Runner"
            echo ""
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --quick        Run quick sanity tests (default)"
            echo "  --full         Run comprehensive test suite"
            echo "  --solver-test  Test all available SAT/SMT solvers"
            echo "  --cdr          Check native PDKIND results and exact counterexamples"
            echo "  --help         Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Utility functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
}

log_skip() {
    echo -e "${YELLOW}[SKIP]${NC} $1"
}

log_section() {
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE} $1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

# Test runner function
run_test() {
    local test_name="$1"
    local test_cmd="$2"
    local expect_success="${3:-true}"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    
    log_info "Running: $test_name"
    
    if eval "$test_cmd" > /tmp/sal_test_output.txt 2>&1; then
        if [ "$expect_success" = "true" ]; then
            log_success "$test_name"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            log_fail "$test_name (expected failure but succeeded)"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            cat /tmp/sal_test_output.txt
            return 1
        fi
    else
        if [ "$expect_success" = "false" ]; then
            log_success "$test_name (expected failure)"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            log_fail "$test_name"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            cat /tmp/sal_test_output.txt
            return 1
        fi
    fi
}

# Check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check for required tools
check_prerequisites() {
    log_section "Checking Prerequisites"
    
    if [ ! -d "$BIN_DIR" ]; then
        log_fail "SAL bin directory not found: $BIN_DIR"
        exit 1
    fi
    
    if [ ! -x "$BIN_DIR/salenv" ]; then
        log_fail "salenv not found or not executable"
        exit 1
    fi
    
    log_success "SAL installation found at $SAL_DIR"
}

# Test sal-wfc (well-formedness checker)
test_sal_wfc() {
    log_section "Testing sal-wfc (Well-Formedness Checker)"
    
    cd "$EXAMPLES_DIR/peterson"
    run_test "WFC: peterson.sal" "$BIN_DIR/sal-wfc peterson"
    
    cd "$EXAMPLES_DIR/bakery"
    run_test "WFC: bakery.sal" "$BIN_DIR/sal-wfc bakery"
    
    cd "$EXAMPLES_DIR/arbiter"
    run_test "WFC: arbiter.sal" "$BIN_DIR/sal-wfc arbiter"
}

# Determine best available solver
get_default_solver() {
    if command_exists kissat; then
        echo "kissat"
    elif command_exists minisat; then
        echo "minisat"
    else
        echo "yices"  # Fallback, may not work on newer systems
    fi
}

# Test sal-bmc (bounded model checker)
test_sal_bmc() {
    log_section "Testing sal-bmc (Bounded Model Checker)"
    
    local solver=$(get_default_solver)
    log_info "Using solver: $solver"
    
    cd "$EXAMPLES_DIR/peterson"
    
    # Basic BMC tests
    run_test "BMC: peterson mutex (valid)" \
        "$BIN_DIR/sal-bmc -s $solver -d 5 peterson mutex"
    
    run_test "BMC: peterson invalid (counterexample)" \
        "$BIN_DIR/sal-bmc -s $solver -d 10 peterson invalid"
    
    # Induction test
    run_test "BMC: peterson mutex (k-induction)" \
        "$BIN_DIR/sal-bmc -s $solver -i peterson mutex"
    
    # Parametric context
    cd "$EXAMPLES_DIR/bakery"
    run_test "BMC: bakery{5,15} mutex" \
        "$BIN_DIR/sal-bmc -s $solver -d 5 --assertion='bakery{5,15}!mutex'"
}

# Test sal-smc (symbolic model checker)
test_sal_smc() {
    log_section "Testing sal-smc (Symbolic Model Checker)"
    
    cd "$EXAMPLES_DIR/peterson"
    
    run_test "SMC: peterson mutex" \
        "$BIN_DIR/sal-smc peterson mutex"
    
    run_test "SMC: peterson invalid (counterexample)" \
        "$BIN_DIR/sal-smc peterson invalid"
    
    run_test "SMC: peterson mutex (backward search)" \
        "$BIN_DIR/sal-smc --backward peterson mutex"
    
    # Liveness
    run_test "SMC: peterson liveness1" \
        "$BIN_DIR/sal-smc peterson liveness1"
    
    # Parametric
    cd "$EXAMPLES_DIR/arbiter"
    run_test "SMC: arbiter{10} at_most_one_ack" \
        "$BIN_DIR/sal-smc --assertion='arbiter{10}!at_most_one_ack'"
}

# Test sal-deadlock-checker
test_sal_deadlock() {
    log_section "Testing sal-deadlock-checker"
    
    cd "$EXAMPLES_DIR/path-finder"
    run_test "Deadlock: pathfinder system" \
        "$BIN_DIR/sal-deadlock-checker pathfinder system"
    
    cd "$EXAMPLES_DIR/bakery"
    run_test "Deadlock: bakery{3,7} system" \
        "$BIN_DIR/sal-deadlock-checker --module='bakery{3,7}!system'"
}

# Test sal-path-finder
test_sal_path_finder() {
    log_section "Testing sal-path-finder"
    
    local solver=$(get_default_solver)
    
    cd "$EXAMPLES_DIR/bakery"
    run_test "PathFinder: bakery{3,7} system" \
        "$BIN_DIR/sal-path-finder -s $solver -d 10 --module='bakery{3,7}!system'"
}

# Test sal-emc (explicit model checker)
test_sal_emc() {
    log_section "Testing sal-emc (Explicit Model Checker)"
    
    cd "$EXAMPLES_DIR/peterson"
    run_test "EMC: peterson mutex" \
        "$BIN_DIR/sal-emc peterson mutex"
    
    # Random simulation
    run_test "EMC: peterson mutex (random simulation)" \
        "$BIN_DIR/sal-emc -s cacheless -d 50 -n 5 peterson mutex"
}

# Test sal-inf-bmc (infinite-state BMC)
test_sal_inf_bmc() {
    log_section "Testing sal-inf-bmc (Infinite-State BMC)"
    
    cd "$EXAMPLES_DIR/inf-bakery"
    run_test "Inf-BMC: inf_bakery mutex" \
        "$BIN_DIR/sal-inf-bmc -d 5 inf_bakery mutex"
    
    cd "$EXAMPLES_DIR/hybrid"
    run_test "Inf-BMC: waterlevel prop" \
        "$BIN_DIR/sal-inf-bmc -d 5 waterlevel prop"
}

# Test ltl2buchi
test_ltl2buchi() {
    log_section "Testing ltl2buchi"
    
    cd "$EXAMPLES_DIR/peterson"
    run_test "LTL2Buchi: peterson liveness1" \
        "$BIN_DIR/ltl2buchi peterson liveness1"
}

# Test different SAT solvers
test_sat_solvers() {
    log_section "Testing SAT Solvers"
    
    cd "$EXAMPLES_DIR/peterson"
    
    # Test each available solver
    for solver in yices kissat minisat; do
        if command_exists "$solver" || [ "$solver" = "yices" ]; then
            run_test "SAT Solver: $solver" \
                "$BIN_DIR/sal-bmc -s $solver -d 5 peterson mutex"
        else
            log_skip "SAT Solver: $solver (not installed)"
            TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        fi
    done
}

# Test SMT solvers
test_smt_solvers() {
    log_section "Testing SMT Solvers"
    
    cd "$EXAMPLES_DIR/inf-bakery"
    
    for solver in yices ics; do
        run_test "SMT Solver: $solver" \
            "$BIN_DIR/sal-inf-bmc -s $solver -d 3 inf_bakery mutex"
    done
}

# Quick test suite
run_quick_tests() {
    log_section "Running Quick Test Suite"
    
    check_prerequisites
    test_sal_wfc
    test_sal_bmc
    test_sal_smc
    test_sal_deadlock
}

# Full test suite
run_full_tests() {
    log_section "Running Full Test Suite"
    
    check_prerequisites
    test_sal_wfc
    test_sal_bmc
    test_sal_smc
    test_sal_deadlock
    test_sal_path_finder
    test_sal_emc
    test_sal_inf_bmc
    test_ltl2buchi
    test_sat_solvers
    test_smt_solvers
}

# Solver comparison tests
run_solver_tests() {
    log_section "Running Solver Tests"
    
    check_prerequisites
    test_sat_solvers
    test_smt_solvers
}

# Print summary
print_summary() {
    log_section "Test Summary"
    
    echo ""
    echo "Tests Run:     $TESTS_RUN"
    echo -e "Tests Passed:  ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests Failed:  ${RED}$TESTS_FAILED${NC}"
    echo -e "Tests Skipped: ${YELLOW}$TESTS_SKIPPED${NC}"
    echo ""
    
    if [ $TESTS_FAILED -eq 0 ]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}Some tests failed.${NC}"
        return 1
    fi
}

# Native PDKIND checks use independent reachability oracles and exact traces.
# Fixtures stay embedded so the frontend does not require another source module.
run_cdr_tests() {
    python3 - "$BIN_DIR" <<'PY_CDR'
import json
import os
from pathlib import Path
import random
import re
import subprocess
import sys
import tempfile
from collections import deque
from fractions import Fraction

bin_dir = Path(sys.argv[1])
if not (bin_dir / "sal-cdr").is_file():
    sys.exit("Build sal-cdr with an MCSAT-enabled Yices2 library first.")
timeout = float(os.environ.get("SAL_CDR_TIMEOUT", "20"))
log_dir = Path(os.environ.get("SAL_CDR_LOG_DIR") or tempfile.mkdtemp(prefix="sal-cdr-regression-"))
log_dir.mkdir(parents=True, exist_ok=True)

# Arithmetic, deadlock, type, scoping, and input cases have explicit expected
# answers. The linear contraction and Cauchy-Schwarz examples correspond to
# SALLY's PDKIND regression examples (example2/3/4/6/8 and nra/cauchy-schwarz).
fixtures = [
    ('initial_bad', 'invalid', """initial_bad: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ x' = x --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x > 0);
END
"""),
    ('one_step_bad', 'invalid', """one_step_bad: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ x' = x + 1 --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x < 1);
END
"""),
    ('deep_bad', 'invalid', """deep_bad: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ x' = IF x < 7 THEN x + 1 ELSE x ENDIF --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x < 7);
END
"""),
    ('unbounded_safe', 'valid', """unbounded_safe: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ x' = x + 1 --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x >= 0);
END
"""),
    ('strengthen', 'valid', """strengthen: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL, y: REAL
    INITIALIZATION [ x = 0 AND y = 0 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE } ]
    TRANSITION [ x' = x + y AND y' = y + 1 --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x >= 0);
END
"""),
    ('nonlinear', 'valid', """nonlinear: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL, y: REAL
    INITIALIZATION [ x = 1 AND y = 1 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE } ]
    TRANSITION [ x' = x * y AND y' = y + 1 --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x >= 1);
END
"""),
    ('deadlock_safe', 'valid', """deadlock_safe: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ FALSE --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x = 0);
END
"""),
    ('deadlock_bad', 'invalid', """deadlock_bad: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 1 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ FALSE --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x = 0);
END
"""),
    ('unreachable_trap', 'valid', """unreachable_trap: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ (x = 0 AND x' = 0) OR (x = -1 AND x' = 1) --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x <= 0);
END
"""),
    ('nondeterministic', 'invalid', """nondeterministic: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ x = 0 --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ TRUE --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x >= 0);
END
"""),
    ('boolean_toggle', 'invalid', """boolean_toggle: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: BOOLEAN
    INITIALIZATION [ x = FALSE --> x IN { q_x: BOOLEAN | TRUE } ]
    TRANSITION [ x' = NOT x --> x' IN { q_x: BOOLEAN | TRUE } ]
  END;
  property: THEOREM system |- G(NOT x);
END
"""),
    ('empty_initial', 'valid', """empty_initial: CONTEXT = BEGIN
  system: MODULE = BEGIN
    OUTPUT x: REAL
    INITIALIZATION [ FALSE --> x IN { q_x: REAL | TRUE } ]
    TRANSITION [ x' = x + 1 --> x' IN { q_x: REAL | TRUE } ]
  END;
  property: THEOREM system |- G(x < 0);
END
"""),
    ('deadend_one', 'invalid', """deadend_one: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL
  INITIALIZATION x=0
  TRANSITION [ x < 1 --> x'=x+1 ]
 END;
 property: THEOREM system |- G(x /= 1);
END
"""),
    ('deadend_three', 'invalid', """deadend_three: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL
  INITIALIZATION x=0
  TRANSITION [ x < 3 --> x'=x+1 ]
 END;
 property: THEOREM system |- G(x /= 3);
END
"""),
    ('sally_2', 'valid', """sally_2: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL
  INITIALIZATION [ x = 0 AND y = 0 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE } ]
  TRANSITION [ x' = x + 1 AND y' = y + 1 --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x = y);
END
"""),
    ('sally_3_a', 'valid', """sally_3_a: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL, n: REAL
  INITIALIZATION [ x = 0 AND y = n AND n > 0 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE }; n IN { q_n: REAL | TRUE } ]
  TRANSITION [ x' = IF y = 0 THEN 0 ELSE x+1 ENDIF AND y' = IF y = 0 THEN x ELSE y-1 ENDIF AND n' = n --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE }; n' IN { q_n: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x+y=n);
END
"""),
    ('sally_3_b', 'invalid', """sally_3_b: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL, n: REAL
  INITIALIZATION [ x = 0 AND y = n AND n > 0 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE }; n IN { q_n: REAL | TRUE } ]
  TRANSITION [ x' = IF y <= 0 THEN 0 ELSE x+1 ENDIF AND y' = IF y <= 0 THEN x ELSE y-1 ENDIF AND n' = n --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE }; n' IN { q_n: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x+y=n);
END
"""),
    ('sally_4_a', 'valid', """sally_4_a: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL
  INITIALIZATION [ x=0 AND y<1 AND y > -1 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE } ]
  TRANSITION [ x'=(3/5)*x+(2/5)*y AND y'<1 AND y' > -1 --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x < 1 AND x > -1);
END
"""),
    ('sally_4_b', 'valid', """sally_4_b: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL
  INITIALIZATION [ x<1 AND x > -1 AND y<1 AND y > -1 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE } ]
  TRANSITION [ x'=(3/5)*x+(2/5)*y AND y'<1 AND y' > -1 --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x < 1 AND x > -1);
END
"""),
    ('sally_6_a', 'valid', """sally_6_a: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL, z: REAL
  INITIALIZATION [ x = -(9/10) AND y=0 AND z=9/10 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE }; z IN { q_z: REAL | TRUE } ]
  TRANSITION [ x'=(3/5)*x-(2/5)*y AND y'=(4/7)*y-(3/7)*z AND z'=(5/9)*z-(4/9)*x --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE }; z' IN { q_z: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x+y+z > -3 AND x+y+z < 3);
END
"""),
    ('sally_6_b', 'valid', """sally_6_b: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL, z: REAL
  INITIALIZATION [ x<1 AND x > -1 AND y<1 AND y > -1 AND z<1 AND z > -1 --> x IN { q_x: REAL | TRUE }; y IN { q_y: REAL | TRUE }; z IN { q_z: REAL | TRUE } ]
  TRANSITION [ x'=(3/5)*x-(2/5)*y AND y'=(4/7)*y-(3/7)*z AND z'=(5/9)*z-(4/9)*x --> x' IN { q_x: REAL | TRUE }; y' IN { q_y: REAL | TRUE }; z' IN { q_z: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x+y+z > -3 AND x+y+z < 3);
END
"""),
    ('sally_8', 'valid', """sally_8: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL
  INITIALIZATION [ x=0 --> x IN { q_x: REAL | TRUE } ]
  TRANSITION [ x'=-x --> x' IN { q_x: REAL | TRUE } ]
 END;
 property: THEOREM system |- G(x>=0);
END
"""),
    ('cauchy_schwarz', 'valid', """cauchy_schwarz: CONTEXT = BEGIN
 system: MODULE = BEGIN
  INPUT x: REAL, y: REAL
  OUTPUT s1: REAL, s2: REAL, s3: REAL
  INITIALIZATION s1=0; s2=0; s3=0
  TRANSITION [ TRUE --> s1'=s1+x*y; s2'=s2+x*x; s3'=s3+y*y ]
 END;
 property: THEOREM system |- G(s1*s1 <= s2*s3);
END
"""),
    ('cauchy_schwarz_strict', 'invalid', """cauchy_schwarz_strict: CONTEXT = BEGIN
 system: MODULE = BEGIN
  INPUT x: REAL, y: REAL
  OUTPUT s1: REAL, s2: REAL, s3: REAL
  INITIALIZATION s1=0; s2=0; s3=0
  TRANSITION [ TRUE --> s1'=s1+x*y; s2'=s2+x*x; s3'=s3+y*y ]
 END;
 property: THEOREM system |- G(s1*s1 < s2*s3);
END
"""),
    ('constant_true_nonempty', 'valid', """constant_true_nonempty: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ x=0 --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(TRUE);
END
"""),
    ('constant_false_nonempty', 'invalid', """constant_false_nonempty: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ x=0 --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(FALSE);
END
"""),
    ('constant_true_empty', 'valid', """constant_true_empty: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ FALSE --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(TRUE);
END
"""),
    ('constant_false_empty', 'valid', """constant_false_empty: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ FALSE --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(FALSE);
END
"""),
    ('fractional_trace', 'invalid', """fractional_trace: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ x=-(9/10) --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(x>=0);
END
"""),
    ('large_fraction_trace', 'invalid', """large_fraction_trace: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ x = -(123456789123456789/999999999999999999) --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(x>=0);
END
"""),
    ('algebraic_trace', 'invalid', """algebraic_trace: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: REAL INITIALIZATION [ x*x=2 AND x>0 --> x IN {q: REAL | TRUE} ] TRANSITION [ TRUE --> x'=x ] END;
 property: THEOREM system |- G(x<=1);
END
"""),
    ('enum_counter', 'invalid', """enum_counter: CONTEXT = BEGIN
 State: TYPE = {alpha, beta, gamma};
 system: MODULE = BEGIN OUTPUT x: State INITIALIZATION x=alpha TRANSITION [ x=alpha --> x'=beta [] x=beta --> x'=gamma [] x=gamma --> x'=alpha ] END;
 property: THEOREM system |- G(x /= beta);
END
"""),
    ('bounded_integer', 'invalid', """bounded_integer: CONTEXT = BEGIN
 system: MODULE = BEGIN OUTPUT x: [0..2] INITIALIZATION x=0 TRANSITION [ x<2 --> x'=x+1 [] x=2 --> x'=0 ] END;
 property: THEOREM system |- G(x /= 2);
END
"""),
    ('enum_strengthen', 'valid', """enum_strengthen: CONTEXT = BEGIN
 State: TYPE = {alpha, beta, gamma};
 system: MODULE = BEGIN
  OUTPUT q: State, r: BOOLEAN
  INITIALIZATION q=alpha; r=FALSE
  TRANSITION [ q=alpha --> q'=alpha; r'=FALSE [] q/=alpha --> q'=gamma; r'=TRUE ]
 END;
 property: THEOREM system |- G(NOT r);
END
"""),
    ('enum_safe', 'valid', """enum_safe: CONTEXT = BEGIN
 State: TYPE = {alpha, beta, gamma};
 system: MODULE = BEGIN
  OUTPUT q: State
  INITIALIZATION q=alpha
  TRANSITION [ q=alpha --> q'=beta [] q=beta --> q'=alpha [] q=gamma --> q'=gamma ]
 END;
 property: THEOREM system |- G(q/=gamma);
END
"""),
    ('dependent_bound', 'valid', """dependent_bound: CONTEXT = BEGIN
 bound: [1..3];
 system: MODULE = BEGIN
  OUTPUT x: [0..bound]
  INITIALIZATION x=0
  TRANSITION [ x<bound --> x'=x+1 [] x=bound --> x'=0 ]
 END;
 property: THEOREM system |- G(x<=bound);
END
"""),
    ('dependent_bad', 'invalid', """dependent_bad: CONTEXT = BEGIN
 bound: [1..3];
 system: MODULE = BEGIN
  OUTPUT x: [0..bound]
  INITIALIZATION x=0
  TRANSITION [ x<bound --> x'=x+1 [] x=bound --> x'=0 ]
 END;
 property: THEOREM system |- G(x<bound);
END
"""),
    ('signed_div_np', 'valid', """signed_div_np: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER
  INITIALIZATION x=-5
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G((x DIV (2)) = -2);
END
"""),
    ('signed_mod_np', 'valid', """signed_mod_np: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER
  INITIALIZATION x=-5
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G((x MOD (2)) = 1);
END
"""),
    ('signed_div_pn', 'valid', """signed_div_pn: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER
  INITIALIZATION x=5
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G((x DIV (-2)) = -2);
END
"""),
    ('signed_mod_pn', 'valid', """signed_mod_pn: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER
  INITIALIZATION x=5
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G((x MOD (-2)) = 1);
END
"""),
    ('signed_div_nn', 'valid', """signed_div_nn: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER
  INITIALIZATION x=-5
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G((x DIV (-2)) = 2);
END
"""),
    ('signed_mod_nn', 'valid', """signed_mod_nn: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER
  INITIALIZATION x=-5
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G((x MOD (-2)) = 1);
END
"""),
    ('let_shadow', 'valid', """let_shadow: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER, y: INTEGER
  INITIALIZATION x=0; y=0
  TRANSITION [ TRUE --> x' = LET a: INTEGER = x+1 IN (LET a: INTEGER = a+2 IN a); y'=y+3 ]
 END;
 property: THEOREM system |- G(x=y);
END
"""),
    ('let_prime', 'valid', """let_prime: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: INTEGER, y: INTEGER
  INITIALIZATION x=0; y=1
  TRANSITION [ (LET a: INTEGER = x' IN a=y) --> x' IN {q: INTEGER | TRUE}; y'=y+1 ]
 END;
 property: THEOREM system |- G(y=x+1);
END
"""),
    ('dependent_nondet', 'valid', """dependent_nondet: CONTEXT = BEGIN
 bound: [1..3];
 system: MODULE = BEGIN
  OUTPUT x: [0..bound]
  INITIALIZATION x IN {v: [0..bound] | TRUE}
  TRANSITION [ TRUE --> x'=x ]
 END;
 property: THEOREM system |- G(x<=bound);
END
"""),
    ('input_property', 'invalid', """input_property: CONTEXT = BEGIN
 system: MODULE = BEGIN
  INPUT u: BOOLEAN
  OUTPUT q: BOOLEAN
  INITIALIZATION q=FALSE
  TRANSITION [ TRUE --> q'=q ]
 END;
 property: THEOREM system |- G(u);
END
"""),
    ('input_initial', 'invalid', """input_initial: CONTEXT = BEGIN
 system: MODULE = BEGIN
  INPUT u: BOOLEAN
  OUTPUT q: BOOLEAN
  INITIALIZATION [ NOT u --> q=FALSE ]
  TRANSITION [ TRUE --> q'=u ]
 END;
 property: THEOREM system |- G(NOT q);
END
"""),
    ('input_primed_correlation', 'valid', """input_primed_correlation: CONTEXT = BEGIN
 system: MODULE = BEGIN
  INPUT u: BOOLEAN
  OUTPUT q: BOOLEAN, started: BOOLEAN, bad: BOOLEAN
  INITIALIZATION q=FALSE; started=FALSE; bad=FALSE
  TRANSITION [ u'=u --> q'=u; started'=TRUE; bad'=started AND (q/=u) ]
 END;
 property: THEOREM system |- G(NOT bad);
END
"""),
    ('input_square', 'valid', """input_square: CONTEXT = BEGIN
 system: MODULE = BEGIN
  INPUT u: REAL
  OUTPUT x: REAL
  INITIALIZATION x=0
  TRANSITION [ TRUE --> x'=u*u ]
 END;
 property: THEOREM system |- G(x>=0);
END
"""),
    ('record_trace', 'invalid', """record_trace: CONTEXT = BEGIN
 Pair: TYPE = [# low: BOOLEAN, high: BOOLEAN #];
 system: MODULE = BEGIN
  OUTPUT r: Pair
  INITIALIZATION r=(# low:=FALSE, high:=TRUE #)
  TRANSITION [ TRUE --> r'=(# low:=NOT r.low, high:=NOT r.high #) ]
 END;
 property: THEOREM system |- G(NOT r.low);
END
"""),
    ('array_safe', 'valid', """array_safe: CONTEXT = BEGIN
 Index: TYPE = [0..1];
 system: MODULE = BEGIN
  OUTPUT a: ARRAY Index OF BOOLEAN
  INITIALIZATION a=[ [i:Index] i=1 ]
  TRANSITION [ TRUE --> a'=[ [i:Index] NOT a[i] ] ]
 END;
 property: THEOREM system |- G(a[0]/=a[1]);
END
"""),
    ('limited_strengthen', 'unknown', """limited_strengthen: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL, y: REAL
  INITIALIZATION x=0; y=0
  TRANSITION [ TRUE --> x'=x+y; y'=y+1 ]
 END;
 property: THEOREM system |- G(x>=0);
END
"""),
]

# Generate forty finite graphs. BFS is independent of either SMT engine and
# checks both safe and unsafe systems, including unreachable cycles/deadlocks.
graphs = {}
for seed in range(40):
    rng = random.Random(seed)
    size = 4 + seed % 9
    edges = {(a, b) for a in range(size) for b in range(size) if rng.random() < 0.12}
    if seed % 3:
        edges.update((a, a) for a in range(size))
    edges = sorted(edges)
    reached, pending = {0}, deque([0])
    while pending:
        current = pending.popleft()
        for a, b in edges:
            if a == current and b not in reached:
                reached.add(b)
                pending.append(b)
    name, bad = f"graph_{seed:02d}", size - 1
    transition = " OR ".join(f"(x={a} AND x'={b})" for a, b in edges) or "FALSE"
    model = f"""{name}: CONTEXT = BEGIN
 system: MODULE = BEGIN
  OUTPUT x: REAL
  INITIALIZATION x=0
  TRANSITION [ {transition} --> x' IN {{q: REAL | TRUE}} ]
 END;
 property: THEOREM system |- G(x/={bad});
END
"""
    fixtures.append((name, "invalid" if bad in reached else "valid", model))
    graphs[name] = (set(edges), bad)


def invoke(command):
    try:
        result = subprocess.run(command, text=True, capture_output=True, timeout=timeout)
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired as error:
        stdout, stderr = error.stdout or b"", error.stderr or b""
        if isinstance(stdout, bytes):
            stdout = stdout.decode(errors="replace")
        if isinstance(stderr, bytes):
            stderr = stderr.decode(errors="replace")
        return None, stdout, stderr + f"\nTimed out after {timeout}s"


def semantic_result(stdout):
    if re.search(r"(?mi)^Counterexample\b|^invalid\.?\s*$", stdout):
        return "invalid"
    if re.search(r"(?m)^proved\.\s*$", stdout):
        return "valid"
    if re.search(r"(?m)^unknown\.\s*$", stdout):
        return "unknown"
    return "error"


def check_witness(name, stdout):
    if name in graphs:
        edges, bad = graphs[name]
        blocks = re.split(r"(?m)^Step (\d+):\s*$", stdout)
        indices = [int(i) for i in blocks[1::2]]
        if not indices or indices != list(range(len(indices))):
            return "counterexample step numbers are missing or discontinuous"
        values = []
        for block in blocks[2::2]:
            assignments = re.findall(r"(?m)^x = ([^\n]+)$", block)
            if len(assignments) != 1 or not re.fullmatch(r"-?\d+", assignments[0].strip()):
                return "counterexample step lacks one exact integer state"
            values.append(int(assignments[0]))
        if not values or values[0] != 0 or values[-1] != bad:
            return "counterexample endpoints do not match the graph"
        if any(edge not in edges for edge in zip(values, values[1:])):
            return "counterexample includes an edge absent from the graph"
    if name == "fractional_trace" and not re.search(r"(?m)^x = -9/10\s*$", stdout):
        return "fractional witness was not printed exactly"
    if name == "large_fraction_trace":
        exact = str(Fraction(-123456789123456789, 999999999999999999))
        if not re.search(r"(?m)^x = " + re.escape(exact) + r"\s*$", stdout):
            return "large rational witness was not printed exactly"
    if name == "algebraic_trace":
        interval = re.search(r"<1\*x\^2\s*\+\s*\(-2\),\s*\(([^,]+),\s*([^)]+)\)>", stdout)
        if "exact Yices2 values" not in stdout or not interval:
            return "algebraic witness lacks its exact polynomial and isolating interval"
        try:
            lower, upper = (Fraction(v.strip()) for v in interval.groups())
        except (ValueError, ZeroDivisionError):
            return "algebraic isolating interval is not rational"
        if not (0 <= lower < upper and lower * lower < 2 < upper * upper):
            return "algebraic interval does not select positive sqrt(2) above 1"
    if name == "dependent_bad":
        constants = re.findall(r"(?m)^bound = (.+);$", stdout)
        if len(constants) != 1 or constants[0] not in {"1", "2", "3"}:
            return "global bound must have one consistent value in [1..3]"
    if name == "input_initial":
        steps = re.split(r"(?m)^Step \d+:\s*$", stdout)[1:]
        values = [{k: v.lower() for k, v in re.findall(r"(?mi)^(u|q) = (true|false)\s*$", step)}
                  for step in steps]
        if any(set(value) != {"u", "q"} for value in values):
            return "counterexample omits an input or output assignment"
        if len(values) < 3 or values[0].get("u") != "false" or values[0].get("q") != "false":
            return "initial input constraint or minimum counterexample length was lost"
        if values[-1].get("q") != "true" or any(b.get("q") != a.get("u") for a, b in zip(values, values[1:])):
            return "counterexample violates q'=u"
    return None


failures, results = [], []
for name, expected, source in fixtures:
    assert expected in {"valid", "invalid"} or (name == "limited_strengthen" and expected == "unknown")
    model = log_dir / f"{name}.sal"
    model.write_text(source)
    options = ["--statistics"]
    if name == "limited_strengthen":
        options.append("--max-depth=1")
    commands = [[str(bin_dir / "sal-wfc"), str(model)],
                [str(bin_dir / "sal-cdr")] + options + [str(model), "property"]]
    record = {"name": name, "expected": expected, "commands": commands}
    code, stdout, stderr = invoke(commands[0])
    (log_dir / f"{name}.wfc.log").write_text(stdout + "\n" + stderr)
    problem = None
    if code != 0 or "Ok." not in stdout:
        problem = "SAL well-formedness check failed"
    else:
        code, stdout, stderr = invoke(commands[1])
        actual = semantic_result(stdout) if code is not None else "timeout"
        record.update(status=actual, exit_code=code)
        (log_dir / f"{name}.cdr.log").write_text(stdout + "\n" + stderr)
        if code != 0 or actual != expected:
            problem = f"expected {expected}, got {actual} (exit {code})"
        elif actual == "invalid":
            problem = check_witness(name, stdout)
    if problem:
        failures.append((name, problem))
        record["failure"] = problem
        print(f"[FAIL] {name}: {problem}", flush=True)
    else:
        print(f"[PASS] {name}: {expected}", flush=True)
    results.append(record)
(log_dir / "results.json").write_text(json.dumps(results, indent=2) + "\n")
print(f"\nPDKIND regressions: {len(fixtures) - len(failures)}/{len(fixtures)} passed.")
print(f"Models, commands, and full traces: {log_dir}")
sys.exit(bool(failures))
PY_CDR
}

# Main
main() {
    echo "SAL Test Suite"
    echo "=============="
    echo "Mode: $TEST_MODE"
    echo "SAL Directory: $SAL_DIR"
    echo ""
    
    case $TEST_MODE in
        cdr)
            run_cdr_tests
            return
            ;;
        quick)
            run_quick_tests
            ;;
        full)
            run_full_tests
            ;;
        solver)
            run_solver_tests
            ;;
    esac
    
    print_summary
}

# Run main
main

