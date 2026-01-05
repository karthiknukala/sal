#!/bin/bash
#
# SAL Test Suite Runner
# Runs comprehensive tests for all SAL tools and features
#
# Usage: ./run_tests.sh [--quick|--full|--solver-test|--help]
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

# Main
main() {
    echo "SAL Test Suite"
    echo "=============="
    echo "Mode: $TEST_MODE"
    echo "SAL Directory: $SAL_DIR"
    echo ""
    
    case $TEST_MODE in
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

