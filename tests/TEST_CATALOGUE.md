# SAL Test Suite Catalogue

This document provides a comprehensive catalogue of all SAL features, tools, options, edge cases, and integrations to enable thorough testing.

## Table of Contents

1. [SAL Tools Overview](#sal-tools-overview)
2. [Tool-Specific Tests](#tool-specific-tests)
   - [sal-bmc](#sal-bmc---bounded-model-checker)
   - [sal-inf-bmc](#sal-inf-bmc---infinite-state-bounded-model-checker)
   - [sal-smc](#sal-smc---symbolic-model-checker)
   - [sal-wmc](#sal-wmc---witness-model-checker)
   - [sal-emc](#sal-emc---explicit-state-model-checker)
   - [sal-sim](#sal-sim---simulator)
   - [sal-deadlock-checker](#sal-deadlock-checker)
   - [sal-path-finder](#sal-path-finder)
   - [sal-path-explorer](#sal-path-explorer)
   - [sal-atg](#sal-atg---automated-test-generator)
   - [sal-wfc](#sal-wfc---well-formedness-checker)
   - [ltl2buchi](#ltl2buchi)
   - [lsal2xml](#lsal2xml)
   - [sal2bool](#sal2bool)
   - [salenv](#salenv---interactive-environment)
3. [SAT Solver Integrations](#sat-solver-integrations)
4. [SMT Solver Integrations](#smt-solver-integrations)
5. [Language Features](#language-features)
6. [Property Types](#property-types)
7. [Code Transformations](#code-transformations)
8. [BDD Options](#bdd-options)
9. [Edge Cases and Error Handling](#edge-cases-and-error-handling)
10. [Parametric Contexts](#parametric-contexts)
11. [Test Examples by Category](#test-examples-by-category)

---

## SAL Tools Overview

| Tool | Description | Primary Use |
|------|-------------|-------------|
| `sal-bmc` | Bounded Model Checker | Finite-state safety/liveness checking with SAT |
| `sal-inf-bmc` | Infinite-state BMC | Infinite-state systems with SMT solvers |
| `sal-smc` | Symbolic Model Checker | BDD-based model checking |
| `sal-wmc` | Witness Model Checker | Counterexample-guided abstraction |
| `sal-emc` | Explicit Model Checker | Explicit state enumeration |
| `sal-sim` | Simulator | Interactive simulation |
| `sal-deadlock-checker` | Deadlock Checker | Find deadlock states |
| `sal-path-finder` | Path Finder | Generate execution traces |
| `sal-path-explorer` | Path Explorer | Interactive path exploration |
| `sal-atg` | Test Generator | Automated test generation |
| `sal-wfc` | Well-formedness Checker | Syntax/type checking |
| `ltl2buchi` | LTL Translator | LTL to Büchi automata |
| `lsal2xml` | LSAL Parser | Convert LSAL to XML |
| `sal2bool` | Boolean Generator | Generate boolean transition relations |
| `salenv` | Interactive Environment | REPL for SAL |

---

## Tool-Specific Tests

### sal-bmc - Bounded Model Checker

#### Basic Options
```bash
# Help
sal-bmc --help
sal-bmc -?

# Version
sal-bmc -V
sal-bmc --version

# Verbosity levels
sal-bmc -v 0 peterson mutex      # Silent
sal-bmc -v 1 peterson mutex      # Normal
sal-bmc -v 2 peterson mutex      # Verbose
sal-bmc -v 3 peterson mutex      # Debug
sal-bmc -v 5 peterson mutex      # Very verbose
```

#### Depth Options
```bash
# Basic depth
sal-bmc -d 5 peterson mutex
sal-bmc --depth=10 peterson mutex

# Depth range
sal-bmc --from=0 --to=10 peterson mutex
sal-bmc --from=5 --to=20 peterson mutex

# Iterative deepening
sal-bmc -it peterson mutex
sal-bmc --iterative peterson mutex
```

#### Verification Modes
```bash
# Standard BMC
sal-bmc peterson mutex

# K-induction
sal-bmc -i peterson mutex
sal-bmc --induction peterson mutex
sal-bmc -i -d 5 peterson mutex    # With specific k

# With lemmas
sal-bmc -i -l lemma1 peterson mutex
sal-bmc --induction --lemma=aux_invariant peterson mutex
sal-bmc -i -l lemma1 -l lemma2 peterson mutex  # Multiple lemmas

# Acyclic paths only
sal-bmc --acyclic peterson mutex
sal-bmc --acyclic -d 20 peterson mutex
```

#### SAT Solver Selection
```bash
# Available solvers
sal-bmc -s yices peterson mutex       # Yices (default)
sal-bmc -s yices2 peterson mutex      # Yices 2
sal-bmc -s ics peterson mutex         # ICS
sal-bmc -s kissat peterson mutex      # Kissat
sal-bmc -s minisat peterson mutex     # MiniSat
sal-bmc -s lingeling peterson mutex   # Lingeling
sal-bmc -s zchaff peterson mutex      # zChaff
sal-bmc -s berkmin peterson mutex     # BerkMin
sal-bmc -s grasp peterson mutex       # GRASP
sal-bmc -s siege peterson mutex       # SIEGE
sal-bmc --solver=kissat peterson mutex
```

#### Qualified Assertions
```bash
# Parametric contexts
sal-bmc --assertion='bakery{5,15}!mutex'
sal-bmc --assertion='arbiter{10}!at_most_one_ack'
sal-bmc --assertion='fischer{2,3,5}!mutual_exclusion'
```

#### Counter-example Options
```bash
# Display induction counterexample
sal-bmc -ice peterson mutex
sal-bmc --display-induction-ce peterson mutex

# Delta path (show only changes)
sal-bmc --delta-path peterson invalid

# Hide local variables
sal-bmc --hide-locals peterson invalid
```

#### Expected Test Results
| Test | Example | Expected Result |
|------|---------|-----------------|
| Valid property | `sal-bmc -d 10 peterson mutex` | No counterexample |
| Invalid property | `sal-bmc -d 10 peterson invalid` | Counterexample found |
| Proven by induction | `sal-bmc -i peterson mutex` | Proved |
| Liveness bug | `sal-bmc bakery{3,7}!liveness_bug` | Counterexample found |

---

### sal-inf-bmc - Infinite-State Bounded Model Checker

#### Basic Usage
```bash
sal-inf-bmc -v 3 fischer mutual_exclusion
sal-inf-bmc -v 10 waterlevel prop
sal-inf-bmc -v 10 gasburner prop3
sal-inf-bmc -v 10 pursuit safety
```

#### SMT Solver Selection
```bash
sal-inf-bmc -s yices inf-bakery mutex      # Yices (default)
sal-inf-bmc -s yices2 inf-bakery mutex     # Yices 2
sal-inf-bmc -s ics inf-bakery mutex        # ICS
sal-inf-bmc -s cvcl inf-bakery mutex       # CVC Lite
sal-inf-bmc -s svc inf-bakery mutex        # SVC
```

#### Induction with Lemmas
```bash
sal-inf-bmc -v 10 -i -d 1 fischer time_aux1
sal-inf-bmc -v 10 -i -d 1 -l time_aux2 fischer time_aux3
sal-inf-bmc -v 10 -i -d 1 -l time_aux3 -l time_aux1 fischer logical_aux1
sal-inf-bmc -v 10 -i -d 0 -l logical_aux1 fischer mutual_exclusion
```

#### Hybrid Systems
```bash
sal-inf-bmc -v 10 waterlevel prop     # Water level monitor
sal-inf-bmc -v 10 gasburner prop3     # Gas burner leakage
sal-inf-bmc -v 10 pursuit safety      # Pursuit game
```

---

### sal-smc - Symbolic Model Checker

#### Basic Usage
```bash
sal-smc peterson mutex
sal-smc -v 3 peterson mutex
sal-smc sync_peterson mutex
```

#### Search Direction
```bash
sal-smc peterson mutex                # Forward search (default)
sal-smc --backward peterson mutex     # Backward search
sal-smc --backward --assertion='arbiter{20}!at_most_one_ack'
```

#### BDD Options
```bash
# Cluster size
sal-smc --cluster-size=4096 peterson mutex
sal-smc --cluster-size=8192 bakery{5,15}!liveness

# Monolithic transition relation
sal-smc --monolithic peterson mutex

# Disable counterexamples (faster)
sal-smc --disable-counter-examples peterson mutex

# Dynamic reordering
sal-smc --disable-bs-dyn-reorder peterson mutex
sal-smc --enable-bs-dyn-reorder peterson mutex
sal-smc --enable-fs-dyn-reorder peterson mutex

# Reordering methods
sal-smc --reorder-method=sift peterson mutex
sal-smc --reorder-method=annealing peterson mutex
sal-smc --reorder-method=genetic peterson mutex
sal-smc --reorder-method=window2 peterson mutex
sal-smc --reorder-method=window3 peterson mutex
sal-smc --reorder-method=window4 peterson mutex
```

#### Weight Parameters (IWLS95 Heuristic)
```bash
sal-smc -W1 6 peterson mutex    # Smoothed variables weight
sal-smc -W2 1 peterson mutex    # Support count weight
sal-smc -W3 1 peterson mutex    # Introduced variables weight
sal-smc -W4 2 peterson mutex    # Max variable position weight
```

#### Liveness Properties
```bash
sal-smc peterson liveness1
sal-smc peterson liveness2
sal-smc peterson liveness3
sal-smc peterson liveness4
sal-smc bakery{5,15}!liveness
```

#### Expected Test Results
| Test | Example | Expected Result |
|------|---------|-----------------|
| Safety property | `sal-smc peterson mutex` | Proved |
| Invalid safety | `sal-smc peterson invalid` | Counterexample |
| Liveness | `sal-smc peterson liveness1` | Proved |
| Liveness bug | `sal-smc peterson livenessbug1` | Counterexample |

---

### sal-wmc - Witness Model Checker

```bash
sal-wmc -v 3 --assertion='bakery{5,15}!mutex'
sal-wmc -v 3 --assertion='qlock1{2,2}!reachable'
```

---

### sal-emc - Explicit State Model Checker

#### Search Strategies
```bash
sal-emc peterson mutex                    # Default
sal-emc -s bfs peterson mutex             # Breadth-first search
sal-emc -s cacheless peterson mutex       # Random simulation
sal-emc -s dfs peterson mutex             # Depth-first search
```

#### Random Simulation
```bash
sal-emc -s cacheless -d 100 -n 20 bakery{5,15}!mutex  # 20 paths, 100 steps
sal-emc -s cacheless -g -n 20 -d 100 sats correctness1  # With type checking
```

#### Compilation and Symmetry
```bash
sal-emc -c peterson mutex                 # Dynamic compilation
sal-emc -y s_bakery{5,15}!mutex          # Symmetry reduction
```

#### Deadlock Checking
```bash
sal-emc -k peterson mutex                 # Check for deadlocks
sal-emc -v 3 -k sats correctness1
```

---

### sal-sim - Simulator

```bash
sal-sim peterson
# Interactive commands in simulator:
# (start-simulation! module)
# (step!)
# (print-state)
# (exit)
```

---

### sal-deadlock-checker

```bash
sal-deadlock-checker peterson system
sal-deadlock-checker -v 3 pathfinder system
sal-deadlock-checker -v 3 --module='bakery{5,15}!system'
sal-deadlock-checker -v 3 --module='qlock1{2,2}!system'
```

---

### sal-path-finder

```bash
sal-path-finder peterson process
sal-path-finder -v 3 pathfinder system
sal-path-finder -v 3 --depth=20 --module='bakery{4,15}!system'
sal-path-finder -v 3 --module='arbiter{20}!arbiter'
```

#### Solver Options
```bash
sal-path-finder -s kissat --module='bakery{5,15}!system'
sal-path-finder -s minisat --module='bakery{5,15}!system'
```

---

### sal-path-explorer

```bash
sal-path-explorer peterson process
sal-path-explorer -v 3 bakery{5,15}!system
```

---

### sal-atg - Automated Test Generator

```bash
sal-atg peterson process
sal-atg -v 3 --module='bakery{5,15}!system'
sal-atg -s kissat --module='arbiter{10}!arbiter'
```

---

### sal-wfc - Well-Formedness Checker

```bash
sal-wfc peterson
sal-wfc -v 3 peterson
sal-wfc ~/examples/peterson.sal
sal-wfc ../tmp/peterson.sal
```

---

### ltl2buchi

```bash
ltl2buchi peterson liveness1
ltl2buchi -v 3 peterson liveness1
ltl2buchi --assertion='bakery{5,15}!liveness'
ltl2buchi -dbo peterson liveness1  # Disable expensive optimizations
```

---

### lsal2xml

```bash
lsal2xml file.lsal
lsal2xml *.lsal
```

---

### sal2bool

```bash
sal2bool peterson mutex
sal2bool --output=output.bool peterson mutex
sal2bool --assertion='bakery{5,15}!mutex'
```

---

### salenv - Interactive Environment

```bash
salenv
salenv-safe     # Safe mode (no shell access)
```

#### REPL Commands
```scheme
;; Load context
(sal/load-context! "peterson")

;; Load assertion
(sal/bmc! "peterson!mutex" :depth 10)

;; Interactive model checking
(sal/smc! "peterson!mutex")

;; Simulator
(sal/simulator! "peterson!process")

;; Help
(help)
(exit)
```

---

## SAT Solver Integrations

| Solver | Option | Status | Notes |
|--------|--------|--------|-------|
| Yices | `-s yices` | Built-in | Default for sal-bmc |
| Yices2 | `-s yices2` | External | Recommended for complex problems |
| ICS | `-s ics` | Built-in | Default for sal-inf-bmc |
| Kissat | `-s kissat` | External | Modern, efficient |
| MiniSat | `-s minisat` | External | Widely used |
| Lingeling | `-s lingeling` | External | Parallel capable |
| zChaff | `-s zchaff` | External | Classic solver |
| BerkMin | `-s berkmin` | External | Requires license file |
| GRASP | `-s grasp` | External | Research tool |
| SIEGE | `-s siege` | External | Research tool |

### Solver Test Matrix
```bash
# Test all solvers with same problem
for solver in yices kissat minisat lingeling; do
  echo "Testing $solver..."
  sal-bmc -s $solver -d 5 peterson mutex
done
```

---

## SMT Solver Integrations

| Solver | Option | Use Case |
|--------|--------|----------|
| Yices | `-s yices` | Default for infinite-state |
| Yices2 | `-s yices2` | Improved performance |
| ICS | `-s ics` | Linear arithmetic |
| CVC Lite | `-s cvcl` | Multiple theories |
| SVC | `-s svc` | Stanford validator |

---

## Language Features

### Types
```sal
% Basic types
INTEGER, NATURAL, REAL, BOOLEAN

% Subrange types
[1..10]
[0..N]

% Array types
ARRAY [1..N] OF BOOLEAN
ARRAY INDEX OF VALUE_TYPE

% Record types
[# field1: TYPE1, field2: TYPE2 #]

% Enumeration types
{idle, waiting, critical}

% Tuple types
[TYPE1, TYPE2, TYPE3]

% SCALARSET (for symmetry reduction)
SCALARSET[1..N]
```

### Module Constructs
```sal
% Base modules
BEGIN ... END

% Composition
module1 || module2      % Asynchronous
module1 [] module2      % Synchronous

% Hiding
LOCAL var IN module

% Renaming
RENAME old TO new IN module
```

### Temporal Operators (LTL)
```sal
G(p)          % Globally
F(p)          % Eventually
X(p)          % Next
p U q         % Until
p R q         % Release
```

---

## Property Types

### Safety Properties (Invariants)
```sal
mutex: THEOREM system |- G(NOT (pc1 = critical AND pc2 = critical));
```

### Liveness Properties
```sal
liveness: THEOREM system |- G(pc1 = waiting => F(pc1 = critical));
```

### LTL Properties
```sal
always_eventually: THEOREM system |- G(F(pc = critical));
```

### CTL Properties (via LTL encoding)
```sal
% AG, AF, EG, EF via appropriate LTL encoding
```

---

## Code Transformations

### Common Subexpression Elimination
```bash
sal-bmc --enable-cse peterson mutex     # Enable (default)
sal-bmc --disable-cse peterson mutex    # Disable
```

### Simplifications
```bash
sal-bmc --enable-simp peterson mutex    # Enable (default)
sal-bmc --disable-simp peterson mutex   # Disable
```

### Slicing (Cone of Influence)
```bash
sal-bmc --enable-slicer peterson mutex  # Enable
sal-bmc --disable-slicer peterson mutex # Disable (default)
```

### Infinite Loop Detection
```bash
sal-bmc --inf-loop-threshold=1024 peterson mutex  # Default
sal-bmc --inf-loop-threshold=2048 peterson mutex  # Higher threshold
```

---

## BDD Options

### Reordering Methods
```bash
--reorder-method=sift
--reorder-method=annealing
--reorder-method=genetic
--reorder-method=window2
--reorder-method=window3
--reorder-method=window4
--reorder-method=none
```

### Variable Ordering
```bash
--var-order-file=order.ord
```

### Cache and Memory
```bash
--initial-bdd-cache-size=65536
--bdd-cache-ratio=4
```

---

## Edge Cases and Error Handling

### Parser Errors
```bash
# Missing context
sal-bmc nonexistent mutex
# Expected: Error message about unknown context

# Syntax errors
sal-wfc bad_syntax.sal
# Expected: Parse error with line/column

# Type errors
sal-wfc type_error.sal
# Expected: Type error message
```

### Solver Errors
```bash
# Missing external solver
sal-bmc -s nonexistent peterson mutex
# Expected: Solver not found error

# Solver timeout (if supported)
sal-bmc --timeout=10 complex_problem mutex
```

### Capacity Limits
```bash
# Very deep search
sal-bmc -d 1000 peterson mutex

# Large state space
sal-smc bakery{100,200}!mutex

# Out of memory (expected failure)
sal-smc --monolithic large_system!mutex
```

### Invalid Options
```bash
# Invalid solver
sal-bmc -s invalid peterson mutex

# Invalid depth
sal-bmc -d -5 peterson mutex

# Conflicting options
sal-bmc -i --from=5 peterson mutex  # from must be 0 for induction
```

---

## Parametric Contexts

### Single Parameter
```bash
sal-smc --assertion='arbiter{10}!at_most_one_ack'
sal-smc --assertion='arbiter{20}!at_most_one_ack'
sal-smc --assertion='arbiter{50}!at_most_one_ack'
```

### Multiple Parameters
```bash
sal-bmc --assertion='bakery{5,15}!mutex'      # N=5, B=15
sal-bmc --assertion='bakery{10,31}!mutex'     # N=10, B=31
sal-inf-bmc --assertion='fischer{2,3,5}!mutual_exclusion'
```

### Parameter Scaling Tests
```bash
# Performance regression testing
for n in 5 10 15 20 25; do
  time sal-smc --assertion="arbiter{$n}!at_most_one_ack"
done
```

---

## Test Examples by Category

### Mutual Exclusion Protocols
| Example | Context | Properties |
|---------|---------|------------|
| Peterson | `peterson` | `mutex`, `invalid`, `liveness1-4`, `livenessbug1` |
| Bakery | `bakery{N,B}` | `mutex`, `liveness`, `liveness_bug` |
| Fischer | `fischer` | `mutual_exclusion`, `time_aux1-3`, `logical_aux1` |
| Qlock | `qlock{N,L}` | `mutex`, `reachable`, `lofree` |

### Bus Arbitration
| Example | Context | Properties |
|---------|---------|------------|
| Arbiter | `arbiter{N}` | `at_most_one_ack`, `no_ack_without_request`, `at_most_one_token`, `every_request_is_eventually_acknowledged` |

### Hybrid Systems
| Example | Context | Properties |
|---------|---------|------------|
| Water Level | `waterlevel` | `prop` |
| Gas Burner | `gasburner` | `prop3` |
| Pursuit | `pursuit` | `safety` |

### Infinite-State Systems
| Example | Context | Properties |
|---------|---------|------------|
| Inf-Bakery | `inf-bakery` | `mutex` |
| Fischer | `fischer` | Multiple lemmas |

### Real-World Examples
| Example | Context | Description |
|---------|---------|-------------|
| PathFinder | `pathfinder` | Mars rover scheduling bug |
| SATS | `sats` | Air traffic concept |
| TTA-Startup | `tta-startup` | Time-triggered architecture |

---

## Complete Test Commands

### Quick Sanity Tests
```bash
# Basic BMC test
sal-bmc -d 5 peterson mutex

# Basic SMC test
sal-smc peterson mutex

# Basic deadlock check
sal-deadlock-checker peterson process

# Basic path finding
sal-path-finder -d 10 peterson process
```

### Comprehensive Tool Tests
```bash
#!/bin/bash
# Run comprehensive tests

EXAMPLES_DIR="examples"

echo "=== BMC Tests ==="
sal-bmc -d 10 $EXAMPLES_DIR/peterson/peterson mutex
sal-bmc -d 10 $EXAMPLES_DIR/peterson/peterson invalid
sal-bmc -i $EXAMPLES_DIR/peterson/peterson mutex

echo "=== SMC Tests ==="
sal-smc $EXAMPLES_DIR/peterson/peterson mutex
sal-smc --backward $EXAMPLES_DIR/peterson/peterson mutex

echo "=== Deadlock Tests ==="
sal-deadlock-checker $EXAMPLES_DIR/pathfinder/pathfinder system

echo "=== WFC Tests ==="
sal-wfc $EXAMPLES_DIR/peterson/peterson

echo "=== Parametric Tests ==="
sal-bmc --assertion='bakery{5,15}!mutex' $EXAMPLES_DIR/bakery/bakery.sal
sal-smc --assertion='arbiter{10}!at_most_one_ack' $EXAMPLES_DIR/arbiter/arbiter.sal
```

### Solver Comparison Tests
```bash
#!/bin/bash
# Compare solver performance

for solver in yices kissat minisat; do
  echo "Testing with $solver..."
  time sal-bmc -s $solver -d 10 peterson mutex
done
```

---

## Output Formats

### Pretty Printing Options
```bash
--sal-syntax          # SAL concrete syntax (default)
--lsal-syntax         # LSAL (Lisp-like) syntax
--full-qualified-names # Show full context names
--pp-max-width=120    # Line width
--pp-max-ribbon=60    # Ribbon width
--pp-max-depth=10     # Nesting depth before ellipsis
--pp-num-lines=100    # Lines before ellipsis
```

### Machine-Readable Output
```bash
--status-messages     # Enable structured status output
```

---

## Environment Variables

```bash
SAL_PATH              # Additional search paths for SAL files
SAL_COLLECT_CLASS_INFO # Enable class info collection (debugging)
```

---

## Performance Testing Guidelines

1. **Baseline Tests**: Establish baseline performance on reference problems
2. **Scaling Tests**: Test with increasing parameter sizes
3. **Memory Tests**: Monitor memory usage for large state spaces
4. **Timeout Tests**: Verify behavior under time constraints
5. **Solver Comparison**: Compare different solver backends

---

## Known Limitations

1. **Dependent types**: Not supported
2. **Infinite state**: Limited to specific tools (sal-inf-bmc)
3. **Recursive functions**: Must be statically unfoldable
4. **RENAME restrictions**: LHS must be a variable
5. **File formats**: SAL and LSAL only (no direct SMT-LIB input)

---

## Test Environment Setup

```bash
# Ensure all external solvers are installed
which kissat
which minisat
which lingeling

# Set PATH if needed
export PATH=/path/to/solvers:$PATH

# Run SAL
./bin/salenv
```

---

*Document Version: 1.0*
*SAL Version: 3.3*
*Last Updated: January 2026*

