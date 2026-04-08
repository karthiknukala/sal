#!/usr/bin/env python3

import argparse
import os
import re
import shlex
import shutil
import subprocess
import sys
import time
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path


@dataclass(frozen=True)
class Solver:
    key: str
    label: str
    args: tuple[str, ...]
    env: tuple[tuple[str, str], ...] = ()


@dataclass
class Query:
    workdir: Path
    args: tuple[str, ...]
    sources: list[str] = field(default_factory=list)

    @property
    def label(self) -> str:
        workdir_label = self.workdir.relative_to(REPO_ROOT).as_posix()
        return f"{workdir_label} :: {' '.join(self.args)}"


@dataclass
class RunResult:
    verdict: str
    elapsed: float | None = None
    detail: str | None = None

    def as_cell(self) -> str:
        if self.elapsed is not None and self.verdict not in ("timeout", "exception"):
            return f"{self.verdict} ({self.elapsed:.2f}s)"
        if self.detail:
            return f"{self.verdict}: {self.detail}"
        return self.verdict


REPO_ROOT = Path(__file__).resolve().parents[1]
EXAMPLES_ROOT = REPO_ROOT / "examples"
SAL_INF_BMC = REPO_ROOT / "bin" / "sal-inf-bmc"
COMMAND_RE = re.compile(r"sal-inf-bmc\b.*")
BENCHMARKABLE_DOCUMENTED_SOLVERS = {"yices", "yices2", "ics", "smtlib2"}
EXCEPTION_PATTERNS = (
    "k-induction rule failed, please try to increase the depth.",
    "feature not supported: non linear problem.",
    "An incompleteness was detected in the decision procedure",
    "logic QF_AUFNIRA is not supported since yices was not built with mcsat",
)


def default_smtlib2_yices_command() -> str | None:
    env_override = os.environ.get("SAL_BENCHMARK_SMTLIB2_YICES_COMMAND")
    if env_override:
        return env_override
    candidate_paths = (
        Path("/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices_smt2"),
        Path("/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices-smt2"),
    )
    for path in candidate_paths:
        if path.exists():
            return f"{path} --mcsat"
    for command_name in ("yices_smt2", "yices-smt2"):
        resolved = shutil.which(command_name)
        if resolved:
            return f"{resolved} --mcsat"
    return None


SMTLIB2_YICES_COMMAND = default_smtlib2_yices_command()

SOLVERS = (
    Solver("yices2", "Yices2", ("-s", "yices2")),
    Solver(
        "smtlib2-yices2",
        "SMTLIB2/Yices2",
        ("-s", "smtlib2"),
        ((("SAL_SMTLIB2_COMMAND", SMTLIB2_YICES_COMMAND),) if SMTLIB2_YICES_COMMAND else (("SAL_SMTLIB2_PROFILE", "yices2"),)),
    ),
    Solver("smtlib2-z3", "SMTLIB2/Z3", ("-s", "smtlib2"), (("SAL_SMTLIB2_PROFILE", "z3"),)),
    Solver("ics", "ICS", ("-s", "ics")),
    Solver("yices", "Yices 1.0", ("-s", "yices")),
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Benchmark documented sal-inf-bmc example queries across multiple solvers."
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=60.0,
        help="Per-query timeout in seconds (default: 60).",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Only run the first N normalized queries.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Optional file to receive the markdown table.",
    )
    parser.add_argument(
        "--solver",
        action="append",
        choices=tuple(solver.key for solver in SOLVERS),
        default=None,
        help="Restrict execution to one or more solver keys.",
    )
    return parser.parse_args()


def selected_solvers(args: argparse.Namespace) -> tuple[Solver, ...]:
    if not args.solver:
        return SOLVERS
    requested = set(args.solver)
    return tuple(solver for solver in SOLVERS if solver.key in requested)


def strip_known_options(tokens: list[str]) -> tuple[str, ...]:
    result: list[str] = []
    idx = 0
    while idx < len(tokens):
        token = tokens[idx]
        if token in ("-v", "--verbose"):
            idx += 2
            continue
        if token.startswith("--verbose="):
            idx += 1
            continue
        if token in ("-s", "--solver"):
            idx += 2
            continue
        if token.startswith("--solver="):
            idx += 1
            continue
        result.append(token)
        idx += 1
    return tuple(result)


def documented_solver(tokens: list[str]) -> str | None:
    idx = 0
    while idx < len(tokens):
        token = tokens[idx]
        if token in ("-s", "--solver") and idx + 1 < len(tokens):
            return tokens[idx + 1]
        if token.startswith("--solver="):
            return token.split("=", 1)[1]
        idx += 1
    return None


def extract_query(path: Path, lineno: int, line: str) -> tuple[tuple[str, ...], str] | None:
    match = COMMAND_RE.search(line)
    if not match:
        return None
    command = match.group(0).split("<---", 1)[0].strip()
    if command.endswith("..."):
        return None
    try:
        tokens = shlex.split(command)
    except ValueError:
        return None
    if not tokens or tokens[0] != "sal-inf-bmc":
        return None
    if len(tokens) > 1 and tokens[1] == "with":
        return None
    solver = documented_solver(tokens[1:])
    if solver is not None and solver not in BENCHMARKABLE_DOCUMENTED_SOLVERS:
        return None
    args = strip_known_options(tokens[1:])
    if not args:
        return None
    return args, f"{path.relative_to(REPO_ROOT)}:{lineno}"


def collect_queries() -> list[Query]:
    queries: OrderedDict[tuple[Path, tuple[str, ...]], Query] = OrderedDict()
    for path in sorted(EXAMPLES_ROOT.rglob("*")):
        if not path.is_file():
            continue
        try:
            lines = path.read_text(errors="ignore").splitlines()
        except OSError:
            continue
        for lineno, line in enumerate(lines, 1):
            extracted = extract_query(path, lineno, line)
            if extracted is None:
                continue
            args, source = extracted
            key = (path.parent.resolve(), args)
            if key not in queries:
                queries[key] = Query(workdir=path.parent.resolve(), args=args, sources=[source])
            else:
                queries[key].sources.append(source)
    return list(queries.values())


def normalize_verdict(stdout: str, stderr: str) -> str:
    combined = "\n".join(part for part in (stdout, stderr) if part)
    if "Counterexample:" in combined:
        return "counterexample"
    if re.search(r"^proved\.$", combined, flags=re.MULTILINE):
        return "proved"
    if "no counterexample between depths:" in combined:
        return "no counterexample"
    if re.search(r"^invalid\.$", combined, flags=re.MULTILINE):
        return "invalid"
    if any(pattern in combined for pattern in EXCEPTION_PATTERNS):
        return "exception"
    return "exception"


def summarize_error(stdout: str, stderr: str) -> str:
    combined = "\n".join(part for part in (stdout.strip(), stderr.strip()) if part)
    if not combined:
        return "no output"
    for line in reversed(combined.splitlines()):
        text = line.strip()
        if text:
            return text[:120]
    return "no output"


def run_query(query: Query, solver: Solver, timeout_s: float) -> RunResult:
    cmd = [str(SAL_INF_BMC), *solver.args, *query.args]
    env = os.environ.copy()
    env.update(dict(solver.env))
    started = time.perf_counter()
    try:
        proc = subprocess.run(
            cmd,
            cwd=query.workdir,
            env=env,
            capture_output=True,
            text=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired:
        return RunResult("timeout")
    elapsed = time.perf_counter() - started
    verdict = normalize_verdict(proc.stdout, proc.stderr)
    if verdict in ("counterexample", "proved", "no counterexample", "invalid"):
        return RunResult(verdict, elapsed=elapsed)
    return RunResult("exception", detail=summarize_error(proc.stdout, proc.stderr))


def render_table(
    queries: list[Query], solvers: tuple[Solver, ...], results: dict[tuple[int, int], RunResult]
) -> str:
    headers = ["Query", *[solver.label for solver in solvers]]
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(["---"] * len(headers)) + " |",
    ]
    for query_idx, query in enumerate(queries):
        row = [query.label]
        for solver_idx, _solver in enumerate(solvers):
            row.append(results[(query_idx, solver_idx)].as_cell())
        lines.append("| " + " | ".join(row) + " |")
    return "\n".join(lines)


def print_summary(
    queries: list[Query], solvers: tuple[Solver, ...], results: dict[tuple[int, int], RunResult]
) -> None:
    unanimous = 0
    disagreements: list[str] = []
    for query_idx, query in enumerate(queries):
        verdicts = [
            results[(query_idx, solver_idx)].verdict
            for solver_idx in range(len(solvers))
            if results[(query_idx, solver_idx)].verdict not in ("timeout", "exception")
        ]
        if verdicts and len(set(verdicts)) == 1:
            unanimous += 1
        elif verdicts:
            disagreements.append(query.label)
    print(
        f"# Summary: {len(queries)} normalized queries, {unanimous} with matching successful verdicts, "
        f"{len(disagreements)} with successful-solver disagreements.",
        file=sys.stderr,
    )
    if disagreements:
        print("# Disagreements:", file=sys.stderr)
        for label in disagreements:
            print(f"#   {label}", file=sys.stderr)


def main() -> int:
    args = parse_args()
    solvers = selected_solvers(args)
    queries = collect_queries()
    if args.limit is not None:
        queries = queries[: args.limit]
    results: dict[tuple[int, int], RunResult] = {}
    for query_idx, query in enumerate(queries):
        print(f"# Running {query_idx + 1}/{len(queries)}: {query.label}", file=sys.stderr)
        for solver_idx, solver in enumerate(solvers):
            results[(query_idx, solver_idx)] = run_query(query, solver, args.timeout)
    table = render_table(queries, solvers, results)
    if args.output is not None:
        args.output.write_text(table + "\n")
    print_summary(queries, solvers, results)
    print(table)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
