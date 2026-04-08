#!/usr/bin/env python3

import argparse
import os
import re
import shlex
import subprocess
import sys
import time
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path


@dataclass(frozen=True)
class Solver:
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
        if self.elapsed is not None and self.verdict not in ("timeout", "error", "unexpected"):
            return f"{self.verdict} ({self.elapsed:.2f}s)"
        if self.detail:
            return f"{self.verdict}: {self.detail}"
        return self.verdict


REPO_ROOT = Path(__file__).resolve().parents[1]
EXAMPLES_ROOT = REPO_ROOT / "examples"
SAL_INF_BMC = REPO_ROOT / "bin" / "sal-inf-bmc"
COMMAND_RE = re.compile(r"sal-inf-bmc\b.*")

SOLVERS = (
    Solver("Yices2", ("-s", "yices2")),
    Solver("SMTLIB2/Yices2", ("-s", "smtlib2"), (("SAL_SMTLIB2_PROFILE", "yices2"),)),
    Solver("SMTLIB2/Z3", ("-s", "smtlib2"), (("SAL_SMTLIB2_PROFILE", "z3"),)),
    Solver("ICS", ("-s", "ics")),
    Solver("Yices 1.0", ("-s", "yices")),
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
    return parser.parse_args()


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


def normalize_verdict(stdout: str) -> str:
    if "Counterexample:" in stdout:
        return "counterexample"
    if re.search(r"^proved\.$", stdout, flags=re.MULTILINE):
        return "proved"
    if "no counterexample between depths:" in stdout:
        return "no counterexample"
    if re.search(r"^invalid\.$", stdout, flags=re.MULTILINE):
        return "invalid"
    return "unexpected"


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
    verdict = normalize_verdict(proc.stdout)
    if proc.returncode == 0 and verdict != "unexpected":
        return RunResult(verdict, elapsed=elapsed)
    if proc.returncode == 0:
        return RunResult("unexpected", elapsed=elapsed, detail=summarize_error(proc.stdout, proc.stderr))
    return RunResult("error", detail=summarize_error(proc.stdout, proc.stderr))


def render_table(queries: list[Query], results: dict[tuple[int, int], RunResult]) -> str:
    headers = ["Query", *[solver.label for solver in SOLVERS]]
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(["---"] * len(headers)) + " |",
    ]
    for query_idx, query in enumerate(queries):
        row = [query.label]
        for solver_idx, _solver in enumerate(SOLVERS):
            row.append(results[(query_idx, solver_idx)].as_cell())
        lines.append("| " + " | ".join(row) + " |")
    return "\n".join(lines)


def print_summary(queries: list[Query], results: dict[tuple[int, int], RunResult]) -> None:
    unanimous = 0
    disagreements: list[str] = []
    for query_idx, query in enumerate(queries):
        verdicts = [
            results[(query_idx, solver_idx)].verdict
            for solver_idx in range(len(SOLVERS))
            if results[(query_idx, solver_idx)].verdict not in ("timeout", "error", "unexpected")
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
    queries = collect_queries()
    if args.limit is not None:
        queries = queries[: args.limit]
    results: dict[tuple[int, int], RunResult] = {}
    for query_idx, query in enumerate(queries):
        print(f"# Running {query_idx + 1}/{len(queries)}: {query.label}", file=sys.stderr)
        for solver_idx, solver in enumerate(SOLVERS):
            results[(query_idx, solver_idx)] = run_query(query, solver, args.timeout)
    table = render_table(queries, results)
    if args.output is not None:
        args.output.write_text(table + "\n")
    print_summary(queries, results)
    print(table)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
