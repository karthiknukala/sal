#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from collections import Counter, OrderedDict
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any


@dataclass(frozen=True)
class Solver:
    key: str
    label: str
    args: tuple[str, ...]
    env: tuple[tuple[str, str], ...] = ()

    def env_dict(self) -> dict[str, str]:
        return dict(self.env)


@dataclass
class Query:
    id: str
    workdir: Path
    args: tuple[str, ...]
    sources: list[str] = field(default_factory=list)
    tags: tuple[str, ...] = ()

    @property
    def label(self) -> str:
        workdir_label = self.workdir.relative_to(REPO_ROOT).as_posix()
        return f"{workdir_label} :: {' '.join(self.args)}"


@dataclass
class RunResult:
    verdict: str
    elapsed: float | None = None
    detail: str | None = None
    phase: str | None = None
    returncode: int | None = None

    def as_cell(self) -> str:
        if self.elapsed is not None and self.verdict not in ("timeout", "exception"):
            return f"{self.verdict} ({self.elapsed:.2f}s)"
        label = self.verdict if self.phase is None else f"{self.verdict}/{self.phase}"
        if self.detail:
            return f"{label}: {self.detail}"
        return label

    def to_expectation(self) -> dict[str, Any]:
        result: dict[str, Any] = {"verdict": self.verdict}
        if self.phase is not None:
            result["phase"] = self.phase
        if self.detail:
            result["detail_contains"] = self.detail
        return result

    def to_json(self) -> dict[str, Any]:
        result = {
            "verdict": self.verdict,
            "elapsed_seconds": self.elapsed,
            "detail": self.detail,
            "phase": self.phase,
            "returncode": self.returncode,
            "cell": self.as_cell(),
        }
        return result


REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = REPO_ROOT / "ci" / "parity_suites.json"
DEFAULT_EXPECTATIONS = REPO_ROOT / "ci" / "parity_expectations.json"
EXAMPLES_ROOT = REPO_ROOT / "examples"
SAL_INF_BMC = Path(os.environ.get("SAL_INF_BMC_BIN", REPO_ROOT / "bin" / "sal-inf-bmc")).resolve()
COMMAND_RE = re.compile(r"sal-inf-bmc\b.*")
BENCHMARKABLE_DOCUMENTED_SOLVERS = {"yices", "yices2", "ics", "smtlib2"}

PHASE_PATTERNS: tuple[tuple[str, tuple[str, ...]], ...] = (
    (
        "translation",
        (
            "Can't be converted to a Yices 2 type",
            "Can't be converted to",
            "Expected SMT-LIB2 solver profile to be a symbol or string",
            "Unknown SMT-LIB2 solver profile",
        ),
    ),
    (
        "solver-launch",
        (
            "Error running Yices.",
            "Error running the SMT-LIB2 solver.",
            "Error executing YICES.",
            "Error executing ICS.",
            "Yices2 could not open the input file",
            "Yices2 exited with code",
            "Yices2 was interrupted",
            "Yices2 terminated unexpectedly",
        ),
    ),
    (
        "model-parse",
        (
            "Unexpected output from Yices 2:",
            "parse error (unexpected token",
            "Unexpected end-of-file while reading",
        ),
    ),
    (
        "solver-output",
        (
            "Unexpected SMT-LIB2 solver output:",
            "logic QF_AUFNIRA is not supported since yices was not built with mcsat",
            "An incompleteness was detected in the decision procedure",
            "feature not supported: non linear problem.",
        ),
    ),
    (
        "ce-reconstruction",
        (
            "The context is unsat. No model.",
            "ground assignments",
        ),
    ),
    (
        "frontend",
        (
            "Error loading configuration script:",
            'Type "procedure" expected, "bbool" provided',
            "k-induction rule failed, please try to increase the depth.",
            "Illegal argument `",
            "Unknown solver identifier",
        ),
    ),
)


def default_smtlib2_yices_command() -> str | None:
    env_override = os.environ.get("SAL_CI_SMTLIB2_YICES2_COMMAND") or os.environ.get(
        "SAL_BENCHMARK_SMTLIB2_YICES_COMMAND"
    )
    if env_override:
        return env_override
    for command_name in ("yices-smt2", "yices_smt2"):
        resolved = shutil.which(command_name)
        if resolved:
            if command_supports_flag(resolved, "--mcsat", "(check-sat)\n"):
                return f"{resolved} --mcsat"
            return resolved
    return None


def default_smtlib2_z3_command() -> str | None:
    env_override = os.environ.get("SAL_CI_SMTLIB2_Z3_COMMAND")
    if env_override:
        return env_override
    resolved = shutil.which("z3")
    if resolved:
        return f"{resolved} -smt2 pp.decimal=true pp.decimal_precision=20"
    return None


def command_supports_flag(command: str, flag: str, probe_text: str) -> bool:
    tokens = shlex.split(command)
    fd, probe_path = tempfile.mkstemp(prefix="sal-benchmark-probe-", text=True)
    os.close(fd)
    probe_file = Path(probe_path)
    try:
        probe_file.write_text(probe_text)
        proc = subprocess.run(
            [*tokens, flag, str(probe_file)],
            capture_output=True,
            text=True,
            check=False,
        )
        return proc.returncode == 0
    finally:
        probe_file.unlink(missing_ok=True)


def build_solvers() -> tuple[Solver, ...]:
    smtlib2_yices_command = default_smtlib2_yices_command()
    smtlib2_z3_command = default_smtlib2_z3_command()
    return (
        Solver("yices2", "Yices2", ("-s", "yices2")),
        Solver(
            "smtlib2-yices2",
            "SMTLIB2/Yices2",
            ("-s", "smtlib2"),
            ((("SAL_SMTLIB2_COMMAND", smtlib2_yices_command),) if smtlib2_yices_command else (("SAL_SMTLIB2_PROFILE", "yices2"),)),
        ),
        Solver(
            "smtlib2-z3",
            "SMTLIB2/Z3",
            ("-s", "smtlib2"),
            ((("SAL_SMTLIB2_COMMAND", smtlib2_z3_command),) if smtlib2_z3_command else (("SAL_SMTLIB2_PROFILE", "z3"),)),
        ),
        Solver("ics", "ICS", ("-s", "ics")),
        Solver("yices", "Yices 1.0", ("-s", "yices")),
    )


SOLVERS = build_solvers()
SOLVER_BY_KEY = {solver.key: solver for solver in SOLVERS}


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
        "--json-output",
        type=Path,
        default=None,
        help="Optional file to receive the structured JSON report.",
    )
    parser.add_argument(
        "--expectations-output",
        type=Path,
        default=None,
        help="Optional file to receive the expectation snippet for the executed suite.",
    )
    parser.add_argument(
        "--solver",
        action="append",
        choices=tuple(solver.key for solver in SOLVERS),
        default=None,
        help="Restrict execution to one or more solver keys.",
    )
    parser.add_argument(
        "--manifest",
        type=Path,
        default=DEFAULT_MANIFEST,
        help=f"Suite manifest file (default: {DEFAULT_MANIFEST}).",
    )
    parser.add_argument(
        "--suite",
        default=None,
        help="Named suite from the manifest. If omitted, the full discovered examples set is used.",
    )
    parser.add_argument(
        "--expectations",
        type=Path,
        default=None,
        help="Optional expectation file used to detect regressions.",
    )
    parser.add_argument(
        "--fail-on-regressions",
        action="store_true",
        help="Exit non-zero on expectation mismatches, unclassified exceptions, or successful-solver disagreements.",
    )
    return parser.parse_args()


def selected_solvers(args: argparse.Namespace, suite_data: dict[str, Any] | None) -> tuple[Solver, ...]:
    if args.solver:
        requested = set(args.solver)
        return tuple(solver for solver in SOLVERS if solver.key in requested)
    if suite_data and suite_data.get("solver_keys"):
        return tuple(SOLVER_BY_KEY[key] for key in suite_data["solver_keys"])
    return SOLVERS


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
            query_id = f"{path.parent.relative_to(REPO_ROOT).as_posix()} :: {' '.join(args)}"
            if key not in queries:
                queries[key] = Query(id=query_id, workdir=path.parent.resolve(), args=args, sources=[source])
            else:
                queries[key].sources.append(source)
    return list(queries.values())


def load_json(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text())


def query_from_manifest(query_id: str, query_data: dict[str, Any]) -> Query:
    workdir = (REPO_ROOT / query_data["workdir"]).resolve()
    return Query(
        id=query_id,
        workdir=workdir,
        args=tuple(query_data["args"]),
        sources=list(query_data.get("sources", [])),
        tags=tuple(query_data.get("tags", [])),
    )


def load_suite_queries(args: argparse.Namespace) -> tuple[list[Query], dict[str, Any] | None]:
    if args.suite is None:
        return collect_queries(), None
    if not args.manifest.exists():
        raise SystemExit(f"Manifest file not found: {args.manifest}")
    manifest = load_json(args.manifest)
    suite_data = manifest.get("suites", {}).get(args.suite)
    if suite_data is None:
        raise SystemExit(f"Unknown suite `{args.suite}` in {args.manifest}")
    query_source = suite_data.get("query_source", "manifest")
    if query_source == "documented_examples":
        queries = collect_queries()
    elif query_source == "manifest":
        query_defs = manifest.get("queries", {})
        query_ids = suite_data.get("query_ids", [])
        missing = [query_id for query_id in query_ids if query_id not in query_defs]
        if missing:
            raise SystemExit(f"Missing manifest query definitions: {missing}")
        queries = [query_from_manifest(query_id, query_defs[query_id]) for query_id in query_ids]
    else:
        raise SystemExit(f"Unknown query_source `{query_source}` for suite `{args.suite}`")
    return queries, suite_data


def classify_exception(stdout: str, stderr: str, detail: str | None) -> str:
    combined = "\n".join(part for part in (stdout, stderr, detail) if part)
    for phase, patterns in PHASE_PATTERNS:
        if any(pattern in combined for pattern in patterns):
            return phase
    return "unknown"


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
    return "exception"


def summarize_error(stdout: str, stderr: str) -> str:
    combined = "\n".join(part for part in (stdout.strip(), stderr.strip()) if part)
    if not combined:
        return "no output"
    for line in reversed(combined.splitlines()):
        text = line.strip()
        if text:
            return text[:200]
    return "no output"


def run_query(query: Query, solver: Solver, timeout_s: float) -> RunResult:
    cmd = [str(SAL_INF_BMC), *solver.args, *query.args]
    env = os.environ.copy()
    env.update(solver.env_dict())
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
        return RunResult(verdict, elapsed=elapsed, returncode=proc.returncode)
    detail = summarize_error(proc.stdout, proc.stderr)
    return RunResult(
        "exception",
        detail=detail,
        phase=classify_exception(proc.stdout, proc.stderr, detail),
        returncode=proc.returncode,
    )


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


def build_summary(
    queries: list[Query], solvers: tuple[Solver, ...], results: dict[tuple[int, int], RunResult]
) -> dict[str, Any]:
    per_solver: dict[str, Any] = {}
    disagreements: list[dict[str, Any]] = []
    unanimous = 0
    for solver_idx, solver in enumerate(solvers):
        counts: Counter[str] = Counter()
        exception_phases: Counter[str] = Counter()
        for query_idx in range(len(queries)):
            result = results[(query_idx, solver_idx)]
            counts[result.verdict] += 1
            if result.verdict == "exception":
                exception_phases[result.phase or "unknown"] += 1
        per_solver[solver.key] = {
            "label": solver.label,
            "counts": dict(sorted(counts.items())),
            "exception_phases": dict(sorted(exception_phases.items())),
        }
    for query_idx, query in enumerate(queries):
        successful = [
            results[(query_idx, solver_idx)].verdict
            for solver_idx in range(len(solvers))
            if results[(query_idx, solver_idx)].verdict not in ("timeout", "exception")
        ]
        if successful and len(set(successful)) == 1:
            unanimous += 1
        elif successful:
            disagreements.append(
                {
                    "query_id": query.id,
                    "query_label": query.label,
                    "results": {
                        solvers[solver_idx].key: results[(query_idx, solver_idx)].to_json()
                        for solver_idx in range(len(solvers))
                    },
                }
            )
    return {
        "query_count": len(queries),
        "unanimous_success_rows": unanimous,
        "successful_disagreements": disagreements,
        "per_solver": per_solver,
    }


def expectation_mismatch(
    expected: dict[str, Any], actual: RunResult, solver: Solver, query: Query
) -> str | None:
    expected_verdict = expected.get("verdict")
    if expected_verdict != actual.verdict:
        return (
            f"{query.id} [{solver.key}] expected verdict `{expected_verdict}`, "
            f"found `{actual.verdict}`"
        )
    expected_phase = expected.get("phase")
    if expected_phase is not None and expected_phase != actual.phase:
        return (
            f"{query.id} [{solver.key}] expected phase `{expected_phase}`, "
            f"found `{actual.phase}`"
        )
    detail_contains = expected.get("detail_contains")
    if detail_contains is not None and detail_contains not in (actual.detail or ""):
        return (
            f"{query.id} [{solver.key}] expected detail containing `{detail_contains}`, "
            f"found `{actual.detail}`"
        )
    return None


def compare_expectations(
    suite_name: str,
    queries: list[Query],
    solvers: tuple[Solver, ...],
    results: dict[tuple[int, int], RunResult],
    expectations_path: Path | None,
    summary: dict[str, Any],
) -> dict[str, Any]:
    comparison = {
        "checked": expectations_path is not None,
        "expectations_path": str(expectations_path) if expectations_path else None,
        "regressions": [],
        "missing_expectations": [],
        "unexpected_queries": [],
        "unclassified_exceptions": [],
        "successful_disagreements": summary["successful_disagreements"],
    }
    if expectations_path is None:
        return comparison
    if not expectations_path.exists():
        raise SystemExit(f"Expectation file not found: {expectations_path}")
    expectations_root = load_json(expectations_path)
    suite_expectations = expectations_root.get("suites", {}).get(suite_name)
    if suite_expectations is None:
        raise SystemExit(f"Suite `{suite_name}` not found in expectation file {expectations_path}")
    expected_queries = suite_expectations.get("queries", {})
    seen_query_ids: set[str] = set()
    for query_idx, query in enumerate(queries):
        seen_query_ids.add(query.id)
        expected_query = expected_queries.get(query.id)
        if expected_query is None:
            comparison["unexpected_queries"].append(query.id)
            continue
        expected_solvers = expected_query.get("solvers", {})
        for solver_idx, solver in enumerate(solvers):
            expected_solver = expected_solvers.get(solver.key)
            if expected_solver is None:
                comparison["missing_expectations"].append(f"{query.id} [{solver.key}]")
                continue
            actual = results[(query_idx, solver_idx)]
            mismatch = expectation_mismatch(expected_solver, actual, solver, query)
            if mismatch:
                comparison["regressions"].append(
                    {
                        "query_id": query.id,
                        "query_label": query.label,
                        "solver": solver.key,
                        "reason": mismatch,
                        "expected": expected_solver,
                        "actual": actual.to_json(),
                    }
                )
            if actual.verdict == "exception" and actual.phase == "unknown":
                comparison["unclassified_exceptions"].append(
                    {
                        "query_id": query.id,
                        "query_label": query.label,
                        "solver": solver.key,
                        "detail": actual.detail,
                    }
                )
    missing_queries = sorted(set(expected_queries) - seen_query_ids)
    comparison["missing_expectations"].extend(missing_queries)
    return comparison


def build_report(
    suite_name: str,
    suite_data: dict[str, Any] | None,
    queries: list[Query],
    solvers: tuple[Solver, ...],
    results: dict[tuple[int, int], RunResult],
    summary: dict[str, Any],
    comparison: dict[str, Any],
    timeout_seconds: float,
) -> dict[str, Any]:
    return {
        "suite": suite_name,
        "description": suite_data.get("description") if suite_data else None,
        "sal_inf_bmc": str(SAL_INF_BMC),
        "timeout_seconds": timeout_seconds,
        "solvers": [
            {
                "key": solver.key,
                "label": solver.label,
                "args": list(solver.args),
                "env": solver.env_dict(),
            }
            for solver in solvers
        ],
        "queries": [
            {
                "id": query.id,
                "label": query.label,
                "workdir": str(query.workdir),
                "args": list(query.args),
                "sources": list(query.sources),
                "tags": list(query.tags),
                "results": {
                    solvers[solver_idx].key: results[(query_idx, solver_idx)].to_json()
                    for solver_idx in range(len(solvers))
                },
            }
            for query_idx, query in enumerate(queries)
        ],
        "summary": summary,
        "comparison": comparison,
    }


def render_expectation_snippet(report: dict[str, Any]) -> dict[str, Any]:
    suite_name = report["suite"]
    queries: dict[str, Any] = {}
    for query in report["queries"]:
        queries[query["id"]] = {
            "label": query["label"],
            "solvers": {
                solver_key: {
                    key: value
                    for key, value in {
                        "verdict": result["verdict"],
                        "phase": result["phase"],
                        "detail_contains": result["detail"],
                    }.items()
                    if value is not None
                }
                for solver_key, result in query["results"].items()
            },
        }
    return {"suites": {suite_name: {"queries": queries}}}


def print_summary(summary: dict[str, Any], comparison: dict[str, Any]) -> None:
    print(
        f"# Summary: {summary['query_count']} queries, "
        f"{summary['unanimous_success_rows']} unanimous successful rows, "
        f"{len(summary['successful_disagreements'])} successful-solver disagreements.",
        file=sys.stderr,
    )
    if comparison["checked"]:
        print(
            f"# Expectation check: {len(comparison['regressions'])} regressions, "
            f"{len(comparison['unclassified_exceptions'])} unclassified exceptions, "
            f"{len(comparison['unexpected_queries'])} unexpected queries.",
            file=sys.stderr,
        )


def should_fail(comparison: dict[str, Any]) -> bool:
    return any(
        (
            comparison["regressions"],
            comparison["missing_expectations"],
            comparison["unexpected_queries"],
            comparison["unclassified_exceptions"],
            comparison["successful_disagreements"],
        )
    )


def main() -> int:
    global args
    args = parse_args()
    queries, suite_data = load_suite_queries(args)
    if args.limit is not None:
        queries = queries[: args.limit]
    solvers = selected_solvers(args, suite_data)
    results: dict[tuple[int, int], RunResult] = {}
    for query_idx, query in enumerate(queries):
        print(f"# Running {query_idx + 1}/{len(queries)}: {query.label}", file=sys.stderr)
        for solver_idx, solver in enumerate(solvers):
            results[(query_idx, solver_idx)] = run_query(query, solver, args.timeout)
    table = render_table(queries, solvers, results)
    summary = build_summary(queries, solvers, results)
    suite_name = args.suite or "documented_examples"
    comparison = compare_expectations(suite_name, queries, solvers, results, args.expectations, summary)
    report = build_report(
        suite_name,
        suite_data or {},
        queries,
        solvers,
        results,
        summary,
        comparison,
        args.timeout,
    )

    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(table + "\n")
    if args.json_output is not None:
        args.json_output.parent.mkdir(parents=True, exist_ok=True)
        args.json_output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    if args.expectations_output is not None:
        args.expectations_output.parent.mkdir(parents=True, exist_ok=True)
        args.expectations_output.write_text(
            json.dumps(render_expectation_snippet(report), indent=2, sort_keys=True) + "\n"
        )

    print_summary(summary, comparison)
    print(table)

    if args.fail_on_regressions and should_fail(comparison):
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
