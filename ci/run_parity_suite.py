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
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[1]
BENCHMARK_SCRIPT = REPO_ROOT / "tools" / "benchmark-inf-bmc-examples.py"
DEFAULT_MANIFEST = REPO_ROOT / "ci" / "parity_suites.json"
DEFAULT_EXPECTATIONS = REPO_ROOT / "ci" / "parity_expectations.json"
DEFAULT_SAL_INF_BMC = REPO_ROOT / "bin" / "sal-inf-bmc"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run a SAL parity suite and prepare CI artifacts.")
    parser.add_argument("--suite", required=True, help="Suite name from the manifest.")
    parser.add_argument(
        "--manifest",
        type=Path,
        default=DEFAULT_MANIFEST,
        help=f"Path to the parity suite manifest (default: {DEFAULT_MANIFEST}).",
    )
    parser.add_argument(
        "--expectations",
        type=Path,
        default=DEFAULT_EXPECTATIONS,
        help=f"Path to the expectation file (default: {DEFAULT_EXPECTATIONS}).",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        required=True,
        help="Directory that will receive JSON, markdown, logs, and preserved temp files.",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=60.0,
        help="Per-query timeout in seconds (default: 60).",
    )
    parser.add_argument(
        "--sal-inf-bmc",
        type=Path,
        default=Path(os.environ.get("SAL_INF_BMC_BIN", DEFAULT_SAL_INF_BMC)),
        help="Path to the sal-inf-bmc executable to use.",
    )
    return parser.parse_args()


def command_on_path(name: str) -> str | None:
    resolved = shutil.which(name)
    return resolved


def command_supports_flag(command: str, flag: str, probe_text: str) -> bool:
    tokens = shlex.split(command)
    probe_file = Path(tempfile.mkstemp(prefix="sal-parity-probe-", text=True)[1])
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


def resolve_smtlib2_yices_command() -> str | None:
    explicit = os.environ.get("SAL_CI_SMTLIB2_YICES2_COMMAND")
    if explicit:
        return explicit
    for name in ("yices-smt2", "yices_smt2"):
        resolved = command_on_path(name)
        if not resolved:
            continue
        if command_supports_flag(resolved, "--mcsat", "(check-sat)\n"):
            return f"{resolved} --mcsat"
        return resolved
    return None


def resolve_z3_command() -> str | None:
    explicit = os.environ.get("SAL_CI_SMTLIB2_Z3_COMMAND")
    if explicit:
        return explicit
    resolved = command_on_path("z3")
    if not resolved:
        return None
    return f"{resolved} -smt2 pp.decimal=true pp.decimal_precision=20"


def resolve_yices2_command() -> str | None:
    explicit = os.environ.get("SAL_CI_YICES2_COMMAND")
    if explicit:
        return explicit
    return command_on_path("yices") or command_on_path("yices2")


def resolve_yices2_mcsat_mode(command: str | None) -> bool:
    explicit = os.environ.get("SAL_CI_YICES2_MCSAT_MODE")
    if explicit is not None:
        return explicit.lower() not in ("0", "false", "no")
    if not command:
        return False
    return command_supports_flag(command, "--mcsat", "(check)\n")


def resolve_ics_command() -> str | None:
    return os.environ.get("SAL_CI_ICS_COMMAND") or command_on_path("ics")


def resolve_yices1_command() -> str | None:
    return os.environ.get("SAL_CI_YICES1_COMMAND")


def scheme_escape(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"')


def write_salrc(path: Path, commands: dict[str, str | None], tmp_dir: Path | None = None) -> None:
    lines: list[str] = []
    if commands.get("yices2"):
        lines.append(f'(sal/set-yices2-command! "{scheme_escape(commands["yices2"])}")')
        lines.append(
            "(sal/set-yices2-mcsat-mode! #t)"
            if commands.get("yices2_mcsat")
            else "(sal/set-yices2-mcsat-mode! #f)"
        )
    if commands.get("yices"):
        lines.append(f'(sal/set-yices-command! "{scheme_escape(commands["yices"])}")')
    if commands.get("ics"):
        lines.append(f'(sal/set-ics-command! "{scheme_escape(commands["ics"])}")')
    if commands.get("smtlib2_yices2"):
        lines.append(f'(sal/set-smtlib2-command! "{scheme_escape(commands["smtlib2_yices2"])}")')
    if tmp_dir is not None:
        tmp_dir.mkdir(parents=True, exist_ok=True)
        lines.append(f'(sal/set-tmp-file-dir! "{scheme_escape(str(tmp_dir))}")')
    path.write_text("\n".join(lines) + ("\n" if lines else ""))


def run_command(
    cmd: list[str],
    *,
    cwd: Path,
    env: dict[str, str],
    log_path: Path | None = None,
    check: bool = False,
) -> subprocess.CompletedProcess[str]:
    proc = subprocess.run(cmd, cwd=cwd, env=env, capture_output=True, text=True, check=False)
    if log_path is not None:
        log_path.parent.mkdir(parents=True, exist_ok=True)
        combined = []
        combined.append("$ " + " ".join(cmd))
        if proc.stdout:
            combined.append(proc.stdout)
        if proc.stderr:
            combined.append(proc.stderr)
        combined.append(f"\n[exit {proc.returncode}]\n")
        log_path.write_text("\n".join(combined))
    if check and proc.returncode != 0:
        raise subprocess.CalledProcessError(proc.returncode, cmd, output=proc.stdout, stderr=proc.stderr)
    return proc


def smoke_check(
    name: str,
    *,
    sal_inf_bmc: Path,
    home: Path,
    cwd: Path,
    extra_env: dict[str, str],
    expected_substring: str,
    log_path: Path,
) -> dict[str, Any]:
    env = os.environ.copy()
    env["HOME"] = str(home)
    env.update(extra_env)
    cmd = [str(sal_inf_bmc), "-v", "3", "-s", "smtlib2", "inf_bakery", "invalid"]
    proc = run_command(cmd, cwd=cwd, env=env, log_path=log_path)
    combined = "\n".join(part for part in (proc.stdout, proc.stderr) if part)
    ok = proc.returncode == 0 and expected_substring in combined
    return {
        "name": name,
        "ok": ok,
        "expected_substring": expected_substring,
        "returncode": proc.returncode,
        "log": str(log_path),
    }


def render_counts_table(report: dict[str, Any]) -> list[str]:
    lines = [
        "| Solver | Counterexample | Proved | No Counterexample | Timeout | Exception |",
        "| --- | ---: | ---: | ---: | ---: | ---: |",
    ]
    for solver in report["solvers"]:
        counts = report["summary"]["per_solver"][solver["key"]]["counts"]
        lines.append(
            "| {label} | {counterexample} | {proved} | {no_counterexample} | {timeout} | {exception} |".format(
                label=solver["label"],
                counterexample=counts.get("counterexample", 0),
                proved=counts.get("proved", 0),
                no_counterexample=counts.get("no counterexample", 0),
                timeout=counts.get("timeout", 0),
                exception=counts.get("exception", 0),
            )
        )
    return lines


def slugify(text: str) -> str:
    slug = re.sub(r"[^A-Za-z0-9]+", "-", text).strip("-").lower()
    return slug or "case"


def rerun_case(
    case: dict[str, Any],
    *,
    commands: dict[str, str | None],
    sal_inf_bmc: Path,
    output_dir: Path,
    report: dict[str, Any],
) -> None:
    query_lookup = {query["id"]: query for query in report["queries"]}
    solver_lookup = {solver["key"]: solver for solver in report["solvers"]}
    query = query_lookup[case["query_id"]]
    solver = solver_lookup[case["solver"]]
    rerun_root = output_dir / "reruns" / f"{slugify(case['query_id'])}-{solver['key']}"
    rerun_root.mkdir(parents=True, exist_ok=True)
    home = rerun_root / "home"
    home.mkdir(parents=True, exist_ok=True)
    tmp_dir = rerun_root / "preserved-tmp"
    write_salrc(home / ".salrc", commands, tmp_dir=tmp_dir)
    env = os.environ.copy()
    env["HOME"] = str(home)
    env["SAL_INF_BMC_BIN"] = str(sal_inf_bmc)
    for key, value in solver.get("env", {}).items():
        env[key] = value
    cmd = [str(sal_inf_bmc), "--preserve-tmp-files", "-v", "3", *solver["args"], *query["args"]]
    run_command(cmd, cwd=Path(query["workdir"]), env=env, log_path=rerun_root / "rerun.log")
    metadata = {
        "query_id": query["id"],
        "query_label": query["label"],
        "solver": solver["key"],
        "solver_label": solver["label"],
        "command": cmd,
        "preserved_tmp_dir": str(tmp_dir),
    }
    (rerun_root / "metadata.json").write_text(json.dumps(metadata, indent=2, sort_keys=True) + "\n")


def collect_rerun_cases(report: dict[str, Any]) -> list[dict[str, Any]]:
    cases: dict[tuple[str, str], dict[str, Any]] = {}
    comparison = report["comparison"]
    for regression in comparison["regressions"]:
        key = (regression["query_id"], regression["solver"])
        cases[key] = {"query_id": regression["query_id"], "solver": regression["solver"]}
    for exception in comparison["unclassified_exceptions"]:
        key = (exception["query_id"], exception["solver"])
        cases[key] = {"query_id": exception["query_id"], "solver": exception["solver"]}
    for disagreement in comparison["successful_disagreements"]:
        for solver_key in disagreement["results"]:
            key = (disagreement["query_id"], solver_key)
            cases[key] = {"query_id": disagreement["query_id"], "solver": solver_key}
    return list(cases.values())


def write_summary(
    summary_path: Path,
    *,
    args: argparse.Namespace,
    sal_inf_bmc: Path,
    commands: dict[str, str | None],
    smoke_results: list[dict[str, Any]],
    report: dict[str, Any] | None,
) -> None:
    lines = [
        f"# Parity Suite: `{args.suite}`",
        "",
        f"- `sal-inf-bmc`: `{sal_inf_bmc}`",
        f"- manifest: `{args.manifest}`",
        f"- expectations: `{args.expectations}`",
        f"- yices2 command: `{commands.get('yices2')}`",
        f"- yices2 mcsat mode: `{commands.get('yices2_mcsat')}`",
        f"- smtlib2/yices2 command: `{commands.get('smtlib2_yices2')}`",
        f"- smtlib2/z3 command: `{commands.get('smtlib2_z3')}`",
        f"- ics command: `{commands.get('ics')}`",
        f"- yices 1.0 command: `{commands.get('yices')}`",
        f"- timeout (seconds): `{args.timeout}`",
        "",
        "## Smoke Checks",
        "",
        "| Check | Status | Log |",
        "| --- | --- | --- |",
    ]
    for smoke in smoke_results:
        status = "PASS" if smoke["ok"] else "FAIL"
        lines.append(f"| `{smoke['name']}` | {status} | `{smoke['log']}` |")
    if report is not None:
        lines.extend(["", "## Counts", ""])
        lines.extend(render_counts_table(report))
        lines.extend(
            [
                "",
                "## Comparison",
                "",
                f"- successful-solver disagreements: {len(report['comparison']['successful_disagreements'])}",
                f"- regressions: {len(report['comparison']['regressions'])}",
                f"- unclassified exceptions: {len(report['comparison']['unclassified_exceptions'])}",
                f"- missing expectations: {len(report['comparison']['missing_expectations'])}",
                f"- unexpected queries: {len(report['comparison']['unexpected_queries'])}",
            ]
        )
        if report["comparison"]["regressions"]:
            lines.extend(["", "## Regressions", ""])
            for regression in report["comparison"]["regressions"]:
                lines.append(f"- `{regression['reason']}`")
        if report["comparison"]["unclassified_exceptions"]:
            lines.extend(["", "## Unclassified Exceptions", ""])
            for exception in report["comparison"]["unclassified_exceptions"]:
                lines.append(
                    f"- `{exception['query_id']} [{exception['solver']}]`: {exception['detail']}"
                )
    summary_path.write_text("\n".join(lines) + "\n")
    github_summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if github_summary:
        with open(github_summary, "a", encoding="utf-8") as out:
            out.write("\n".join(lines))
            out.write("\n")


def main() -> int:
    args = parse_args()
    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    sal_inf_bmc = args.sal_inf_bmc.resolve()
    if not sal_inf_bmc.exists():
        raise SystemExit(f"sal-inf-bmc not found: {sal_inf_bmc}")

    commands = {
        "yices2": resolve_yices2_command(),
        "yices2_mcsat": None,
        "smtlib2_yices2": resolve_smtlib2_yices_command(),
        "smtlib2_z3": resolve_z3_command(),
        "ics": resolve_ics_command(),
        "yices": resolve_yices1_command(),
    }
    commands["yices2_mcsat"] = resolve_yices2_mcsat_mode(commands["yices2"])

    suite_manifest = json.loads(args.manifest.read_text())
    suite = suite_manifest["suites"][args.suite]
    solver_keys = suite["solver_keys"]
    if "yices2" in solver_keys and not commands["yices2"]:
        raise SystemExit("Unable to resolve a native Yices2 command.")
    if "smtlib2-yices2" in solver_keys and not commands["smtlib2_yices2"]:
        raise SystemExit("Unable to resolve an SMT-LIB2 Yices2 command.")
    if "smtlib2-z3" in solver_keys and not commands["smtlib2_z3"]:
        raise SystemExit("Unable to resolve a Z3 SMT-LIB2 command.")
    if "ics" in solver_keys and not commands["ics"]:
        raise SystemExit("The full suite requires SAL_CI_ICS_COMMAND (or an `ics` binary on PATH).")
    if "yices" in solver_keys and not commands["yices"]:
        raise SystemExit("The full suite requires SAL_CI_YICES1_COMMAND.")

    base_home = Path(tempfile.mkdtemp(prefix="sal-parity-home-"))
    write_salrc(base_home / ".salrc", commands)

    smoke_results = [
        smoke_check(
            "smtlib2-salrc-default",
            sal_inf_bmc=sal_inf_bmc,
            home=base_home,
            cwd=REPO_ROOT / "examples" / "inf-bakery",
            extra_env={},
            expected_substring=f"SMT-LIB2 command: {commands['smtlib2_yices2']}",
            log_path=output_dir / "smoke" / "smtlib2-salrc-default.log",
        ),
        smoke_check(
            "smtlib2-env-override",
            sal_inf_bmc=sal_inf_bmc,
            home=base_home,
            cwd=REPO_ROOT / "examples" / "inf-bakery",
            extra_env={"SAL_SMTLIB2_COMMAND": commands["smtlib2_z3"]},
            expected_substring=f"SMT-LIB2 command: {commands['smtlib2_z3']}",
            log_path=output_dir / "smoke" / "smtlib2-env-override.log",
        ),
    ]

    benchmark_env = os.environ.copy()
    benchmark_env["HOME"] = str(base_home)
    benchmark_env["SAL_INF_BMC_BIN"] = str(sal_inf_bmc)
    benchmark_env["SAL_CI_SMTLIB2_YICES2_COMMAND"] = commands["smtlib2_yices2"] or ""
    benchmark_env["SAL_CI_SMTLIB2_Z3_COMMAND"] = commands["smtlib2_z3"] or ""

    matrix_path = output_dir / "matrix.md"
    report_path = output_dir / "report.json"
    expect_snippet_path = output_dir / "expectation-snippet.json"
    benchmark_log = output_dir / "benchmark.log"
    benchmark_cmd = [
        sys.executable,
        str(BENCHMARK_SCRIPT),
        "--suite",
        args.suite,
        "--manifest",
        str(args.manifest),
        "--expectations",
        str(args.expectations),
        "--fail-on-regressions",
        "--timeout",
        str(args.timeout),
        "--output",
        str(matrix_path),
        "--json-output",
        str(report_path),
        "--expectations-output",
        str(expect_snippet_path),
    ]
    benchmark_proc = run_command(
        benchmark_cmd,
        cwd=REPO_ROOT,
        env=benchmark_env,
        log_path=benchmark_log,
    )

    report: dict[str, Any] | None = None
    if report_path.exists():
        report = json.loads(report_path.read_text())

    smoke_failed = any(not smoke["ok"] for smoke in smoke_results)
    benchmark_failed = benchmark_proc.returncode != 0

    if report is not None and benchmark_failed:
        for case in collect_rerun_cases(report):
            rerun_case(case, commands=commands, sal_inf_bmc=sal_inf_bmc, output_dir=output_dir, report=report)

    summary_path = output_dir / "summary.md"
    write_summary(
        summary_path,
        args=args,
        sal_inf_bmc=sal_inf_bmc,
        commands=commands,
        smoke_results=smoke_results,
        report=report,
    )

    if smoke_failed:
        return 1
    return benchmark_proc.returncode


if __name__ == "__main__":
    raise SystemExit(main())
