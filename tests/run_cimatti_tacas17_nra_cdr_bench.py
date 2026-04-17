#!/usr/bin/env python3

"""Run sal-cdr profiles over the TACAS'17 NRA benchmark suite.

The script executes `sal-cdr` and `sal-cdr -i` on the generated SAL suite,
writes a per-benchmark TSV plus a family-summary TSV, and emits a Markdown
report with family/total tables alongside the checked-in 2018/2021 paper
baselines.
"""

from __future__ import annotations

import argparse
import csv
import errno
import os
import pty
import select
import signal
import subprocess
import sys
import tempfile
import time
from collections import defaultdict
from pathlib import Path


PROFILE_DEFS = {
    "sal-cdr": {
        "label": "sal-cdr",
        "args": [],
    },
    "sal-cdr-i": {
        "label": "sal-cdr -i",
        "args": ["-i"],
    },
}


def normalize_yices2_command(path: Path) -> Path:
    if path.name in {"yices_smt2", "yices-smt2"}:
        sibling = path.with_name("yices")
        if sibling.exists():
            return sibling
    return path


def read_tsv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle, delimiter="\t"))


def write_tsv(path: Path, fieldnames: list[str], rows: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, delimiter="\t")
        writer.writeheader()
        writer.writerows(rows)


def parse_result(output: str) -> str:
    if "Counterexample:" in output or "\ninvalid.\n" in output or output.rstrip().endswith("invalid."):
        return "invalid"
    if "\nvalid.\n" in output or output.rstrip().endswith("valid."):
        return "valid"
    if "\nunknown.\n" in output or output.rstrip().endswith("unknown."):
        return "unknown"
    return "error"


def load_metadata(path: Path, families: set[str], known_only: bool) -> list[dict[str, str]]:
    rows = read_tsv(path)
    rows.sort(key=lambda row: row["source_vmt"])
    if known_only:
        rows = [row for row in rows if row["expected_status"] != "unknown"]
    if families:
        rows = [row for row in rows if row["family"] in families]
    if not rows:
        raise SystemExit("No benchmarks selected.")
    return rows


def run_command(cmd: list[str], env: dict[str, str], timeout_s: int) -> tuple[str, float]:
    start = time.monotonic()
    master_fd, slave_fd = pty.openpty()
    proc = subprocess.Popen(
        cmd,
        env=env,
        stdin=subprocess.DEVNULL,
        stdout=slave_fd,
        stderr=slave_fd,
        start_new_session=True,
    )
    os.close(slave_fd)
    chunks: list[bytes] = []
    try:
        deadline = start + timeout_s
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise subprocess.TimeoutExpired(cmd, timeout_s)
            ready, _, _ = select.select([master_fd], [], [], remaining)
            if ready:
                try:
                    data = os.read(master_fd, 65536)
                except OSError as exc:
                    if exc.errno == errno.EIO:
                        break
                    raise
                if not data:
                    break
                chunks.append(data)
                continue
            if proc.poll() is not None:
                break

        proc.wait(timeout=1)
        elapsed = time.monotonic() - start
        combined_output = b"".join(chunks).decode("utf-8", errors="replace")
        combined_output = combined_output.replace("\r\n", "\n").replace("\r", "\n")
        result = parse_result(combined_output)
        if proc.returncode != 0 and result == "error":
            result = "error"
        return result, elapsed
    except subprocess.TimeoutExpired:
        try:
            os.killpg(proc.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            proc.wait(timeout=1)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            proc.wait()
        return "timeout", time.monotonic() - start
    finally:
        try:
            os.close(master_fd)
        except OSError:
            pass


def summarize(results: list[dict[str, str]]) -> dict[str, dict[str, dict[str, float]]]:
    summary: dict[str, dict[str, dict[str, float]]] = defaultdict(lambda: defaultdict(lambda: defaultdict(float)))
    for row in results:
        profile = row["profile"]
        family = row["family"]
        summary[profile][family]["benchmarks"] += 1
        summary[profile][family]["time_s"] += float(row["time_s"])
        summary[profile][family][row["result"]] += 1
        if row["result"] in {"valid", "invalid"}:
            summary[profile][family]["solved"] += 1
        if row["expected_status"] != "unknown":
            summary[profile][family]["oracle"] += 1
            if row["matches_expected"] == "yes":
                summary[profile][family]["oracle_matches"] += 1
            if row["matches_expected"] == "no":
                summary[profile][family]["oracle_mismatches"] += 1
    return summary


def total_for_profile(profile_summary: dict[str, dict[str, float]]) -> dict[str, float]:
    total = defaultdict(float)
    for family in profile_summary.values():
        for key, value in family.items():
            total[key] += value
    return total


def markdown_table(headers: list[str], rows: list[list[str]]) -> str:
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join("---" for _ in headers) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(row) + " |")
    return "\n".join(lines)


def build_summary_rows(profile: str, profile_summary: dict[str, dict[str, float]]) -> list[list[str]]:
    rows: list[list[str]] = []
    for family in sorted(profile_summary):
        stats = profile_summary[family]
        rows.append(
            [
                profile,
                family,
                str(int(stats["benchmarks"])),
                str(int(stats["solved"])),
                str(int(stats["valid"])),
                str(int(stats["invalid"])),
                str(int(stats["unknown"])),
                str(int(stats["timeout"])),
                str(int(stats["error"])),
                f"{stats['time_s']:.1f}",
                str(int(stats["oracle"])),
                str(int(stats["oracle_matches"])),
                str(int(stats["oracle_mismatches"])),
            ]
        )
    total = total_for_profile(profile_summary)
    rows.append(
        [
            profile,
            "total",
            str(int(total["benchmarks"])),
            str(int(total["solved"])),
            str(int(total["valid"])),
            str(int(total["invalid"])),
            str(int(total["unknown"])),
            str(int(total["timeout"])),
            str(int(total["error"])),
            f"{total['time_s']:.1f}",
            str(int(total["oracle"])),
            str(int(total["oracle_matches"])),
            str(int(total["oracle_mismatches"])),
        ]
    )
    return rows


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    root = Path(__file__).resolve().parents[1]
    suite_dir = root / "examples" / "cimatti-tacas17-nra"
    parser.add_argument("--sal-root", type=Path, default=root, help="Path to the SAL checkout root.")
    parser.add_argument(
        "--metadata",
        type=Path,
        default=suite_dir / "benchmark_metadata.tsv",
        help="Per-benchmark metadata TSV.",
    )
    parser.add_argument(
        "--paper-2021",
        type=Path,
        default=suite_dir / "paper_results_2021.tsv",
        help="2021 paper comparison TSV.",
    )
    parser.add_argument(
        "--paper-2018",
        type=Path,
        default=suite_dir / "paper_results_2018_vmt_nra.tsv",
        help="2018 paper comparison TSV.",
    )
    parser.add_argument(
        "--profiles",
        default="sal-cdr,sal-cdr-i",
        help="Comma-separated sal-cdr profiles to run (default: sal-cdr,sal-cdr-i).",
    )
    parser.add_argument(
        "--yices2",
        type=Path,
        required=True,
        help="Path to the configured Yices2 executable recorded in .salrc.",
    )
    parser.add_argument(
        "--frame-cap",
        type=int,
        default=32,
        help="Maximum frame index passed via --to (default: 32).",
    )
    parser.add_argument(
        "--timeout-s",
        type=int,
        default=20,
        help="Per-benchmark timeout in seconds (default: 20).",
    )
    parser.add_argument(
        "--known-only",
        action="store_true",
        help="Run only benchmarks with a non-unknown expected_status.",
    )
    parser.add_argument(
        "--families",
        default="",
        help="Comma-separated family filter.",
    )
    parser.add_argument(
        "--results-output",
        type=Path,
        default=root / "tests" / "cimatti_tacas17_nra_cdr_results.tsv",
        help="Per-benchmark result TSV output.",
    )
    parser.add_argument(
        "--summary-output",
        type=Path,
        default=root / "tests" / "cimatti_tacas17_nra_cdr_summary.md",
        help="Markdown summary output.",
    )
    parser.add_argument(
        "--summary-tsv",
        type=Path,
        default=root / "tests" / "cimatti_tacas17_nra_cdr_summary.tsv",
        help="Family summary TSV output.",
    )
    args = parser.parse_args()

    profiles = [item.strip() for item in args.profiles.split(",") if item.strip()]
    invalid_profiles = [profile for profile in profiles if profile not in PROFILE_DEFS]
    if invalid_profiles:
        raise SystemExit(f"Unknown profiles: {', '.join(invalid_profiles)}")

    families = {item.strip() for item in args.families.split(",") if item.strip()}
    metadata = load_metadata(args.metadata, families, args.known_only)

    yices2_command = normalize_yices2_command(args.yices2)

    version_proc = subprocess.run(
        [str(yices2_command), "--version"],
        capture_output=True,
        text=True,
        check=False,
    )
    solver_version = (version_proc.stdout or version_proc.stderr).splitlines()
    solver_version_str = solver_version[0].strip() if solver_version else "unknown"

    results: list[dict[str, str]] = []
    sal_cdr = args.sal_root / "bin" / "sal-cdr"

    for profile in profiles:
        profile_def = PROFILE_DEFS[profile]
        for row in metadata:
            sal_path = args.sal_root / "examples" / "cimatti-tacas17-nra" / "sal" / row["sal_file"]
            cmd = [
                str(sal_cdr),
                *profile_def["args"],
                "--solver=yices2",
                f"--to={args.frame_cap}",
                str(sal_path),
                "property",
            ]
            with tempfile.TemporaryDirectory(prefix="sal-cdr-home-", dir="/tmp") as tmp_home:
                salrc_path = Path(tmp_home) / ".salrc"
                salrc_path.write_text(f'(sal/set-yices2-command! "{yices2_command}")\n')
                env = os.environ.copy()
                env["HOME"] = tmp_home
                result, elapsed = run_command(cmd, env, args.timeout_s)
            expected = row["expected_status"]
            match = ""
            if expected != "unknown" and result in {"valid", "invalid"}:
                match = "yes" if result == expected else "no"
            results.append(
                {
                    "profile": profile,
                    "source_vmt": row["source_vmt"],
                    "family": row["family"],
                    "expected_status": expected,
                    "status_source": row["status_source"],
                    "sal_file": row["sal_file"],
                    "result": result,
                    "result_classification": result,
                    "time_s": f"{elapsed:.3f}",
                    "matches_expected": match,
                    "frame_cap": str(args.frame_cap),
                    "timeout_s": str(args.timeout_s),
                    "solver_path": str(yices2_command),
                    "solver_version": solver_version_str,
                }
            )
            print(
                f"{profile}\t{row['source_vmt']}\t{row['family']}\t{result}\t{elapsed:.2f}s",
                file=sys.stderr,
                flush=True,
            )

    write_tsv(
        args.results_output,
        [
            "profile",
            "source_vmt",
            "family",
            "expected_status",
            "status_source",
            "sal_file",
            "result",
            "result_classification",
            "time_s",
            "matches_expected",
            "frame_cap",
            "timeout_s",
            "solver_path",
            "solver_version",
        ],
        results,
    )

    summary = summarize(results)
    summary_rows_dicts: list[dict[str, str]] = []
    for profile in profiles:
        for row in build_summary_rows(profile, summary[profile]):
            summary_rows_dicts.append(
                {
                    "profile": row[0],
                    "family": row[1],
                    "benchmarks": row[2],
                    "solved": row[3],
                    "valid": row[4],
                    "invalid": row[5],
                    "unknown": row[6],
                    "timeout": row[7],
                    "error": row[8],
                    "time_s": row[9],
                    "oracle": row[10],
                    "matches": row[11],
                    "mismatches": row[12],
                }
            )
    write_tsv(
        args.summary_tsv,
        [
            "profile",
            "family",
            "benchmarks",
            "solved",
            "valid",
            "invalid",
            "unknown",
            "timeout",
            "error",
            "time_s",
            "oracle",
            "matches",
            "mismatches",
        ],
        summary_rows_dicts,
    )

    paper_2021 = {row["family"]: row for row in read_tsv(args.paper_2021)}
    paper_2018 = {row["family"]: row for row in read_tsv(args.paper_2018)}

    markdown_sections = [
        "# sal-cdr TACAS'17 NRA Summary",
        "",
        f"- Profiles: {', '.join(profiles)}",
        f"- Solver path: `{yices2_command}`",
        f"- Solver version: `{solver_version_str}`",
        f"- Timeout (s): {args.timeout_s}",
        f"- Frame cap: {args.frame_cap}",
        f"- Benchmarks selected: {len(metadata)}",
        "",
        "## Family Summary",
        "",
    ]

    family_rows: list[list[str]] = []
    for profile in profiles:
        family_rows.extend(build_summary_rows(profile, summary[profile]))
    markdown_sections.append(
        markdown_table(
            [
                "profile",
                "family",
                "bench",
                "solved",
                "valid",
                "invalid",
                "unknown",
                "timeout",
                "error",
                "time_s",
                "oracle",
                "matches",
                "mismatches",
            ],
            family_rows,
        )
    )
    markdown_sections.extend(["", "## 2021 Comparison (Solved/Valid/Invalid)", ""])

    comparison_rows_2021: list[list[str]] = []
    timing_rows_2021: list[list[str]] = []
    for profile in profiles:
        profile_summary = summary[profile]
        total = total_for_profile(profile_summary)
        families_in_profile = sorted(profile_summary)
        for family in families_in_profile:
            if family not in paper_2021:
                continue
            stats = profile_summary[family]
            baseline = paper_2021[family]
            comparison_rows_2021.append(
                [
                    profile,
                    family,
                    f"{int(stats['solved'])}/{int(stats['valid'])}/{int(stats['invalid'])}",
                    f"{baseline['kind_solved']}/{baseline['kind_valid']}/{baseline['kind_invalid']}",
                    f"{baseline['pdkind_solved']}/{baseline['pdkind_valid']}/{baseline['pdkind_invalid']}",
                    f"{baseline['ic3_solved']}/{baseline['ic3_valid']}/{baseline['ic3_invalid']}",
                ]
            )
            timing_rows_2021.append(
                [
                    profile,
                    family,
                    f"{stats['time_s']:.1f}",
                    baseline["kind_time_s"],
                    baseline["pdkind_time_s"],
                    baseline["ic3_time_s"],
                ]
            )
        if "total" in paper_2021 and int(total["benchmarks"]) == len(metadata):
            baseline = paper_2021["total"]
            comparison_rows_2021.append(
                [
                    profile,
                    "total",
                    f"{int(total['solved'])}/{int(total['valid'])}/{int(total['invalid'])}",
                    f"{baseline['kind_solved']}/{baseline['kind_valid']}/{baseline['kind_invalid']}",
                    f"{baseline['pdkind_solved']}/{baseline['pdkind_valid']}/{baseline['pdkind_invalid']}",
                    f"{baseline['ic3_solved']}/{baseline['ic3_valid']}/{baseline['ic3_invalid']}",
                ]
            )
            timing_rows_2021.append(
                [
                    profile,
                    "total",
                    f"{total['time_s']:.1f}",
                    baseline["kind_time_s"],
                    baseline["pdkind_time_s"],
                    baseline["ic3_time_s"],
                ]
            )

    markdown_sections.append(
        markdown_table(
            ["profile", "family", "sal-cdr", "kind", "pdkind", "ic3-nra"],
            comparison_rows_2021,
        )
    )
    markdown_sections.extend(["", "## 2021 Timing Comparison (time_s)", ""])
    markdown_sections.append(
        markdown_table(
            ["profile", "family", "sal-cdr", "kind", "pdkind", "ic3-nra"],
            timing_rows_2021,
        )
    )
    markdown_sections.extend(["", "## 2018 Comparison (Invalid/Valid)", ""])

    comparison_rows_2018: list[list[str]] = []
    for profile in profiles:
        profile_summary = summary[profile]
        total = total_for_profile(profile_summary)
        for family in sorted(profile_summary):
            if family not in paper_2018:
                continue
            stats = profile_summary[family]
            baseline = paper_2018[family]
            comparison_rows_2018.append(
                [
                    profile,
                    family,
                    f"{int(stats['invalid'])}/{int(stats['valid'])}",
                    f"{baseline['k_z3_invalid']}/{baseline['k_z3_valid']}",
                    f"{baseline['k_mathsat_invalid']}/{baseline['k_mathsat_valid']}",
                    f"{baseline['bmc_z3_invalid']}/{baseline['bmc_z3_valid']}",
                ]
            )
        if "total" in paper_2018 and int(total["benchmarks"]) == len(metadata):
            baseline = paper_2018["total"]
            comparison_rows_2018.append(
                [
                    profile,
                    "total",
                    f"{int(total['invalid'])}/{int(total['valid'])}",
                    f"{baseline['k_z3_invalid']}/{baseline['k_z3_valid']}",
                    f"{baseline['k_mathsat_invalid']}/{baseline['k_mathsat_valid']}",
                    f"{baseline['bmc_z3_invalid']}/{baseline['bmc_z3_valid']}",
                ]
            )

    markdown_sections.append(
        markdown_table(
            ["profile", "family", "sal-cdr", "k-z3", "k-mathsat", "bmc-z3"],
            comparison_rows_2018,
        )
    )

    args.summary_output.parent.mkdir(parents=True, exist_ok=True)
    args.summary_output.write_text("\n".join(markdown_sections) + "\n")

    print(f"Wrote per-benchmark results to {args.results_output}")
    print(f"Wrote family summary TSV to {args.summary_tsv}")
    print(f"Wrote Markdown summary to {args.summary_output}")


if __name__ == "__main__":
    main()
