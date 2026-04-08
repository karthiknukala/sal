#!/usr/bin/env python3

"""Run SAL k-induction over the TACAS'17 NRA benchmark suite.

The script reads the generated SAL manifest plus benchmark metadata, executes
`sal-inf-bmc -i -it` with Yices2/MCSAT, writes a per-benchmark TSV, and prints
paper-style family summaries that can be compared with the 2018 and 2021
evaluation tables.
"""

from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
import tempfile
import time
from collections import defaultdict
from pathlib import Path


def read_tsv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle, delimiter="\t"))


def load_metadata(path: Path) -> list[dict[str, str]]:
    rows = read_tsv(path)
    rows.sort(key=lambda row: row["source_vmt"])
    return rows


def parse_result(output: str) -> str:
    if "Counterexample:" in output:
        return "invalid"
    if "proved." in output:
        return "valid"
    if "k-induction rule failed" in output:
        return "unknown"
    if "no counterexample between depths" in output:
        return "unknown"
    return "error"


def print_table(title: str, headers: list[str], rows: list[list[str]]) -> None:
    print(title)
    widths = [len(header) for header in headers]
    for row in rows:
        for index, cell in enumerate(row):
            widths[index] = max(widths[index], len(cell))
    header_line = "  ".join(header.ljust(widths[index]) for index, header in enumerate(headers))
    print(header_line)
    print("  ".join("-" * widths[index] for index in range(len(headers))))
    for row in rows:
        print("  ".join(cell.ljust(widths[index]) for index, cell in enumerate(row)))
    print()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    root = Path(__file__).resolve().parents[1]
    suite_dir = root / "examples" / "cimatti-tacas17-nra"
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
        help="2021 paper comparison table.",
    )
    parser.add_argument(
        "--paper-2018",
        type=Path,
        default=suite_dir / "paper_results_2018_vmt_nra.tsv",
        help="2018 VMT(NRA) comparison table.",
    )
    parser.add_argument(
        "--sal-root",
        type=Path,
        default=root,
        help="Path to the SAL checkout root.",
    )
    parser.add_argument(
        "--solver",
        default="yices2",
        help="Solver passed to sal-inf-bmc (default: yices2).",
    )
    parser.add_argument(
        "--yices2",
        type=Path,
        required=True,
        help="Path to the Yices2 binary to record in .salrc.",
    )
    parser.add_argument(
        "--max-k",
        type=int,
        default=32,
        help="Maximum k used with `-i -it --to=<k>` (default: 32).",
    )
    parser.add_argument(
        "--timeout-s",
        type=int,
        default=60,
        help="Per-benchmark timeout in seconds (default: 60).",
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
        "--output",
        type=Path,
        default=root / "tests" / "cimatti_tacas17_nra_kind_results.tsv",
        help="Where to write the per-benchmark result TSV.",
    )
    args = parser.parse_args()

    metadata = load_metadata(args.metadata)
    if args.known_only:
        metadata = [row for row in metadata if row["expected_status"] != "unknown"]
    if args.families:
        allowed = {family.strip() for family in args.families.split(",") if family.strip()}
        metadata = [row for row in metadata if row["family"] in allowed]

    if not metadata:
        raise SystemExit("No benchmarks selected.")

    results: list[dict[str, str]] = []
    sal_inf_bmc = args.sal_root / "bin" / "sal-inf-bmc"

    with tempfile.TemporaryDirectory(prefix="sal-kind-home-", dir="/tmp") as tmp_home:
        salrc_path = Path(tmp_home) / ".salrc"
        salrc_path.write_text(f'(sal/set-yices2-command! "{args.yices2}")\n')
        env = os.environ.copy()
        env["HOME"] = tmp_home

        for row in metadata:
            sal_path = args.sal_root / "examples" / "cimatti-tacas17-nra" / "sal" / row["sal_file"]
            cmd = [
                str(sal_inf_bmc),
                "-s",
                args.solver,
                "-i",
                "-it",
                "--from=0",
                f"--to={args.max_k}",
                str(sal_path),
                "property",
            ]
            start = time.monotonic()
            try:
                completed = subprocess.run(
                    cmd,
                    env=env,
                    capture_output=True,
                    text=True,
                    timeout=args.timeout_s,
                    check=False,
                )
                elapsed = time.monotonic() - start
                combined_output = completed.stdout + completed.stderr
                verdict = parse_result(combined_output)
                if completed.returncode != 0 and verdict == "error":
                    verdict = "error"
            except subprocess.TimeoutExpired:
                elapsed = time.monotonic() - start
                combined_output = ""
                verdict = "timeout"

            expected = row["expected_status"]
            match = ""
            if expected != "unknown" and verdict in {"valid", "invalid"}:
                match = "yes" if verdict == expected else "no"

            results.append(
                {
                    "source_vmt": row["source_vmt"],
                    "family": row["family"],
                    "expected_status": expected,
                    "status_source": row["status_source"],
                    "sal_file": row["sal_file"],
                    "result": verdict,
                    "time_s": f"{elapsed:.3f}",
                    "matches_expected": match,
                    "max_k": str(args.max_k),
                    "timeout_s": str(args.timeout_s),
                }
            )
            print(
                f"{row['source_vmt']}\t{row['family']}\t{verdict}\t{elapsed:.2f}s",
                file=sys.stderr,
                flush=True,
            )

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "source_vmt",
                "family",
                "expected_status",
                "status_source",
                "sal_file",
                "result",
                "time_s",
                "matches_expected",
                "max_k",
                "timeout_s",
            ],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(results)

    summary: dict[str, dict[str, float]] = defaultdict(lambda: defaultdict(float))
    expected_summary: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    for row in results:
        family = row["family"]
        summary[family]["benchmarks"] += 1
        summary[family]["time_s"] += float(row["time_s"])
        summary[family][row["result"]] += 1
        if row["result"] in {"valid", "invalid"}:
            summary[family]["solved"] += 1
        if row["expected_status"] != "unknown":
            expected_summary[family]["oracle"] += 1
            if row["result"] in {"valid", "invalid"}:
                expected_summary[family]["oracle_solved"] += 1
            if row["matches_expected"] == "yes":
                expected_summary[family]["oracle_matches"] += 1
            if row["matches_expected"] == "no":
                expected_summary[family]["oracle_mismatches"] += 1

    total_row = {
        "benchmarks": sum(int(summary[family]["benchmarks"]) for family in summary),
        "solved": sum(int(summary[family]["solved"]) for family in summary),
        "valid": sum(int(summary[family]["valid"]) for family in summary),
        "invalid": sum(int(summary[family]["invalid"]) for family in summary),
        "unknown": sum(int(summary[family]["unknown"]) for family in summary),
        "timeout": sum(int(summary[family]["timeout"]) for family in summary),
        "error": sum(int(summary[family]["error"]) for family in summary),
        "time_s": sum(summary[family]["time_s"] for family in summary),
        "oracle": sum(int(expected_summary[family]["oracle"]) for family in expected_summary),
        "oracle_matches": sum(int(expected_summary[family]["oracle_matches"]) for family in expected_summary),
        "oracle_mismatches": sum(int(expected_summary[family]["oracle_mismatches"]) for family in expected_summary),
    }

    summary_rows: list[list[str]] = []
    for family in sorted(summary):
        summary_rows.append(
            [
                family,
                str(int(summary[family]["benchmarks"])),
                str(int(summary[family]["solved"])),
                str(int(summary[family]["valid"])),
                str(int(summary[family]["invalid"])),
                str(int(summary[family]["unknown"])),
                str(int(summary[family]["timeout"])),
                str(int(summary[family]["error"])),
                f"{summary[family]['time_s']:.1f}",
                str(int(expected_summary[family]["oracle"])),
                str(int(expected_summary[family]["oracle_matches"])),
                str(int(expected_summary[family]["oracle_mismatches"])),
            ]
        )
    summary_rows.append(
        [
            "total",
            str(total_row["benchmarks"]),
            str(total_row["solved"]),
            str(total_row["valid"]),
            str(total_row["invalid"]),
            str(total_row["unknown"]),
            str(total_row["timeout"]),
            str(total_row["error"]),
            f"{total_row['time_s']:.1f}",
            str(total_row["oracle"]),
            str(total_row["oracle_matches"]),
            str(total_row["oracle_mismatches"]),
        ]
    )
    print_table(
        "SAL k-induction summary",
        [
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
        summary_rows,
    )

    paper_2021 = {row["family"]: row for row in read_tsv(args.paper_2021)}
    comparison_2021: list[list[str]] = []
    for family in sorted(summary):
        if family not in paper_2021:
            continue
        row = paper_2021[family]
        comparison_2021.append(
            [
                family,
                f"{int(summary[family]['solved'])}/{int(summary[family]['valid'])}/{int(summary[family]['invalid'])}",
                f"{row['kind_solved']}/{row['kind_valid']}/{row['kind_invalid']}",
                f"{row['pdkind_solved']}/{row['pdkind_valid']}/{row['pdkind_invalid']}",
                f"{row['ic3_solved']}/{row['ic3_valid']}/{row['ic3_invalid']}",
            ]
        )
    full_suite_run = "total" in paper_2021 and total_row["benchmarks"] == int(paper_2021["total"]["total"])
    if full_suite_run:
        row = paper_2021["total"]
        comparison_2021.append(
            [
                "total",
                f"{total_row['solved']}/{total_row['valid']}/{total_row['invalid']}",
                f"{row['kind_solved']}/{row['kind_valid']}/{row['kind_invalid']}",
                f"{row['pdkind_solved']}/{row['pdkind_valid']}/{row['pdkind_invalid']}",
                f"{row['ic3_solved']}/{row['ic3_valid']}/{row['ic3_invalid']}",
            ]
        )
    print_table(
        "2021 paper comparison (solved/valid/invalid)",
        ["family", "sal", "kind", "pdkind", "ic3-nra"],
        comparison_2021,
    )

    timing_2021: list[list[str]] = []
    for family in sorted(summary):
        if family not in paper_2021:
            continue
        row = paper_2021[family]
        timing_2021.append(
            [
                family,
                f"{summary[family]['time_s']:.1f}",
                row["kind_time_s"],
                row["pdkind_time_s"],
                row["ic3_time_s"],
            ]
        )
    if full_suite_run:
        row = paper_2021["total"]
        timing_2021.append(
            [
                "total",
                f"{total_row['time_s']:.1f}",
                row["kind_time_s"],
                row["pdkind_time_s"],
                row["ic3_time_s"],
            ]
        )
    print_table(
        "2021 paper timing comparison (time_s)",
        ["family", "sal", "kind", "pdkind", "ic3-nra"],
        timing_2021,
    )

    paper_2018 = {row["family"]: row for row in read_tsv(args.paper_2018)}
    comparison_2018: list[list[str]] = []
    for family in sorted(summary):
        if family not in paper_2018:
            continue
        row = paper_2018[family]
        comparison_2018.append(
            [
                family,
                f"{int(summary[family]['invalid'])}/{int(summary[family]['valid'])}",
                f"{row['k_z3_invalid']}/{row['k_z3_valid']}",
                f"{row['k_mathsat_invalid']}/{row['k_mathsat_valid']}",
                f"{row['bmc_z3_invalid']}/{row['bmc_z3_valid']}",
            ]
        )
    if full_suite_run and "total" in paper_2018:
        row = paper_2018["total"]
        comparison_2018.append(
            [
                "total",
                f"{total_row['invalid']}/{total_row['valid']}",
                f"{row['k_z3_invalid']}/{row['k_z3_valid']}",
                f"{row['k_mathsat_invalid']}/{row['k_mathsat_valid']}",
                f"{row['bmc_z3_invalid']}/{row['bmc_z3_valid']}",
            ]
        )
    print_table(
        "2018 paper comparison (invalid/valid)",
        ["family", "sal", "k-z3", "k-mathsat", "bmc-z3"],
        comparison_2018,
    )

    print(f"Wrote per-benchmark results to {args.output}")


if __name__ == "__main__":
    main()
