#!/usr/bin/env python3

"""Build benchmark metadata for the TACAS'17 NRA suite.

The source papers provide complete family-level counts, but only a subset of
the benchmarks have source-backed per-instance truth labels that we can recover
locally with high confidence. This script classifies every benchmark into the
paper's families and records per-benchmark expected results where we currently
have a defensible oracle.
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path


HANDCRAFTED_VALID = {
    "paper_example_1.vmt",
    "paper_example_2.vmt",
    "paper_example_3.vmt",
    "testdiv.vmt",
    "testdiv-manual.vmt",
    "heron-sqrt-10-0.01.vmt",
    "heron-sqrt-10-0.1.vmt",
    "heron-sqrt-100-0.01.vmt",
    "heron-sqrt-100-0.1.vmt",
    "heron-sqrt-2-0.01.vmt",
    "heron-sqrt-2-0.1.vmt",
    "heron-sqrt-5-0.01.vmt",
    "heron-sqrt-5-0.1.vmt",
}

HANDCRAFTED_INVALID = {
    "example_4.vmt",
}

HYCOMP = {
    "ball_count_1d_plain.hydi.vmt",
    "ball_count_2d_hill.hydi.vmt",
    "ball_count_2d_plain.hydi.vmt",
    "ball_count_2d_slope.hydi.vmt",
    "etcs_braking_2.hydi.vmt",
    "simple_ballistics_invar.hydi.vmt",
    "simple_ballistics_reach.hydi.vmt",
}

ISAT3_CFG_VALID = {
    "cfa_test0006.vmt",
    "cfa_test0012.vmt",
    "cfa_test0014.vmt",
    "cfa_test0015.vmt",
    "cfa_test0017.vmt",
    "cfa_test0018.vmt",
    "cfa_test0019.vmt",
}

ISAT3_CFG_INVALID = {
    "cfa_test0005.vmt",
    "cfa_test0013.vmt",
    "cfa_test0016.vmt",
}

NUXMV_VALID = {
    "Mode_plus_Longitudinal_modified_g120_nuxmv_kratos.vmt",
    "Mode_plus_Longitudinal_modified_g130_nuxmv_kratos.vmt",
}

TCM_VALID = {
    "Circuit_Overshoot_BSP.vmt",
    "Model_REAL.vmt",
}

ISAT3 = {"minigolf.vmt"}


def classify_family(source_vmt: str) -> str:
    if source_vmt in HANDCRAFTED_VALID or source_vmt in HANDCRAFTED_INVALID:
        return "handcrafted"
    if source_vmt in HYCOMP:
        return "hycomp"
    if source_vmt in ISAT3_CFG_VALID or source_vmt in ISAT3_CFG_INVALID:
        return "isat3-cfg"
    if source_vmt in NUXMV_VALID:
        return "nuxmv"
    if source_vmt in TCM_VALID:
        return "tcm"
    if source_vmt in ISAT3:
        return "isat3"
    if source_vmt.startswith("sas13-"):
        return "sas13"
    return "hyst"


def expected_status(source_vmt: str) -> tuple[str, str, str]:
    if source_vmt in HANDCRAFTED_VALID:
        return (
            "valid",
            "TOCL18 family count + direct benchmark semantics",
            "Handcrafted family reconstructed from the paper/example benchmarks and Heron examples.",
        )
    if source_vmt in HANDCRAFTED_INVALID:
        return (
            "invalid",
            "TOCL18 family count + direct benchmark semantics",
            "Counterexample benchmark from the paper's nonlinear running example.",
        )
    if source_vmt in ISAT3_CFG_VALID:
        return (
            "valid",
            "TOCL18 Table 3 + local SAL BMC sweep",
            "No counterexample found up to depth 30; family total is 7 safe / 3 unsafe.",
        )
    if source_vmt in ISAT3_CFG_INVALID:
        return (
            "invalid",
            "TOCL18 Table 3 + local SAL BMC sweep",
            "Concrete counterexample reproduced locally with SAL.",
        )
    if source_vmt in NUXMV_VALID:
        return (
            "valid",
            "TOCL18 benchmark description",
            "The paper states the two nuXmv benchmarks are safe.",
        )
    if source_vmt in TCM_VALID:
        return (
            "valid",
            "TOCL18 benchmark description",
            "The paper states the two TCM benchmarks are safe.",
        )
    return (
        "unknown",
        "",
        "",
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest",
        type=Path,
        default=Path(__file__).resolve().parent / "sal" / "MANIFEST.tsv",
        help="Path to the generated SAL manifest.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parent / "benchmark_metadata.tsv",
        help="Where to write the metadata TSV.",
    )
    args = parser.parse_args()

    rows: list[dict[str, str]] = []
    with args.manifest.open(newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        for row in reader:
            source_vmt = row["source_vmt"]
            family = classify_family(source_vmt)
            status, status_source, status_notes = expected_status(source_vmt)
            rows.append(
                {
                    "source_vmt": source_vmt,
                    "sal_file": row["sal_file"],
                    "context_name": row["context_name"],
                    "family": family,
                    "expected_status": status,
                    "status_source": status_source,
                    "status_notes": status_notes,
                }
            )

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "source_vmt",
                "sal_file",
                "context_name",
                "family",
                "expected_status",
                "status_source",
                "status_notes",
            ],
            delimiter="\t",
        )
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    main()
