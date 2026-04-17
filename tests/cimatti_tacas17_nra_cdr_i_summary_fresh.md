# sal-cdr TACAS'17 NRA Summary

- Profiles: sal-cdr-i
- Solver path: `/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices`
- Solver version: `Yices 2.7.0`
- Timeout (s): 20
- Frame cap: 32
- Benchmarks selected: 28

## Family Summary

| profile | family | bench | solved | valid | invalid | unknown | timeout | error | time_s | oracle | matches | mismatches |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| sal-cdr-i | handcrafted | 14 | 1 | 0 | 1 | 0 | 12 | 1 | 258.0 | 14 | 1 | 0 |
| sal-cdr-i | isat3-cfg | 10 | 3 | 0 | 3 | 0 | 4 | 3 | 122.4 | 10 | 3 | 0 |
| sal-cdr-i | nuxmv | 2 | 0 | 0 | 0 | 0 | 0 | 2 | 3.0 | 2 | 0 | 0 |
| sal-cdr-i | tcm | 2 | 0 | 0 | 0 | 0 | 2 | 0 | 40.1 | 2 | 0 | 0 |
| sal-cdr-i | total | 28 | 4 | 0 | 4 | 0 | 18 | 6 | 423.5 | 28 | 4 | 0 |

## 2021 Comparison (Solved/Valid/Invalid)

| profile | family | sal-cdr | kind | pdkind | ic3-nra |
| --- | --- | --- | --- | --- | --- |
| sal-cdr-i | handcrafted | 1/0/1 | 3/2/1 | 14/13/1 | 10/9/1 |
| sal-cdr-i | isat3-cfg | 3/0/3 | 9/6/3 | 10/7/3 | 8/6/2 |
| sal-cdr-i | nuxmv | 0/0/0 | 0/0/0 | 1/1/0 | 2/2/0 |
| sal-cdr-i | tcm | 0/0/0 | 2/2/0 | 2/2/0 | 2/2/0 |
| sal-cdr-i | total | 4/0/4 | 48/24/24 | 82/59/23 | 73/58/15 |

## 2021 Timing Comparison (time_s)

| profile | family | sal-cdr | kind | pdkind | ic3-nra |
| --- | --- | --- | --- | --- | --- |
| sal-cdr-i | handcrafted | 258.0 | 0 | 4 | 381 |
| sal-cdr-i | isat3-cfg | 122.4 | 9 | 8 | 14 |
| sal-cdr-i | nuxmv | 3.0 | 0 | 1118 | 158 |
| sal-cdr-i | tcm | 40.1 | 0 | 0 | 1 |
| sal-cdr-i | total | 423.5 | 855 | 1971 | 986 |

## 2018 Comparison (Invalid/Valid)

| profile | family | sal-cdr | k-z3 | k-mathsat | bmc-z3 |
| --- | --- | --- | --- | --- | --- |
| sal-cdr-i | handcrafted | 1/0 | 1/2 | 1/2 | 1/0 |
| sal-cdr-i | isat3-cfg | 3/0 | 2/6 | 1/4 | 3/0 |
| sal-cdr-i | nuxmv | 0/0 | 0/0 | 0/0 | 0/0 |
| sal-cdr-i | tcm | 0/0 | 0/2 | 0/2 | 0/0 |
| sal-cdr-i | total | 4/0 | 25/22 | 13/20 | 26/0 |
