# sal-cdr TACAS'17 NRA Summary

- Profiles: sal-cdr, sal-cdr-i
- Solver path: `/Users/e35480/git/yices2/build/arm-apple-darwin24.6.0-release/bin/yices`
- Solver version: `Yices 2.7.0`
- Timeout (s): 20
- Frame cap: 32
- Benchmarks selected: 28

## Family Summary

| profile | family | bench | solved | valid | invalid | unknown | timeout | error | time_s | oracle | matches | mismatches |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 14 | 3 | 2 | 1 | 11 | 0 | 0 | 118.9 | 14 | 3 | 0 |
| sal-cdr | isat3-cfg | 10 | 3 | 3 | 0 | 3 | 4 | 0 | 116.1 | 10 | 3 | 0 |
| sal-cdr | nuxmv | 2 | 0 | 0 | 0 | 0 | 0 | 2 | 2.3 | 2 | 0 | 0 |
| sal-cdr | tcm | 2 | 0 | 0 | 0 | 0 | 1 | 1 | 29.1 | 2 | 0 | 0 |
| sal-cdr | total | 28 | 6 | 5 | 1 | 14 | 5 | 3 | 266.3 | 28 | 6 | 0 |
| sal-cdr-i | handcrafted | 14 | 2 | 1 | 1 | 0 | 12 | 0 | 250.5 | 14 | 2 | 0 |
| sal-cdr-i | isat3-cfg | 10 | 6 | 3 | 3 | 0 | 4 | 0 | 109.2 | 10 | 6 | 0 |
| sal-cdr-i | nuxmv | 2 | 0 | 0 | 0 | 0 | 0 | 2 | 2.6 | 2 | 0 | 0 |
| sal-cdr-i | tcm | 2 | 0 | 0 | 0 | 0 | 1 | 1 | 29.8 | 2 | 0 | 0 |
| sal-cdr-i | total | 28 | 8 | 4 | 4 | 0 | 17 | 3 | 392.1 | 28 | 8 | 0 |

## 2021 Comparison (Solved/Valid/Invalid)

| profile | family | sal-cdr | kind | pdkind | ic3-nra |
| --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 3/2/1 | 3/2/1 | 14/13/1 | 10/9/1 |
| sal-cdr | isat3-cfg | 3/3/0 | 9/6/3 | 10/7/3 | 8/6/2 |
| sal-cdr | nuxmv | 0/0/0 | 0/0/0 | 1/1/0 | 2/2/0 |
| sal-cdr | tcm | 0/0/0 | 2/2/0 | 2/2/0 | 2/2/0 |
| sal-cdr | total | 6/5/1 | 48/24/24 | 82/59/23 | 73/58/15 |
| sal-cdr-i | handcrafted | 2/1/1 | 3/2/1 | 14/13/1 | 10/9/1 |
| sal-cdr-i | isat3-cfg | 6/3/3 | 9/6/3 | 10/7/3 | 8/6/2 |
| sal-cdr-i | nuxmv | 0/0/0 | 0/0/0 | 1/1/0 | 2/2/0 |
| sal-cdr-i | tcm | 0/0/0 | 2/2/0 | 2/2/0 | 2/2/0 |
| sal-cdr-i | total | 8/4/4 | 48/24/24 | 82/59/23 | 73/58/15 |

## 2021 Timing Comparison (time_s)

| profile | family | sal-cdr | kind | pdkind | ic3-nra |
| --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 118.9 | 0 | 4 | 381 |
| sal-cdr | isat3-cfg | 116.1 | 9 | 8 | 14 |
| sal-cdr | nuxmv | 2.3 | 0 | 1118 | 158 |
| sal-cdr | tcm | 29.1 | 0 | 0 | 1 |
| sal-cdr | total | 266.3 | 855 | 1971 | 986 |
| sal-cdr-i | handcrafted | 250.5 | 0 | 4 | 381 |
| sal-cdr-i | isat3-cfg | 109.2 | 9 | 8 | 14 |
| sal-cdr-i | nuxmv | 2.6 | 0 | 1118 | 158 |
| sal-cdr-i | tcm | 29.8 | 0 | 0 | 1 |
| sal-cdr-i | total | 392.1 | 855 | 1971 | 986 |

## 2018 Comparison (Invalid/Valid)

| profile | family | sal-cdr | k-z3 | k-mathsat | bmc-z3 |
| --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 1/2 | 1/2 | 1/2 | 1/0 |
| sal-cdr | isat3-cfg | 0/3 | 2/6 | 1/4 | 3/0 |
| sal-cdr | nuxmv | 0/0 | 0/0 | 0/0 | 0/0 |
| sal-cdr | tcm | 0/0 | 0/2 | 0/2 | 0/0 |
| sal-cdr | total | 1/5 | 25/22 | 13/20 | 26/0 |
| sal-cdr-i | handcrafted | 1/1 | 1/2 | 1/2 | 1/0 |
| sal-cdr-i | isat3-cfg | 3/3 | 2/6 | 1/4 | 3/0 |
| sal-cdr-i | nuxmv | 0/0 | 0/0 | 0/0 | 0/0 |
| sal-cdr-i | tcm | 0/0 | 0/2 | 0/2 | 0/0 |
| sal-cdr-i | total | 4/4 | 25/22 | 13/20 | 26/0 |
