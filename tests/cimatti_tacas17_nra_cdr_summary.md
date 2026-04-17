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
| sal-cdr | handcrafted | 14 | 3 | 2 | 1 | 11 | 0 | 0 | 104.9 | 14 | 3 | 0 |
| sal-cdr | isat3-cfg | 10 | 3 | 3 | 0 | 3 | 4 | 0 | 106.9 | 10 | 3 | 0 |
| sal-cdr | nuxmv | 2 | 0 | 0 | 0 | 0 | 0 | 2 | 2.1 | 2 | 0 | 0 |
| sal-cdr | tcm | 2 | 0 | 0 | 0 | 0 | 1 | 1 | 25.0 | 2 | 0 | 0 |
| sal-cdr | total | 28 | 6 | 5 | 1 | 14 | 5 | 3 | 238.9 | 28 | 6 | 0 |
| sal-cdr-i | handcrafted | 14 | 1 | 0 | 1 | 0 | 12 | 1 | 257.0 | 14 | 1 | 0 |
| sal-cdr-i | isat3-cfg | 10 | 3 | 0 | 3 | 0 | 4 | 3 | 132.1 | 10 | 3 | 0 |
| sal-cdr-i | nuxmv | 2 | 0 | 0 | 0 | 0 | 0 | 2 | 2.4 | 2 | 0 | 0 |
| sal-cdr-i | tcm | 2 | 0 | 0 | 0 | 0 | 1 | 1 | 25.4 | 2 | 0 | 0 |
| sal-cdr-i | total | 28 | 4 | 0 | 4 | 0 | 17 | 7 | 416.9 | 28 | 4 | 0 |

## 2021 Comparison (Solved/Valid/Invalid)

| profile | family | sal-cdr | kind | pdkind | ic3-nra |
| --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 3/2/1 | 3/2/1 | 14/13/1 | 10/9/1 |
| sal-cdr | isat3-cfg | 3/3/0 | 9/6/3 | 10/7/3 | 8/6/2 |
| sal-cdr | nuxmv | 0/0/0 | 0/0/0 | 1/1/0 | 2/2/0 |
| sal-cdr | tcm | 0/0/0 | 2/2/0 | 2/2/0 | 2/2/0 |
| sal-cdr | total | 6/5/1 | 48/24/24 | 82/59/23 | 73/58/15 |
| sal-cdr-i | handcrafted | 1/0/1 | 3/2/1 | 14/13/1 | 10/9/1 |
| sal-cdr-i | isat3-cfg | 3/0/3 | 9/6/3 | 10/7/3 | 8/6/2 |
| sal-cdr-i | nuxmv | 0/0/0 | 0/0/0 | 1/1/0 | 2/2/0 |
| sal-cdr-i | tcm | 0/0/0 | 2/2/0 | 2/2/0 | 2/2/0 |
| sal-cdr-i | total | 4/0/4 | 48/24/24 | 82/59/23 | 73/58/15 |

## 2021 Timing Comparison (time_s)

| profile | family | sal-cdr | kind | pdkind | ic3-nra |
| --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 104.9 | 0 | 4 | 381 |
| sal-cdr | isat3-cfg | 106.9 | 9 | 8 | 14 |
| sal-cdr | nuxmv | 2.1 | 0 | 1118 | 158 |
| sal-cdr | tcm | 25.0 | 0 | 0 | 1 |
| sal-cdr | total | 238.9 | 855 | 1971 | 986 |
| sal-cdr-i | handcrafted | 257.0 | 0 | 4 | 381 |
| sal-cdr-i | isat3-cfg | 132.1 | 9 | 8 | 14 |
| sal-cdr-i | nuxmv | 2.4 | 0 | 1118 | 158 |
| sal-cdr-i | tcm | 25.4 | 0 | 0 | 1 |
| sal-cdr-i | total | 416.9 | 855 | 1971 | 986 |

## 2018 Comparison (Invalid/Valid)

| profile | family | sal-cdr | k-z3 | k-mathsat | bmc-z3 |
| --- | --- | --- | --- | --- | --- |
| sal-cdr | handcrafted | 1/2 | 1/2 | 1/2 | 1/0 |
| sal-cdr | isat3-cfg | 0/3 | 2/6 | 1/4 | 3/0 |
| sal-cdr | nuxmv | 0/0 | 0/0 | 0/0 | 0/0 |
| sal-cdr | tcm | 0/0 | 0/2 | 0/2 | 0/0 |
| sal-cdr | total | 1/5 | 25/22 | 13/20 | 26/0 |
| sal-cdr-i | handcrafted | 1/0 | 1/2 | 1/2 | 1/0 |
| sal-cdr-i | isat3-cfg | 3/0 | 2/6 | 1/4 | 3/0 |
| sal-cdr-i | nuxmv | 0/0 | 0/0 | 0/0 | 0/0 |
| sal-cdr-i | tcm | 0/0 | 0/2 | 0/2 | 0/0 |
| sal-cdr-i | total | 4/0 | 25/22 | 13/20 | 26/0 |
