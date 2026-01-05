/*
 * SAL CUDD Compatibility Header
 *
 * Handles differences between CUDD 2.x and CUDD 3.x:
 * - CUDD 3.x: util.h is merged into cudd.h, cuddInt.h is internal only
 * - CUDD 2.x: requires separate util.h and cuddInt.h
 */

#ifndef CUDD_COMPAT_H
#define CUDD_COMPAT_H

/*
 * Standard C headers required by CUDD 3.x's cudd.h
 * These must be included before cudd.h because it uses FILE and size_t
 * without including the necessary headers.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

/*
 * Forward declaration for MtrNode (Multi-way Tree Node)
 * In CUDD 3.x, mtr.h is not installed as a public header, but the
 * MtrNode type is used in cudd.h function declarations.
 * We provide a forward declaration to allow compilation.
 */
#ifdef CUDD_VERSION_3
/* Forward declaration for CUDD 3.x where mtr.h is internal */
typedef struct MtrNode MtrNode;

/*
 * MTR constants from mtr.h (not publicly installed in CUDD 3.x)
 * These are needed for Cudd_MakeTreeNode which is conditionally
 * available in cudd.h when MTR_H_ is defined.
 */
#define MTR_H_  /* Enable MTR declarations in cudd.h */
#define MTR_DEFAULT 0x00000000
#define MTR_FIXED   0x00002000
#define MTR_TERMINAL 0x00000001
#define MTR_SOFT    0x00000002
#define MTR_NEWNODE 0x00000004
#endif

#include "cudd.h"

/* 
 * CUDD 2.x required util.h for utility functions.
 * In CUDD 3.x, these are either in cudd.h or removed.
 */
#ifndef CUDD_VERSION_3
#include "util.h"
#endif

/*
 * cuddInt.h contains internal CUDD data structures.
 * In CUDD 3.x, these structures are opaque and shouldn't be accessed.
 * For CUDD 2.x, we may need some internal definitions.
 * 
 * Note: Most SAL code should work without cuddInt.h.
 * If specific internal access is needed, it should be wrapped
 * in version-specific code.
 */
#ifndef CUDD_VERSION_3
/* Only include cuddInt.h for CUDD 2.x if it exists */
/* #include "cuddInt.h" */
#endif

#endif /* CUDD_COMPAT_H */

