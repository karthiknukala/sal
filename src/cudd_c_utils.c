/*
 * SAL 3.1, Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
 *
 * SAL is free software; you can redistribute it and/or 
 * modify it under the terms of the GNU General Public License 
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 * GNU General Public License for more details. 
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software 
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
 *
 */

#include "cudd_compat.h"

#ifdef assert
#undef assert
#endif

#include "bigloo.h"

extern int GC_finalize_on_demand;

#ifndef FOREIGN_TO_COBJ
#  define FOREIGN_TO_COBJ(x) ((void *)CREF(x))
#endif

#ifndef GC_PTR
typedef void * GC_PTR;
#endif

/*===========================================================================
 * MEMORY MANAGEMENT FOR ARM64 (Apple Silicon)
 *
 * Problem: On ARM64, Boehm GC finalizers can run during CUDD operations,
 * causing race conditions that corrupt CUDD's internal structures.
 *
 * Solution: Don't use Boehm GC finalizers for BDD nodes. Instead:
 * 1. Disable finalization for CUDD nodes
 * 2. Provide periodic cleanup at Scheme-level safe points
 * 3. Accept some memory overhead for automatic cleanup via Scheme-level tracking
 *
 * The Scheme code in bdd.scm tracks nodes and can call cleanup periodically.
 *===========================================================================*/

/*===========================================================================
 * GC Finalization Control
 *===========================================================================*/

void sal_enable_full_finalization(void) {
  GC_finalize_on_demand = 0;
}

void sal_disable_full_finalization(void) {
  GC_finalize_on_demand = 1;
}

/* Stubs for deferred finalization interface - not used in this implementation */
void cudd_process_deferred_finalizations(void) {
  /* Cleanup is handled at Scheme level */
}

int cudd_deferred_queue_size(void) {
  return 0;
}

void cudd_enter_operation(void) {
  /* No-op */
}

void cudd_leave_operation(void) {
  /* No-op */
}

int cudd_in_operation(void) {
  return 0;
}

/*===========================================================================
 * Manager and Node Creation/Registration
 *===========================================================================*/

DdManager * cudd_new_manager(void) {
  DdManager * result;
  /* Disable automatic finalization to prevent bus errors on ARM64 */
  GC_finalize_on_demand = 1;
  result = Cudd_Init(0,0,CUDD_UNIQUE_SLOTS,CUDD_CACHE_SLOTS,0);
  return result;
}

void check_cudd(DdManager * manager) {
  if (Cudd_DebugCheck(manager) != 0) {
    C_FAILURE("CuddCheck", "Bugs were found in CUDD consistency check", BUNSPEC);
  }
}

int num_finalizations = 0;

void cudd_register_node(obj_t manager, obj_t node)
{
  /*
   * DO NOT use Boehm GC finalizers for BDD nodes on ARM64.
   * Memory cleanup is handled at the Scheme level via periodic collection.
   */
  (void)manager;
  (void)node;
}

void cudd_register_manager(obj_t manager) 
{
  /* Don't register manager finalizer either */
  (void)manager;
}

/*===========================================================================
 * Basic Node Operations
 *===========================================================================*/

DdNode * cudd_then(DdNode * n) {
  return Cudd_T(n);
}

DdNode * cudd_else(DdNode * n) {
  return Cudd_E(n);
}

int cudd_is_complement(DdNode * n) {
  return Cudd_IsComplement(n);
}

int cudd_is_constant(DdNode * n) {
  return Cudd_IsConstant(n);
}

/*===========================================================================
 * Safe CUDD Operations
 *
 * These wrappers protect arguments from being collected during the call
 * by incrementing reference counts before and decrementing after.
 *===========================================================================*/

DdNode * s_Cudd_ReadOne(DdManager * manager) {
  DdNode * result = Cudd_ReadOne(manager);
  if (result != 0)
    Cudd_Ref(result);
  return result;
}

DdNode * s_Cudd_bddNewVar(DdManager * manager) {
  DdNode * result = Cudd_bddNewVar(manager);
  if (result != 0)
    Cudd_Ref(result);
  return result;
}

DdNode * s_Cudd_bddIthVar(DdManager * manager, unsigned i) {
  DdNode * result = Cudd_bddIthVar(manager, i);
  if (result != 0)
    Cudd_Ref(result);
  return result;
}

DdNode * s_Cudd_Not(DdNode * n) {
  DdNode * result = Cudd_Not(n);
  assert(result != 0);
  Cudd_Ref(result);
  return result;
}

#define MAKE_CUDD_UNARY_OP(CUDD_FUN)  \
DdNode * s_##CUDD_FUN(DdManager * manager, DdNode * n1) {  \
  DdNode * result; \
  Cudd_Ref(n1);  \
  result = CUDD_FUN(manager, n1); \
  if (result != 0)  \
     Cudd_Ref(result); \
  Cudd_RecursiveDeref(manager, n1); \
  return result; \
} 

#define MAKE_CUDD_BINARY_OP(CUDD_FUN)  \
DdNode * s_##CUDD_FUN(DdManager * manager, DdNode * n1, DdNode * n2) {  \
  DdNode * result; \
  Cudd_Ref(n1);  \
  Cudd_Ref(n2); \
  result = CUDD_FUN(manager, n1, n2); \
  if (result != 0)  \
     Cudd_Ref(result); \
  Cudd_RecursiveDeref(manager, n1); \
  Cudd_RecursiveDeref(manager, n2); \
  return result; \
} 

#define MAKE_CUDD_TERNARY_OP(CUDD_FUN)  \
DdNode * s_##CUDD_FUN(DdManager * manager, DdNode * n1, DdNode * n2, DdNode * n3) {  \
  DdNode * result; \
  Cudd_Ref(n1);  \
  Cudd_Ref(n2); \
  Cudd_Ref(n3); \
  result = CUDD_FUN(manager, n1, n2, n3); \
  if (result != 0)  \
     Cudd_Ref(result); \
  Cudd_RecursiveDeref(manager, n1); \
  Cudd_RecursiveDeref(manager, n2); \
  Cudd_RecursiveDeref(manager, n3); \
  return result; \
} 


MAKE_CUDD_BINARY_OP(Cudd_bddAnd);
MAKE_CUDD_BINARY_OP(Cudd_bddOr);
MAKE_CUDD_BINARY_OP(Cudd_bddNor);
MAKE_CUDD_BINARY_OP(Cudd_bddXor);
MAKE_CUDD_BINARY_OP(Cudd_bddXnor);
MAKE_CUDD_BINARY_OP(Cudd_bddNand);
MAKE_CUDD_BINARY_OP(Cudd_bddRestrict);
MAKE_CUDD_BINARY_OP(Cudd_bddConstrain);
MAKE_CUDD_BINARY_OP(Cudd_bddSqueeze);
MAKE_CUDD_BINARY_OP(Cudd_bddExistAbstract);
MAKE_CUDD_BINARY_OP(Cudd_bddUnivAbstract);
MAKE_CUDD_TERNARY_OP(Cudd_bddIte);
MAKE_CUDD_TERNARY_OP(Cudd_bddAndAbstract);
MAKE_CUDD_UNARY_OP(Cudd_Support);
MAKE_CUDD_UNARY_OP(Cudd_bddVarMap);

DdNode * s_Cudd_bddAndLimit(DdManager * manager, DdNode * n1, DdNode * n2, unsigned max) {  
  DdNode * result; 
  Cudd_Ref(n1);  
  Cudd_Ref(n2); 
  result = Cudd_bddAndLimit(manager, n1, n2, max); 
  if (result != 0) 
    Cudd_Ref(result); 
  Cudd_RecursiveDeref(manager, n1); 
  Cudd_RecursiveDeref(manager, n2); 
  return result; 
}

DdNode * s_Cudd_bddCompose(DdManager * manager, DdNode * n1, DdNode * n2, int var_id) {  
  DdNode * result; 
  Cudd_Ref(n1);  
  Cudd_Ref(n2); 
  result = Cudd_bddCompose(manager, n1, n2, var_id); 
  if (result != 0) 
    Cudd_Ref(result); 
  Cudd_RecursiveDeref(manager, n1); 
  Cudd_RecursiveDeref(manager, n2); 
  return result; 
}

DdNode * s_Cudd_IndicesToCube(DdManager * manager, int * indices, int n) {
  DdNode * result = Cudd_IndicesToCube(manager, indices, n);
  if (result != 0)
    Cudd_Ref(result);
  return result;
}

DdNode * s_Cudd_bddSwapVariables(DdManager * manager, DdNode * n1, DdNode ** arr1, DdNode ** arr2, int num) {
  DdNode * result; 
  Cudd_Ref(n1);  
  result = Cudd_bddSwapVariables(manager, n1, arr1, arr2, num);
  if (result != 0)
    Cudd_Ref(result);
  Cudd_RecursiveDeref(manager, n1);
  return result;
}

DdNode * s_Cudd_bddPickOneMinterm(DdManager * manager, DdNode * n1, DdNode ** vars, int num) {
  DdNode * result; 
  Cudd_Ref(n1);  
  result = Cudd_bddPickOneMinterm(manager, n1, vars, num);
  if (result != 0)
    Cudd_Ref(result);
  Cudd_RecursiveDeref(manager, n1);
  return result;
}

DdNode ** s_Cudd_bddPickArbitraryMinterms(DdManager * manager, DdNode * n1, DdNode ** vars, int num_vars, int num_minterms) {
  DdNode ** result; 
  Cudd_Ref(n1);  
  result = Cudd_bddPickArbitraryMinterms(manager, n1, vars, num_vars, num_minterms);
  if (result != 0) {
    int i;
    for (i = 0; i < num_minterms; i++)
      Cudd_Ref(result[i]);
  }
  Cudd_RecursiveDeref(manager, n1);
  return result;
}

DdNode * s_Cudd_SubsetCompress(DdManager * manager, DdNode * n1, int num_vars, int threshold) {
  DdNode * result; 
  Cudd_Ref(n1);  
  result = Cudd_SubsetCompress(manager, n1, num_vars, threshold);
  if (result != 0)
    Cudd_Ref(result);
  Cudd_RecursiveDeref(manager, n1);
  return result;
}


/*===========================================================================
 * Cube Operations
 *===========================================================================*/

int * result_cube;

DdGen * cudd_first_cube (DdManager * m, DdNode * n) {
  CUDD_VALUE_TYPE value;
  DdGen * result = Cudd_FirstCube(m, n, &result_cube, &value);
  return result;
}

int cudd_next_cube (DdGen * gen) {
  CUDD_VALUE_TYPE value;
  return Cudd_NextCube(gen, &result_cube, &value);
}

int cudd_get_maximum_pos(DdManager * m, DdNode * cube) {
  int num_vars = Cudd_ReadSize(m);
  int max = 0;
  int i;
  cudd_first_cube(m, cube);
  for(i = 0; i < num_vars; i++) {
    if (result_cube[i] == 1) {
      int pos = Cudd_ReadIndex(m, i);
      if (pos > max)
        max = pos;
    }
  }
  return max;
}

void copy_cube_to_vector(obj_t vector, int num_vars)
{
  static obj_t dont_care = NULL;
  int i;
  if (dont_care == NULL)
    dont_care = string_to_symbol("x");
  for(i = 0; i < num_vars; i++) {
    if (result_cube[i] == 0)
      VECTOR_SET(vector, i, BFALSE);
    else if (result_cube[i] == 1)
      VECTOR_SET(vector, i, BTRUE);
    else
      VECTOR_SET(vector, i, dont_care);
  }
}

int * cudd_result_cube(void) {
  return result_cube;
}

/*===========================================================================
 * Reordering
 *===========================================================================*/

void cudd_enable_sift_reordering(DdManager * manager) {
  Cudd_AutodynEnable(manager, CUDD_REORDER_SIFT);
}

void cudd_enable_symm_sift_reordering(DdManager * manager) {
  Cudd_AutodynEnable(manager, CUDD_REORDER_SYMM_SIFT);
}

/*===========================================================================
 * Verbose Mode and Display
 *===========================================================================*/

int cudd_indent = 4;
int verbose_mode = 0;
int reorder_increment = 200000;

void cudd_set_verbose_mode(void) {
  verbose_mode = 1;
}

void cudd_set_reorder_increment(int n) {
  reorder_increment = n;
}

void cudd_show_indent(void) {
  int i;
  for(i = 0; i < cudd_indent; i++)
    fprintf(stderr, " ");
}

/*===========================================================================
 * CUDD GC Hooks
 *===========================================================================*/

int cudd_gc_pre_hook(DdManager * manager, const char * msg, void * value) {
  unsigned int num_dead;

  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "SALenv GC... done\n");
  }
  
  num_dead = Cudd_ReadDead(manager);
  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "begin BDD GC (dead node count: %u)...\n", num_dead);
    fflush(stderr);
  }
  return 1;
}

int cudd_gc_post_hook(DdManager * manager, const char * msg, void * value) {
  unsigned int num_dead = Cudd_ReadDead(manager);
  
  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "end BDD GC (dead node count: %u)...\n", num_dead);
    fflush(stderr);
  }
  return 1;
}

int cudd_reordering_pre_hook(DdManager * manager, const char * msg, void * value) {
  long n = Cudd_ReadNodeCount(manager);
  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "begin BDD reordering (node count: %ld)...\n", n);
    fflush(stderr);
  }
  return 1;
}

int cudd_reordering_post_hook(DdManager * manager, const char * msg, void * value) {
  long n = Cudd_ReadNodeCount(manager);
  int curr = Cudd_ReadNextReordering(manager);
  Cudd_SetNextReordering(manager, curr + reorder_increment);
  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "end BDD reordering (node count: %ld)...\n", n);
    fflush(stderr);
  }
  return 1;
}

void cudd_enable_hooks(DdManager * manager) {
  Cudd_AddHook(manager, cudd_gc_pre_hook, CUDD_PRE_GC_HOOK);
  Cudd_AddHook(manager, cudd_gc_post_hook, CUDD_POST_GC_HOOK);
  Cudd_AddHook(manager, cudd_reordering_pre_hook, CUDD_PRE_REORDERING_HOOK);
  Cudd_AddHook(manager, cudd_reordering_post_hook, CUDD_POST_REORDERING_HOOK);
}

MtrNode * cudd_group_vars_default(DdManager * manager, int low, int n) {
  return Cudd_MakeTreeNode(manager, low, n, MTR_DEFAULT);
}

MtrNode * cudd_group_vars_fixed(DdManager * manager, int low, int n) {
  return Cudd_MakeTreeNode(manager, low, n, MTR_FIXED);
}

int cudd_gen_dot_file(DdManager * manager, int n, DdNode ** nodes, char * file) {
  FILE * f = fopen(file, "w");
  int result;
  if (f == NULL)
    return 0;
  result = Cudd_DumpDot(manager, n, nodes, NULL, NULL, f);
  fclose(f);
  return result;
}

/*===========================================================================
 * CUDD 3.x Compatibility Helpers
 *===========================================================================*/

DdNode ** cudd_node_array_alloc(int n) {
  return (DdNode **)GC_MALLOC(n * sizeof(DdNode *));
}

void cudd_node_array_set(DdNode ** arr, int i, DdNode * val) {
  arr[i] = val;
}

DdNode * cudd_node_array_ref(DdNode ** arr, int i) {
  return arr[i];
}

int cudd_node_array_null(DdNode ** arr) {
  return arr == NULL;
}

int cudd_gen_null(DdGen * gen) {
  return gen == NULL;
}

int cudd_node_null(DdNode * node) {
  return node == NULL;
}
