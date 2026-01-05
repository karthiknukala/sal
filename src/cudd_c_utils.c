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

/*
 * Use compatibility header that handles CUDD 2.x vs 3.x differences
 */
#include "cudd_compat.h"

/*
 * Fix a compilation warning: both cudd.h and bigloo.h may
 * define assert as a macro. gcc complains that assert is
 * redefined.
 */
#ifdef assert
#undef assert
#endif

#include "bigloo.h"

/*
 * Modern Bigloo (4.x) may not export GC_finalize_on_demand directly.
 * We use a wrapper function instead.
 */
extern int GC_finalize_on_demand;

/*
 * Compatibility macros for Bigloo 4.x
 * In older Bigloo, FOREIGN_TO_COBJ was used; in modern versions,
 * we may need CREF or direct access depending on the object type.
 */
#ifndef FOREIGN_TO_COBJ
#  define FOREIGN_TO_COBJ(x) ((void *)CREF(x))
#endif

/*
 * GC_PTR type for finalizer callbacks - modern GC uses void*
 */
#ifndef GC_PTR
typedef void * GC_PTR;
#endif

DdManager * cudd_new_manager() {
  GC_finalize_on_demand = 0;
  return Cudd_Init(0,0,CUDD_UNIQUE_SLOTS,CUDD_CACHE_SLOTS,0);
}


void check_cudd(DdManager * manager) {
  if (Cudd_DebugCheck(manager) != 0) {
    C_FAILURE("CuddCheck", "Bugs were found in CUDD consistency check", BUNSPEC);
  }
}

int num_finalizations=0;

static void cudd_dec_ref_finalizer(GC_PTR obj, GC_PTR data)
{
  DdNode * node = (DdNode *) FOREIGN_TO_COBJ((obj_t) obj);
  DdManager * manager = (DdManager *) FOREIGN_TO_COBJ((obj_t) data);

  num_finalizations++;
  Cudd_RecursiveDeref(manager, node);
}

static void cudd_manager_finalizer(GC_PTR obj, GC_PTR data) 
{
  DdManager * manager = (DdManager *) FOREIGN_TO_COBJ((obj_t) obj); 
  Cudd_Quit(manager);
}

void cudd_register_node(obj_t manager, obj_t node)
{
  GC_register_finalizer(node, cudd_dec_ref_finalizer, manager, 0, 0);
}

void cudd_register_manager(obj_t manager) 
{
  GC_register_finalizer(manager, cudd_manager_finalizer, 0, 0, 0);
}

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

/***************************************************************

Bruno found the GC interaction bug that was happening on the MAC.

On the MAC plataform registers are used to pass arguments. So,
the following problem may happen:

1) (bdd/or n1 n2) is called, and n1 and n2 are dead after the call.
 Assume the registers r3 and r4 are used to store n1 and n2.

2) The Cudd nodes associated with n1 and n2 are sent to Cudd_bddOr,
   in the process r3 and r4 are overwritten. Therefore, there isn't
   any reference to n1 and n2. 

3) Cudd_bddOr may invoke the CUDD GC, suppose it invokes it.
   
4) The CUDD GC invokes cudd_gc_pre_hook, which invokes GC_gcollect and
   GC_invoke_finalizers. Since n1 and n2 are not referenced, they are
   garbage collected. 

5) When n1 and n2 are finalized the reference counter of the associated
   nodes is decremented, and they can be garbage collected by CUDD. This
   is a BUG, since n1 and n2 will be used by Cudd_bddOr.

So, the bug happens when:

1) n1 and/or n2 are not used after the call (bdd/or n1 n2)
2) registers are used to pass arguments
3) n1 and n2 are stored in registers that are overwritten before
   calling the CUDD GC

Solution:
  Increment the reference counter of the CUDD nodes associated
  with n1 and n2 before calling the CUDD function, and decrement
  them after the call.
 
***************************************************************/

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
  /* Cudd_Not is a macro, so I don't need to increment/decrement the reference count of n */
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


/**********************************************************/


int * result_cube;

DdGen * cudd_first_cube (DdManager * m, DdNode * n) {
  CUDD_VALUE_TYPE value;
  DdGen * result = Cudd_FirstCube(m, n, &result_cube, &value);
  /* printf("result = %d\n", result_cube[0]); */
  return result;
}

int cudd_next_cube (DdGen * gen) {
  CUDD_VALUE_TYPE value;
  return Cudd_NextCube(gen, &result_cube, &value);
}

/* return the highest position (in the current order) of a variable in the given cube */
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

int * cudd_result_cube() {
  return result_cube;
}

void cudd_enable_sift_reordering(DdManager * manager) {
  Cudd_AutodynEnable(manager, CUDD_REORDER_SIFT);
}

void cudd_enable_symm_sift_reordering(DdManager * manager) {
  Cudd_AutodynEnable(manager, CUDD_REORDER_SYMM_SIFT);
}

int cudd_indent = 4;
int verbose_mode = 0;
int reorder_increment = 200000;

void cudd_set_verbose_mode() {
  verbose_mode = 1;
}

void cudd_set_reorder_increment(int n) {
  reorder_increment = n;
}

void cudd_show_indent() {
  int i;
  for(i = 0; i < cudd_indent; i++)
    fprintf(stderr, " ");
}

/***********************************
The CUDD GC is always executed 


*************************************/


int cudd_gc_pre_hook(DdManager * manager, const char * msg, void * value) {
  unsigned int num_dead;
  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "SALenv GC... ");
    fflush(stderr);
  }
  GC_gcollect();  
  GC_invoke_finalizers(); 
  if (verbose_mode)
    fprintf(stderr, "done\n");
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
    fprintf(stderr, "begin BDD reordering (node count: %d)...\n", n);
    fflush(stderr);
  }
  return 1;
}

int cudd_reordering_post_hook(DdManager * manager, const char * msg, void * value) {
  long n = Cudd_ReadNodeCount(manager);
  int curr = Cudd_ReadNextReordering(manager);
  Cudd_SetNextReordering(manager, curr + reorder_increment);
  /* printf("curr = %d\n", curr); 
     printf("new curr = %d\n", Cudd_ReadNextReordering(manager)); */
  if (verbose_mode) {
    cudd_show_indent();
    fprintf(stderr, "end BDD reordering (node count: %d)...\n", n);
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

/*
 * CUDD 3.x compatibility: Helper functions for array operations on
 * opaque types. These are needed because DdNode and DdGen are opaque
 * in CUDD 3.x, so Bigloo can't generate array access code directly.
 */

/* Allocate an array of DdNode pointers */
DdNode ** cudd_node_array_alloc(int n) {
  return (DdNode **)GC_MALLOC(n * sizeof(DdNode *));
}

/* Set element in DdNode* array */
void cudd_node_array_set(DdNode ** arr, int i, DdNode * val) {
  arr[i] = val;
}

/* Get element from DdNode* array */
DdNode * cudd_node_array_ref(DdNode ** arr, int i) {
  return arr[i];
}

/* Check if array pointer is null */
int cudd_node_array_null(DdNode ** arr) {
  return arr == NULL;
}

/* Check if generator pointer is null */
int cudd_gen_null(DdGen * gen) {
  return gen == NULL;
}

/* Check if node pointer is null */
int cudd_node_null(DdNode * node) {
  return node == NULL;
}
