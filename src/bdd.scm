;;
;; SAL 3.1, Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License 
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details. 
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software 
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
;;

(module bdd
        (extern 
         ;; Use compatibility header that handles CUDD 2.x vs 3.x differences
         (include "cudd_compat.h")
         (type bdd-reorder
               (enum (same "CUDD_REORDER_SAME")
                     (none "CUDD_REORDER_NONE")
                     (random "CUDD_REORDER_RANDOM")
                     (random-pivot "CUDD_REORDER_RANDOM_PIVOT")
                     (sift "CUDD_REORDER_SIFT")
                     (sift-converge "CUDD_REORDER_SIFT_CONVERGE")
                     (symm-sift "CUDD_REORDER_SYMM_SIFT")
                     (symm-sift-conv "CUDD_REORDER_SYMM_SIFT_CONV")
                     (window2 "CUDD_REORDER_WINDOW2")
                     (window3 "CUDD_REORDER_WINDOW3")
                     (window4 "CUDD_REORDER_WINDOW4")
                     (window2-conv "CUDD_REORDER_WINDOW2_CONV")
                     (window3-conv "CUDD_REORDER_WINDOW3_CONV")
                     (window4-conv "CUDD_REORDER_WINDOW4_CONV")
                     (group-sift "CUDD_REORDER_GROUP_SIFT")
                     (group-sift-conv "CUDD_REORDER_GROUP_SIFT_CONV")
                     (annealing "CUDD_REORDER_ANNEALING")
                     (genetic "CUDD_REORDER_GENETIC")
                     (linear "CUDD_REORDER_LINEAR")
                     (linear-converge "CUDD_REORDER_LINEAR_CONVERGE")
                     (lazy-sift "CUDD_REORDER_LAZY_SIFT")
                     (exact "CUDD_REORDER_EXACT"))
               "Cudd_ReorderingType")
         (type CuddManager (opaque) "DdManager *")
         (type CuddMtrNode (opaque) "MtrNode *")

;; 
;; CUDD 3.x compatibility: DdNode and DdGen are opaque types whose
;; internal structure is not exposed in the public headers. We declare
;; them as opaque pointer types and use C helper functions for any
;; operations that would require knowing the struct size.
;;
;; For CuddNodeArray (DdNode **), we use void* to avoid needing struct size
;; for array operations. C helper functions handle the actual array access.
;;
         (type CuddNode (opaque) "DdNode *")
         (type IntPtr (pointer int) "int *")
         (type CuddNodeArray (opaque) "DdNode **")
         (type CuddGen (opaque) "DdGen *")
         ;; C helper functions for array operations on opaque types
         (cudd-node-array-alloc::CuddNodeArray (::int) "cudd_node_array_alloc")
         (cudd-node-array-set!::void (::CuddNodeArray ::int ::CuddNode) "cudd_node_array_set")
         (cudd-node-array-ref::CuddNode (::CuddNodeArray ::int) "cudd_node_array_ref")
         (cudd-node-array-null?::bool (::CuddNodeArray) "cudd_node_array_null")
         (cudd-gen-null?::bool (::CuddGen) "cudd_gen_null")
         (cudd-node-null?::bool (::CuddNode) "cudd_node_null")
         (cudd-new-manager::CuddManager () "cudd_new_manager")
         (cudd-read-one::CuddNode (::CuddManager) "s_Cudd_ReadOne")
         (cudd-ref::void (::CuddNode) "Cudd_Ref")
         (cudd-deref::void (::CuddNode) "Cudd_Deref")
         (cudd-recursive-deref::void (::CuddManager ::CuddNode) "Cudd_RecursiveDeref")
         (cudd-and::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddAnd")
         (cudd-and-limit::CuddNode (::CuddManager ::CuddNode ::CuddNode ::uint) "s_Cudd_bddAndLimit")
         (cudd-nand::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddNand")
         (cudd-or::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddOr")
         (cudd-nor::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddNor")
         (cudd-xor::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddXor")
         (cudd-xnor::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddXnor")
         (cudd-ite::CuddNode (::CuddManager ::CuddNode ::CuddNode ::CuddNode) "s_Cudd_bddIte")
         (cudd-not::CuddNode (::CuddNode) "s_Cudd_Not")
         (cudd-new-var::CuddNode (::CuddManager) "s_Cudd_bddNewVar")
         (cudd-ith-var::CuddNode (::CuddManager ::int) "s_Cudd_bddIthVar")
         (cudd-var-order::int (::CuddManager ::int) "Cudd_ReadPerm")
         (cudd-var-at-pos::int (::CuddManager ::int) "Cudd_ReadInvPerm")
         (cudd-exists::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddExistAbstract")
         (cudd-for-all::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddUnivAbstract")
         (cudd-leq::int (::CuddManager ::CuddNode ::CuddNode) "Cudd_bddLeq")
         (cudd-compose::CuddNode (::CuddManager ::CuddNode ::CuddNode ::int) "s_Cudd_bddCompose")
         (cudd-and-abstract::CuddNode (::CuddManager ::CuddNode ::CuddNode ::CuddNode) "s_Cudd_bddAndAbstract")
         (cudd-print-debug::int (::CuddManager ::CuddNode ::int ::int) "Cudd_PrintDebug")
         (cudd-print-minterm::int (::CuddManager ::CuddNode) "Cudd_PrintMinterm")
         (cudd-delete-manager::void (::CuddManager) "Cudd_Quit")
         (cudd-size::int (::CuddNode) "Cudd_DagSize")
         (cudd-num-vars::int (::CuddManager) "Cudd_ReadSize")
         (cudd-indices-to-cube::CuddNode (::CuddManager ::IntPtr ::int) "s_Cudd_IndicesToCube")
         (cudd-first-cube::CuddGen (::CuddManager ::CuddNode) "cudd_first_cube")
         (cudd-next-cube::int (::CuddGen) "cudd_next_cube")
         (fill-vector-with-cube::void (::obj ::int) "copy_cube_to_vector")
         (cudd-swap-vars::CuddNode (::CuddManager ::CuddNode ::CuddNodeArray ::CuddNodeArray ::int) "s_Cudd_bddSwapVariables")
         (cudd-set-var-map::int (::CuddManager ::CuddNodeArray ::CuddNodeArray ::int) "Cudd_SetVarMap") 
         (cudd-var-map::CuddNode (::CuddManager ::CuddNode) "s_Cudd_bddVarMap")
         (cudd-count-minterm::double (::CuddManager ::CuddNode ::int) "Cudd_CountMinterm")
         (cudd-enable-hooks::void (::CuddManager) "cudd_enable_hooks")
         (cudd-set-next-var::int (::CuddManager ::int) "Cudd_bddSetNsVar")
         (cudd-set-curr-var::int (::CuddManager ::int) "Cudd_bddSetPsVar")
         (cudd-set-input-var::int (::CuddManager ::int) "Cudd_bddSetPiVar") 
         (cudd-enable-dynamic-reordering::void (::CuddManager ::bdd-reorder) "Cudd_AutodynEnable")
         (cudd-disable-dynamic-reordering::void (::CuddManager) "Cudd_AutodynDisable")
         (cudd-register-node::void (::obj ::obj) "cudd_register_node")
         (cudd-register-manager::void (::obj) "cudd_register_manager")
         (cudd-group-vars-default::CuddMtrNode (::CuddManager ::int ::int) "cudd_group_vars_default")
         (cudd-group-vars-fixed::CuddMtrNode (::CuddManager ::int ::int) "cudd_group_vars_fixed")
         (cudd-reduce-heap::int (::CuddManager ::bdd-reorder ::int) "Cudd_ReduceHeap")
         (cudd-set-next-reordering::void (::CuddManager ::uint) "Cudd_SetNextReordering")
         (cudd-read-next-reordering::uint (::CuddManager) "Cudd_ReadNextReordering")
         (cudd-gen-dot-file::int (::CuddManager ::int ::CuddNodeArray ::string) "cudd_gen_dot_file")
         (cudd-support::CuddNode (::CuddManager ::CuddNode) "s_Cudd_Support")
         (cudd-peek-one-min-term::CuddNode (::CuddManager ::CuddNode ::CuddNodeArray ::int) "s_Cudd_bddPickOneMinterm")
         (cudd-peek-min-terms::CuddNodeArray (::CuddManager ::CuddNode ::CuddNodeArray ::int ::int) "s_Cudd_bddPickArbitraryMinterms")
         (cudd-set-reoder-increment::void (::int) "cudd_set_reorder_increment")
         (cudd-set-verbose-mode::void () "cudd_set_verbose_mode")
         (cudd-set-sift-max-swap::void (::CuddManager ::int) "Cudd_SetSiftMaxSwap")
         (cudd-set-sift-max-var::void (::CuddManager ::int) "Cudd_SetSiftMaxVar")
         (cudd-get-max-pos::int (::CuddManager ::CuddNode) "cudd_get_maximum_pos")
         (cudd-num-nodes::long (::CuddManager) "Cudd_ReadNodeCount")
         (cudd-restrict::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddRestrict")
         (cudd-constrain::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddConstrain")
         (cudd-squeeze::CuddNode (::CuddManager ::CuddNode ::CuddNode) "s_Cudd_bddSqueeze")
         (cudd-debug-check::int (::CuddManager) "Cudd_DebugCheck")
         (cudd-then::CuddNode (::CuddNode) "cudd_then")
         (cudd-else::CuddNode (::CuddNode) "cudd_else")
         (cudd-is-constant::int (::CuddNode) "cudd_is_constant")
         (cudd-is-complement::int (::CuddNode) "cudd_is_complement")
         (cudd-var::uint (::CuddNode) "Cudd_NodeReadIndex")
         (cudd-estimate-cofactor-simple::int (::CuddNode ::int) "Cudd_EstimateCofactorSimple")
         (cudd-estimate-cofactor::int (::CuddManager ::CuddNode ::int ::int) "Cudd_EstimateCofactor")
         (cudd-subset-compress::CuddNode (::CuddManager ::CuddNode ::int ::int) "s_Cudd_SubsetCompress")
         )
        (include "utility.sch")
        (include "scmobj.sch")
        (include "api.sch")
        (import dot-interface front-end)
        (export 
         (make-bdd-manager)
         (bdd/num-nodes manager)
         (bdd/enable-finalization! flag)
         (bdd/set-curr-var! manager index)
         (bdd/set-next-var! manager index)
         (bdd/set-input-var! manager index)
         (bdd/var-order manager index)
         (bdd/var-at manager pos)
         (bdd-manager? obj)
         (bdd? obj)
         (bdd/complement? bdd)
         (bdd/constant? bdd)
         (bdd/manager bdd)
         (bdd/then bdd)
         (bdd/else bdd)
         (bdd/var bdd)
         (bdd/register-node manager bdd)
         (bdd/true manager)
         (bdd/false manager)
         (bdd/new-var manager)
         (bdd/ith-var manager i)
         (bdd/and bdd1 bdd2)
         (bdd/and-limit bdd1 bdd2 max-size)
         (bdd/nand bdd1 bdd2)
         (bdd/or bdd1 bdd2)
         (bdd/or-limit bdd1 bdd2 max-size)
         (bdd/xor bdd1 bdd2)
         (bdd/nor bdd1 bdd2)
         (bdd/xnor bdd1 bdd2)
         (bdd/diff bdd1 bdd2)
         (bdd/iff bdd1 bdd2)
         (bdd/ite bdd1 bdd2 bdd3)
         (bdd/not bdd)
         (bdd/size bdd)
         (bdd/print-debug bdd)
         (bdd/print-minterm bdd)
         (bdd/eq? bdd1 bdd2)
         (bdd/num-vars manager)
         (bdd/compose bdd1 bdd2 var-id)
         (bdd/first-cube-vector bdd)
         (bdd/next-cube-vector! generator cube-vector)
         (bdd/for-each-cube-vector proc bdd)
         (cube-vector->cube manager cube-vector)
         (bdd/le? bdd1 bdd2)
         (indices-list->cube manager indices-list)
         (indices-vector->cube manager indices-vector)
         (bdd/exists bdd indices)
         (bdd/for-all bdd indices)
         (bdd/and-exists bdd1 bdd2 indices)
         (indices-list->bdd-var-array manager indices-list)
         (indices-vector->bdd-var-array manager indices-vector)
         (bdd/swap-vars bdd bdd-var-array1 bdd-var-array2 n)
         (bdd/swap-vars+ bdd alist)
         (bdd/set-var-map! manager array1 array2 n)
         (bdd/set-var-map-with-alist! manager alist)
         (bdd/var-map bdd)
         (bdd/num-solutions bdd num-vars)
         (bdd/false? bdd)
         (bdd/true? bdd)
         (bdd/enable-dynamic-reordering! manager reordering-type)
         (bdd/disable-dynamic-reordering! manager)
         (bdd/reorder! manager reordering-type . min-size)
         (bdd/group-vars! manager first-id num-vars . fixed?)
         (bdd/set-next-reordering! manager n)
         (bdd/next-reordering manager)
         (bdd/cofactor f g)
         (bdd/show . bdds)
         (bdd/support bdd)
         (bdd/set-dynamic-reorder-initial-threshold i)
         (bdd/set-dynamic-reorder-threshold-increment i)
         (bdd/set-sift-max-var! manager n)
         (bdd/set-sift-max-swap! manager n)
         (bdd/cube-max-pos cube)
         (bdd/minimize bdd-f bdd-c)
         (bdd/between lower upper)
         (bdd/peek-one-min-term bdd vars n)
         (bdd/peek-min-terms bdd vars num-vars num-min-terms)
         (make-gc-collector-for m)
         (bdd/estimate-cofactor-simple bdd var-id)
         (bdd/estimate-cofactor bdd var-id phase)
         (bdd/subset-compress bdd num-vars threshold)
         )
        )

(front-end/add-toggle-option! 
 "BDD Interface" 
 "finalization" 
 "GC finalization. GC finalization allows the BDD package and SALenv garbagge collectors to interact (default: enabled)."
 (lambda (flag)
   (bdd/enable-finalization! flag)))

(define (id->reordering-type order-id)
  (case order-id
    ((bdd-reorder-same) (bdd-reorder-same))
    ((bdd-reorder-random) (bdd-reorder-random))
    ((bdd-reorder-random-pivot) (bdd-reorder-random-pivot))
    ((bdd-reorder-sift) (bdd-reorder-sift))
    ((bdd-reorder-sift-converge) (bdd-reorder-sift-converge)) 
    ((bdd-reorder-symm-sift) (bdd-reorder-symm-sift)) 
    ((bdd-reorder-symm-sift-conv) (bdd-reorder-symm-sift-conv)) 
    ((bdd-reorder-window2) (bdd-reorder-window2)) 
    ((bdd-reorder-window3) (bdd-reorder-window3)) 
    ((bdd-reorder-window4) (bdd-reorder-window4)) 
    ((bdd-reorder-window2-conv) (bdd-reorder-window2-conv)) 
    ((bdd-reorder-window3-conv) (bdd-reorder-window3-conv)) 
    ((bdd-reorder-window4-conv) (bdd-reorder-window4-conv)) 
    ((bdd-reorder-group-sift) (bdd-reorder-group-sift)) 
    ((bdd-reorder-group-sift-conv) (bdd-reorder-group-sift-conv)) 
    ((bdd-reorder-annealing) (bdd-reorder-annealing)) 
    ((bdd-reorder-genetic) (bdd-reorder-genetic)) 
    ((bdd-reorder-linear) (bdd-reorder-linear)) 
    ((bdd-reorder-linear-converge) (bdd-reorder-linear-converge)) 
    ((bdd-reorder-lazy-sift) (bdd-reorder-lazy-sift)) 
    ((bdd-reorder-exact) (bdd-reorder-exact)) 
    (else (error 'bdd/enable-dynamic-reordering! "Unknown BDD reordering type." order-id))))
  
(define-api (bdd/reorder! manager reordering-type-id . min-size)
  :doc "Reorders the BDD variables in the given manager. @code{reordering-type-id} is a symbol which specifies the reordering strategy. @code{min-size} is an optional argument and it disables variable reordering if the number of BDD nodes is less than the given argument. The following reordering stategies are available: @code{bdd-reorder-random}, @code{bdd-reorder-random-pivot}, @code{bdd-reorder-sift}, @code{bdd-reorder-sift-converge}, @code{bdd-reorder-symm-sift}, @code{bdd-reorder-symm-sift-conv}, @code{bdd-reorder-window2}, @code{bdd-reorder-window3}, @code{bdd-reorder-window4}, @code{bdd-reorder-window2-conv}, @code{bdd-reorder-window3-conv}, @code{bdd-reorder-window4-conv}, @code{bdd-reorder-group-sift}, @code{bdd-reorder-group-sift-conv}, @code{bdd-reorder-annealing}, @code{bdd-reorder-genetic}, @code{bdd-reorder-linear}, @code{bdd-reorder-linear-converge}, @code{bdd-reorder-lazy-sift}, @code{bdd-reorder-exact}."
  :examples '((bdd/reorder! manager 'bdd-reorder-sift)
              (bdd/reorder! manager 'bdd-reorder-sift-converge 10000))
  (unless (eq? reordering-type-id 'bdd-reorder-none)
    (let ((min-size (optional-arg min-size 1024)))
      (cudd-reduce-heap manager (id->reordering-type reordering-type-id) min-size))
    #unspecified))

(define-api (bdd/enable-dynamic-reordering! manager reordering-type)
  :doc "Enables dynamic variable reordering in the given BDD manager. @code{reordering-type} is a symbol which specifies the reordering strategy to be used. See @code{bdd/reorder!}."
  (if (eq? reordering-type 'bdd-reorder-none)
    (cudd-disable-dynamic-reordering manager)
    (cudd-enable-dynamic-reordering manager (id->reordering-type reordering-type)))
  #unspecified)

(define-api (bdd/disable-dynamic-reordering! manager)
  :doc "Disables dynamic variable reordering in the given BDD manager. See @code{bdd/enable-dynamic-reordering!}."
  (cudd-disable-dynamic-reordering manager)
  #unspecified)

(define *finalization?* #t)

(define-api (bdd/enable-finalization! flag)
  :doc "Finalization allows the Scheme garbage collector to interact with the CUDD (the BDD library) garbage collector. If finalization is disabled, the BDD nodes will not be garbage collected, and memory may be wasted. This function is useful for debugging purposes." 
  (set! *finalization?* flag))

(define *initial-reorder-threshold* 100000)

(define-api (bdd/set-dynamic-reorder-initial-threshold i)
  :doc "Sets the minimum number of nodes necessary to trigger dynamic variable reordering. See @code{bdd/set-dynamic-reorder-threshold-increment}."
  (set! *initial-reorder-threshold* i))

(define-api (bdd/set-dynamic-reorder-threshold-increment i)
  :doc "Dynamic reordering is performed when the number of BDD nodes is greater than a threshold @code{n}. After the a round of dynamic variable reordering it will only be activated again if the number of BDD nodes is greater than @code{n+i}, where @code{i} is the given argument. See @code{bdd/set-dynamic-reorder-initial-threshold}."
  (cudd-set-reoder-increment i)
  #unspecified)

(define-api (bdd/set-sift-max-var! manager n)
  :doc "Sets the number of variables that will be moved during the sift reordering strategy. See @code{bdd/reorder!}."
  (cudd-set-sift-max-var manager n)
  #unspecified)

(define-api (bdd/set-sift-max-swap! manager n)
  :doc "Sets the maximum number of swaps that can be performed during the sift reordering strategy. See @code{bdd/reorder!}."
  (cudd-set-sift-max-swap manager n)
  #unspecified)

(define-api (make-bdd-manager)
  :doc "Creates a new binary decision diagram (BDD) manager. BDD manager is responsible for memory management, variable reordering, and caching of BDD nodes."
  :examples '((begin
                (define m (make-bdd-manager))
                (define n1 (bdd/new-var m))
                (define n2 (bdd/new-var m))
                (define a (bdd/and n1 n2))
                (bdd/print-debug a)))
  (let ((result::obj (cudd-new-manager)))
    (cudd-enable-hooks result)
    (cudd-set-next-reordering result *initial-reorder-threshold*)
    (when (>= (verbosity-level) 10)
      (cudd-set-verbose-mode)
      #unspecified)
    (when *finalization?*
      (cudd-register-manager result)
      #unspecified)
    result))

(define-api (bdd/num-nodes manager)
  :doc "Returns the number of BDD nodes in the given manager."
  :examples '((begin
                (define m (make-bdd-manager))
                (bdd/num-nodes m)))
  (cudd-num-nodes manager))

(define-api (bdd-manager? obj)
  :doc "Returns @code{#t} if @code{obj} is a BDD manager."
  (CuddManager? obj))

(define-api (bdd? obj)
  :doc "Returns @code{#t} if @code{obj} is a BDD node."
  (and (pair? obj) (CuddNode? (cdr obj))))

(define-api (bdd/manager bdd)
  :doc "Returns the BDD manager which owns the BDD node @code{bdd}."
  :examples '((begin
                (define m (make-bdd-manager))
                (define n (bdd/new-var m))
                (eq? (bdd/manager n) m)))
  [assert (bdd) (bdd? bdd)]
  (car bdd))

(define (bdd/node bdd)
  [assert (bdd) (bdd? bdd)]
  (cdr bdd))

(define (bdd/cube-max-pos cube)
  [assert (cube) (bdd? cube)]
  (cudd-get-max-pos (car cube) (cdr cube)))

(define (bdd/register-node manager bdd-node)
  (when *finalization?*
    (cudd-register-node manager bdd-node)
    #unspecified)
  ;; [assert (manager bdd) (= (cudd-debug-check manager) 0)]
  (cons manager bdd-node))

(define (bdd/complement? bdd)
  (= (cudd-is-complement (bdd/node bdd)) 1))

(define (bdd/constant? bdd)
  (= (cudd-is-constant (bdd/node bdd)) 1))

;; the node is not registered, it is resposability of the user to register it
(define (bdd/then bdd)
  (when (bdd/constant? (bdd/node bdd))
    (error 'bdd/then "bdd/then was called on a constant BDD." #unspecified))
  (cudd-then (bdd/node bdd)))

;; the node is not registered, it is resposability of the user to register it
(define (bdd/else bdd)
  (when (bdd/constant? (bdd/node bdd))
    (error 'bdd/then "bdd/else was called on a constant BDD." #unspecified))
  (cudd-else (bdd/node bdd)))

(define (bdd/var bdd)
  (cudd-var (bdd/node bdd)))

(define (bdd/set-curr-var! manager index)
  (= (cudd-set-curr-var manager index) 1))

(define (bdd/set-next-var! manager index)
  (= (cudd-set-next-var manager index) 1))

(define (bdd/set-input-var! manager index)
  (= (cudd-set-input-var manager index) 1))

(define (bdd/var-order manager index)
  (cudd-var-order manager index))

(define (bdd/var-at manager pos)
  (cudd-var-at-pos manager pos))

(define-inline (check-result result fun-name)
  (when (cudd-node-null? result)
    (error 'bdd-interface (string-append "Error executing function " (symbol->string fun-name) ". This error is usually produced when the program is running out of memory.") #unspecified)))

(define-macro (define-bdd-proc cudd-fun external-name args . svsv)
  `(define-api (,external-name ,@args)
     ,@svsv
     (let ((result::obj (,cudd-fun ,@args)))
       (check-result result (quote ,external-name))
       (let ((result (bdd/register-node manager result)))
         ;; [assert (result manager) (= (cudd-debug-check manager) 0)]
         result))))

(define-macro (define-bdd-op cudd-fun external-name args . svsv)
  `(define-api (,external-name ,@args)
     ,@svsv
     (let ((manager (bdd/manager ,(car args))))
       ,@(map (lambda (arg)
                `[assert (manager ,arg) (eq? (bdd/manager ,arg) manager)])
              args)
       (let ((result::obj (,cudd-fun manager ,@(map (lambda (arg) `(cdr ,arg)) args))))
         (check-result result (quote ,external-name))
         (let ((result (bdd/register-node manager result)))
           ;; [assert (result manager) (= (cudd-debug-check manager) 0)]
           result)))))
  
(define-bdd-proc cudd-read-one bdd/true (manager)
  :doc "Returns the BDD representing the constant function @code{true}.")
(define-bdd-proc cudd-new-var bdd/new-var (manager)
  :doc "Creates a new BDD variable in the given manager."
  :examples '((begin
                (define m (make-bdd-manager))
                (define n (bdd/new-var m)))))
(define-bdd-proc cudd-ith-var bdd/ith-var (manager i)
  :doc "Returns the BDD variable with index @code{i} if it already exists, or creates a new BDD variable."
  :examples '((begin
                (define m (make-bdd-manager))
                (define n1 (bdd/new-var m))
                (define n2 (bdd/ith-var m 0))
                (bdd/eq? n1 n2))))
(define-bdd-op cudd-and bdd/and (bdd1 bdd2)
  :doc "Returns the conjunction of the BDDs @code{bdd1} and @code{bdd2}.")
(define-bdd-op cudd-or bdd/or (bdd1 bdd2)
  :doc "Returns the disjunction of the BDDs @code{bdd1} and @code{bdd2}.")
(define-bdd-op cudd-xor bdd/xor (bdd1 bdd2)
  :doc "Returns the XOR of @code{bdd1} and @code{bdd2}.")
(define-bdd-op cudd-nor bdd/nor (bdd1 bdd2)
  :doc "Returns the NOR of @code{bdd1} and @code{bdd2}.")
(define-bdd-op cudd-nand bdd/nand (bdd1 bdd2)
  :doc "Returns the NAND of @code{bdd1} and @code{bdd2}.")
(define-bdd-op cudd-xnor bdd/xnor (bdd1 bdd2)
  :doc "Returns the XNOR of @code{bdd1} and @code{bdd2}.")
(define-bdd-op cudd-ite bdd/ite (bdd1 bdd2 bdd3)
  :doc "Implements @code{ITE(bdd1, bdd2, bdd3)}, where @code{ITE} stands for if-then-else.")
(define-bdd-op cudd-support bdd/support (bdd)
  :doc "Finds the variables on which @code{bdd} depends. Returns a BDD consisting of the product of the variables.")
(define-bdd-op cudd-restrict bdd/minimize (bdd-f bdd-c)
  :doc "Finds a smaller BDD that agrees with @code{bdd-f} over @code{bdd-c}.")
(define-bdd-op cudd-constrain bdd/cofactor (bdd1 bdd2)
  :doc "Computes the cofactor of @code{bdd1} with respect to @code{bdd2}.")
(define-bdd-op cudd-squeeze bdd/between (lower upper)
  :doc "Finds a smaller BDD in the function interval defined by @code{lower} (lowerbound) and @code{upper} (upperbound).") 
(define-api (bdd/and-limit bdd1 bdd2 max-size)
  :doc "Computes the conjunction of @code{bdd1} and @code{bdd2}. Returns @code{#f} if more than @code{max-size} BDD nodes are created."
  (let ((manager (bdd/manager bdd1)))
    [assert (manager bdd2) (eq? (bdd/manager bdd2) manager)]
    (let ((result::obj (cudd-and-limit manager (cdr bdd1) (cdr bdd2) max-size)))
      (if (cudd-node-null? result)
        #f
        (bdd/register-node manager result)))))

(define-api (bdd/or-limit bdd1 bdd2 max-size)
  :doc "Computes the disjunction of @code{bdd1} and @code{bdd2}. Returns @code{#f} if more than @code{max-size} BDD nodes are created."
  (let ((tmp (bdd/and-limit (bdd/not bdd1) (bdd/not bdd2) max-size)))
    (if tmp
      (bdd/not tmp)
      #f)))

(define-api (bdd/compose bdd1 bdd2 var-id)
  :doc "Substitutes @code{bdd2} for @code{var-id} in the BDD @code{bdd1}."
  (let ((manager (bdd/manager bdd1)))
    [assert (manager bdd2) (eq? (bdd/manager bdd2) manager)]
    (let ((result::obj (cudd-compose manager (cdr bdd1) (cdr bdd2) var-id)))
      (check-result result 'bdd/compose)
      (bdd/register-node manager result))))

(define-api (bdd/not bdd)
  :doc "Returns the complements of @code{bdd}."
  (let ((result::obj (cudd-not (cdr bdd))))
    (check-result result 'bdd/not)
    (bdd/register-node (car bdd) result)))

(define-api (bdd/false manager)
  :doc "Returns the BDD representing the constant function @code{false}."
  (bdd/not (bdd/true manager)))

(define-api (bdd/iff bdd1 bdd2)
  :doc "Returns the BDD representing IFF (if-and-only-if) of @code{bdd1} and @code{bdd2}."
  (bdd/xor bdd1 (bdd/not bdd2)))

(define-api (bdd/diff bdd1 bdd2)
  :doc "Returns the difference of @code{bdd1} and @code{bdd2}. The difference is @code{(bdd/and bdd1 (bdd/not bdd2))}."
  (bdd/and bdd1 (bdd/not bdd2)))

(define-api (bdd/num-vars manager)
  :doc "Returns the number of variables in the given manager."
  :examples '((begin 
                (define m (make-bdd-manager))
                (define n1 (bdd/new-var m))
                (define n2 (bdd/new-var m))
                (bdd/num-vars m)))
  (cudd-num-vars manager))

(define-api (bdd/size bdd)
  :doc "Return the number of nodes in the given BDD."
  (cudd-size (cdr bdd)))

(define-api (bdd/print-debug bdd)
  :doc "Prints a textual representation of the given BDD. This function is used for debugging purposes."
  (= (cudd-print-debug (car bdd) (cdr bdd) 1024 2) 1))

(define-api (bdd/print-minterm bdd)
  :doc "Print a minterm (i.e., an element of the set represented by the BDD) of the given BDD. This function is used for debugging purposes."
  (= (cudd-print-minterm (car bdd) (cdr bdd)) 1))

(define-api (bdd/eq? bdd1 bdd2)
  :doc "Returns @code{#t} if @code{bdd1} and @code{bdd2} represents the same boolean function (i.e., set)."
  (=CuddNode? (cdr bdd1) (cdr bdd2)))

(define-api (bdd/false? bdd)
  :doc "Returns @code{#t} if @code{bdd} is the constant function @code{false}. See @code{bdd/true?}."
  :examples '((begin
                (define m (make-bdd-manager))
                (define n1 (bdd/new-var m))
                (define n2 (bdd/and n1 (bdd/not n1)))
                (bdd/false? n2)))
  (bdd/eq? bdd (bdd/false (car bdd))))

(define-api (bdd/true? bdd)
  :doc "Returns @code{#t} if @code{bdd} is the constant function @code{true}. See @code{bdd/false?}."
  (bdd/eq? bdd (bdd/true (car bdd))))

(define-api (bdd/first-cube-vector bdd)
  :doc "Returns two values, the first value is a generator, and the second is a cube of the given BDD. The cube is returned as a Scheme vector, the size of the vector is equal to the number of variables in the BDD manager. An element of the vector is @code{true}/@code{false} if the variable appears positively/negatively in the cube, and @code{'x} if it is a don't care. The generator can be used to obtain the other cubes of the BDD. The function @code{bdd/print-debug} prints the list of cubes of a BDD. Returns @code{#f,#f} if the BDD represents the empty set. See @code{bdd/next-cube-vector!}."
  (if (bdd/false? bdd)
    (values #f #f)
    (let* ((manager (car bdd))
           (node (cdr bdd))
           (num-vars (bdd/num-vars manager))
           (generator (cudd-first-cube manager node)))
      (if (cudd-gen-null? generator)
        (values #f #f)
        (let ((cube (make-vector num-vars #f)))
          (fill-vector-with-cube cube num-vars)
          (values generator cube))))))

(define-api (bdd/next-cube-vector! generator cube)
  :doc "Return the next cube of a BDD. @code{generator} is the cube generator returned by @code{bdd/first-cube-vector}. @code{cube} is a Scheme vector which is going to filled with the next cube. See @code{bdd/first-cube-vector}. The size of the vector @code{cube} must be equal to the number of variables in the BDD manager. Returns @code{#f} if there aren't more cubes to be generated."
  (let ((num-vars (vector-length cube)))
    (let ((more? (cudd-next-cube generator)))
      (cond
       ((= more? 0)
        #f)
       (else
        (fill-vector-with-cube cube num-vars)
        cube)))))

(define-api (bdd/for-each-cube-vector proc bdd)
  :doc "Calls the procedure @code{proc} with each cube in @code{bdd}. This procedure is implemented using @code{bdd/first-cube-vector} and @code{bdd/next-cube-vector!}. @code{proc} receives a Scheme vector representing a cube of the given BDD. The elements of the vector are @code{#t} (true), @code{#f} (false), and @code{'x} (don't care)."
  :examples '((begin 
                (define m (make-bdd-manager))
                (define n1 (bdd/new-var m))
                (define n2 (bdd/new-var m))
                (bdd/for-each-cube-vector print (bdd/or n1 n2))))
  (multiple-value-bind
      (generator cube)
      (bdd/first-cube-vector bdd)
    (cond
     (generator
      (proc cube)
      (let loop ()
        (when (bdd/next-cube-vector! generator cube)
          (proc cube)
          (loop))))
     (else 
      #unspecified))))

(define (cube-vector->cube manager cube-vector)
  ;; this implementation is a little bit inefficient... 
  (let ((len (vector-length cube-vector)))
    (let loop ((i 0)
               (result (bdd/true manager)))
      (if (< i len)
        (let ((val (vector-ref cube-vector i)))
          (cond
           ((eq? val 'x) 
            (loop (+ i 1) result))
           (else
            (let ((new-result (bdd/and result 
                                       (if val
                                         (bdd/ith-var manager i)
                                         (bdd/not (bdd/ith-var manager i))))))
              (loop (+ i 1) new-result)))))
        result))))

(define-api (bdd/le? bdd1 bdd2)
  :doc "Returns @code{#t} if the set represented by @code{bdd1} is smaller than the set represented by @code{bdd2}."
  [assert (bdd1 bdd2) (eq? (car bdd1) (car bdd2))]
  (= (cudd-leq (car bdd1) (cdr bdd1) (cdr bdd2)) 1))

(define-api (indices-list->cube manager indices-list)
  :doc "Converts a list of variable indices (integers) in a BDD cube (conjunction of BDD variables). Each integer specifies a BDD variable id. See @code{indices-vector->cube}."
  :examples '((begin
                (define m (make-bdd-manager))
                (bdd/new-var m)
                (bdd/new-var m)
                (bdd/new-var m)
                (define cube1 (indices-list->cube m '(0 2)))
                (bdd/print-debug cube1)))
  (let* ((n (length indices-list))
         (indices-int-ptr (make-IntPtr n)))
    (let loop ((i 0)
               (indices-list indices-list))
      (when (< i n)
        (IntPtr-set! indices-int-ptr i (car indices-list))
        (loop (+ i 1) (cdr indices-list))))
    (let ((result::obj (cudd-indices-to-cube manager indices-int-ptr n)))
      (check-result result 'indices-list->cube)
      (bdd/register-node manager result))))

(define-api (indices-vector->cube manager indices-vector)
  :doc "Converts a vector of variable indices (integers) in a BDD cube (conjunction of BDD variables). Each integer specifies a BDD variable id. See @code{indices-list->cube}."
  (let* ((n (vector-length indices-vector))
         (indices-int-ptr (make-IntPtr n)))
    (let loop ((i 0))
      (when (< i n)
        (IntPtr-set! indices-int-ptr i (vector-ref indices-vector i))
        (loop (+ i 1))))
    (let ((result::obj (cudd-indices-to-cube manager indices-int-ptr n)))
      (check-result result 'indices-vector->cube)
      (bdd/register-node manager result))))

(define-inline (to-cube manager indices)
  (cond
   ((list? indices)
    (indices-list->cube manager indices))
   ((vector? indices)
    (indices-vector->cube manager indices))
   (else
    indices)))

(define-api (bdd/exists bdd indices)    
  :doc "Returns a new BDD where the variables in @code{indices} were existentially abstracted. @code{indices} may be a list of variable ids (integers), a vector of variable ids, or a BDD cube (i.e., a conjunction of BDD variables)."
  (let* ((manager (car bdd))
         (result::obj (cudd-exists manager (cdr bdd) (cdr (to-cube manager indices)))))
    (check-result result 'bdd/exists)
    (bdd/register-node manager result)))
  
(define-api (bdd/for-all bdd indices)
  :doc "Returns a new BDD where the variables in @code{indices} were universally abstracted. @code{indices} may be a list of variable ids (integers), a vector of variable ids, or a BDD cube (i.e., a conjunction of BDD variables)."
  (let* ((manager (car bdd))
         (result::obj (cudd-for-all manager (cdr bdd) (cdr (to-cube manager indices)))))
    (check-result result 'bdd/for-all)
    (bdd/register-node manager result)))

(define-api (bdd/and-exists bdd1 bdd2 indices)
  :doc "Takes the conjunction of @code{bdd1} and @code{bdd2} and simultaneously existentially abstract the variables in @code{indices}. @code{indices} may be a list of variable ids (integers), a vector of variable ids, or a BDD cube (i.e., a conjunction of BDD variables)."
  [assert (bdd1 bdd2) (eq? (car bdd1) (car bdd2))]
  (let* ((manager (car bdd1))
         (result::obj (cudd-and-abstract manager (cdr bdd1) (cdr bdd2) (cdr (to-cube manager indices)))))
    (check-result result 'bdd/and-exists)
    (bdd/register-node manager result)))
  
(define-api (indices-list->bdd-var-array manager indices-list)
  :doc "Converts a list of variable indices (integers) in a CUDD BDD array. Each integer specifies a BDD variable id. See @code{indices-vector->bdd-var-array}."
  (let* ((n (length indices-list))
         (bdd-array (cudd-node-array-alloc n)))
    (let loop ((i 0)
               (indices-list indices-list))
      (when (< i n)
        (let ((var (bdd/ith-var manager (car indices-list))))
          (cudd-ref (cdr var))
          (cudd-node-array-set! bdd-array i (cdr var))
          (loop (+ i 1) (cdr indices-list)))))
    bdd-array))

(define-api (indices-vector->bdd-var-array manager indices-vector)
  :doc "Converts a vector of variable indices (integers) in a CUDD BDD array. Each integer specifies a BDD variable id. See @code{indices-list->bdd-var-array}."
  (let* ((n (vector-length indices-vector))
         (bdd-array (cudd-node-array-alloc n)))
    (let loop ((i 0))
      (when (< i 0)
        (let ((var (bdd/ith-var manager (vector-ref indices-vector i))))
          (cudd-ref (cdr var))
          (cudd-node-array-set! bdd-array i (cdr var))
          (loop (+ i 1)))))
    bdd-array))

(define-api (bdd/swap-vars bdd bdd-var-array1 bdd-var-array2 n)
  :doc "Swaps the variables in the given BDD. The BDD variables in @code{bdd-var-array1} are replaced with the variables in @code{bdd-var-array2}. @code{bdd-var-array1} and @code{bdd-var-array2} must have size @code{n}. See @code{indices-list->bdd-var-array} and @code{indices-vector->bdd-var-array}."
  (let* ((manager (car bdd))
         (result::obj (cudd-swap-vars manager (cdr bdd) bdd-var-array1 bdd-var-array2 n)))
      (check-result result 'bdd/swap-vars)
      (bdd/register-node manager result)))

(define-api (bdd/swap-vars+ bdd alist)
  :doc "Swaps the variables in the given BDD. @code{alist} is an association list (i.e., list of pairs) of the variables ids which are going to be swapped."
  (let* ((manager (car bdd))
         (n (length alist))
         (array1 (indices-list->bdd-var-array manager (map car alist)))
         (array2 (indices-list->bdd-var-array manager (map cdr alist)))
         (result::obj (bdd/swap-vars bdd array1 array2 n)))
    (check-result result 'bdd/swap-vars+)
    (bdd/register-node manager result)))

(define (bdd/set-var-map! manager array1 array2 n)
  (when (= (cudd-set-var-map manager array1 array2 n) 0)
    (error 'bdd-interface "Error executing function bdd/set-var-map!." #unspecified)))
  
(define (bdd/set-var-map-with-alist! manager alist)
  (let* ((n (length alist))
         (array1 (indices-list->bdd-var-array manager (map car alist)))
         (array2 (indices-list->bdd-var-array manager (map cdr alist))))
    (when (= (cudd-set-var-map manager array1 array2 n) 0)
      (error 'bdd-interface "Error executing function bdd/set-var-map-with-alist!." #unspecified))))

(define-bdd-op cudd-var-map bdd/var-map (bdd))

(define-api (bdd/num-solutions bdd num-vars)
  :doc "Returns the number of elements in the set represented by @code{BDD}. The function is assumed to depend on @code{num-vars} variables. The result is represented as a double."
  (cudd-count-minterm (car bdd) (cdr bdd) num-vars))

(define-api (bdd/group-vars! manager first-id num-vars . fixed?)
  :doc "Groups the @code{num-vars} BDD variables starting at id @code{first-id}. Grouped variables remain together during variable reordering. If @code{fixed?} is @code{#t} then the order inside the group is maintained fixed (the default behavior is to allow the variables to be reordered inside the group."
  (let ((fixed? (optional-arg fixed? #f)))
    (if fixed?
      (cudd-group-vars-fixed manager first-id num-vars)
      (cudd-group-vars-default manager first-id num-vars))
    #unspecified))
      
(define (bdd/set-next-reordering! manager n)
  (cudd-set-next-reordering manager n)
  #unspecified)

(define (bdd/next-reordering manager)
  (cudd-read-next-reordering manager))

(define-api (bdd/show . bdds)
  :doc "Displays the BDD as a graph. You must have the program dotty installed in your system to use this procedure."
  (unless (null? bdds)
    (let* ((manager (caar bdds))
           (n (length bdds))
           (bdd-array (cudd-node-array-alloc n)))
      (let loop ((i 0)
                 (bdds bdds))
        (when (< i n)
          (cudd-node-array-set! bdd-array i (cdar bdds))
          (loop (+ i 1) (cdr bdds))))
      (let ((result (cudd-gen-dot-file manager n bdd-array *sal-dot-tmp-file*)))
        (when (= result 0)
          (error 'bdd/show "BDD package failed to generate DOT file." #unspecified))
        (dot/show)))))

(define-api (bdd/peek-one-min-term bdd vars n)
  :doc "Returns a arbitrary minterm of the given BDD. @code{vars} is a CUDD var array of size @code{n}. See @{indices-list->bdd-var-array} and @code{indices-vector->bdd-var-array}. A minterm represents an element in the set represented by the given BDD."
  (let ((result (cudd-peek-one-min-term (car bdd) (cdr bdd) vars n)))
    (check-result result 'bdd/peek-one-min-term)
    (bdd/register-node (car bdd) result)))

(define-api (bdd/peek-min-terms bdd vars num-vars num-minterms)
  :doc "Return a list of arbitrary minterms containing at most @code{num-minterms}. See @code{bdd/peek-one-min-term}."
  (let* ((total-num-minterms (bdd/num-solutions bdd num-vars))
         (total-num-minterms-as-int (inexact->exact total-num-minterms))
         (num-minterms (cond
                        ((=fl total-num-minterms 0.0)
                         0)
                        ((= total-num-minterms-as-int 0)
                         num-minterms) ;; the number of numterms doesn't fit in a bigloo integer
                        (else
                         (min num-minterms total-num-minterms-as-int))))
         (min-term-array (cudd-peek-min-terms (car bdd) (cdr bdd) vars num-vars num-minterms)))
    (when (cudd-node-array-null? min-term-array)
      (error 'bdd/peek-min-terms "Error executing function bdd/peek-min-terms." #unspecified))
    (let loop ((i 0)
               (result-list '()))
      (if (< i num-minterms)
        (let* ((min-term (cudd-node-array-ref min-term-array i))
               (registred-min-term (bdd/register-node (car bdd) min-term)))
          (loop (+ i 1) (cons registred-min-term result-list)))
        result-list))))
          
(define-api (make-gc-collector-for m)
  :doc "Returns a procedure (monitor) which may be invoked from time to time to force garbage collection of BDD nodes if necessary. Garbage collection is considered necessary when the number of new nodes is greater than a predefined threshold."
  (let ((size-after-last-gc #f)
        (threshold 50000))
    (lambda ()
      (let ((curr-size (bdd/num-nodes m)))
        (when (or (and (not size-after-last-gc) (> curr-size threshold))
                  (and size-after-last-gc (> curr-size (+ size-after-last-gc threshold))))
          (force-gc!)
          (set! size-after-last-gc (bdd/num-nodes m)))))))

(define-api (bdd/estimate-cofactor-simple bdd var-id)
  :doc "Estimate the size of the cofactor of @code{bdd} with respect to @code{var-id}. See @code{bdd/estimate-cofactor} for a more precise and expensive estimative."
  (cudd-estimate-cofactor-simple (bdd/node bdd) var-id))

(define-api (bdd/estimate-cofactor bdd var-id phase)
  :doc "Estimate the size of the cofactor of @code{bdd} with respect to @code{var-id} and @code{phase} (positive/negative). See @code{bdd/estimate-cofactor-simple} for a less precise and cheaper estimative."
  (let ((m (bdd/manager bdd)))
    (if phase
      (cudd-estimate-cofactor m (bdd/node bdd) var-id 1)
      (cudd-estimate-cofactor m (bdd/node bdd) var-id 0))))

(define (bdd/subset-compress bdd num-vars threshold)
  (let ((manager (bdd/manager bdd)))
    (let ((result::obj (cudd-subset-compress manager (cdr bdd) num-vars threshold)))
      (if (cudd-node-null? result)
        #f
        (bdd/register-node manager result)))))
