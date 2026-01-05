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

(module sal-esm-initialization
        (include "sal.sch")
        (import sal-esm-lhs sal-module sal-esm queue unique-names sal-lhs-set sal-expression
                iterators sal-type sal-esm-access-level-table sal-ast-env sal-expr-evaluator
                sal-ast-copy sal-pp) 
        (export (sal-esm-module/must-initialized-vars esm-module)
                (sal-esm-module/may-initialized-vars esm-module)
                (sal-esm-module/add-implicit-initialization esm-module))
        )

;;
;; Collect LHS that is always initialized.  The procedure computes a
;; safe approximation. That is, it may fail to include an initialized
;; variable in the result set, but it will never include a non
;; initialized one in the result set.
;;

(define (sal-esm-module/must-initialized-vars esm-module)
  (sal-esm/collect-must-initialization (slot-value esm-module :initialization) (make-empty-env) (make-sal-lhs-set)))

(define-generic (sal-esm/collect-must-initialization esm env lhs-set))

(define-method (sal-esm/collect-must-initialization :around (esm <primitive>) (env <primitive>) (lhs-set <primitive>))
  (call-next-method))

(define-method (sal-esm/collect-must-initialization (esm <primitive>) (env <primitive>) (lhs-set <primitive>))
  lhs-set)

(define-method (sal-esm/collect-must-initialization (esm <sal-esm-seq>) (env <primitive>) (lhs-set <primitive>))
  (fold-left (lambda (curr child)
               (sal-esm/collect-must-initialization child env curr))
             lhs-set
             (slot-value esm :statements)))

(define-method (sal-esm/collect-must-initialization (esm <sal-esm-choice>) (env <primitive>) (lhs-set <primitive>))
  (sal-lhs-set/union (fold-left (lambda (curr child)
                                  (let ((child-inits (sal-esm/collect-must-initialization child env (make-sal-lhs-set))))
                                    (if (eq? curr #f)
                                      child-inits
                                      (sal-lhs-set/intersection curr child-inits))))
                                #f
                                (slot-value esm :statements))
                     lhs-set))

(define-method (sal-esm/collect-must-initialization (esm <sal-esm-case>) (env <primitive>) (lhs-set <primitive>))
  (sal-lhs-set/union (fold-left (lambda (curr child)
                                  (let ((child-inits (sal-esm/collect-must-initialization (slot-value child :statement) env (make-sal-lhs-set))))
                                    (if (eq? curr #f)
                                      child-inits
                                      (sal-lhs-set/intersection curr child-inits))))
                                #f
                                (slot-value esm :case-entries))
                     lhs-set))

(define-method (sal-esm/collect-must-initialization (esm <sal-esm-assignment>) (env <primitive>) (lhs-set <primitive>))
  (let ((lhs (sal-ast/substitute (slot-value esm :lhs) env)))
    (if (sal-lhs/ground? lhs)
      (sal-lhs-set/add lhs-set lhs)
      lhs-set)))

(define-method (sal-esm/collect-must-initialization (esm <sal-esm-multi-choice>) (env <primitive>) (lhs-set <primitive>))
  (sal-esm/collect-must-initialization (slot-value esm :statement) env lhs-set))

(define (collect-core esm env lhs-set proc-body)  
  (try
   (let* ((place-provider esm)
          (local-decls (slot-value esm :local-decls))
          (body (slot-value esm :statement))
          (types (map (lambda (var-decl)
                        (ensure ((var-decl <sal-var-decl>))
                          (slot-value var-decl :type)))
                      local-decls))
          (tuple-type (make-ast-instance <sal-tuple-type> place-provider
                                         :types types))
          (proc-child (lambda (child)
                        (sal-expr/evaluate-core child env 0)))
          (iterator (sal-type/make-iterator-core tuple-type proc-child))
          (result-set lhs-set))
     (iterator/for-each 
      (lambda (tuple-literal)
        (ensure ((tuple-literal <sal-tuple-literal>))
          (let ((env (update-env* env local-decls (slot-value tuple-literal :exprs))))
            (set! result-set (proc-body body env result-set)))))
      iterator)
     result-set)
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ __)
             (sal-esm/collect-must-initialization (slot-value esm :statement) env lhs-set)))))

(define-method (sal-esm/collect-must-initialization (esm <sal-esm-multi-seq>) (env <primitive>) (lhs-set <primitive>))
  (collect-core esm env lhs-set sal-esm/collect-must-initialization))

(define (sal-esm-module/may-initialized-vars esm-module)
  (sal-esm/collect-may-initialization (slot-value esm-module :initialization) (make-empty-env) (make-sal-lhs-set)))

(define-generic (sal-esm/collect-may-initialization esm env lhs-set))

(define-method (sal-esm/collect-may-initialization (esm <primitive>) (env <primitive>) (lhs-set <primitive>))
  lhs-set)

(define-method (sal-esm/collect-may-initialization (esm <sal-esm-composition-statement>) (env <primitive>) (lhs-set <primitive>))
  (fold-left (lambda (curr child)
               (sal-esm/collect-may-initialization child env curr))
             lhs-set
             (slot-value esm :statements)))

(define-method (sal-esm/collect-may-initialization (esm <sal-esm-case>) (env <primitive>) (lhs-set <primitive>))
  (fold-left (lambda (curr entry)
               (sal-esm/collect-may-initialization (slot-value entry :statement) env curr))
             lhs-set
             (slot-value esm :statements)))

(define-method (sal-esm/collect-may-initialization (esm <sal-esm-multi-choice>) (env <primitive>) (lhs-set <primitive>))
  (sal-esm/collect-may-initialization (slot-value esm :statement) env lhs-set))

(define-method (sal-esm/collect-may-initialization (esm <sal-esm-multi-seq>) (env <primitive>) (lhs-set <primitive>))
  (collect-core esm env lhs-set sal-esm/collect-may-initialization))
  
(define-method (sal-esm/collect-may-initialization (esm <sal-esm-assignment>) (env <primitive>) (lhs-set <primitive>))
  (sal-lhs-set/add lhs-set (lhs->ground-lhs (sal-ast/substitute (slot-value esm :lhs) env))))

(define-generic (lhs->ground-lhs lhs))

(define-method (lhs->ground-lhs (lhs <primitive>))
  lhs)

(define-method (lhs->ground-lhs (lhs <sal-selection>))
  (if (sal-lhs/ground? lhs)
    lhs
    (lhs->ground-lhs (sal-selection/target lhs))))

(define *must-initialized-vars* #unspecified)
(define *may-initialized-vars* #unspecified)
(define *implicit-initializations* #unspecified)
;; 
;; Add implicit initialization (non-deterministic) assignments. 
;; Since the initialized analysis of variables is approximate,
;; the implicit assignments should only be performed when the
;; variable was not already assigned.
;;
(define (sal-esm-module/add-implicit-initialization esm-module)
  (dlet ((*implicit-initializations* (make-queue))
         (*must-initialized-vars* (sal-esm-module/must-initialized-vars esm-module))
         (*may-initialized-vars* (sal-esm-module/may-initialized-vars esm-module)))
    [assert (*must-initialized-vars* *may-initialized-vars*) (sal-lhs-set/subset? *must-initialized-vars* *may-initialized-vars*)]
    (let ((initialization-access-levels (sal-esm-module/initialization-access-levels esm-module)))
      (for-each (lambda (var-decl)
                  (unless (instance-of? var-decl <sal-input-state-var-decl>)
                    (let ((lhs (make-sal-name-expr var-decl))
                          (lvl (sal-esm-access-level-table/get-level initialization-access-levels var-decl)))
                      (add-implicit-initialization! (slot-value var-decl :type) lhs lvl))))
                (sal-module/state-variables esm-module))
      (let* ((place-provider (slot-value esm-module :initialization))
             (new-initialization (sal-esm/make-esm-seq* (cons (slot-value esm-module :initialization) (queue->list *implicit-initializations*))
                                                        place-provider)))
        (update-ast-slots esm-module 
                          :initialization new-initialization)))))

(define-generic (add-implicit-initialization! type lhs lvl))

(define-method (add-implicit-initialization! :around (type <sal-type>) (lhs <sal-expr>) (lvl <primitive>))
  (cond
   ((sal-lhs-set/contains? *must-initialized-vars* lhs)
    ;; do nothing because the lhs was fully initialized.
    #unspecified)
   ((= lvl 0) 
    ;; add implicit initialization, since the lhs was not partially initialized.
    (let* ((place-provider lhs)
           (idx (gen-unique-name 'idx))
           (idx-id (make-sal-identifier place-provider idx))
           (idx-decl (make-ast-instance <sal-var-decl> place-provider
                                        :id idx-id
                                        :type type))
           (idx-name (make-sal-name-expr idx-decl))
           (assignment (make-ast-instance <sal-esm-assignment> place-provider
                                          :lhs lhs
                                          :rhs idx-name))
           (multi-choice (make-ast-instance <sal-esm-multi-choice> place-provider
                                            :local-decls (list idx-decl)
                                            :statement assignment))
           (new-init (if (sal-lhs-set/empty? (sal-lhs-set/intersection *may-initialized-vars* (sal-lhs-set/add (make-sal-lhs-set) lhs)))
                       multi-choice
                       ;; if the lhs may be initialized, then I should add a test before performing the assignment.
                       (make-ast-instance <sal-esm-when-undefined> place-provider
                                          :lhs lhs
                                          :statement multi-choice))))
      (queue/insert! *implicit-initializations*
                     new-init)))
   (else
    ;; check the lhs components
    (call-next-method))))

(define-method (add-implicit-initialization! (type <sal-type>) (lhs <sal-expr>) (lvl <primitive>))
  [assert (type lhs lvl) (> lvl 0)]
  ;; Consider the following declaration
  ;;   t : [ bool, [bool, bool]]
  ;; and the following initialization
  ;;   t.2.1 = true
  ;; Then the access level of t is 2, but the first argument of t does not have depth 2. 
  ;; So, this method is simply a hack to handle cases like the first argument of t.
  (add-implicit-initialization! type lhs (- lvl 1)))

(define-method (add-implicit-initialization! (type <sal-type-name>) (lhs <sal-expr>) (lvl <primitive>))
  (add-implicit-initialization! (sal-type-name/definition type) lhs lvl))

(define-method (add-implicit-initialization! (type <sal-subtype>) (lhs <sal-expr>) (lvl <primitive>))
  (if (or (sal-type/tuple? type) (sal-type/record? type) (sal-type/function? type))
    (sign-unsupported-feature lhs "Partial initialization of state variables that are (or contain) subtypes is not supported by this tool. Solution: provide an explicit initialization for the state variable '~a'." (sal-name-ref/name (sal-lhs/name-expr lhs)))
    ;; Read the comment in the method specialized for <sal-type>
    (add-implicit-initialization! type lhs (- lvl 1))))

(define-method (add-implicit-initialization! (type <sal-tuple-type>) (lhs <sal-expr>) (lvl <primitive>))
  (let loop ((idx 1)
             (types (slot-value type :types)))
    (unless (null? types)
      (let ((place-provider lhs))
        (add-implicit-initialization! (car types) 
                                      (make-ast-instance <sal-tuple-selection> place-provider
                                                         :target lhs
                                                         :idx (make-sal-numeral idx place-provider))
                                      (- lvl 1))
        (loop (+ idx 1) (cdr types))))))
  
(define-method (add-implicit-initialization! (type <sal-record-type>) (lhs <sal-expr>) (lvl <primitive>))
  (let loop ((fields (slot-value type :fields)))
    (unless (null? fields)
      (let ((place-provider lhs))
        (add-implicit-initialization! (slot-value (car fields) :type)
                                      (make-ast-instance <sal-record-selection> place-provider
                                                         :target lhs
                                                         :idx (slot-value (car fields) :id))
                                      (- lvl 1))
        (loop (cdr fields))))))

(define-method (add-implicit-initialization! (type <sal-array-type>) (lhs <sal-expr>) (lvl <primitive>))
  (try
   (let* ((domain-type (slot-value type :domain))
          (range-type (slot-value type :range))
          (place-provider lhs)
          (it (sal-type/make-iterator domain-type)))
     (iterator/for-each (lambda (domain-value)
                          (add-implicit-initialization! range-type
                                                        (make-ast-instance <sal-array-selection> place-provider
                                                                           :fun lhs
                                                                           :arg domain-value)
                                                        (- lvl 1)))
                        it))
   (catch* '(type-iterator expr-evaluator)
           (lambda (_ msg)
             (sign-unsupported-feature "Failed to produce implicit initialization for state variable '~a'. Reason: ~a"
                                       (sal-name-ref/name (sal-lhs/name-expr lhs))
                                       msg)))))
     



;;
;; Automatic lifting
;; -----------------
;; 
;; Lift variables (LHS) that were not initialized. 
;; The lifted variables are initialized with the "undefined"
;; value instead of a non-deterministic one. A runtime error
;; will occur if the "undefined" value is used. The error can
;; be tracked using the -g option of the explicit state model checker.
;; The automatic lifting produces an overhead of one bit per lifting.
;;
;; Possible option:
;; * the user provides a list of variables that are not going to be lifted,
;;   even when they are not initialized.
;; I didn't implement this option because it can be simulated by explicit
;; non-deterministic assignments.
;; 
;;
;; Automatic lifting is trivial for variables
;;
;; 1) every occurrence of the lifted variable "x" is
;;    substituted by "down(x)"
;; 2) every assignment of "x' = e" is substituted by
;;    "x' = up(e)"
;; 3) the assignment "x = undef" is included in the 
;;    initialization section.
;;
;; The problem is more complicated when parts of a variable (i.e.,
;; lhs) are lifted. Example, suppose "x.1" is lifted, and the specification
;; contains the assignment "x'.1.2 = e" 
;;
;; The trick here is to lift all components of x.1 instead of lifting it
;; as a whole. The access level table can help here.
;;

