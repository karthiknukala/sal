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

(module sal-ltl
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-expression sal-type sal-nnf bdd sal-ast-table sal-ast-for-each 
                sal-dnf sal-ast-expand sal-ast-env sal-ast-simplify sal-ast-eq
                sal-pp dot-interface sal-ast-list pretty symbol-table symbol-set
                iterators unique-names sal-cse runtime front-end sal-finite-expressions
                sal-esm sal-environment)
        (export (sal-ltl/enable-expensive-ba-optimizations! flag)
                (sal-ltl-expr->primitive-ltl-expr expr)
                (ltl->vwaa formula)
                (vwaa->dot agraph)
                (vwaa/display agraph)
                (vwaa->gba agraph)
                (gba->dot agraph)
                (gba/display agraph)
                (gba/simplify! agraph)
                (gba->ba agraph)
                (ba->dot agraph)
                (ba/display agraph)
                (ba/simplify! agraph)
                (ltl->ba expr)
                (ba/accepts-everything? graph)
                (ba/empty? graph)
                (ba->monitor graph place-provider esm?)
                (ba->bool-monitor graph place-provider)
                (ltl->monitor expr . svsv)
                (ltl->dot expr))
        )

;; TODO: rewrite this module using the graph module.
;; GBA and BA can be reprented using the graph data structure defined at graph.scm
(define *sal-ba-expensive-optimizations* #t)

(define (sal-ltl/enable-expensive-ba-optimizations! flag)
  (set! *sal-ba-expensive-optimizations* flag))

(front-end/add-full-option!
 "LTL Support" 
 "-dbo"
 "--disable-expensive-ba-opt"
 "Disable expensive BA (buchi automata) optimizations. Some optimizations may be very expensive for big LTL properties."
 (lambda ()
   (sal-ltl/enable-expensive-ba-optimizations! #f)))

(define (sal-expr/contains-ltl-operator? expr)
  (sal-ast/find (lambda (n)
                  (instance-of? n <sal-ltl-application>))
                expr))

(define (to-process-ltl? expr)
  (or (instance-of? expr <sal-or>)
      (instance-of? expr <sal-and>)
      (instance-of? expr <sal-not>)
      (sal-expr/contains-ltl-operator? expr)))

(define (sal-ltl-expr->nnf expr)
  (sal-expr->nnf expr to-process-ltl?))

(define (sal-ltl-expr->dnf expr)
  (sal-expr->dnf expr to-process-ltl?))

(define (sal-ltl-expr->primitive-ltl-expr expr)
  (let* ((expanded-expr (sal-ast/simplify (sal-ast/expand-quantifiers (sal-ast/expand-names expr (make-empty-env)))))
         (expanded-expr2 (sal-ast/expand-applications expanded-expr
                                                      (make-empty-env) 
                                                      (lambda (app)
                                                        (instance-of? app <sal-ltl-application>)))))
    (ltl/simplify (sal-ltl-expr->nnf expanded-expr2))))

;; Now, sal-ltl-expr/collect-atoms returns a new expressions where the leaves are bdd variables (atoms) or <sal-true> or <sal-false>
(define (sal-ltl-expr/collect-atoms expr)
  (let* ((m (make-bdd-manager))
         (t (make-sal-ast-table))
         (i (make-eq-hash-table))
         (e (make-sal-ast-table))
         (new-expr (sal-ltl-expr/collect-atoms-core expr m t i e)))
    (values new-expr m t i)))

(define-generic (sal-ltl-expr/collect-atoms-core expr m t i e))

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-true>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  expr)

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-false>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  expr)

(define (make-fresh-bool-var place-provider)
  (let* ((new-name (gen-unique-name 'atom))
         (new-id (make-sal-identifier place-provider new-name))
         (bool-type-name (make-sal-builtin-name <sal-bool-type> place-provider))
         (new-decl (make-ast-instance <sal-var-decl> place-provider
                                      :id new-id
                                      :type bool-type-name))
         (new-name-expr (make-sal-name-expr new-decl place-provider)))
    new-name-expr))
  
(define (collect-atom expr m t i e)
  (cond
   ((sal-ast-table/get e expr) => cdr) ;; return the fresh variable representing the atom
   (else
    (let* ((bdd-var (bdd/new-var m))
           (atom (make-fresh-bool-var expr)))
      (sal-ast-table/put! t atom bdd-var)
      (eq-hash-table/put! i (bdd/var bdd-var) expr)
      (sal-ast-table/put! e expr atom)
      atom))))

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-expr>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  (collect-atom expr m t i e))

(define (sal-application/collect-atoms app m t i e)
  (copy-ast app
            :arg (apply make-application-argument
                        (map (lambda (arg)
                               (sal-ltl-expr/collect-atoms-core arg m t i e))
                             (sal-application/argument-list app)))))

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-ltl-application>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  (sal-application/collect-atoms expr m t i e))

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-and>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  (if (sal-expr/contains-ltl-operator? expr)
    (sal-application/collect-atoms expr m t i e)
    (collect-atom expr m t i e)))

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-or>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  (if (sal-expr/contains-ltl-operator? expr)
    (sal-application/collect-atoms expr m t i e)
    (collect-atom expr m t i e)))

(define-method (sal-ltl-expr/collect-atoms-core (expr <sal-not>) (m <primitive>) (t <primitive>) (i <primitive>) (e <primitive>))
  ;; expression must be in nnf
  [assert (expr) (not (sal-expr/contains-ltl-operator? expr))]
  (let ((arg (slot-value expr :arg)))
    (cond
     ((sal-ast-table/get e expr) => cdr)
     ((sal-ast-table/get e arg) =>
      (lambda (entry)
        (let* ((atom (cdr entry))
               (bdd-var (cdr (sal-ast-table/get t atom)))
               (not-bdd-var (bdd/not bdd-var))
               (not-atom (copy-ast expr :arg atom)))
          (sal-ast-table/put! e expr not-atom)
          (sal-ast-table/put! t not-atom not-bdd-var)
          not-atom)))
     (else
      (let* ((bdd-var (bdd/new-var m))
             (not-bdd-var (bdd/not bdd-var))
             (atom (make-fresh-bool-var expr))
             (not-atom (copy-ast expr :arg atom)))
        (sal-ast-table/put! t atom bdd-var)
        (sal-ast-table/put! t not-atom not-bdd-var)
        (eq-hash-table/put! i (bdd/var bdd-var) arg)
        (sal-ast-table/put! e arg atom)
        (sal-ast-table/put! e expr not-atom)
        not-atom)))))

;; Checking for "p <= q" is hard in general. Hence, I just look for simple
;; cases that can be detected by purely syntatic means. 
;;
;; Note: "(ltl/le? phi psi) returns true" if "p implies q"
(define-generic (ltl/le? p q))

(define-method (ltl/le? (p <sal-expr>) (q <sal-expr>))
  ;; conservative result
  #f)

;; false implies `q'
(define-method (ltl/le? (p <sal-false>) (q <sal-expr>))
  #t)

;; `p' implies true
(define-method (ltl/le? :around (p <sal-expr>) (q <sal-true>))
  #t)

;; `p' implies `p'
(define-method (ltl/le? :around (p <sal-expr>) (q <sal-expr>))
  (sal-ast/equivalent? p q))

;; `p' implies (and q_1 ... q_n) if for-all q_i `p' implies `q_i'
(define-method (ltl/le? (p <sal-expr>) (q <sal-and>))
  (for-all (cut ltl/le? p <>) (sal-application/argument-list q)))

;; `p' implies (or q_1 ... q_n) if exists q_i `p' implies `q_i'
(define-method (ltl/le? (p <sal-expr>) (q <sal-and>))
  (exists (cut ltl/le? p <>) (sal-application/argument-list q)))

;; (or p_1 ... p_n) implies q if for-all p_i `p_i' implies `q'
(define-method (ltl/le? (p <sal-or>) (q <sal-expr>))
  (for-all (cut ltl/le? <> q) (sal-application/argument-list p)))

;; `p' implies `(U q-1 q-2)'  if `p' implies `q-2'
(define-method (ltl/le? (p <sal-expr>) (q <sal-ltl-u>))
  (ltl/le? p (sal-binary-application/arg2 q)))

;; `p' implies `(W q-1 q-2)'  if `p' implies `q-2'
(define-method (ltl/le? (p <sal-expr>) (q <sal-ltl-w>))
  (ltl/le? p (sal-binary-application/arg2 q)))

;; `(R p-1 p-2)' implies `q' if `p-2' implies `q'
(define-method (ltl/le? (p <sal-ltl-r>) (q <sal-expr>))
  (ltl/le? (sal-binary-application/arg2 p) q))

;; `(M p-1 p-2)' implies `q' if `p-2' implies `q'
(define-method (ltl/le? (p <sal-ltl-m>) (q <sal-expr>))
  (ltl/le? (sal-binary-application/arg2 p) q))

;; `(U p-1 p-2)' implies `q' if `p-1' implies `q' and `p-2' implies `q'
(define-method (ltl/le? (p <sal-ltl-u>) (q <sal-expr>))
  (for-all (cut ltl/le? <> q) (sal-application/argument-list p)))

;; `(W p-1 p-2)' implies `q' if `p-1' implies `q' and `p-2' implies `q'
(define-method (ltl/le? (p <sal-ltl-w>) (q <sal-expr>))
  (for-all (cut ltl/le? <> q) (sal-application/argument-list p)))

;; `p' implies `(R q-1 q-2)'  if `p' implies `q-1' and `p' implies `q-2'
(define-method (ltl/le? (p <sal-expr>) (q <sal-ltl-r>))
  (for-all (cut ltl/le? p <>) (sal-application/argument-list q)))

;; `p' implies `(M q-1 q-2)'  if `p' implies `q-1' and `p' implies `q-2'
(define-method (ltl/le? (p <sal-expr>) (q <sal-ltl-m>))
  (for-all (cut ltl/le? p <>) (sal-application/argument-list q)))
  
;; `(U p-1 p-2)' implies `(U q-1 q-2)' if `p-1' implies `q-1' and `p-2' implies `q-2'
(define-method (ltl/le? (p <sal-ltl-u>) (q <sal-ltl-u>))
  (for-all (lambda (p-i q-i) (ltl/le? p-i q-i)) (sal-application/argument-list p) (sal-application/argument-list q)))

;; `(W p-1 p-2)' implies `(W q-1 q-2)' if `p-1' implies `q-1' and `p-2' implies `q-2'
(define-method (ltl/le? (p <sal-ltl-w>) (q <sal-ltl-w>))
  (for-all (lambda (p-i q-i) (ltl/le? p-i q-i)) (sal-application/argument-list p) (sal-application/argument-list q)))

;; `(R p-1 p-2)' implies `(R q-1 q-2)' if `p-1' implies `q-1' and `p-2' implies `q-2'
(define-method (ltl/le? (p <sal-ltl-r>) (q <sal-ltl-r>))
  (for-all (lambda (p-i q-i) (ltl/le? p-i q-i)) (sal-application/argument-list p) (sal-application/argument-list q)))

;; `(M p-1 p-2)' implies `(M q-1 q-2)' if `p-1' implies `q-1' and `p-2' implies `q-2'
(define-method (ltl/le? (p <sal-ltl-m>) (q <sal-ltl-m>))
  (for-all (lambda (p-i q-i) (ltl/le? p-i q-i)) (sal-application/argument-list p) (sal-application/argument-list q)))

(define (ltl/simplify expr)
  (ltl/simplify-core expr (make-empty-env)))

(define-generic (ltl/simplify-core expr env))

(define-method (ltl/simplify-core (expr <sal-ast>) (env <primitive>))
  (sal-ast/local-simplify-core expr env ltl/simplify-core))

(define (g? expr)
  ;; (R false p)
  (and (instance-of? expr <sal-ltl-r>)
       (sal-expr/false? (sal-binary-application/arg1 expr))))

(define (g-arg expr)
  (sal-binary-application/arg2 expr))

(define (f? expr)
  ;; (U true p)
  (and (instance-of? expr <sal-ltl-u>)
       (sal-expr/true? (sal-binary-application/arg1 expr))))

(define (f-arg expr)
  (sal-binary-application/arg2 expr))

(define (gf? expr)
  ;; (R false (U true p))
  (and (g? expr) (f? (g-arg expr))))

(define (gf-arg expr)
  (f-arg (g-arg expr)))

(define (fg? expr)
  ;; (U true (R false p))
  (and (f? expr) (g? (f-arg expr))))

(define (fg-arg expr)
  (g-arg (f-arg expr)))

(define (gg? expr)
  ;; (R false (R false p))
  (and (g? expr) (g? (g-arg expr))))

(define (gg-arg expr)
  (g-arg (g-arg expr)))

(define (ff? expr)
  ;; (U true (U true p))
  (and (f? expr) (f? (f-arg expr))))

(define (ff-arg expr)
  (f-arg (f-arg expr)))

(define-method (ltl/simplify-core (expr <sal-ltl-x>) (env <primitive>))
  (let ((arg (ltl/simplify-core (slot-value expr :arg) env)))
    (cond
     ;; (X true) = true
     ;; (X false) = false
     ;; (X (G (F p))) = (G (F p))
     ;; (X (F (G p))) = (F (G p))
     ((or (sal-expr/true? arg)
          (sal-expr/false? arg)
          (gf? arg)
          (fg? arg))
      arg)
     (else
      (make-sal-builtin-application <sal-ltl-x> expr
                                    arg)))))

(define-method (ltl/simplify-core (expr <sal-ltl-u>) (env <primitive>))
  (cond
   ((ff? expr) ;; (F (F m)) = (F m)
    (f-arg expr))
   (else
    (multiple-value-bind
        (arg1 arg2)
        (sal-binary-application/arguments expr)
      (let ((p (ltl/simplify-core arg1 env))
            (q (ltl/simplify-core arg2 env)))
        (cond
         ((or (sal-expr/true? q) ;; (U p true) = true
              (sal-expr/false? q) ;; (U p false) = false
              (ltl/le? p q)  ;; (U p q) = q if `p' implies `q'
              (gf? q) ;; (U p (G F m)) = (G (F m))
              (fg? q) ;; (U p (F G m)) = (F (G m))
              (and (instance-of? q <sal-ltl-u>) ;; (U p (U q-1 q-2)) = (U q-1 q-2) if `p' implies `q-1'
                   (ltl/le? p (sal-binary-application/arg1 q))))
          q)
         ((and (instance-of? p <sal-ltl-x>) ;; (U (X p) (X q)) = (X (U p q))
               (instance-of? q <sal-ltl-x>))
          (make-sal-builtin-application <sal-ltl-x> expr
                                        (make-sal-builtin-application <sal-ltl-u> expr
                                                                      (slot-value p :arg)
                                                                      (slot-value q :arg))))
         ((ltl/le? (make-sal-not+ q) p)  ;; (U p q) = (U true q) if (not q) implies p
          (make-sal-builtin-application <sal-ltl-u> expr
                                        (make-sal-true expr)
                                        q))
         (else
          (make-sal-builtin-application <sal-ltl-u> expr
                                        p q))))))))

(define-method (ltl/simplify-core (expr <sal-ltl-r>) (env <primitive>))
  (cond
   ((gg? expr) ;; (G (G m)) = (G m)
    (g-arg expr))
   (else
    (multiple-value-bind
        (arg1 arg2)
        (sal-binary-application/arguments expr)
      (let ((p (ltl/simplify-core arg1 env))
            (q (ltl/simplify-core arg2 env)))
        (cond
         ((or (sal-expr/true? q) ;; (R p true) = true
              (sal-expr/false? q) ;; (R p false) = false
              (gf? q) ;; (R p (G F m)) = (G (F m))
              (fg? q) ;; (R p (F G m)) = (F (G m))
              (and (instance-of? q <sal-ltl-r>) ;; (R p (R q-1 q-2)) = (R q-1 q-2) if `q-1' implies `p'
                   (ltl/le? (sal-binary-application/arg1 q) p)))
          q)
         ((and (instance-of? p <sal-ltl-x>) ;; (R (X p) (X q)) = (X (R p q))
               (instance-of? q <sal-ltl-x>))
          (make-sal-builtin-application <sal-ltl-x> expr
                                        (make-sal-builtin-application <sal-ltl-r> expr
                                                                      (slot-value p :arg)
                                                                      (slot-value q :arg))))
         ((ltl/le? p q)  ;; (R p q) = p if `q' implies `p'
          p)
         ((ltl/le? p (make-sal-not+ q)) ;; (R p q) = (R false q) if p implies (not q) 
          (make-sal-builtin-application <sal-ltl-r> expr
                                        (make-sal-false expr)
                                        q))
         (else
          (make-sal-builtin-application <sal-ltl-r> expr
                                        p q))))))))

(define-method (ltl/simplify-core (expr <sal-ltl-w>) (env <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (let ((p (ltl/simplify-core arg1 env))
          (q (ltl/simplify-core arg2 env)))
      (cond
       ((or (sal-expr/true? q) ;; (W p true) = true
            (sal-expr/false? q) ;; (W p false) = false
            (ltl/le? p q)  ;; (W p q) = q if `p' implies `q'
            (and (instance-of? q <sal-ltl-w>) ;; (W p (W q-1 q-2)) = (W q-1 q-2) if `p' implies `q-1'
                 (ltl/le? p (sal-binary-application/arg1 q))))
        q)
       ((ltl/le? (make-sal-not+ q) p)  ;; (W p q) = (W true q) if (not q) implies p
        (make-sal-builtin-application <sal-ltl-w> expr
                                      (make-sal-true expr)
                                      q))
       (else
        (make-sal-builtin-application <sal-ltl-w> expr
                                      p q))))))

(define-method (ltl/simplify-core (expr <sal-ltl-m>) (env <primitive>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (let ((p (ltl/simplify-core arg1 env))
          (q (ltl/simplify-core arg2 env)))
      (cond
       ((or (sal-expr/true? q) ;; (M p true) = true
            (sal-expr/false? q) ;; (M p false) = false
            (and (instance-of? q <sal-ltl-m>) ;; (M p (M q-1 q-2)) = (M q-1 q-2) if `q-1' implies `p'
                 (ltl/le? (sal-binary-application/arg1 q) p)))
        q)
       ((ltl/le? p q)  ;; (M p q) = p if `q' implies `p'
        p)
       ((ltl/le? p (make-sal-not+ q)) ;; (M p q) = (M false q) if p implies (not q) 
        (make-sal-builtin-application <sal-ltl-m> expr
                                      (make-sal-false expr)
                                      q))
       (else
        (make-sal-builtin-application <sal-ltl-m> expr
                                      p q))))))

(define (group-ltl-x app mk-app)
  (let ((expr-list (sal-application/argument-list app))
        (place-provider app))
    (multiple-value-bind
        (x-expr-list rest-expr-list)
        (split-list expr-list (cut instance-of? <> <sal-ltl-x>))
      (if (or (null? x-expr-list)
              (null? (cdr x-expr-list)))
        app
        (apply mk-app
               (make-sal-builtin-application <sal-ltl-x> place-provider
                                             (apply mk-app
                                                    (map (cut slot-value <> :arg) x-expr-list)))
               rest-expr-list)))))

(define (group-ltl-gf app-or)
  (let ((expr-list (sal-application/argument-list app-or))
        (place-provider app-or))
    (multiple-value-bind
        (gf-expr-list rest-expr-list)
        (split-list expr-list gf?)
      (if (or (null? gf-expr-list)
              (null? (cdr gf-expr-list)))
        app-or
        (apply make-sal-or+
               (make-sal-builtin-application <sal-ltl-r> place-provider
                                             (make-sal-false place-provider)
                                             (make-sal-builtin-application <sal-ltl-u> place-provider
                                                                           (make-sal-true place-provider)
                                                                           (apply make-sal-or+
                                                                                  (map gf-arg gf-expr-list))))
               rest-expr-list)))))

(define (group-ltl-fg app-and)
  (let ((expr-list (sal-application/argument-list app-and))
        (place-provider app-and))
    (multiple-value-bind
        (fg-expr-list rest-expr-list)
        (split-list expr-list fg?)
      (if (or (null? fg-expr-list)
              (null? (cdr fg-expr-list)))
        app-and
        (apply make-sal-and+
               (make-sal-builtin-application <sal-ltl-u> place-provider
                                             (make-sal-true place-provider)
                                             (make-sal-builtin-application <sal-ltl-r> place-provider
                                                                           (make-sal-false place-provider)
                                                                           (apply make-sal-and+
                                                                                  (map fg-arg fg-expr-list))))
               rest-expr-list)))))
    
(define-method (ltl/simplify-core (expr <sal-or>) (env <primitive>))
  (let* ((tmp1 (call-next-method))
         (tmp2 (group-ltl-gf tmp1))
         (tmp3 (group-ltl-x tmp2 make-sal-or+)))
    tmp3))
    
(define-method (ltl/simplify-core (expr <sal-and>) (env <primitive>))
  (let* ((tmp1 (call-next-method))
         (tmp2 (group-ltl-fg tmp1))
         (tmp3 (group-ltl-x tmp2 make-sal-and+)))
    tmp3))

;; Some simplifications that I didn't implement                    
;; (phi R psi) AND (phi R gamma) = phi R (psi AND gamma)           
;; (phi M psi) AND (phi M gamma) = phi M (psi AND gamma)           
;; (phi U psi) OR (phi U gamma) = phi U (psi OR gamma)             
;; (phi W psi) OR (phi W gamma) = phi W (psi OR gamma)              
;; (phi R psi) OR (gamma R psi) = (phi OR gamma) R psi           
;; (phi U psi) AND (gamma U psi) = (phi AND gamma) U psi           
;; (phi M psi) OR (gamma M psi) = (phi OR gamma) M psi             
;; (phi W psi) AND (gamma W psi) = (phi AND gamma) W psi 

;;-------------------------------------------------------------------------
;;
;; LTL -> Very Weak Alternating Automaton (VWAA)
;;
;;-------------------------------------------------------------------------


;; Pre VWAA transition is used when we are converting a LTL formula to a VWAA
;;   :label is a BDD
;;   :target-list is a list of LTL formulas
;;
;; VWAA transition
;;  :label is a BDD
;;  :target-set is a set of symbols
;;
(define-class <pre-vwaa-transition> () (:label :target-list))
(define-class <vwaa-transition> () (:label :target-set))

(define-class <ltl-context> () (:manager :atom-table :bdd-var-idx->atom :pre-vwaa-transition-list-true :place-provider))

(define (make-pre-vwaa-transition-true context)
  (make-instance <pre-vwaa-transition>
                 :label (bdd/true (slot-value context :manager))
                 :target-list '()))

(define (make-pre-vwaa-transition-list-true context)
  (list (make-pre-vwaa-transition-true context)))

(define (make-ltl-context formula)
  (multiple-value-bind 
      (new-formula manager atom-table inv)
      (sal-ltl-expr/collect-atoms formula)
    (let ((context (make-instance <ltl-context> 
                                  :manager manager
                                  :atom-table atom-table
                                  :bdd-var-idx->atom inv
                                  :place-provider formula)))
      (set-slot-value! context :pre-vwaa-transition-list-true (make-pre-vwaa-transition-list-true context))
      (values new-formula context))))

(define (bdd->sal-expr bdd context)
  (let* ((m (slot-value context :manager))
         (bdd-var-idx->atom (slot-value context :bdd-var-idx->atom))
         (place-provider (slot-value context :place-provider))
         (result (make-sal-false place-provider)))
    (bdd/for-each-cube-vector (lambda (cube-vector)
                                (let ((len (vector-length cube-vector)))
                                  (let loop ((i 0)
                                             (sal-cube (make-sal-true place-provider)))
                                    (cond 
                                     ((< i len)
                                      (let* ((var-idx-value (vector-ref cube-vector i))
                                             (sal-atom (cond
                                                        ((eq-hash-table/get bdd-var-idx->atom i) => cdr)
                                                        (else (internal-error)))))
                                        (case var-idx-value
                                          ((#t) (loop (+ i 1) (make-sal-and+ sal-cube sal-atom)))
                                          ((#f) (loop (+ i 1) (make-sal-and+ sal-cube (make-sal-not+ sal-atom))))
                                          (else (loop (+ i 1) sal-cube))))) 
                                     (else
                                      (set! result (make-sal-or+ result sal-cube)))))))
                              bdd)
    result))

(define (pre-vwaa-transition->doc trans context)
  (let* ((label (bdd->sal-expr (slot-value trans :label) context))
         (target-list (slot-value trans :target-list)))
    (pp/list-style1 'transition (sal->doc label) (apply pp/list-style6 (map sal->doc target-list)))))

(define (pre-vwaa-transition/pp trans context)
  (sal/pretty (pre-vwaa-transition->doc trans context)))

(define (pre-vwaa-transition-list->doc trans-list context)
  (apply pp/list-style6 (map (cut pre-vwaa-transition->doc <> context) trans-list)))

(define (pre-vwaa-transition-list/pp trans-list context)
  (sal/pretty (pre-vwaa-transition-list->doc trans-list context)))
 
;; return ture if target-list1 is more generic than target-list2
(define (ltl-target-list/le? target-list1 target-list2)
  (for-all (lambda (t2) (exists (lambda (t1) (ltl/le? t1 t2)) target-list1)) target-list2))

;; A target (formula) is only inserted in a list if it is not redundant.
;; A formula "f" is redundant, if there is a formula "g" in the list such that "g <= f", i.e., "g IMPLIES f".
;; If this not the case, then "f" is inserted in the list, and all formula "g" "f <= g" will be removed.
(define (target-list/insert target-list f)
  (let ((not-f (make-sal-not+ f)))
    (cond
     ((exists (cut ltl/le? <> not-f) target-list)
      (list (make-sal-false f)))
     ((exists (cut ltl/le? <> f) target-list)
      target-list)
     (else
      (cons f (remove-if (cut ltl/le? f <>) target-list))))))

(define (target-list/union target-list1 target-list2)
  (fold-left target-list/insert
             target-list1
             target-list2))

(define (accepting-formula? f)
  (or (instance-of? f <sal-ltl-u>)
      (instance-of? f <sal-ltl-m>)))

;; A formula is only inserted in a list if it is not redundant or 
;; if its affects the accepting condition (i.e., U-formula).
(define (target-list/fair-insert targ-list f)
  (if (not (accepting-formula? f))
    (target-list/insert targ-list f)
    (cons f (remove-if (lambda (g)
                         (or (sal-ast/equivalent? f g)
                             (and (ltl/le? f g)
                                  (not (accepting-formula? g)))))
                       targ-list))))

;; Make a fair-union of two target lists.
;; Some redundant formulas will be removed. Redundant formulas that
;; affect accepting conditions (i.e., U-formulas) will not be removed. 
;; For instance (a) union (b U a) = (a, b U a)
(define (target-list/fair-union target-list1 target-list2)
  (with-output-to-trace 'ltl
                        (trace 'ltl "fair-union of:")
                        (sal/pp target-list1)
                        (print "")
                        (sal/pp target-list2)
                        (print ""))
  (let ((result (fold-left target-list/fair-insert
                           target-list1
                           target-list2)))
    (with-output-to-trace 'ltl
                          (trace 'ltl "result:")
                          (sal/pp result) 
                          (print "")
                          (trace 'ltl "--------------------"))
    result))

;;
;; t1 <= t2 means "t1 IS MORE GENERIC THAN t2", "t1 IMPLIES t2"
;;
;; I use the concept of being "MORE GENERIC THAN" to remove redundant 
;; transitions. I mean if t1 <= t2, then t2 can be removed.
;;
;; t1 <= t2 if
;;   :label t2 <= :label t1   AND
;;   :target-list t2 <= :target-list t1   AND
;;   ((source is U-state) IMPLIES (target t2 does not contain source IMPLIES target t1 does not contain source))
;;
;; The last condition is really trick... the idea is that 
;; if source is a U-state, then (we can not compate t1 and t2, if t2 is NOT a self loop and t1 is a self loop)
;; By not compare we mean that we return #f
;; 
;; If we do not use the last condition, then we can stop to accept some traces
(define (pre-vwaa-transition/le? trans1 trans2 source context)
  (and (bdd/le? (slot-value trans2 :label) (slot-value trans1 :label))
       (ltl-target-list/le? (slot-value trans2 :target-list) (slot-value trans1 :target-list))
       (imply (accepting-formula? source)
              (imply (not (exists (lambda (target) (sal-ast/equivalent? target source)) (slot-value trans2 :target-list)))
                     (not (exists (lambda (target) (sal-ast/equivalent? target source)) (slot-value trans1 :target-list)))))))


;;
;; A transition is only inserted in a list if it is not redundant.
;; A transition "t1" is redundant, if there is a transition "t2" in the list such that "t2 <= t1", i.e., "t2 IS MORE GENERIC THAN t1".
;; If this not the case, then "t1" is inserted in the list, and all transition "t2" "t1 <= t2" will be removed.
;;
;; "t1" is not inserted if the set of atoms (i.e., labels) contains FALSE.
(define (pre-vwaa-transition-list/insert vwaa-transition-list t1 source context)
  (with-output-to-trace 'ltl
                        (trace 'ltl "vwaa-transition-list/insert:")
                        (pre-vwaa-transition-list/pp vwaa-transition-list context)
                        (print "")
                        (pre-vwaa-transition/pp t1 context)
                        (print ""))
  (let ((result (if (or (bdd/false? (slot-value t1 :label))
                        (exists (lambda (t2) (pre-vwaa-transition/le? t2 t1 source context)) vwaa-transition-list))
                  vwaa-transition-list
                  (cons t1 (remove-if (lambda (t2) (pre-vwaa-transition/le? t1 t2 source context)) vwaa-transition-list)))))
    (with-output-to-trace 'ltl
                          (trace 'ltl "result:")
                          (pre-vwaa-transition-list/pp result context)
                          (print "")
                          (trace 'ltl "----------------"))
    result))

;;
;; Make the union of two vwaa-transition-lists.
;; All redundant transitions will be removed.
;; This function uses the function vwaa-transition-list/insert.
;;
(define (pre-vwaa-transition-list/union vwaa-trans-list1 vwaa-trans-list2 source context)
  (cond 
   ((null? vwaa-trans-list1) vwaa-trans-list2)
   ((null? vwaa-trans-list2) vwaa-trans-list1)
   (else
    (fold-left (lambda (result curr)
                 (pre-vwaa-transition-list/insert result curr source context))
               vwaa-trans-list1
               vwaa-trans-list2))))


(define (pre-vwaa-transition/merge trans1 trans2 context)
  (make-instance <pre-vwaa-transition>
                 :label (bdd/and (slot-value trans1 :label) (slot-value trans2 :label))
                 :target-list (target-list/fair-union (slot-value trans1 :target-list) (slot-value trans2 :target-list))))
    
;; Make the product of two vwaa-transition-lists.
;; All redundant transitions are ignored.
(define (pre-vwaa-transition-list/product vwaa-trans-list1 vwaa-trans-list2 source context) 
  (cond 
   ((eq? vwaa-trans-list1 (slot-value context :pre-vwaa-transition-list-true))
    vwaa-trans-list2)
   ((eq? vwaa-trans-list2 (slot-value context :pre-vwaa-transition-list-true))
    vwaa-trans-list1)
   (else
    (let ((result-list '()))
      (for-each (lambda (t1) 
                  (for-each (lambda (t2) 
                              (set! result-list (pre-vwaa-transition-list/insert result-list
                                                                                 (pre-vwaa-transition/merge t1 t2 context)
                                                                                 source
                                                                                 context)))
                            vwaa-trans-list2)) 
                vwaa-trans-list1)
      result-list))))

(define-generic (ltl->pre-vwaa-transition-list expr source context))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-true>) (source <sal-expr>) (context <ltl-context>))
  (slot-value context :pre-vwaa-transition-list-true))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-false>) (source <sal-expr>) (context <ltl-context>))
  '())

(define-method (ltl->pre-vwaa-transition-list (expr <sal-or>) (source <sal-expr>) (context <ltl-context>))
  (let ((arg-list (sal-application/argument-list expr)))
    (fold-left (lambda (vwaa-trans-list child-expr)
                 (pre-vwaa-transition-list/union vwaa-trans-list
                                                 (ltl->pre-vwaa-transition-list child-expr source context)
                                                 source
                                                 context))
               (ltl->pre-vwaa-transition-list (car arg-list) source context)
               (cdr arg-list))))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-and>) (source <sal-expr>) (context <ltl-context>))
  (let ((arg-list (sal-application/argument-list expr)))
    (fold-left (lambda (vwaa-trans-list child-expr)
                 (pre-vwaa-transition-list/product vwaa-trans-list
                                                   (ltl->pre-vwaa-transition-list child-expr source context)
                                                   source
                                                   context))
               (ltl->pre-vwaa-transition-list (car arg-list) source context)
               (cdr arg-list))))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-ltl-x>) (source <sal-expr>) (context <ltl-context>))
  (let* ((arg (slot-value expr :arg))
         (arg-dnf (sal-ltl-expr->dnf arg))
         (cube-list (sal-dnf/cube-list arg-dnf)))
    (map (lambda (cube)
           (make-instance <pre-vwaa-transition>
                          :label (bdd/true (slot-value context :manager))
                          :target-list (sal-cube/literal-list cube)))
         cube-list)))

(define (ltl-until->pre-vwaa-transition-list expr source context)
  (multiple-value-bind 
      (p q)
      (sal-binary-application/arguments expr)
    (pre-vwaa-transition-list/union (ltl->pre-vwaa-transition-list q source context)
                                    (pre-vwaa-transition-list/product (ltl->pre-vwaa-transition-list p source context)
                                                                      (list (make-instance <pre-vwaa-transition>
                                                                                           :label (bdd/true (slot-value context :manager))
                                                                                           :target-list (list expr)))
                                                                      source
                                                                      context)
                                    source
                                    context)))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-ltl-u>) (source <sal-expr>) (context <ltl-context>))
  (ltl-until->pre-vwaa-transition-list expr source context))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-ltl-w>) (source <sal-expr>) (context <ltl-context>))
  (ltl-until->pre-vwaa-transition-list expr source context))

(define (ltl-release->pre-vwaa-transition-list expr source context)
  (multiple-value-bind 
      (p q)
      (sal-binary-application/arguments expr)
    (pre-vwaa-transition-list/product (ltl->pre-vwaa-transition-list q source context)
                                      (pre-vwaa-transition-list/union (ltl->pre-vwaa-transition-list p source context)
                                                                      (list (make-instance <pre-vwaa-transition>
                                                                                           :label (bdd/true (slot-value context :manager))
                                                                                           :target-list (list expr)))
                                                                      source
                                                                      context)
                                      source
                                      context)))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-ltl-r>) (source <sal-expr>) (context <ltl-context>))
  (ltl-release->pre-vwaa-transition-list expr source context))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-ltl-m>) (source <sal-expr>) (context <ltl-context>))
  (ltl-release->pre-vwaa-transition-list expr source context))

(define-method (ltl->pre-vwaa-transition-list :around (expr <sal-expr>) (source <sal-expr>) (context <ltl-context>))
  (cond
   ((sal-ast-table/get (slot-value context :atom-table) expr) =>
    (lambda (entry)
      (let ((label (cdr entry)))
        [assert (label) (bdd? label)]
        (list (make-instance <pre-vwaa-transition>
                             :label label
                             :target-list '())))))
   (else
    (call-next-method))))

(define-method (ltl->pre-vwaa-transition-list (expr <sal-expr>) (source <sal-expr>) (context <ltl-context>))
  (sign-unsupported-feature expr "Failed to convert expression to VWAA (very weak alternating automaton)."))

(define (ltl->pre-vwaa-initial-nodes formula)
  (let ((dnf (sal-ltl-expr->dnf formula)))
    ;; (sal/pp dnf) (print "")
    (sal-dnf/cube-list dnf)))

(define (ltl->pre-vwaa-final-nodes formula)
  (let ((final-nodes '()))
    (sal-ast/for-each (lambda (n)
                        (when (accepting-formula? n)
                          (push! n final-nodes)))
                      formula)
    final-nodes))

;;
;; A VWAA node has no trace of a LTL formula, I mean, all node information is represented by using IDs...
;;
;; source is a symbol representing the node
;; transitions is a list of <vwaa-transition>
;;
;; I'm not merging equivalent nodes, since I never found one example when n1 different from n2, and n1 is equivalent n2
;; Two nodes n1,n2 are equivalent if:
;; transitions of n1 = transitions n2
;; and 
;; n1 is a final state if and only if n2 is a final state.
(define-class <vwaa-node> () (:source :transition-list :final?))

;; :initial-nodes is a list of symbol-set, where each symbol in the set represents a node-id in the graph
;; :final-nodes is a list of symbols (node-ids)
;; :node-table is a symbol-table symbol(node-id) -> vwaa-node
;; :formula->id sal-ast-table which maps atoms to bdd-var idxs
;; :context is a <ltl-context> object
(define-class <vwaa-graph> () (:initial-nodes :final-nodes :node-table :formula->id :context))

(define (ltl->vwaa formula)
  (verbose-message 2 "  LTL -> VWAA (very weak alternating automata)...")
  (let ((formula (sal-ltl-expr->primitive-ltl-expr formula)))
     ;; (sal/pp formula) 
     ;; (print "")
    (multiple-value-bind
        (formula context)
        (make-ltl-context formula)
      ;; (sal/pp formula) 
      ;; (print "")
      (let* ((pre-initial-nodes (ltl->pre-vwaa-initial-nodes formula))
             (pre-final-nodes (ltl->pre-vwaa-final-nodes formula))
             (formula-id-mapping (make-sal-ast-table))
             (curr-id 0)
             [get-node-id (lambda (formula)
                            (trace 'ltl "node-id of ~a" (sal-ast->list formula))
                            (let ((result
                                   (cond 
                                    ((sal-ast-table/get formula-id-mapping formula) => cdr)
                                    (else 
                                     (set! curr-id (+ curr-id 1))
                                     (let ((node-id (symbol-append 'n (string->symbol (number->string curr-id)))))
                                       (sal-ast-table/put! formula-id-mapping formula node-id)
                                       node-id)))))
                              (trace 'ltl "  id = ~a" result)
                              result))]
             [formula-list->node-set (lambda (formula-list)
                                       (fold-left (lambda (set formula)
                                                    (symbol-set/add set (get-node-id formula)))
                                                  (make-symbol-set)
                                                  formula-list))]
             [make-vwaa-node (lambda (source pre-transition-list)
                               (make-instance <vwaa-node>
                                              :source (get-node-id source)
                                              :transition-list (map (lambda (pre-transition)
                                                                      (make-instance <vwaa-transition>
                                                                                     :label (slot-value pre-transition :label)
                                                                                     :target-set (formula-list->node-set 
                                                                                                  (slot-value pre-transition :target-list))))
                                                                    pre-transition-list)
                                              :final? (accepting-formula? source)))]
             (formulas-already-processed (make-sal-ast-table))
             [formula-already-processed? (lambda (formula) (sal-ast-table/get formulas-already-processed formula))]
             (node-table (make-symbol-table))
             (initial-nodes (map (lambda (cube) 
                                   (formula-list->node-set (sal-cube/literal-list cube)))
                                 pre-initial-nodes))
             (final-nodes (fold-left (lambda (set formula)
                                       (symbol-set/add set (get-node-id formula)))
                                     (make-symbol-set)
                                     pre-final-nodes))
             (build-accepting-all-node (lambda ()
                                         (let ((node-id (symbol-append 'n (string->symbol (number->string (+ curr-id 1)))))) 
                                           (make-instance <vwaa-node>
                                                          :source node-id
                                                          :target-list (list (make-instance <vwaa-transition>
                                                                                            :label (bdd/true (slot-value context :manager))
                                                                                            :target-set (symbol-set/add (make-symbol-set) node-id)))
                                                          :final? #f)))))
        (for-each (lambda (initial-cube)
                    (let process-formula-list ((formula-list (sal-cube/literal-list initial-cube)))
                      (for-each (lambda (ltl-formula)
                                  (unless (formula-already-processed? ltl-formula)
                                    (sal-ast-table/put! formulas-already-processed ltl-formula #unspecified)
                                    (let* ((transitions (ltl->pre-vwaa-transition-list ltl-formula ltl-formula context))
                                           (new-node (make-vwaa-node ltl-formula transitions)))
                                      (symbol-table/add! node-table (slot-value new-node :source) new-node)
                                      ;; process targets!
                                      (for-each (lambda (transition)
                                                  ;; (print "transition = " transition)
                                                  (process-formula-list (slot-value transition :target-list)))
                                                transitions))))
                                formula-list)))
                  pre-initial-nodes)
        (make-instance <vwaa-graph>
                       :initial-nodes initial-nodes
                       :final-nodes final-nodes
                       :node-table node-table
                       :formula->id formula-id-mapping
                       :context context)))))

(define (vwaa->dot agraph)
  (let* ((node-mapping '())
         (initials (slot-value agraph :initial-nodes))
         (formula->id (slot-value agraph :formula->id))
         (node-table (slot-value agraph :node-table))
         (context (slot-value agraph :context)))
    (print "digraph vwaa {")
    (print "initial [label = \"\", shape = \"plaintext\"];")
    ;; print label
    (display "mapping [shape = \"plaintext\", label = \"Mapping\\l")
    (sal-ast-table/for-each (lambda (formula node-id)
                              (display* node-id " -> ")
                              (sal/pp-single-line formula)
                              (display "\\l"))
                            formula->id)
    (print "\"];")
    ;; create nodes
    (symbol-table/for-each (lambda (node-id node)
                             (print node-id (if (slot-value node :final?) "[color = red]" "") ";"))
                           node-table)
    ;; connect initial states
    (for-each (lambda (initial-set)
                (let ((fake-id (gensym "i")))
                  (symbol-set/for-each (lambda (node-id)
                                         (print "initial -> " node-id " [label = \"" fake-id "\"];"))
                                       initial-set)))
              initials)
    ;; create arrows
    (symbol-table/for-each (lambda (node-id node)
                             (let ((transition-list (slot-value node :transition-list)))
                               (for-each (lambda (transition)
                                           (let* ((label (slot-value transition :label))
                                                  (sal-label (bdd->sal-expr label context))
                                                  (target-set (slot-value transition :target-set)))
                                             (if (symbol-set/empty? target-set)
                                               (let ((fake-name (gensym)))
                                                 (print fake-name "[label =\"\", shape = \"plaintext\"];")
                                                 (display node-id)
                                                 (display " -> ")
                                                 (display fake-name)
                                                 (display " [ label = \"")
                                                 (sal/pp-single-line sal-label)
                                                 (print "\"];"))
                                               (let ((id (if (= (symbol-set/size target-set) 1) "" (symbol-append (gensym "t") ':))))
                                                 (symbol-set/for-each (lambda (target)
                                                                        (display node-id)
                                                                        (display " -> ")
                                                                        (display target)
                                                                        (display "[ label = \"")
                                                                        (display id)
                                                                        (sal/pp-single-line sal-label)
                                                                        (print "\"];"))
                                                                      target-set)))))
                                         transition-list)))
                           node-table)
    (print "}")))

(define (vwaa/display agraph)
  (with-output-to-file *sal-dot-tmp-file*
    (lambda ()
      (vwaa->dot agraph)))
  (dot/show))

;; -------------------------------------------------------------------------------------------------------
;;
;; Converting Very Weak Alternating Automaton (VWAA) To a Generalized Buchi Automata (GBA)
;;
;; For each final state in th VWAA we will have a mark in the GBA. The marks are associated with
;; transitions. A transitions has a mark if it is not the source of a final state OR it is not the target
;; of a final state.
;; A trace is accepting in the GBA if it contains each mark infinitely often.
;;
;; -------------------------------------------------------------------------------------------------------


;; :label is a BDD
;; :target-set is a set of symbols (VWAA node-ids)
;; :marks is a set of symbols (VWAA node-ids)
(define-class <pre-gba-transition> () (:label :target-set :mark-set))

;; t1 <= t2 means "t1 IS MORE GENERIC THAN t2"
(define (pre-gba-transition/le? trans1 trans2 context)
  (and 
   ;; the label of trans1 is more generic than the label of trans2, that is,
   ;; label trans2 IMPLIES label of trans1
   (bdd/le? (slot-value trans2 :label) (slot-value trans1 :label))
   ;; trans1 has less targets than trans2, that is,
   ;; "there are less stuff to be valid in the next state"
   (symbol-set/subset? (slot-value trans1 :target-set) (slot-value trans2 :target-set))
   ;; trans1 has more marks than trans2
   (symbol-set/subset? (slot-value trans2 :mark-set) (slot-value trans1 :mark-set))))

(define (pre-gba-transition/equal? trans1 trans2 context)
  (and (pre-gba-transition/le? trans1 trans2 context)
       (pre-gba-transition/le? trans2 trans1 context)))

(define (pre-gba-transition-list/equal? trans-list1 trans-list2 context)
  (and (for-all (lambda (t1) (exists (lambda (t2) (pre-gba-transition/equal? t1 t2 context)) trans-list2)) trans-list1)
       (for-all (lambda (t2) (exists (lambda (t1) (pre-gba-transition/equal? t1 t2 context)) trans-list1)) trans-list2)))

(define (pre-gba-transition-list/insert gba-trans-list t1 context)
  (if (or (bdd/false? (slot-value t1 :label))
          (exists (lambda (t2) (pre-gba-transition/le? t2 t1 context)) gba-trans-list))
    gba-trans-list
    (cons t1 (remove-if (lambda (t2) (pre-gba-transition/le? t1 t2 context)) gba-trans-list))))

(define (pre-gba-transition-list/union trans-list1 trans-list2 context)
  (fold-left (lambda (result-list transition)
               (pre-gba-transition-list/insert result-list transition context))
             trans-list1
             trans-list2))

(define (vwaa-graph/node graph node-id)
  (symbol-table/lookup (slot-value graph :node-table) node-id))

(define (vwaa-node-set/pre-gba-transition-list-product vwaa-node-set final-state-set context vwaa-graph)
  (let ((bdd-manager (slot-value context :manager)))
    (if (symbol-set/empty? vwaa-node-set)
      (list (make-instance <pre-gba-transition>
                           :label (bdd/true (slot-value context :manager))
                           :target-set (make-symbol-set)
                           :mark-set final-state-set))
      (let* ([compute-mark-set
              (lambda (label target-set)
                (symbol-set/fold (lambda (final-state-id mark-set)
                                   (if (or (not (symbol-set/member? final-state-id target-set)) ;; transition goes to a final state
                                           (let ((final-state-node (vwaa-graph/node vwaa-graph final-state-id)))
                                             ;; In a previous version, the following test was:
                                             ;; (not (eq? final-state-id source)))
                                             ;; where source source was an extra parameter of the function, that is, the souce of the
                                             ;; pre-gba-transition list we are building.
                                             ;;
                                             ;; The new test produces more compact gba, and the idea is:
                                             ;; 
                                             ;; - the label and target-set pair is more specific thant a non self loop final state transition
                                             (exists (lambda (final-state-transition)
                                                       (and 
                                                        ;; final-state-transition is not a self loop.
                                                        (not (symbol-set/member? final-state-id (slot-value final-state-transition :target-set)))
                                                        ;; `label' is more specific than the :label of a final state transition
                                                        (bdd/le? label (slot-value final-state-transition :label))
                                                        ;; target-set is more specific that the :target-set of a final state transition
                                                        (symbol-set/subset? (slot-value final-state-transition :target-set) target-set)))
                                                     (slot-value final-state-node :transition-list))))
                                     (symbol-set/add mark-set final-state-id)
                                     mark-set))
                                 (make-symbol-set)
                                 final-state-set))]
           [vwaa-transition->pre-gba-transition 
            (lambda (vwaa-trans)
              (let ((label (slot-value vwaa-trans :label))
                    (target-set (slot-value vwaa-trans :target-set)))
                (make-instance <pre-gba-transition>
                               :label label
                               :target-set target-set
                               :mark-set (compute-mark-set label target-set))))]
           [pre-gba-transition/product 
            (lambda (trans1 trans2)
              (let ((label (bdd/and (slot-value trans1 :label) (slot-value trans2 :label)))
                    (target-set (symbol-set/union (slot-value trans1 :target-set) (slot-value trans2 :target-set))))
                (make-instance <pre-gba-transition>
                               :label label
                               :target-set target-set
                               :mark-set (compute-mark-set label target-set))))]
           [pre-gba-transition-list/product
            (lambda (trans-list1 trans-list2)
              (let ((result '()))
                (for-each (lambda (t1)
                            (for-each (lambda (t2)
                                        (set! result (pre-gba-transition-list/insert result
                                                                                     (pre-gba-transition/product t1 t2)
                                                                                     context)))
                                      trans-list2))
                          trans-list1)
                result))]
           (gba-transition-lists (symbol-set/fold (lambda (node-id list)
                                                    (let* ((node (vwaa-graph/node vwaa-graph node-id))
                                                           (transition-list (slot-value node :transition-list)))
                                                      (cons (map vwaa-transition->pre-gba-transition transition-list) list)))
                                                  '()
                                                  vwaa-node-set)))
        (fold-left pre-gba-transition-list/product
                   (car gba-transition-lists)
                   (cdr gba-transition-lists))))))

;; In vwaa->gba convertion gba-nodes can be merged, so source are lists of set of symbols, where each symbol is a vwaa node-id.
;; Anyway... the output will be converted to an id in the final step of the
;; vwaa->gba conversion.
(define-class <pre-gba-node> () (:source-set-list :transition-list))

(define (pre-gba-node/equivalent? anode1 anode2 context)
  ;; (print "gba-node/equivalent? " anode1 anode2)
  (pre-gba-transition-list/equal? (slot-value anode1 :transition-list)
                                  (slot-value anode2 :transition-list)
                                  context))

(define (pre-gba-node-list/insert gba-node-list anode context)
  (let loop ((alist gba-node-list))
    (cond
     ((null? alist) (cons anode gba-node-list))
     ((pre-gba-node/equivalent? (car alist) anode context)
      (pre-gba-node-list/insert (remq (car alist) gba-node-list)
                                (make-instance <pre-gba-node>
                                               :source-set-list (append (slot-value (car alist) :source-set-list)
                                                                        (slot-value anode :source-set-list))
                                               :transition-list (pre-gba-transition-list/union (slot-value (car alist) :transition-list)
                                                                                               (slot-value anode :transition-list)
                                                                                               context))
                                context))
     (else (loop (cdr alist))))))

(define (pre-gba-node-list/contains-source? gba-node-list source-set)
  (exists (lambda (curr) 
            (let ((curr-source-set-list (slot-value curr :source-set-list)))
              (exists (cut symbol-set/equal? source-set <>) curr-source-set-list)))
          gba-node-list))

(define-class <gba-graph> () (:initial-nodes :node-vector :num-marks :vwaa-node-mapping :context))

(define-class <gba-transition> () (:label :target :mark-list))
(define-class <gba-node> () (:source :transition-list))

(define (vwaa->gba agraph)
  (verbose-message 2 "  VWAA -> GBA (generalized buchi automata)...")
  (let* ((initial-nodes (slot-value agraph :initial-nodes))
         (node-table (slot-value agraph :node-table))
         (final-nodes (slot-value agraph :final-nodes))
         (context (slot-value agraph :context))
         (gba-node-list '()))
    [assert (final-nodes) (symbol-set? final-nodes)]
    (for-each (lambda (node-set)
                (let loop ((node-set node-set))
                  (unless (pre-gba-node-list/contains-source? gba-node-list node-set)
                    (let* ((result-transitions (vwaa-node-set/pre-gba-transition-list-product node-set final-nodes context agraph))
                           (result-node (make-instance <pre-gba-node>
                                                       :source-set-list (list node-set)
                                                       :transition-list result-transitions)))
                      (set! gba-node-list (pre-gba-node-list/insert gba-node-list result-node context))
                      (for-each (lambda (transition) 
                                  (loop (slot-value transition :target-set)))
                                result-transitions)))))
              initial-nodes)
    (let* ((node-mapping '()) ;; list of pairs, where each pair is a (list of symbol-sets, id) 
           (gba-node-vector (make-vector (length gba-node-list)))
           (curr-id 0)
           (get-node-id (lambda (node-set)
                          (let ((result (find (lambda (pair) 
                                                (exists (cut symbol-set/equal? node-set <>)
                                                        (car pair)))
                                              node-mapping)))
                            (and result 
                                 (cdr result)))))
           (def-node-id (lambda (anode)
                          (let* ((source-set-list (slot-value anode :source-set-list))
                                 (new-node-id curr-id)
                                 (new-node (make-instance <gba-node>
                                                          :source new-node-id
                                                          :transition-list (slot-value anode :transition-list)))) ;; temporary...
                            (vector-set! gba-node-vector new-node-id new-node)
                            (set! curr-id (+ curr-id 1))
                            (push! (cons source-set-list new-node-id) node-mapping)
                            new-node-id)))
           (num-marks (symbol-set/size final-nodes))
           (convert-mark-set (lambda (mark-set)
                               (symbol-set/fold (lambda (mark mark-list)
                                                  (cons (+ (symbol-set/rank final-nodes mark) 1) mark-list))
                                                '()
                                                mark-set)))
           (is-initial? (lambda (anode)
                          (let ((source-set-list (slot-value anode :source-set-list)))
                            (exists (lambda (initial-state) 
                                      (exists (cut symbol-set/equal? initial-state <>)
                                              source-set-list))
                                    initial-nodes))))
           (initial-nodes-id '()))
      ;; create nodes ids
      (for-each (lambda (anode)
                  (let ((node-id (def-node-id anode)))
                    (when (is-initial? anode)
                      (push! node-id initial-nodes-id))))
                gba-node-list)
      ;; update transitions
      (vector/for-each (lambda (anode)
                         (let* ((transitions (slot-value anode :transition-list))
                                (new-transitions (map (lambda (pre-transition)
                                                        (let ((target-set (slot-value pre-transition :target-set))
                                                              (mark-set (slot-value pre-transition :mark-set)))
                                                          (make-instance <gba-transition>
                                                                         :target (get-node-id target-set)
                                                                         :label (slot-value pre-transition :label)
                                                                         :mark-list (convert-mark-set mark-set))))
                                                      transitions)))
                           (set-slot-value! anode :transition-list new-transitions)))
                       gba-node-vector)
      (make-instance <gba-graph>
                     :initial-nodes initial-nodes-id
                     :node-vector gba-node-vector
                     :num-marks num-marks
                     :vwaa-node-mapping node-mapping
                     :context context))))

(define (gba->dot agraph)
  (let ((initial-nodes (slot-value agraph :initial-nodes))
        (node-vector (slot-value agraph :node-vector))
        (id-node-mapping (slot-value agraph :vwaa-node-mapping))
        (context (slot-value agraph :context)))
    (print "digraph vwaa {")
    (print "final [label = \"\", shape = \"plaintext\"];")
    ;; print label
    (unless (null? id-node-mapping)
      (display "mapping [shape = \"plaintext\", label = \"Mapping\\l")
      (for-each (lambda (id-nodes-pair)
                  (sal/pp-single-line (car id-nodes-pair))
                  (display " -> ")
                  (display (cdr id-nodes-pair))
                  (display "\\l"))
                id-node-mapping)
      (print "\"];"))
    ;; print nodes
    (vector/for-each (lambda (anode)
                       (let ((source (slot-value anode :source)))
                         (print "n" source (if (memq source initial-nodes) "[color=red]" "") ";")))
                     node-vector)
    ;; print transitions
    (vector/for-each (lambda (anode)
                       (let ((source (slot-value anode :source))
                             (transitions (slot-value anode :transition-list)))
                         (for-each (lambda (transition)
                                     (let* ((label (slot-value transition :label))
                                            (sal-label (bdd->sal-expr label context))
                                            (target (slot-value transition :target))
                                            (marks (slot-value transition :mark-list)))
                                       (display* "n" source " -> " "n" target "[ label = \"")
                                       (sal/pp-single-line sal-label)
                                       (display " : ")
                                       (for-each (lambda (mark) (display* mark " ")) marks)
                                       (print "\"];")))
                                   transitions)))
                     node-vector)
    (print "}")))

(define (gba/display agraph)
  (with-output-to-file *sal-dot-tmp-file*
    (lambda ()
      (gba->dot agraph)))
  (dot/show))

;;-------------------------------------------------------------
;;
;; Simple graph algorithms used to simplify GBA and BA
;; 
;;-------------------------------------------------------------

(define (find-strongly-connected-components number-of-nodes successors-proc)
  (letrec ((colors (make-vector number-of-nodes 'white)) ;; used for depth first search
           (node-list '()) ;; list of nodes that will be ordered by finishing time in the first depth first search.
           [dfs-visit (lambda (node-id)
                        (let ((iterator (successors-proc node-id)))
                          (vector-set! colors node-id 'gray)
                          (iterator/for-each (lambda (target-id)
                                               (when (eq? (vector-ref colors target-id) 'white) 
                                                 (dfs-visit target-id)))
                                             iterator)
                          (vector-set! colors node-id 'black)
                          (push! node-id node-list)))])
    (do ((node-id 0 (+ node-id 1)))
        ((= node-id number-of-nodes))
      (if (eq? (vector-ref colors node-id) 'white)
        (dfs-visit node-id)))
    (vector-fill! colors 'white) ;; reusing colors vector
    (letrec ((transposed-graph (let* ((graph (make-vector number-of-nodes '()))
                                      (add-edge (lambda (source-id target-id)
                                                  (vector-set! graph source-id (cons target-id (vector-ref graph source-id))))))
                                 (do ((source-id 0 (+ source-id 1)))
                                     ((= source-id number-of-nodes))
                                   (iterator/for-each (lambda (target-id) (add-edge target-id source-id)) (successors-proc source-id)))
                                 graph))
             (predecessors (make-vector number-of-nodes #f))
             [dfs-visit (lambda (node-id)
                          (vector-set! colors node-id 'gray)
                          (for-each (lambda (target-id)
                                      (if (and (eq? node-id target-id) ;; self loop
                                               (not (vector-ref predecessors node-id)))
                                        (vector-set! predecessors node-id node-id))
                                      (when (eq? (vector-ref colors target-id) 'white) 
                                        (vector-set! predecessors target-id node-id)
                                        (dfs-visit target-id)))
                                    (vector-ref transposed-graph node-id))
                          (vector-set! colors node-id 'black))])
      (for-each (lambda (node-id)
                  (dfs-visit node-id))
                node-list)
      (let loop ((sccs '())
                 (node-id 0))
        (if (< node-id number-of-nodes)
          (let ((predecessor (vector-ref predecessors node-id)))
            (if predecessor
              (let inner-loop ((to-add (adjoin predecessor (adjoin node-id '()))) ;; predecessor and node-id may be equal
                           (curr-sccs sccs)
                           (new-sccs '()))
                (if (null? curr-sccs)
                  (loop (cons to-add new-sccs) (+ node-id 1))
                  (if (null? (intersection to-add (car curr-sccs)))
                    (inner-loop to-add (cdr curr-sccs) (cons (car curr-sccs) new-sccs))
                    (inner-loop (union (car curr-sccs) to-add) (cdr curr-sccs) new-sccs))))
              (loop sccs (+ node-id 1))))
          sccs)))))

;; Return a list of nodes that reach a fair SCC
(define (nodes-that-reach-fair-scc number-of-nodes successors-proc in-fair-scc?)
  (letrec ((result #f)
           (reach-fair-scc '()) ;; list of nodes that reach a fair SCC
           (colors (make-vector number-of-nodes 'white))
           [dfs-visit (lambda (node-id)
                        (cond 
                         ((in-fair-scc? node-id)
                          (pushnew! node-id reach-fair-scc)
                          #t)
                         ((memq node-id reach-fair-scc)
                          #t)
                         (else 
                          (vector-set! colors node-id 'black)
                          (bind-exit (exit)
                            (iterator/for-each (lambda (target-id)
                                                 (when (or (and (eq? (vector-ref colors target-id) 'white)
                                                                (dfs-visit target-id))
                                                           (memq target-id reach-fair-scc))
                                                   (pushnew! node-id reach-fair-scc)
                                                   (exit #t)))
                                               (successors-proc node-id))
                            #f))))]
           (reach-fair-scc? (lambda (node-id) (memq node-id reach-fair-scc))))
          (do ((node-id 0 (+ node-id 1)))
              ((= node-id number-of-nodes))
            (dfs-visit node-id))
          reach-fair-scc))

;; After some simplifications, a graph may contain unreachable nodes
(define (remove-unreachable-nodes! node-set initial-nodes successors-proc map-node build-graph)
  (let* ((number-of-nodes (vector-length node-set))
         (marked-nodes (make-vector number-of-nodes #f))
         (mark-node (lambda (node-id) (vector-set! marked-nodes node-id #t)))
         (num-marked-nodes 0)
         (marked? (lambda (node-id) 
                    (vector-ref marked-nodes node-id))))
    (for-each (lambda (initial-node-id)
                (let loop ((node-id initial-node-id))
                  (unless (marked? node-id)
                    (mark-node node-id)
                    (set! num-marked-nodes (+ num-marked-nodes 1))
                    (iterator/for-each loop (successors-proc node-id)))))
              initial-nodes)
    (if (= number-of-nodes num-marked-nodes)
      #f
      (let* ((node-mapping '())
             (new-node-id (lambda (old-id)
                            ;; (print "new-node-id old-id = " old-id ", node-mapping = " node-mapping)
                            (cond 
                             ((assq old-id node-mapping) => cdr)
                             (else #f))))
             (map-node-id (lambda (old-id new-id)
                            (push! (cons old-id new-id) node-mapping)))
             (curr-id 0)
             (new-node-set (make-vector num-marked-nodes)))
        (let loop ((node-id 0))
          (when (< node-id number-of-nodes)
            (when (marked? node-id)
              (map-node-id node-id curr-id)
              (set! curr-id (+ curr-id 1)))
            (loop (+ node-id 1))))
        (let ((new-initial-nodes (map new-node-id initial-nodes)))
          (do ((node-id 0 (+ node-id 1)))
              ((= node-id number-of-nodes))
            (when (marked? node-id)
              (let ((new-id (new-node-id node-id)))
                (vector-set! new-node-set new-id (map-node node-id new-node-id)))))
          (build-graph new-initial-nodes new-node-set))))))

;;-------------------------------------------------------------
;;
;; GBA Simplifications
;; 
;;-------------------------------------------------------------

(define (gba/get-node node-vector node-id)
  (vector-ref node-vector node-id))

(define (gba-node/transition-iterator node)
  (iterator/map (lambda (transition)
                  (slot-value transition :target))
                (make-list-iterator (slot-value node :transition-list))))

(define (gba/remove-unreachable-nodes! agraph)
  (let ((node-vector (slot-value agraph :node-vector)))
    (or (remove-unreachable-nodes! node-vector
                                   (slot-value agraph :initial-nodes)
                                   (lambda (node-id) (gba-node/transition-iterator (gba/get-node node-vector node-id)))
                                   (lambda (node-id new-node-id-mapping)
                                     (let ((new-id (new-node-id-mapping node-id))
                                           (transitions (slot-value (gba/get-node node-vector node-id) :transition-list)))
                                       (for-each (lambda (transition) 
                                                   (set-slot-value! transition :target (new-node-id-mapping (slot-value transition :target))))
                                                 transitions)
                                       (make-instance <gba-node>
                                                      :source new-id
                                                      :transition-list transitions)))
                                   (lambda (new-initial-nodes new-node-vector)
                                     (copy-instance agraph
                                                    :initial-nodes new-initial-nodes
                                                    :node-vector new-node-vector)))
        agraph)))

(define (make-empty-gba context)
  (make-instance <gba-graph>
                 :initial-nodes '(0)
                 :node-vector (vector (make-instance <gba-node>
                                                     :source 0
                                                     :transition-list '()))
                 :num-marks 0
                 :vwaa-node-mapping '()
                 :context context))

(define (gba-transtion-list/compress! transition-list context)
  (let ((new-list '()))
    (for-each (lambda (t1)
                (for-each (lambda (t2)
                            (when (eq? (slot-value t1 :target) (slot-value t2 :target))
                              (cond
                               ((subset? (slot-value t1 :mark-list) (slot-value t2 :mark-list))
                                ;; it is better to follow `t2'
                                (set-slot-value! t1 :label (bdd/and (slot-value t1 :label) (bdd/not (slot-value t2 :label)))))
                               (else
                                ;; it is better to follow `t1'
                                (set-slot-value! t2 :label (bdd/and (slot-value t2 :label) (bdd/not (slot-value t1 :label))))))))
                          new-list)
                (unless (bdd/false? (slot-value t1 :label))
                  (push! t1 new-list)))
              transition-list)
    (remove-if (lambda (t) (bdd/false? (slot-value t :label))) new-list)))

(define (gba/compress-transitions! agraph)
  (let ((node-vector (slot-value agraph :node-vector))
        (context (slot-value agraph :context)))
    (vector/for-each (lambda (node)
                       (let* ((transition-list (slot-value node :transition-list))
                              (new-transition-list (gba-transtion-list/compress! transition-list context)))
                         (set-slot-value! node :transition-list new-transition-list)))
                     node-vector)))

(define (gba/sccs agraph)
  (let* ((node-vector (slot-value agraph :node-vector))
         (successors-proc (lambda (node-id) 
                            (gba-node/transition-iterator (gba/get-node node-vector node-id))))
         (number-of-nodes (vector-length node-vector)))
    (find-strongly-connected-components number-of-nodes successors-proc)))

(define (gba/fair-sccs agraph sccs)
  (let* ((node-vector (slot-value agraph :node-vector))
         (num-marks (slot-value agraph :num-marks))
         (mark-found-vector (make-vector num-marks #f)))
    (filter (lambda (scc)
              ;; A strongly connected compontent is fair if it contains an accepting cycle, i.e.
              ;; all marks appear in the scc
              (vector-fill! mark-found-vector #f)
              (for-each (lambda (node-id)
                          (let ((node (gba/get-node node-vector node-id)))
                            (for-each (lambda (transition)
                                        (if (memq (slot-value transition :target) scc) ;; transition inside the scc
                                          (for-each (lambda (mark)
                                                      (vector-set! mark-found-vector (- mark 1) #t)) ;; marks are from 1 to num-marks
                                                    (slot-value transition :mark-list))))
                                      (slot-value node :transition-list))))
                        scc)
              (iterator/for-all identity (make-vector-iterator mark-found-vector)))
            sccs)))

(define (in-scc? sccs node-id)
  (exists (lambda (scc) (memq node-id scc)) sccs))

;; Optimization 
;; Remove marks of transitions of unfair nodes, i.e., nodes that are not in a fair sccs
(define (gba/remove-marks-of-unfair-nodes! agraph fair-sccs)  
  (let ((node-vector (slot-value agraph :node-vector)))
    (vector/for-each (lambda (node)
                       (unless (in-scc? fair-sccs (slot-value node :source))
                         (for-each (lambda (transition)
                                     (set-slot-value! transition :mark-list '()))
                                   (slot-value node :transition-list))))
                     node-vector)))


;; Remove nodes that can't reach a node in a fair-scc
;; Actually, this function doesn't remove any nodes, but the transitions to the nodes.
(define (gba/remove-nodes-that-cannot-reach-fair-scc! agraph fair-sccs)
  (let* ((node-vector (slot-value agraph :node-vector))
         (successors-proc (lambda (node-id) 
                            (gba-node/transition-iterator (gba/get-node node-vector node-id))))
         (number-of-nodes (vector-length node-vector))
         (reach-fair-scc (nodes-that-reach-fair-scc number-of-nodes successors-proc (cut in-scc? fair-sccs <>)))
         (reach-fair-scc? (lambda (node-id) (memq node-id reach-fair-scc))))
    (when (not (= (length reach-fair-scc) number-of-nodes))
            ;; I only keep transitions to nodes that reach a fair SCC
      (vector/for-each (lambda (node)
                         (set-slot-value! node :transition-list
                                          (filter (lambda (transition)
                                                    (reach-fair-scc? (slot-value transition :target)))
                                                  (slot-value node :transition-list))))
                       node-vector))))

;; ---------------- IMPORTANT -----------------------
;; This transformation is WRONG!
;; For instance, it produces an invalid buchi automata for the formula
;; X(G(a))
;; --------------------------------------------------
;;
;; The following optimization is a little HACK
;; basically it will try to remove very specific type of nodes...
;;
;; I remove nodes "n" such that
;; 
;; - "n" is not in a SCC
;; - exists only one transition from "n"
;; - this transition is labeled "true" 
;; - the target node "nt" of this transition, contains
;;   a self loop labeled "true"
;;   
;; Actually, this function doesn't remove any nodes, but the transitions to the nodes.
(define (gba/remove-irrelevant-nodes! agraph sccs)
  (let* ((node-vector (slot-value agraph :node-vector))
         (context (slot-value agraph :context))
         (initial-nodes (slot-value agraph :initial-nodes))
         (m (slot-value context :manager)))
    (vector/for-each (lambda (node)
                       (let ((node-id (slot-value node :source)))
                             (unless (in-scc? sccs node-id) ;; I only consider nodes that are not in a strongly connected component
                               (let ((transitions (slot-value node :transition-list)))
                                 (when (and (not (null? transitions)) (null? (cdr transitions)))
                                   (let* ((transition (car transitions))
                                          (target-id (slot-value transition :target))
                                          (target (gba/get-node node-vector target-id)))
                                     (when (and (bdd/true? (slot-value transition :label))
                                                (exists (lambda (transition) ;; target also contains a "true label" self loop
                                                          (and (= (slot-value transition :target) target-id)
                                                               (bdd/true? (slot-value transition :label))))
                                                        (slot-value target :transition-list)))
                                       ;; node contains only one transition labeled "true" to a node which
                                       ;; contains a self loop labeled "true"
                                       ;; So, node can be removed, since it is not in a SCC
                                       (when (memq node-id initial-nodes)
                                         (set! initial-nodes (cons target-id (delete node-id initial-nodes)))
                                         (set-slot-value! agraph :initial-nodes initial-nodes))
                                       ;; update transitions
                                       (vector/for-each (lambda (node)
                                                          (for-each (lambda (transition)
                                                                      (let ((old-target-id (slot-value transition :target)))
                                                                        (when (= old-target-id node-id)
                                                                          (set-slot-value! transition :target target-id))))
                                                                    (slot-value node :transition-list)))
                                                        node-vector))))))))
                         node-vector)))
    
(define (gba/simplify! agraph)
  (verbose-message 2 "  simplifying GBA...")
  (let* ((sccs (gba/sccs agraph))
         (fair-sccs (gba/fair-sccs agraph sccs)))
    (gba/remove-marks-of-unfair-nodes! agraph fair-sccs)
    (gba/compress-transitions! agraph)
    ;; (gba/remove-irrelevant-nodes! agraph sccs) ;; Read comment above
    (gba/remove-nodes-that-cannot-reach-fair-scc! agraph fair-sccs)
    (set-slot-value! agraph :vwaa-node-mapping '())
    (gba/remove-unreachable-nodes! agraph)))

;;-------------------------------------------------------------------------
;;
;; Converting a Generalized Buchi Automata (GBA) To a Buchi Automata (BA)
;;
;;-------------------------------------------------------------------------

;; I do not use a pre-ba-transition... 
(define-class <ba-transition> () (:label :target))

(define (ba-transition/le? trans1 trans2 context)
  (and (bdd/le? (slot-value trans2 :label) (slot-value trans1 :label))
       (equal? (slot-value trans2 :target) (slot-value trans1 :target))))

(define (ba-transition/equal? trans1 trans2)
  (and (bdd/eq? (slot-value trans2 :label) (slot-value trans1 :label))
       (equal? (slot-value trans2 :target) (slot-value trans1 :target))))

(define (ba-transition-list/equal? trans-list1 trans-list2) 
  (and
   (for-all (lambda (t1) (exists (lambda (t2) (ba-transition/equal? t1 t2)) trans-list2)) trans-list1)
   (for-all (lambda (t2) (exists (lambda (t1) (ba-transition/equal? t1 t2)) trans-list1)) trans-list2)))

(define (ba-transition-list/insert ba-trans-list t1 context)
  (if (or (bdd/false? (slot-value t1 :label))
          (exists (lambda (t2) (ba-transition/le? t2 t1 context)) ba-trans-list))
    ba-trans-list
    (cons t1 (remove-if (lambda (t2) (ba-transition/le? t1 t2 context)) ba-trans-list))))

(define (ba-transition-list/union trans-list1 trans-list2 context)
  (fold-left (lambda (result-list transition)
               (ba-transition-list/insert result-list transition context))
             trans-list1
             trans-list2))

(define (gba-transition->ba-transition gba-trans curr-mark num-marks)
  (let ((next-mark  (let ((marks (slot-value gba-trans :mark-list))
                          (curr-mark (if (= curr-mark num-marks) 0 curr-mark))) ;; modulo arithmetic
                      (let loop ((i curr-mark)
                                 (k (+ curr-mark 1)))
                        [assert (i k) (= (+ i 1) k)]
                        (cond
                         ((> k num-marks) i)
                         ((memq k marks)
                          (loop k (+ k 1)))
                         (else i))))))
    (make-instance <ba-transition>
                   :label (slot-value gba-trans :label)
                   :target (cons (slot-value gba-trans :target)
                                 next-mark))))

(define (gba-transition-list->ba-transition-list gba-trans-list curr-mark num-marks context)
  (fold-left (lambda (result-list gba-trans)
               (ba-transition-list/insert result-list 
                                          (gba-transition->ba-transition gba-trans curr-mark num-marks)
                                          context))
             '()
             gba-trans-list))

;; In gba->ba convertion ba-nodes can be merged, so a source is a list of pairs.
;; Anyway... the output will be converted to an id in the final step of the
;; gba->ba conversion.
(define-class <pre-ba-node> () (:source-list :transition-list))

(define (pre-ba-node/final? anode num-marks)
  (let ((source-list (slot-value anode :source-list)))
    (exists (lambda (real-source) (= (cdr real-source) num-marks)) source-list)))

(define (pre-ba-node/equivalent? anode1 anode2 num-marks)
  (and
   (ba-transition-list/equal? (slot-value anode1 :transition-list)
                              (slot-value anode2 :transition-list))
   (eq? (pre-ba-node/final? anode1 num-marks) 
        (pre-ba-node/final? anode2 num-marks))))


(define (pre-ba-node-list/insert ba-node-list anode num-marks context)
  (let loop ((alist ba-node-list))
    (cond
     ((null? alist) (cons anode ba-node-list))
     ((pre-ba-node/equivalent? (car alist) anode num-marks)
      (pre-ba-node-list/insert (remq (car alist) ba-node-list)
                               (make-instance <pre-ba-node>
                                              :source-list (append (slot-value (car alist) :source-list)
                                                                   (slot-value anode :source-list))
                                              :transition-list (ba-transition-list/union (slot-value (car alist) :transition-list)
                                                                                         (slot-value anode :transition-list)
                                                                                         context))
                               num-marks
                               context))
     (else (loop (cdr alist))))))

(define (pre-ba-node-list/contains-source? ba-node-list source curr-mark)
  (exists (lambda (anode) 
            (let ((source-list (slot-value anode :source-list)))
              (exists (lambda (source-mark-pair) 
                        (and (eq? (car source-mark-pair) source)
                             (= (cdr source-mark-pair) curr-mark)))
                      source-list)))
          ba-node-list))


(define-class <ba-graph> () (:initial-nodes :final-nodes :node-vector :gba-node-mapping :context))

(define-class <ba-node> () (:source :transition-list))


(define (make-empty-ba context)
  (make-instance <ba-graph>
                 :initial-nodes '(0)
                 :final-nodes '()
                 :node-vector (vector (make-instance <ba-node>
                                                     :source 0
                                                     :transition-list '()))
                 :gba-node-mapping '()
                 :context context))

(define (gba->ba agraph)
  (verbose-message 2 "  GBA -> BA (buchi automata)...")
  (let* ((initial-nodes (slot-value agraph :initial-nodes))
         (node-vector (slot-value agraph :node-vector))
         (num-marks (slot-value agraph :num-marks))
         (context (slot-value agraph :context))
         (ba-node-list '()))
    (for-each (lambda (initial-node)
                (let loop ((node-id initial-node)
                           (curr-mark 0))
                  (let ((node (gba/get-node node-vector node-id)))
                    (unless (pre-ba-node-list/contains-source? ba-node-list node-id curr-mark)
                      (let* ((result-transitions (gba-transition-list->ba-transition-list (slot-value node :transition-list)
                                                                                          curr-mark
                                                                                          num-marks
                                                                                          context))
                             (result-node (make-instance <pre-ba-node>
                                                         :source-list (list (cons node-id curr-mark))
                                                         :transition-list result-transitions)))
                        (set! ba-node-list (pre-ba-node-list/insert ba-node-list result-node num-marks context))
                        (for-each (lambda (transition)
                                    (let ((target (slot-value transition :target)))
                                      (loop (car target) (cdr target))))
                                  result-transitions))))))
              initial-nodes)
    ;; create node ids,...
    (let* ((node-mapping '())
           (curr-id 0)
           (ba-node-vector (make-vector (length ba-node-list)))
           (get-node-id (lambda (node-mark-pair)
                          (let ((result (find (lambda (pair) 
                                                (exists (lambda (node-mark-pair2)
                                                          (equal? node-mark-pair node-mark-pair2))
                                                        (car pair))) 
                                              node-mapping)))
                            (and result 
                                 (cdr result)))))
           (def-node-id (lambda (anode)
                          (let* ((source-list (slot-value anode :source-list))
                                 (new-node-id curr-id)
                                 (new-node (make-instance <ba-node>
                                                          :source new-node-id
                                                          :transition-list (slot-value anode :transition-list))))
                            (vector-set! ba-node-vector new-node-id new-node)
                            (set! curr-id (+ curr-id 1))
                            (push! (cons source-list new-node-id) node-mapping)
                            new-node-id)))
           (initial? (lambda (n) (exists (lambda (s) (and (memq (car s) initial-nodes) (= (cdr s) 0))) 
                                         (slot-value n :source-list))))
           (final? (lambda (n) (exists (lambda (s) (= (cdr s) num-marks))
                                       (slot-value n :source-list))))
           (initial-nodes-id '())
           (final-nodes-id '()))
      ;; create nodes ids
      (for-each (lambda (anode)
                  (let* ((node-id (def-node-id anode)))
                    (when (initial? anode)
                      (push! node-id initial-nodes-id))
                    (when (final? anode)
                      (push! node-id final-nodes-id))))
                ba-node-list)
      ;; update transitions
      (vector/for-each (lambda (anode)
                         (let ((transitions (slot-value anode :transition-list)))
                           (for-each (lambda (transition)
                                       (let ((target (slot-value transition :target)))
                                         (set-slot-value! transition :target (get-node-id target))))
                                     transitions)))
                       ba-node-vector)
      (make-instance <ba-graph>
                     :initial-nodes initial-nodes-id
                     :final-nodes final-nodes-id
                     :node-vector ba-node-vector
                     :gba-node-mapping node-mapping
                     :context context))))

(define (ba->dot agraph)
  (let ((initial-nodes (slot-value agraph :initial-nodes))
        (final-nodes (slot-value agraph :final-nodes))
        (node-vector (slot-value agraph :node-vector))
        (context (slot-value agraph :context))
        (gba-node-mapping (slot-value agraph :gba-node-mapping)))
    (print "digraph vwaa {")
    (print "final [label = \"\", shape = \"plaintext\"];")
    ;; print nodes
    (vector/for-each (lambda (anode)
                       (let ((source (slot-value anode :source)))
                         (print "n" source 
                                "["
                                "shape=" (if (memq source final-nodes) "doublecircle" "circle")
                                (if (memq source initial-nodes) ", color=red" "")
                                "];")))
                     node-vector)
    ;; print label
    (unless (null? gba-node-mapping)
      (display "mapping [shape = \"plaintext\", label = \"Mapping\\l")
      (for-each (lambda (id-nodes-pair)
                  (display* (car id-nodes-pair) " -> ")
                  (display (cdr id-nodes-pair))
                  (display "\\l"))
                gba-node-mapping)
      (print "\"];"))
    ;; print info label
    (display "info [shape = \"plaintext\", label = \"red circles: initial states\\ldouble circles: final states\"];")
    ;; print transitions
    (vector/for-each (lambda (anode)
                       (let* ((source (slot-value anode :source))
                              (transitions (slot-value anode :transition-list)))
                         (for-each (lambda (transition)
                                     (let* ((label (slot-value transition :label))
                                            (sal-label (bdd->sal-expr label context))
                                            (target (slot-value transition :target)))
                                       (display* "n" source " -> " "n" target "[ label = \"")
                                       (sal/pp-single-line sal-label)
                                       (print "\"];")))
                                   transitions)))
                     node-vector)
    (print "}")))

(define (ba/display agraph)
  (with-output-to-file *sal-dot-tmp-file*
    (lambda ()
      (ba->dot agraph)))
  (dot/show))

;;-------------------------------------------------------------
;;
;; BA Simplifications
;; 
;;-------------------------------------------------------------

(define (ba/get-node node-vector node-id)
  (vector-ref node-vector node-id))

(define (ba-node/transition-iterator anode)
  (iterator/map (lambda (transition)
                  (slot-value transition :target)) 
                (make-list-iterator (slot-value anode :transition-list))))

(define (ba/remove-unreachable-nodes! agraph)
  (let ((node-vector (slot-value agraph :node-vector))
        (final-nodes (slot-value agraph :final-nodes))
        (new-final-nodes '()))
    (or (remove-unreachable-nodes! node-vector
                                   (slot-value agraph :initial-nodes)
                                   (lambda (node-id) (ba-node/transition-iterator (ba/get-node node-vector node-id)))
                                   (lambda (node-id new-node-id-mapping)
                                     ;; hack... to set the set of final states
                                     (when (eq? new-final-nodes '())
                                       (set! new-final-nodes (map-and-filter new-node-id-mapping final-nodes)))
                                     (let ((new-id (new-node-id-mapping node-id))
                                           (transitions (slot-value (ba/get-node node-vector node-id) :transition-list)))
                                       (for-each (lambda (transition)
                                                   (set-slot-value! transition :target (new-node-id-mapping (slot-value transition :target))))
                                                 transitions)
                                       (make-instance <ba-node>
                                                      :source new-id
                                                      :transition-list transitions)))
                                   (lambda (new-initial-nodes new-node-vector)
                                     (copy-instance agraph
                                                    :initial-nodes new-initial-nodes
                                                    :final-nodes new-final-nodes
                                                    :node-vector new-node-vector)))
        agraph)))

(define (ba/sccs agraph)
  (gba/sccs agraph))

(define (ba/fair-sccs agraph sccs)
  (let ((final-nodes (slot-value agraph :final-nodes)))
    (filter (lambda (scc)
              ;; A SCC is fair if it contains a final state
              (exists (lambda (node-id)
                        (memq node-id final-nodes))
                      scc))
            sccs)))

;; I will remove nodes that can't reach a node in a fair-scc
;;
;; returns true if a node was removed
(define (ba/remove-nodes-that-cant-reach-fair-scc! agraph fair-sccs)
  (gba/remove-nodes-that-cannot-reach-fair-scc! agraph fair-sccs))

(define (ba/can-merge-nodes? agraph node-id1 node-id2 unfair-sccs)
  [assert (node-id1) (not (in-scc? (ba/sccs agraph) node-id1))] ;; this test is only valid if node1 is NOT in a SCC
  (let ((final-nodes (slot-value agraph :final-nodes))
        (node-vector (slot-value agraph :node-vector)))
    ;; this transformation is not valid if
    ;;  node1 is in final, and node2 is in a unfair scc
    (and (not (and (memq node-id1 final-nodes) 
                   (in-scc? unfair-sccs node-id2)))
         (let ((node1 (ba/get-node node-vector node-id1))
               (node2 (ba/get-node node-vector node-id2)))
           (ba-transition-list/equal? (slot-value node1 :transition-list)
                                      (slot-value node2 :transition-list))))))

(define (ba/merge-nodes! agraph sccs unfair-sccs)
  (and *sal-ba-expensive-optimizations*
       (let ((node-vector (slot-value agraph :node-vector))
             (initial-nodes (slot-value agraph :initial-nodes)))
         (vector/for-each (lambda (node1)
                            (let ((node-id1 (slot-value node1 :source)))
                              (unless (in-scc? sccs node-id1)
                                (vector/for-each  (lambda (node2)
                                                    (let ((node-id2 (slot-value node2 :source)))
                                                      (unless (= node-id1 node-id2)
                                                        (when (ba/can-merge-nodes? agraph node-id1 node-id2 unfair-sccs)
                                                          ;; update the initial-states if necessary
                                                          (when (memq node-id1 initial-nodes)
                                                            (set! initial-nodes (cons node-id2 (delete node-id1 initial-nodes)))
                                                            (set-slot-value! agraph :initial-nodes initial-nodes))
                                                          ;; update transitions
                                                          (vector/for-each (lambda (node)
                                                                             (for-each (lambda (transition)
                                                                                         (let ((target-id (slot-value transition :target)))
                                                                                           (when (= target-id node-id1)
                                                                                             (set-slot-value! transition :target node-id2))))
                                                                                       (slot-value node :transition-list)))
                                                                           node-vector)))))
                                                  node-vector))))
                          node-vector))))

(define (relation/greatest-fixed-point num-elements proc)
  (let* ((relation (make-vector num-elements))
         (rel? (lambda (n1 n2)
                 (vector-ref (vector-ref relation n1) n2))))
    (do ((i 0 (+ i 1)))
        ((= i num-elements))
      (vector-set! relation i (make-vector num-elements #t)))
    (let loop ((modified? #f))
      (do ((node-id1 0 (+ node-id1 1)))
          ((= node-id1 num-elements))
        (let* ((vect (vector-ref relation node-id1)))
          (do ((node-id2 0 (+ node-id2 1)))
              ((= node-id2 num-elements))
            (let ((element? (vector-ref vect node-id2)))
              (unless (eq? element?
                           (and element? (proc node-id1 node-id2 rel?)))
                (vector-set! vect node-id2 #f)
                (set! modified? #t))))))
      (if modified?
        (loop #f)
        rel?))))

(define (ba/direct-simulation agraph sccs)
  (let* ((final-nodes (slot-value agraph :final-nodes))
         (node-vector (slot-value agraph :node-vector))
         (context (slot-value agraph :context))
         (number-of-nodes (vector-length node-vector)))
    (relation/greatest-fixed-point
     number-of-nodes
     (lambda (idx1 idx2 rel?)
       (or (= idx1 idx2)
           (let* ((node1 (vector-ref node-vector idx1))
                  (node2 (vector-ref node-vector idx2))
                  (final1? (memq idx1 final-nodes))
                  (final2? (memq idx2 final-nodes)))
             (and
              (or (imply final1? final2?) (not (in-scc? sccs idx1)))
              (for-all (lambda (t1)
                         (let ((label1 (slot-value t1 :label))
                               (target1 (slot-value t1 :target)))
                           (exists (lambda (t2)
                                     (let ((label2 (slot-value t2 :label))
                                           (target2 (slot-value t2 :target)))
                                       (and 
                                        (bdd/le? label1 label2)
                                        (rel? target1 target2))))
                                   (slot-value node2 :transition-list))))
                       (slot-value node1 :transition-list)))))))))


(define (compute-inverted-transitions node-vector)
  (let* ((number-of-nodes (vector-length node-vector))
         (result (make-vector number-of-nodes '())))
    (do ((node-id 0 (+ node-id 1)))
        ((= node-id number-of-nodes))
      (let ((node (vector-ref node-vector node-id)))
        (for-each (lambda (trans)
                    (let ((label (slot-value trans :label))
                          (target (slot-value trans :target)))
                      (vector-set! result target (cons (make-instance <ba-transition>
                                                                      :label label 
                                                                      :target node-id)
                                                       (vector-ref result target)))))
                  (slot-value node :transition-list))))
    result))

(define (ba/reverse-simulation agraph sccs)
  (let* ((final-nodes (slot-value agraph :final-nodes))
         (initial-nodes (slot-value agraph :initial-nodes))
         (context (slot-value agraph :context))
         (node-vector (slot-value agraph :node-vector))
         (number-of-nodes (vector-length node-vector))
         (inverted-transitions (compute-inverted-transitions node-vector)))
    (relation/greatest-fixed-point 
     number-of-nodes
     (lambda (idx1 idx2 rel?)
       (or (= idx1 idx2)
           (let* ((final1? (memq idx1 final-nodes))
                  (final2? (memq idx2 final-nodes))
                  (initial1? (memq idx1 initial-nodes))
                  (initial2? (memq idx2 initial-nodes)))
             (and
              (imply initial1? initial2?)
              (or (imply final1? final2?) (not (in-scc? sccs idx1)))
              (for-all (lambda (t1)
                         (let ((label1 (slot-value t1 :label))
                               (target1 (slot-value t1 :target)))
                           (exists (lambda (t2)
                                     (let ((label2 (slot-value t2 :label))
                                           (target2 (slot-value t2 :target)))
                                       (and 
                                        (bdd/le? label1 label2)
                                        (rel? target1 target2))))
                                   (vector-ref inverted-transitions idx2))))
                       (vector-ref inverted-transitions idx1)))))))))



;;
;; (p,q) are weak reverse equivalent iff
;;
;;  - p is Initial <=> q is Initial
;;  - FORALL t in inv-trans(p) EXISTS t' in inv-trans(q). label(t) <=> label(t') AND (target(t), target(t')) are weak reverse equivalent
;;
(define (ba/reverse-weak-equivalence agraph)
  (let* ((final-nodes (slot-value agraph :final-nodes))
         (initial-nodes (slot-value agraph :initial-nodes))
         (node-vector (slot-value agraph :node-vector))
         (number-of-nodes (vector-length node-vector))
         (inverted-transitions (compute-inverted-transitions node-vector))
         (subset-trans? (lambda (idx1 idx2 rel?)
                          (for-all (lambda (t1)
                                     (let ((label1 (slot-value t1 :label))
                                           (target1 (slot-value t1 :target)))
                                       (exists (lambda (t2)
                                                 (let ((label2 (slot-value t2 :label))
                                                       (target2 (slot-value t2 :target)))
                                                   (and 
                                                    (bdd/eq? label1 label2)
                                                    (rel? target1 target2))))
                                               (vector-ref inverted-transitions idx2))))
                                   (vector-ref inverted-transitions idx1)))))
    (relation/greatest-fixed-point 
     number-of-nodes
     (lambda (idx1 idx2 rel?)
       (or (= idx1 idx2)
           (let* ((initial1? (obj->boolean (memq idx1 initial-nodes)))
                  (initial2? (obj->boolean (memq idx2 initial-nodes))))
             (and
              (eq? initial1? initial2?)
              (subset-trans? idx1 idx2 rel?)
              (subset-trans? idx2 idx1 rel?))))))))

(define (compute-equivalence-classes rel? number-of-nodes)
  (let loop1 ((idx1 0)
              (result '()))
    (if (< idx1 number-of-nodes)
      (let ((eq-class (let loop2 ((idx2 (+ idx1 1))
                                  (eq-class (list idx1)))
                        (if (< idx2 number-of-nodes)
                          (if (rel? idx1 idx2)
                            (loop2 (+ idx2 1)
                                   (append! eq-class (list idx2)))
                            (loop2 (+ idx2 1)
                                   eq-class))
                          eq-class))))
        (if (> (length eq-class) 1)
          (loop1 (+ idx1 1) (cons eq-class result))
          (loop1 (+ idx1 1) result)))
      result)))

(define (ba/union-transitions! transitions1 transitions2)
  (let ((result transitions1))
    (for-each (lambda (trans2)
                (let* ((target2 (slot-value trans2 :target))
                       (label2 (slot-value trans2 :label))
                       (trans1 (find (lambda (trans1)
                                       (= (slot-value trans1 :target) target2))
                                     transitions1)))
                  (if trans1
                    (set-slot-value! trans1 :label (bdd/or (slot-value trans1 :label) label2))
                    (set! result (cons trans2 result)))))
              transitions2)
    result))

;;
;; Remove infeasible transitions
;; transitions which label = #f 
(define (ba/remove-infeasible-transitions! agraph)
  (let* ((node-vector (slot-value agraph :node-vector))
         (context (slot-value agraph :context)))
    (vector/for-each (lambda (anode)
                       (set-slot-value! anode :transition-list 
                                        (filter (lambda (t) 
                                                  (not (bdd/false? (slot-value t :label))))
                                                (slot-value anode :transition-list))))
                     node-vector)))

;;
;;  If (p,q) are weak reverse equivalent, Then
;;  then can be combined in the following way:
;;
;;  If p if Final
;;     Transitions(p) := Transitions(p) Union Transitions(q)
;;     Transitions(q) := Empty Set
;;  Otherwise
;;     Transitions(p) := Empty Set
;;     Transitions(q) := Transitions(p) Union Transitions(q)
;; Returns #t if the optimization reduced the BA size
(define (ba/combine-weak-reverse-equivalent-nodes! agraph)
  (and *sal-ba-expensive-optimizations*
       (let* ((node-vector (slot-value agraph :node-vector))
              (number-of-nodes (vector-length node-vector))
              (context (slot-value agraph :context))
              (m (slot-value context :manager))
              (final-nodes (slot-value agraph :final-nodes))
              (weak-reverse-equivalent? (ba/reverse-weak-equivalence agraph))
              (equivalence-classes (compute-equivalence-classes weak-reverse-equivalent? number-of-nodes)))
         (cond
          ((null? equivalence-classes)
           (for-each (lambda (eq-class)
                       (let ((can-node-idx (find (lambda (node-idx) (memq node-idx final-nodes)) eq-class)))
                         (unless can-node-idx
                           (set! can-node-idx (car eq-class)))
                         (let ((can-node (vector-ref node-vector can-node-idx)))
                           (for-each (lambda (node-idx)
                                       (unless (= node-idx can-node-idx)
                                         (set-slot-value! can-node :transition-list
                                                          (let ((transitions1 (slot-value (vector-ref node-vector node-idx) :transition-list))
                                                                (transitions2 (slot-value can-node :transition-list)))
                                                            (ba/union-transitions! transitions1 transitions2)))
                                         (set-slot-value! (vector-ref node-vector node-idx) :transition-list '())))
                                     eq-class)
                           ;; remove transitions to nodes that are in the same equivalence class
                           (set-slot-value! can-node :transition-list
                                            (filter (lambda (trans)
                                                      (let ((target-idx (slot-value trans :target)))
                                                        (or (= target-idx can-node-idx)
                                                            (not (memq target-idx eq-class)))))
                                                    (slot-value can-node :transition-list)))
                           )))
                     equivalence-classes)
           (ba/remove-infeasible-transitions! agraph)
           #t)
          (else
           #f)))))


;; Join equivalent nodes
;; Returns #t if the optimization reduced the BA size
(define (ba/join-direct-equivalent-nodes! agraph sccs)
  (and *sal-ba-expensive-optimizations*
       (let* ((node-vector (slot-value agraph :node-vector))
              (number-of-nodes (vector-length node-vector))
              (direct-simulation? (ba/direct-simulation agraph sccs))
              (equivalent-nodes? (lambda (idx1 idx2) (and (direct-simulation? idx1 idx2) (direct-simulation? idx2 idx1))))
              (equivalence-classes (compute-equivalence-classes equivalent-nodes? number-of-nodes))
              (non-can-of-equivalence-class? (lambda (node-idx)
                                               (exists (lambda (eq-class)
                                                         ;; (print "eq-class = " eq-class)
                                                         (and (not (eq? node-idx (car eq-class)))
                                                              (memq node-idx eq-class)))
                                                       equivalence-classes)))
              (get-equivalence-class (lambda (node-idx)
                                       (find (lambda (eq-class) (memq node-idx eq-class)) equivalence-classes)))
              (node->canonical-node (lambda (node-idx)
                                      (cond
                                       ((get-equivalence-class node-idx) => car)
                                       (else node-idx))))
              (node-list->canonical-node-list (lambda (node-list)
                                                (let loop ((nodes node-list))
                                                  (if (null? nodes)
                                                    '()
                                                    (adjoin (node->canonical-node (car nodes))
                                                            (loop (cdr nodes))))))))
         ;; Join equivalent nodes
         (cond
          ((null? equivalence-classes)
           (vector/for-each (lambda (anode) 
                              (if (non-can-of-equivalence-class? anode)
                                (set-slot-value! anode :transition-list '())
                                (for-each (lambda (trans)
                                            (let* ((target (slot-value trans :target))
                                                   (eq-class (find (lambda (eq-class) (memq target eq-class)) equivalence-classes)))
                                              (when eq-class
                                                ;; (print "applying opt --> " (car eq-class))
                                                (set-slot-value! trans :target (car eq-class)))))
                                          (slot-value anode :transition-list))))
                            node-vector)
           ;; remove equivalent initial nodes
           (set-slot-value! agraph :initial-nodes (node-list->canonical-node-list (slot-value agraph :initial-nodes)))
           (set-slot-value! agraph :final-nodes (node-list->canonical-node-list (slot-value agraph :final-nodes)))
           #t)
          (else #f))
         )))

;;  If nodes "a" and "b" are targets of a node "n", and ("a","b") in direct-simulation relation
;;  Then, label(a) := label(a) AND NOT label(b).
;;
;; Returns #t if the optimization reduced the BA size
(define (ba/apply-direct-simulation-optimizations! agraph sccs)
  (and *sal-ba-expensive-optimizations*
       (let* ((node-vector (slot-value agraph :node-vector))
              (number-of-nodes (vector-length node-vector))
              (direct-simulation? (ba/direct-simulation agraph sccs))
              (context (slot-value agraph :context))
              (optimized? #f))
         (vector/for-each (lambda (anode)
                            (let* ((source (slot-value anode :source))
                                   (transitions (slot-value anode :transition-list)))
                              (for-each (lambda (t1)
                                          (let ((target1 (slot-value t1 :target)))
                                            (for-each (lambda (t2)
                                                        (when (and (not (eq? t1 t2))
                                                                   (direct-simulation? target1 (slot-value t2 :target)))
                                                          (set-slot-value! t1 :label
                                                                           (bdd/and (slot-value t1 :label)
                                                                                    (bdd/not (slot-value t2 :label))))
                                                          (set! optimized? #t)))
                                                      transitions)))
                                        transitions)))
                          node-vector)
         ;;
         ;; Remove a initial state p if there is a state q such that (p,q) in direct simulation relation.
         ;;
         (let ((initial-nodes (slot-value agraph :initial-nodes))
               (new-initial-nodes '()))
           (for-each (lambda (idx1)
                       (unless (exists (lambda (idx2) (and (not (= idx1 idx2)) (direct-simulation? idx1 idx2))) initial-nodes)
                         (push! idx1 new-initial-nodes)))
                     initial-nodes)
           (when (not (= (length initial-nodes) (length new-initial-nodes)))
             (set! optimized? #t))
           (set-slot-value! agraph :initial-nodes new-initial-nodes))
         optimized?)))

;;
;; If (p, q) is in reverse-simulation, then 
;; For each transition t of p with target n, if there is a transition t' of q with target n, then
;;  Label(t) = Label(t) AND NOT Label(t')
;;
;; Returns #t if the optimization reduced the BA size
(define (ba/apply-reverse-simulation-optimizations! agraph sccs)
  (and *sal-ba-expensive-optimizations*
       (let* ((node-vector (slot-value agraph :node-vector))
              (number-of-nodes (vector-length node-vector))
              (reverse-simulation? (ba/reverse-simulation agraph sccs))
              (context (slot-value agraph :context))
              (optimized? #f))
         (do ((idx1 0 (+ idx1 1)))
             ((= idx1 number-of-nodes))
           (let ((node1 (vector-ref node-vector idx1)))
             (do ((idx2 0 (+ idx2 1)))
                 ((= idx2 number-of-nodes))
               (let ((node2 (vector-ref node-vector idx2)))
                 (when (and (not (= idx1 idx2)) (reverse-simulation? idx1 idx2))
                   (for-each (lambda (trans1)
                               (let ((target1 (slot-value trans1 :target)))
                                 (for-each (lambda (trans2)
                                             (when (= target1 (slot-value trans2 :target))
                                               (set-slot-value! trans1 :label
                                                                (bdd/and (slot-value trans1 :label) 
                                                                         (bdd/not (slot-value trans2 :label))))
                                               (set! optimized? #t)))
                                           (slot-value node2 :transition-list))))
                             (slot-value node1 :transition-list)))))))
         optimized?)))

;;
;; Remove initial states that do not contain successors.
(define (ba/remove-initial-states-without-successors! agraph)
  (let ((node-vector (slot-value agraph :node-vector)))
    (set-slot-value! agraph :initial-nodes 
                     (let ((result (filter (lambda (idx) 
                                             (let ((curr-node (vector-ref node-vector idx)))
                                               (not (null? (slot-value curr-node :transition-list)))))
                                           (slot-value agraph :initial-nodes))))
                       ;; I should have at least one initial state, even if it does not contain successors!
                       ;; (print "initial states = " (ba-graph/initial-states agraph))
                       (when (null? result)
                         (set! result (list (car (slot-value agraph :initial-nodes)))))
                       result))))

(define (ba-transtion-list/compress! transition-list bdd-manager)
  (let ((new-list '()))
    (for-each (lambda (t1)
                (let ((merged? (bind-exit (exit)
                                 (for-each (lambda (t2)
                                             (when (eq? (slot-value t1 :target) (slot-value t2 :target))
                                               (set-slot-value! t2 :label (bdd/or (slot-value t2 :label) 
                                                                                  (slot-value t1 :label)))
                                               (exit #t)))
                                           new-list)
                                 #f)))
                  (unless merged?
                    (push! t1 new-list))))
              transition-list)
    new-list))

(define (ba/compress-transitions! agraph)
  (let* ((node-vector (slot-value agraph :node-vector))
         (context (slot-value agraph :context))
         (m (slot-value context :manager)))
    (vector/for-each (lambda (node)
                       (let* ((transition-list (slot-value node :transition-list))
                              (new-transition-list (ba-transtion-list/compress! transition-list m)))
                         (set-slot-value! node :transition-list new-transition-list)))
                     node-vector)))

(define (ba/simplify! agraph)
  (verbose-message 2 "  simplifying BA...")
  (ba/compress-transitions! agraph)
  (let* ((sccs (ba/sccs agraph))
         (fair-sccs (ba/fair-sccs agraph sccs))
         (unfair-sccs (difference sccs fair-sccs))
         (_ (ba/merge-nodes! agraph sccs unfair-sccs))
         (opt1? (ba/combine-weak-reverse-equivalent-nodes! agraph))
         (sccs1 (if opt1? (ba/sccs agraph) sccs))
         (opt2? (ba/join-direct-equivalent-nodes! agraph sccs1))
         (sccs2 (if opt2? (ba/sccs agraph) sccs1))
         (opt3? (ba/apply-direct-simulation-optimizations! agraph sccs2))
         (sccs3 (if opt3? (ba/sccs agraph) sccs2))
         (opt4? (ba/apply-reverse-simulation-optimizations! agraph sccs3))
         (sccs4 (if opt4? (ba/sccs agraph) sccs3))
         (fair-sccs4 (ba/fair-sccs agraph sccs4)))
    (ba/remove-infeasible-transitions! agraph)
    (ba/remove-initial-states-without-successors! agraph)
    ;; final nodes that are not in a SCC are not real final nodes
    (set-slot-value! agraph :final-nodes (filter (lambda (node-idx) (in-scc? sccs4 node-idx)) (slot-value agraph :final-nodes)))
    (set-slot-value! agraph :gba-node-mapping '())
    (ba/remove-nodes-that-cant-reach-fair-scc! agraph fair-sccs4)
    (ba/remove-unreachable-nodes! agraph)))

(define (ltl->ba expr)
  (let* ((vwaa (ltl->vwaa expr))
         (gba1 (vwaa->gba vwaa))
         (gba2 (gba/simplify! gba1))
         (ba (gba->ba gba2)))
    (ba/simplify! ba)))

(define (accept-everything-self-loop? source-idx transition context)
  (and (= (slot-value transition :target) source-idx)
       (bdd/true? (slot-value transition :label))))

(define (ba/empty? graph)
  (let* ((node-vector (slot-value graph :node-vector))
         (n (vector-length node-vector)))
    (or (= n 0)
        (and (= n 1)
             (let* ((node (vector-ref node-vector 0))
                    (trans (slot-value node :transition-list)))
               (null? trans))))))

(define (ba/accepts-everything? graph)
  (let* ((node-vector (slot-value graph :node-vector))
         (n (vector-length node-vector)))
    (and (= n 1)
         (let* ((node (vector-ref node-vector 0))
                (trans (slot-value node :transition-list)))
           (and (= (length trans) 1)
                (accept-everything-self-loop? 0 (car trans) (slot-value graph :context)))))))
       
;; returns #t if the Buchi Automata can be viewed as a regular automata.
;; The following test, checks if there is only one final state, and it contains a self loop labeled true.
(define (ba/regular-automata? ba)
  (let ((final-nodes (slot-value ba :final-nodes)))
    (and (= (length final-nodes) 1)
         (let* ((final-node-idx (car final-nodes))
                (node-vector (slot-value ba :node-vector))
                (final-node (vector-ref node-vector final-node-idx))
                (final-node-transition-list (slot-value final-node :transition-list)))
           (and (= (length final-node-transition-list) 1)
                (accept-everything-self-loop? final-node-idx (car final-node-transition-list) (slot-value ba :context)))))))

;; returns #t if the Buchi Automata is weak.
;; A BA is weak iff:
;;   forall f in final-nodes. T(f,l,f') => f' is final-node.
;; In other words, final states only contain transitions to final states.
(define (ba/weak? ba)
  (let ((final-nodes (slot-value ba :final-nodes))
        (node-vector (slot-value ba :node-vector)))
    (for-all (lambda (final-node-idx)
               (let* ((final-node (vector-ref node-vector final-node-idx))
                      (final-node-transition-list (slot-value final-node :transition-list)))
                 (for-all (lambda (transition)
                            (let ((target (slot-value transition :target)))
                              (memq target final-nodes)))
                          final-node-transition-list)))
             final-nodes)))

;; converts a Buchi Automata in four components
;; - local-variable declaration
;; - initialization predicated expr
;; - transition relation expr
;; - property
;;    a) (G (not final-state))  if safety property
;;    b) (accepting final-state) if liveness property
;;    c) (weak-accepting final-state) if liveness property, and the buchi automata is weak, that is, 
;;         final states only contain transitions to themselves
(define (ba->monitor graph place-provider esm?)
  (let* ((node-vector (slot-value graph :node-vector))
         (num-nodes (vector-length node-vector))
         (pc-var-decl (mk-pc-monitor-decl num-nodes place-provider))
         (pc (make-sal-name-expr pc-var-decl place-provider))
         (next-pc (make-ast-instance <sal-next-operator> place-provider
                                     :name-expr pc))
         (initial-nodes (slot-value graph :initial-nodes))
         (final-nodes (slot-value graph :final-nodes))
         (initial-pred (mk-initial-pred pc initial-nodes place-provider esm?))
         (final-pred (mk-final-pred pc final-nodes place-provider))
         (trans-rel (mk-trans-rel pc next-pc graph place-provider esm?))
         (property (mk-property graph final-pred place-provider)))
    ;; (print "init-pred:") (sal/pp initial-pred) (print "")
    ;; (print "final-pred:") (sal/pp final-pred) (print "")
    ;; (print "trans-rel:") (sal/pp trans-rel) (print "")
    (values (list pc-var-decl)
            initial-pred
            trans-rel
            property)))

(define (mk-pc-monitor-decl num-nodes place-provider)
  (let* ((lower (make-sal-numeral 0 place-provider))
         ;; I need at least two elements in the interval, because:
         ;; - (pc = 0) simplifies to TRUE, if the interval is [0..0]
         ;; in this case, the property will be something like (weak-accepting true)
         ;; the slicer will fail to track the dependencies, and will remove relevant variables.
         (upper-val (if (= num-nodes 1) 1 (- num-nodes 1))) 
         (upper (make-sal-numeral upper-val place-provider))
         (pc-type (make-sal-subrange lower upper place-provider))
         (pc-name (gen-unique-name 'ba-pc))
         (pc-id (make-sal-identifier place-provider pc-name))
         (pc-var-decl (make-ast-instance <sal-local-state-var-decl> place-provider
                                         :id pc-id
                                         :type pc-type)))
    pc-var-decl))

(define (mk-lhs-eq-idx-pred lhs node-idx place-provider)
  (make-sal-equality+ lhs
                      (make-sal-numeral node-idx place-provider)))

(define (mk-lhs-assign-idx lhs node-idx place-provider esm?)
  (let ((num (make-sal-numeral node-idx place-provider)))
    (if esm?
      (make-ast-instance <sal-esm-assignment> place-provider
                         :lhs lhs
                         :rhs num)
      (quick-change-class! (make-sal-equality lhs num)
                           <sal-assignment>))))

(define (mk-initial-pred pc initial-nodes place-provider esm?)
  (let ((assignments (map (lambda (node-idx)
                            (mk-lhs-assign-idx pc node-idx place-provider esm?))
                          initial-nodes)))
    (if esm?
      (and (not (null? initial-nodes))
           (sal-esm/make-esm-choice* assignments place-provider))
      (if (null? initial-nodes)
        (make-sal-false place-provider)
        (apply make-sal-choice+ assignments)))))

(define (mk-trans-rel pc next-pc graph place-provider esm?)
  (let* ((context (slot-value graph :context))
         (node-vector (slot-value graph :node-vector))
         (trans-list (vector/fold 
                      (lambda (trans-list node)
                        (let ((source-idx (slot-value node :source)))
                          (fold-left (lambda (trans-list transition)
                                       (let* ((target-idx (slot-value transition :target))
                                              (label (slot-value transition :label))
                                              (sal-label (bdd->sal-expr label context))
                                              (guard-expr (make-sal-and+
                                                           (mk-lhs-eq-idx-pred pc source-idx place-provider)
                                                           sal-label))
                                              (assignment (mk-lhs-assign-idx next-pc target-idx place-provider esm?))
                                              (trans-expr (if esm?
                                                            (sal-esm/make-esm-seq 
                                                             (make-ast-instance <sal-esm-guard> guard-expr
                                                                                :expr guard-expr)
                                                             assignment
                                                             place-provider)
                                                            (make-sal-and+ guard-expr assignment))))
                                         (cons trans-expr trans-list)))
                                     trans-list
                                     (slot-value node :transition-list))))
                      '()
                      node-vector)))
    (if esm?
      (and (not (null? trans-list))
           (sal-esm/make-esm-choice* trans-list place-provider))
      (let ((trans-rel (if (null? trans-list)
                         (make-sal-false place-provider)
                         (apply make-sal-choice+ trans-list))))
        (sal-ast/cse trans-rel)))))

(define (mk-final-pred pc final-nodes place-provider)
  (if (null? final-nodes)
    (make-sal-false place-provider)
    (apply make-sal-or+ (map (lambda (node-idx)
                               (mk-lhs-eq-idx-pred pc node-idx place-provider))
                             final-nodes))))

(define (mk-property graph final-pred place-provider)
  (cond
   ((ba/regular-automata? graph)
    (make-sal-builtin-application <sal-ltl-g> place-provider
                                  (make-sal-not final-pred)))
   ((ba/weak? graph)
    (make-sal-builtin-application <sal-weak-accepting> place-provider
                                  final-pred))
   (else
    (make-sal-builtin-application <sal-accepting> place-provider
                                  final-pred))))

(define (ba->bool-monitor graph place-provider)
  (multiple-value-bind
      (pc-var-decl-list initial-pred trans-rel property)
      (ba->monitor graph place-provider #f)
    (multiple-value-bind
        (bool-pc-var-decl-list env-data)
        (sal-var-decl-list->sal-bool-var-decl-list pc-var-decl-list gen-unique-name)
      [assert (pc-var-decl-list) (= (length pc-var-decl-list) 1)]
      [assert (pc-var-decl-list) (instance-of? (car pc-var-decl-list) <sal-var-decl>)]
      [assert (bool-pc-var-decl-list) (for-all (cut instance-of? <> <sal-var-decl>) bool-pc-var-decl-list)]
      [assert (env-data) (= (length env-data) 1)]
      (let* ((pc-var-decl (car pc-var-decl-list))
             (env (update-env (used-state-decls->env initial-pred trans-rel property) 
                              pc-var-decl 
                              (car env-data)))
             (pc-type (slot-value pc-var-decl :type))
             (pc (make-sal-name-expr pc-var-decl))
             (valid-bool-pc (sal-type/finite-rep-membership-expr pc-type env pc))
             ;; (_ (breakpoint "bool-ba" (pc-var-decl bool-pc-var-decl-list env 
             ;;                          pc-type pc valid-bool-pc initial-pred trans-rel property) #t))
             (bool-initial-pred (sal-expr->boolean-expr initial-pred env))
             (bool-trans-rel (make-sal-and+ (sal-expr->boolean-expr trans-rel env)
                                            valid-bool-pc))
             (bool-property (sal-expr->boolean-expr property env)))
        (values bool-pc-var-decl-list
                bool-initial-pred
                bool-trans-rel
                bool-property)))))

(define (collect-used-bool-state-decls! ast decl-table)
  (sal-ast/for-each (lambda (ast)
                      (when (and (instance-of? ast <sal-name-expr>)
                                 (instance-of? (slot-value ast :decl) <sal-state-var-decl>)
                                 (sal-type/boolean? (slot-value (slot-value ast :decl) :type)))
                        (eq-hash-table/put! decl-table (slot-value ast :decl) #t)))
                    ast))

(define (used-state-decls->env initial-pred trans-rel property)
  (let ((decl-table (make-eq-hash-table))
        (result-env (make-empty-env)))
    (collect-used-bool-state-decls! initial-pred decl-table)
    (collect-used-bool-state-decls! trans-rel decl-table)
    (collect-used-bool-state-decls! property decl-table)
    (eq-hash-table/for-each-key (lambda (bool-state-decl)
                                  [assert (bool-state-decl) (sal-type/boolean? (slot-value bool-state-decl :type))]
                                  (let* ((name-expr (make-sal-name-expr bool-state-decl))
                                         (env-data (cons (list name-expr)
                                                         (slot-value bool-state-decl :type))))
                                    (set! result-env (update-env result-env
                                                                 bool-state-decl
                                                                 env-data))))
                                decl-table)
    result-env))

;; converts a LTL expression in four components:
;; - local-variable declaration
;; - initialization predicated expr
;; - transition relation expr
;; - property
;;    a) (G (not final-state))  if safety property
;;    b) (accepting final-state) if liveness property
;;    c) (weak-accepting final-state) if liveness property, and the buchi automata is weak, that is, 
;;         final states only contain transitions to themselves
;; The first three components define a monitor for the property.
(define (ltl->monitor expr . svsv) 
  (let ((bool-ba? (svsv/value svsv :bool? #f))
        (esm? (svsv/value svsv :esm? #f))
        (ba (ltl->ba expr)))
    [assert (bool-ba? esm?) (not (and bool-ba? esm?))]
    ;; (ba/display ba)
    (cond
     ((ba/accepts-everything? ba)
      ;; property is trivially false (everything is a bug)
      (values '() (make-sal-true expr) (make-sal-true expr) (make-sal-builtin-application <sal-ltl-g> expr
                                                                                         (make-sal-false expr))))
     ((ba/empty? ba)
      ;; property is trivially true
      (values '() (make-sal-true expr) (make-sal-true expr) (make-sal-builtin-application <sal-ltl-g> expr
                                                                                         (make-sal-true expr))))
     (else
      (verbose-message 2 "  number of states in the BA: ~a" (vector-length (slot-value ba :node-vector)))
      (if bool-ba?
        (ba->bool-monitor ba expr)
        (ba->monitor ba expr esm?))))))

(define (ltl->dot expr)
  (let ((ba (ltl->ba expr)))
    (ba->dot ba)))
        

