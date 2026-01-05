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

;;=========================================================
;; CTL model checking with generation of symbolic evidence
;; and interactive extraction of explicit witnesses and
;; counterexamples as paths.
;;---------------------------------------------------------
;; March 1, 2004
;;=========================================================

(module sal-wmc-core
  (include "sal.sch")
  (include "fast-hash-table.sch")
  (import bdd sal-bdd-fsm sal-bdd-cluster sal-bdd-context front-end sal-flat-modules
    symbol-table sal-expression sal-path queue sal-pp runtime sal2bdd
    sal-path-pp sal-derived-path sal-nnf sal-ast-env sal-ast-expand sal-ast-simplify
    sal-smc-core sal-assertion)
  (export (sal-wmc/ctl-check fsm expr)
    (sal-wmc/display-trace wmc-object)
    (sal-wmc/display-trace-starting-at wmc-object state)
    (sal-wmc/get-initial-bdd wmc-object)
    <wmc-witness>
    <ag-wmc-witness>
    <af-wmc-witness>
    <ax-wmc-witness>
    <au-wmc-witness>
    <ar-wmc-witness>
    <eg-wmc-witness>
    <ef-wmc-witness>
    <ex-wmc-witness>
    <eu-wmc-witness>
    <er-wmc-witness>
    <atom-wmc-witness>
    <and-wmc-witness>
    <or-wmc-witness>
    <wmc-counterexample>
    <ag-wmc-counterexample>
    <af-wmc-counterexample>
    <ax-wmc-counterexample>
    <au-wmc-counterexample>
    <ar-wmc-counterexample>
    <eg-wmc-counterexample>
    <ef-wmc-counterexample>
    <ex-wmc-counterexample>
    <eu-wmc-counterexample>
    <er-wmc-counterexample>
    <atom-wmc-counterexample>
    <and-wmc-counterexample>
    <or-wmc-counterexample>)
  )

(define (pre-image fsm set-of-states)
  ;; Leonardo's Remark:
  ;; Choice variables are like input variables... so we must project them, otherwise we will have a 
  ;; semantics that is not compatible with the user expectation.
  ;; BTW, input variables produce a similar problem. We have to talk with Shankar about this issue. 
  ;; Projection is not a good option for input variables, since the user would not be allowed to write
  ;; properties using input variables.
  ;; Maybe we should consider iCTL. iCTL is an extension of CTL which has the operators:
  ;; - forall_i  (for all input configurations)
  ;; - exists_i   (exists an input configuration)
  ;; Example: (exists_i (EX p))
  (sal-bdd-fsm/pre-image fsm set-of-states))

(define-generic (sal-wmc fsm expr init))

(define-class <wmc-evidence> () (:fp
         :expr
         :in
         :bad
         :sub1
         :sub2))

(define-class <ag-wmc-evidence> (<wmc-evidence>) ())
(define-class <ax-wmc-evidence> (<wmc-evidence>) ())
(define-class <af-wmc-evidence> (<wmc-evidence>) ())
(define-class <au-wmc-evidence> (<wmc-evidence>) ())
(define-class <ar-wmc-evidence> (<wmc-evidence>) ())
(define-class <eg-wmc-evidence> (<wmc-evidence>) ())
(define-class <ex-wmc-evidence> (<wmc-evidence>) ())
(define-class <ef-wmc-evidence> (<wmc-evidence>) ())
(define-class <eu-wmc-evidence> (<wmc-evidence>) ())
(define-class <er-wmc-evidence> (<wmc-evidence>) ())
(define-class <atom-wmc-evidence> (<wmc-evidence>) ())
(define-class <and-wmc-evidence> (<wmc-evidence>) ())
(define-class <or-wmc-evidence> (<wmc-evidence>) ())

(define-class <wmc-witness-or-counterexample> () (:bdd-list :fsm :expr))       

(define-class <wmc-witness> (<wmc-witness-or-counterexample>) ())

(define-class <unary-wmc-witness> (<wmc-witness>) (:sub))
(define-class <binary-wmc-witness> (<wmc-witness>) (:valid1 :valid2 :sub1 :sub2))

(define-class <ag-wmc-witness> (<unary-wmc-witness>) ())
(define-class <af-wmc-witness> (<unary-wmc-witness>) ())
(define-class <ax-wmc-witness> (<unary-wmc-witness>) ())

(define-class <au-wmc-witness> (<binary-wmc-witness>) ())
(define-class <ar-wmc-witness> (<binary-wmc-witness>) ())
(define-class <eg-wmc-witness> (<unary-wmc-witness>) ())
(define-class <ef-wmc-witness> (<unary-wmc-witness>) ())
(define-class <ex-wmc-witness> (<unary-wmc-witness>) ())
(define-class <eu-wmc-witness> (<binary-wmc-witness>) ())
(define-class <er-wmc-witness> (<binary-wmc-witness>) ())
(define-class <atom-wmc-witness> (<wmc-witness>) ())
(define-class <and-wmc-witness> (<binary-wmc-witness>) ())
(define-class <or-wmc-witness> (<binary-wmc-witness>) ())

(define-class <wmc-counterexample> (<wmc-witness-or-counterexample>) ())
(define-class <unary-wmc-counterexample> (<wmc-counterexample>) (:sub))
(define-class <binary-wmc-counterexample> (<wmc-counterexample>) (:valid1 :valid2 :sub1 :sub2))

(define-class <ag-wmc-counterexample> (<unary-wmc-counterexample>) ())
(define-class <af-wmc-counterexample> (<unary-wmc-counterexample>) ())
(define-class <ax-wmc-counterexample> (<unary-wmc-counterexample>) ())
(define-class <au-wmc-counterexample> (<binary-wmc-counterexample>) ())
(define-class <ar-wmc-counterexample> (<binary-wmc-counterexample>) ())
(define-class <eg-wmc-counterexample> (<unary-wmc-counterexample>) ())
(define-class <ef-wmc-counterexample> (<unary-wmc-counterexample>) ())
(define-class <ex-wmc-counterexample> (<unary-wmc-counterexample>) ())
(define-class <eu-wmc-counterexample> (<binary-wmc-counterexample>) ())
(define-class <er-wmc-counterexample> (<binary-wmc-counterexample>) ())
(define-class <atom-wmc-counterexample> (<wmc-counterexample>) ())
(define-class <and-wmc-counterexample> (<binary-wmc-counterexample>) ())
(define-class <or-wmc-counterexample> (<binary-wmc-counterexample>) ())

(define-generic (temporal? w-or-ce))
(define-method (temporal? (w-or-ce <wmc-witness-or-counterexample>)) #t)
(define-method (temporal? (w-or-ce <and-wmc-witness>)) #f)
(define-method (temporal? (w-or-ce <or-wmc-witness>)) #f)
(define-method (temporal? (w-or-ce <atom-wmc-witness>)) #f)
(define-method (temporal? (w-or-ce <and-wmc-counterexample>)) #f)
(define-method (temporal? (w-or-ce <or-wmc-counterexample>)) #f)
(define-method (temporal? (w-or-ce <atom-wmc-counterexample>)) #f)

(define-generic (wmc-for-each w-or-ce proc))
(define-method (wmc-for-each (w-or-ce <wmc-witness-or-counterexample>) (proc <primitive>))
  (proc w-or-ce))
(define-method (wmc-for-each (w-or-ce <unary-wmc-witness>) (proc <primitive>))
;;  (breakpoint "wmc-for-each: <unary-wmc-witness>" (w-or-ce) #t)
  (proc w-or-ce)
  (wmc-for-each (car (slot-value w-or-ce :sub)) proc))
(define-method (wmc-for-each (w-or-ce <binary-wmc-witness>) (proc <primitive>))
;;  (breakpoint "wmc-for-each: <binary-wmc-witness>" (proc w-or-ce) #t)
  (proc w-or-ce)
  (wmc-for-each (car (slot-value w-or-ce :sub1)) proc) ;;;;
  (wmc-for-each (car (slot-value w-or-ce :sub2)) proc))
(define-method (wmc-for-each (w-or-ce <unary-wmc-counterexample>) (proc <primitive>))
;;  (breakpoint "wmc-for-each: <unary-wmc-counterexample>" (w-or-ce) #t)
  (proc w-or-ce)
  (wmc-for-each (car (slot-value w-or-ce :sub)) proc))
(define-method (wmc-for-each (w-or-ce <binary-wmc-counterexample>) (proc <primitive>))
;;  (breakpoint "wmc-for-each: <binary-wmc-counterexample>" (w-or-ce) #t)
  (proc w-or-ce)
  (wmc-for-each (car (slot-value w-or-ce :sub1)) proc)
  (wmc-for-each (car (slot-value w-or-ce :sub2)) proc))

(define (contains-temporal? w-or-ce)
;;(breakpoint "contains-temporal?" (w-or-ce wmc-for-each temporal?) #t)
  (bind-exit (exit)
       (wmc-for-each w-or-ce
         (lambda (sub-w-or-ce)
           (when (temporal? sub-w-or-ce)
           (exit #t))))
       #f))

;; sal-wmc/ctl-check 
;; main function to invoke the model checker: 
;; calls sal-wmc/wmc to build witnesses and
;; counterexamples. Input is 
;; [fsm]  - transition system
;; [expr] - CTL formula

(define *wmc-atom-cache-table* #unspecified)

(define (wmc-initialize!)
  (set! *wmc-atom-cache-table* (make-eq-hash-table)))

(define (wmc-finalize!)
  (set! *wmc-atom-cache-table* #unspecified))

(define (sal-wmc/ctl-check fsm expr)
  (wmc-initialize!)
  (unwind-protect
   (let* ((tmp-expr (sal-ast/simplify (sal-ast/expand-quantifiers (sal-ast/expand-names expr (make-empty-env)))))
    (tmp-expr2 (sal-ast/expand-applications 
          tmp-expr
          (make-empty-env) 
          (lambda (app)
      (instance-of? app <sal-ctl-application>))))
    (tmp-expr3 (sal-expr->nnf tmp-expr2)))
     (let ((evidence (display-runtime 1 "verification time: ~a secs"
              (lambda ()
          (sal-wmc fsm tmp-expr3 (sal-bdd-fsm/initial-states fsm))))))
       (cond 
  ((instance-of? evidence <wmc-evidence>)
;  (breakpoint "sal-wmc/ctl-check" (evidence fsm tmp-expr3) #t) 
   (let ((valid? (bdd/false? (car (slot-value evidence :bad)))))  
     ;; was: ((valid? (for-all bdd/false? (slot-value evidence :bad))))
     (cond (valid?
      (print "The formula is VALID. ")
      (sal-wmc/symbolic-witness evidence fsm))
     (else (print "The formula is INVALID")
           (sal-wmc/symbolic-counterexample evidence fsm)))))
  (else
   ;;(breakpoint "starting ctl-check" (fsm tmp-expr3) #t)
   (sign-error "Feature not yet implemented.")))))
   (wmc-finalize!)))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ltl-application>) (init <bdd>))
  (sign-unsupported-feature expr "sal-wmc does not support LTL properties. You should use sal-smc."))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-ag>) (init <bdd>))
  (let* ((place-info (format-with-location expr ""))
   (result (sal-wmc/AG fsm (slot-value expr :arg) init place-info)))
    (set-slot-value! result :expr expr)
    result))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-af>) (init <bdd>))
  (let* ((place-info (format-with-location expr ""))
   (result (sal-wmc/AF fsm (slot-value expr :arg) init place-info)))
    (set-slot-value! result :expr expr)
    result))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-ax>) (init <bdd>))
  (let* ((place-info (format-with-location expr ""))
   (result (sal-wmc/AX fsm (slot-value expr :arg) init place-info)))
    (set-slot-value! result :expr expr)
    result))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-au>) (init <bdd>))
  (let ((place-info (format-with-location expr "")))
    (multiple-value-bind
     (arg1 arg2)
     (sal-binary-application/arguments expr)
     (let ((result (sal-wmc/AU fsm arg1 arg2 init place-info)))
       (set-slot-value! result :expr expr)
       result))))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-ar>) (init <bdd>))
  (let ((place-info (format-with-location expr "")))
    (multiple-value-bind
     (arg1 arg2)
     (sal-binary-application/arguments expr)
     (let ((result (sal-wmc/AR fsm arg1 arg2 init place-info)))
       (set-slot-value! result :expr expr)
       result))))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-eg>) (init <bdd>))
  (let* ((place-info (format-with-location expr ""))
   (result (sal-wmc/EG fsm (slot-value expr :arg) init place-info)))
    (set-slot-value! result :expr expr)
    result))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-ef>) (init <bdd>))
  (let* ((place-info (format-with-location expr ""))
   (result (sal-wmc/EF fsm (slot-value expr :arg) init place-info)))
    (set-slot-value! result :expr expr)
    result))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-ex>) (init <bdd>))
  (let* ((place-info (format-with-location expr ""))
   (result (sal-wmc/EX fsm (slot-value expr :arg) init place-info)))
    (set-slot-value! result :expr expr)
    result))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-eu>) (init <bdd>))
  (let ((place-info (format-with-location expr "")))
    (multiple-value-bind
     (arg1 arg2)
     (sal-binary-application/arguments expr)
     (let ((result (sal-wmc/EU fsm arg1 arg2 init place-info)))
       (set-slot-value! result :expr expr)
       result))))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-ctl-er>) (init <bdd>))
  (let ((place-info (format-with-location expr "")))
    (multiple-value-bind
     (arg1 arg2)
     (sal-binary-application/arguments expr)
      (let ((result (sal-wmc/ER fsm arg1 arg2 init place-info)))
  (set-slot-value! result :expr expr)
  result))))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-and>) (init <bdd>))
  (if (sal-expr/contains-temporal-operators? expr)
      (let ((result (sal-wmc/and* fsm (sal-application/argument-list expr) init)))
; (breakpoint "sal-expr/contains-temporal-operators?" (fsm expr init result) #t)
  (set-slot-value! result :expr expr)
  result)
      (sal-wmc/atom fsm expr init)))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-or>) (init <bdd>))
  (if (sal-expr/contains-temporal-operators? expr)
      (let ((result (sal-wmc/or* fsm (sal-application/argument-list expr) init)))
  (set-slot-value! result :expr expr)
; (breakpoint "sal-wmc: or" (fsm expr init result) #t)
  result)
      (sal-wmc/atom fsm expr init)))

(define-method (sal-wmc (fsm <sal-bdd-fsm>) (expr <sal-expr>) (init <bdd>))
  (sal-wmc/atom fsm expr init))

(define (sal-wmc/atom fsm expr init)
  (let ((expr-bdd  (cond 
        ((eq-hash-table/get *wmc-atom-cache-table* expr) =>
         cdr)
        (else 
         (let ((expr-bdd (sal-expr->bdd-core expr (make-empty-env) fsm)))
           (eq-hash-table/put! *wmc-atom-cache-table* expr expr-bdd)
           expr-bdd)))))
    (make-instance <atom-wmc-evidence>
       :fp  '()
       :expr expr
       :in  (list init)
       :bad (list (bdd/diff init expr-bdd))
       :sub1 '()
       :sub2 '())))

(define (sal-wmc/AG fsm expr init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/AG* fsm init expr (bdd/false bdd-manager) (bdd/false bdd-manager) '() '() place-info 1 collect!)))

(define (sal-wmc/AG* fsm init phi V V+ instates subwitnesses place-info it collect!)
  (let* ((phi-witness (sal-wmc fsm phi init))
   (init-phi (bdd/diff init (car (slot-value phi-witness :bad))))
   (new-init-phi (bdd/diff init-phi V))
   (instates (cons init instates))
   (V+ (bdd/or V+ new-init-phi)))
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (AG at ~a)" it place-info)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size new-init-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
          ;    (collect!)
    ;; (print "AG* init: ")
    ;; (sal-bdd-fsm/display-states-without-choices fsm init 100 #f)
    ;; (print "------------------")
    (if (bdd/false? new-init-phi)
  (let* ((w-fp (nupre-list~* fsm V+))
         (innegstates (map (lambda (state) (bdd/diff state (car w-fp))) instates)))
    ;; (print "V+: ")
    ;; (sal-bdd-fsm/display-states-without-choices fsm V+ 100 #f)
    ;; (print "---------------------")
    ;; (breakpoint "sal-wmc/AG*" (w-fp innegstates fsm V V+ instates) #t)
    (make-instance <ag-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi-witness subwitnesses)
       :sub2 '()))
  (sal-wmc/AG* fsm 
         (ctl-image fsm new-init-phi) 
         phi
         (bdd/or V init)
         V+
         instates
         (cons phi-witness subwitnesses)
         place-info (+ it 1) collect!))))

(define (sal-wmc/AF fsm expr init place-info)
  ;; (breakpoint "sal-wmc/AF" (fsm init) #t)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/AF* fsm init expr (bdd/false bdd-manager) (bdd/false bdd-manager) '() '() place-info 1 collect!)))

(define (sal-wmc/AF* fsm init phi V V- instates subwitnesses place-info it collect!)
  (let* ((phi-witness (sal-wmc fsm phi init))
   (init-not-phi (car (slot-value phi-witness :bad)))
   (new-init-not-phi (bdd/diff init-not-phi V))
   (instates (cons init instates))
   (V- (bdd/or V- new-init-not-phi)))
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (AF at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-not-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (if (bdd/false? new-init-not-phi)
  (let* ((w-fp (nupre-list* fsm V-))
         (innegstates (map (lambda (state) (bdd/and state (car w-fp))) instates)))
    ;; (breakpoint "sal-wmc/AF*" (w-fp init phi V V- fsm innegstates instates) #t)
    (make-instance <af-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi-witness subwitnesses)
       :sub2 '()))
  (sal-wmc/AF* fsm 
         (ctl-image fsm new-init-not-phi) 
         phi
         (bdd/or V init)
         V-
         instates
         (cons phi-witness subwitnesses)
         place-info (+ it 1) collect!))))

(define (sal-wmc/AU fsm expr1 expr2 init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/AU* fsm init expr1 expr2 (bdd/false bdd-manager) (bdd/false bdd-manager) (bdd/false bdd-manager) 
     '() '() '() '() place-info 1 collect!)))

(define (sal-wmc/AU* fsm init phi1 phi2 V V-not-12 V-not-2 instates n-instates subwitnesses1 subwitnesses2 place-info it collect!)
  (let* ((phi2-witness (sal-wmc fsm phi2 init))
   (init-not-phi2 (car (slot-value phi2-witness :bad)))
   (phi1-witness (sal-wmc fsm phi1 init-not-phi2))
   (init-not-phi1 (car (slot-value phi1-witness :bad)))
   (init-eu (bdd/diff init-not-phi2 init-not-phi1)); not phi2 and phi1
   (new-init-phi (bdd/diff init-eu V)); new (not phi2 and phi1) states
   (instates (cons init instates))
   (n-instates (cons init-not-phi2 n-instates))
   (V-not-2 (bdd/or V-not-2 init-not-phi2)) ;; not phi2
   (V-not-12 (bdd/or V-not-12 init-not-phi1))) ;; not phi1 and not phi2
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (AU at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (if (bdd/false? new-init-phi)
  (let* ((w-fp (union-nupre-list* fsm V-not-2 V-not-12))
         (innegstates (map (lambda (state) (bdd/and state (car w-fp))) n-instates)))
    (make-instance <au-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi1-witness subwitnesses1)
       :sub2 (cons phi2-witness subwitnesses2)))
  (sal-wmc/AU* fsm 
         (ctl-image fsm new-init-phi) 
         phi1
         phi2
         (bdd/or V init)
         V-not-12
         V-not-2
         instates
         n-instates
         (cons phi1-witness subwitnesses1)
         (cons phi1-witness subwitnesses1)
         place-info (+ it 1) collect!))))

(define (sal-wmc/AR fsm expr1 expr2 init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/AR* fsm init expr1 expr2 (bdd/false bdd-manager) (bdd/false bdd-manager) (bdd/false bdd-manager) 
     '() '() '() '() place-info 1 collect!)))

(define (sal-wmc/AR* fsm init phi1 phi2 V Vphi2 Vphi21 instates n-instates subwitnesses1 subwitnesses2 place-info it collect!)
  (let* ((phi2-witness (sal-wmc fsm phi2 init))
   (init-phi2 (bdd/diff init (car (slot-value phi2-witness :bad))))
   (phi1-witness (sal-wmc fsm phi1 init-phi2))
   (init-not-phi1 (car (slot-value phi1-witness :bad)))
   (new-init-phi (bdd/diff init-not-phi1 V))
   (instates (cons init instates))
   (n-instates (cons init-phi2 n-instates))
   (Vphi2 (bdd/or Vphi2 init-phi2))
   (Vphi21 (bdd/or Vphi21 new-init-phi)))
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (AR at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (if (bdd/false? new-init-phi)
  (let* ((w-fp (union-nupre-list~* fsm Vphi2 Vphi21))
         (innegstates (map (lambda (state) (bdd/diff state (car w-fp))) n-instates)))
    (make-instance <ar-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi1-witness subwitnesses1)
       :sub2 (cons phi2-witness subwitnesses2)))
  (sal-wmc/AR* fsm 
         (ctl-image fsm new-init-phi) 
         phi1
         phi2
         (bdd/or V init)
         Vphi2
         Vphi21
         instates
         n-instates
         (cons phi1-witness subwitnesses1)
         (cons phi1-witness subwitnesses1)
         place-info (+ it 1) collect!))))

(define (sal-wmc/AX fsm expr init place-info)
  (let ((bdd-manager (sal-bdd-fsm/manager fsm)))
    (sal-wmc/AX* fsm init expr '() place-info 1)))

(define (sal-wmc/AX* fsm init phi subwitnesses place-info it)
  ;; (print "calling... AX*")
  (let* ((next-init (ctl-image fsm init))
   (phi-witness (sal-wmc fsm phi next-init))
   (not-phi (car (slot-value phi-witness :bad)))
   (not-phi-pre-image (pre-image fsm not-phi))
   (w (bdd/or not-phi-pre-image not-phi)))
    (make-instance <ax-wmc-evidence>
       :fp w
       :in (cons next-init (list init)) ;(cons (bdd/diff (ctl-image fsm init) not-phi) (list init))
       :bad (list (bdd/and init not-phi-pre-image) (bdd/and next-init not-phi)) ;(list (bdd/and init w))
       :sub1 (cons phi-witness subwitnesses)
       :sub2 '())))

(define (sal-wmc/EG fsm expr init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/EG* fsm init expr (bdd/false bdd-manager) (bdd/false bdd-manager) '() '() place-info 1 collect!)))

(define (sal-wmc/EG* fsm init phi V V+ instates subwitnesses place-info it collect!)
  (let* ((phi-witness (sal-wmc fsm phi init))
   (init-phi (bdd/diff init (car (slot-value phi-witness :bad))))
   (new-init-phi (bdd/diff init-phi V))       ; I'
   (instates (cons init instates))
   (V+ (bdd/or V+ new-init-phi)))
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (EG at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
;    (print "EG* init: ")
;    (sal-bdd-fsm/display-states-without-choices fsm init 100 #f)
;    (print "------------------")
;    (print "init-phi: ")
;    (sal-bdd-fsm/display-states-without-choices fsm init-phi 100 #f)
;    (print "------------------")
;    (print "new-init-phi: ")
;    (sal-bdd-fsm/display-states-without-choices fsm new-init-phi 100 #f)
;    (print "------------------")
    (if (bdd/false? new-init-phi)
  (let* ((w-fp (nupre-list* fsm V+))
         (innegstates (map (lambda (state) (bdd/diff state (car w-fp))) instates)))
;(print "V+: ")
;(sal-bdd-fsm/display-states-without-choices fsm V+ 100 #f)
;(print "---------------------")
;(print "w-fp: ")
;(sal-bdd-fsm/display-states-without-choices fsm (car w-fp) 100 #f)
;(print "---------------------")
;(print "innegstates: ")
;(sal-bdd-fsm/display-states-without-choices fsm (car (reverse innegstates)) 100 #f)
;(print "---------------------")
    (make-instance <eg-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi-witness subwitnesses)
       :sub2 '()))
  (sal-wmc/EG* fsm 
         (ctl-image fsm new-init-phi) 
         phi
         (bdd/or V init)
         V+
         instates
         (cons phi-witness subwitnesses)
         place-info (+ it 1) collect!))))

(define (sal-wmc/EF fsm expr init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
     (sal-wmc/EF* fsm init expr (bdd/false bdd-manager) (bdd/false bdd-manager) '() '() place-info 1 collect!)))

(define (sal-wmc/EF* fsm init phi V V- instates subwitnesses place-info it collect!)
  (let* ((phi-witness (sal-wmc fsm phi init))
   (init-not-phi (car (slot-value phi-witness :bad)))
   (new-init-not-phi (bdd/diff init-not-phi V))
   (instates (cons init instates))
   (V- (bdd/or V- new-init-not-phi)))
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (EF at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-not-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (if (bdd/false? new-init-not-phi)
  (let* ((w-fp (nupre-list~* fsm V-))
         (innegstates (map (lambda (state) (bdd/and state (car w-fp))) instates)))
    (make-instance <ef-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi-witness subwitnesses)
       :sub2 '()))
  (sal-wmc/EF* fsm 
         (ctl-image fsm new-init-not-phi) 
         phi
         (bdd/or V init)
         V-
         instates
         (cons phi-witness subwitnesses)
         place-info (+ it 1) collect!))))

(define (sal-wmc/EU fsm expr1 expr2 init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/EU* fsm init expr1 expr2 (bdd/false bdd-manager) (bdd/false bdd-manager) (bdd/false bdd-manager) 
     '() '() '() '() place-info 1 collect!)))

(define (sal-wmc/EU* fsm init phi1 phi2 V V-not-12 V-not-2 instates n-instates subwitnesses1 subwitnesses2 place-info it collect!)
  (let* ((phi2-witness (sal-wmc fsm phi2 init))
   (init-not-phi2 (car (slot-value phi2-witness :bad)))
   (phi1-witness (sal-wmc fsm phi1 init-not-phi2))
   (init-not-phi1 (car (slot-value phi1-witness :bad)))
   (init-eu (bdd/diff init-not-phi2 init-not-phi1))
   (new-init-phi (bdd/diff init-eu V))
   (instates (cons init instates))
   (n-instates (cons init-not-phi2 n-instates))
   (V-not-2 (bdd/or V-not-2 init-not-phi2))
   (V-not-12 (bdd/or V-not-12 init-not-phi1)))
    (verbose-message 3 "  Forward iteration ~a (EU at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (collect!)
    (if (bdd/false? new-init-phi)
  (let* ((w-fp (union-nupre-list~* fsm V-not-2 V-not-12))
         (innegstates (map (lambda (state) (bdd/and state (car w-fp))) n-instates)))
    (make-instance <eu-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi1-witness subwitnesses1)
       :sub2 (cons phi2-witness subwitnesses2)))
  (sal-wmc/EU* fsm 
         (ctl-image fsm new-init-phi) 
         phi1
                     phi2
         (bdd/or V init)
         V-not-12
                     V-not-2
         instates
         n-instates
         (cons phi1-witness subwitnesses1)
         (cons phi1-witness subwitnesses1)
         place-info (+ it 1) collect!))))

(define (sal-wmc/ER fsm expr1 expr2 init place-info)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (collect! (make-gc-collector-for bdd-manager)))
    (sal-wmc/ER* fsm init expr1 expr2 (bdd/false bdd-manager) (bdd/false bdd-manager) (bdd/false bdd-manager) 
     '() '() '() '() place-info 1 collect!)))

(define (sal-wmc/ER* fsm init phi1 phi2 V Vphi2 Vphi21 instates n-instates subwitnesses1 subwitnesses2 place-info it collect!)
  (let* ((phi2-witness (sal-wmc fsm phi2 init))
   (init-phi2 (bdd/diff init (car (slot-value phi2-witness :bad))))
   (phi1-witness (sal-wmc fsm phi1 init-phi2))
   (init-not-phi1 (car (slot-value phi1-witness :bad)))
   (new-init-phi (bdd/diff init-not-phi1 V))
   (instates (cons init instates))
   (n-instates (cons init-phi2 n-instates))
   (Vphi2 (bdd/or Vphi2 init-phi2))
   (Vphi21 (bdd/or Vphi21 new-init-phi)))
    (collect!)
    (verbose-message 3 "  Forward iteration ~a (ER at ~a)" it place-info)
    (verbose-message 3 "    frontier: ~a nodes" (bdd/size new-init-phi))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (if (bdd/false? new-init-phi)
  (let* ((w-fp (union-nupre-list* fsm Vphi2 Vphi21))
         (innegstates (map (lambda (state) (bdd/diff state (car w-fp))) n-instates)))
    (make-instance <er-wmc-evidence>
       :fp w-fp
       :in (reverse instates)
       :bad (reverse innegstates)
       :sub1 (cons phi1-witness subwitnesses1)
       :sub2 (cons phi2-witness subwitnesses2)))
  (sal-wmc/ER* fsm 
         (ctl-image fsm new-init-phi) 
         phi1
                     phi2
         (bdd/or V init)
         Vphi2
                     Vphi21
         instates
         n-instates
         (cons phi1-witness subwitnesses1)
         (cons phi1-witness subwitnesses1)
         place-info (+ it 1) collect!))))

(define (sal-wmc/EX fsm expr init place-info)
  (let ((bdd-manager (sal-bdd-fsm/manager fsm)))
    (sal-wmc/EX* fsm init expr '() place-info 1)))

(define (sal-wmc/EX* fsm init phi subwitnesses place-info it)
  (let* ((next-init (ctl-image fsm init))
   (phi-witness (sal-wmc fsm phi next-init))
   (not-phi (car (slot-value phi-witness :bad)))
   (not-phi-wp (ctl-wp fsm not-phi))
   ;; (V (bdd/or init next-init))
   (w (bdd/or not-phi-wp not-phi)))
    ;(breakpoint "sal-wmc/EX*" (phi-witness not-phi V w fsm next-init init) #t)
    (make-instance <ex-wmc-evidence>
       :fp w
       :in (cons next-init (list init)) ;(cons (bdd/diff (ctl-image fsm init) not-phi) (list init))
       :bad (list (bdd/and init not-phi-wp) (bdd/and next-init not-phi))
       :sub1 (cons phi-witness subwitnesses)
       :sub2 '())))

(define (sal-wmc/prop* fsm expr-list init bdd-op wit-class mk-proc)
  (let loop ((expr (car expr-list))
       (expr-list (cdr expr-list)))
    (let ((wit1 (sal-wmc fsm expr init)))
;       (display "OR component expr: " ) (sal/pp expr) (print "")
;       (print "Bad states:")
;       (sal-bdd-fsm/display-states-without-choices fsm (car (slot-value wit1 :bad)) 100 #f)
;       (print "==========================")
      
      (if (null? expr-list)
  wit1
  (let ((wit2 (loop (car expr-list) (cdr expr-list))))
    (unless (null? (cdr expr-list))
      (set-slot-value! wit2 :expr (apply mk-proc expr-list)))   
    (make-instance wit-class
       :fp '()
       :in (list init)
       :bad (list (bdd-op (car (slot-value wit1 :bad))
              (car (slot-value wit2 :bad))))
       :sub1 (list wit1)
       :sub2 (list wit2)))))))
  
(define (sal-wmc/and* fsm expr-list init)
  (sal-wmc/prop* fsm expr-list init bdd/or <and-wmc-evidence> make-sal-and))

(define (sal-wmc/or* fsm expr-list init)
  ;; (print "calling sal-wmc/or*")
  (sal-wmc/prop* fsm expr-list init bdd/and <or-wmc-evidence> make-sal-or))

(define (nupre* fsm start)
  (let loop ((it 1)
       (start start))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (let ((new-start (bdd/and (pre-image fsm start) start)))
      (if (bdd/eq? start new-start)
    start
    (loop (+ it 1) new-start)))))

(define (nupre-list* fsm start)
  (let loop ((it 1)
       (curr-list (list start)))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (let* ((curr (car curr-list))
     (new-curr (bdd/and (pre-image fsm curr) curr))
     (new-curr-list (cons new-curr curr-list)))
      (if (bdd/eq? curr new-curr)
    curr-list
    (loop (+ it 1) new-curr-list)))))

(define (complement fsm bdd)
  (sal-bdd-fsm/filter-invalid-state-rep fsm (bdd/not bdd)))

(define (ctl-wp fsm fmla)
  (complement fsm (pre-image fsm (complement fsm fmla))))

(define (ctl-image fsm set-of-states)
  (let ((manager (sal-bdd-fsm/manager fsm)))
    (sal-bdd-fsm/image-core fsm set-of-states set-of-states (bdd/true manager))))

(define (nupre-list~* fsm start)
  (let loop ((it 1)
       (curr-list (list start)))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (let* ((curr (car curr-list))
     (new-curr (bdd/and (ctl-wp fsm curr) curr))
     (new-curr-list (cons new-curr curr-list)))
      (if (bdd/eq? curr new-curr)
    curr-list
    (loop (+ it 1) new-curr-list)))))

(define (union-nupre-list~* fsm start set)
  (let loop ((it 1)
       (curr-list (list start)))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (let* ((curr (car curr-list))
     (new-curr (bdd/and (bdd/or (ctl-wp fsm curr) set) curr))
     (new-curr-list (cons new-curr curr-list)))
      (if (bdd/eq? curr new-curr)
    curr-list
    (loop (+ it 1) new-curr-list)))))

(define (union-nupre~* fsm start set)
  (let loop ((it 1)
       (start start))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (let ((new-start (bdd/and (bdd/or (ctl-wp fsm start) set) start)))
      (if (bdd/eq? start new-start)
    start
    (loop (+ it 1) new-start)))))

;; nu X . start /\ ( set \/ pre(X))
(define (union-nupre-list* fsm start set)
  (let loop ((it 1)
       (curr-list (list start)))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (let* ((curr (car curr-list))
     (new-curr (bdd/and (bdd/or (pre-image fsm curr) set) curr))
     (new-curr-list (cons new-curr curr-list)))
      (if (bdd/eq? curr new-curr)
    curr-list
    (loop (+ it 1) new-curr-list)))))

(define (union-nupre* fsm start set)
  (let loop ((it 1)
       (start start))
    (verbose-message 3 "  Backward iteration ~a" it)
    (verbose-message 3 "    bdd size: ~a nodes" (bdd/size start))
    (verbose-message 3 "    total bdd nodes: ~a" (bdd/num-nodes (sal-bdd-fsm/manager fsm)))
    (let ((new-start (bdd/and (bdd/or (pre-image fsm start) set) start)))
      (if (bdd/eq? start new-start)
    start
    (loop (+ it 1) new-start)))))

;;===================================
;; construction of symbolic witnesses
;;===================================

(define-generic (sal-wmc/symbolic-witness evidence fsm))

;;---------------------------
;; symbolic witness for atom
;;---------------------------

(define-method (sal-wmc/symbolic-witness (evidence <wmc-evidence>) (fsm <sal-bdd-fsm>))
;(breakpoint "sal-wmc/symbolic-witness->atom" (evidence fsm) #t)
  (make-instance <atom-wmc-witness>
     :fsm fsm
     :expr (slot-value evidence :expr)
     :bdd-list '()))

;;----------------------------
;; symbolic witness for AND/OR
;;----------------------------

(define-method (sal-wmc/symbolic-witness (evidence <and-wmc-evidence>) (fsm <sal-bdd-fsm>))
 ; (breakpoint "sal-wmc/symbolic-witness-->AND" (evidence fsm) #t)
  (symbolic-witness-AND-OR evidence fsm sal-wmc/symbolic-witness <and-wmc-witness> bdd/and))

(define-method (sal-wmc/symbolic-witness (evidence <or-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-AND-OR evidence fsm sal-wmc/symbolic-witness <or-wmc-witness> bdd/or))

(define (symbolic-witness-AND-OR evidence fsm wit-proc wit-class bdd-operator)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   ;(wit1 (wit-proc (car (slot-value evidence :sub1)) fsm))
   (wit1 (map (cut wit-proc <> fsm) (slot-value evidence :sub1))) ;
   ;(wit2 (wit-proc (car (slot-value evidence :sub2)) fsm))
   (wit2 (map (cut wit-proc <> fsm) (slot-value evidence :sub2))))
    ;; (breakpoint "symbolic-witness-AND-OR" (evidence fsm wit1 wit2 bdd-operator) #t)
    (make-instance wit-class
       :valid1 (not (exists (lambda (b)
            (not (bdd/false? b)))
          (slot-value (car (slot-value evidence :sub1)) :bad)))
       :valid2 (not (exists (lambda (b)
            (not (bdd/false? b)))
          (slot-value (car (slot-value evidence :sub2)) :bad)))
       :fsm fsm
       :expr (slot-value evidence :expr)
       :bdd-list (list (bdd-operator (car (slot-value (car (slot-value evidence :sub1)) :in))
             (car (slot-value (car (slot-value evidence :sub2)) :in))))
       :sub1 wit1
       :sub2 wit2)))

;;---------------------------
;; symbolic witness for EG/AG
;;---------------------------

(define-method (sal-wmc/symbolic-witness (evidence <eg-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-G evidence fsm sal-wmc/symbolic-witness <eg-wmc-witness>))

(define-method (sal-wmc/symbolic-witness (evidence <ag-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-G evidence fsm sal-wmc/symbolic-witness <ag-wmc-witness>))

(define (symbolic-witness-G evidence fsm wit-proc wit-class)
  (symbolic-evidence-F/G evidence fsm bdd/and wit-proc wit-class))

;;---------------------------
;; symbolic witness for EF/AF
;;---------------------------

(define-method (sal-wmc/symbolic-witness (evidence <ef-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-F evidence fsm sal-wmc/symbolic-witness <ef-wmc-witness>))

(define-method (sal-wmc/symbolic-witness (evidence <af-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-F evidence fsm sal-wmc/symbolic-witness <af-wmc-witness>))

(define (symbolic-witness-F evidence fsm wit-proc wit-class)
  (symbolic-evidence-F/G evidence fsm bdd/diff wit-proc wit-class))

(define (symbolic-evidence-F/G evidence fsm bdd-operator wit-proc wit-class)
  ;; (print "symbolic-witness-F/G")
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (V (fold-left bdd/or (bdd/false bdd-manager) (slot-value evidence :in)))
   (w-eg (map (lambda (w_i)
          (bdd-operator V w_i))
        (slot-value evidence :fp))))
  ;;  (breakpoint "sal-wmc/symbolic-witness-F/G" (evidence fsm bdd-operator wit-proc wit-class w-eg) 
  ;;  (subclass? wit-class <af-wmc-counterexample>))
    (make-instance wit-class
       :fsm fsm
       :expr (slot-value evidence :expr)
       :bdd-list w-eg
       :sub (map (cut wit-proc <> fsm) (slot-value evidence :sub1)))))

;;---------------------------
;; symbolic witness for EX/AX
;;---------------------------

(define-method (sal-wmc/symbolic-witness (evidence <ex-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-X evidence fsm sal-wmc/symbolic-witness <ex-wmc-witness> bdd/diff))

(define-method (sal-wmc/symbolic-witness (evidence <ax-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-X evidence fsm sal-wmc/symbolic-witness <ax-wmc-witness> bdd/diff))

(define (symbolic-evidence-X evidence fsm wit-proc wit-class bdd-operator)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (V (fold-left bdd/or (bdd/false bdd-manager) (slot-value evidence :in)))
   (w-ex (bdd-operator V (slot-value evidence :fp))))
  ; (breakpoint "symbolic-witness-X" (evidence fsm w-ex bdd-operator V) #t)
    (make-instance wit-class
       :fsm fsm
       :expr (slot-value evidence :expr)
       :bdd-list (list w-ex)
       :sub (map (cut wit-proc <> fsm) (slot-value evidence :sub1)))))

;;---------------------------
;; symbolic witness for EU/AU
;;---------------------------

(define-method (sal-wmc/symbolic-witness (evidence <eu-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-U evidence fsm sal-wmc/symbolic-witness <eu-wmc-witness>))

(define-method (sal-wmc/symbolic-witness (evidence <au-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-U evidence fsm sal-wmc/symbolic-witness <au-wmc-witness>))

(define (symbolic-witness-U evidence fsm wit-proc wit-class)
  (symbolic-evidence-U/R evidence fsm bdd/diff wit-proc wit-class))

;;---------------------------
;; symbolic witness for ER/AR
;;---------------------------

(define-method (sal-wmc/symbolic-witness (evidence <er-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-R evidence fsm sal-wmc/symbolic-witness <er-wmc-witness>))

(define-method (sal-wmc/symbolic-witness (evidence <ar-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-R evidence fsm sal-wmc/symbolic-witness <ar-wmc-witness>))

(define (symbolic-witness-R evidence fsm wit-proc wit-class)
  (symbolic-evidence-U/R evidence fsm bdd/and wit-proc wit-class))

(define (symbolic-evidence-U/R evidence fsm bdd-operator wit-proc wit-class)
  (let* ((bdd-manager (sal-bdd-fsm/manager fsm))
   (V (fold-left bdd/or (bdd/false bdd-manager) (slot-value evidence :in)))
   (w-eg (map (lambda (w_i)
          (bdd-operator V w_i))
        (slot-value evidence :fp))))
    (make-instance wit-class
       :fsm fsm
       :expr (slot-value evidence :expr)
       :bdd-list w-eg
       :sub1 (map (cut wit-proc <> fsm) (slot-value evidence :sub1))
       :sub2 (map (cut wit-proc <> fsm) (slot-value evidence :sub2)))))


;;=========================================
;; construction of symbolic counterexamples
;;=========================================

(define-generic (sal-wmc/symbolic-counterexample evidence fsm))

;;-----------------------------
;; symbolic counterexample atom
;;-----------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <wmc-evidence>) (fsm <sal-bdd-fsm>))
  (make-instance <atom-wmc-counterexample>
     :fsm fsm
     :expr (slot-value evidence :expr)
     :bdd-list '()))

;;-----------------------------
;; symbolic counterexample AND
;;-----------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <and-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-witness-AND-OR evidence fsm sal-wmc/symbolic-counterexample <and-wmc-counterexample> bdd/or))

(define-method (sal-wmc/symbolic-counterexample (evidence <or-wmc-evidence>) (fsm <sal-bdd-fsm>))
  ;(breakpoint "sal-wmc/symbolic-counterexample->OR" (evidence fsm) #t)
  (symbolic-witness-AND-OR evidence fsm sal-wmc/symbolic-counterexample <or-wmc-counterexample> bdd/and))

;;----------------------------------
;; symbolic counterexample for EF/AF
;;----------------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <ef-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-F/G evidence fsm bdd/and sal-wmc/symbolic-counterexample <ef-wmc-counterexample>))

(define-method (sal-wmc/symbolic-counterexample (evidence <af-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-F/G evidence fsm bdd/and sal-wmc/symbolic-counterexample <af-wmc-counterexample>))

;;----------------------------------
;; symbolic counterexample for EG/AG
;;----------------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <eg-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-F/G evidence fsm bdd/diff sal-wmc/symbolic-counterexample <eg-wmc-counterexample>))

(define-method (sal-wmc/symbolic-counterexample (evidence <ag-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-F/G evidence fsm bdd/diff sal-wmc/symbolic-counterexample <ag-wmc-counterexample>))

;;----------------------------------
;; symbolic counterexample for EX/AX
;;----------------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <ex-wmc-evidence>) (fsm <sal-bdd-fsm>))
;(print "sal-wmc/symbolic-counterexample for EX")
  (symbolic-evidence-X evidence fsm sal-wmc/symbolic-counterexample <ex-wmc-counterexample> bdd/and))

(define-method (sal-wmc/symbolic-counterexample (evidence <ax-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-X evidence fsm sal-wmc/symbolic-counterexample <ax-wmc-counterexample> bdd/and))

;;----------------------------------
;; symbolic counterexample for EU/AU
;;----------------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <eu-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-U/R evidence fsm bdd/and sal-wmc/symbolic-counterexample <eu-wmc-counterexample>))

(define-method (sal-wmc/symbolic-counterexample (evidence <au-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-U/R evidence fsm bdd/and sal-wmc/symbolic-counterexample <au-wmc-counterexample>))

;;----------------------------------
;; symbolic counterexample for ER/AR
;;----------------------------------

(define-method (sal-wmc/symbolic-counterexample (evidence <er-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-U/R evidence fsm bdd/diff sal-wmc/symbolic-counterexample <er-wmc-counterexample>))

(define-method (sal-wmc/symbolic-counterexample (evidence <ar-wmc-evidence>) (fsm <sal-bdd-fsm>))
  (symbolic-evidence-U/R evidence fsm bdd/diff sal-wmc/symbolic-counterexample <ar-wmc-counterexample>))

;;----------------------------------------

(define-generic (sal-wmc/get-initial-bdd wmc-object))

(define-method (sal-wmc/get-initial-bdd (wmc-object <wmc-witness>))
  (let* ((fsm (slot-value wmc-object :fsm))
   (initials (sal-bdd-fsm/initial-states fsm)))
    initials))

(define-method (sal-wmc/get-initial-bdd (wmc-object <wmc-counterexample>))
  (if (null? (slot-value wmc-object :bdd-list)) ;; no counterexample
      #f
      (let* ((fsm (slot-value wmc-object :fsm))
       (bdd-list (slot-value wmc-object :bdd-list))
       (ce (car bdd-list))  ;; was bdd-list works only for X??????????????????
       (bad-initials (bdd/and ce
            (sal-bdd-fsm/initial-states fsm))))
;(breakpoint "sal-wmc/get-initial-bdd" (wmc-object fsm bdd-list ce bad-initials) #t)
   bad-initials)))

;;===========================================
;; explicit witness/counterexample extraction
;;===========================================
         
(define-generic (sal-wmc/display-trace wmc-object))

(define-method (sal-wmc/display-trace (wmc-object <wmc-witness-or-counterexample>))
;(breakpoint "sal-wmc/display-tracebefore" (wmc-object) #t)
  (unless (null? (slot-value wmc-object :bdd-list)) ;; no counterexample
    (let* ((fsm (slot-value wmc-object :fsm))
     (initials (sal-wmc/get-initial-bdd wmc-object))
     (initials-list (sal-bdd-fsm/peek-states-without-choices fsm initials 100)))
      (cond
       ((= (length initials-list) 1)
        (let* ((initial (sal-bdd-fsm/peek-state-without-choices fsm initials)))
     ; (breakpoint "sal-wmc/display-trace" (wmc-object fsm initials initial) #t)
    (sal-wmc/display-trace-starting-at wmc-object initial)))
       (else
        (sal-bdd-fsm/display-state-list fsm initials-list #f #f)
        (print "Please choose from above list of initial states a starting state for the path.")
        (let ((backtrack (lambda () (quit)))
        (continue (lambda (idx)
        (when (or (<= idx 0)
            (> idx (length initials-list)))
              (sign-error "Invalid index.")) 
        (let* ((pos-idx (- idx 1))
               (initial (list-ref initials-list pos-idx)))
          (sal-wmc/display-trace-starting-at wmc-object initial)))))
    (print "Please use the function `(continue idx)' to continue the output. 'idx' is the state index in the list above.")
    (print "The function '(backtrack)' can be used to backtrack.")
    (display-separator)
    (stop-and-interact "sal-wmc > " (backtrack continue))))))))

(define-generic (sal-wmc/display-trace-starting-at wmc-object state))

(define-method (sal-wmc/display-trace-starting-at (wmc-object <wmc-witness>) (state <bdd>))
  ;; Do nothing
  #unspecified
)

;;-------
;; atom

(define-method (sal-wmc/display-trace-starting-at (wmc-object <atom-wmc-witness>) (state <bdd>))
  (sal-bdd-fsm/display-states-without-choices (slot-value wmc-object :fsm) state 100 #f))

(define-method (sal-wmc/display-trace-starting-at (wmc-object <atom-wmc-counterexample>) (state <bdd>))
  (sal-bdd-fsm/display-states-without-choices (slot-value wmc-object :fsm) state 100 #f))

;;-----------------
;; AND

(define-method (sal-wmc/display-trace-starting-at (wmc-object <and-wmc-witness>) (state <bdd>))
  (display-trace-AND-core wmc-object state))

(define-method (sal-wmc/display-trace-starting-at (wmc-object <and-wmc-counterexample>) (state <bdd>))
  (display-trace-AND-core wmc-object state))

;;-----------------
;; OR

(define-method (sal-wmc/display-trace-starting-at (wmc-object <or-wmc-witness>) (state <bdd>))
  (display-trace-OR-core wmc-object state))

(define-method (sal-wmc/display-trace-starting-at (wmc-object <or-wmc-counterexample>) (state <bdd>))
  (display-trace-OR-core wmc-object state))

;;-----------------
;; EG

(define-method (sal-wmc/display-trace-starting-at (wmc-object <eg-wmc-witness>) (state <bdd>))
  (display-trace-EG-core wmc-object state))

;; counterex EG is witness AF

(define-method (sal-wmc/display-trace-starting-at (wmc-object <eg-wmc-counterexample>) (state <bdd>))
  (display-trace-AF-core wmc-object state))

;;--------------------
;; AF

(define-method (sal-wmc/display-trace-starting-at (wmc-object <af-wmc-witness>) (state <bdd>))
  (display-trace-AF-core wmc-object state))

;; counterex AF is witness EG

(define-method (sal-wmc/display-trace-starting-at (wmc-object <af-wmc-counterexample>) (state <bdd>))
  (display-trace-EG-core wmc-object state))

;;--------------------
;; EF

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ef-wmc-witness>) (state <bdd>))
  (display-trace-EF-core wmc-object state))

;; counterex EF is witness AG

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ef-wmc-counterexample>) (state <bdd>))
  (display-trace-AG-core wmc-object state))

;;--------------------
;; AG

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ag-wmc-witness>) (state <bdd>))
  (display-trace-AG-core wmc-object state))

;; counterex AG is witness EF

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ag-wmc-counterexample>) (state <bdd>))
  (display-trace-EF-core wmc-object state))

;;--------------------
;; EX

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ex-wmc-witness>) (state <bdd>))
  (display-trace-EX-core wmc-object state))

;; counterex EX is witness AX

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ex-wmc-counterexample>) (state <bdd>))
  (display-trace-AX-core wmc-object state))

;;--------------------
;; AX

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ax-wmc-witness>) (state <bdd>))
  (display-trace-AX-core wmc-object state))

;; counterex AX is witness EX

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ax-wmc-counterexample>) (state <bdd>))
  (display-trace-EX-core wmc-object state))

;;--------------------
;; AU (not implemented yet)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <au-wmc-witness>) (state <bdd>))
  ;; TODO
  #unspecified)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <au-wmc-counterexample>) (state <bdd>))
  ;; TODO
  #unspecified)

;;--------------------
;; EU (not implemented yet)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <eu-wmc-witness>) (state <bdd>))
  ;; TODO
  #unspecified)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <eu-wmc-counterexample>) (state <bdd>))
  ;; TODO
  #unspecified)

;;--------------------
;; AR (not implemented yet)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ar-wmc-witness>) (state <bdd>))
  ;; TODO
  #unspecified)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <ar-wmc-counterexample>) (state <bdd>))
  ;; TODO
  #unspecified)

;;--------------------
;; ER (not implemented yet)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <er-wmc-witness>) (state <bdd>))
  ;; TODO
  #unspecified)

(define-method (sal-wmc/display-trace-starting-at (wmc-object <er-wmc-counterexample>) (state <bdd>))
  ;; TODO
  #unspecified)


;;--------------------

(define (display-separator)
  (print "------------------------------"))

(define (formula-op-id expr)
;  (breakpoint "formula-op-id" (expr) #t)
  (if (and (instance-of? expr <sal-application>)
     (instance-of? (slot-value expr :fun) <sal-name-expr>))
      (sal-name-ref/name (slot-value expr :fun))
      (internal-error)))

(define (display-formula-info wmc-object)
  (let ((expr (slot-value wmc-object :expr)))
    (print "Path for formula " (formula-op-id expr) " located at: " (format-with-location expr ""))))

(define (display-formula-name wmc-object)
  (let ((expr (slot-value wmc-object :expr)))
    (print "Formula:" (formula-op-id expr) " located at: " (format-with-location expr ""))))

(define (display-trace-EG-core wmc-object state)
  (display-separator)
  (display-formula-info wmc-object)
  (let* ((fsm (slot-value wmc-object :fsm))
   (care-set (car (slot-value wmc-object :bdd-list)))
   (bdd-manager (sal-bdd-fsm/manager fsm)))
    (let loop ((state state) 
         (trace (list state))
         (states-so-far (bdd/false bdd-manager)))
      (cond
       ((bdd/false? (bdd/diff state states-so-far))
  (display-trace fsm trace)
  (let ((sub-witness-list (reverse (slot-value wmc-object :sub))))
  ;  (breakpoint "display-trace-EG-core" (sub-witness-list fsm wmc-object contains-temporal?) #t)
    (cond
     ((contains-temporal? (car sub-witness-list))
      (let ((backtrack (lambda () (quit)))
      (continue (lambda (idx)
            (when (or (< idx 0)
          (>= idx (length trace)))
            (sign-error "Invalid index."))   
            (let* ((pos-idx (- (- (length trace) idx) 1))
             (sub-witness (list-ref sub-witness-list pos-idx))
             (state (list-ref trace pos-idx)))
        (sal-wmc/display-trace-starting-at sub-witness state)))))
        (print "Please use the function `(continue idx)' to continue the output. 'idx' is the state index in the list above.")
        (print "The function '(backtrack)' can be used to backtrack.")
        (stop-and-interact "sal-wmc > " (backtrack continue))))
     (else
      (display-separator)
      (print "Done.")))))
       (else
  (let* ((new-states (bdd/and (ctl-image fsm state) care-set))
         (_ (sal-assert "display-trace-EG-core" (new-states fsm state care-set wmc-object) (not (bdd/false? new-states))))
         (new-state (sal-bdd-fsm/peek-state-without-choices fsm new-states)))
    (loop new-state
    (cons new-state trace)
    (bdd/or states-so-far state))))))))

(define (display-trace-AG-core wmc-object state)
  (display-separator)
  (let* ((fsm (slot-value wmc-object :fsm))
   (bdd-manager (sal-bdd-fsm/manager fsm)))
    (let loop ((state state)
         (trace (list state))
         (states-so-far (bdd/false bdd-manager)))
      (cond
       ((bdd/false? (bdd/diff state states-so-far))
  (display-separator)
  (print "Main path:")
  (display-formula-info wmc-object)
  (display-trace fsm trace)
  (display-separator)
  (print "Done.")
  (let ((sub-witness-list (reverse (slot-value wmc-object :sub))))
    (cond   ; if subformula contains temp. op. then output corresponding evidence
     ((contains-temporal? (car sub-witness-list))      
      (display-separator)
      (print "Construct now path for the direct subformula.")
      (display-formula-name (car sub-witness-list))
      (let ((backtrack (lambda () (quit)))
      (continue (lambda (idx)
            (when (or (< idx 0)
          (>= idx (length trace)))
            (sign-error "Invalid index."))   
            (let* ((pos-idx (- (- (length trace) idx) 1))
             (sub-witness (list-ref sub-witness-list pos-idx))
             (state (list-ref trace pos-idx)))
        (sal-wmc/display-trace-starting-at sub-witness state)))))
        (print "Please use the function `(continue idx)' to continue the output. 'idx' is the state index in the list above.")
        (print "The function '(backtrack)' can be used to backtrack.")
        (stop-and-interact "sal-wmc > " (backtrack continue))))
     (else
      (display-separator)
      (print "Done.")))))
       (else
  (let* ((new-states (ctl-image fsm state))
         (_ (assert (new-states) (not (bdd/false? new-states))))
         (new-states-list (sal-bdd-fsm/peek-states-without-choices fsm new-states 100)))
    (cond
     ((= (length new-states-list) 1)
      (let* ((new-state (sal-bdd-fsm/peek-state-without-choices fsm new-states)))
        (loop new-state
        (cons new-state trace)
        (bdd/or states-so-far state))))
     (else
      (print "Path so far:")
      (display-trace fsm trace)
      (display-separator)
      (display-separator)
      (print "Please choose from following list a state through which the path for")
            (display-formula-name wmc-object)
            (print "should go.")
      (display-separator)
      (sal-bdd-fsm/display-state-list fsm new-states-list #f #f)
      (let ((backtrack (lambda () (quit)))
      (continue (lambda (idx)
            (when (or (<= idx 0)
          (> idx (length new-states-list)))
            (sign-error "Invalid index.")) 
            (let* ((pos-idx (- idx 1))
             (new-state (list-ref new-states-list pos-idx)))
        (loop new-state
              (cons new-state trace)
              (bdd/or states-so-far state))))))
        (print "Please use the function `(continue idx)' to continue the output. 'idx' is the state index in the list above.")
        (print "The function '(backtrack)' can be used to backtrack.")
        (display-separator)
        (stop-and-interact "sal-wmc > " (backtrack continue)))))))))))


(define (find-min-sublist-starting-at-state fsm bdd-list state)
  ;; (breakpoint "min-sublist" (fsm bdd-list state) #t)
  (let loop ((bdd-list bdd-list))
    (cond
     ((null? bdd-list)
      '())
     ((null? (cdr bdd-list))
      [assert (bdd-list state) (not (bdd/false? (bdd/and state (car bdd-list))))]
      bdd-list)
     (else
      (let ((curr (car bdd-list))
      (succ (cadr bdd-list)))
  [assert (bdd-list curr state) (not (bdd/false? (bdd/and curr state)))]
  (if (bdd/false? (bdd/and succ state))
      bdd-list
      (loop (cdr bdd-list))))))))

(define (display-trace-EF-core wmc-object state)
  (display-separator)
  (display-formula-info wmc-object)
  (let* ((fsm (slot-value wmc-object :fsm))
   (symb-wit (slot-value wmc-object :bdd-list))
   (min-sublist (find-min-sublist-starting-at-state fsm symb-wit state))
   (bdd-manager (sal-bdd-fsm/manager fsm)))
;(breakpoint "display-trace-EF-core" (fsm symb-wit min-sublist bdd-manager state) #t)
    [assert (state min-sublist) (not (bdd/false? (bdd/and state (car min-sublist))))]
    (let loop ((state state)
         (trace (list state))
         (min-sublist (cdr min-sublist))
         (sub-witness-list (reverse (slot-value wmc-object :sub))))   ;;; April 7
      (cond
       ((null? min-sublist)
  (display-trace fsm trace)
  (cond
   ((contains-temporal? (car sub-witness-list))
    (display-separator)
    (print "Construct now path for the direct subformula of EF.")
    (display-formula-name (car sub-witness-list))
    ;; (breakpoint "display-trace-EF-core" (sub-witness-list state fsm) #t)
    (sal-wmc/display-trace-starting-at (car sub-witness-list) state))
   (else
    (display-separator)
    (print "Done."))))
       (else
  (let* ((new-states (bdd/and (ctl-image fsm state) (car min-sublist)))
         (_ (assert (new-states) (not (bdd/false? new-states))))
         (new-state (sal-bdd-fsm/peek-state-without-choices fsm new-states)))
    (loop new-state
    (cons new-state trace)
    (cdr min-sublist)
    (cdr sub-witness-list))))))))

(define (display-trace-AF-core wmc-object state)
  (display-separator)
;  (display-formula-info wmc-object)
  (let* ((fsm (slot-value wmc-object :fsm))
   (symb-wit (slot-value wmc-object :bdd-list))
   (good-states (car (reverse symb-wit)))
   (min-sublist (find-min-sublist-starting-at-state fsm symb-wit state))
   (bdd-manager (sal-bdd-fsm/manager fsm)))
    [assert (state min-sublist) (not (bdd/false? (bdd/and state (car min-sublist))))]
    (let loop ((state state)
         (trace (list state))
         (min-sublist (cdr min-sublist)))
      (cond
       ((bdd/false? (bdd/diff state good-states))
  (display-separator)
  (display-formula-info wmc-object)
  (display-trace fsm trace)
  (display-separator)
  (print "Done.")
  (let ((sub-witness-list (reverse (slot-value wmc-object :sub))))
    (cond   ; if subformula contains temp. op. then output corresponding evidence
     ((contains-temporal? (car sub-witness-list))
      (display-separator)
      (print "Construct now path for the direct subformula of AF.")
      (display-formula-name (car sub-witness-list))
      (sal-wmc/display-trace-starting-at (car sub-witness-list) state))
     (else
      (display-separator)
      (print "Done.")))))
       (else
  (let* ((new-states (bdd/and (ctl-image fsm state) (car min-sublist)))
         (_ (assert (new-states) (not (bdd/false? new-states))))
         (new-states-list (sal-bdd-fsm/peek-states-without-choices fsm new-states 100)))
    (sal-bdd-fsm/display-state-list fsm new-states-list #f #f)
    (print "Please choose from above list a state through which the path should go.")
          ;(breakpoint "new-states-list" (fsm new-states-list) #t)
    (let ((backtrack (lambda () (quit)))
    (continue (lambda (idx)
          (when (or (<= idx 0)
              (> idx (length new-states-list)))
          (sign-error "Invalid index.")) 
          (let* ((pos-idx (- idx 1))
           (new-state (list-ref new-states-list pos-idx)))
            ;(breakpoint "pos-idx" (fsm new-states-list pos-idx new-state) #t)
            (loop new-state
            (cons new-state trace)
            (cdr min-sublist))))))
      (print "Please use the function `(continue idx)' to continue the output. 'idx' is the state index in the list above.")
      (print "The function '(backtrack)' can be used to backtrack.")
      (display-separator)
            (print "Path so far:")
            (display-trace fsm trace)
      (stop-and-interact "sal-wmc > " (backtrack continue)))))))))

(define (display-trace-AND-core wmc-object state)
  (display-separator)
  (display-formula-info wmc-object)
  (let* ((fsm (slot-value wmc-object :fsm))
   (symb-wit1 (car (slot-value wmc-object :sub1)))
   (symb-wit2 (car (slot-value wmc-object :sub2)))
   (valid1? (slot-value wmc-object :valid1))  ; valid1? = T
   (valid2? (slot-value wmc-object :valid2))) ; valid2? = T
;(breakpoint "display-trace-AND-core" (fsm symb-wit1 symb-wit2 valid1? valid2?) #t)
    (cond
     ((or (and valid1? valid2?) (and (not valid1?) (not valid2?)))
      (let ((backtrack (lambda () (quit)))
      (continue (lambda (idx)
      (cond
       ((= idx 1)
        (sal-wmc/display-trace-starting-at symb-wit1 state))
       ((= idx 2)
        (sal-wmc/display-trace-starting-at symb-wit2 state))
       (else "Invalid index.")))))
  (print "Please use the function `(continue 1)' to construct path for the left-hand formula, and `(continue 2)' for the right-hand formula.")
  (print "The function '(backtrack)' can be used to backtrack.")
  (stop-and-interact "sal-wmc > " (backtrack continue))))
     (valid1?
      (sal-wmc/display-trace-starting-at symb-wit2 state))
     (else
      (sal-wmc/display-trace-starting-at symb-wit1 state)))
    (display-separator)
    (print "Done.")))

(define (display-trace-OR-core wmc-object state)
  (display-separator)
  (display-formula-info wmc-object)
  (let* ((fsm (slot-value wmc-object :fsm))
   (symb-wit1 (car (slot-value wmc-object :sub1)))
   (symb-wit2 (car (slot-value wmc-object :sub2)))
   (valid1? (slot-value wmc-object :valid1))  ; valid1? = T
   (valid2? (slot-value wmc-object :valid2))) ; valid2? = T
;(breakpoint "display-trace-OR-core" (state fsm symb-wit1 symb-wit2 valid1? valid2?) #t)
    (cond
     ((and valid1? valid2?) 
      (print "*** both valid ***")
      (let ((backtrack (lambda () (quit)))
      (continue (lambda (idx)
      (cond
       ((= idx 1)
        (sal-wmc/display-trace-starting-at symb-wit1 state))
       ((= idx 2)
        (sal-wmc/display-trace-starting-at symb-wit2 state))
       (else "Invalid index.")))))
  (print "Please use the function `(continue 1)' to construct path for the left-hand formula, and `(continue 2)' for the right-hand formula.")
  (print "The function '(backtrack)' can be used to backtrack.")
  (stop-and-interact "sal-wmc > " (backtrack continue))))
     ((and (not valid1?) (not valid2?))
      (print "*** both invalid ***")
      (let ((backtrack (lambda () (quit)))
      (continue (lambda (idx)
      (cond
       ((= idx 1)
        (sal-wmc/display-trace-starting-at symb-wit1 state))
       ((= idx 2)
        (sal-wmc/display-trace-starting-at symb-wit2 state))
       (else "Invalid index.")))))
  (print "Please use the function `(continue 1)' to construct path for the left-hand formula, and `(continue 2)' for the right-hand formula.")
  (print "The function '(backtrack)' can be used to backtrack.")
  (stop-and-interact "sal-wmc > " (backtrack continue))))
     (valid1? 
      (print "*** 1. valid ***")
      (display-separator)
      (display-formula-name symb-wit1)
      (display-separator)
      (sal-wmc/display-trace-starting-at symb-wit1 state))
     (else  ;(and valid2? (not valid1?))
      (print "*** 2. valid ***")
      (display-separator)
      (display-formula-name symb-wit2)
      (display-separator)
      (sal-wmc/display-trace-starting-at symb-wit2 state)))
    (display-separator)
    (print "Done.")))

(define (display-trace-EX-core wmc-object state)
  (display-separator)
  (display-formula-info wmc-object)
  (let* ((fsm (slot-value wmc-object :fsm))
   (symb-wit (car (slot-value wmc-object :bdd-list)))
   (new-states (bdd/and (ctl-image fsm state) symb-wit))
   (_ (assert (new-states) (not (bdd/false? new-states))))
   (new-state (sal-bdd-fsm/peek-state-without-choices fsm new-states))
   (sub-witness-list (reverse (slot-value wmc-object :sub))))
    (display-trace fsm (list new-state state))
    (let ((sub-witness-list (reverse (slot-value wmc-object :sub))))
      (cond   ; if subformula contains temp. op. then output corresponding evidence
       ((contains-temporal? (car sub-witness-list))
  (sal-wmc/display-trace-starting-at (car sub-witness-list) new-state))
       (else
  (display-separator)
  (print "Done."))))))

(define (display-trace-AX-core wmc-object state)
  (display-separator)
  (let* ((fsm (slot-value wmc-object :fsm))
   (symb-wit (car (slot-value wmc-object :bdd-list)))
   (new-states (bdd/and (ctl-image fsm state) symb-wit))
   (_ (assert (new-states) (not (bdd/false? new-states))))
   (new-states-list (sal-bdd-fsm/peek-states-without-choices fsm new-states 100)))
    (sal-bdd-fsm/display-state-list fsm new-states-list #f #f)
    (print "Please choose from above list a state through which the path should go.")
    (let ((backtrack (lambda () (quit)))
    (continue (lambda (idx)
          (when (or (<= idx 0)
        (> idx (length new-states-list)))
          (sign-error "Invalid index.")) 
          (let* ((pos-idx (- idx 1))
           (new-state (list-ref new-states-list pos-idx)))
      (display-formula-info wmc-object)
      (display-trace fsm (list new-state state))
      (let ((sub-witness-list (reverse (slot-value wmc-object :sub))))
        (cond   ; if subformula contains temp. op. then output corresponding evidence
         ((contains-temporal? (car sub-witness-list))
          (display-separator)
          (sal-wmc/display-trace-starting-at (car sub-witness-list) new-state))
         (else
          (display-separator)
          (print "Done."))))))))
      (print "Please use the function `(continue idx)' to continue the output. 'idx' is the state index in the list above.")
      (print "The function '(backtrack)' can be used to backtrack.")
      (stop-and-interact "sal-wmc > " (backtrack continue)))))

(define (display-trace fsm trace)
  (let* ((path (sal-bdd-fsm/make-path fsm trace #t))
   (original-path (sal-derived-path->original-path path)))
    (sal-path/pp original-path)))
   

  




  
            



