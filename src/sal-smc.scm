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

(module sal-smc
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-env bdd symbol-table sal-ast-for-each sal-expression queue sal-ast-list
                sal-assertion sal-path sal-environment sal-pp runtime front-end sal-module
                symbol-set ordering sal-decls sal2bdd sal-bdd-context sal-bdd-fsm sal-bdd-cluster
                sal-flat-module-to-bdd-fsm sal-smc-core sal-flat-modules sal-smc-prioritized-traversal
                sal-trace-info)
        (export (sal-module-models/smc-invariant-core module-models fsm . forward?)
                (sal-module-models/smc-accepting module-models fsm)
                (sal-module-models/smc-ctl-core module-models trans-system)
                (sal-smc/enable-counter-examples! flag)
                (sal-smc/counter-examples?)
                (sal-smc/find-path-from-initial-state module goal)
                (sal-smc/find-path-from-initial-state-with-at-most module goal max-steps)
                (sal-smc/enable-prioritized-traversal! flag)
                (sal-smc/set-prioritized-traversal-strategy! strategy-id)
                (sal-smc/set-prioritized-traversal-threshold! num)
                )
        )

(define *sal-smc-trans-build-counter-examples* #t)
(define *sal-smc-forward-search?* #t)
(define *sal-smc-enable-counter-examples* #t)
(define *sal-smc-prioritized-traversal* #f)
(define *sal-smc-prioritized-traversal-strategy* 'greedy)
(define *sal-smc-prioritized-traversal-threshold* 10000)

(define-api (sal-smc/enable-counter-examples! (flag boolean?))
	(set! *sal-smc-enable-counter-examples* flag))

(define-api (sal-smc/counter-examples?)
	*sal-smc-enable-counter-examples*)

(define-api (sal-smc/enable-prioritized-traversal! (flag boolean?))
	(set! *sal-smc-prioritized-traversal* flag))

(define-api (sal-smc/set-prioritized-traversal-strategy! (id symbol?))
	(unless (memq id '(min dfs bfs greedy))
		(sign-invalid-arg "Unknown prioritized traversal strategy: `~a'." id))
	(set! *sal-smc-prioritized-traversal-strategy* id))

(define-api (sal-smc/set-prioritized-traversal-threshold! (num number?))
	(unless (> num 0)
		(sign-invalid-arg "Invalid prioritized traversal threshold: ~a." num))
	(set! *sal-smc-prioritized-traversal-threshold* num))

(front-end/add-simple-option! 
 "BDD Interface" "--disable-counter-examples"
 "Disable the generation of counterexamples. The model checker will run faster. This option will also disable traceability."
 (lambda ()
	 (sal-smc/enable-counter-examples! #f)
	 (sal/set-trace-info-enabled! #f)))

(front-end/add-full-option!
 "Prioritized Traversal"
 "-pt"
 "--prioritize-traversal"
 "Use prioritized traversal to verify safety properties. In a prioritized traversal, the BDDs representing the frontier are broken in smaller pieces when they exceed a specified threshold. This option is ignored if the property is not a safety property."
 (lambda ()
	 (set! *sal-smc-prioritized-traversal* #t)))

(front-end/add-full-option!
 "Prioritized Traversal"
 "-pts <name>"
 "--pt-strategy=<name>" 
 "Set the strategy used in the prioritized traversal (default: greedy). Available strategies: dfs (depth-first search), bfs (breadth-first search), min (smallest frontier first), greedy (`easiest' to process first)."  
 (lambda (name)
	 (sal-smc/set-prioritized-traversal-strategy! (object->symbol name))))

(front-end/add-full-option!
 "Prioritized Traversal"
 "-ptt <num>"
 "--pt-threshold=<num>"
 "Set the threshold (number of nodes) for splitting a BDD in a prioritized traversal. (default: 10000)"
 (front-end-adapter/nz-nat-arg 
	(lambda (arg)
		(sal-smc/set-prioritized-traversal-threshold! arg))))

(front-end/add-simple-option! 
 "BDD Interface" "--backward"
 "Use backward search to prove LTL properties (default: forward search)."
 (lambda ()
	 (set! *sal-smc-forward-search?* #f)))

(define (check-if-compatible-fsm module-models fsm)
	(let ((module (slot-value module-models :module)))
		(unless (eq? module (slot-value fsm :flat-module))
			(sign-source-error module-models "Invalid call to sal-module-models/prove-core. This property is not related to the given transition system."))))

(define (check-if-invariant module-models fun-name)
	(unless (sal-module-models/invariant? module-models)
		(sign-source-error module-models "The property is not an invariant, and the proof method ~a can only be used to prove invariants." fun-name)))

(define-generic (sal-smc/find-path-from-initial-state module goal)
	:doc "Returns a SAL path starting at an initial state, and ending in a state in @code{goal}.")

(define-method (sal-smc/find-path-from-initial-state (module <sal-bdd-fsm>) (goal <bdd>))
	(sal-smc/find-path-from-initial-state-core module goal))

(define-method (sal-smc/find-path-from-initial-state (module <sal-flat-module>) (goal <sal-expr>))
	(let* ((fsm (sal-flat-module->sal-bdd-fsm module))
				 (goal-bdd (sal-expr->bdd goal fsm)))
		(sal-smc/find-path-from-initial-state-core fsm goal-bdd)))

(define (sal-smc/find-path-from-initial-state-core fsm goal-bdd)
	(verbose-message 1 "finding state starting at the initial state...")
	(display-runtime 2 "  search time: ~a secs"
		(lambda ()
			(let ((trace (sal-bdd-fsm/make-trace-to fsm goal-bdd)))
				(cond
				 (trace
					(values (sal-bdd-fsm/make-path fsm trace #t) fsm))
				 (else
					(values #f fsm)))))))

(define-generic (sal-smc/find-path-from-initial-state-with-at-most module goal max-steps)
	:doc "Similar to @code{sal-smc/find-path-from-initial-state}, but the path contains at most @code{max-steps} states.")

(define-method (sal-smc/find-path-from-initial-state-with-at-most (module <sal-bdd-fsm>) (goal <bdd>) (max-steps <primitive>))
	(sal-smc/find-path-from-initial-state-with-at-most-core module goal max-steps))

(define-method (sal-smc/find-path-from-initial-state-with-at-most (module <sal-flat-module>) (goal <sal-expr>) (max-steps <primitive>))
	(let* ((fsm (sal-flat-module->sal-bdd-fsm module))
				 (goal-bdd (sal-expr->bdd goal fsm)))
		(sal-smc/find-path-from-initial-state-with-at-most-core fsm goal-bdd max-steps)))

(define (sal-smc/find-path-from-initial-state-with-at-most-core fsm goal-bdd max-steps)
	(verbose-message 1 "finding state starting at the initial state with at most ~a steps..." max-steps)
	(display-runtime 2 "  search time: ~a secs"
		(lambda ()
			(let ((trace (sal-bdd-fsm/make-trace-from-to-with-at-most fsm (sal-bdd-fsm/initial-states fsm) goal-bdd max-steps)))
				(cond
				 (trace
					(values (sal-bdd-fsm/make-path fsm trace #t) fsm))
				 (else
					(values #f fsm)))))))
	
(define (strategy-id->prioritized-traversal-proc id)
	(case id
		((dfs) sal-bdd-fsm/dfs-prioritized-find-trace)
		((bfs) sal-bdd-fsm/bfs-prioritized-find-trace)
		((min) sal-bdd-fsm/min-prioritized-find-trace)
		(else sal-bdd-fsm/greedy-prioritized-find-trace)))
	
(define (display-building-counterexample-msg)
	(verbose-message 3 "  INVALID, building counterexample..."))

(define (sal-module-models/smc-invariant-core module-models fsm . forward?)
	(let ((forward? (optional-arg forward? *sal-smc-forward-search?*))
				(m (sal-bdd-context/manager fsm)))
		(check-if-compatible-fsm module-models fsm)
		(check-if-invariant module-models 'sal-module-models/smc-invariant-core)
		(verbose-message 1 "proving invariant or producing counterexample using BDDs...")
		(verbose-message 1 "  using ~a search" (if forward? "forward" "backward"))
		(display-runtime 2 "  verification time: ~a secs"
			(lambda ()
				(let* ((property-body (sal-module-models/invariant-body module-models))
							 (property-bdd (sal-bdd-fsm/exec-pre-search-thunk-enabling-bdd-var-reordering-when-active fsm
																																																				(lambda () (sal-expr->bdd property-body fsm))))
							 (not-property-bdd (bdd/not property-bdd))
							 (trace->result (lambda (trace)
																(cond
																 (trace
																	(display-building-counterexample-msg)
																	(values #f (sal-bdd-fsm/make-path fsm trace forward?)))
																 (else
																	(values #t #unspecified))))))
					(cond
					 (*sal-smc-prioritized-traversal*
						(let ((find-trace-proc (strategy-id->prioritized-traversal-proc *sal-smc-prioritized-traversal-strategy*)))
							(multiple-value-bind
									(invalid-states reached-states trace)
									(find-trace-proc fsm (sal-bdd-fsm/initial-states fsm) not-property-bdd *sal-smc-prioritized-traversal-threshold*)
								(if *sal-smc-enable-counter-examples*
									(trace->result trace)
									(values #t #unspecified)))))
					 ((and forward? *sal-smc-enable-counter-examples*)
						(trace->result (sal-bdd-fsm/make-trace-to fsm not-property-bdd)))
					 ((and (not forward?) *sal-smc-enable-counter-examples*)
						(trace->result (sal-bdd-fsm/make-backward-trace-from fsm not-property-bdd)))
					 (forward?
						(values (bdd/false? (sal-bdd-fsm/reachable fsm not-property-bdd)) #unspecified))
					 (else
						(values (bdd/false? (sal-bdd-fsm/backward-reachable fsm not-property-bdd)) #unspecified))))))))

(define (sal-module-models/smc-accepting module-models fsm)
	(check-if-compatible-fsm module-models fsm)
	(unless (sal-module-models/accepting? module-models)
		(sign-error "Only accepting condition properties (low level encoding of LTL) can be verified by sal-smc. Please use sal-module-models/ltl->ba to convert the property."))
	(verbose-message 1 "proving or producing counterexample using BDDs...")
	(display-runtime 2 "  verification time: ~a secs"
		(lambda ()
			(let ((m (sal-bdd-context/manager fsm)))
				(let* ((final-states-expr (sal-module-models/accepting-body module-models))
							 (final-states (sal-bdd-fsm/exec-pre-search-thunk-enabling-bdd-var-reordering-when-active fsm (lambda () (sal-expr->bdd final-states-expr fsm))))
							 (initial-states (sal-bdd-fsm/initial-states fsm)))
					(let ((accepting-final-states (sal-bdd-fsm/final-accepting-states fsm 
																																						final-states 
																																						(sal-module-models/weak-accepting? module-models) 
																																						*sal-smc-forward-search?*)))
						(cond
						 ((bdd/false? accepting-final-states)
							;; there isn't a counterexample
							(values #t #unspecified))
						 (*sal-smc-enable-counter-examples*
							;; To get the SCCs, I have to compute the accepting paths in the opposite direction
							(display-building-counterexample-msg)
							(let ((SCCs (sal-bdd-fsm/final-accepting-states fsm accepting-final-states (sal-module-models/weak-accepting? module-models) (not *sal-smc-forward-search?*))))
								(values #f (sal-bdd-fsm/make-accepting-cyclic-path fsm SCCs))))
						 (else
							(values #f #unspecified)))))))))

;---------------------------------------------------
;
; Support for CTL operators
;
;---------------------------------------------------

(define-method (sal-expr->bdd-core (prop <sal-ctl-ex>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(sal-bdd-fsm/EX fsm 
									(sal-expr->bdd-core (slot-value prop :arg) env fsm)
									(slot-value fsm :restriction)))

(define-method (sal-expr->bdd-core (prop <sal-ctl-ef>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(sal-bdd-fsm/EF fsm
									(sal-expr->bdd-core (slot-value prop :arg) env fsm)
									(slot-value fsm :restriction)))

(define-method (sal-expr->bdd-core (prop <sal-ctl-eg>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(sal-bdd-fsm/EG fsm
									(sal-expr->bdd-core (slot-value prop :arg) env fsm)
									(slot-value fsm :restriction)))
									
(define-method (sal-expr->bdd-core (prop <sal-ctl-eu>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(multiple-value-bind
			(arg1 arg2)
			(sal-binary-application/arguments prop)
		(sal-bdd-fsm/EU fsm
										(sal-expr->bdd-core arg1 env fsm) 
										(sal-expr->bdd-core arg2 env fsm)
										(slot-value fsm :restriction))))
	
(define-method (sal-expr->bdd-core (prop <sal-ctl-ax>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(sal-bdd-fsm/AX fsm 
									(sal-expr->bdd-core (slot-value prop :arg) env fsm)
									(slot-value fsm :restriction)))

(define-method (sal-expr->bdd-core (prop <sal-ctl-af>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(sal-bdd-fsm/AF fsm
									(sal-expr->bdd-core (slot-value prop :arg) env fsm)
									(slot-value fsm :restriction)))

	
(define-method (sal-expr->bdd-core (prop <sal-ctl-ag>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(sal-bdd-fsm/AG fsm
									(sal-expr->bdd-core (slot-value prop :arg) env fsm)
									(slot-value fsm :restriction)))

(define-method (sal-expr->bdd-core (prop <sal-ctl-au>) (env <primitive>) (fsm <sal-bdd-fsm>))
	(multiple-value-bind
			(arg1 arg2)
			(sal-binary-application/arguments prop)
		(sal-bdd-fsm/AU fsm
										(sal-expr->bdd-core arg1 env fsm) 
										(sal-expr->bdd-core arg2 env fsm)
										(slot-value fsm :restriction))))

(define (sal-module-models/smc-ctl-core module-models fsm)
	(check-if-compatible-fsm module-models fsm)
	(verbose-message 1 "proving CTL formula using BDDs...")
	(display-runtime 2 "  proof body time: ~a secs"
		(lambda ()
			(let* ((initial-states (slot-value fsm :initial-states))
						 (rs (sal-bdd-fsm/reachable-states fsm))
						 (_ (set-slot-value! fsm :restriction rs))
						 (property-body (slot-value module-models :expr))
						 (property-bdd (sal-bdd-fsm/remove-choice-vars fsm (sal-expr->bdd property-body fsm)))
						 (bad-initial-states (bdd/and initial-states (bdd/not property-bdd))))
				(if (bdd/false? bad-initial-states)
					(values #t #unspecified)
					(values #f bad-initial-states))))))
