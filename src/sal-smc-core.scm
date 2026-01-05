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

(module sal-smc-core
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import bdd sal-bdd-fsm sal-bdd-cluster sal-bdd-context front-end sal-flat-modules
                symbol-table sal-expression sal-path queue sal-pp runtime sal2bdd
                sal-path-pp sal-derived-path sal-flat-module-to-bdd-fsm sal-module)
        (export (sal-smc/set-reorder-predicate! pred)
                (sal-bdd-fsm/find-trace fsm image-proc start-set target-set build-trace? r)
                (sal-bdd-fsm/reachable fsm target-set)
                (sal-bdd-fsm/backward-reachable fsm source-set)
                (sal-bdd-fsm/make-trace-from-to fsm source-set target-set)
                (sal-bdd-fsm/make-trace-to fsm target-set)
                (sal-bdd-fsm/make-trace-from-to-with-at-most fsm source-set target-set max-steps)
                (sal-bdd-fsm/make-backward-trace-from fsm source-set)
                (sal-bdd-fsm/reachable-states fsm)
                (sal-bdd-fsm/layered-reachable-states fsm)
                (sal-bdd-fsm/num-states fsm state-set)
                (sal-bdd-fsm/num-reachable-states fsm)
                (sal-bdd-fsm/peek-state fsm state-set)
                (sal-bdd-fsm/peek-state-without-choices fsm state-set)
                (sal-bdd-fsm/peek-states fsm state-set num-states)
                (sal-bdd-fsm/peek-states-without-choices fsm state-set num-states)
                (sal-bdd-fsm/state->assignment-table fsm bdd-state)
                (sal-bdd-fsm/trace->step-list fsm trace forward?)
                (sal-bdd-fsm/make-step-list fsm from-states to-states restriction)
                (sal-bdd-fsm/for-each-state fsm proc state-list)
                (sal-bdd-fsm/display-state-list fsm states hide-locals? hide-input?)
                (sal-bdd-fsm/display-states-without-choices fsm states max-states hide-locals?)
                (sal-bdd-fsm/display-states fsm states max-states hide-locals?)
                (sal-bdd-fsm/display-transition fsm state1 state2)
                (sal-bdd-fsm/make-path fsm trace forward?)
                (sal-bdd-fsm/make-path-from-to fsm from-states to-state restriction)
                (sal-bdd-fsm/deadlock-states fsm)
                (sal-bdd-fsm/closed-subset fsm state-set)
                (sal-bdd-fsm/final-accepting-states fsm final weak? forward?)
                (sal-bdd-fsm/final-accepting-states-backward fsm final)
                (sal-bdd-fsm/final-accepting-states-forward fsm final)
                (sal-bdd-fsm/final-accepting-states-weak-backward fsm final)
                (sal-bdd-fsm/final-accepting-states-weak-forward fsm final)
                (sal-bdd-fsm/make-accepting-cyclic-path fsm accepting-final-states)
                (sal-bdd-fsm/EX-core fsm p-lower p-upper r)
                (sal-bdd-fsm/EX fsm p r)
                (sal-bdd-fsm/AX fsm p r)
                (sal-bdd-fsm/EU fsm q p r)
                (sal-bdd-fsm/AU fsm q p r)
                (sal-bdd-fsm/EF fsm p r)
                (sal-bdd-fsm/AF fsm p r)
                (sal-bdd-fsm/EG fsm p r)
                (sal-bdd-fsm/AG fsm p r)
                (sal-bdd-fsm/EG-fair fsm p r fc)
                (sal-bdd-fsm/EBU fsm q p inf sup r)
                (sal-bdd-fsm/ABU fsm q p inf sup r)
                (sal-bdd-fsm/EUF fsm p inf sup r)
                (sal-bdd-fsm/AUF fsm p inf sup r))
        )

(define *sal-smc-reorder-predicate* #f)

(define *reorder-predicate-doc*  "Set a user-defined predicate which is evaluated during the construction of the set of reachable states. If this predicate evaluates to true, then the BDD variables are reordered. The predicate receives two parameters: fsm (reference to the finite state machine object), rs (BDD representing the set of reached states so far).\nExample:\n   -rp '(lambda (fsm rs) (> (bdd/size rs) 100000))'\nMeaning: Execute variable reordering when the number of nodes in the set of reached states is greater than 100000. Remark: The code defining the predicate can be stored in a separated script file. You can force any sal tool to load a script by adding its file name in the command line.")

(define-api (sal-smc/set-reorder-predicate! pred)
  :doc *reorder-predicate-doc*
  (unless (procedure? pred)
    (sign-error "Invalid reordering predicate. Argument is not a procedure."))
  (set! *sal-smc-reorder-predicate* pred))

(front-end/add-full-option! 
 "BDD Interface" 
 "-rp <scheme-expr>" 
 "--reorder-predicate=<scheme-expr>"
 *reorder-predicate-doc*
 (lambda (arg) 
   (let ((pred (try 
                (with-input-from-string arg (lambda ()
                                              (eval (read))))
                (lambda (e p m o)
                  (sign-error "Parsing Scheme expression '~a' for option -rp (--reorder-predicate)." arg)))))
     (sal-smc/set-reorder-predicate! pred))))

(define (display-iteration-info it-msg i manager frontier)
  (verbose-message 2 it-msg i)
  (verbose-message 3 "    frontier size: ~a nodes, total node count: ~a" (bdd/size frontier) (bdd/num-nodes manager)))

(define (reorder-when-needed fsm rs)
  (when *sal-smc-reorder-predicate*
    (let ((result (try
                   (*sal-smc-reorder-predicate* fsm rs)
                   (lambda (e p m o)
                     (sign-error "Executing reordering predicate, reason: ~a, ~a" m o)))))
      (when result
        (verbose-message 3 "    reordering BDD variables...")
        (verbose-message 3 "    number of BDD nodes in the set of reached states before reordering: ~a" (bdd/size rs))
        (sal-bdd-fsm/reorder! fsm)
        (verbose-message 3 "    number of BDD nodes in the set of reached states after reordering: ~a" (bdd/size rs))
        ))))

;; Find a trace from start-set to target-set restricted to r.
;; The result is:
;;   - subset of target-set -- reached from start-set
;;   - set of visited states -- states visited in the search
;;   - trace (list of bdds) -- when build-trace? is not #f
(define (sal-bdd-fsm/find-trace fsm image-proc start-set target-set build-trace? r)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m))
         (start-set (bdd/and start-set r))
         (cons-layer (lambda (bdd layer-list)
                       (if build-trace?
                         (cons bdd layer-list)
                         layer-list))))
    (let loop ((lower-bound start-set)
               (reachable-states start-set)
               (layer-list '())
               (i 1))
      (collect!)
      (cond
       ((bdd/false? lower-bound)
        (status-message :smc-num-visited-states (sal-bdd-fsm/num-states fsm reachable-states))
        (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
        (values lower-bound reachable-states #f))
       (else
        (let ((target-found (bdd/and target-set lower-bound)))
          (cond
           ((not (bdd/false? target-found))
            (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
            (values target-found reachable-states (cons-layer target-found layer-list)))
           (else
            (let ((lower-size (bdd/size lower-bound))
                  (upper-size (bdd/size reachable-states)))
              (status-message :smc-iteration i lower-size upper-size)
              (verbose-message 2 "  iteration: ~a" i)
              (verbose-message 3 "  frontier lower bound: ~a nodes, upper bound: ~a nodes" lower-size upper-size)
              (when (>= (verbosity-level) 100)
                (verbose-message 100 "  frontier lower bound: ~a states, upper bound: ~a states" 
                                 (sal-bdd-fsm/num-states fsm lower-bound) 
                                 (sal-bdd-fsm/num-states fsm reachable-states)))
              (let ((frontier (if (< lower-size upper-size) lower-bound reachable-states))) ; too expensive: (bdd/between lower-bound reachable-states)))
                (verbose-message 3 "  using frontier with ~a nodes" (bdd/size frontier))
                (verbose-message 3 "  total bdd node count: ~a" (bdd/num-nodes m))
                (reorder-when-needed fsm reachable-states)
                (let* ((upper-bound reachable-states)
                       ;; (care-set (bdd/not reachable-states))
                       (image (bdd/and (image-proc fsm frontier) r))  ;;  upper-bound care-set))
                       (new-reachable-states (bdd/or reachable-states image))
                       (new-lower-bound (bdd/and image (bdd/not reachable-states)))
                       (new-layer-list (cons-layer lower-bound layer-list)))
                  (loop new-lower-bound
                        new-reachable-states
                        new-layer-list
                        (+ i 1)))))))))))))

(define (sal-bdd-fsm/layered-reachable-states-core fsm)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m))
         (initial-states (sal-bdd-fsm/initial-states fsm)))
    (let loop ((frontier initial-states)
               (reachable-states initial-states)
               (layer-list '())
               (i 1))
      (collect!)
      (cond
       ((bdd/false? frontier)
        (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
        (values reachable-states (reverse layer-list)))
       (else
        (verbose-message 2 "  iteration: ~a" i)
        (verbose-message 3 "  frontier: ~a nodes" (bdd/size frontier))
        (verbose-message 3 "  total bdd node count: ~a" (bdd/num-nodes m))
        (reorder-when-needed fsm reachable-states)
        (let* ((image (sal-bdd-fsm/image fsm frontier))
               (new-reachable-states (bdd/or reachable-states image))
               (new-frontier (bdd/diff image reachable-states))
               (new-layer-list (cons frontier layer-list)))
          (loop new-frontier new-reachable-states new-layer-list (+ i 1))))))))

(define-api (sal-bdd-fsm/reachable fsm target-set)
  :doc "Returns a subset of @code{target-set} which is reachable from the set of initial states of the finite state machine @code{fsm}."
  (sal-bdd-fsm/find-trace fsm sal-bdd-fsm/image (sal-bdd-fsm/initial-states fsm) target-set #f (bdd/true (sal-bdd-fsm/manager fsm))))

(define-api (sal-bdd-fsm/backward-reachable fsm source-set)
  :doc "Returns the subset of the initial states that is backward reachable from @code{source-set}." 
  (sal-bdd-fsm/find-trace fsm sal-bdd-fsm/pre-image-with-choices source-set (sal-bdd-fsm/initial-states fsm) #f (bdd/true (sal-bdd-fsm/manager fsm))))

(define-api (sal-bdd-fsm/make-trace-from-to fsm source-set target-set)
  :doc "Makes a trace from a subset of @code{source-set} to a subset of @code{target-set}. @code{source-set} and @code{target-set} are BDDs. Three values are returned: the subset of @code{target-set}, the set of states visited, and a list of BDDs (called the @code{trace}) represents a set of paths from @code{source-set} to @code{target-set}. This list of BDDs can be translated to a SAL path object using the procedure @code{sal-bdd-fsm/make-path}."
  (let ((m (sal-bdd-fsm/manager fsm)))
    (multiple-value-bind
        (target-subset visited-states trace)
        (sal-bdd-fsm/find-trace fsm sal-bdd-fsm/image source-set target-set #t (bdd/true (sal-bdd-fsm/manager fsm)))
      trace)))

(define-api (sal-bdd-fsm/make-trace-from-to-with-at-most fsm source-set target-set max-steps)
  :doc "Similar to @code{sal-bdd-fsm/make-trace-from-to}, but the trace an have at most @code{max-steps} steps."
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m)))
    (let loop ((frontier source-set)
               (reachable-states source-set)
               (layer-list '())
               (max-steps max-steps)
               (i 1))
      (collect!)
      (cond
       ((or (= max-steps 0) (bdd/false? frontier))
        (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
        #f)
       (else
        (let ((target-found (bdd/and target-set frontier)))
          (cond
           ((not (bdd/false? target-found))
            (verbose-message 3 "  number of visited states: ~a" (sal-bdd-fsm/num-states fsm reachable-states))
            (cons target-found layer-list))
           (else
            (verbose-message 2 "  iteration: ~a" i)
            (verbose-message 3 "  frontier: ~a nodes" (bdd/size frontier))
            (verbose-message 3 "  total bdd node count: ~a" (bdd/num-nodes m))
            (reorder-when-needed fsm reachable-states)
            (let* ((image (sal-bdd-fsm/image fsm frontier))
                   (new-reachable-states (bdd/or reachable-states image))
                   (new-frontier (bdd/diff image reachable-states))
                   (new-layer-list (cons frontier layer-list)))
              (loop new-frontier 
                    new-reachable-states 
                    new-layer-list 
                    (- max-steps 1)
                    (+ i 1)))))))))))

(define (sal-bdd-fsm/make-trace-to fsm target-set)
  :doc "Similar to @code{sal-bdd-fsm/make-trace-from-to}, but the initial set of states of the finite state machine is used as the @code{source-set}."
  (sal-bdd-fsm/make-trace-from-to fsm (sal-bdd-fsm/initial-states fsm) target-set))

(define-api (sal-bdd-fsm/make-backward-trace-from fsm source-set)
  :doc "See @code{sal-bdd-fsm/make-trace-to}."
  (let ((m (sal-bdd-fsm/manager fsm)))
    (multiple-value-bind
        (initial-subset visited-states trace)
        (sal-bdd-fsm/find-trace fsm sal-bdd-fsm/pre-image-with-choices source-set (sal-bdd-fsm/initial-states fsm) #t (bdd/true (sal-bdd-fsm/manager fsm)))
      trace)))

;; return the set of reachable states starting from a given set.
;; If starting-at is not provided the set of initial states is used.
;; The result is a BDD
(define-api (sal-bdd-fsm/reachable-states fsm)
  :doc "Returns the set of reachable states of the finite state machine @code{fsm}."
  (cond
   ((slot-value fsm :reachable-states) => identity)
   (else
    (sal-bdd-fsm/reachable-states-core fsm)
    (slot-value fsm :reachable-states))))

(define-api (sal-bdd-fsm/layered-reachable-states fsm)
  :doc "Returns a list of BDDs. The disjunction of these BDD represents the set of reachable states. The first BDD in the list represents the set of initial states, the second BDD is the set of states reached in one step from the set of initial states, and so on."
  (cond
   ((slot-value fsm :layered-reachable-states) => identity)
   (else
    (multiple-value-bind
        (rs layer-list)
        (sal-bdd-fsm/layered-reachable-states-core fsm)
      (set-slot-value! fsm :reachable-states rs)
      (set-slot-value! fsm :layered-reachable-states layer-list)
      layer-list))))

(define (sal-bdd-fsm/reachable-states-core fsm)
  (verbose-message 2 "  computing set of reachable states...")
  (display-runtime 2 "  time to compute set of reachable states: ~a secs"
    (lambda ()
      (let ((m (sal-bdd-fsm/manager fsm)))
        ;; use find-trace to compute the set of reachable states... using the empty target-set
        (multiple-value-bind
            (_ reachable-states _)
            (sal-bdd-fsm/find-trace fsm sal-bdd-fsm/image (slot-value fsm :initial-states) (bdd/false m) #f (bdd/true m))
          (set-slot-value! fsm :reachable-states reachable-states)
          reachable-states)))))

;; Remark: state-set must not contain next-variables
(define-api (sal-bdd-fsm/num-states fsm state-set)
  :doc "Returns the number of states in the set represented by the BDD @code{state-set}."
  [assert (state-set) (bdd/eq? state-set (bdd/exists state-set (slot-value fsm :next-cube)))]
  (bdd/num-solutions (sal-bdd-fsm/remove-choice-vars fsm state-set) 
                     (sal-bdd-fsm/num-non-choice-vars fsm)))
  
(define-api (sal-bdd-fsm/num-reachable-states fsm)
  :doc "Returns the number of reachable states in the finite state machine."
  (sal-bdd-fsm/num-states fsm (sal-bdd-fsm/reachable-states fsm)))

;; Remark: state-set must not contain next-variables
(define-api (sal-bdd-fsm/peek-state fsm state-set)
  :doc "Peek an arbitrary state in the set represented by the BDD @code{state-set}. The result is a minterm BDD."
  [assert (state-set) (bdd? state-set)]
  (bdd/peek-one-min-term state-set 
                         (slot-value fsm :curr-var-array)
                         (sal-bdd-fsm/num-vars fsm)))

(define-api (sal-bdd-fsm/peek-state-without-choices fsm state-set)
  :doc "Similar to @code{sal-bdd-fsm/peek-state}, but choice variables are not included."
  (bdd/peek-one-min-term (sal-bdd-fsm/remove-choice-vars fsm state-set)
                         (slot-value fsm :no-choice-var-array)
                         (sal-bdd-fsm/num-non-choice-vars fsm)))

;; Remark: state-set must not contain next-variables
(define-api (sal-bdd-fsm/peek-states fsm state-set num-states)
  :doc "Returns a list of BDD minterms. The list will have at most @code{num-states} elements. Each BDD minterm represent an element of the set represented by the BDD @code{state-set}."
  (bdd/peek-min-terms state-set 
                      (slot-value fsm :curr-var-array)
                      (sal-bdd-fsm/num-vars fsm)
                      num-states))

(define-api (sal-bdd-fsm/peek-states-without-choices fsm state-set num-states)
  :doc "Similar to @code{sal-bdd-fsm/peek-states}, but choice variables are not included."
  (bdd/peek-min-terms (sal-bdd-fsm/remove-choice-vars fsm state-set)
                      (slot-value fsm :no-choice-var-array)
                      (sal-bdd-fsm/num-non-choice-vars fsm)
                      num-states))

;; convert a BDD minterm (state) in an assignment table
(define (sal-bdd-fsm/state->assignment-table fsm bdd-state)
  (let* ((bdd-manager (slot-value fsm :bdd-manager))
         (flat-module (slot-value fsm :flat-module))
         (state-vars (slot-value flat-module :state-vars))
         (curr-vars-table (slot-value fsm :curr-vars))
         (assignment-table (make-eq-hash-table)))
    (for-each (lambda (state-var-decl)
                (let* ((bdd-var-value (cond
                                       ((eq-hash-table/get curr-vars-table state-var-decl) =>
                                        cdr)
                                       (else
                                        (internal-error))))
                       (_ [assert (bdd-manager bdd-var-value) (bdd? bdd-var-value)])
                       (var-value (bdd/le? bdd-state bdd-var-value))) ;; the default value will be false
                  (eq-hash-table/put! assignment-table
                                      state-var-decl
                                      (if var-value
                                        (make-sal-true state-var-decl)
                                        (make-sal-false state-var-decl)))))
              state-vars)
    assignment-table))

;; state-list must be a list of BDD minterms
(define (sal-bdd-fsm/for-each-state fsm proc state-list)
  (let* ((bool-assignment-table-list (map (cut sal-bdd-fsm/state->assignment-table fsm <>) state-list))
         (bool-flat-module (slot-value fsm :flat-module))
         (assignment-table-list (map (cut boolean-assignment-table->assignment-table <> bool-flat-module) bool-assignment-table-list)))
    (for-each proc assignment-table-list)))

;; state-list must be a list of BDD minterms
(define-api (sal-bdd-fsm/display-state-list fsm state-list hide-locals? hide-input?)
  :doc "Print the minterm BDD list @code{state-list}. A minterm BDD list can be produced using the procedure @code{sal-bdd-fsm/peek-states}. If @code{hide-locals?} is @code{#t}, then the local variables are not displayed. If @code{hide-input?} is @code{#t}, then the input variables are not displayed."
  (let* ((bool-flat-module (slot-value fsm :flat-module))
         (flat-module (sal-derived-flat-module/original-flat-module bool-flat-module))
         (i 1))
    (sal-bdd-fsm/for-each-state fsm (lambda (assignment-table)
                                      (print "State " i)
                                      (set! i (+ i 1))
                                      (sal-flat-module/display-variables flat-module assignment-table #f hide-locals? hide-input?)
                                      (print "-----------------------------"))
                                state-list)
    #unspecified))

(define (check-num-states-limit fsm states max)
  (let ((num-states (sal-bdd-fsm/num-states fsm states)))
    (when (> num-states max)
      (print "Only " max " of " num-states " states were displayed."))))

(define (check-empty-states states)
  (when (bdd/false? states)
    (sign-error "Cannot display states, the given set of states is empty.")))
  

;; states must be a BDD
(define-api (sal-bdd-fsm/display-states fsm states max-states hide-locals?)
  :doc "Display at most @code{max-states} states from the set represented by the BDD @code{states}. if @code{hide-locals?} is @code{#t}, then input variables are not displayed."
  (check-empty-states states)
  (let* ((state-list (sal-bdd-fsm/peek-states fsm states max-states)))
    (sal-bdd-fsm/display-state-list fsm state-list hide-locals? #t)
    (check-num-states-limit fsm states max-states)
    #unspecified))

;; states must be a BDD
(define-api (sal-bdd-fsm/display-states-without-choices fsm states max-states hide-locals?)
  :doc "Similar to @code{sal-bdd-fsm/display-states}, but the choice variables are not included."
  (check-empty-states states)
  (let* ((state-list (sal-bdd-fsm/peek-states-without-choices fsm states max-states)))
    (sal-bdd-fsm/display-state-list fsm state-list hide-locals? #f)
    (check-num-states-limit fsm states max-states)
    #unspecified))

(define-api (sal-bdd-fsm/display-transition fsm state1 state2)
  :doc "Display a transition from @code{state1} to @code{state2}. @code{state1} and @code{state2} must be BDDs representing sets of states, and they must satisfy the following condition: every state in the set @code{state2} must have a predecessor in the set @code{state1}."
  (let* ((bool-flat-module (slot-value fsm :flat-module))
         (flat-module (sal-derived-flat-module/original-flat-module bool-flat-module))
         (trace-info (slot-value flat-module :transition-trace-info))
         (state2 (sal-bdd-fsm/peek-state fsm state2))
         (state1-with-inputs (bdd/and state1 (sal-bdd-fsm/pre-image-with-choices fsm state2)))
         (assignment-table1 (boolean-assignment-table->assignment-table (sal-bdd-fsm/state->assignment-table fsm state1-with-inputs) bool-flat-module))
         (assignment-table2 (boolean-assignment-table->assignment-table (sal-bdd-fsm/state->assignment-table fsm state2) bool-flat-module)))
    ;; (breakpoint "foo" (state1-values state2-values state2 state1-with-inputs m flat-module fsm bool-flat-module) #t)
    (sal-flat-module/display-variables flat-module assignment-table1 #f #f #f)
    (sal-trace-info/display trace-info assignment-table1 flat-module #t)
    (sal-flat-module/display-variables flat-module assignment-table2 #f #f #f)
    #unspecified))
         
(define (make-step-info fsm state)
  (let ((curr-assignment-table (sal-bdd-fsm/state->assignment-table fsm state)))
    (make-instance <sal-step> 
                   :assignment-table curr-assignment-table
                   :constraint-list '())))

(define (sign-failed-to-build-counter-example)
  (sign-error "Failed to build trace, your specification seems to have a type error, please, use the SAL-PVS type checker (if available). A common reason for this error is related to the use of subranges, for instance, a variable X has type [1..3], but there is a state where X is 0. You can use sal-bmc to obtain the trace which contains invalid states."))

(define (sal-bdd-fsm/trace->step-list fsm trace forward?)
  [assert (trace) (not (null? trace))]
  (let* ((step-info-list '())
         (add-step!
          (lambda (state)
            (push! (make-step-info fsm state) step-info-list)))
         (last-states (car trace))
         (last-state (sal-bdd-fsm/peek-state fsm last-states))
         ;; if the trace was built using forward execution, we have to use
         ;;; pre-image to construct the counter-example.
         (image-proc (if forward? sal-bdd-fsm/pre-image-core sal-bdd-fsm/image-core))
         (m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m)))
    (let loop ((curr-state last-state)
               (trace (cdr trace))
               (i 1))
      (status-message :smc-generating-counterexample-step i)
      (verbose-message 10 "  generating step: ~a..." i)
      (collect!)
      (add-step! curr-state)
      (unless (null? trace)
        (let* ((care-set (car trace))
               (tmp (image-proc fsm curr-state curr-state care-set)))
          (when (bdd/false? tmp)
            (sign-failed-to-build-counter-example))
          [assert (fsm tmp curr-state) (not (bdd/false? tmp))]
          (let* ((curr-bdd (car trace))
                 (prev-states (bdd/and curr-bdd tmp)))
            (when (bdd/false? prev-states)
              (sign-failed-to-build-counter-example))
            (let ((prev-state (sal-bdd-fsm/peek-state fsm prev-states)))
              (loop prev-state
                    (cdr trace)
                    (+ i 1))))))
      (if forward?
        step-info-list
        (reverse step-info-list)))))

;; Build a step list for a state in from-states to a state in to-states.
;; The set restriction contains all states in the paths from from-states to to-states.
;; This set is used as an optimization.
;; from-states, to-states, and restriction are BDDs. 
;; Return #f, if it failed to create such step-list.
(define (sal-bdd-fsm/make-step-list fsm from-states to-states restriction)
  (if (bdd/false? to-states)
    #f
    (let* ((step-info-list '())
           (add-step! (lambda (state)
                        (push! (make-step-info fsm state) step-info-list)))
           (to-states (bdd/and to-states restriction))
           (from-states (bdd/and from-states restriction))
           (to-state (sal-bdd-fsm/peek-state fsm to-states)))
      (let loop ((curr-state to-state)
                 (not-used-states (bdd/not to-state))
                 (i 1))
        (status-message :smc-generating-counterexample-step i)
        (verbose-message 10 "  generating step: ~a..." i)
        (cond
         ((bdd/false? curr-state)
          #f)
         ((not (bdd/false? (bdd/and curr-state from-states)))
          (add-step! curr-state)
          step-info-list)
         (else
          (add-step! curr-state)
          (let* ((tmp (sal-bdd-fsm/pre-image-core fsm curr-state curr-state restriction))
                 (prev-states (bdd/and (bdd/and tmp restriction) not-used-states)))
            (if (bdd/false? prev-states)
              #f
              (let ((prev-state (sal-bdd-fsm/peek-state fsm prev-states)))
                (loop prev-state
                      (bdd/and not-used-states (bdd/not prev-state))
                      (+ i 1)))))))))))

(define-api (sal-bdd-fsm/make-path fsm trace forward?)
  :doc "Returns a SAL path object (see @code{<sal-path>}). @code{trace} is a list of BDDs representing set of paths. A trace can be produced using procedures such as: @code{sal-bdd-fsm/make-trace-from-to} and @code{sal-bdd-fsm/make-backward-trace-from}. @code{forward?} must be @code{#t}/@code{#f} if the trace was built using forward/backward reachability."
  (make-instance <sal-concrete-path>
                 :flat-module (slot-value fsm :flat-module)
                 :step-info-list (sal-bdd-fsm/trace->step-list fsm trace forward?)
                 :auxiliary-decls '() 
                 :global-constraint-list '()))

(define (sal-bdd-fsm/make-path-from-to fsm from-states to-state restriction)
  (make-instance <sal-concrete-path>
                 :flat-module (slot-value fsm :flat-module)
                 :step-info-list (sal-bdd-fsm/make-step-list fsm from-states to-state restriction)
                 :auxiliary-decls '() 
                 :global-constraint-list '()))

(define-api (sal-bdd-fsm/deadlock-states fsm)
  :doc "Returns the set of states that are in deadlock. A state is in deadlock if it does not have successors. In SAL, all states must have at least one successor. If the state is in deadlock, your specification must be fixed."
  (status-message :smc-deadlock-checking)
  (verbose-message 1 "detecting deadlock states...")
  (display-runtime 2 "  deadlock state detection time: ~a secs"
    (lambda ()
      (when (bdd/false? (sal-bdd-fsm/initial-states fsm))
        (sign-error "Invalid module, the set of initial states is empty."))
      (let* ((m (sal-bdd-fsm/manager fsm))
             (rs (sal-bdd-fsm/reachable-states fsm))
             ;; computes (EX true)
             (valid-states (sal-bdd-fsm/filter-invalid-state-rep fsm (bdd/true m)))
             (tmp-states-with-successors (sal-bdd-fsm/pre-image-core fsm valid-states valid-states rs))
             (states-with-successors (sal-bdd-fsm/remove-choice-vars fsm tmp-states-with-successors))
             ;; intersects reachable states and states without successors
             (aux (bdd/and rs (bdd/not states-with-successors)))
             (result (sal-bdd-fsm/filter-invalid-state-rep fsm aux)))
        result))
    :deadlock-checking-time))
    
;; return a closed subset of state-set.
;; A set S is closed iff S=image(S)
;;
;; Remark: state-set must not contain input and next variables.
(define-api (sal-bdd-fsm/closed-subset fsm state-set)
  :doc "Returns a closed subset of @code{state-set}. We say a set @code{S} is closed if the image of @code{S} is @code{S}. @code{state-set} is a BDD."
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m))
         (backward-cube (sal-bdd-fsm/backward-cube fsm))
         (T (slot-value fsm :transition-relation-cluster))) ;; cube of next-variables
    (let loop ((S state-set)
               (i 0))
      (verbose-message 3 "    closed-set iteration: ~a" i)
      (collect!)
      (let* ((S' (sal-bdd-fsm/map-curr->next fsm S))
             (not-S' (bdd/not S'))
             (S-and-not-S' (bdd/and S not-S'))
             (not-closed (sal-bdd-fsm/remove-choice-vars fsm (sal-bdd-cluster/pre-image T S-and-not-S' (bdd/true m) backward-cube))))
        ;; not-closed is the set of states NC(x) such that
        ;; NC(x) = Exists x' . S(x) and (not S(x')) and T(x,x')
        (if (bdd/false? not-closed) ;; empty set
          S
          (let ((new-S (bdd/and S (bdd/not not-closed))))
            (loop new-S (+ i 1))))))))

;; compute the set of states that satisfy (EX p) restricted to r
;; where, p is a set between p-lower <= p <= p-upper
(define (sal-bdd-fsm/EX-core fsm p-lower p-upper r)
  ;; use care-sets to optimize the computation
  (let ((pre-p (sal-bdd-fsm/remove-choice-vars fsm (sal-bdd-fsm/pre-image-core fsm p-lower p-upper r))))
    (bdd/and pre-p r)))

;----------------------------------------------------------------
;
; CTL Symbolic Model Checking
;
;----------------------------------------------------------------
  
;; compute the set of states that satisfy (EX p) restricted to r
(define (sal-bdd-fsm/EX fsm p r)
  (sal-bdd-fsm/EX-core fsm p p r))

;; compute the set of states that satisfy (AX p) restricted to r
(define (sal-bdd-fsm/AX fsm p r)
  (bdd/and (bdd/not (sal-bdd-fsm/pre-image fsm (bdd/not p))) r))

;; compute the set of states that satisfy (EU q p) restricted to r
;; The following function is correct because EX distributes over disjunctions.
;; Remark: AX doesn't distribute over disjunctions.
(define (sal-bdd-fsm/EU fsm q p r)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m))
         (p-r (bdd/and p r)))
    (let loop ((S p-r) ;; frontier
               (R p-r)
               (i 0))
      (display-iteration-info "  EU iteration: ~a" i m S)
      (collect!)
      (if (bdd/false? S)
        R
        (let* ((care-set r) ;; (bdd/and r (bdd/not R)))
               (tmp (bdd/and q (sal-bdd-fsm/EX-core fsm S R care-set)))
               (new-S (bdd/and tmp (bdd/not R)))
               (new-R (bdd/or R new-S)))
          (loop new-S new-R (+ i 1)))))))

;; compute the set of states that satisfy (AU q p) restricted to r
(define (sal-bdd-fsm/AU fsm q p r)
  (let ((not-p (bdd/not p))
        (not-q (bdd/not q)))
    (bdd/and (bdd/not (bdd/or (sal-bdd-fsm/EU fsm not-p (bdd/and not-p not-q) r)
                              (sal-bdd-fsm/EG fsm not-p r)))
             r)))

;; compute the set of states that satisfy (EF p) restricted to r
(define (sal-bdd-fsm/EF fsm p r)
  (let ((m (sal-bdd-fsm/manager fsm)))
    (sal-bdd-fsm/EU fsm (bdd/true m) p r)))

;; compute the set of states that satisfy (AF p) restricted to r
(define (sal-bdd-fsm/AF fsm p r)
  (let ((m (sal-bdd-fsm/manager fsm)))
    (sal-bdd-fsm/AU fsm (bdd/true m) p r)))

;; compute the set of states that satisfy (EG p) restricted to r
(define (sal-bdd-fsm/EG fsm p r)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m)))
    (let loop ((S (bdd/and p r))
               (old-S (bdd/false m))
               (i 0))
      (verbose-message 2 "  EG iteration: ~a" i)
      (verbose-message 3 "    result size: ~a nodes, total node count: ~a" (bdd/size S) (bdd/num-nodes m))
      (collect!)
      (if (bdd/eq? S old-S)
        S
        (let* ((pre-S (sal-bdd-fsm/EX fsm S r)) 
               (new-S (bdd/and S pre-S)))
          (loop new-S S (+ i 1)))))))

;; compute the set of states that satisfy (AG p) restricted to r
(define (sal-bdd-fsm/AG fsm p r)
  (bdd/and (bdd/not (sal-bdd-fsm/EF fsm (bdd/not p) r)) r))
  
;; computes (EG p) restricted to r, under the fairness constrains fc
;; fc is a list of BDDs.
(define (sal-bdd-fsm/EG-fair fsm p r fc)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m)))
    (let loop ((S (bdd/and p r))
               (old-S (bdd/false m))
               (i 0))
      (verbose-message 2 "  EG-fair iteration: ~a" i)
      (verbose-message 3 "    result size: ~a nodes, total node count: ~a" (bdd/size S) (bdd/num-nodes m))
      (collect!)
      (if (bdd/eq? S old-S)
        S
        (let* ((fair-S (fair-iter fsm S fc r))
               (pre-fair-S (sal-bdd-fsm/EX fsm fair-S (bdd/true m)))
               (new-S (bdd/and S pre-fair-S)))
          (loop new-S S (+ i 1)))))))

;; computes [(EU y (and y c_1)) and (EU y (and y c_2)) ... and (EU y (and y c_n))] restricted to r
;; where c_1, ... c_n are the element of the list fc
(define (fair-iter fsm y fc r)
  (let loop ((fc fc)
             (result r))
    (if (null? fc)
      result
      (let* ((curr-c (car fc))
             (y-c (bdd/and y curr-c))
             (curr-eu (sal-bdd-fsm/EU fsm y y-c r))
             (new-result (bdd/and result curr-eu)))
        (loop (cdr fc)
              new-result)))))

;----------------------------------------------------------------
;
; Bounded CTL Symbolic Model Checking
;
;----------------------------------------------------------------
    
(define (sal-bdd-fsm/EBU fsm q p inf sup r)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m)))
    (if (or (> inf sup) (< inf 0))
      (bdd/false m)
      (let ((p-r (bdd/and p r)))
        ;; compute R = (or p (and q (X R))) for the states in the bound
        (let ((R1 (let loop ((S p-r) ;; frontier
                             (R p-r)
                             (i sup))
                    (display-iteration-info "  EBU/ABU step: ~a" i m S)
                    (collect!)
                    (cond
                     ((bdd/false? S)
                      R) ;; fixpoint found
                     ((= i inf)
                      R) ;; [inf..sup] was covered
                     (else
                      (let* ((tmp (bdd/and q (sal-bdd-fsm/EX fsm S r)))
                             (new-S (bdd/and tmp (bdd/not R)))
                             (new-R (bdd/or R new-S)))
                        (loop new-S new-R (- i 1))))))))
          ;; compute R = (and q (X R)) for the states before the bound
          (let loop ((S R1)
                     (old-S (bdd/false m))
                     (i inf))
            (verbose-message 2 "  EBU/ABU prefix step: ~a" i)
            (verbose-message 3 "    result size: ~a nodes, total node count: ~a" (bdd/size S) (bdd/num-nodes m))
            (cond
             ((bdd/eq? S old-S)
              S) ;; fixpoint found
             ((= i 0)
              S) ;; [0..inf] was covered
             (else
              (let* ((tmp (bdd/and q (sal-bdd-fsm/EX fsm S (bdd/true m))))
                     (new-S (bdd/and S tmp)))
                (loop new-S S (- i 1)))))))))))
                
(define (sal-bdd-fsm/ABU fsm q p inf sup r)
  (sign-error "Feature not implemented yet."))

;; compute the set of states that satisfy (EUF p inf sup) restricted to r
(define (sal-bdd-fsm/EUF fsm p inf sup r)
  (let ((m (sal-bdd-fsm/manager fsm)))
    (sal-bdd-fsm/EBU fsm (bdd/true m) p inf sup r)))

;; compute the set of states that satisfy (AUF p inf sup) restricted to r
(define (sal-bdd-fsm/AUF fsm p inf sup r)
  (let ((m (sal-bdd-fsm/manager fsm)))
    (sal-bdd-fsm/ABU fsm (bdd/true m) p inf sup r)))

;----------------------------------------------------------------
;
; Forward/Backward LTL Symbolic Model Checking
;
;----------------------------------------------------------------

;; computes the set of final states that are in buchi accepting paths...
;; A path is buchi accepting if it contains final states infinitely often.
(define-api (sal-bdd-fsm/final-accepting-states fsm final weak? forward?)
  :doc "Returns the set of final states (subset of @code{final}) which are in Buchi accepting paths. @code{weak?} is @code{#t} if the Buchi automata embedded in the finite state machine is weak. if @code{forward?} is @code{#t}/@code{#f}, then forward/backward reachability is used."
  (cond
   ((and forward? weak?)
    (sal-bdd-fsm/final-accepting-states-weak-forward fsm final))
   (forward? 
    (sal-bdd-fsm/final-accepting-states-forward fsm final))
   (weak?
    (sal-bdd-fsm/final-accepting-states-weak-backward fsm final))
   (else
    (sal-bdd-fsm/final-accepting-states-backward fsm final))))

(define (sal-bdd-fsm/final-accepting-states-backward fsm final)
  (let* ((m (sal-bdd-fsm/manager fsm))
         (collect! (make-gc-collector-for m))
         (rs (sal-bdd-fsm/reachable-states fsm)))
    (let loop ((S (bdd/true m))
               (old-S (bdd/false m))
               (i 0))
      (status-message :backward-fair-cycle-detection-iteration i)
      (verbose-message 2 "  fair cycle detection: ~a" i)
      (status-message :backward-fair-cycle-detection (bdd/size S) (bdd/num-nodes m))
      (verbose-message 3 "    result size: ~a nodes, total node count: ~a" (bdd/size S) (bdd/num-nodes m))
      (collect!)
      (if (bdd/eq? S old-S)
        (bdd/and S final) ;; fixpoint found
        (let* ((EX-S (sal-bdd-fsm/EX fsm S rs))
               (EX-S-and-final (bdd/and EX-S final))
               (new-S (sal-bdd-fsm/EF fsm EX-S-and-final rs)))
          (loop new-S S (+ i 1)))))))

(define (sal-bdd-fsm/final-accepting-states-forward fsm final)
  (let* ((rs (sal-bdd-fsm/reachable-states fsm))
         (m (sal-bdd-fsm/manager fsm))
         (not-final (bdd/not final))
         (rs-final (bdd/and rs final)) ;; set of final reachable states
         (collect! (make-gc-collector-for m)))
    (let loop ((S rs-final)
               (i 0))
      [assert (S final) (bdd/le? S final)]
      ;; greatest fixpoint
      (status-message :fair-cycle-detection-iteration i)
      (verbose-message 2 "  fair cycle detection (forward): ~a" i)
      (status-message :fair-cycle-detection (bdd/size S) (bdd/num-nodes m))
      (verbose-message 3 "    result size: ~a nodes, total node count: ~a" (bdd/size S) (bdd/num-nodes m))
      ;; (bdd/print-debug S)
      (collect!)
      (let* ((X (bdd/and (sal-bdd-fsm/image fsm S) not-final)) ;; states reached from S that are not final
             (non-final-rs ;; reached states from S that are not final
              ;; nested least fixpoint
              (let loop2 ((lower X) ;; frontier lower bound
                          (upper X) ;; visited states
                          (j 0))
                [assert (lower not-final) (bdd/le? lower not-final)]
                [assert (upper not-final) (bdd/le? upper not-final)]
                (cond
                 ((bdd/false? lower)
                  upper)
                 (else
                  (status-message :fair-cycle-detection-nested-fixpoint j)
                  (verbose-message 3 "    nested fixpoint (forward): ~a" j)
                  (status-message :fair-cycle-detection (bdd/size lower) (bdd/size upper))
                  (verbose-message 3 "      frontier lower bound: ~a nodes, upper bound: ~a nodes" (bdd/size lower) (bdd/size upper))
                  (let ((frontier (bdd/between lower upper)))
                    (verbose-message 3 "      using frontier with ~a nodes" (bdd/size frontier))
                    (verbose-message 3 "      total bdd node count: ~a" (bdd/num-nodes m))
                    (let* ((image (sal-bdd-fsm/image fsm frontier))
                           (image-not-final (bdd/and image not-final))
                           (new-upper (bdd/or image-not-final upper))
                           (new-lower (bdd/and image-not-final (bdd/not upper))))
                      (loop2 new-lower new-upper (+ j 1))))))))
             (image-S (sal-bdd-fsm/image fsm S)) 
             (image-S-final (bdd/and image-S final)) ;; states reached from S that are final
             (image-non-final-rs (sal-bdd-fsm/image fsm non-final-rs)) ;; states reached from non-final-rs 
             (image-non-final-rs-final (bdd/and image-non-final-rs final)) ;; states reached from non-final-rs that are final
             (new-S (bdd/or image-S-final image-non-final-rs-final)))
        (if (bdd/eq? S new-S)
          S ;; fixpoint reached
          (loop new-S (+ i 1)))))))

(define (sal-bdd-fsm/final-accepting-states-weak-backward fsm final)
  (let ((rs (sal-bdd-fsm/reachable-states fsm)))
    ;; computes the set of states that in buchi accepting paths of weak buchi automata...
    ;; A buchi automata is weak, if final states only have transitions to themselves.
    ;; A path is buchi accepting if it contains final states infinitely often.
    ;; Since the buchi automata is weak, we can compute the result using the following
    ;; formula:
    ;;   (EF (EG final))
    (status-message :backward-weak-fair-cycle-detection)
    (verbose-message 2 "  weak fair cycle detection (backward)...")
    (bdd/and (sal-bdd-fsm/EG fsm final rs) rs)))
  
(define (sal-bdd-fsm/final-accepting-states-weak-forward fsm final)
  (let* ((rs (sal-bdd-fsm/reachable-states fsm))
         (m (sal-bdd-fsm/manager fsm))
         (rs-final (bdd/and rs final))
         (collect! (make-gc-collector-for m)))
    ;; (sal-bdd-fsm/display-states-without-choices fsm rs-final 1000 #f)
    (let loop ((S rs-final)
               (old-S (bdd/false m))
               (i 0))
      (status-message :weak-fair-cycle-detection i)
      (verbose-message 2 "  weak fair cycle detection (forward): ~a" i)
      (status-message :fair-cycle-detection (bdd/size S) (bdd/num-nodes m))
      (verbose-message 3 "    result size: ~a nodes, total node count: ~a" (bdd/size S) (bdd/num-nodes m))
      ;; (sal-bdd-fsm/display-states-without-choices fsm S 1000 #f)
      ;; (print "===================================================")
      ;; (bdd/print-debug S)
      (collect!)
      (cond
       ((bdd/eq? S old-S)
        [assert (final S) (bdd/le? S final)]
        S) ;; fixpoint was found
       (else
        (let* ((care-set final) ;; I only care for final states
               (image-S (sal-bdd-fsm/image-core fsm S S care-set)) 
               (image-S-final (bdd/and image-S final)))
          [assert (image-S-final S rs-final) (bdd/le? image-S-final S)]
          [assert (image-S-final rs-final) (bdd/le? image-S-final rs-final)]
          (loop image-S-final S (+ i 1))))))))
  
;----------------------------------------------------------------
;
; Cycle generation
;
;----------------------------------------------------------------
;; state-list state describes a path which contains a cycle.
;; This function will partition the path in a prefix, and a cycle.
(define (prefix-and-cycle state-list state)
  (let ((state-list (reverse state-list))
        (prefix-queue (make-queue)))
    (let loop ((state-list state-list))
      (cond
       ((null? state-list)
        (internal-error))
       ((bdd/eq? state (car state-list))
        (queue/insert! prefix-queue (car state-list))
        (values (queue->list prefix-queue)
                (append state-list (list state))))
       (else
        (queue/insert! prefix-queue (car state-list))
        (loop (cdr state-list)))))))

(define-api (sal-bdd-fsm/make-accepting-cyclic-path fsm accepting-final-states)
  :doc "Returns a cyclic SAL path (see @code{<sal-cyclic-path>}) such that the cycle contains a state from @code{accepting-final-states}. @code{accepting-final-states} is a BDD."
  (let ((accepting-final-states (sal-bdd-fsm/remove-choice-vars fsm accepting-final-states))
        (m (sal-bdd-fsm/manager fsm))
        (prefix-trace (sal-bdd-fsm/make-trace-to fsm accepting-final-states)))
    [assert (prefix-trace) (and prefix-trace (not (null? prefix-trace)))]
    [assert (prefix-trace) (for-all bdd? prefix-trace)]
    (let ((final-states (car prefix-trace)))
      [assert (final-states) (bdd? final-states)]
      [assert (final-states accepting-final-states) (bdd/le? final-states accepting-final-states)]
      ;; The following loop is a hack to fix a bug that occurs when sal-smc tries to build a counterexample for liveness properties.
      ;; This bug shouldn't happen, and it is unclear the real source of it. In principle any state of accepting-final-states is
      ;; in a cycle.
      (let loop ((final-states final-states))
        (when (bdd/false? final-states)
          (sign-error "Failed to build counterexample, the property is INVALID, use sal-bmc to obtain the counterexample."))
        (let* ((F (sal-bdd-fsm/remove-choice-vars fsm (sal-bdd-fsm/peek-state fsm final-states))) ;; arbitrary final state
               (image-F (sal-bdd-fsm/image fsm F)) ;; compute the successors of F
               (cycle-trace (sal-bdd-fsm/make-trace-from-to fsm image-F F)))
          (cond
           (cycle-trace
            [assert (F accepting-final-states) (bdd/le? F accepting-final-states)]
            [assert (fsm accepting-final-states F image-F cycle-trace final-states) (and cycle-trace (not (null? cycle-trace)))]
            (let ((prefix-step-list (sal-bdd-fsm/trace->step-list fsm (cons F (cdr prefix-trace)) #t))
                  (cycle-step-list (sal-bdd-fsm/trace->step-list fsm (append cycle-trace (list F)) #t)))
              (make-instance <sal-cyclic-path>
                             :flat-module (slot-value fsm :flat-module)
                             :step-info-list prefix-step-list
                             :cycle-step-info-list cycle-step-list)))
           (else
            (let ((new-final-states (bdd/diff final-states F)))
              (loop new-final-states)))))))))
      
