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

(sal/load-source! "sal-script-util.scm")

(open-api-group! :name "SAL Simulator" :doc "BDD based simulator")

(define-api (import! (ctx-name string?))
  :doc "Import (load) a SAL context."
  (sal/import ctx-name))

(define-api (set-sal-syntax!)
  :doc "Set the SAL default syntax."
  (sal/set-sal-pp-proc! sal-ast->sal-doc)
  (sal/set-make-sal-string-reader-proc! make-sal-string-reader))

(define-api (set-lsal-syntax!)
  :doc "Set the LSAL syntax."
  (sal/set-sal-pp-proc! sal-ast->lsal-doc)
  (sal/set-make-sal-string-reader-proc! make-lsal-string-reader))

(set-sal-syntax!) ;; Now SAL syntax is the default input syntax

(define-api (flat-module->fsm (flat-module <sal-boolean-flat-module>))
:doc "Convert a boolean flat module is a finite state machine.
The finite state machine is encoded using BDDs."
  (sal-flat-module->sal-bdd-fsm flat-module))

(define-api (make-fsm (module-str string?))
  :doc "Build a finite state machine for a SAL module expression.
@code{module-str} must be a string. The default syntax is LSAL.
To switch to SAL syntax, use (set-sal-reader!)."
  :examples '((define fsm (make-fsm "peterson!system"))
              (begin
                (import! "four-slot")
                (define fsm (make-fsm "system"))))
  (make-bdd-fsm module-str))

(define-api (initial-states (fsm <sal-bdd-fsm>))
  :doc "Return the set of initial states of a finite state machine.
The result is a BDD."
  :examples '((begin
                (define fsm (make-fsm "peterson!system"))
                (define initial (initial-states fsm))
                (display-states fsm initial)))
  (slot-value fsm :initial-states))

(define-api (image (fsm <sal-bdd-fsm>) (states <bdd>))
  :doc "Compute the forward image of a set of states. The result is a BDD."
  :examples '((begin
                (define fsm (make-fsm "peterson!system"))
                (define initial (initial-states fsm))
                (display-states fsm (image fsm initial))))
  (sal-bdd-fsm/image fsm states))

(define-api (pre-image-with-choices (fsm <sal-bdd-fsm>) (states <bdd>))
  :doc "Compute the backward image of a set of states including traceability information. The result
is a BDD."
  (sal-bdd-fsm/pre-image-with-choices fsm states))

(define-api (pre-condition (fsm <sal-bdd-fsm>) (states <bdd>))
  :doc "Compute the pre-condition for a set of states. The result is a BDD."
  (sal-bdd-fsm/pre-condition fsm states))

(define-api (pre-image (fsm <sal-bdd-fsm>) (states <bdd>))
  :doc "Compute the backward image of a set of states. The result is a BDD."
  (sal-bdd-fsm/pre-image fsm states))

(define-api (max-subset (fsm <sal-bdd-fsm>) (states <bdd>) (cost string?))
  :doc "Return a subset of the given states. The returned subset contains the maximums with respect to the given 'cost' expression. The argument states must be a BDD, and 'cost' a valid state expression of type integer."
  (sal-bdd-fsm/max-subset fsm states cost))

(define-api (min-subset (fsm <sal-bdd-fsm>) (states <bdd>) (cost string?))
  :doc "Return a subset of the given states. The returned subset contains the minimums with respect to the given 'cost' expression. The argument states must be a BDD, and 'cost' a valid state expression of type integer."
  (sal-bdd-fsm/min-subset fsm states cost))

(define-api (reachable-states (fsm <sal-bdd-fsm>))
  :doc "Return the set of reachable states. The result is a BDD."
  :examples '((begin
                (define fsm (make-fsm "peterson!system"))
                (define rs (reachable-states fsm))
                (display-states fsm rs 100)))
  (sal-bdd-fsm/reachable-states fsm))

(define-api (num-reachable-states (fsm <sal-bdd-fsm>))
  :doc "Return the number of reachable states."
  :examples '((begin
                (define fsm (make-fsm "peterson!system"))
                (num-reachable-states fsm)))
  (sal-bdd-fsm/num-reachable-states fsm))

(define-api (peek-state (fsm <sal-bdd-fsm>) (states <bdd>))
  :doc "Peek a state in the set @code{states}. The result is a 
minterm (BDD). The support of @code{states} must contain only current variables."
  (sal-bdd-fsm/peek-state fsm states))

(define-api (peek-state-without-choices (fsm <sal-bdd-fsm>) (states <bdd>))
  (sal-bdd-fsm/peek-state-without-choices fsm states))

(define-api (peek-states (fsm <sal-bdd-fsm>) (states <bdd>) (max integer?))
  :doc "Peek some states in the set @code{states}. @code{max} is the maximum
number of states to get. The result is a list of minterms (BDDs). The support of 
@code{states} must contain only current variables."
  (sal-bdd-fsm/peek-states fsm states max))

(define-api (peek-states-without-choices (fsm <sal-bdd-fsm>) (states <bdd>) (max integer?))
  (sal-bdd-fsm/peek-states-without-choices fsm states max))

(define-api (display-states (fsm <sal-bdd-fsm>) (states <bdd>) . max)
  :doc "Display at most @code{max} states in @code{states}. The default value of
@code{max} is 10."
  (let ((max (optional-arg max 10)))
    (sal-bdd-fsm/display-states-without-choices fsm states max #f)
    #unspecified))

(define-api (state-set-union (states1 <bdd>) (states2 <bdd>))
  :doc "Return a new set of states that contains the union of the given
sets. The result is a BDD."
  (bdd/or states1 states2))

(define-api (state-set-intersection (states1 <bdd>) (states2 <bdd>))
  :doc "Return a new set of states that contains the intersection of the given
sets. The result is a BDD."
  (bdd/and states1 states2))

(define-api (state-set-complement (states <bdd>))
  :doc "Return a new set of states that contains the complement of the given
set. The result is a BDD."
  (bdd/not states))

(define-api (deadlock-states (fsm <sal-bdd-fsm>))
  :doc "Return the set of states of a finite state machines that do not
contain successors. The result is a BDD."
  (sal-bdd-fsm/deadlock-states fsm))

(define-api (invalid-states (fsm <sal-bdd-fsm>))
  :doc "Return the set of states of a finite state machine which are invalid,
that is, they do not satisfy the type constraints in the SAL specification.
For instance, if a state variable @code{x} is declared to be in the range [3..7], 
but there is a state where its value is 0."
  (state-set-intersection (reachable-states fsm) 
                          (state-set-complement (slot-value fsm :valid-latch))))

(define-api (display-transition (fsm <sal-bdd-fsm>) (state1 <bdd>) (state2 <bdd>))
  :doc "Display a transition from @code{state1} to @code{state2}. If @code{state1}
and @code{state2} are not minterms, then random states in the sets are selected.
If there isn't a transition from @code{state1} to @code{state2}, then an error
is produced."
  (sal-bdd-fsm/display-transition fsm state1 state2)
  #unspecified)
    
(define-api (make-state-expr (expr-str string?) (fsm <sal-bdd-fsm>))
  :doc "Build a bdd for a SAL expression."
  :examples '((begin
                (import! "peterson")
                (define fsm (make-fsm "system"))
                (define expr1 (make-state-expr "pc1 = trying"))))
  (make-state-expression-bdd expr-str fsm))

(define-api (find-trace (fsm <sal-bdd-fsm>) (states1 <bdd>) (states2 <bdd>))
  :doc "Find a trace from the set @code{states1} to the set @code{states2}. The result
is a list of BDDs. Return @code{#f} if such trace doesn't exist."
  (sal-bdd-fsm/make-trace-from-to fsm states1 states2))

(define-api (display-trace (fsm <sal-bdd-fsm>) trace)
  :doc "Display a trace represented by a list of BDDs. If the list of BDDs represent
more than one trace, one of them is randomly chosen."
  (let* ((bool-flat-module (slot-value fsm :flat-module))
         (flat-module (sal-derived-flat-module/original-flat-module bool-flat-module))
         (bool-step-list (sal-bdd-fsm/trace->step-list fsm trace #t))
         (step-list (sal-bool-step-list->step-list bool-step-list bool-flat-module)))
    (sal-step-list/recover-executed-transition-info! step-list flat-module)
    (sal-step-list/display step-list flat-module #t #f #t #f 0)
    #unspecified))

(define *current-trace* '())
(define *current-fsm* #f)
(define *current-visited* #f)
(define *current-state-list* #f)

(define-api (start-simulation! obj)
  :doc "Start a new simulation. @code{obj} must be a string (SAL module),
a boolean flat module, or a finite state machine. A simulation is composed of:
  - a current trace (Actually a set of traces).
  - a current finite state machine.
  - a the set of already visited states."
  :examples '((start-simulation! "peterson!system")
              (begin
                (define fsm (make-fsm "peterson!system"))
                (start-simulation! fsm)))
  (let ((fsm (cond 
              ((instance-of? obj <sal-boolean-flat-module>)
               (flat-module->fsm obj))
              ((string? obj)
               (make-fsm obj))
              ((instance-of? obj <sal-bdd-fsm>)
               obj)
              (else
               (sign-error "Argument must be a: string (SAL module), a boolean flat module, or a finite state machine.")))))
    (set! *current-fsm* fsm)
    (set! *current-trace* (list (slot-value fsm :initial-states)))
    (set! *current-visited* (car *current-trace*))))

(define-api (current-fsm)
  :doc "Return the finite state machine used in the current (active) simulation."
  *current-fsm*)

(define-api (current-states)
  :doc "Return the frontier in the current (active) simulation."
  (check-simulation)
  (car *current-trace*))

(define-api (current-visited)
  :doc "Return the set of visited states in the current (active) simulation.
Remark: Only the procedures @code{start-simulation!} and @code{step!} will change
the value of the set of visited states."
  *current-visited*)

(define-api (current-trace)
  :doc "Return the current trace"
  *current-trace*)

(define-api (set-current-trace! new-trace)
  :doc "Set a new current trace, @code{new-trace} must be a list of BDDs"
  (check-simulation)
  (unless (and (list? new-trace)
               (not (null? new-trace))
               (for-all bdd? new-trace))
    (sign-error "Invalid argument, argument must be a (non-empty) list of BDDs"))
  (set! *current-trace* new-trace))

(define (check-simulation)
  (when (or (null? *current-trace*)
            (not *current-fsm*))
    (sign-error "There isn't an active simulation. Please use the function `start-simulation!'.")))

(define (check-num-states-limit fsm states max)
  (let ((num-states (sal-bdd-fsm/num-states fsm states)))
    (when (> num-states max)
      (print "Only " max " of " num-states " states were displayed.")
      (print "Remark: the command (display-curr-states <max>) can be used to display up to <max> states."))))

(define-api (display-curr-states . max)
  :doc "Display at most @code{max} states in the frontier of the current (active) simulation.
The default value of @code{max} is 10."
  :examples '((begin
                (start-simulation! "peterson!system")
                (step!)
                (display-curr-states)))
  (let ((max (optional-arg max 10)))
    (check-simulation)
    (let ((state-list (sal-bdd-fsm/peek-states-without-choices *current-fsm* (car *current-trace*) max)))
      (set! *current-state-list* state-list)
      (sal-bdd-fsm/display-state-list *current-fsm* state-list #f #f)
      (check-num-states-limit *current-fsm* (car *current-trace*) max)
      #unspecified)))

(define-api (backtrack!)
  :doc "Backtrack the current simulation, that is, drop the top of the current trace."
  :examples '((begin
                (start-simulation! "peterson!system")
                (display-curr-states)
                (step!)
                (display-curr-states)
                (backtrack!)
                (display-curr-states)))
  (check-simulation)
  (when (null? (cdr *current-trace*))
    (sign-error "Failed to backtrack: trace contains only the initial step."))
  (set! *current-trace* (cdr *current-trace*)))

(define-api (display-curr-trace)
  :doc "Display a current trace in the current (active) simulation. There is more than
one trace, one is randomly chosen."
  :examples '((begin
                (start-simulation! "peterson!system")
                (step!)
                (step!)
                (display-curr-trace)))
  (check-simulation)
  (display-trace *current-fsm* *current-trace*))

(define-api (step!)
  :doc "Execute one simulation step. The current trace, and the set of visited states
are updated."
  :examples '((begin
                (start-simulation! "peterson!system")
                (step!)
                (step!)
                (display-curr-trace)))
  (check-simulation)
  (when (bdd/false? (car *current-trace*))
    (sign-error "There is no current state. Please backtrack or reinitialize the simulation."))
  (set! *current-trace* (cons (image *current-fsm* (car *current-trace*)) *current-trace*))
  (set! *current-state-list* #f)
  (set! *current-visited* (state-set-union (car *current-trace*)
                                           *current-visited*)))

(define (obj->fsm-bdd obj fsm)
  (cond
   ((bdd? obj) ob)
   ((string? obj) (make-state-expr obj fsm))
   (else
    (sign-error "Argument must be a string (SAL expression), or a BDD."))))

(define-api (run! goal)
  :doc "Find a state which satisfies @code{goal} which is reachable from the current set of states. @code{goal} may be a BDD, or a string representing a state expression. The result is a boolean indicating whether a state satisfying @code{goal} is reachable or not."
  :examples '((begin
                (import! "peterson")
                (start-simulation! "system")
                (run! "pc1 = critical and pc2 = trying")
                (display-curr-trace)))
  (check-simulation)
  (let* ((goal-bdd (obj->fsm-bdd goal *current-fsm*)) 
         (trace (find-trace *current-fsm* (car *current-trace*) goal-bdd)))
    (cond
     ((and trace (not (null? trace)))
      (for-each (lambda (bdd)
                  (set! *current-trace* (cons bdd *current-trace*))
                  (set! *current-state-list* #f)
                  (set! *current-visited* (state-set-union bdd *current-visited*)))
                (cdr (reverse trace)))
      #t)
     (else
      #f))))

(define-api (select-state! (idx natural?))
  :doc "Select one state in the frontier of the current (active) simulation. @code{idx}
must be a positive number. The index is the same used by the function @code{display-curr-trace}."
  :examples '((begin
                (start-simulation! "peterson!system")
                (step!)
                (display-curr-trace)
                (select-state! 1)
                (display-curr-trace)))
  (check-simulation)
  (when (<= idx 0)
    (sign-error "Invalid state index."))
  (let ((max idx))
    (when (or (not *current-state-list*)
              (< (length *current-state-list*) max))
      (set! *current-state-list* (peek-states-without-choices *current-fsm* (car *current-trace*) max)))
    (when (< (length *current-state-list*) max)
      (sign-error "Invalid state index."))
    (set! *current-trace* (cons (list-ref *current-state-list* (- idx 1)) (cdr *current-trace*)))))

(define-api (filter-curr-states! obj)
  :doc "Filter the frontier of the current (active) simulation using a BDD or a SAL expression."
  :examples '((begin
                (import! "peterson")
                (start-simulation! "system")
                (step!)
                (display-curr-trace)
                (filter-curr-states! "pc1 = trying")
                (display-curr-trace)))
  (check-simulation)
  (let ((bdd (obj->fsm-bdd obj *current-fsm*)))
    (set! *current-trace* (cons (state-set-intersection (car *current-trace*) bdd)
                                (cdr *current-trace*)))))

(define-api (restart! obj)
  :doc "Restart the simulation using the given set of initial states. The argument can be a string representing a SAL expression, or a BDD. In both cases, the argument specifies a set of states, which may or may not be reachable."
  :examples '((begin
                (import! "peterson")
                (start-simulation! "system")
                (restart! "pc1 = critical and pc2 = critical")))
  (let ((new-start (obj->fsm-bdd obj *current-fsm*)))
    (set! *current-trace* (list new-start))))
                
;; (close-api-group!)

(define (help-commands)
  (api/show-entry "SAL Simulator"))

(define (show-command cmd)
  (print "")
  (display "      ") (pp cmd)
  (print ""))

(define (show-example)
  (print "The SAL specification used is this example is located at: <salenv-dir>/examples/peterson")
  (print "- Import context 'peterson'")
  (show-command '(import! "peterson"))
  (print "- Starting the simulation of the module system")
  (show-command '(start-simulation! "system"))
  (print "- Display set of initial states")
  (show-command '(display-curr-states))
  (print "- Execute one step")
  (show-command '(step!))
  (print "- Display the frontier of the simulation")
  (show-command '(display-curr-states))
  (print "- Select one of the states in the frontier")
  (show-command '(select-state! 1))
  (print "- Show the current trace")
  (show-command '(display-curr-trace))
  (print "- Execute another step")
  (show-command '(step!))
  (print "- Display the frontier of the simulation")
  (show-command '(display-curr-states))
  (print "- Filter the frontier of the simulation")
  (show-command '(filter-curr-states! "(= pc1 trying)"))
  (print "- Display the frontier of the simulation")
  (show-command '(display-curr-states))
  (print "- Show the current trace")
  (show-command '(display-curr-trace))
  (print "- Backtrack")
  (show-command '(backtrack!))
  (print "- Show the current trace")
  (show-command '(display-curr-trace)))

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Traceability" "BDD Interface"))

(set! *help-message*
  "-  (help-commands) print the main commands of the SAL simulator.
-  (show-example) print an example (tutorial), that is, a small sequence of commands.
-  (help <prefix>) print information about all procedures/classes whose name starts with <prefix>.
-  (show-commands <prefix>) print the name of all procedures/classes whose name starts with <prefix>.
-  (apropos <rexpr>) show all procedures/classes whose name/documentation match the regular expression <rexpr>.")

(define sim-header (string-append (sal/header "SAL Simulator") "\nType `(exit)' to exit.\nType `(help)' for help."))

(gen-front-end sal-sim
              sim-header
              #f
              "Examples: 
  sal-sim"
              (lambda (else)
                (print-error "Illegal argument `" else "'. Usage:")
                (sal-sim/simple-help))
              (begin
                (print sim-header)
                (repl)))

(sal-sim/main)



