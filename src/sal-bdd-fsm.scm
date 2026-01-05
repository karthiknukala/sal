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

(module sal-bdd-fsm
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import bdd sal-bdd-context)
        (export <sal-bdd-fsm>
                (sal-bdd-fsm/manager fsm)
                (sal-bdd-fsm/flat-module fsm)
                (sal-bdd-fsm/curr-indices fsm)
                (sal-bdd-fsm/next-indices fsm)
                (sal-bdd-fsm/curr-var-cube fsm)
                (sal-bdd-fsm/next-var-cube fsm)
                (sal-bdd-fsm/input-var-cube fsm)
                (sal-bdd-fsm/forward-cube fsm)
                (sal-bdd-fsm/backward-cube fsm)
                (sal-bdd-fsm/num-vars fsm)
                (sal-bdd-fsm/num-non-choice-vars fsm)
                (sal-bdd-fsm/set-next->curr-map! fsm)
                (sal-bdd-fsm/set-curr->next-map! fsm)
                (sal-bdd-fsm/map-curr->next fsm state-set)
                (sal-bdd-fsm/map-next->curr fsm state-set)
                (sal-bdd-fsm/filter-invalid-input-rep fsm state-set)
                (sal-bdd-fsm/filter-invalid-latch-rep fsm state-set)
                (sal-bdd-fsm/filter-invalid-state-rep fsm state-set)
                (sal-bdd-fsm/invalid-states fsm state-set)
                (sal-bdd-fsm/initial-states fsm)
                (sal-bdd-fsm/remove-choice-vars fsm state-set)
                (sal-bdd-fsm/remove-input-vars fsm state-set)
                (sal-bdd-fsm/complement fsm state-set))
        )

(define-class <sal-bdd-fsm> (<sal-bdd-context>)
  (:initial-states ;; set of initial states
   :flat-module
   :transition-relation-cluster ;; transition relation
   :next-cube ;; cube that contains the next variables, this cube is used to perform quantifier elimination
   :curr-cube ;; cube that contains current state variables  
   :curr-indices ;; list of indices of the current variables
   :next-indices ;; list of indices of the next variables
   :input-cube ;; cube that contains the input variables
   :choice-cube ;; cube of choice vars
   :next-var-array  ;; an array of BDD current state variables, this array is used to implement substitution: next --> curr, and curr --> next
   :curr-var-array  ;; an array of BDD next state variables, this array is used to implement substitution: next --> curr, and curr --> next
   :no-choice-var-array
   :num-vars
   :num-choice-vars 
   :valid-input ;; BDD that encodes an expression that evaluates to true iff the representation of the input variables is valid.
   :valid-latch ;; BDD that encodes an expression that evaluates to true iff the representation of the state is valid.
   :reachable-states ;; set of reachable states
   :layered-reachable-states ;; set of reachable states divided in layers (list of BDDs). Each layer represents a set of reachable states after a given number of steps.
   :restriction ;; auxiliary BDD used to compute CTL operators...
   :new-let-var-proc ;; procedure used to create let variables which represent big BDDs. 
   )
  :doc "Represents a finite state machine encoded using BDDs. @code{:initial-states} is a BDD representing the set of initial states. @code{:flat-module} is the module that was used to create this object. @code{:transition-relation-cluster} a reference to a @code{<sal-bdd-cluster>} object, and represents the transition relation. @code{:next-cube} contains a BDD cube of the next variables, and is used to perform quantifier elimination.")

(define-api (sal-bdd-fsm/manager fsm)
  :doc "Returns the BDD manager used by the finite state machine @code{fsm}."
  (sal-bdd-context/manager fsm))

(define-api (sal-bdd-fsm/flat-module fsm)
  :doc "Returns the flat module that was used to generate the finite state machine @code{fsm}."
  (slot-value fsm :flat-module))

(define-api (sal-bdd-fsm/curr-var-cube fsm)
  :doc "Returns a BDD cube which contains the current variables. See @code{sal-bdd-fsm/next-var-cube}."
  (slot-value fsm :curr-cube))

(define-api (sal-bdd-fsm/next-var-cube fsm)
  :doc "Returns a BDD cube which contains the next variables. See @code{sal-bdd-fsm/next-var-cube}." 
  (slot-value fsm :next-cube))

(define-api (sal-bdd-fsm/curr-indices fsm)
  :doc "Returns a list of the current variable indices in @code{fsm}."
  (slot-value fsm :curr-indices))

(define-api (sal-bdd-fsm/next-indices fsm)
  :doc "Returns a list of the next variable indices in @code{fsm}."
  (slot-value fsm :next-indices))

(define-api (sal-bdd-fsm/input-var-cube fsm)
  :doc "Returns a BDD cube which contains the input variables of the finite state machine." 
  (slot-value fsm :input-cube))

(define-api (sal-bdd-fsm/forward-cube fsm)
  :doc "Alias for @code{sal-bdd-fsm/curr-var-cube}."
  (slot-value fsm :curr-cube))

(define-api (sal-bdd-fsm/backward-cube fsm)
  :doc "Alias for @code{sal-bdd-fsm/curr-next-cube}."
  (slot-value fsm :next-cube))

;; number of current variables
(define-api (sal-bdd-fsm/num-vars fsm)
  :doc "Returns the number of BDD variables used to encode the finite state machine @code{fsm}."
  (slot-value fsm :num-vars))

;; number of "real" current variables
(define-api (sal-bdd-fsm/num-non-choice-vars fsm)
  :doc "Returns the number of choice variables used to encode the finite state machine @code{fsm}. Choice variables are auxiliary variables created by SAL to store which transition was executed at each step."
  (- (slot-value fsm :num-vars) (slot-value fsm :num-choice-vars)))

(define (sal-bdd-fsm/set-next->curr-map! fsm)
  (let ((curr-var-array (slot-value fsm :curr-var-array))
        (next-var-array (slot-value fsm :next-var-array))
        (bdd-manager (slot-value fsm :bdd-manager))
        (array-size (sal-bdd-fsm/num-vars fsm)))
    (bdd/set-var-map! bdd-manager next-var-array curr-var-array array-size)))

(define (sal-bdd-fsm/set-curr->next-map! fsm)
  (let ((curr-var-array (slot-value fsm :curr-var-array))
        (next-var-array (slot-value fsm :next-var-array))
        (bdd-manager (slot-value fsm :bdd-manager))
        (array-size (sal-bdd-fsm/num-vars fsm)))
    (bdd/set-var-map! bdd-manager curr-var-array next-var-array array-size)))

(define-api (sal-bdd-fsm/map-curr->next fsm state-set)
  :doc "Map the current variables in the BDD @code{state-set} to next variables."
  (sal-bdd-fsm/set-curr->next-map! fsm)
  (bdd/var-map state-set))

(define-api (sal-bdd-fsm/map-next->curr fsm state-set)
  :doc "Map the next variables in the BDD @code{state-set} to current variables."
  (sal-bdd-fsm/set-next->curr-map! fsm)
  (bdd/var-map state-set))

(define-api (sal-bdd-fsm/filter-invalid-input-rep fsm state-set)
  :doc "Filter invalid inputs due to representation problems. For instance, the type @code{[0..2]} is represented with two bits, but the pair of bits @code{11} is an invalid representation."
  (bdd/and (slot-value fsm :valid-input) state-set))

(define-api (sal-bdd-fsm/filter-invalid-latch-rep fsm state-set)
  :doc "Similar to @code{sal-bdd-fsm/filter-invalid-input-rep}, but for invalid latch variables."
  (bdd/and (slot-value fsm :valid-latch) state-set))

;; combines the previous two functions
(define-api (sal-bdd-fsm/filter-invalid-state-rep fsm state-set)
  :doc "Combines the functions @code{sal-bdd-fsm/filter-invalid-input-rep} and @code{sal-bdd-fsm/filter-invalid-latch-rep}."
  (sal-bdd-fsm/filter-invalid-input-rep fsm 
                                        (sal-bdd-fsm/filter-invalid-latch-rep fsm state-set)))                                    

;; returns a subset of state-set which represents invalid states
(define-api (sal-bdd-fsm/invalid-states fsm state-set)
  :doc "Returns a subset of the BDD @code{state-set} which represents invalid states. See @code{sal-bdd-fsm/filter-invalid-state-rep}."
  (bdd/and (bdd/or (bdd/not (slot-value fsm :valid-latch))
                   (bdd/not (slot-value fsm :valid-input)))
           state-set))

(define-api (sal-bdd-fsm/initial-states fsm)
  :doc "Returns the set of initial states in the finite state machine @code{fsm}."
  (slot-value fsm :initial-states))

(define-api (sal-bdd-fsm/remove-choice-vars fsm state-set)
  :doc "Eliminate choice variables in the BDD @code{state-set}. Choice variables are auxiliary variables created by SAL to store which transition was executed at each step."
  (bdd/exists state-set (slot-value fsm :choice-cube)))

(define-api (sal-bdd-fsm/remove-input-vars fsm state-set)
  :doc "Remove the input variables from the set represented by the BDD @code{state-set}."
  (bdd/exists state-set (slot-value fsm :input-cube)))

(define-api (sal-bdd-fsm/complement fsm state-set)
  :doc "Returns the complement of the set represented by the BDD @code{state-set}. This function is not equivalent to @code{bdd/not}, since invalid states are removed from the result. See @code{sal-bdd-fsm/filter-invalid-state-rep}."
  (sal-bdd-fsm/filter-invalid-state-rep fsm (bdd/not state-set)))
