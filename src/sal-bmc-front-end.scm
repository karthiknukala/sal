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


(define *bmc-name* "SAL Bounded Model Checker")
(define *bmc-usage* 
"Usage: sal-bmc [options] <context-name> <assertion-name>
   or  sal-bmc [options] <file-name> <assertion-name>
   or  sal-bmc [options] --assertion='<assertion-expr>'")
(define *bmc-examples* "Examples: 
  sal-bmc --depth=10 peterson mutex

  sal-bmc ./peterson.sal mutex

  sal-bmc ~/examples/peterson.sal mutex

  sal-bmc --from=5 --to=10 peterson invalid

  sal-bmc --verbose=3 --depth=10 peterson mutex

  sal-bmc -v 3 --depth=20 peterson mutex

  sal-bmc --depth=10 --assertion='peterson!mutex'

  sal-bmc --depth=10 --assertion='(@ mutex peterson)'

  sal-bmc --depth=20 --assertion='arbiter{10}!at_most_one_ack'

  sal-bmc --depth=20 --assertion='(@ at-most-one-ack (arbiter () (10)))'

  sal-bmc --depth=10 --disable-traceability peterson invalid

  sal-bmc --depth=10 --delta-path peterson invalid

  sal-bmc --depth=10 --lemma=mutex --induction peterson invalid

  sal-bmc --depth=10 --lemma=mutex -i peterson invalid

  sal-bmc -i --lemma=at_most_one_ack --assertion='arbiter{10}!no_ack_without_request'")

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Abstraction" "Traceability" "Path Pretty Printing" "LTL Support" "Verification"))

(sal/load-source! "sal-bmc-core-front-end.scm")

(define (bmc/make-assertion qualified-name-str)
  (make-boolean-assertion qualified-name-str :ltl? #t :skolemize? *skolemize*))

(define (bmc/make-lemmas lemmas assertion-qualified-name assertion)
  (sal-bmc/create-assertion-lemmas lemmas assertion-qualified-name assertion))

(define (bmc/invariant assertion)
  (sal-bmc/invariant assertion *from-depth* *to-depth* *acyclic?* (not *iterative?*) *solver-id*))

(define (bmc/liveness assertion)
  (sal-bmc/liveness assertion *to-depth* (not *iterative?*) *solver-id*))

(define (bmc/k-induction assertion lemma-exprs)
  (sal-bmc/k-induction assertion *to-depth* *acyclic?* lemma-exprs *solver-id*))

(sal-bmc/main)

