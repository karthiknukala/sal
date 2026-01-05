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

(define *bmc-name* "SAL Infinite Bounded Model Checker")
(define *bmc-usage* 
"Usage: sal-inf-bmc [options] <context-name> <assertion-name>
   or  sal-inf-bmc [options] <file-name> <assertion-name>
   or  sal-inf-bmc [options] --assertion='<assertion-expr>'")
(define *bmc-examples* "Examples:
  sal-inf-bmc inf-bakery mutex
 
  sal-inf-bmc ~/examples/inf-bakery.sal mutex

  sal-inf-bmc -d 15 -v 3 ../tmp/inf-bakery.sal mutex")

(front-end/set-categories! '("Help" "Misc" "Assertion" "Pretty Printing" "Code Transformations" "Extra Code Transformations" "Abstraction" "Traceability" "Path Pretty Printing" "LTL Support" "Verification" "Var Mapping"))

(sal/load-source! "sal-bmc-core-front-end.scm")

(define (bmc/make-assertion qualified-name-str)
  (make-flat-assertion qualified-name-str :ltl? #t :skolemize? *skolemize*))

(define (bmc/make-lemmas lemmas assertion-qualified-name assertion)
  (sal-inf-bmc/create-assertion-lemmas lemmas assertion-qualified-name assertion))

(define (bmc/invariant assertion)
  (sal-inf-bmc/invariant assertion *from-depth* *to-depth* *acyclic?* (not *iterative?*) *solver-id*))

(define (bmc/liveness assertion)
  (sal-inf-bmc/liveness assertion *to-depth* (not *iterative?*) *solver-id*))

(define (bmc/k-induction assertion lemma-exprs)
  (sal-inf-bmc/k-induction assertion *to-depth* *acyclic?* lemma-exprs *solver-id*))

(sal-bmc/main)

