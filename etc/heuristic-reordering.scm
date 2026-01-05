;;
;; SAL 3.1 Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
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

;; Configuration Script
;;
;; Purpose: Try the reorder BDD variables during the
;; construction of the set of reachable states. This
;; script may be used as a template to build more 
;; complex scripts.
;;
;; How to use: Add the path to this file in the command line of sal-smc
;;
;; Example: 
;;  Go to the SAL examples directory.
;;  
;;  % cd bakery
;;
;;  % sal-smc -v 10 --assertion='bakery{;7,15}!mutex' ../../etc/heuristic-reordering.scm
;;

;; Configuration
(define *min-bdd-size-for-reordering* 10000) ;; Only consider variable reordering if the number of BDD nodes is greater than 10000
(define *maximum-number-of-variable-reorders* 10) ;; maximum number of variable reordering that will be performed.
(define *growth-factor* 1.2) ;; only reorder if after the previous reordering step
                             ;; there was an 20% increase in the number of
                             ;; BDDs nodes

;; Auxiliary Global variables
(define *prev-bdd-size* 0)

;; BDD Variable Reordering Predicate
(define (reordering-pred fsm rs)
  ;; rs is the BDD representing the set of reached states so far.
  (let ((size (bdd/size rs)))
    (cond
     ((and (> size *min-bdd-size-for-reordering*)
           (> *maximum-number-of-variable-reorders* 0)
           (> size (* *growth-factor* *prev-bdd-size*)))
      (set! *maximum-number-of-variable-reorders* (- *maximum-number-of-variable-reorders* 1))
      (set! *prev-bdd-size* size)
      #t)
     (else
      #f))))

(sal-smc/set-reorder-predicate! reordering-pred)
