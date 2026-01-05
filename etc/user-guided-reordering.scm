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

;; Configuration Script
;;
;; Purpose: Ask the user if he wants to reorder the BDD variables during the
;; construction of the set of reachable states. 
;;
;; How to use: Add the path to this file in the command line of sal-smc
;;
;; Example: 
;;  Go to the SAL examples directory.
;;  
;;  % cd bakery
;;
;;  % sal-smc -v 10 --assertion='bakery{;7,15}!mutex' ../../etc/user-guided-reordering.scm
;;

(define (reordering-pred fsm rs)
  ;; Only bothers the user if the number of BDD nodes representing the set of reached states is greater than 10000
  (cond
   ((> (bdd/size rs) 10000)
    (print "\nReorder BDD variables (Yes/No)?")
    (let ((ans (read)))
      (or (eq? ans 'Y) (eq? ans 'Yes) (eq? ans 'y) (eq? ans 'yes))))
   (else
    #f)))

(sal-smc/set-reorder-predicate! reordering-pred)
