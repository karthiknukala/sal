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
;; Purpose: Save the BDD variable order in a file after
;; each forced BDD variable reordering.
;; This script may be used as a template to build more 
;; complex scripts.
;; 
;; The files are going to be saved in the current directory,
;; and their names are: bdd-var-order-1, bdd-var-order-2, ...
;;
;; How to use: Add the path to this file in the command line of sal-smc or sal-wmc
;;
;; Example: 
;;  Go to the SAL examples directory.
;;  
;;  % cd ultralog
;;
;;  % sal-smc -v 10 --num-reorders=3 ultralog_light prop_1 ../../etc/save-var-order.scm
;;

;; Configuration
(define *file-name-prefix* "bdd-var-order-")

;; Auxiliary Global variables
(define *curr-idx* 1)

(define-method (sal-bdd-fsm/reorder! :around (fsm <sal-bdd-fsm>))
  (let* ((result (call-next-method))
         (file-name (string-append *file-name-prefix* (object->string *curr-idx*))))
    (var-order/save fsm file-name)
    (set! *curr-idx* (+ *curr-idx* 1))
    result))
