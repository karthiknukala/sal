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

(module sal-smc-api
        (include "sal.sch")
        (import sal-flat-module-to-bdd-fsm sal-smc sal-api sal2bdd sal-bdd-fsm)
        (export (make-bdd-fsm module-str)
                (boolean-flat-module->bdd-fsm flat-module)
                (make-state-expression-bdd expr-str fsm))
        )

(define-api (boolean-flat-module->bdd-fsm (flat-module <sal-boolean-flat-module>))
  (sal-flat-module->sal-bdd-fsm flat-module))

(define-api (make-bdd-fsm (module-str string?))
  (sal-flat-module->sal-bdd-fsm (make-boolean-flat-module module-str)))

(define-generic (make-state-expression-bdd expr fsm))

(define-method (make-state-expression-bdd (expr-str <primitive>) (fsm <sal-bdd-fsm>))
  (sal-expr->bdd (make-boolean-state-expression expr-str (slot-value fsm :flat-module)) fsm))

(define-method (make-state-expression-bdd (expr <sal-expr>) (fsm <sal-bdd-fsm>))
  (sal-expr->bdd expr fsm))
