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

(module sat-boolean-smt-bmc-context
        (include "sal.sch")
        (import sat-context sat-boolean-bmc-context sat-boolean-smt-context sat-bmc-context)
        (export <sat-boolean-smt-bmc-context>
                (sat-boolean-smt-bmc-context/init! ctx flat-module cont-proc)
                (make-sat-boolean-smt-bmc-context flat-module cont-proc))
        )


(define-class <sat-boolean-smt-bmc-context> (<sat-boolean-smt-context> <sat-boolean-bmc-context>) ())

(define (sat-boolean-smt-bmc-context/init! ctx flat-module cont-proc)
  (sat-boolean-bmc-context/init! ctx flat-module)
  (sat-boolean-smt-context/init! ctx 
                                 (lambda ()
                                   (cont-proc ctx))))

(define (make-sat-boolean-smt-bmc-context flat-module cont-proc)
  (let ((ctx (make-instance <sat-boolean-smt-bmc-context>)))
    (sat-boolean-smt-bmc-context/init! ctx flat-module cont-proc)
    ctx))

(define-method (sat-bmc-context/id-at (ctx <sat-boolean-smt-bmc-context>) (id <primitive>) (step <primitive>))
  (let ((result (call-next-method)))
    (set-slot-value! ctx :vars (cons result (slot-value ctx :vars)))
    result))

(define-method (sat-bmc-context/make-path (ctx <sat-boolean-smt-bmc-context>))
  (sat-context/solve ctx))
