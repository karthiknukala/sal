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

(module sat-cvcl-bmc-context
        (include "sal.sch")
        (import sat-generic-bmc-context sat-context sat-cvcl-context sat-bmc-context)
        (export <sat-cvcl-bmc-context>
                <sat-cvc-bmc-context>
                (make-sat-cvcl-bmc-context flat-module cont-proc)
                (make-sat-cvc-bmc-context flat-module cont-proc))
        )

(define-class <sat-cvcl-bmc-context> (<sat-cvcl-context> <sat-generic-bmc-context>) ())

(define (make-sat-cvcl-bmc-context flat-module cont-proc)
  (let ((ctx (make-instance <sat-cvcl-bmc-context>)))
    (sat-generic-bmc-context/init! ctx flat-module cont-proc)))

(define-class <sat-cvc-bmc-context> (<sat-cvc-context> <sat-generic-bmc-context>) ())

(define (make-sat-cvc-bmc-context flat-module cont-proc)
  (let ((ctx (make-instance <sat-cvc-bmc-context>)))
    (sat-generic-bmc-context/init! ctx flat-module cont-proc)))


        
