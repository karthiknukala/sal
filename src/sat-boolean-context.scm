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

(module sat-boolean-context
        (include "sal.sch")
        (import sat-context)
        (export <sat-boolean-context>
                (sat-boolean-context/init! ctx)
                (sat-boolean-context/make-aux-var ctx))
        )

(define-class <sat-boolean-context> (<sat-context>) (:next-idx))

(define (sat-boolean-context/init! ctx)
  (set-slot-value! ctx :next-idx 1))

(define-generic (sat-boolean-context/make-aux-var ctx))

(define-method (sat-boolean-context/make-aux-var (ctx <sat-boolean-context>))
  (let* ((next-idx (slot-value ctx :next-idx))
         (result (symbol-append 'a (object->symbol next-idx))))
    (set-slot-value! ctx :next-idx (+ next-idx 1))
    result))

(define-method (sat-context/add-auxiliary-decl (ctx <sat-boolean-context>) (decl <sal-decl>))
  ;; do nothing, boolean contexts do not need auxiliary declarations...
  #unspecified)





