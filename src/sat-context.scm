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

(module sat-context
        (include "sal.sch")
        (export <sat-context>
                (sat-context/make-and* ctx sat-args)
                (sat-context/make-or* ctx sat-args)
                (sat-context/make-not ctx sat-arg)
                (sat-context/make-iff ctx sat-arg1 sat-arg2)
                (sat-context/make-eq ctx sat-arg1 sat-arg2)
                (sat-context/make-true ctx)
                (sat-context/make-false ctx)
                (sat-context/make-ite ctx c t e)
                (sat-context/assert ctx sat-arg)
                (sat-context/add-auxiliary-decl ctx decl)
                (sat-context/solve ctx))
        )
                
(define-class <sat-context> () ())

(define-generic (sat-context/make-and* ctx sat-args))

(define-generic (sat-context/make-not ctx sat-arg))

(define-generic (sat-context/make-or* ctx sat-args))

(define-method (sat-context/make-or* (ctx <sat-context>) (sat-args <primitive>))
  (sat-context/make-not ctx (sat-context/make-and* ctx (map (cut sat-context/make-not ctx <>) sat-args))))

(define-generic (sat-context/make-iff ctx sat-arg1 sat-arg2))

(define-generic (sat-context/make-eq ctx sat-arg1 sat-arg2))

(define-generic (sat-context/make-true ctx))

(define-generic (sat-context/make-false ctx))

(define-generic (sat-context/make-ite ctx c t e))

(define-generic (sat-context/assert ctx sat-arg))

(define-generic (sat-context/add-auxiliary-decl ctx decl))

(define-method (sat-context/make-false (ctx <sat-context>))
  (sat-context/make-not ctx (sat-context/make-true ctx)))

(define-generic (sat-context/solve ctx))
