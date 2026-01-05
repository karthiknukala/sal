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

(module sat-boolean-bmc-context
        (include "sal.sch")
        (import sat-boolean-context sat-bmc-context polarity fast-hash-table sal-ast-env)
        (export <sat-boolean-bmc-context>
                (sat-boolean-bmc-context/init! ctx flat-module)
                (make-sat-boolean-bmc-context flat-module))
        )

(define-class <sat-boolean-bmc-context> (<sat-boolean-context> <sat-bmc-context>) ())

(define (sat-boolean-bmc-context/init! ctx flat-module)
  (sat-bmc-context/init! ctx flat-module)
  (sat-boolean-context/init! ctx))
  
(define (make-sat-boolean-bmc-context flat-module)
  (let ((result (make-instance <sat-boolean-bmc-context>)))
    (sat-boolean-bmc-context/init! result flat-module)
    result))

(define-method (sat-bmc-context/id-at (ctx <sat-boolean-bmc-context>) (id <primitive>) (step <primitive>))
  (let* ((next-idx (slot-value ctx :next-idx))
         (result (symbol-append 'x (object->symbol next-idx))))
    (set-slot-value! ctx :next-idx (+ next-idx 1))
    result))

(define-method (sat-bmc-context/decl-at (ctx <sat-boolean-bmc-context>) (decl <sal-decl>) (step <primitive>))
  (let ((table (sat-bmc-context/decl-table-at ctx step)))
    (cond 
     ((eq-hash-table/get table decl) =>
      cdr)
     (else
      (let* ((name (sal-decl/name decl))
             (name-at (sat-bmc-context/id-at ctx name step))
             (inv-table (slot-value ctx :inv-step-decls)))
        (eq-hash-table/put! table decl name-at)
        (eq-hash-table/put! inv-table name-at (cons decl step))
        name-at)))))

(define-method (sal-ast->sat (ast <sal-name-expr>) (ctx <sat-boolean-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (cond
   ((instance-of? (slot-value ast :decl) <sal-state-var-decl>)
    (sat-bmc-context/decl-at ctx (slot-value ast :decl) step))
   ((lookup-env (slot-value ast :decl) env) =>
    identity)
   (else 
    ;; (breakpoint "sal-ast->sat" (ast ctx env step polarity) #t)
    (internal-error))))

(define-method (sal-ast->sat (ast <sal-let-expr>) (ctx <sat-boolean-bmc-context>) (env <primitive>) (step <primitive>) (polarity <polarity>))
  (let* ((let-decls (slot-value ast :local-decls))
         (new-vals (map (lambda (decl)
                          (sal-ast->sat (slot-value decl :value) ctx env step *pos-neg*))
                        let-decls))
         (new-env (update-env* env let-decls new-vals)))
    (sal-ast->sat (slot-value ast :expr) ctx new-env step polarity)))

(define (bool-skolemize ast ctx env step polarity)
  (let* ((local-decls (slot-value ast :local-decls))
         (new-vars (map (lambda (_)
                          (sat-boolean-context/make-aux-var ctx))
                        local-decls))
         (new-env (update-env* env local-decls new-vars)))
    (sal-ast->sat (slot-value ast :expr) ctx new-env step polarity)))
  
(define-method (sal-ast->sat (ast <sal-exists-expr>) (ctx <sat-boolean-bmc-context>) (env <primitive>) (step <primitive>) (polarity <pos>))
  (bool-skolemize ast ctx env step polarity))

(define-method (sal-ast->sat (ast <sal-for-all-expr>) (ctx <sat-boolean-bmc-context>) (env <primitive>) (step <primitive>) (polarity <pos>))
  (bool-skolemize ast ctx env step polarity))
