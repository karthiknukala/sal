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

(module ltl-ctl
        (include "sal.sch")
        (import sal-expression sal-ast-for-each queue xformat)
        (export (ltl->ctl expr)
                (ctl->ltl expr))
        )

(define (ltl->ctl expr)
  (ltl->ctl-core expr))

(define (ctl->ltl expr)
  (ctl->ltl-core expr))

(define (sign-ltl->ctl-failure expr)
  (error 'ltl->ctl-failure (xformat #f "Failed to convert the LTL property at ~a to CTL." (format-with-location expr "")) expr))

(define (sign-ctl->ltl-failure expr)
  (error 'ctl->ltl-failure (xformat #f "Failed to convert the CTL property at ~a to LTL." (format-with-location expr "")) expr))

(define (no-temporal-op? expr)
  (not (sal-ast/find (cut instance-of? <> <sal-temporal-application>) expr)))

(define-generic (ltl->ctl-core expr))

(define-method (ltl->ctl-core (expr <sal-expr>))
  (unless (no-temporal-op? expr)
    (sign-ltl->ctl-failure expr))
  expr)
  
(define-method (ltl->ctl-core (expr <sal-ltl-g>))
  (make-sal-builtin-application <sal-ctl-ag> expr
                                (ltl->ctl-core (slot-value expr :arg))))

(define-method (ltl->ctl-core (expr <sal-ltl-f>))
  (unless (no-temporal-op? (slot-value expr :arg))
    (sign-ltl->ctl-failure expr))
  (make-sal-builtin-application <sal-ctl-af> expr
                                (ltl->ctl-core (slot-value expr :arg))))

(define-method (ltl->ctl-core (expr <sal-ltl-x>))
  (make-sal-builtin-application <sal-ctl-ax> expr
                                (ltl->ctl-core (slot-value expr :arg))))

(define-method (ltl->ctl-core (expr <sal-and>))
  (apply make-sal-builtin-application <sal-and> expr
         (map ltl->ctl-core (sal-application/argument-list expr))))

(define (separate-temporal-arguments arg-list)
  (let ((non-temporal-args (make-queue))
        (temporal-args (make-queue)))
    (for-each (lambda (arg)
                (if (no-temporal-op? arg)
                  (queue/insert! non-temporal-args arg)
                  (queue/insert! temporal-args arg)))
              arg-list)
    (values (queue->list non-temporal-args)
            (queue->list temporal-args))))

(define-method (ltl->ctl-core (expr <sal-or>))
  (multiple-value-bind
      (non-temporal-args temporal-args)
      (separate-temporal-arguments (sal-application/argument-list expr))
    (unless (<= (length temporal-args) 1)
      (sign-ltl->ctl-failure expr))
    (if (= (length temporal-args) 0)
      expr
      (apply make-sal-builtin-application <sal-or> expr
             (ltl->ctl-core (car temporal-args))
             non-temporal-args))))

(define-method (ltl->ctl-core (expr <sal-implies>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (unless (no-temporal-op? arg1)
      (sign-ltl->ctl-failure expr))
    (make-sal-builtin-application <sal-implies> expr
                                  arg1
                                  (ltl->ctl-core arg2))))

(define-method (ltl->ctl-core (expr <sal-for-all-expr>))
  (copy-instance expr
                 :expr (ltl->ctl-core (slot-value expr :expr))))

(define-method (ltl->ctl-core (expr <sal-let-expr>))
  (unless (for-all no-temporal-op? (slot-value expr :local-decls))
    (sign-ltl->ctl-failure expr))
  (copy-instance expr
                 :expr (ltl->ctl-core (slot-value expr :expr))))

(define-generic (ctl->ltl-core expr))

(define-method (ctl->ltl-core (expr <sal-expr>))
  (unless (no-temporal-op? expr)
    (sign-ctl->ltl-failure expr))
  expr)

(define-method (ctl->ltl-core (expr <sal-ctl-ag>))
  (make-sal-builtin-application <sal-ltl-g> expr
                                (ctl->ltl-core (slot-value expr :arg))))

(define-method (ctl->ltl-core (expr <sal-ctl-af>))
  (unless (no-temporal-op? (slot-value expr :arg))
    (sign-ctl->ltl-failure expr))
  (make-sal-builtin-application <sal-ltl-f> expr
                                (ctl->ltl-core (slot-value expr :arg))))

(define-method (ctl->ltl-core (expr <sal-ctl-ax>))
  (make-sal-builtin-application <sal-ltl-x> expr
                                (ctl->ltl-core (slot-value expr :arg))))

(define-method (ctl->ltl-core (expr <sal-and>))
  (apply make-sal-builtin-application <sal-and> expr
         (map ctl->ltl-core (sal-application/argument-list expr))))

(define-method (ctl->ltl-core (expr <sal-or>))
  (multiple-value-bind
      (non-temporal-args temporal-args)
      (separate-temporal-arguments (sal-application/argument-list expr))
    (unless (<= (length temporal-args) 1)
      (sign-ctl->ltl-failure expr))
    (if (= (length temporal-args) 0)
      expr
      (apply make-sal-builtin-application <sal-or> expr
             (ctl->ltl-core (car temporal-args))
             non-temporal-args))))

(define-method (ctl->ltl-core (expr <sal-implies>))
  (multiple-value-bind
      (arg1 arg2)
      (sal-binary-application/arguments expr)
    (unless (no-temporal-op? arg1)
      (sign-ctl->ltl-failure expr))
    (make-sal-builtin-application <sal-implies> expr
                                  arg1
                                  (ctl->ltl-core arg2))))

(define-method (ctl->ltl-core (expr <sal-for-all-expr>))
  (copy-instance expr
                 :expr (ctl->ltl-core (slot-value expr :expr))))

(define-method (ctl->ltl-core (expr <sal-let-expr>))
  (unless (for-all no-temporal-op? (slot-value expr :local-decls))
    (sign-ctl->ltl-failure expr))
  (copy-instance expr
                 :expr (ctl->ltl-core (slot-value expr :expr))))









