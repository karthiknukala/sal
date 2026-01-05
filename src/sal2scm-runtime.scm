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

(module sal2scm-runtime
        (include "iterators.sch")
        (include "api.macros")
        (include "utility.macros")
        (include "scmobj.macros")
        (import gmp-scheme xformat scmobj utility sal-error api scm2sal sal-pp
                fast-hash-table sal-expression)
        (export (inline esm/force-integer val)
                (sign-sal-scm-runtime-error-core error-msg . args)
                (sign-sal-scm-runtime-error ctx-name line column error-msg . args)
                (sign-sal-scm-runtime-error-old place-provider error-msg . args)
                (sign-sal-scm-runtime-type-error-core ctx-name line column msg val type ctx)
                (sal-scm/register-runtime-info! data)
                (sign-sal-scm-runtime-type-error ctx-name line column msg info-idx val)
                (sign-sal-scm-runtime-invalid-arg-error ctx-name line column arg-pos info-idx val)
                (sal-scm/dbg-expr info-idx arg)
                (sal-scm/dbg-print . args)
                (sal-scm/get-val-map info-idx)
                (sal-scm/get-val->idx-map info-idx)
                (sal-scm/loop-detector proc)
                (sal-scm/set-max-loop-depth! num)
                (sal-scm/error-handler proc ctx-name line-num col-num)
                (sal-scm/update-vector vect idx val)
                (sal-scm/safe-update-vector vect idx val ctx-name line column)
                (sal-scm/safe-vector-ref vect idx ctx-name line column)
                (sal-scm/vector-for-all pred vect)
                (sal-scm/subrange-iterator lower upper)
                (sal-scm/gmp-subrange-iterator lower upper)
                (sal-scm/eq? val1 val2)
                (sal-scm/hash val)
                (sal-scm/create-subrange-array lower upper proc)
                (sal-scm/create-gmp-subrange-array lower upper proc)
                (sal-scm/create-scalar-array num proc)
                (sal-scm/create-bool-array proc)
                (inline sal-scm/check-bounds-core ctx-name line column idx vect-len)
                (inline sal-scm/check-bounds ctx-name line column vect idx)
                (sal-scm/instrumented-div arg1 arg2 ctx-name line column)
                (sal-scm/instrumented-mpq-div arg1 arg2 ctx-name line column)
                (sal-scm/instrumented-idiv arg1 arg2 ctx-name line column)
                (sal-scm/instrumented-modulo arg1 arg2 ctx-name line column)
                (sal-scm/instrumented-mpq-idiv arg1 arg2 ctx-name line column)
                (sal-scm/instrumented-mpq-modulo arg1 arg2 ctx-name line column)
                (sal-scm/check-accessor arg expected-tag ctx-name line column)
                )
        )

(define-inline (esm/force-integer val)
  (if (number? val)
    val
    (mpq->integer val)))

(define *sal-scm-loop-depth* 0)
(define *sal-scm-max-loop-depth* 1024)

(define-api (sal-scm/set-max-loop-depth! num)
  (set! *sal-scm-max-loop-depth* num))

(define (sign-sal-scm-runtime-error-core error-msg . args)
  (error 'sal-scm-runtime-error (apply xformat #f error-msg args) #unspecified))

(define (sign-sal-scm-runtime-error ctx-name line-num col-num error-msg . args)
  (let ((msg (apply xformat #f error-msg args)))
    (error 'sal-scm-runtime-error (xformat #f "[Context: ~a, line(~d), column(~d)] ~a" ctx-name line-num col-num msg) #unspecified)))

(define (sign-sal-scm-runtime-error-old place-provider error-msg . args)
  (sign-sal-error 'sal-scm-runtime-error place-provider error-msg args))

(define (sign-sal-scm-runtime-type-error-core ctx-name line column msg val type ctx)
  (let ((sal-val (scm-value->sal val type ctx)))
    (sign-sal-scm-runtime-error ctx-name line column 
                                "Type error. ~a\nExpected type:\n~a\nActual value:\n~a"
                                msg
                                (with-output-to-string 
                                  (lambda ()
                                    (sal/pp type)))
                                (with-output-to-string 
                                  (lambda ()
                                    (sal/pp sal-val))))))

(define (sign-sal-scm-runtime-type-error ctx-name line column msg info-idx val)
  (let ((entry (eq-hash-table/get *runtime-info* info-idx)))
    [assert (entry) (and (pair? entry) (number? (car entry)))]
    (let ((info (cdr entry)))
      (sign-sal-scm-runtime-type-error-core ctx-name line column msg val (car info) (cdr info)))))

(define (sign-sal-scm-runtime-invalid-arg-error ctx-name line column arg-pos info-idx val)
  (sign-sal-scm-runtime-type-error  ctx-name line column 
                                    (xformat #f "Invalid argument at position ~a." arg-pos)
                                    info-idx
                                    val))

;; contains data used to sign runtime type errors
(define *runtime-info* (make-eq-hash-table))
(define *runtime-info-next-idx* 0)

(define (sal-scm/register-runtime-info! data)
  (let ((result *runtime-info-next-idx*))
    (set! *runtime-info-next-idx* (+ *runtime-info-next-idx* 1))
    (eq-hash-table/put! *runtime-info* result data)
    result))

(define (sal-scm/get-val-map info-idx)
  (let ((entry (eq-hash-table/get *runtime-info* info-idx)))
    [assert (entry) (and (pair? entry) (number? (car entry)))]
    [assert (entry) (vector? (cdr entry))]
    (cdr entry)))

(define (sal-scm/get-val->idx-map info-idx)
  (let ((entry (eq-hash-table/get *runtime-info* info-idx)))
    [assert (entry) (and (pair? entry) (number? (car entry)))]
    (cdr entry)))

(define (sal-scm/dbg-expr info-idx arg)
  (let* ((entry (eq-hash-table/get *runtime-info* info-idx))
         (_ [assert (entry) (pair? entry)])
         (info-data (cdr entry))
         (sal-arg (car info-data))
         (type (cadr info-data))
         (ctx (cddr info-data)))
    (display* (format-with-location sal-arg "")
              ": ")
    (sal/pp-single-line sal-arg)
    (display " --> ")
    (sal/pp-single-line (scm-value->sal arg type ctx))
    (print "")
    arg))

(define (sal-scm/dbg-print . args)
  (let ((last-val #f))
    (for-each (lambda (arg)
                (if (string? arg)
                  (display arg)
                  (let* ((info-idx (car arg))
                         (val (cdr arg))
                         (entry (eq-hash-table/get *runtime-info* info-idx)))
                    (set! last-val val)
                    [assert (entry) (pair? entry)]
                    (let* ((info-data (cdr entry))
                           (type (car info-data))
                           (ctx (cdr info-data))
                           ;; (_ [assert (type) (instance-of? type <sal-type>)])
                           ;; (_ [assert (ctx) (instance-of? ctx <sal-scm-context>)])
                           (sal-val (scm-value->sal val type ctx)))
                      (sal/pp-single-line sal-val)))))
              args)
    (print "")
    last-val))
  
(define (sal-scm/loop-detector proc)
  (dlet ((*sal-scm-max-loop-depth* (+ *sal-scm-max-loop-depth* 1)))
    (when (> *sal-scm-loop-depth* *sal-scm-max-loop-depth*)
      (set! *sal-scm-loop-depth* 0)
      (sign-sal-scm-runtime-error-core "Infinite loop detector threshold was reached while executing the specification. Please increase the threshold or rewrite your specification. The threshold can be increased by adding `(sal-scm/set-max-loop-depth! <num>)' in the `.salrc' file in your home directory."))
    (proc)))

(define (sal-scm/error-handler proc ctx-name line-num col-num)
  (try
   (proc)
   (lambda (escape proc msg obj)
     (if (or (eq? proc 'sal-to-scm-reported-error)
             (eq? proc 'esm-delay-action)
             (eq? proc 'esm-weak-delay-action)
             (eq? proc 'esm-action-failure))
       (error proc msg obj)
       (error 'sal-to-scm-reported-error (xformat #f "[Context: ~a, line(~d), column(~d)] ~a" ctx-name line-num col-num msg) obj)))))

;; (define-inline (check-if-assigned v)
;;  (when (or (eq? v 'not-assigned) (eq? v 'delayed))
;;    (esm/delay)))

(define-inline (sal-scm/copy-vector v)
  (copy-vector v (vector-length v)))

(define (sal-scm/update-vector vect idx val)
  (let ((result (sal-scm/copy-vector vect)))
    (vector-set! result idx val)
    result))

(define-inline (sal-scm/check-bounds-core ctx-name line column idx vect-len)
  (when (or (< idx 0) (>= idx vect-len))
    (sign-sal-scm-runtime-error ctx-name line column "Array index out of bounds.")))

(define-inline (sal-scm/check-bounds ctx-name line column vect idx)
  (sal-scm/check-bounds-core ctx-name line column idx (vector-length vect)))

(define (sal-scm/safe-update-vector vect idx val ctx-name line column)
  (sal-scm/check-bounds ctx-name line column vect idx)
  (sal-scm/update-vector vect idx val))

(define (sal-scm/safe-vector-ref vect idx ctx-name line column)
  (sal-scm/check-bounds ctx-name line column vect idx)
  (vector-ref vect idx))

(define (sal-scm/vector-for-all pred vect)
  (bind-exit (exit)
    (let ((n (vector-length vect)))
      (let loop ((i 0))
        (when (< i n)
          (unless (pred (vector-ref vect i))
            (exit #f))
          (loop (+ i 1)))))
    #t))

(define (sal-scm/subrange-iterator lower upper)
  (let ((curr lower))
    (make-iterator (<= curr upper)
                   (let ((result curr))
                     (set! curr (+ curr 1))
                     result)
                   (set! curr lower))))

(define (sal-scm/gmp-subrange-iterator lower upper)
  (let ((curr lower))
    (make-iterator (<=mpq curr upper)
                   (let ((result curr))
                     (set! curr (+mpq curr *mpq-one*))
                     result)
                   (set! curr lower))))

(define (sal-scm/eq? val1 val2)
  (cond
   ((number? val1)
    [assert (val1 val2) (number? val2)]
    (= val1 val2))
   ((boolean? val1)
    [assert (val1 val2) (boolean? val2)]
    (eq? val1 val2))
   ((mpq? val1)
    [assert (val1 val2) (mpq? val2)]
    (=mpq val1 val2))
   ((vector? val1) 
    (let ((n (vector-length val1)))
      [assert (n val2 val1) (= (vector-length val2) n)]
      (let loop ((idx 0))
        (if (< idx n)
          (and (sal-scm/eq? (vector-ref val1 idx) (vector-ref val2 idx))
               (loop (+ idx 1)))
          #t))))
   ((pair? val1)
    [assert (val1 val2) (pair? val2)]
    [assert (val1 val2) (imply (= (car val1) (car val2)) (and (list? val1) (list? val2) (= (length val1) (length val2))))]
    (and (= (car val1) (car val2))
         (for-all sal-scm/eq? (cdr val1) (cdr val2))))
   ((eq? val1 '())
    (eq? val1 val2))
   (else
    ;; (breakpoint "sal-scm/eq?" (val1 val2) #t)
    (internal-error))))

(define (sal-scm/hash val)
  (cond
   ((number? val)
    val)
   ((boolean? val)
    (if val 3 7))
   ((mpq? val)
    (mpq->integer val))
   ((vector? val)
    (let ((n (vector-length val)))
      (let loop ((idx 0)
                 (result 0))
        (if (< idx n)
          (loop (+ idx 1) (+ (* 3 result) (sal-scm/hash (vector-ref val idx))))
          result))))
   ((pair? val)
    (+ (car val)
       (fold-left (lambda (result val)
                    (+ (* 3 result) (sal-scm/hash val)))
                  0
                  (cdr val))))
   (else
    0)))
   
(define (sal-scm/create-subrange-array lower upper proc)
  (let* ((num (+ (- upper lower) 1))
         (result (make-vector num)))
     (let loop ((idx 0)
                (val lower))
       (when (< idx num)
         (vector-set! result idx (proc val))
         (loop (+ idx 1) (+ val 1))))
     result))

(define (sal-scm/create-gmp-subrange-array lower upper proc)
  (let* ((num (+ (mpq->integer (-mpq upper lower)) 1))
         (result (make-vector num)))
     (let loop ((idx 0)
                (val lower))
       (when (< idx num)
         (vector-set! result idx (proc val))
         (loop (+ idx 1) (+mpq val *mpq-one*))))
     result))
    
(define (sal-scm/create-scalar-array num proc)
  (let ((result (make-vector num)))
    (let loop ((idx 0))
      (when (< idx num)
        (vector-set! result idx (proc idx))
        (loop (+ idx 1))))
    result))

(define (sal-scm/create-bool-array proc)
  (let ((result (make-vector 2)))
    (vector-set! result 0 (proc #f))
    (vector-set! result 1 (proc #t))
    result))

(define-inline (check-non-zero arg2 ctx-name line column)
  (when (=fx arg2 0)
    (sign-sal-scm-runtime-error ctx-name line column "Division by zero.")))

(define-inline (check-mpq-non-zero arg2 ctx-name line column)
  (when (=mpq arg2 *mpq-zero*)
    (sign-sal-scm-runtime-error ctx-name line column "Division by zero.")))

(define (sal-scm/instrumented-div arg1 arg2 ctx-name line column)
  (check-non-zero arg2 ctx-name line column)
  (unless (=fx (modulo arg1 arg2) 0)
    (sign-sal-scm-runtime-error ctx-name line column "Operation produced a rational number. You must enable GMP to handle your example, or use integer division (DIV) instead.")) 
  (/fx arg1 arg2))

(define (sal-scm/instrumented-mpq-div arg1 arg2 ctx-name line column)
  (check-mpq-non-zero arg2 ctx-name line column)
  (/mpq arg1 arg2))

(define (sal-scm/instrumented-idiv arg1 arg2 ctx-name line column)
  (check-non-zero arg2 ctx-name line column)
  (/fx arg1 arg2))

(define (sal-scm/instrumented-modulo arg1 arg2 ctx-name line column)
  (check-non-zero arg2 ctx-name line column)  
  (modulo arg1 arg2))

(define (sal-scm/instrumented-mpq-idiv arg1 arg2 ctx-name line column)
  (check-mpq-non-zero arg2 ctx-name line column)
  (div-mpq arg1 arg2))

(define (sal-scm/instrumented-mpq-modulo arg1 arg2 ctx-name line column)
  (check-mpq-non-zero arg2 ctx-name line column)
  (%mpq arg1 arg2))

(define (sal-scm/check-accessor arg expected-tag ctx-name line column)
  (unless (=fx (car arg) expected-tag)
    (sign-sal-scm-runtime-error ctx-name line column "Invalid use of datatype accessor.")))
