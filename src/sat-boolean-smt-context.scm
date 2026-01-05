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

(module sat-boolean-smt-context
        (include "sal.sch")
        (import sat-context sat-boolean-context tmp-files smt-interface)
        (export <sat-boolean-smt-context>
                (sat-boolean-smt-context/init! ctx cont-proc))
        )

(define-class <sat-boolean-smt-context> (<sat-boolean-context>) (:file-name :num-parenthesis :num-asserts :vars))

(define *aux-next-id* 0)
(define-inline (mk-aux-var!)
  (let ((r *aux-next-id*))
    (set! *aux-next-id* (+ *aux-next-id* 1))
    r))
(define-inline (display-var var)
  (cond
   ((symbol? var)
    (display var))
   (else
    [assert (var) (number? var)]
    (display* "$a" var))))

(define (sat-boolean-smt-context/init! ctx cont-proc)
  (let ((tmp-file-name (sal/setup-tmp-file! "tmp.smt"))
        (file-name (sal/setup-tmp-file! "input.smt")))
    (set! *aux-next-id* 0)
    (sat-boolean-context/init! ctx)
    (set-slot-value! ctx :num-parenthesis 0)
    (set-slot-value! ctx :num-asserts 0)
    (set-slot-value! ctx :file-name file-name)
    (with-output-to-file tmp-file-name
      (lambda ()
        (cont-proc)))
    (unwind-protect
     (with-output-to-file file-name
       (lambda ()
         (print "(benchmark sal_bmc")
         (print ":source { Automatically produced by sal-bmc }")
         (print ":status unknown")
         (print ":logic QF_UF")
         (unless (null? (slot-value ctx :vars))
           (display ":extrapreds (")
           (for-each (lambda (var)
                       (display* " (" var ")"))
                     (slot-value ctx :vars))
           (print ")"))
         (print ":formula")
         (cond
          ((= (slot-value ctx :num-asserts) 0)
           (print "true"))
          ((= (slot-value ctx :num-asserts) 1)
           (print-file tmp-file-name))
          (else
           (print "(and ")
           (print-file tmp-file-name)
           (print ")")))
         (print ")")))
     (sal/delete-tmp-file! tmp-file-name))))

(define-method (sat-context/make-true (ctx <sat-boolean-smt-context>))
  'true)

(define-method (sat-context/make-false (ctx <sat-boolean-smt-context>))
  'false)

(define-method (sat-boolean-context/make-aux-var (ctx <sat-boolean-smt-context>))
  ;; this method is not called in this file for efficiency reasons
  (let* ((next-idx (slot-value ctx :next-idx))
         (result (symbol-append '$aux (object->symbol next-idx))))
    (set-slot-value! ctx :next-idx (+ next-idx 1))
    result))

(define (inc-num-parenthesis ctx)
  (set-slot-value! ctx :num-parenthesis (+ 1 (slot-value ctx :num-parenthesis))))

(define-method (sat-context/make-ite (ctx <sat-boolean-smt-context>) (c <primitive>) (t <primitive>) (e <primitive>))
  (let ((aux (mk-aux-var!)))
    (display "(flet (")
    (display-var aux)
    (display " (if_then_else ")
    (display-var c)
    (display " ")
    (display-var t)
    (display " ")
    (display-var e)
    (print "))")
    (inc-num-parenthesis ctx)
    aux))

(define-method (sat-context/make-or* (ctx <sat-boolean-smt-context>) (sat-args <primitive>))
  (let ((aux (mk-aux-var!)))
    (display "(flet (")
    (display-var aux)
    (display " (or")
    (for-each (lambda (arg) (display " ") (display-var arg)) sat-args)
    (print "))")
    (inc-num-parenthesis ctx)
    aux))

(define-method (sat-context/make-and* (ctx <sat-boolean-smt-context>) (sat-args <primitive>))
  (let ((aux (mk-aux-var!)))
    (display "(flet (")
    (display-var aux)
    (display " (and")
    (for-each (lambda (arg) (display " ") (display-var arg)) sat-args)
    (print "))")
    (inc-num-parenthesis ctx)
    aux))

(define-method (sat-context/make-not (ctx <sat-boolean-smt-context>) (sat-arg <primitive>))
  (let ((aux (mk-aux-var!)))
    (display "(flet (")
    (display-var aux)
    (display " (not ")
    (display-var sat-arg)
    (print ")) ")
    (inc-num-parenthesis ctx)
    aux))

(define (make-bool-eq ctx arg1 arg2)
  (let ((aux (mk-aux-var!)))
    (display "(flet (")
    (display-var aux)
    (display " (iff ")
    (display-var arg1)
    (display " ")
    (display-var arg2)
    (print ")) ")
    (inc-num-parenthesis ctx)
    aux))

(define-method (sat-context/make-iff (ctx <sat-boolean-smt-context>) (sat-arg1 <primitive>) (sat-arg2 <primitive>))
  (make-bool-eq ctx sat-arg1 sat-arg2))

(define-method (sat-context/make-eq (ctx <sat-boolean-smt-context>) (sat-arg1 <primitive>) (sat-arg2 <primitive>))
  (make-bool-eq ctx sat-arg1 sat-arg2))

(define-method (sat-context/assert (ctx <sat-boolean-smt-context>) (sat-arg <primitive>))
  (display-var sat-arg)
  (let loop ((idx (slot-value ctx :num-parenthesis)))
    (when (> idx 0)
      (display ")")
      (loop (- idx 1))))
  (print)
  (set-slot-value! ctx :num-asserts (+ (slot-value ctx :num-asserts) 1))
  (set-slot-value! ctx :num-parenthesis 0))

(define-method (sat-context/solve (ctx <sat-boolean-smt-context>))
  (unwind-protect
   (smt/execute (slot-value ctx :file-name))
   (sal/delete-tmp-file! (slot-value ctx :file-name))))






    

        
