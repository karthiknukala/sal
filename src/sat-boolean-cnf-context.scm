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

(module sat-boolean-cnf-context
        (include "sal.sch")
        (import sat-context sat-boolean-context tmp-files)
        (export <sat-boolean-cnf-context>
                (sat-boolean-cnf-context/init! ctx solver-proc cont-proc))
        )

(define-class <sat-boolean-cnf-context> (<sat-boolean-context>) (:file-name :num-clauses :solver-proc))

(define-inline (make-aux-var ctx)
  (let ((result (slot-value ctx :next-idx)))
    (set-slot-value! ctx :next-idx (+ result 1))
    result))

(define-method (sat-boolean-context/make-aux-var (ctx <sat-boolean-cnf-context>))
  (make-aux-var ctx))

(define (sat-boolean-cnf-context/init! ctx solver-proc cont-proc)
  (let ((tmp-file-name (sal/setup-tmp-file! "tmp.cnf"))
        (file-name (sal/setup-tmp-file! "input.cnf")))
    (sat-boolean-context/init! ctx)
    (set-slot-value! ctx :solver-proc solver-proc)
    (set-slot-value! ctx :num-clauses 1)
    (set-slot-value! ctx :file-name file-name)
    (let ((first-var (make-aux-var ctx)))
      [assert (first-var) (= first-var 1)]
      (with-output-to-file tmp-file-name
        (lambda ()
          (print "1 0") ;; add clause (1 = true)
          (cont-proc)))
      (unwind-protect
       (with-output-to-file file-name
         (lambda ()
           (print "p cnf " (- (slot-value ctx :next-idx) 1) " " (slot-value ctx :num-clauses))
           (print-file tmp-file-name)))
       (sal/delete-tmp-file! tmp-file-name)))))

(define-inline (display-cnf-clause1 ctx lit1)
  (print lit1 " 0")
  (set-slot-value! ctx :num-clauses (+ (slot-value ctx :num-clauses) 1)))

(define-inline (display-cnf-clause2 ctx lit1 lit2)
  (print lit1 " " lit2 " 0")
  (set-slot-value! ctx :num-clauses (+ (slot-value ctx :num-clauses) 1)))

(define-inline (display-cnf-clause3 ctx lit1 lit2 lit3)
  (print lit1 " " lit2 " " lit3 " 0")
  (set-slot-value! ctx :num-clauses (+ (slot-value ctx :num-clauses) 1)))

(define-inline (display-cnf-clause ctx literals)
  (for-each (lambda (lit)
              (display* lit " "))
            literals)
  (print " 0")
  (set-slot-value! ctx :num-clauses (+ (slot-value ctx :num-clauses) 1)))

(define-inline (display-cnf-clause-neg-all-but-first ctx literals)
  (display* (car literals) " ")
  (for-each (lambda (lit)
              (display* (- 0 lit) " "))
            (cdr literals))
  (print " 0")
  (set-slot-value! ctx :num-clauses (+ (slot-value ctx :num-clauses) 1)))
  
(define-method (sat-context/make-and* (ctx <sat-boolean-cnf-context>) (sat-args <primitive>))
  (let* ((result (make-aux-var ctx))
         (neg-result (- 0 result)))
    (display-cnf-clause-neg-all-but-first ctx (cons result sat-args))
    (for-each (lambda (arg)
                (display-cnf-clause2 ctx neg-result arg))
              sat-args)
    result))

(define-method (sat-context/make-or* (ctx <sat-boolean-cnf-context>) (sat-args <primitive>))
  (let* ((result (make-aux-var ctx))
         (neg-result (- 0 result)))
    (display-cnf-clause ctx (cons neg-result sat-args))
    (for-each (lambda (arg)
                (display-cnf-clause2 ctx result (- 0 arg)))
              sat-args)
    result))

(define-method (sat-context/make-not (ctx <sat-boolean-cnf-context>) (sat-arg <primitive>))
  (- 0 sat-arg))

(define-method (sat-context/make-true (ctx <sat-boolean-cnf-context>))
  1)

(define-method (sat-context/make-false (ctx <sat-boolean-cnf-context>))
  -1)

(define-method (sat-context/make-ite (ctx <sat-boolean-cnf-context>) (c <primitive>) (t <primitive>) (e <primitive>))
  (let* ((result (make-aux-var ctx))
         (neg-result (- 0 result))
         (neg-c (- 0 c))
         (neg-t (- 0 t))
         (neg-e (- 0 e)))
    (display-cnf-clause3 ctx neg-result neg-c t)
    (display-cnf-clause3 ctx neg-result c e)
    (display-cnf-clause3 ctx result c neg-e)
    (display-cnf-clause3 ctx result neg-c neg-t)
    result))

(define (make-cnf-bool-eq ctx arg1 arg2)
  (let* ((result (make-aux-var ctx))
         (neg-result (- 0 result))
         (neg-arg1 (- 0 arg1))
         (neg-arg2 (- 0 arg2)))
    (display-cnf-clause3 ctx neg-result arg1 neg-arg2)
    (display-cnf-clause3 ctx neg-result neg-arg1 arg2)
    (display-cnf-clause3 ctx result arg1 arg2)
    (display-cnf-clause3 ctx result neg-arg1 neg-arg2)
    result))
    
(define-method (sat-context/make-iff (ctx <sat-boolean-cnf-context>) (sat-arg1 <primitive>) (sat-arg2 <primitive>))
  (make-cnf-bool-eq ctx sat-arg1 sat-arg2))

(define-method (sat-context/make-eq (ctx <sat-boolean-cnf-context>) (sat-arg1 <primitive>) (sat-arg2 <primitive>))
  (make-cnf-bool-eq ctx sat-arg1 sat-arg2))

(define-method (sat-context/assert (ctx <sat-boolean-cnf-context>) (sat-arg <primitive>))
  (display-cnf-clause1 ctx sat-arg))

(define-method (sat-context/solve (ctx <sat-boolean-cnf-context>))
  (unwind-protect
   ((slot-value ctx :solver-proc) (slot-value ctx :file-name))
   (sal/delete-tmp-file! (slot-value ctx :file-name))))



