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

(module sat-boolean-ics-context
        (include "sal.sch")
        (import sat-context sat-boolean-context tmp-files ics-interface)
        (export <sat-boolean-ics-context>
                (sat-boolean-ics-context/init! ctx cont-proc))
        )

(define-class <sat-boolean-ics-context> (<sat-boolean-context>) (:file-name :top-level-args))

(define (sat-boolean-ics-context/init! ctx cont-proc)
  (let ((file-name (sal/setup-tmp-file! "input.ics")))
    (sat-boolean-context/init! ctx)
    (set-slot-value! ctx :file-name file-name)
    (set-slot-value! ctx :top-level-args '())
    (with-output-to-file file-name
      (lambda ()
        (cont-proc)
        (display "sat ")
        (display-ics-infix-app (slot-value ctx :top-level-args) "&")
        (print ".")))))

(define (display-ics-infix-app args ics-op)
  (let loop ((first? #t)
             (args args))
    (unless (null? args)
      (unless first?
        (display* " " ics-op " "))
      (display (car args))
      (loop #f (cdr args)))))
  
(define (make-ics-app ctx args ics-op)
  (let ((result (sat-boolean-context/make-aux-var ctx)))
    (display* "prop " result " := ")
    (display-ics-infix-app args ics-op)
    (print ".")
    result))

(define-method (sat-context/make-and* (ctx <sat-boolean-ics-context>) (sat-args <primitive>))
  (if (null? sat-args)
    "tt"
    (make-ics-app ctx sat-args "&")))

(define-method (sat-context/make-or* (ctx <sat-boolean-ics-context>) (sat-args <primitive>))
  (if (null? sat-args)
    "ff"
    (make-ics-app ctx sat-args "|")))

(define-method (sat-context/make-not (ctx <sat-boolean-ics-context>) (sat-arg <primitive>))
  (let ((result (sat-boolean-context/make-aux-var ctx)))
    (print "prop " result " := ~" sat-arg ".")
    result))

(define (make-ics-bool-eq ctx arg1 arg2)
  (let ((result (sat-boolean-context/make-aux-var ctx)))
    (print "prop " result " := " arg1 " <=> " arg2 ".")
    result))

(define-method (sat-context/make-iff (ctx <sat-boolean-ics-context>) (sat-arg1 <primitive>) (sat-arg2 <primitive>))
  (make-ics-bool-eq ctx sat-arg1 sat-arg2))

(define-method (sat-context/make-eq (ctx <sat-boolean-ics-context>) (sat-arg1 <primitive>) (sat-arg2 <primitive>))
  (make-ics-bool-eq ctx sat-arg1 sat-arg2))

(define-method (sat-context/make-true (ctx <sat-boolean-ics-context>))
  "tt")

(define-method (sat-context/make-false (ctx <sat-boolean-ics-context>))
  "ff")

(define-method (sat-context/make-ite (ctx <sat-boolean-ics-context>) (c <primitive>) (t <primitive>) (e <primitive>))
  (let ((result (sat-boolean-context/make-aux-var ctx)))
    (print "prop " result " := if " c " then " t " else " e " end.")
    result))

(define-method (sat-context/assert (ctx <sat-boolean-ics-context>) (sat-arg <primitive>))
  (set-slot-value! ctx :top-level-args (cons sat-arg (slot-value ctx :top-level-args))))

(define-method (sat-context/solve (ctx <sat-boolean-ics-context>))
  (unwind-protect 
   (ics/execute-core (slot-value ctx :file-name))
   (sal/delete-tmp-file! (slot-value ctx :file-name))))
