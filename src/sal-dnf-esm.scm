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

(module sal-dnf-esm
        (include "sal.sch")
        (import sal-esm sal-ast-expand sal-ast-env 
                sal-ast-copy iterators sal-type 
                sal-expr-evaluator runtime sal-ast-for-each)
        (export (esm->dnf-esm esm)
                (esm->dnf-esm-core esm env dnf))
        )

(define-generic (esm->dnf-esm esm))

(define-method (esm->dnf-esm (esm <sal-esm-module>))
  (status-message :esm-dnf-translation)
  (verbose-message 1 "building DNF module representation...")
  (display-runtime 2 "  DNF translation time: ~a secs"
    (lambda ()
      (copy-ast esm
                :initialization (esm->dnf-esm (slot-value esm :initialization))
                :transition (esm->dnf-esm (slot-value esm :transition))))
    :esm-dnf-translation-time))

(define-method (esm->dnf-esm (esm <sal-esm-statement>))
  (let ((dnf (esm->dnf-esm-core esm (make-empty-env) (list (cons '() '()))))
        (place-provider esm))
    (sal-esm/make-esm-choice* (map (lambda (entry)
                                     (let ((local-decls (car entry))
                                           (guards '())
                                           (assignments '()))
                                       (for-each (lambda (stat)
                                                   (if (instance-of? stat <sal-esm-guard>)
                                                     (push! stat guards)
                                                     (push! stat assignments)))
                                                 (cdr entry))
                                       (let ((seq-statement (sal-esm/make-esm-seq 
                                                             (sal-esm/make-esm-seq* guards place-provider)
                                                             (sal-esm/make-esm-seq* assignments place-provider)
                                                             place-provider)))
                                         (if (null? local-decls)
                                           seq-statement
                                           (make-ast-instance <sal-esm-multi-choice> place-provider
                                                              :local-decls local-decls
                                                              :statement seq-statement)))))
                                   dnf)
                              place-provider)))

;; dnf structure is a list of pairs.
;; each pair is composed of a list of declarations (quantified variables) and a
;; list of esm leaves.
(define-generic (esm->dnf-esm-core esm env dnf))

(define (append-statement esm env dnf)
  ;; add esm to each list of esm leaves.
  (map (lambda (entry)
         (cons (car entry) (cons (sal-ast/substitute esm env) (cdr entry))))
       dnf))

(define-method (esm->dnf-esm-core (esm <sal-esm-leaf>) (env <primitive>) (dnf <primitive>))
  (append-statement esm env dnf))

(define-method (esm->dnf-esm-core (esm <sal-esm-seq>) (env <primitive>) (dnf <primitive>))
  (let loop ((statements (slot-value esm :statements))
             (dnf dnf))
    (if (null? statements)
      dnf
      (loop (cdr statements)
            (esm->dnf-esm-core (car statements) env dnf)))))

(define-method (esm->dnf-esm-core (esm <sal-esm-choice>) (env <primitive>) (dnf <primitive>))
  (if (for-all (lambda (arg)
                 (instance-of? arg <sal-esm-assignment>))
               (slot-value esm :statements))
    (append-statement esm env dnf)
    (let ((dnf-list (map (cut esm->dnf-esm-core <> env dnf) (slot-value esm :statements))))
      (fold-left append '() dnf-list))))

(define-method (esm->dnf-esm-core (esm <sal-esm-multi-choice>) (env <primitive>) (dnf <primitive>))
  (let* ((local-decls (slot-value esm :local-decls))
         (new-local-decls (map (cut sal-ast/substitute <> env) local-decls))
         (new-dnf (map (lambda (entry)
                         (cons (append new-local-decls (car entry)) (cdr entry)))
                       dnf))
         (new-env (update-env* env local-decls new-local-decls)))
    (esm->dnf-esm-core (slot-value esm :statement) new-env new-dnf)))

(define-method (esm->dnf-esm-core (esm <sal-esm-multi-seq>) (env <primitive>) (dnf <primitive>))
  (if (sal-ast/find (lambda (ast)
                      (or (instance-of? ast <sal-esm-multi-choice>)
                          (instance-of? ast <sal-esm-choice>)))
                    esm)
    (try
     (let* ((place-provider esm)
            (local-decls (slot-value esm :local-decls))
            (body (slot-value esm :statement))
            (types (map (cut slot-value <> :type) local-decls))
            (tuple-type (make-ast-instance <sal-tuple-type> place-provider
                                           :types types))
            (proc-child (lambda (child)
                          (sal-expr/evaluate-core child env 0)))
            (iterator (sal-type/make-iterator-core tuple-type proc-child))
            (dnf dnf))
       (iterator/for-each 
        (lambda (tuple-literal)
          (let* ((new-env (update-env* env local-decls (slot-value tuple-literal :exprs)))
                 (new-dnf (esm->dnf-esm-core body new-env dnf)))
            (set! dnf new-dnf)))
        iterator)
       dnf)
     (catch* '(type-iterator expr-evaluator)
             (lambda (_ msg)
               (sign-expander-error esm "Failed to expand quantifier, reason: ~a" msg))))
    (append-statement esm env dnf)))


  



