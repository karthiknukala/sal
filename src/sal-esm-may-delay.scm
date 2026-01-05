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

(module sal-esm-may-delay
        (include "sal.sch")
        (import sal-esm-lhs runtime sal-lhs-set)
        (export (sal-esm-module/mark-may-delay! esm-module)
                (sal-esm/mark-may-delay! esm ctx available-lhs))
        )

;; Implements the MAY-delay analysis.
;;
;; This analysis is used by the explicit state model
;; checker to decide whether a esm-leaf may be delayed
;; or not during simulation/verification time. Much
;; more efficient code can be produced when we know
;; a esm leaf cannot be delayed.

;; SEE ALSO: sal-esm/rearrange at sal-esm-rearrange.scm

(define (sal-esm-module/mark-may-delay! esm-module)
  (status-message :esm-mark-may-delay)
  (verbose-message 1 "marking statements that may be delayed...")
  (display-runtime 2 "  marking execution time: ~a secs"
    (lambda ()
      (let* ((conservative? #t)
             (ctx (make-sal-esm-used-provided-lhs-ctx esm-module conservative?))
             (lhs-empty-set (make-sal-lhs-set)))
        (sal-esm-used-provided-lhs-ctx/init-section! ctx 'definition)
        (sal-esm/mark-may-delay! (slot-value esm-module :definition) ctx lhs-empty-set)
        (sal-esm-used-provided-lhs-ctx/init-section! ctx 'initialization)
        (sal-esm/mark-may-delay! (slot-value esm-module :initialization) ctx lhs-empty-set)
        (sal-esm-used-provided-lhs-ctx/init-section! ctx 'transition)
        (sal-esm/mark-may-delay! (slot-value esm-module :transition) ctx lhs-empty-set)))
    :esm-mark-may-delay-time))
  
(define-generic (sal-esm/mark-may-delay! esm ctx available-lhs))

(define-method (sal-esm/mark-may-delay! (esm <primitive>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  esm)

(define-method (sal-esm/mark-may-delay! (esm <sal-esm-leaf>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (multiple-value-bind
      (used-lhs provided-lhs)
      (sal-esm/used-and-provided-lhs esm ctx)
    (cond
     ((sal-lhs-set/subset? used-lhs available-lhs) 
      ;; (print "the following statement will not be delayed:")
      ;; (sal/pp esm) (print "")
      (set-slot-value! esm :no-delay? #t))
     (else
      ;; (print "MAY BE DELAYED:")
      ;; (sal/pp esm) (print "")
      (set-slot-value! esm :no-delay? #f)))))

(define-method (sal-esm/mark-may-delay! (esm <sal-esm-new-binds-statement>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (sal-esm/mark-may-delay! (slot-value esm :statement) ctx available-lhs))

(define-method (sal-esm/mark-may-delay! (esm <sal-esm-choice>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (for-each (cut sal-esm/mark-may-delay! <> ctx available-lhs) (slot-value esm :statements)))

(define-method (sal-esm/mark-may-delay! (esm <sal-esm-case>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (for-each (lambda (entry)
              (sal-esm/mark-may-delay! (slot-value entry :statement) ctx available-lhs))
            (slot-value esm :case-entries)))

(define-method (sal-esm/mark-may-delay! (esm <sal-esm-when-undefined>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (sal-esm/mark-may-delay! (slot-value esm :statement) ctx available-lhs))
  
(define-method (sal-esm/mark-may-delay! (esm <sal-esm-seq>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (for-each (lambda (statement)
              (multiple-value-bind
                  (used-lhs provided-lhs)
                  (sal-esm/used-and-provided-lhs statement ctx)
                (sal-esm/mark-may-delay! statement ctx available-lhs)
                (set! available-lhs (sal-lhs-set/union available-lhs provided-lhs))))
            (slot-value esm :statements)))

