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

(module sal-esm-rearrange
        (include "sal.sch")
        (import sal-esm-lhs runtime sal-lhs-set queue)
        (export (sal-esm-module/rearrange esm-module)
                (sal-esm/rearrange esm ctx available-lhs))
        )

;;----------------------------------------
;; Rearrange the commands in such a way that
;; the need for a dynamic scheduler is minimized. 
;;
;;----------------------------------------

(define (sal-esm-module/rearrange esm-module)
  (status-message :esm-static-scheduler)
  (verbose-message 1 "computing static execution order...")
  (display-runtime 2 "  static order execution time: ~a secs"
    (lambda ()
      (let* ((ctx (make-sal-esm-used-provided-lhs-ctx esm-module))
             (lhs-empty-set (make-sal-lhs-set))
             ;; in the first pass... the definitions that do not depend of input variables are moved to the beginning...
             (new-definition-1 (begin
                                 (sal-esm-used-provided-lhs-ctx/init-section! ctx 'pre-definition)
                                 (sal-esm/rearrange (slot-value esm-module :definition) ctx lhs-empty-set)))
             (new-definition (begin
                               ;; (print "SECOND PASS....")
                               ;; (breakpoint "sal-esm-module/rearrange" (new-definition-1 ctx) #t)
                               ;; second pass...
                               (sal-esm-used-provided-lhs-ctx/init-section! ctx 'definition)
                               (sal-esm/rearrange new-definition-1 ctx lhs-empty-set)))
             (new-initialization (begin 
                                   (sal-esm-used-provided-lhs-ctx/init-section! ctx 'initialization)
                                   (sal-esm/rearrange (slot-value esm-module :initialization) ctx lhs-empty-set)))
             (new-transition (begin 
                               (sal-esm-used-provided-lhs-ctx/init-section! ctx 'transition)
                               (sal-esm/rearrange (slot-value esm-module :transition) ctx lhs-empty-set))))
        (values
         (copy-ast esm-module
                   :definition new-definition
                   :initialization new-initialization
                   :transition new-transition)
         ctx)))
    :esm-static-scheduler-time))
  
(define-generic (sal-esm/rearrange esm ctx available-lhs))

(define-method (sal-esm/rearrange (esm <primitive>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  esm)

(define-method (sal-esm/rearrange (esm <sal-esm-leaf>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  esm)

(define-method (sal-esm/rearrange (esm <sal-esm-new-binds-statement>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (copy-ast esm
            :statement (sal-esm/rearrange (slot-value esm :statement) ctx available-lhs)))

(define-method (sal-esm/rearrange (esm <sal-esm-choice>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (copy-ast esm
            :statements (map (cut sal-esm/rearrange <> ctx available-lhs) (slot-value esm :statements))))

(define-method (sal-esm/rearrange (esm <sal-esm-case>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (copy-ast esm
            :case-entries (map (lambda (entry)
                                 (copy-ast entry
                                           :statement (sal-esm/rearrange (slot-value entry :statement) ctx available-lhs)))
                               (slot-value esm :case-entries))))

(define-method (sal-esm/rearrange (esm <sal-esm-when-undefined>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  esm)

(define-method (sal-esm/rearrange (esm <sal-esm-seq>) (ctx <sal-esm-used-provided-lhs-ctx>) (available-lhs <primitive>))
  (let ((entries (map (lambda (statement)
                         ;; (print "processing...")
                         ;; (sal/pp statement) (print "")
                        (multiple-value-bind
                            (used-lhs provided-lhs)
                            (sal-esm/used-and-provided-lhs statement ctx)
                           ;; (breakpoint "sal-esm/rearrange" (esm ctx available-lhs statement used-lhs provided-lhs) #t)
                           ;; (print "used-lhs:")
                           ;; (sal/pp used-lhs) (print "")
                           ;; (print "provided-lhs:")
                           ;; (sal/pp provided-lhs) (print "")
                          (cons statement (cons used-lhs provided-lhs))))
                      (slot-value esm :statements)))
        (ordered-statements (make-queue)))
    (let loop ((entries entries))
      (unless (null? entries)
        (let ((remaining-entries (make-queue))
              (processed? #f))
          (let inner-loop ((entries entries))
            (unless (null? entries)
              (let* ((curr-entry (car entries))
                     (curr-stat (car curr-entry))
                     (curr-used-provided (cdr curr-entry))
                     (curr-used-set (car curr-used-provided))
                     (curr-provided-set (cdr curr-used-provided)))
                (cond 
                 ((sal-lhs-set/subset? curr-used-set available-lhs)
                  (let ((new-stat (sal-esm/rearrange curr-stat ctx available-lhs)))
                    (set! available-lhs (sal-lhs-set/union available-lhs curr-provided-set))
                    (queue/insert! ordered-statements new-stat)
                    (set! processed? #t)))
                 (else
                  (queue/insert! remaining-entries curr-entry))))
              (inner-loop (cdr entries))))
          (let ((remaining-entries (queue->list remaining-entries)))
            (cond
             (processed?
              [assert (remaining-entries entries) (< (length remaining-entries) (length entries))]
              (loop remaining-entries))
             (else
              [assert (remaining-entries entries) (= (length remaining-entries) (length entries))]
              (queue/append! ordered-statements (map car entries))))))))
    (copy-ast esm
              :statements (queue->list ordered-statements))))
