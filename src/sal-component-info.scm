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

(module sal-component-info
        (include "sal.sch")
        (import queue sal-ast-for-each sal-module)
        (export (sal-component-info/empty? info)
                (sal-component-info/contains-multi-component? info)
                (sal-component-info/simplify info)
                (sal-component-info/convert-data component-info proc))
        )

(define-generic (sal-component-info/empty? info))

(define-method (sal-component-info/empty? (info <sal-base-component-info>))
  (and (null? (slot-value info :input-data))
       (null? (slot-value info :output-data))
       (null? (slot-value info :owned-data))))

(define-method (sal-component-info/empty? (info <sal-multi-component-info>))
  (sal-component-info/empty? (slot-value info :component)))

(define-method (sal-component-info/empty? (info <sal-composite-component-info>))
  (for-all sal-component-info/empty? (slot-value info :components)))

(define (make-empty-component-info place-provider)
  (make-ast-instance <sal-base-component-info> place-provider
                     :input-data '()
                     :output-data '()
                     :owned-data '()))

(define (sal-component-info/contains-multi-component? info)
  (sal-ast/find (cut instance-of? <> <sal-multi-component-info>) info))
                      
(define-generic (sal-component-info/simplify info))

(define-method (sal-component-info/simplify (info <sal-component-info>))
  info)

(define-method (sal-component-info/simplify (info <sal-composite-component-info>))
  (let* ((component-queue (make-queue))
         (insert! (lambda (comp)
                    (unless (sal-component-info/empty? comp)
                      (queue/insert! component-queue comp))))
         (components (slot-value info :components)))
    (for-each (lambda (component)
                (let ((new-component (sal-component-info/simplify component)))
                  (if (instance-of? new-component <sal-composite-component-info>)
                    (for-each (lambda (new-component-child)
                                (insert! new-component-child))
                              (slot-value new-component :components))
                    (insert! new-component))))
              components)
    (let ((new-components (queue->list component-queue)))
      (cond
       ((null? new-components)
        (make-empty-component-info info))
       ((null? (cdr new-components))
        (car new-components))
       (else
        (copy-ast info 
                  :components new-components))))))

(define-method (sal-component-info/simplify (info <sal-multi-component-info>))
  (copy-ast info :component (sal-component-info/simplify (slot-value info :component))))

;; proc is a function that converts a datum -> a list of datum 
;; Remark: this function can be used to remove datum (proc returns the empty list).
(define-generic (sal-component-info/convert-data component-info proc))
(define-method (sal-component-info/convert-data (component-info <sal-base-component-info>) (proc <primitive>))
  (let* ((aux-decl-queue (make-queue))
         (convert-data (lambda (data-list)
                         (let ((data-queue (make-queue)))
                           (for-each (lambda (datum)
                                       (queue/append! data-queue (proc datum)))
                                     data-list)
                           (queue->list data-queue)))))
    (copy-ast component-info
              :input-data (convert-data (slot-value component-info :input-data))
              :output-data (convert-data (slot-value component-info :output-data))
              :owned-data (convert-data (slot-value component-info :owned-data)))))

(define-method (sal-component-info/convert-data (info <sal-composite-component-info>) (proc <primitive>))
  (copy-ast info
            :components (map (cut sal-component-info/convert-data <> proc) (slot-value info :components))))

(define-method (sal-component-info/convert-data (info <sal-multi-component-info>) (proc <primitive>))
  (copy-ast info
            :component (sal-component-info/convert-data (slot-value info :component) proc)))




