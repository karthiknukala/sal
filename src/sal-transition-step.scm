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

(module sal-transition-step
        (include "sal.sch")
        (import gmp-scheme sal-expression sal-type iterators queue 
                sal-expr-evaluator pretty sal-pp sal-trace-info)
        (export <sal-transition-step>
                <sal-simple-transition-step>
                <sal-else-transition-step>
                <sal-nested-transition-step>
                <sal-labeled-transition-step>
                <sal-module-transition-step>
                <sal-module-instance-transition-step>
                <sal-nested-list-transition-step>
                <sal-let-transition-step>
                (make-sal-transition-step info choice-assignment-proc)
                (sal-transition-step->doc transition-step detailed?))
        )

(define-class <sal-transition-step> (<sal-ast>) ()
  :doc "Abstract class for all transition information subclasses. It contains just place information since it is a subclass of @code{<sal-ast>}.")
(define-class <sal-simple-transition-step> (<sal-transition-step>) ()
  :doc "Simple transition step information (i.e., just line and column numbers).")
(define-class <sal-else-transition-step> (<sal-transition-step>) ()
  :doc "Signs that an ELSE transition was performed.")
(define-class <sal-nested-transition-step> (<sal-transition-step>) (:nested-transition)
  :doc "Superclass for all nested transition steps, that is, transition steps which contain other transition step information object.")
(define-class <sal-labeled-transition-step> (<sal-nested-transition-step>) (:label)
  :doc "A labeled transition with label @{:label} was executed.")
(define-class <sal-module-transition-step> (<sal-nested-transition-step>) ()
  :doc "A transition of the module located at @code{:place} was executed, the slot {:nested-transition} contains additional information.")
(define-class <sal-module-instance-transition-step> (<sal-module-transition-step>) ()
  :doc "A transition of the module instance located at @code{:place} was executed, the slot {:nested-transition} contains additional information.")
(define-class <sal-nested-list-transition-step> (<sal-transition-step>) (:nested-transition-list)
  :doc "A set of transitions was executed (it is the case when synchronous composition is used in the model). @code{:nested-transition-list} contains a list of transition step objects which contains information about each transition in the set.")
(define-class <sal-let-transition-step> (<sal-nested-transition-step>) (:with-var-value-list)
  :doc "A transition step which binds the variables in the list of pairs (variable - value) in @code{:with-var-value-list}.")
(define (make-sal-transition-step info choice-assignment-proc)
  (make-sal-transition-step-core info choice-assignment-proc '()))

(define (try-to-evaluate value)
  (try-until-success (sal-expr/evaluate value) value))

(define (choice-value choice-var-name choice-assignment-proc array-idx-list)
  (try
   (let ((choice-array (choice-assignment-proc choice-var-name))
         (array-idx-list (reverse array-idx-list)))
     (let loop ((value choice-array)
                (array-idx-list array-idx-list))
       (if (null? array-idx-list)
         value
         (loop (try-to-evaluate (sal-expr/apply value (car array-idx-list)))
               (cdr array-idx-list)))))
   (catch 'expr-evaluator
          (lambda (_) 'unknown))))

(define-generic (make-sal-transition-step-core info choice-assignment-proc array-idx-list))

(define-method (make-sal-transition-step-core (info <sal-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (make-ast-instance <sal-transition-step> info))

(define-method (make-sal-transition-step-core (info <sal-transition-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (make-ast-instance <sal-simple-transition-step> info))

(define-method (make-sal-transition-step-core (info <sal-else-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (make-ast-instance <sal-else-transition-step> info))

(define-method (make-sal-transition-step-core (info <sal-nested-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (make-ast-instance <sal-nested-transition-step> info
                     :nested-transition (make-sal-transition-step-core (slot-value info :info) choice-assignment-proc array-idx-list)))

(define-method (make-sal-transition-step-core (info <sal-module-instance-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (change-class (call-next-method) <sal-module-instance-transition-step>))

(define-method (make-sal-transition-step-core (info <sal-labeled-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (make-ast-instance <sal-labeled-transition-step> info
                     :nested-transition (make-sal-transition-step-core (slot-value info :info) choice-assignment-proc array-idx-list)
                     :label (slot-value info :label)))

(define-method (make-sal-transition-step-core (info <sal-choice-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (let ((value (choice-value (slot-value info :choice-var-name) choice-assignment-proc array-idx-list)))
    (cond
     ((instance-of? value <sal-numeral>)
      (let* ((idx (mpq->integer (slot-value value :num)))
             (selected-info (list-ref (slot-value info :info-list) idx)))
        (make-sal-transition-step-core selected-info choice-assignment-proc array-idx-list)))
     (else
      (warning-message "Information necessary to build transition is missing.") 
      (make-ast-instance <sal-transition-step> info)))))

(define-method (make-sal-transition-step-core (info <sal-sequence-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (if (sal-trace-info/contains-choice? info)
    (make-ast-instance <sal-nested-list-transition-step> info
                       :nested-transition-list (map (cut make-sal-transition-step-core <> choice-assignment-proc array-idx-list)
                                                    (slot-value info :info-list)))
    (make-ast-instance <sal-module-transition-step> info)))

(define-method (make-sal-transition-step-core (info <sal-multi-choice-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
    (let* ((original-var-names (slot-value info :original-var-names))
           (choice-var-names (slot-value info :choice-var-names)))
      [assert (original-var-names choice-var-names) (= (length original-var-names) (length choice-var-names))]
      [assert (original-var-names) (not (null? original-var-names))]
      (make-ast-instance <sal-let-transition-step> info
                         :with-var-value-list (map (lambda (original-var-name choice-var-name)
                                                     (cons original-var-name
                                                           (choice-value choice-var-name choice-assignment-proc array-idx-list)))
                                                   original-var-names
                                                   choice-var-names)
                         :nested-transition (make-sal-transition-step-core (slot-value info :info) choice-assignment-proc array-idx-list))))

(define-method (make-sal-transition-step-core (info <sal-multi-sequence-trace-info>) (choice-assignment-proc <primitive>) (array-idx-list <primitive>))
  (cond
   ((sal-trace-info/find-choice-var info) =>
    ;; I use the value of the choice var to compute the number of elements in the multi sequence
    (lambda (choice-var)
      ;; (breakpoint "make-sal-transition-step-core" (info choice-var choice-assignment-proc array-idx-list) #t)
      (let* ((value (choice-value choice-var choice-assignment-proc array-idx-list))
             (value-type (sal-expr/type value)))
        [assert (value-type) (sal-type/array? value-type)]
        (try
         (let* ((it (sal-type/make-iterator (sal-function-type/domain value-type)))
                (child-trans-step-queue (make-queue))
                (child-info (slot-value info :info))
                (idx-var-name (slot-value info :idx-var-name)))
           (iterator/for-each (lambda (domain-value)
                                (let* ((child-trans-step (make-sal-transition-step-core child-info choice-assignment-proc (cons domain-value array-idx-list)))
                                       (let-trans-step (make-ast-instance <sal-let-transition-step> child-info
                                                                          :with-var-value-list (list (cons idx-var-name domain-value))
                                                                          :nested-transition child-trans-step)))
                                  (queue/insert! child-trans-step-queue let-trans-step)))
                              it)
           (make-ast-instance <sal-nested-list-transition-step> info
                              :nested-transition-list (queue->list child-trans-step-queue)))
         (catch 'type-iterator
                (lambda (_)
                  (make-ast-instance <sal-module-transition-step> info)))))))
   (else
    (make-ast-instance <sal-module-transition-step> info))))

(define-generic (sal-transition-step->doc transition-step detailed?))

(define-method (sal-transition-step->doc (transition-step <sal-transition-step>) (detailed? <primitive>))
  (format-with-location transition-step ""))

(define-method (sal-transition-step->doc (transition-step <sal-simple-transition-step>) (detailed? <primitive>))
  (pp/concat "transition at " (format-with-location transition-step "")))

(define-method (sal-transition-step->doc (transition-step <sal-else-transition-step>) (detailed? <primitive>))
  (pp/concat "else transition at " (format-with-location transition-step "")))

(define-method (sal-transition-step->doc (transition-step <sal-nested-transition-step>) (detailed? <primitive>))
  (sal-transition-step->doc (slot-value transition-step :nested-transition) detailed?))

(define-method (sal-transition-step->doc (transition-step <sal-module-transition-step>) (detailed? <primitive>))
  (pp/concat "module at " (format-with-location transition-step "")))

(define-method (sal-transition-step->doc (transition-step <sal-module-instance-transition-step>) (detailed? <primitive>))
  (if detailed?
    (pp/list-style4 "module instance at" 2 (format-with-location transition-step "")
                    (sal-transition-step->doc (slot-value transition-step :nested-transition) detailed?))
    (call-next-method)))

(define-method (sal-transition-step->doc (transition-step <sal-labeled-transition-step>) (detailed? <primitive>))
  (pp/list-style4 'label 2 (sal-identifier/name (slot-value transition-step :label))
                  (call-next-method)))

(define-method (sal-transition-step->doc (transition-step <sal-nested-list-transition-step>) (detailed? <primitive>))
  (apply pp/list-style6 (map (cut sal-transition-step->doc <> detailed?) (slot-value transition-step :nested-transition-list))))

(define-method (sal-transition-step->doc (transition-step <sal-let-transition-step>) (detailed? <primitive>))
  (let ((pp-choice (lambda (entry)
                     (let* ((original-var-name (car entry))
                            (value (cdr entry))
                            (indent (+ (symbol-length original-var-name) 3)))
                       (pp/group (pp/nest* indent original-var-name " =" *doc-line* (sal->doc value))))))
        (var-values (slot-value transition-step :with-var-value-list)))
    (pp/list-style3 "with" 2 2
                    (apply pp/concat
                           (pp-choice (car var-values))
                           (map (lambda (entry)
                                  (pp/group (pp/concat "," *doc-line* (pp-choice entry))))
                                (cdr var-values)))
                    (pp/concat "at " (format-with-location transition-step ""))
                    (call-next-method))))


