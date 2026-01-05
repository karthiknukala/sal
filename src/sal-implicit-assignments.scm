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

(module sal-implicit-assignments
        (include "sal.sch")
        (import sal-expression sal-ast-support sal-ast-eq symbol-table queue sal-ast-list
                sal-ast-simplify sal-module sal-type sal-decls sal-pp)
        (export (sal-base-module/insert-implicit-assignments base-module))
        )

(define-generic (sal-lhs/supported? lhs))
(define-method (sal-lhs/supported? (lhs <sal-expr>)) #t)
(define-method (sal-lhs/supported? (lhs <sal-selection>))
  (sal-lhs/supported? (sal-selection/target lhs)))
(define-method (sal-lhs/supported? (lhs <sal-array-selection>))
  (and (call-next-method)
       (sal-expr/first-order-value? (slot-value lhs :arg))))

(define (sal-simple-definition/trivial? simple-def)
  [instance-check sal-simple-definition/trivial? simple-def <sal-simple-definition>]
  (sal-lhs/trivial? (slot-value simple-def :lhs)))

(define-generic (sal-simple-definition/supported? simple-def))
(define-method (sal-simple-definition/supported? (simple-def <sal-simple-definition>))
  (sal-lhs/supported? (slot-value simple-def :lhs)))
(define-method (sal-simple-definition/supported? (simple-def <sal-simple-selection-definition>))
  (sal-lhs/trivial? (slot-value simple-def :lhs)))

(define-generic (sal-simple-definition/convert-to-supported simple-def))
(define-method (sal-simple-definition/convert-to-supported (simple-def <sal-simple-selection-definition>))
  (sign-unsupported-feature simple-def "The left-hand-side of the simple definition cannot be efficiently simplified. This simple definition is a non-deterministic one, that is, it uses the IN operator. You can avoid the problem rewriting your simple definition, and using an auxiliary variable. For instance, the simple definition \"x[i] IN {0,2,4,6};\" can be encoded as \"x[i] = new-var;\", where new-var is a new variable, and is defined in the DEFINITION section as \"new-var IN {0, 2, 4, 6};\"."))
(define-method (sal-simple-definition/convert-to-supported (simple-def <sal-simple-definition>))
  [assert (simple-def) (not (sal-simple-definition/supported? simple-def))]
;   (print "converting:")
;   (sal/pp simple-def) (print "")
;   (print "get-new-lhs: ")
;   (sal/pp (get-new-lhs (slot-value simple-def :lhs)))
;   (print "\n------------------")
  (let* ((lhs (slot-value simple-def :lhs))
         (new-lhs (get-new-lhs (slot-value simple-def :lhs))))
    (multiple-value-bind
        (new-rhs new-rhs-pos)
        (gen-new-rhs lhs new-lhs)
      (set-slot-value! new-rhs-pos :new-value (slot-value simple-def :rhs))
      (let ((new-def (copy-ast simple-def
                               :lhs new-lhs
                               :rhs new-rhs)))
        ;; (print "new-def: ")
        ;; (sal/pp new-def) (print "")
        new-def))))

(define-generic (get-new-lhs lhs))
(define-method (get-new-lhs (lhs <sal-next-operator>))
  (values lhs #t))
(define-method (get-new-lhs (lhs <sal-selection>))
  (multiple-value-bind
      (new-lhs no-unknown-array-sel?)
      (get-new-lhs (sal-selection/target lhs))
    (if no-unknown-array-sel?
      (values lhs     #t)
      (values new-lhs #f))))
(define-method (get-new-lhs (lhs <sal-array-selection>))
  (multiple-value-bind
      (new-lhs no-unknown-array-sel?)
      (get-new-lhs (slot-value lhs :fun))
    (if (and no-unknown-array-sel?
             (sal-expr/first-order-value? (slot-value lhs :arg)))
      (values lhs #t)
      (values new-lhs #f))))

(define-generic (gen-new-rhs lhs new-lhs))
(define-method (gen-new-rhs (lhs <sal-array-selection>) (new-lhs <sal-expr>))
  (if (eq? (slot-value lhs :fun) new-lhs)
    (let ((new-rhs (make-ast-instance <sal-array-update> lhs
                                      :target (sal-next-lhs->lhs new-lhs)
                                      :idx (slot-value lhs :arg))))
      (values new-rhs new-rhs))
    (multiple-value-bind
        (new-rhs rhs-pos)
        (gen-new-rhs (slot-value lhs :fun) new-lhs)
      (let ((new-rhs-pos (make-ast-instance <sal-array-update> lhs
                                            :target (sal-next-lhs->lhs (slot-value lhs :fun))
                                            :idx (slot-value lhs :arg))))
        (set-slot-value! rhs-pos :new-value new-rhs-pos)
        (values new-rhs new-rhs-pos)))))
(define-method (gen-new-rhs (lhs <sal-selection>) (new-lhs <sal-expr>))
  (multiple-value-bind
      (new-rhs rhs-pos)
      (gen-new-rhs (slot-value lhs :target) new-lhs)
    (let* ((update-class (if (instance-of? lhs <sal-tuple-selection>)
                           <sal-tuple-update>
                           <sal-record-update>))
           (new-rhs-pos (make-ast-instance update-class lhs
                                           :target (sal-next-lhs->lhs (slot-value lhs :target))
                                           :idx (slot-value lhs :idx))))
      (set-slot-value! rhs-pos :new-value new-rhs-pos)
      (values new-rhs new-rhs-pos))))

(define (throw-invalid-simple-def lhs)
  (sign-source-error lhs "Invalid (or non-supported) simple definition:\n - Error case: you are trying to define the same location (left-hand-side) twice.\n - Non-supported feature: there are two left-hand-sides (such as: X'[i] and X'[j]) which define two different indices of an array, but it was not possible to decide whether the indexes are different (i.e., i /= j) or not. If this is the case, please, rewrite your specification using array updates. For instance, the following definitions:\n\n   X'[i] = expr1;\n   X'[j] = expr2\n\n can be rewritten as:\n\n   X' = (X WITH [i] := expr1) WITH [j] := expr2"))

(define-class <sal-assignment-info> () ())
(define-class <sal-undef-assignment-info> (<sal-assignment-info>) ())
(define-class <sal-partially-def-assignment-info> (<sal-assignment-info>) ())
(define-class <sal-def-assignment-info> (<sal-partially-def-assignment-info>) ())
(define-class <sal-def-assignment-info-with-rhs> (<sal-def-assignment-info>) (:rhs))
(define-class <sal-collection-assignment-info> (<sal-partially-def-assignment-info>) (:children))
(define-class <sal-tuple-assignment-info> (<sal-collection-assignment-info>) ())
(define-class <sal-record-assignment-info> (<sal-collection-assignment-info>) ())
(define-class <sal-array-assignment-info> (<sal-collection-assignment-info>) ())
(define-class <sal-entry-info> () (:idx :info))

(define *sal-undef-assignment-info* (make-instance <sal-undef-assignment-info>))
(define *sal-def-assignment-info* (make-instance <sal-def-assignment-info>))

(define-generic (sal-assignment-info/union info1 info2))
(define-method (sal-assignment-info/union (info1 <sal-assignment-info>) (info2 <sal-undef-assignment-info>)) info1)
(define-method (sal-assignment-info/union (info1 <sal-undef-assignment-info>) (info2 <sal-assignment-info>)) info2)
(define-method (sal-assignment-info/union (info1 <sal-def-assignment-info>) (info2 <sal-partially-def-assignment-info>))
  ;; (sal/pp info1) (print "") (sal/pp info2) (print "\n------------")
  (throw-invalid-simple-def (slot-value info1 :rhs)))
(define-method (sal-assignment-info/union (info1 <sal-partially-def-assignment-info>) (info2 <sal-def-assignment-info>))
  (throw-invalid-simple-def (slot-value info2 :rhs)))
(define-method (sal-assignment-info/union (info1 <sal-collection-assignment-info>) (info2 <sal-collection-assignment-info>))
  [assert (info1 info2) (eq? (class-of info1) (class-of info2))]
  (let* ((children1 (slot-value info1 :children))
         (children2 (slot-value info2 :children))
         (result (map (lambda (child)
                        (copy-instance child))
                      children1)))
    (for-each (lambda (entry2)
                (unless (exists (lambda (result-entry)
                                  [assert (result-entry entry2) (instance-of? (slot-value result-entry :idx) <sal-ast>)]
                                  [assert (entry2 result-entry) (instance-of? (slot-value entry2 :idx) <sal-ast>)]
                                  (and (sal-ast/equivalent? (slot-value result-entry :idx)
                                                            (slot-value entry2 :idx))
                                       (set-slot-value! result-entry :info (sal-assignment-info/union (slot-value entry2 :info) 
                                                                                                      (slot-value result-entry :info)))
                                       #t))
                                result)
                  (set! result (cons entry2 result))))
              children2)
    (make-instance (class-of info1) 
                   :children result)))

(define-generic (sal-lhs->assignment-info lhs info))
(define-method (sal-lhs->assignment-info (lhs <sal-name-expr>) (info <sal-assignment-info>))
  info)
(define-method (sal-lhs->assignment-info (lhs <sal-next-operator>) (info <sal-assignment-info>))
  info)
(define (sal-collection->assignment-info lhs info collection-info-class)
  (let ((entry (make-instance <sal-entry-info> 
                              :idx (sal-selection/idx lhs)
                              :info info)))
    (sal-lhs->assignment-info (sal-selection/target lhs)
                              (make-instance collection-info-class
                                             :children (list entry)))))
(define-method (sal-lhs->assignment-info (lhs <sal-tuple-selection>) (info <sal-assignment-info>))
  (sal-collection->assignment-info lhs info <sal-tuple-assignment-info>))
(define-method (sal-lhs->assignment-info (lhs <sal-record-selection>) (info <sal-assignment-info>))
  (sal-collection->assignment-info lhs info <sal-record-assignment-info>))
(define-method (sal-lhs->assignment-info (lhs <sal-array-selection>) (info <sal-assignment-info>))
  (sal-collection->assignment-info lhs info <sal-array-assignment-info>))

(define (sal-simple-definition->assignment-info simple-def)
  [instance-check sal-lhs->assignment-info simple-def <sal-simple-definition>]
  (let* ((lhs (slot-value simple-def :lhs))
         (info (make-instance <sal-def-assignment-info-with-rhs> :rhs (slot-value simple-def :rhs))))
    (sal-lhs->assignment-info lhs info)))


(define-generic (sal-assignment-info->implicit-value info pos))
(define-method (sal-assignment-info->implicit-value (info <sal-undef-assignment-info>) (pos <sal-expr>))
  pos)
(define-method (sal-assignment-info->implicit-value (info <sal-def-assignment-info>) (pos <sal-expr>))
  ;; do nothing
  #f)
(define-method (sal-assignment-info->implicit-value (info <sal-def-assignment-info-with-rhs>) (pos <sal-expr>))
  (slot-value info :rhs))
(define (collection-implicit-value info pos mk-selection mk-update)
  (let loop ((children (slot-value info :children))
             (target pos))
    (if (null? children)
      target
      (let* ((curr-entry (car children))
             (new-pos (mk-selection pos (slot-value curr-entry :idx)))
             (curr (sal-assignment-info->implicit-value (slot-value curr-entry :info) new-pos)))
        (loop (cdr children)
              (mk-update target (slot-value curr-entry :idx) curr))))))

(define (mk-simple-selection-proc class-name)
  (lambda (target idx)
    (make-ast-instance class-name target
                       :target target
                       :idx idx)))

(define (mk-update-proc class-name)
  (lambda (target idx new-value)
    (make-ast-instance class-name target
                       :target target
                       :idx idx
                       :new-value new-value)))

(define-method (sal-assignment-info->implicit-value (info <sal-tuple-assignment-info>) (pos <sal-expr>))
  (collection-implicit-value info pos (mk-simple-selection-proc <sal-tuple-selection>) (mk-update-proc <sal-tuple-update>)))
(define-method (sal-assignment-info->implicit-value (info <sal-record-assignment-info>) (pos <sal-expr>))
  (collection-implicit-value info pos (mk-simple-selection-proc <sal-record-selection>) (mk-update-proc <sal-record-update>)))
(define-method (sal-assignment-info->implicit-value (info <sal-array-assignment-info>) (pos <sal-expr>))
  (collection-implicit-value info pos 
                             (lambda (target idx)
                               (make-ast-instance <sal-array-selection> target
                                                  :fun target
                                                  :arg idx))
                             (mk-update-proc <sal-array-update>)))
  
(define (sal-assignment-info->implicit-assignment info name-expr)
  [instance-check sal-assignment-info->implicit-assignment info <sal-assignment-info>]
  (let ((value (sal-assignment-info->implicit-value info name-expr)))
    (and value
         (make-ast-instance <sal-simple-definition> name-expr
                            :lhs (make-ast-instance <sal-next-operator> name-expr
                                                    :name-expr name-expr)
                            :rhs value))))
                            
(define (make-sal-assignment-tracker)
  (make-symbol-table))

(define (sal-tracker/add-variable tracker var-name)
  (symbol-table/add tracker var-name *sal-undef-assignment-info*))

(define (state-variables->tracker state-variables)
  (fold-left
   (lambda (tracker var-decl)
     (if (instance-of? var-decl <sal-input-state-var-decl>)
       tracker
       (sal-tracker/add-variable tracker (sal-decl/name var-decl))))
   (make-sal-assignment-tracker)
   state-variables))

(define (simplify-simple-def simple-def)
  (update-ast-slots simple-def 
                    :lhs (sal-ast/simplify (slot-value simple-def :lhs))))

(define-generic (process-definition def tracker region))

(define-method (process-definition (def <sal-for-all-definition>) (tracker <primitive>) (region <primitive>))
  (sign-unsupported-feature def "FORALL definitions are not supported by the implicit assignments transformation."))

(define-method (process-definition (def <sal-simple-definition>) (tracker <primitive>) (region <primitive>))
  (trace 'implicit-assignments "processing simple definition ~a" (sal-ast->list def))
  (let* ((name-expr (sal-lhs/name-expr (slot-value def :lhs)))
         (var-name (sal-name-ref/name name-expr))
         (curr-info (symbol-table/lookup tracker var-name)))
    (with-output-to-trace 'implicit-assignments
                          (trace 'implicit-assignments "current assignment info:")
                          (sal/pp curr-info)
                          (print "")
                          (trace 'implicit-assignments "------------------------"))
    (cond
     ((sal-simple-definition/trivial? def)
      (trace 'implicit-assignments "trivial simple definition")
      (if (instance-of? curr-info <sal-undef-assignment-info>)
        ;; TODO: investigate this assignment...
        (values (symbol-table/add tracker var-name *sal-def-assignment-info*) def) ;; (values (symbol-table/add tracker var-name (sal-simple-definition->assignment-info def)) def)
        (throw-invalid-simple-def def)))
     (else 
      (when (eq? region 'definition)
        (sign-unsupported-feature def "In the DEFINITION section, the left-hand-side of simple definitions must contain only a variable, that is, array/tuple/record-selections are not allowed."))
      (let* ((simple-def (simplify-simple-def def))
             (new-simple-def (if (sal-simple-definition/supported? simple-def)
                               simple-def
                               (if (eq? region 'initialization)
                                 (sign-unsupported-feature def "Unsupported left-hand-side in the INITIALIZATION section. Please rewrite the simple definition.")
                                 (sal-simple-definition/convert-to-supported simple-def))))
             (_ (trace 'implicit-assignments "simple definition after conversion: ~a" (sal-ast->list new-simple-def)))
             (def-info (sal-simple-definition->assignment-info new-simple-def))
             (result-info (sal-assignment-info/union curr-info def-info))
             (new-tracker (symbol-table/add tracker var-name result-info)))
        (with-output-to-trace 'implicit-assignments
                              (trace 'implicit-assignments "new assignment info:")
                              (sal/pp def-info)
                              (print "")
                              (trace 'implicit-assignments "------------------------")
                              (trace 'implicit-assignments "union assignment info:")
                              (sal/pp result-info)
                              (print "")
                              (trace 'implicit-assignments "------------------------"))
        (values new-tracker #f))))))

(define (add-implicit-assignments tracker def-queue state-vars)
  (symbol-table/for-each (lambda (var-name info)
                           ;; Remark: a variable cannot be partially defined in a DEFDECL section
                           (let* ((var-decl (sal-decl-list/fast-lookup state-vars var-name))
                                  (name-expr (make-sal-name-expr var-decl))
                                  (new-assignment (sal-assignment-info->implicit-assignment info name-expr)))
                             (when new-assignment
                               (queue/insert! def-queue new-assignment))
                             (set! tracker (symbol-table/add tracker var-name *sal-def-assignment-info*))))
                         tracker))

(define (process-definitions definition-list tracker state-vars)
  (for-each (lambda (definition)
              (multiple-value-bind
                  (new-tracker new-definition)
                  (process-definition definition tracker 'definition)
                [assert (new-definition) new-definition]
                [assert (definition new-definition) (eq? definition new-definition)]
                (set! tracker new-tracker)))
            definition-list)
  tracker)

(define (process-assignments assignments tracker state-vars)
  (let ((new-simple-defs (make-queue))
        (cmd-tracker tracker))
    (for-each (lambda (simple-def)
                (multiple-value-bind
                    (new-tracker new-simple-def)
                    (process-definition simple-def cmd-tracker 'transition)
                  (when new-simple-def
                    (queue/insert! new-simple-defs new-simple-def))
                  (set! cmd-tracker new-tracker)))
              assignments)
    (with-output-to-trace 'implicit-assignments
                          (trace 'implicit-assignments "tracker:")
                          (sal/pp cmd-tracker)
                          (print "")
                          (trace 'implicit-assignments "----------------------"))
    (add-implicit-assignments cmd-tracker new-simple-defs state-vars)
    (queue->list new-simple-defs)))

(define-generic (process-command cmd tracker state-vars))
(define-method (process-command (cmd <sal-guarded-command>) (tracker <primitive>) (state-vars <primitive>))
  (copy-ast cmd :assignments (process-assignments (slot-value cmd :assignments) tracker state-vars)))
(define-method (process-command (cmd <sal-else-command>) (tracker <primitive>) (state-vars <primitive>))
  (copy-ast cmd :assignments (process-assignments (slot-value cmd :assignments) tracker state-vars)))
(define-method (process-command (cmd <sal-labeled-command>) (tracker <primitive>) (state-vars <primitive>))
  (copy-ast cmd :command (process-command (slot-value cmd :command) tracker state-vars)))
(define-method (process-command (cmd <sal-multi-command>) (tracker <primitive>) (state-vars <primitive>))
  (copy-ast cmd :command (process-command (slot-value cmd :command) tracker state-vars)))

(define (process-command-section cmd-section tracker state-vars)
  [instance-check process-command-section cmd-section <sal-command-section>]
  (trace 'implicit-assignments "processing command section ~a" (sal-ast->list cmd-section))
  (copy-ast cmd-section
            :commands (map (cut process-command <> tracker state-vars) (slot-value cmd-section :commands))
            :else-command (let ((else-cmd (slot-value cmd-section :else-command)))
                            (if else-cmd
                              (process-command else-cmd tracker state-vars)
                              #f))))

(define (process-transition definition-list command-section tracker state-vars)
  (let ((result-definition-queue (make-queue)))
    (for-each (lambda (definition)
                (multiple-value-bind
                    (new-tracker new-definition)
                    (process-definition definition tracker 'transition)
                  (when new-definition
                    (queue/insert! result-definition-queue new-definition))
                  (set! tracker new-tracker)))
              definition-list)
    (if command-section
      (values (queue->list result-definition-queue)
              (process-command-section command-section tracker state-vars))
      (begin
        (add-implicit-assignments tracker result-definition-queue state-vars)
        (values (queue->list result-definition-queue)
                #f)))))

(define (sal-base-module/insert-implicit-assignments base-module)
  [instance-check sal-base-module/insert-implicit-assignments base-module <sal-base-module>]
  (verbose-message 2 "calculating implicit assignments of base module at ~a..." (format-with-location base-module ""))
  (let* ((state-vars (sal-module/state-variables base-module))
         (tracker (state-variables->tracker state-vars))
         (tracker-after-definitions (process-definitions (slot-value base-module :definitions) tracker state-vars)))
    (multiple-value-bind
        (new-transition-definitions new-transition-command-section)
        (process-transition (slot-value base-module :transition-definitions) 
                            (slot-value base-module :transition-command-section) 
                            tracker-after-definitions 
                            state-vars)
      (copy-ast base-module
                :transition-definitions new-transition-definitions
                :transition-command-section new-transition-command-section))))

