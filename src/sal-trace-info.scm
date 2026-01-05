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

(module sal-trace-info
        (include "sal.sch")
        (import unique-names sal-expression front-end sal-type sal-decls)
        (export (sal/set-trace-info-enabled! flag)
                (sal/trace-info-enabled?)
                (make-sal-choice-var-decl choice-desired-type synch-decl-list place-provider)
                (make-sal-command-section-choice-var-decl cmd-sec synch-decl-list) 
                (make-sal-choice-trace-info cmd-sec choice-var-decl commands-trace-info-list else-trace-info)
                (make-sal-choice-test choice-var-decl synch-decl-list rhs-value place-provider)
                (make-sal-choice-esm-assignment choice-var-decl synch-decl-list rhs-value place-provider)
                (make-sal-asynch-trace-info children-flat-modules synch-decl-list place-provider)
                (make-sal-multi-sequence-trace-info local-decl child-trace-info place-provider)
                (make-sal-multi-choice-trace-info choice-var-decl local-decl child-trace-info place-provider)
                (make-sal-transition-trace-info place-provider)
                (make-sal-labeled-trace-info labeled-command nested-trace-info)
                (make-sal-multi-command-choice-trace-info multi-command synch-decl-list nested-trace-info)
                (sal-else-command/trace-info command)
                (sal/set-seq-trace-info-based-on-children! parent-module child-module-1 child-module-2 place-provider)
                (sal/set-module-instance-trace-info! module place-provider)
                (sal-command-section/num-alternatives cmd-sec)
                (sal-trace-info/find-choice-var info)
                (sal-trace-info/contains-choice? info))
        )

;;--------------------------------------------------------------------
;;
;; Options/Flags
;;
;;--------------------------------------------------------------------

(define *sal-trace-info-enabled?* #t)

(define-api (sal/set-trace-info-enabled! flag)
  :doc "Turn on/off traceability. Traceability information is extra information consists of extra information inserted in SAL modules that supports the generation of detailed counterexamples."
  (set! *sal-trace-info-enabled?* flag))

(define-api (sal/trace-info-enabled?)
  :doc "Return #t (#f) when traceability is enabled (disabled). See also sal/set-trace-info-enabled!."
  *sal-trace-info-enabled?*)

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Traceability"
                                      :long-opt-name "--disable-traceability"
                                      :description "SALenv creates extra variables to produce detailed path (and counterexamples). This option diables the generation of such variables. Remark: SALenv model checkers will probably execute faster, but counterexamples will contain just the value of the module variables."
                                      :proc (lambda () (set! *sal-trace-info-enabled?* #f))))

;;--------------------------------------------------------------------
;;
;; Auxiliary trace info production functions
;;
;;--------------------------------------------------------------------

;; decl-list is a list of variable declarations of multi-synchronous modules
(define (make-sal-choice-type choice-desired-type synch-decl-list place-provider)
  (fold-left (lambda (result-type curr-decl)
               (let ((array-domain-type (slot-value curr-decl :type)))
                 (make-ast-instance <sal-array-type> place-provider
                                    :domain array-domain-type
                                    :range result-type)))
             choice-desired-type
             synch-decl-list))

;; decl-list is a list of variable declarations of multi-synchronous modules
(define (make-sal-choice-var-decl choice-desired-type synch-decl-list place-provider)
  (if (sal/trace-info-enabled?)
    (let* ((choice-var-name (gen-unique-name 'choice))
           (choice-var-id (make-sal-identifier place-provider choice-var-name))
           (choice-type (make-sal-choice-type choice-desired-type synch-decl-list place-provider)))
      (make-ast-instance <sal-choice-input-state-var-decl> place-provider
                         :id choice-var-id
                         :type choice-type))
    #f))

(define (make-sal-command-section-choice-var-decl cmd-sec synch-decl-list) 
  (if (sal/trace-info-enabled?)
    (let* ((num-alts (sal-command-section/num-alternatives cmd-sec))
           (place-provider cmd-sec)
           (choice-var-decl (if (> num-alts 1)
                              (let* ((lower (make-sal-numeral 0 place-provider))
                                     (upper (make-sal-numeral (- num-alts 1) place-provider))
                                     (choice-desired-type (make-sal-subrange lower upper place-provider)))
                                (make-sal-choice-var-decl choice-desired-type synch-decl-list place-provider))
                              #f)))
      choice-var-decl)
    #f))
  
(define (make-sal-choice-trace-info cmd-sec choice-var-decl commands-trace-info-list else-trace-info)
  (if (sal/trace-info-enabled?)
    (let* ((num-alts (sal-command-section/num-alternatives cmd-sec))
           (place-provider cmd-sec))
      (if (> num-alts 1)
        (make-ast-instance <sal-choice-trace-info> place-provider
                           :choice-var-name (sal-decl/name choice-var-decl)
                           :info-list (if else-trace-info
                                        (append commands-trace-info-list (list else-trace-info))
                                        commands-trace-info-list))
        (begin
          [assert (else-trace-info commands-trace-info-list) (or else-trace-info (= (length commands-trace-info-list) 1))]
          (or else-trace-info (car commands-trace-info-list)))))
    #f))

(define (make-sal-choice-assignment-core choice-var-decl synch-decl-list rhs-value place-provider mk-assignment-proc)
  (let ((lhs (fold-left (lambda (result-lhs curr-decl)
                          (let* ((name-expr (make-sal-name-expr curr-decl place-provider)))
                            (make-ast-instance <sal-array-selection> place-provider
                                               :fun result-lhs
                                               :arg name-expr)))
                        (make-sal-name-expr choice-var-decl place-provider)
                        (reverse synch-decl-list))))
    (mk-assignment-proc lhs rhs-value)))

(define (make-sal-choice-test choice-var-decl synch-decl-list rhs-value place-provider)
  (if (sal/trace-info-enabled?)
    (make-sal-choice-assignment-core choice-var-decl synch-decl-list rhs-value place-provider
                                     make-sal-equality)
    (make-sal-true place-provider)))

(define (make-sal-choice-esm-assignment choice-var-decl synch-decl-list rhs-value place-provider)
  (if (sal/trace-info-enabled?)
    (make-sal-choice-assignment-core choice-var-decl synch-decl-list rhs-value place-provider
                                     (lambda (lhs rhs-value)
                                       (make-ast-instance <sal-esm-choice-assignment> place-provider
                                                          :lhs lhs
                                                          :rhs rhs-value)))
    #f))

(define (make-sal-asynch-trace-info children-flat-modules synch-decl-list place-provider)
  (if (sal/trace-info-enabled?)
    (let* ((num-flat-modules (length children-flat-modules))
           (trace-info-list (map (cut slot-value <> :transition-trace-info) children-flat-modules))
           (choice-vars (fold-left (lambda (choice-vars flat-module)
                                     (append choice-vars (slot-value flat-module :choice-vars)))
                                   '()
                                   children-flat-modules))
           (choice-desired-type (make-sal-subrange (make-sal-numeral 0 place-provider) 
                                                   (make-sal-numeral (- num-flat-modules 1) place-provider)
                                                   place-provider))
           (new-choice-var-decl (make-sal-choice-var-decl choice-desired-type synch-decl-list place-provider))
           (new-transition-trace-info (make-ast-instance <sal-choice-trace-info> place-provider
                                                         :choice-var-name (sal-decl/name new-choice-var-decl)
                                                         :info-list trace-info-list))
           (new-choice-vars (cons new-choice-var-decl choice-vars)))
      (values new-transition-trace-info new-choice-var-decl new-choice-vars))
    (values #f #f #f)))

(define (make-sal-multi-sequence-trace-info local-decl child-trace-info place-provider)
  (if (sal/trace-info-enabled?)
    (make-ast-instance <sal-multi-sequence-trace-info> place-provider
                       :idx-var-name (sal-decl/name local-decl)
                       :info child-trace-info)
    #f))

(define (make-sal-multi-choice-trace-info choice-var-decl local-decl child-trace-info place-provider)
  (if (sal/trace-info-enabled?)
    (make-ast-instance <sal-multi-choice-trace-info> place-provider
                       :choice-var-names (list (sal-decl/name choice-var-decl))
                       :original-var-names (list (sal-decl/name local-decl))
                       :info child-trace-info)
    #f))

(define (make-sal-transition-trace-info place-provider)
  (if (sal/trace-info-enabled?)
    (make-ast-instance <sal-transition-trace-info> place-provider)
    #f))

(define (make-sal-labeled-trace-info labeled-command nested-trace-info)
  (if (sal/trace-info-enabled?)
    (make-ast-instance <sal-labeled-trace-info> labeled-command
                       :label (slot-value labeled-command :label)
                       :info nested-trace-info)
    #f))

(define (make-sal-multi-command-choice-trace-info multi-command synch-decl-list nested-trace-info)
  (if (sal/trace-info-enabled?)
    (let* ((place-provider multi-command)
           (local-decls (slot-value multi-command :local-decls))
           (choice-decls (map (lambda (local-decl)
                                (make-sal-choice-var-decl (slot-value local-decl :type) synch-decl-list place-provider))
                              local-decls))
           (local-names (sal-decl-list->symbol-list local-decls))
           (choice-names (sal-decl-list->symbol-list choice-decls))
           (new-trace-info (make-ast-instance <sal-multi-command-choice-trace-info> place-provider
                                              :choice-var-names choice-names
                                              :original-var-names local-names
                                              :info nested-trace-info)))
      (values new-trace-info choice-decls))
    (values #f #f)))

(define (sal/set-seq-trace-info-based-on-children! parent-module child-module-1 child-module-2 place-provider)
  (when (sal/trace-info-enabled?)
    (let* ((transition-trace-info1 (slot-value child-module-1 :transition-trace-info))
           (transition-trace-info2 (slot-value child-module-2 :transition-trace-info))
           (choice-vars1 (slot-value child-module-1 :choice-vars))
           (choice-vars2 (slot-value child-module-2 :choice-vars))
           (new-transition-trace-info (make-ast-instance <sal-sequence-trace-info> place-provider
                                                         :info-list (list transition-trace-info1 
                                                                          transition-trace-info2)))
           (new-choice-vars (append choice-vars1 choice-vars2)))
      (set-slot-value! parent-module :transition-trace-info new-transition-trace-info)
      (set-slot-value! parent-module :choice-vars new-choice-vars))))

(define (sal/set-module-instance-trace-info! module place-provider)
  (when (sal/trace-info-enabled?)
    (set-slot-value! module
                     :transition-trace-info (make-ast-instance <sal-module-instance-trace-info> place-provider
                                                               :info (slot-value module :transition-trace-info)))))

(define-generic (sal-else-command/trace-info command))
(define-method (sal-else-command/trace-info (command <sal-labeled-command>))
  (if (sal/trace-info-enabled?)
    (make-ast-instance <sal-labeled-trace-info> command
                       :label (slot-value command :label)
                       :info (sal-else-command/trace-info (slot-value command :command)))
    #f))
(define-method (sal-else-command/trace-info (command <sal-else-command>))
  (if (sal/trace-info-enabled?)
    (make-ast-instance <sal-else-trace-info> command)
    #f))

(define (sal-command-section/num-alternatives cmd-sec)
  (+ (length (slot-value cmd-sec :commands)) (if (slot-value cmd-sec :else-command) 1 0)))

    

(define-generic (sal-trace-info/find-choice-var info))
(define-method (sal-trace-info/find-choice-var (info <sal-trace-info>)) #f)
(define-method (sal-trace-info/find-choice-var (info <sal-nested-trace-info>))
  (sal-trace-info/find-choice-var (slot-value info :info)))
(define-method (sal-trace-info/find-choice-var (info <sal-nested-list-trace-info>))
  (bind-exit (exit)
    (for-each (lambda (info)
                (cond
                 ((sal-trace-info/find-choice-var info) => exit)))
              (slot-value info :info-list))
    #f))
(define-method (sal-trace-info/find-choice-var (info <sal-multi-choice-trace-info>))
  (car (slot-value info :choice-var-names)))
(define-method (sal-trace-info/find-choice-var (info <sal-choice-trace-info>))
  (slot-value info :choice-var-name))

(define (sal-trace-info/contains-choice? info)
  (sal-trace-info/find-choice-var info))
