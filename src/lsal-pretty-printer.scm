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

(module lsal-pretty-printer
        (include "sal.sch")
        (import pretty sal-ast queue sal-ast-support sal-expression gmp-scheme sal-context symbol-table)
        (export (sal-ast->lsal-doc ast pp-info depth))
        )

(define-generic (sal-ast->lsal-doc ast pp-info depth))

(define-method (sal-ast->lsal-doc :around (ast <primitive>) (pp-info <pp-info++>) (depth <primitive>))
  (if (> depth (slot-value pp-info :max-depth))
    "..."
    (call-next-method)))

(define-macro (doc-node n)
  `(sal-ast->lsal-doc ,n pp-info (+ depth 1)))

(define-macro (doc-slot slot-id)
  `(doc-node (slot-value ast ,slot-id)))

(define-macro (doc-optional-slot slot-id)
  `(cond
    ((slot-value ast ,slot-id) =>
     (lambda (n) (doc-node n)))
    (else
     *doc-nil*)))

(define-macro (doc-list lst)
  `(map (lambda (n) (doc-node n)) ,lst))

(define-macro (doc-list-slot slot-id)
  `(doc-list (slot-value ast ,slot-id)))

(define-macro (pp-method class . body)
  `(define-method (sal-ast->lsal-doc (ast ,class) (pp-info <pp-info++>) (depth <primitive>))
     ,@body))

(define-macro (indent)
  `(slot-value pp-info :default-indent))

(define-macro (nl)
  `*doc-line*)

(define (pp-entry lhs rhs)
  (pp/concat lhs "::" (pp/nest* (+ (string-length (to-string lhs)) 2) rhs)))

;; (put 'pp-method 'scheme-indent-function 2)

(pp-method <primitive> (to-string (instance-class-name ast)))
(pp-method <sal-identifier> (slot-value ast :name))
(pp-method <sal-decl> (doc-slot :id))
(pp-method <sal-var-decl> 
    (pp-entry (doc-slot :id) (doc-slot :type)))
(pp-method <sal-const-decl>
    (let* ((id (doc-slot :id))
           (id-len (string-length (to-string id))))
      (pp/group (pp/concat (pp-entry id (doc-slot :type))
                           (pp/nest* id-len (nl) (doc-slot :value))))))
(pp-method <sal-constant-decl>
    (pp/list-style4 'define (indent) (pp-entry (doc-slot :id) (doc-slot :type)) (doc-optional-slot :value)))
(pp-method <sal-type-decl>
    (pp/list-style4 'define-type (indent) (doc-slot :id) (doc-optional-slot :type)))
(pp-method <sal-module-decl>
    (let* ((local-decls (slot-value (slot-value ast :parametric-module) :local-decls))
           (header (if (null? local-decls)
                     (doc-slot :id)
                     (apply pp/list-style1 (doc-slot :id) (doc-list local-decls)))))
      (pp/list-style4 'define-module (indent) header (doc-slot :parametric-module))))
(pp-method <sal-parametric-module>
    (doc-slot :module))
(pp-method <sal-context>
    (apply pp/list-style3 'context (indent) 2 (doc-slot :id)
           (apply pp/list-style6 (doc-list-slot :params))
           (doc-list (queue->list (slot-value ast :declarations)))))
(pp-method <sal-name-ref>
    (sal-name-ref/name ast))



;;
;; BD: bug fixes
;; 1) don't see why we need (string-downcase ... )
;; 2) (pp/list-style1 ctx-name actuals) is wrong:
;;    actuals is an ast list, not a doc list
;;
(pp-method <sal-qualified-name-ref>
    (cond
     ((or (slot-value pp-info :simplify-qualified-names?)
          (sal-name-ref/builtin? ast))
;;       (string-downcase (to-string (sal-name-ref/name ast))))
      (to-string (sal-name-ref/name ast)))
     (else
      (let* ((context-ref (sal-qualified-name-ref/context-ref ast))
             (actuals (sal-qualified-name-ref/actuals ast))
             (ctx-name (sal-context/name context-ref)))
        (if (null? actuals)
          (pp/list-style1 "@" (sal-name-ref/name ast) ctx-name)
;;          (pp/list-style1 "@" (sal-name-ref/name ast) (pp/list-style1 ctx-name actuals)))))))
          (pp/list-style1 "@" (sal-name-ref/name ast) (pp/list-style1 ctx-name (doc-list actuals))))))))

(pp-method <sal-function-type>
    (pp/list-style1 '-> (doc-slot :domain) (doc-slot :range)))
(pp-method <sal-array-type>
    (pp/list-style1 'array (doc-slot :domain) (doc-slot :range)))
(pp-method <sal-domain-tuple-type>
    (apply pp/list-style6-core "" "" (doc-list-slot :types)))
(pp-method <sal-tuple-type>
    (apply pp/list-style1 'tuple (doc-list-slot :types)))
(pp-method <sal-record-type>
    (apply pp/list-style1 'record (doc-list-slot :fields)))
(pp-method <sal-field>
    (pp-entry (doc-slot :id) (doc-slot :type)))
(pp-method <sal-subtype>
    (pp/list-style1 'subtype (doc-slot :expr)))
(pp-method <sal-scalar-type>
    (apply pp/list-style1 'scalar (doc-list-slot :scalar-elements)))
(pp-method <sal-data-type>
    (apply pp/list-style1 'datatype (doc-list-slot :constructors)))
(pp-method <sal-subrange>
    (pp/list-style1 'subrange (doc-slot :lower) (doc-slot :upper)))
(pp-method <sal-state-type>
    (pp/list-style1 'state (doc-slot :module)))
(pp-method <sal-scalar-set-type>
    (pp/list-style1 'scalar-set (doc-slot :expr)))
(pp-method <sal-ring-set-type>
    (pp/list-style1 'ring-set (doc-slot :expr)))
(pp-method <sal-in>
    (pp/list-style1 'in (doc-slot :arg) (doc-slot :fun)))
(pp-method <sal-definition-expression>
    (doc-slot :expr))      
(pp-method <sal-numeral>
    (mpq->string (slot-value ast :num)))
(pp-method <sal-application>
    (let ((id (doc-node (slot-value ast :fun)))
          (args (doc-list (sal-application/argument-list ast))))
      (if (symbol? id)
        (apply pp/list-style1 id args)
        (apply pp/list-style2 id (indent) args))))
(define-macro (pp-multi-bind tag decl-slot-id expr-slot-id)
  `(pp/list-style4 ,tag (indent) 
                   (apply pp/list-style6 (doc-list-slot ,decl-slot-id))
                   (doc-slot ,expr-slot-id)))
(pp-method <sal-lambda>
    (pp-multi-bind 'lambda :local-decls :expr))

(define (pp-alternatives ast pp-info depth)
  (let* ((else-expr (slot-value ast :else-expr))
         (else-doc (if (instance-of? else-expr <sal-conditional>)
                     (pp-alternatives else-expr pp-info depth)
                     (list (pp/list-style5 (indent) 'else (doc-node else-expr))))))
    (cons (pp/list-style5 (indent) (doc-slot :cond-expr) (doc-slot :then-expr))
          else-doc)))
    
(pp-method <sal-conditional>
    (if (instance-of? (slot-value ast :else-expr) <sal-conditional>)
      (apply pp/list-style5 (indent) 'cond (pp-alternatives ast pp-info depth))
      (pp/list-style4 'if (indent)
                      (doc-slot :cond-expr)
                      (doc-slot :then-expr)
                      (doc-slot :else-expr))))
        
(pp-method <sal-let-expr>
    (pp-multi-bind 'let :local-decls :expr))
(pp-method <sal-let-decl>
    (pp/list-style6 (call-next-method)))         
(pp-method <sal-array-literal>
    (pp-multi-bind 'mk-array :local-decls :expr))
(pp-method <sal-tuple-literal>
    (apply pp/list-style1 'mk-tuple (doc-list-slot :exprs)))
(pp-method <sal-record-literal>
    (apply pp/list-style1 'mk-record (doc-list-slot :entries)))
(pp-method <sal-record-entry>
    (pp-entry (doc-slot :id) (doc-slot :expr)))
(pp-method <sal-set-pred-expr>
    (pp-multi-bind 'set-pred :local-decls :expr))
(pp-method <sal-for-all-expr>
    (pp-multi-bind 'for-all :local-decls :expr))
(pp-method <sal-exists-expr>
    (pp-multi-bind 'exists :local-decls :expr))
(pp-method <sal-simple-selection>
    (pp/concat (doc-slot :target) "." (doc-slot :idx)))
(pp-method <sal-array-selection>
    (let ((array (doc-slot :fun)))
      (pp/concat array (pp/nest* (indent) "[" (doc-slot :arg) "]"))))
(pp-method <sal-update-expr>
    (pp/list-style1 'update (doc-slot :target) (doc-slot :idx) (doc-slot :new-value)))
(pp-method <sal-tuple-update>
    (pp/list-style1 'update (doc-slot :target) (pp/concat "." (doc-slot :idx)) (doc-slot :new-value)))
(pp-method <sal-record-update>
    (pp/list-style1 'update (doc-slot :target) (pp/concat "." (doc-slot :idx)) (doc-slot :new-value)))
(pp-method <sal-array-update>
    (pp/list-style1 'update (doc-slot :target) (pp/concat "[" (doc-slot :idx) "]") (doc-slot :new-value)))
(pp-method <sal-next-operator>
    (pp/concat (doc-slot :name-expr) "'"))
(pp-method <sal-pre-operator>
    (let loop ((i 1)
               (expr (slot-value ast :expr)))
      (cond
       ((instance-of? expr <sal-pre-operator>)
        (loop (+ i 1) (slot-value expr :expr)))
       ((= i 1)
        (pp/concat "(pre " (doc-node expr) ")"))
       (else
        (pp/concat "(pre^" (object->string i) " " (doc-node expr) ")")))))
(pp-method <sal-string-expr>
    (pp/concat "\"" (slot-value ast :string) "\""))
(pp-method <sal-mod-init>
    (pp/list-style1 'init (doc-slot :module)))
(pp-method <sal-mod-trans>
    (pp/list-style1 'trans (doc-slot :module)))
(pp-method <sal-simple-definition>
    (pp/list-style1 '= (doc-slot :lhs) (doc-slot :rhs)))
(pp-method <sal-simple-selection-definition>
    (pp/list-style1 'in (doc-slot :lhs) (doc-slot :rhs)))
(pp-method <sal-for-all-definition>
    (apply pp/list-style4 'for-all (indent) 
           (apply pp/list-style6 (doc-list-slot :local-decls))
           (doc-list-slot :definitions)))
(pp-method <sal-command-section>
    (apply pp/list-style1 "[]"
           (append (doc-list-slot :commands)
                   (list (doc-optional-slot :else-command)))))
(pp-method <sal-guarded-command>
    (apply pp/list-style6 
           (doc-slot :guard)
           '-->
           (doc-list-slot :assignments)))
(pp-method <sal-labeled-command>
    (pp/list-style4 'label (indent) (doc-slot :label) (doc-slot :command)))
(pp-method <sal-multi-command>
    (pp-multi-bind "[]" :local-decls :command))
(pp-method <sal-else-command>
    (apply pp/list-style6 
           'else
           '-->
           (doc-list-slot :assignments)))
(pp-method <sal-asynch-composition>
    (pp/list-style1 "[]" (doc-slot :module1) (doc-slot :module2)))
(pp-method <sal-synch-composition>
    (pp/list-style1 "||" (doc-slot :module1) (doc-slot :module2)))
(pp-method <sal-observer>
    (pp/list-style1 'observer (doc-slot :module1) (doc-slot :module2)))
(pp-method <sal-multi-asynch-composition>
    (pp/list-style4 "[]" (indent)
                    (pp/concat "(" (doc-node (car (slot-value ast :local-decls))) ")")
                    (doc-slot :module)))
(pp-method <sal-multi-synch-composition>
    (pp/list-style4 "||" (indent)
                    (pp/concat "(" (doc-node (car (slot-value ast :local-decls))) ")")
                    (doc-slot :module)))
(pp-method <sal-renaming>
    (pp-multi-bind 'rename :renames :module))
(pp-method <sal-rename>
    (pp/list-style6 (doc-slot :from-name) (doc-slot :to-expr)))
(pp-method <sal-hiding>
    (pp/list-style4 'hide (indent) 
                    (apply pp/list-style6 (doc-list-slot :identifiers))
                    (doc-slot :module)))
(pp-method <sal-new-output>
    (pp/list-style4 'new-output (indent)
                    (apply pp/list-style6 (doc-list-slot :identifiers))
                    (doc-slot :module)))
(pp-method <sal-with-module>
    (pp/list-style4 'with (indent)
                    (apply pp/list-style6 (doc-list-slot :new-state-vars))
                    (doc-slot :module)))
(pp-method <sal-module-instance>
    (if (null? (slot-value ast :actuals))
      (doc-slot :module-name)
      (apply pp/list-style5 (indent) (doc-slot :module-name) (doc-list-slot :actuals))))
(define-macro (cmd-sec tag def-slot-id cmd-slot-id)
  `(if (or (not (null? (slot-value ast ,def-slot-id)))
           (slot-value ast ,cmd-slot-id))
     (apply pp/list-style2 ,tag (indent) 
            (append (doc-list-slot ,def-slot-id)
                    (if (slot-value ast ,cmd-slot-id)
                      (list (doc-slot ,cmd-slot-id))
                      '())))
     *doc-nil*))
(pp-method <sal-base-module>
    (apply pp/list-style2 'begin (indent) 
           (append 
            (doc-list-slot :state-vars)
            (list
             (if (not (null? (slot-value ast :definitions)))
               (apply pp/list-style2 'definition (indent) (doc-list-slot :definitions))
               *doc-nil*)
             (cmd-sec 'initialization :initialization-definitions :initialization-command-section)
             (cmd-sec 'transition :transition-definitions :transition-command-section)))))

(pp-method <sal-flat-module>
           (apply pp/list-style2 'flat-module (indent)
                  (append
                   (doc-list-slot :state-vars)
                   (list
                    (pp/list-style2 'definition (indent) (doc-slot :definition))
                    (pp/list-style2 'initialization (indent) (doc-slot :initialization))
                    (pp/list-style2 'transition (indent) (doc-slot :transition))
                    (pp/list-style2 'skip (indent) (doc-slot :skip))
                    (pp/list-style2 'valid-input (indent) (doc-slot :valid-input-expr))
                    (pp/list-style2 'valid-state (indent) (doc-slot :valid-state-expr))
                    (pp/list-style2 'valid-constant (indent) (doc-slot :valid-constant-expr))
                    ))))

(pp-method <sal-local-state-var-decl>
    (pp/list-style1 'local (call-next-method)))
(pp-method <sal-input-state-var-decl>
    (pp/list-style1 'input (call-next-method)))
(pp-method <sal-global-state-var-decl>
    (pp/list-style1 'global (call-next-method)))
(pp-method <sal-output-state-var-decl>
    (pp/list-style1 'output (call-next-method)))
(pp-method <sal-assertion-decl>
    (pp/list-style4 (slot-value ast :kind) (indent) (doc-slot :id)
                    (doc-slot :assertion-expr)))
(pp-method <sal-module-models>
    (pp/list-style1 "|-" (doc-slot :module) (doc-slot :expr)))
(pp-method <sal-module-implements>
    (pp/list-style1 'implements (doc-slot :module1) (doc-slot :module2)))
(pp-method <sal-assertion-proposition>
    (pp/list-style1 (slot-value ast :op)
                    (doc-list-slot :assertion-exprs)))

(pp-method <sal-base-component-info>
    (pp/list-style1 'component
                    (apply pp/list-style1 'input (doc-list-slot :input-data))
                    (apply pp/list-style1 'output (doc-list-slot :output-data))
                    (apply pp/list-style1 'owned (doc-list-slot :owned-data))))

(pp-method <sal-composite-component-info>
    (apply pp/list-style1 'composite (doc-list-slot :components)))

(pp-method <sal-multi-component-info>
    (pp/list-style1 'multi 
                    (pp/concat "(" (doc-node (car (slot-value ast :local-decls))) ")")
                    (doc-slot :component)))

(pp-method <sal-esm-module>
           (apply pp/list-style2 'esm-module  (indent)
                  (append
                   (doc-list-slot :state-vars)
                   (list
                    (if (slot-value ast :definition) 
                      (pp/list-style2 'definition (indent) (doc-slot :definition))
                      *doc-nil*)
                    (if (slot-value ast :initialization)
                      (pp/list-style2 'initialization (indent) (doc-slot :initialization))
                      *doc-nil*)
                    (if (slot-value ast :transition)
                      (pp/list-style2 'transition (indent) (doc-slot :transition))
                      *doc-nil*)))))

(pp-method <sal-esm-choice>
  (apply pp/list-style1 "choice" (doc-list-slot :statements)))

(pp-method <sal-esm-case>
  (apply pp/list-style4 "case" (indent) (doc-slot :expr) (doc-list-slot :case-entries)))

(pp-method <sal-esm-case-entry>
  (pp/list-style6 (doc-slot :value) (doc-slot :statement)))        

(pp-method <sal-esm-when-undefined>
  (pp/list-style4 "when-undefined" (indent) (doc-slot :lhs) (doc-slot :statement)))

(pp-method <sal-esm-seq>
  (apply pp/list-style1 "seq" (doc-list-slot :statements)))

(pp-method <sal-esm-multi-seq>
  (pp-multi-bind 'for-each :local-decls :statement))

(pp-method <sal-esm-multi-choice>
  (pp-multi-bind 'choice :local-decls :statement))

(pp-method <sal-esm-guard>
  (pp/list-style1 'guard (doc-slot :expr)))

(pp-method <sal-esm-assignment>
  (pp/list-style1 ":=" (doc-slot :lhs) (doc-slot :rhs)))

(pp-method <sal-esm-choice-assignment>
  (pp/list-style1 "=:=" (doc-slot :lhs) (doc-slot :rhs)))



