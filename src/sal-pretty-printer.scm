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

(module sal-pretty-printer
        (include "sal.sch")
        (import pretty sal-ast queue sal-ast-support sal-expression gmp-scheme sal-context 
                symbol-table sal-environment sal-pp)
        (export (sal-ast->sal-doc ast pp-info depth)
                (doc-list-with-sep doc-lst separator . group?)
                (doc-list-with-sep+ op cp doc-lst separator)
                )
        )

(define-generic (sal-ast->sal-doc ast pp-info depth))

(define-macro (doc-node n)
  `(sal-ast->sal-doc ,n pp-info (+ depth 1)))

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
  `(define-method (sal-ast->sal-doc (ast ,class) (pp-info <pp-info++>) (depth <primitive>))
     ,@body))

(define-macro (indent)
  `(slot-value pp-info :default-indent))

(define-macro (nl)
  `*doc-line*)

(define (doc-list-with-sep doc-lst separator . group?)
  (let ((group? (optional-arg group? #t)))
    (cond
     ((null? doc-lst)
      *doc-nil*)
     ((null? (cdr doc-lst))
      (car doc-lst))
     (else
      (let ((body (pp/concat (car doc-lst) (apply pp/concat (map (lambda (doc) (pp/concat separator *doc-line* doc)) (cdr doc-lst))))))
        (if group?
          (pp/group body)
          body))))))

(define (doc-list-with-sep+ op cp doc-lst separator)
  (pp/concat op (pp/nest (string-length op) (doc-list-with-sep doc-lst separator)) cp))

(define (doc-list-with-terminator doc-lst terminator)
  (cond
   ((null? doc-lst)
    *doc-nil*)
   (else
    (apply pp/concat (map (lambda (doc) (pp/concat *doc-line* doc terminator)) doc-lst)))))

(define (doc-simple-app tag list)
  (pp/concat tag (pp/nest (symbol-length tag)
                          (doc-list-with-sep+ "(" ")" list ","))))
(define (doc-app fun list)
  (pp/concat "(" fun ")" (doc-list-with-sep+ "(" ")" list ",")))

(define-macro (pp-binary-core tag doc1 doc2)
  `(pp/group (pp/concat ,doc1
                        (nl) ,tag " " (pp/nest (+ (string-length ,tag) 1) ,doc2))))

(define-macro (pp-binary tag slot1-id slot2-id)
  `(pp-binary-core ,tag (doc-slot ,slot1-id) (doc-slot ,slot2-id)))

(pp-method <primitive> (to-string (instance-class-name ast)))
(pp-method <sal-identifier> (slot-value ast :name))
(pp-method <sal-decl> (doc-slot :id))
(pp-method <sal-var-decl> 
    (pp-binary ":" :id :type))
(pp-method <sal-const-decl>
    (let* ((id (doc-slot :id))
           (id-len (string-length (to-string id))))
      (pp-binary-core ":" (doc-slot :id) (pp-binary "=" :type :value))))
(pp-method <sal-constant-decl>
    (let* ((id (doc-slot :id))
           (id-len (symbol-length id)))
      (pp/group (pp/concat id ": " (pp/nest (+ id-len 2)  (doc-slot :type))
                           (if (slot-value ast :value)
                             (pp/nest* (indent)  " =" (nl) (doc-slot :value))
                             *doc-nil*)))))
(pp-method <sal-type-decl>
    (pp/group (pp/concat (doc-slot :id) ": TYPE"
                         (if (slot-value ast :type)
                           (pp/nest* (indent) " =" (nl) (doc-slot :type))
                           *doc-nil*))))
(pp-method <sal-module-decl>
    (let ((local-decls (slot-value (slot-value ast :parametric-module) :local-decls)))
      (pp/group (pp/concat (doc-slot :id)
                           (if (null? local-decls)
                             *doc-nil*
                             (pp/nest* (indent) (pp/line "") (doc-list-with-sep+ "[" "]" (doc-list local-decls) ",")))
                           ": MODULE =" 
                           (pp/nest* (indent) (nl) (doc-slot :parametric-module))))))
(pp-method <sal-parametric-module>
    (doc-slot :module))
(pp-method <sal-context>
    (pp/concat (doc-slot :id) 
               (if (null? (slot-value ast :params))
                 *doc-nil*
                 (pp/group (pp/concat "{" (doc-list-with-sep (doc-list-slot :params) ",")
                                      "}")))
               ": CONTEXT =" (nl) 
               "BEGIN" 
               (pp/nest* (indent) (doc-list-with-terminator (doc-list (filter (lambda (decl)
                                                                                (not (instance-of? decl <sal-implicit-decl>)))
                                                                              (queue->list (slot-value ast :declarations)))) ";"))
               (nl)
               "END"))
(pp-method <sal-name-ref>
    (sal-name-ref/name ast))
(pp-method <sal-qualified-name-ref>
    (cond
     ((or (slot-value pp-info :simplify-qualified-names?)
          (sal-name-ref/builtin? ast))
      (sal-name-ref/name ast))
     (else
      (let* ((context-ref (sal-qualified-name-ref/context-ref ast))
             (actuals (sal-qualified-name-ref/actuals ast))
             (ctx-name (sal-context/name context-ref))
             (var-name (sal-name-ref/name ast)))
        (if (null? actuals)
          (pp/concat ctx-name "!" var-name)
          (let ((ctx-name-len (symbol-length ctx-name)))
            (pp/group (pp/concat ctx-name 
                                 (pp/nest* ctx-name-len 
                                           (pp/group (pp/concat "{" 
                                                                (doc-list-with-sep (doc-list actuals) ",")
                                                                "}"))
                                           (pp/line "") "!" var-name)))))))))

(pp-method <sal-qualified-name-expr>
    (cond 
      ((and (sal-name-ref/builtin? ast)
            (sal-name-ref/decl-attribute ast :sal-syntax)) 
       =>
       identity)
      (else
       (call-next-method))))
(pp-method <sal-function-type>
    (pp/group (pp/nest 1 (pp/concat "[" (doc-slot :domain) (nl) "-> " (doc-slot :range) "]"))))
(pp-method <sal-array-type>
    (pp/group (pp/concat "ARRAY " (pp/nest 6 (doc-slot :domain))
                         (nl) "OF " (pp/nest 3 (doc-slot :range)))))
;; (pp-method <sal-domain-tuple-type>
;;    (doc-list-with-sep (doc-list-slot :types) ",")) 
(pp-method <sal-tuple-type>
    (doc-list-with-sep+ "[" "]" (doc-list-slot :types) ","))
(pp-method <sal-record-type>
    (doc-list-with-sep+ "[# " " #]" (doc-list-slot :fields) ","))
(pp-method <sal-field>
    (pp/concat (doc-slot :id) ": " (pp/nest (+ (string-length (object->string (sal-identifier/name (slot-value ast :id)))) 2)
                                            (doc-slot :type))))
;; (pp-binary ":" :id :type)) ;; pp/concat (doc-slot :id) ": " (doc-slot :type)))
(pp-method <sal-subtype>
    (let* ((tag "SUBTYPE OF ")
           (len (string-length tag)))
      (pp/concat tag (pp/nest* len (doc-slot :expr)))))
(pp-method <sal-scalar-type>
    (doc-list-with-sep+ "{" "}" (doc-list-slot :scalar-elements) ","))
(pp-method <sal-data-type>
    (let* ((tag "DATATYPE ")
           (len (string-length tag)))
      (pp/concat tag (pp/nest* len (doc-list-with-sep (doc-list-slot :constructors) ",")))))
(pp-method <sal-subrange>
    (pp/concat "[" (doc-slot :lower) ".." (doc-slot :upper) "]"))
(pp-method <sal-state-type>
    (doc-simple-app 'STATE (list (doc-slot :module))))       
(pp-method <sal-scalar-set-type>
    (doc-simple-app 'SCALARSET (list (doc-slot :expr))))       
(pp-method <sal-ring-set-type>
    (doc-simple-app 'RINGSET (list (doc-slot :expr))))       

(pp-method <sal-in>
    (pp-binary "IN" :arg :fun))
(pp-method <sal-definition-expression>
    (doc-slot :expr))      
(pp-method <sal-numeral>
    (mpq->string (slot-value ast :num)))
           
(define (pp-application app pp-info depth)
  (let ((fun (slot-value app :fun))
        (arg-list (sal-application/argument-list app)))
    (cond
     ((sal-application/infix? app)
      (let* ((precedence (sal-application/precedence app))
             (associativity (sal-application/associativity app))
             (should-insert-parens? (lambda (left-child? child)
                                      (and (instance-of? child <sal-application>)
                                           (sal-application/infix? child)
                                           (or (> precedence (sal-application/precedence child))
                                               (and (= precedence (sal-application/precedence child))
                                                    (not (eq? associativity 'left-right))
                                                    (or (and left-child? (not (eq? associativity 'left)))
                                                        (and (not left-child?) (not (eq? associativity 'right)))))))))
             (doc-arg (lambda (left-child? arg)
                        (if (should-insert-parens? left-child? arg)
                          (pp/concat "(" (doc-node arg) ")")
                          (doc-node arg))))
             (num-args (length arg-list))
             (first-args (list-head arg-list (- num-args 1)))
             (last-arg (list-tail arg-list (- num-args 1))))
        (doc-list-with-sep (append (map (cut doc-arg #t <>) first-args)
                                   (map (cut doc-arg #f <>) last-arg))
                           (pp/concat " " (doc-node fun)))))
     ((and (instance-of? fun <sal-name-expr>)
           (or (not (instance-of? fun <sal-qualified-name-expr>))
               (slot-value pp-info :simplify-qualified-names?)
               (sal-name-ref/builtin? fun)))
      (doc-simple-app (doc-node fun) (doc-list arg-list)))
     (else
      (doc-app (doc-node fun) (doc-list arg-list))))))

(pp-method <sal-application>
  (pp-application ast pp-info depth))

(define-macro (pp-multi-bind-core tag decls body)
  `(pp/list-style4 ,tag (indent) (pp/concat (doc-list-with-sep+ "(" ")" ,decls ",") ":") ,body))

(define-macro (pp-multi-bind-expr tag)
  `(pp-multi-bind-core ,tag (doc-list-slot :local-decls) (doc-slot :expr)))

(pp-method <sal-lambda>
   (pp-multi-bind-expr 'LAMBDA))

(define (doc-prefix prefix doc)
  (pp/concat prefix (pp/nest (string-length prefix) doc)))

(define (pp-if tag ast pp-info depth)
  (pp/concat (pp/group (pp/concat (doc-prefix tag (doc-slot :cond-expr)) " THEN"
                                  (pp/nest* (indent) (nl) (doc-slot :then-expr))))
             (nl) (pp-else (slot-value ast :else-expr) pp-info depth)))

(define (pp-else ast pp-info depth)
  (if (instance-of? ast <sal-conditional>)
    (pp-if "ELSIF " ast pp-info depth)
    (pp/concat (doc-prefix "ELSE " (doc-node ast)) " ENDIF")))

(pp-method <sal-conditional>
  (pp/group (pp-if "IF " ast pp-info depth)))

(pp-method <sal-let-expr>
  (let* ((let-tag "LET ")
         (let-tag-len (string-length let-tag))
         (in-tag "IN ")
         (in-tag-len (string-length in-tag)))
    (pp/group
     (pp/concat "(" let-tag (pp/nest let-tag-len (doc-list-with-sep (doc-list-slot :local-decls) ","))
                (nl) in-tag (pp/nest* in-tag-len (doc-slot :expr) ")")))))

(pp-method <sal-let-decl>
  (pp/group (pp/concat (doc-slot :id) ": " (doc-slot :type)
                       (nl) "= " (doc-slot :value))))

(pp-method <sal-array-literal>
  (pp/group (pp/concat "[[" (car (doc-list-slot :local-decls)) "]" (pp/nest* 1 (nl) (doc-slot :expr)) "]")))

(pp-method <sal-tuple-literal>
  (doc-list-with-sep+ "(" ")" (doc-list-slot :exprs) ","))

(pp-method <sal-record-literal>
  (doc-list-with-sep+ "(# " " #)" (doc-list-slot :entries) ","))

(pp-method <sal-record-entry>
  (pp-binary ":=" :id :expr))

(pp-method <sal-set-pred-expr>
  (pp/group (pp/concat "{" (car (doc-list-slot :local-decls)) " |" (pp/nest* 1 (nl) (doc-slot :expr)) "}")))

(pp-method <sal-for-all-expr>
  (pp-multi-bind-expr 'FORALL))

(pp-method <sal-exists-expr>
  (pp-multi-bind-expr 'EXISTS))

(pp-method <sal-simple-selection>
    (pp/concat (doc-slot :target) "." (doc-slot :idx)))

(pp-method <sal-array-selection>
    (let ((array (doc-slot :fun))
          (idx (pp/nest* (indent) "[" (doc-slot :arg) "]"))
          (fun (slot-value ast :fun)))
      (if (or (instance-of? fun <sal-ast-leaf>)
              (sal-expr/lhs? fun)
              (instance-of? fun <sal-pre-operator>))
        (pp/concat array idx)
        (pp/concat "(" array ")" idx))))

(define-macro (pp-new-value)
  `(if (instance-of? (slot-value ast :new-value) <sal-ast-leaf>)
     (doc-slot :new-value)
     (pp/concat "(" (doc-slot :new-value) ")")))

(pp-method <sal-tuple-update>
    (pp/concat (doc-slot :target) " WITH ." (doc-slot :idx) " := " (pp-new-value)))

(pp-method <sal-record-update>
    (pp/concat (doc-slot :target) " WITH ." (doc-slot :idx) " := " (pp-new-value)))

(pp-method <sal-function-update>
    (pp/concat (doc-slot :target) " WITH (" (doc-slot :idx) ") := " (pp-new-value)))

(pp-method <sal-array-update>
    (pp/concat (doc-slot :target) " WITH [" (doc-slot :idx) "] := " (pp-new-value)))

(pp-method <sal-next-operator>
    (pp/concat (doc-slot :name-expr) "'"))

(pp-method <sal-pre-operator>
    (let loop ((i 1)
               (expr (slot-value ast :expr)))
      (cond
       ((instance-of? expr <sal-pre-operator>)
        (loop (+ i 1) (slot-value expr :expr)))
       ((= i 1)
        (pp/concat "PRE(" (doc-node expr) ")"))
       (else
        (pp/concat "PRE^" (object->string i) "(" (doc-node expr) ")")))))

(pp-method <sal-string-expr>
    (pp/concat "\"" (slot-value ast :string) "\""))

(pp-method <sal-mod-init>
    (doc-app 'INIT (list (doc-slot :module))))

(pp-method <sal-mod-trans>
    (doc-app 'TRANS (list (doc-slot :module))))

(pp-method <sal-simple-definition>
    (pp-binary "=" :lhs :rhs))

(pp-method <sal-simple-selection-definition>
    (pp-binary "IN" :lhs :rhs))

(pp-method <sal-for-all-definition>
    (pp-multi-bind-core 'FORALL (doc-list-slot :local-decls) (doc-list-with-sep (doc-list-slot :definitions) ",")))

(pp-method <sal-command-section>
    (doc-list-with-sep+ "[" "]" (append (doc-list-slot :commands)
                                        (if (slot-value ast :else-command)
                                          (list (doc-slot :else-command))
                                          '()))
                        (pp/concat (nl) "[]")))

(pp-method <sal-guarded-command>
    (pp/group (pp/concat (doc-slot :guard) (nl) "--> " (pp/nest 4 (doc-list-with-sep (doc-list-slot :assignments) ";")))))

(pp-method <sal-labeled-command>
    (let ((label (doc-slot :label)))
      (pp/concat (doc-slot :label) ": " (pp/nest (+ (symbol-length label) 2) (doc-slot :command)))))

(pp-method <sal-multi-command>
    (pp/group (pp/concat "([] (" (car (doc-list-slot :local-decls)) ") " (pp/nest* 4 (nl) (doc-slot :command)) ")")))

(pp-method <sal-else-command>
    (pp/concat "ELSE --> " (pp/nest 9 (doc-list-with-sep (doc-list-slot :assignments) ";"))))

(define-macro (pp-composition tag)
  `(pp/group (pp/concat "(" (pp/nest* 1 (doc-slot :module1) (nl) ,tag " " (pp/nest (+ (string-length ,tag) 1) (doc-slot :module2)) ")"))))
  
(pp-method <sal-asynch-composition>
    (pp-composition "[]"))

(pp-method <sal-synch-composition>
    (pp-composition "||"))

(pp-method <sal-observer>
    (pp-composition "OBSERVES"))

(define-macro (pp-multi-composition tag)
  `(pp/group (pp/concat "(" ,tag " (" (doc-node (car (slot-value ast :local-decls))) ")" 
                        (pp/nest* (+ 3 (string-length ,tag)) (nl) (doc-slot :module)))))

(pp-method <sal-multi-asynch-composition>
   (pp-multi-composition "[]"))

(pp-method <sal-multi-synch-composition>
   (pp-multi-composition "||"))

(define-macro (pp-module-org-core tag struct-doc)
  `(pp/concat "(" (pp/nest* 1 ,tag " " (pp/nest (+ (string-length ,tag) 1) (doc-list-with-sep ,struct-doc ","))
                            (nl)
                            "IN " (pp/nest 3 (doc-slot :module)) ")")))

(define-macro (pp-module-org tag)
  `(pp-module-org-core ,tag (doc-list-slot :identifiers)))

(pp-method <sal-hiding>
   (pp-module-org "LOCAL"))

(pp-method <sal-new-output>
   (pp-module-org "OUTPUT"))

(pp-method <sal-renaming>
   (pp-module-org-core "RENAME" (doc-list-slot :renames)))

(pp-method <sal-rename>
   (pp/concat (doc-slot :from-name) " TO " (doc-slot :to-expr)))

(pp-method <sal-with-module>
   (pp/concat "(" (pp/nest* 1 "WITH " (pp/nest (string-length "WITH ") (doc-list-with-sep (doc-list-slot :new-state-vars) ";"))
                            (nl)
                            (doc-slot :module) ")")))

(pp-method <sal-module-instance>
    (if (null? (slot-value ast :actuals))
      (doc-slot :module-name)
      (pp/group (pp/concat (doc-slot :module-name) (pp/line "") (doc-list-with-sep+ "[" "]" (doc-list-slot :actuals) ",")))))

(pp-method <sal-local-state-var-decl>
    (pp/concat "LOCAL " (call-next-method)))
(pp-method <sal-input-state-var-decl>
    (pp/concat "INPUT " (call-next-method)))
(pp-method <sal-global-state-var-decl>
    (pp/concat "GLOBAL " (call-next-method)))
(pp-method <sal-output-state-var-decl>
    (pp/concat "OUTPUT " (call-next-method)))

(define-macro (cmd-sec tag def-slot-id cmd-slot-id)
  `(if (or (not (null? (slot-value ast ,def-slot-id)))
           (slot-value ast ,cmd-slot-id))
     (pp/group (pp/concat (nl) ,tag 
                          (pp/nest* (indent) (nl) (doc-list-with-sep (doc-list-slot ,def-slot-id) ";" #f)
                                    (if (slot-value ast ,cmd-slot-id)
                                      (pp/concat (if (null? (slot-value ast ,def-slot-id))
                                                   *doc-nil*
                                                   (pp/concat ";" (nl)))
                                                 (doc-slot ,cmd-slot-id))
                                      *doc-nil*))))
     *doc-nil*))

(define (pp-state-vars state-vars pp-info depth)
  (apply pp/concat (map (lambda (state-var)
                          (pp/concat (nl) (doc-node state-var)))
                        state-vars)))

(pp-method <sal-base-module>
   (pp/concat "BEGIN" (pp/nest* (indent) 
                                (pp-state-vars (slot-value ast :state-vars) pp-info depth)
                                (if (null? (slot-value ast :definitions))
                                  *doc-nil*
                                  (pp/concat (nl) "DEFINITION"
                                             (pp/nest* (indent) (nl) (doc-list-with-sep (doc-list-slot :definitions) ";" #f))))
                                (cmd-sec 'INITIALIZATION :initialization-definitions :initialization-command-section)
                                (cmd-sec 'TRANSITION :transition-definitions :transition-command-section))
              (nl) "END"))

(define-macro (pp-section tag section-id)
  `(pp/concat (nl) ,tag (pp/nest* (indent) (nl) (doc-slot ,section-id))))

(pp-method <sal-flat-module>
           (pp/concat "BEGIN_FLAT" (pp/nest* (indent) 
                                    (pp-state-vars (slot-value ast :state-vars) pp-info depth)
                                    (pp-section "DEFINITION" :definition)
                                    (pp-section "INITIALIZATION" :initialization)
                                    (pp-section "TRANSITION" :transition)
                                    (pp-section "SKIP" :skip)
                                    (pp-section "VALID_INPUT" :valid-input-expr)
                                    (pp-section "VALID_STATE" :valid-state-expr)
                                    (pp-section "VALID_CONSTANT" :valid-constant-expr))
                      (nl) "END"))

(pp-method <sal-assertion-decl>
   (let* ((header (symbol-append (slot-value (slot-value ast :id) :name) '|: | (slot-value ast :kind)))
          (header-len (symbol-length header)))
     (pp/group (pp/concat header (pp/nest* (+ header-len 1) " " (doc-slot :assertion-expr))))))

(pp-method <sal-assertion-proposition>
    (doc-simple-app (slot-value ast :op)
                    (doc-list-slot :assertion-exprs)))

(pp-method <sal-module-models>
    (pp-binary "|-" :module :expr))

(pp-method <sal-module-implements>
    (pp-binary "IMPLEMENTS" :module1 :module2))

(pp-method <sal-esm-module>
           (pp/concat "ESM_MODULE" (pp/nest* (indent)
                                     (pp-state-vars (slot-value ast :state-vars) pp-info depth)
                                     (if (slot-value ast :definition)
                                       (pp-section "DEFINITION" :definition)
                                       *doc-nil*)
                                     (if (slot-value ast :initialization)
                                       (pp-section "INITIALIZATION" :initialization)
                                       *doc-nil*)
                                     (if (slot-value ast :transition)
                                       (pp-section "TRANSITION" :transition)
                                       *doc-nil*))
                      (nl) "END"))

(pp-method <sal-esm-choice>
  (doc-list-with-sep+ "(" ")" (doc-list-slot :statements) "[]"))

(pp-method <sal-esm-case>
  (apply pp/list-style4 "CASE" (indent) (doc-slot :expr) (doc-list-slot :case-entries)))

(pp-method <sal-esm-case-entry>
  (pp/list-style6 (doc-slot :value) (doc-slot :statement)))        

(pp-method <sal-esm-when-undefined>
  (pp/list-style4 "WHEN_UNDEFINED" (indent) (doc-slot :expr) (doc-slot :statement)))

(pp-method <sal-esm-seq>
  (doc-list-with-sep+ "(" ")" (doc-list-slot :statements) ";"))

(pp-method <sal-esm-multi-seq>
  (pp-multi-bind-core 'FOR_EACH (doc-list-slot :local-decls) (doc-slot :statement)))

(pp-method <sal-esm-multi-choice>
  (pp-multi-bind-core 'CHOICE (doc-list-slot :local-decls) (doc-slot :statement)))

(pp-method <sal-esm-guard>
  (pp/concat "GUARD(" (doc-slot :expr) ")"))

(pp-method <sal-esm-assignment>
  (pp-binary ":=" :lhs :rhs))

(pp-method <sal-esm-choice-assignment>
  (pp-binary "=:=" :lhs :rhs))




