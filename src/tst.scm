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

(define (sal-ast/used-contexts ast)
  (let ((ctx-table (make-hashtable)))
    (sal-ast/used-contexts-core ast ctx-table (make-sal-ast-table))
    (hashtable-key-list ctx-table)))

(define-generic (sal-ast/used-contexts-core ast found-ctxs found-names))

(define-method (sal-ast/used-contexts-core :around (ast <sal-ast>) (found-ctxs <primitive>) (found-names <primitive>))
  (cond
   ((sal-ast/context ast) =>
    (lambda (ctx)
      (hashtable-put! found-ctxs ctx #t))))
  (call-next-method))

(define-method (sal-ast/used-contexts-core (ast <sal-ast>) (found-ctxs <primitive>) (found-names <primitive>))
  (sal-ast/for-each-children (cut sal-ast/used-contexts-core <> found-ctxs found-names)
                             ast))

(define-method (sal-ast/used-contexts-core (ast <sal-module-name>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-module-name/definition ast)))
      (sal-ast/used-contexts-core def found-ctxs found-names))))

(define-method (sal-ast/used-contexts-core (ast <sal-type-name>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-type-name/definition ast)))
      (when def
        (sal-ast/used-contexts-core def found-ctxs found-names)))))

(define-method (sal-ast/used-contexts-core (ast <sal-name-expr>) (found-ctxs <primitive>) (found-names <primitive>))
  (unless (sal-ast-table/contains? found-names ast)
    (sal-ast-table/put! found-names ast #t)
    (let ((def (sal-name-expr/definition ast)))
      (when def
        (sal-ast/used-contexts-core def found-ctxs found-names)))))


(for-each (lambda (ctx) (print ">>>> " (sal-decl/name ctx)) #unspecified) (sal-ast/used-contexts (sal/assertion-name "peterson!mutex")))


(begin
  (define env (make-sal-environment))
  (define n (template->sxml
             (LAMBDAABSTRACTION
              (VARDECLS
               (VARDECL (IDENTIFIER x) (TYPENAME INTEGER))
               (VARDECL (IDENTIFIER y) (TYPENAME NATURAL)))
              (ARRAYLITERAL
               (VARDECL (IDENTIFIER z) (TYPENAME NATURAL))
               (APPLICATION
                (NAMEEXPR +)
                (TUPLELITERAL
                 (NAMEEXPR z)
                 (NAMEEXPR w)
                 (NAMEEXPR x)))))))
  (symbol-set->list (sal-env/free-variables env n)))

(begin
  (define env (make-sal-environment))
  (define code (template->sxml
                (LAMBDAABSTRACTION
                 (VARDECLS
                  (VARDECL (IDENTIFIER x) (TYPENAME INTEGER))
                  (VARDECL (IDENTIFIER y) (TYPENAME NATURAL)))
                 (ARRAYLITERAL
                  (VARDECL (IDENTIFIER z) (TYPENAME NATURAL))
                  (APPLICATION
                   (NAMEEXPR +)
                   (TUPLELITERAL
                    (NAMEEXPR h)
                    (NAMEEXPR z)
                    (NAMEEXPR w)
                    (NAMEEXPR y)
                    (NAMEEXPR x)))))))
  (define t1 (template->sxml
              (LETEXPRESSION
               (LETDECLARATIONS
                (LETDECLARATION (IDENTIFIER x) (TYPENAME NATURAL) (NAMEEXPR z)))
               (APPLICATION
                (NAMEEXPR +)
                (TUPLELITERAL
                 (NAMEEXPR x)
                 (NAMEEXPR x))))))
  (define t2 (template->sxml
               (APPLICATION
                (NAMEEXPR +)
                (TUPLELITERAL
                 (NAMEEXPR y)
                 (NAMEEXPR y)))))
  (symbol-set->list (sal-env/free-variables env t2))                 
  (define subst `((h . ,t1) (w . ,t2)))
  (sxml/pp (sal-env/substitute env code subst)))

(sxml/pp (build-internal-qualified-name 'tst `(,(template->sxml (TYPEDECL (IDENTIFIER T)))) `(,(template->sxml (VARDECL (IDENTIFIER N) (TYPENAME NATURAL))))))

(begin
  (define env (make-sal-environment))
  
  (define nat-length (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER length) (CONTEXTNAME (IDENTIFIER list) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))
  
  (sxml/pp (sal-env/qualified-name-declaration env nat-length))
  )


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'list))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  ;; (define ctx (sal-env/context env 'mutex))
  (define name (template->sxml (QUALIFIEDASSERTIONNAME (IDENTIFIER mutual-exclusion) (CONTEXTNAME (IDENTIFIER mutex)))))
  (define expansion (sal/expand env name))
  (define result (car expansion))
  (define defs (cdr expansion))
  (for-each sxml/pp defs)
  (sxml/pp result))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  ;; (define ctx (sal-env/context env 'mutex))
  (define name (template->sxml (QUALIFIEDASSERTIONNAME (IDENTIFIER deadlock-free) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))
  (define expansion (sal/expand env name))
  (define result (car expansion))
  (define defs (cdr expansion))
  (for-each sxml/pp defs)
  (sxml/pp result)

;   (sxml/pp (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER idle-process)
;                                                                                         (CONTEXTNAME (IDENTIFIER pcp-scheduler)
;                                                                                                      (ACTUALPARAMETERS
;                                                                                                       (ACTUALEXPRS
;                                                                                                        (
)


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'let-star))
  (define ctx-decls (sal-context/declarations ctx))
  (for-each sxml/pp ctx-decls))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic))
  (define ctx-decls (sal-context/declarations ctx))
  (for-each sxml/pp ctx-decls))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'simple-set))
  (define ctx-decls (sal-context/declarations ctx))
  (for-each sxml/pp ctx-decls))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic))
  (define ctx-decls (sal-context/declarations ctx))
  (define expansion (sal/expand env (template->sxml (BEGIN ,@ctx-decls))))
  (define result (car expansion))
  (define defs (cdr expansion))
  (for-each sxml/pp defs)
  (print "------------------------------")
  (sxml/pp result)
)

;;  (define ctx (sal-env/context env 'pcp-scheduler))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define decls (make-sal-global-context-for-module-at env 'pcp-scheduler 'simple-system))
  (for-each (lambda (n) (sxml/pp n) (print "----------------")) (queue->list decls)))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define global-ctx (make-sal-global-context-for-assertion-at env 'tst-pcp-generic 'deadlock-free))
  (for-each (lambda (n) 
              (print "attributes: ")
              (for-each (lambda (pair)
                          (let ((key (car pair))
                                (val (cdr pair)))
                            (display* key " = ")
                            (cond 
                             ((sxml-node? val) 
                              (print "sxml-node"))
                             ((and (list? val) (not (null? val)) (sxml-node? (car val)))
                              (print "sxml-node-list"))
                             (else
                              (print val)))))
                        (sxml/attributes n))
              (print "node: ")
              (sal/pp-node-with-place-info n) 
              (print "----------------"))
            (sal-global-context/declarations global-ctx)))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define global-ctx (make-sal-global-context-for-assertion-at env 'list 'append-assoc))
  (for-each (lambda (n) 
              (print "attributes: ")
              (for-each (lambda (pair)
                          (let ((key (car pair))
                                (val (cdr pair)))
                            (display* key " = ")
                            (cond 
                             ((sxml-node? val) 
                              (print "sxml-node"))
                             ((and (list? val) (not (null? val)) (sxml-node? (car val)))
                              (print "sxml-node-list"))
                             (else
                              (print val)))))
                        (sxml/attributes n))
              (print "node: ")
              (sal/pp-node-with-place-info n) 
              (print "----------------"))
            (sal-global-context/declarations global-ctx)))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-list))
  (sxml/pp (sal-context/constant-declaration ctx 'add-elements))
)

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic))
  (sxml/pp (sal-context/constant-declaration ctx 'tasks))
)

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("/homes/demoura/transeda-ex/ex2"))
  (define ctx (sal-env/context env 'tb_top_c))
  (for-each sxml/pp (sal-context/declarations ctx)))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'arbiter))
  (for-each sxml/pp (sal-context/declarations ctx)))



(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'simple-set))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-task))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-rsrc))
  (for-each sxml/pp (sal-context/declarations ctx)))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-scheduler))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  ;; (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-generic))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic))
  (for-each sxml/pp (sal-context/declarations ctx)))


(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context env 'simplemutex2))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'rename-tst))
  (define mod (sal-context/module-declaration ctx 'tst))
  (sxml/pp (sal/expand-renames mod)))
  (sxml/match-or-fail mod
    ((MODULEDECLARATION ?- ?- ?mod-body)
     (sxml/pp (sal/expand-renames mod-body)))))


(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context env 'tst_pcp_generic))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context env 'fourslot))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context env 'four-slot))
  (for-each sxml/pp (sal-context/declarations ctx)))


(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context env 'ex5))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (define env (make-sal-environment))
  
  (define nat-remove-first (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER remove_first) (CONTEXTNAME (IDENTIFIER list) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))  
  (sxml/pp (sal-env/qualified-name-declaration env nat-remove-first))
  )



(begin
  (define env (make-sal-environment))
  
  (define ceiling_aux (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER ceiling_aux) 
                                                         (CONTEXTNAME (IDENTIFIER pcp_task) 
                                                                      (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL))
                                                                                        (ACTUALEXPRS (NAMEEXPR j))))))) 
  (define result (sal-env/qualified-name-declaration env ceiling_aux))
  (print "===========================================================")
  (print "===========================================================")
  (sxml/pp result)
  )


(begin
  (define env (make-sal-environment))
  
  (define tasks (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER tasks) 
                                                         (CONTEXTNAME (IDENTIFIER tst_pcp_generic)))))
  (define result (sal-env/qualified-name-declaration env tasks))
  (sxml/pp result)
  )


(begin
  (define env (make-sal-environment))
  (define mutex (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER mutex)
                                                     (CONTEXTNAME (IDENTIFIER simplemutex2)))))
  (sxml/pp (sal-env/qualified-name-declaration env mutex)))

(begin
  (define env (make-sal-environment))
  (define system-mod (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system)
                                                          (CONTEXTNAME (IDENTIFIER simplemutex2)))))
  (sxml/pp (sal-env/qualified-name-declaration env system-mod)))

(begin
  (define env (make-sal-environment))
  (define monitor (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER monitored_system)
                                                       (CONTEXTNAME (IDENTIFIER tst_pcp_generic)))))
  (sxml/pp (sal-env/qualified-name-declaration env monitor)))


(begin
  (define env (make-sal-environment))
  (define monitor_inst (template->sxml (MODULEINSTANCE
                                        (QUALIFIEDMODULENAME (IDENTIFIER monitored_system)
                                                             (CONTEXTNAME (IDENTIFIER tst_pcp_generic)))
                                        (MODULEACTUALS))))
  (sxml/pp (sal-env/module-instance env monitor_inst))
  (sxml/pp (sal/module-type (sal-env/module-instance env monitor_inst)))
)




(begin
  (define env (make-sal-environment))
  (define system-mod (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system)
                                                      (CONTEXTNAME (IDENTIFIER simplemutex3)))))
  (sxml/pp (sal-env/qualified-name-declaration env system-mod)))


(begin
  (define env (make-sal-environment))
  (define system-mod (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system)
                                                      (CONTEXTNAME (IDENTIFIER simplemutex3)))))
  (sxml/pp (sal-env/qualified-name-declaration env system-mod)))


(begin
  (define env (make-sal-environment))
  (define system-mod (template->sxml (MODULEINSTANCE
                                      (QUALIFIEDMODULENAME (IDENTIFIER system)
                                                           (CONTEXTNAME (IDENTIFIER simplemutex3)))
                                      (MODULEACTUALS))))
  (sxml/pp (sal-env/module-instance env system-mod)))
  (sxml/pp (sal/module-type (sal-env/module-instance env system-mod))))



(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context env 'simplemutex3))
  (sal/type-check env ctx))



(define t (list->sxml '(TUPLETYPE (TYPENAME NATURAL)
                                  (TYPENAME NATURAL))))

(sxml/match t ((TUPLETYPE (TYPENAME (when ?id (sal-env/builtin-type? env id))) ?-)
               (print id)))


(sal-env/subtype? env (list->sxml '(TYPENAME NATURAL)) (list->sxml '(TYPENAME INTEGER)))
(sal-env/subtype? env (list->sxml '(TYPENAME REAL)) (list->sxml '(TYPENAME INTEGER)))

(sal-env/subtype? env 
                  (list->sxml '(TUPLETYPE (TYPENAME NATURAL)
                                          (TYPENAME NATURAL)))
                  (list->sxml '(TUPLETYPE (TYPENAME INTEGER)
                                          (TYPENAME REAL))))

(sal-env/subtype? env
                  (list->sxml '(SUBRANGE (NUMERAL "5") (NUMERAL "10")))
                  (list->sxml '(SUBRANGE (NUMERAL "0") (NUMERAL "100"))))

(sal-env/equivalent-types? env
                  (list->sxml '(SUBRANGE (NUMERAL "0") (NUMERAL "100")))
                  (list->sxml '(SUBRANGE (NUMERAL "0") (NUMERAL "100"))))


(and
 (<=mpq (sal-numeral->mpq (list->sxml '(NUMERAL "0"))) (sal-numeral->mpq (list->sxml '(NUMERAL "0"))))
 (>=mpq (sal-numeral->mpq (list->sxml '(NUMERAL "10"))) (sal-numeral->mpq (list->sxml '(NUMERAL "100")))))


(sal-numeral->mpq (list->sxml '(NUMERAL "10")))


(expand-once '(sxml/match type2
                          ((TYPENAME (or NATURAL INTEGER REAL))
                           #t)))

(sxml/match (template->sxml (TYPENAME PLACE: "10" INTEGER))
  ((TYPENAME (?? (lambda (n) (print (sxml/attribute n 'PLACE)) #t))  (or NATURAL INTEGER REAL)) 'ok))

(expand-once
 '(sxml/match (list->sxml '(TYPENAME INTEGER))
              ((TYPENAME (or NATURAL INTEGER)) 'ok)))


(let ((type1 (list->sxml '(TYPENAME NATURAL)))
      (type2 (list->sxml '(TYPENAME INTEGER))))
  (sxml/match-or-fail type1
                      ((TYPENAME NATURAL)
                       (sxml/match type2
                                   ((TYPENAME (or NATURAL INTEGER REAL))
                                    #t)))))

(sal-env/follow-type-references env (list->sxml '(TYPENAME NATURAL)))

(sxml/pp (sal-env/context-ast env 'list))

(expand-once  '(sxml/match type
    ((TYPENAME ?id)
     (unless (sal-env/builtin-type? self id)
       (sign-source-error self type "Unknow type \"~a\"." id))
     type)
    ((QUALIFIEDTYPENAME ?- ?-)
     (sxml/match (sal-env/qualified-type-name-declaration self type)
       ((TYPEDECLARATION ?- ?type-def)
        (sal-env/follow-type-references self type-def))
       ((TYPEDECLARATION ?-)
        'unknown)
       (?decl (sign-source-error self decl "Invalid SAL abstract syntax tree."))))
    (?-
     type)))


(define t (template->sxml (FUNCTIONTYPE PLACE: "10 20 30 40"
                           (TUPLETYPE
                            (TYPENAME NATURAL)
                            (TYPENAME NATURAL))
                           (ARRAYTYPE (TYPENAME NATURAL) (TYPENAME INTEGER)))))


(sxml/pp 
 (sxml/bottom-up-rewriter t
                          ((TYPENAME ?id) id)))                             

(sxml/pp
 (sxml/exhaustive-bottom-up-rewriter t
                                     ((TYPENAME ?id) id)))                              



(sxml/pp (template->sxml (TYPENAME ,(+ 10 20))))

(sxml/pp (template->sxml (TYPENAME ,@(list (+ 10 20) 100))))


(match-case '((kwote assign) ($ 'aaa) 10 20 30) ((or ((or ((kwote kwote) ?tag) ?tag) ($ . ?tst) . ?children)
                                                     ((or ((kwote kwote) ?tag) ?tag) . ?children)) (print ">>>>> tst = " tst ", tag=" tag ", children=" children)))


(match-case '(assign ($ 'aaa) 10 20 30) ((or ((or ((kwote kwote) ?tag) ?tag) ($ . ?tst) . ?children)
                                                     ((or ((kwote kwote) ?tag) ?tag) . ?children)) (print ">>>>> tst = " tst ", tag=" tag ", children=" children)))


(match-case '(assign 10 20 30) ((or ((or ((kwote kwote) ?tag) ?tag) ($ . ?tst) . ?children)
                                                     ((or ((kwote kwote) ?tag) ?tag) . ?children)) (print ">>>>> tst = " tst ", tag=" tag ", children=" children)))



(define env (make-sal-environment))
(define ctx (sal-env/context-ast env 'aux))
(define n1 (sxml/find-node (sxml/match-lambda
                            ((UPDATEEXPRESSION ?- ?- ?-)
                             #t))
                           ctx))

(sxml/pp n1)

(define t1 (template->sxml (TUPLELITERAL (NUMERAL "10") (NUMERAL "20"))))

(sxml/pp (sal-env/expression-approx-type env n1))

(sxml/pp (sal-env/expression-approx-type env t1))


(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context-ast env 'aux))
  (define n1 (sxml/find-node (sxml/match-lambda
                              ((UPDATEEXPRESSION ?- ?- ?-)
                               #t))
                             ctx))
  (sxml/pp (sal-env/expression-approx-type env n1)))


(begin
  (define env (make-sal-environment))
  (define ctx (sal-env/context-ast env 'aux))
  (define n1 (sxml/find-node (sxml/match-lambda
                              ((RECORDLITERAL . ?-)
                               #t))
                             ctx))
  (sxml/pp (sal-env/expression-approx-type env n1)))



(begin
  (define env (make-sal-environment))
  (define sxml1 (template->sxml (LAMBDAABSTRACTION 
                                 (VARDECLS 
                                  (VARDECL
                                   (IDENTIFIER x)
                                   (TYPENAME NATURAL))
                                  (VARDECL
                                   (IDENTIFIER y)
                                   (TYPENAME NATURAL)))
                                 (APPLICATION
                                  (NAMEEXPR +)
                                  (TUPLELITERAL
                                   (NAMEEXPR x)
                                   (NAMEEXPR y))))))
  (define sxml2 (template->sxml (LAMBDAABSTRACTION 
                                 (VARDECLS 
                                  (VARDECL
                                   (IDENTIFIER w)
                                   (TYPENAME NATURAL))
                                  (VARDECL
                                   (IDENTIFIER h)
                                   (TYPENAME NATURAL)))
                                 (APPLICATION
                                  (NAMEEXPR +)
                                  (TUPLELITERAL
                                   (NAMEEXPR w)
                                   (NAMEEXPR h))))))
  (sal-env/alpha-equivalent? env (make-symbol-table) sxml1 (make-symbol-table) sxml2))


(begin
  (define env (make-sal-environment))
  (define vvv-parser (make-sal-parser env))
  (try
   (sal-parser/parse-file vvv-parser "tst.sal")
   (lambda (escape proc msg obj)
     (print "proc = " proc)
     (print "msg  = " msg)
     (print "obj  = " obj)
     (escape #f))))

(begin
  (define env (make-sal-environment))
  (define vvv-parser (make-ls-parser env))
  (sxml/pp (ls-parser/parse-file vvv-parser "four-slot.lsal")))

(begin
  (define env (make-sal-environment))
  (define vvv-parser (make-ls-parser env))
  (sxml/pp (ls-parser/parse-file vvv-parser "mutex.lsal")))

(begin
  (define env (make-sal-environment))
  (define vvv-parser (make-ls-parser env))
  (sxml/pp (ls-parser/parse-file vvv-parser "list.lsal")))

(begin
  (define env (make-sal-environment))
  (define cons-func (template->sxml (QUALIFIEDNAMEEXPR
                                     (IDENTIFIER cons?)
                                     (CONTEXTNAME
                                      (IDENTIFIER list)
                                      (ACTUALPARAMETERS
                                       (ACTUALTYPES (TYPENAME NATURAL)))))))
  (sxml/pp (sal-env/qualified-name-declaration env cons-func)))


(sxml/hash (template->sxml
              (CONSTANTDECLARATION
               (IDENTIFIER cons?)
               (FUNCTIONTYPE
                (QUALIFIEDTYPENAME
                 (IDENTIFIER list)
                 (CONTEXTNAME
                  (IDENTIFIER list)
                  (ACTUALPARAMETERS
                   (ACTUALTYPES (TYPENAME NATURAL)))))
                (TYPENAME BOOLEAN)))))
(sxml/hash (template->sxml
            (CONSTANTDECLARATION
             (IDENTIFIER cons?)
             (FUNCTIONTYPE
              (QUALIFIEDTYPENAME
               (IDENTIFIER list)
               (CONTEXTNAME
                (IDENTIFIER list)
                (ACTUALPARAMETERS
                 (ACTUALTYPES (TYPENAME SALENV1001:unique:T:1033)))))
              (TYPENAME BOOLEAN)))))


(iterator/for-each sxml/pp (sal-evaluator/make-type-iterator 1 1 (template->sxml (RECORDTYPE (FIELDDECLARATION (IDENTIFIER "flag") (TYPENAME BOOLEAN)) (FIELDDECLARATION (IDENTIFIER "idx") (SUBRANGE (NUMERAL "1") (NUMERAL "4")))))))

(iterator/for-each sxml/pp (sal-evaluator/make-type-iterator 1 1 (template->sxml (DATATYPE (CONSTRUCTOR (IDENTIFIER nil)) (CONSTRUCTOR (IDENTIFIER turn) (ACCESSOR (IDENTIFIER flag) (TYPENAME BOOLEAN))) (CONSTRUCTOR (IDENTIFIER msg) (ACCESSOR (IDENTIFIER id1) (TYPENAME BOOLEAN)) (ACCESSOR (IDENTIFIER id2) (TYPENAME BOOLEAN)))))))

(iterator/for-each sxml/pp (sal-evaluator/make-type-iterator 1 1 (template->sxml (FUNCTIONTYPE (TUPLETYPE (TYPENAME BOOLEAN) (TYPENAME BOOLEAN)) (TYPENAME BOOLEAN)))))


(iterator/for-each sxml/pp (sal-evaluator/make-type-iterator 1 1 (template->sxml (ARRAYTYPE (SUBRANGE (NUMERAL "1") (NUMERAL "4")) (TYPENAME BOOLEAN)))))

(iterator/for-each sxml/pp (sal-evaluator/make-type-iterator 1 1 (template->sxml (TYPENAME CHAR))))

(sal-evaluator/number-of-type-elements 1 1 (template->sxml (ARRAYTYPE (SUBRANGE (NUMERAL "1") (NUMERAL "4")) (TYPENAME BOOLEAN))))

(sal-evaluator/number-of-type-elements 1 1 (template->sxml (FUNCTIONTYPE (TUPLETYPE (TYPENAME BOOLEAN) (TYPENAME BOOLEAN)) (TYPENAME BOOLEAN))))

(sal-evaluator/number-of-type-elements 1 1 (template->sxml (DATATYPE (CONSTRUCTOR (IDENTIFIER nil)) (CONSTRUCTOR (IDENTIFIER turn) (ACCESSOR (IDENTIFIER flag) (TYPENAME BOOLEAN))) (CONSTRUCTOR (IDENTIFIER msg) (ACCESSOR (IDENTIFIER id1) (TYPENAME BOOLEAN)) (ACCESSOR (IDENTIFIER id2) (TYPENAME BOOLEAN))))))

(sal-evaluator/number-of-type-elements 1 1 (template->sxml (RECORDTYPE (FIELDDECLARATION (IDENTIFIER "flag") (TYPENAME BOOLEAN)) (FIELDDECLARATION (IDENTIFIER "idx") (SUBRANGE (NUMERAL "1") (NUMERAL "4"))))))

(sal-evaluator/number-of-type-elements 1 1 (template->sxml (TUPLETYPE (TYPENAME BOOLEAN) (SUBRANGE (NUMERAL "1") (NUMERAL "5")))))

(sal-evaluator/number-of-type-elements 1 1 (template->sxml (SCALARTYPE (SCALARELEMENT pc1) (SCALARELEMENT pc2) (SCALARELEMENT pc3))))


(iterator/for-each sxml/pp (sal-evaluator/make-type-iterator 1 1 (template->sxml (SCALARTYPE (SCALARELEMENT pc1) (SCALARELEMENT pc2) (SCALARELEMENT pc3)))))


(sxml/pp
 (sal-evaluator/convert-application-to-let-expression #unspecified
                                                      (template->sxml (LAMBDAABSTRACTION (VARDECLS (VARDECL PLACE: 10 (IDENTIFIER x) (TYPENAME BOOLEAN))
                                                                                                   (VARDECL (IDENTIFIER y) (TYPENAME BOOLEAN)))
                                                                                         (APPLICATION (NAMEEXPR OR) (TUPLELITERAL 
                                                                                                                     (NAMEEXPR x)
                                                                                                                     (NAMEEXPR y)))))
                                                      (template->sxml (TUPLELITERAL (NAMEEXPR FALSE) (NAMEEXPR TRUE)))))



(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define global-ctx (make-sal-global-context-for-assertion-at env 'list 'append-assoc))
  (define evaluator (make-sal-evaluator global-ctx))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NAMEEXPR a) (NUMERAL "0"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (NAMEEXPR >) (TUPLELITERAL 
                                                                         (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5")))
                                                                         (NUMERAL "12"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (NAMEEXPR <) (TUPLELITERAL 
                                                                         (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5")))
                                                                         (NUMERAL "12"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (NAMEEXPR >=) (TUPLELITERAL 
                                                                          (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5")))
                                                                          (NUMERAL "12"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (NAMEEXPR <=) (TUPLELITERAL 
                                                                          (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5")))
                                                                          (NUMERAL "12"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (NAMEEXPR =) (TUPLELITERAL 
                                                                         (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5")))
                                                                         (NUMERAL "12"))))))
  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (NAMEEXPR =) (TUPLELITERAL 
                                                                         (APPLICATION (NAMEEXPR +) (TUPLELITERAL (NUMERAL "10") (NUMERAL "5")))
                                                                         (NUMERAL "15"))))))

  (sxml/pp (sal-evaluator/evaluate evaluator (template->sxml 
                                              (APPLICATION (LAMBDAABSTRACTION EXECUTABLE: #t (VARDECLS (VARDECL (IDENTIFIER a) (TYPENAME NATURAL)))
                                                                              (APPLICATION EXECUTABLE: #t (NAMEEXPR EXECUTABLE: #t +)
                                                                                           (TUPLELITERAL EXECUTABLE: #t
                                                                                                         (NAMEEXPR EXECUTABLE: #t a)
                                                                                                         (NUMERAL EXECUTABLE: #t "1"))))
                                                           (NUMERAL EXECUTABLE: #t "10")))))

  (sal-evaluator/executable? evaluator (make-sal-empty-evaluation-environment) 
                             (template->sxml
                              (LAMBDAABSTRACTION EXECUTABLE: #t (VARDECLS (VARDECL (IDENTIFIER a) (TYPENAME NATURAL)))
                                                 (APPLICATION EXECUTABLE: #t (NAMEEXPR EXECUTABLE: #t +)
                                                              (TUPLELITERAL EXECUTABLE: #t
                                                               (NAMEEXPR EXECUTABLE: #t a)
                                                               (NUMERAL EXECUTABLE: #t "1"))))))




  )


(begin
  (trace/enable! 'update-expression-evaluation)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
;   (define ctx (sal-env/context env 'simple))
;   (for-each sxml/pp (sal-context/declarations ctx)))

  (define global-ctx (make-sal-global-context-for-assertion-at env 'simple 'tst))
;   (symbol-set->list
;    (sal-global-context/free-variables global-ctx (template->sxml (LETEXPRESSION (LETDECLARATIONS
;                                                                                  (LETDECLARATION (IDENTIFIER n) (TYPENAME NATURAL) (NAMEEXPR x)))
;                                                                                 (ARRAYSELECTION (NAMEEXPR y) (NAMEEXPR n))))))
  ;; (for-each sxml/pp (sal-global-context/declarations global-ctx)))
  (define ev (sal-global-context/evaluator global-ctx))
  ;; (sal-evaluator/set-maximum-number-of-nested-beta-reductions! ev 2)
  (sal-evaluator/set-maximum-number-of-elements-to-iterate! ev 2)
  (sal-global-context/simplify! global-ctx)
  (for-each sxml/pp (sal-global-context/declarations global-ctx))
  )

(sxml/pp
 (sal/substitute (make-symbol-table) (template->sxml (LETEXPRESSION
                                                       (LETDECLARATIONS
                                                        (LETDECLARATION (IDENTIFIER x) (TYPENAME NATURAL) (APPLICATION
                                                                                                           (NAMEEXPR +)
                                                                                                           (TUPLELITERAL 
                                                                                                            (NAMEEXPR x)
                                                                                                            (NUMERAL 1)))))
                                                       (NAMEEXPR x)))
                  (list (cons 'x (template->sxml (NUMERAL 10))))))
  
(sxml/pp
 (sal/substitute (make-symbol-table) (template->sxml (LAMBDAABSTRACTION
                                                       (VARDECLS (VARDECL (IDENTIFIER y) (TYPENAME NATURAL)))
                                                       (APPLICATION
                                                        (NAMEEXPR +)
                                                        (TUPLELITERAL 
                                                         (NAMEEXPR y)
                                                         (NAMEEXPR x)))))
                  (list (cons 'x (template->sxml (NUMERAL 10))))))


(begin
  (trace/enable! 'expand-modules)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define global-ctx (make-sal-global-context-for-assertion-at env 'mutex 'mutual-exclusion))
  (sal-global-context/simplify! global-ctx)
  (for-each (lambda (decl)
            (lsal/pp decl 100)
              (print "")
              (print "------------------"))
          (sal-global-context/declarations global-ctx))
  (print "=================================")
  (sal-global-context/expand-modules! global-ctx)
  ; (for-each sxml/pp (sal-global-context/declarations global-ctx))
  (for-each (lambda (decl)
              (lsal/pp decl 100)
              (print "")
              (print "------------------"))
            (sal-global-context/declarations global-ctx))
)


(begin
  (define pos1 *undef*)
  (sxml/pp pos1)
  (define pos2 
    (update-pos #unspecified pos1
                (template->sxml (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL))) 
                (template->sxml (TUPLESELECTION (NAMEEXPR a) (NUMERAL "1")))))
  (sxml/pp pos2)
  (define pos3
    (update-pos #unspecified pos2
                (template->sxml (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL))) 
                (template->sxml (TUPLESELECTION (NAMEEXPR a) (NUMERAL "2")))))
  (sxml/pp pos3))
    

(begin
  (define pos1 *undef*)
  (sxml/pp pos1)
  (define pos2 
    (update-pos #unspecified pos1
                (template->sxml (TUPLETYPE (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)) 
                                           (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (TUPLESELECTION (NAMEEXPR a) (NUMERAL "1")) (NUMERAL "2")))))
  (sxml/pp pos2)
  (define pos3
    (update-pos #unspecified pos2
                (template->sxml (TUPLETYPE (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)) 
                                           (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (TUPLESELECTION (NAMEEXPR a) (NUMERAL "2")) (NUMERAL "1")))))
  (sxml/pp pos3)
  (define pos4
    (update-pos #unspecified pos3
                (template->sxml (TUPLETYPE (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)) 
                                           (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (TUPLESELECTION (NAMEEXPR a) (NUMERAL "1")) (NUMERAL "1")))))
  (sxml/pp pos4)
  )


(begin
  (define pos1 *undef*)
  (sxml/pp pos1)
  (define pos2 
    (update-pos #unspecified pos1
                (template->sxml (RECORDTYPE 
                                 (FIELDDECLARATION (IDENTIFIER x) 
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)))
                                 (FIELDDECLARATION (IDENTIFIER y)
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL)))))
                (template->sxml (TUPLESELECTION (RECORDSELECTION (NAMEEXPR a) (IDENTIFIER x)) (NUMERAL "1")))))
  (sxml/pp pos2)
  (define pos3
    (update-pos #unspecified pos2
                (template->sxml (RECORDTYPE 
                                 (FIELDDECLARATION (IDENTIFIER x) 
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)))
                                 (FIELDDECLARATION (IDENTIFIER y)
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL)))))
                (template->sxml (TUPLESELECTION (RECORDSELECTION (NAMEEXPR a) (IDENTIFIER y)) (NUMERAL "2")))))
  (sxml/pp pos3)
  (define pos4
    (update-pos #unspecified pos3
                (template->sxml (RECORDTYPE 
                                 (FIELDDECLARATION (IDENTIFIER x) 
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)))
                                 (FIELDDECLARATION (IDENTIFIER y)
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL)))))
                (template->sxml (TUPLESELECTION (RECORDSELECTION (NAMEEXPR a) (IDENTIFIER x)) (NUMERAL "2")))))
  (sxml/pp pos4)
  (define pos5
    (update-pos #unspecified pos4
                (template->sxml (RECORDTYPE 
                                 (FIELDDECLARATION (IDENTIFIER x) 
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)))
                                 (FIELDDECLARATION (IDENTIFIER y)
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL)))))
                (template->sxml (TUPLESELECTION (RECORDSELECTION (NAMEEXPR a) (IDENTIFIER y)) (NUMERAL "1")))))
  (sxml/pp pos5)
  (define pos6
    (update-pos #unspecified pos5
                (template->sxml (RECORDTYPE 
                                 (FIELDDECLARATION (IDENTIFIER x) 
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL)))
                                 (FIELDDECLARATION (IDENTIFIER y)
                                                   (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR NATURAL) (NAMEEXPR REAL)))))
                (template->sxml (TUPLESELECTION (RECORDSELECTION (NAMEEXPR a) (IDENTIFIER y)) (NUMERAL "3")))))
  (sxml/pp pos6)
  )


(begin
  (define pos1 *undef*)
  (sxml/pp pos1)
  (define pos2 
    (update-pos #unspecified pos1
                (template->sxml (ARRAYTYPE (SUBRANGE 1 5) (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (ARRAYSELECTION (NAMEEXPR a) (NUMERAL "3")) (NUMERAL "1")))))
  (sxml/pp pos2)
  (define pos3 
    (update-pos #unspecified pos2
                (template->sxml (ARRAYTYPE (SUBRANGE 1 5) (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (ARRAYSELECTION (NAMEEXPR a) (NUMERAL "1")) (NUMERAL "2")))))
  (sxml/pp pos3)
  (define pos4
    (update-pos #unspecified pos3
                (template->sxml (ARRAYTYPE (SUBRANGE 1 5) (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (ARRAYSELECTION (NAMEEXPR a) (NUMERAL "3")) (NUMERAL "2")))))
  (sxml/pp pos4)
  (define pos5
    (update-pos #unspecified pos1
                (template->sxml (ARRAYTYPE (SUBRANGE 1 5) (TUPLETYPE (NAMEEXPR NATURAL) (NAMEEXPR REAL))))
                (template->sxml (TUPLESELECTION (ARRAYSELECTION (NAMEEXPR a) (APPLICATION (NAMEEXPR a) (NAMEEXPR b))) (NUMERAL "2")))))
  (sxml/pp pos5)
)

(begin
  (print "")
  (sxml/pp
   (sal/convert-to-supported-simple-def
    #unspecified
    (template->sxml (SIMPLEDEFINITION
                     (TUPLESELECTION
                      (ARRAYSELECTION
                       (RECORDSELECTION
                        (NEXTOPERATOR
                         (NAMEEXPR a))
                        (IDENTIFIER x))
                       (APPLICATION (NAMEEXPR inc) (NUMERAL "0")))
                      (NUMERAL "1"))
                     (RHSEXPRESSION (NAMEEXPR foo)))))))


(begin
  (print "")
  (sxml/pp
   (sal/convert-to-supported-simple-def
    #unspecified
    (template->sxml (SIMPLEDEFINITION
                     (ARRAYSELECTION
                      (ARRAYSELECTION
                       (NEXTOPERATOR
                        (NAMEEXPR a))
                       (APPLICATION (NAMEEXPR dec) (NUMERAL "1")))
                      (APPLICATION (NAMEEXPR inc) (NUMERAL "0")))
                     (RHSEXPRESSION (NAMEEXPR foo)))))))


(begin
  (print "")
  (sxml/pp
   (sal/convert-to-supported-simple-def
    #unspecified
    (template->sxml (SIMPLEDEFINITION
                     (TUPLESELECTION
                      (ARRAYSELECTION
                       (ARRAYSELECTION
                        (RECORDSELECTION
                         (NEXTOPERATOR
                          (NAMEEXPR a))
                         (IDENTIFIER idx))
                        (APPLICATION (NAMEEXPR dec) (NUMERAL "1")))
                      (APPLICATION (NAMEEXPR inc) (NUMERAL "0")))
                      (NUMERAL "10"))
                     (RHSEXPRESSION (NAMEEXPR foo)))))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (make-sal-global-context-for-module-at env 'bug 'system))
  (for-each sxml/pp (sal-global-context/declarations ctx)))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (sal-env/finite-type? env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER list) (CONTEXTNAME (IDENTIFIER list) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))
  (sal-env/finite-type? env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER ClockRange) (CONTEXTNAME (IDENTIFIER pcp-scheduler) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME BOOLEAN)) (ACTUALEXPRS (NUMERAL 3) (TYPENAME T)))))))
  (sal-env/finite-type? env (template->sxml (TUPLETYPE (TYPENAME BOOLEAN) (SUBRANGE (NUMERAL 0) (NUMERAL 10)))))
  

  (sxml/pp (sal-env/follow-type-references env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER ClockRange) (CONTEXTNAME (IDENTIFIER pcp-scheduler) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME BOOLEAN)) (ACTUALEXPRS (NUMERAL 3) (TYPENAME T))))))))

  (sal-env/user-defined-recursive-function? env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER length) (CONTEXTNAME (IDENTIFIER list) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))

  (sal-env/user-defined-recursive-function? env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER length) (CONTEXTNAME (IDENTIFIER list) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))

  (sal-env/user-defined-recursive-function? env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER insert) (CONTEXTNAME (IDENTIFIER simple-set) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))


(sxml/pp  (sal-env/follow-expr-references env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER insert) (CONTEXTNAME (IDENTIFIER simple-set) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL))))))))

(sxml/pp
 (sal-env/follow-expr-references env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER length) (CONTEXTNAME (IDENTIFIER list) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL))))))))

(sal-env/number-of-type-elements env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER Set) (CONTEXTNAME (IDENTIFIER simple-set) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME NATURAL)))))))

(sal-env/number-of-type-elements env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER Set) (CONTEXTNAME (IDENTIFIER simple-set) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME CHAR)))))))

(sal-env/number-of-type-elements env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER Set) (CONTEXTNAME (IDENTIFIER simple-set) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME BOOLEAN)))))))

  )



(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'startup))
  (define ctx-decls (sal-context/declarations ctx))
  (for-each (lambda (decl) 
              (sxml/pp (sal/expand-else-command decl)))
            ctx-decls))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-in))
  (define ctx-decls (sal-context/declarations ctx))
  (for-each (lambda (decl) 
              (sxml/pp (sal/expand-else-command decl)))
            ctx-decls))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-scheduler))
  (define ctx-decls (sal-context/declarations ctx))
  (for-each (lambda (decl) 
              (sxml/pp (sal/expand-else-command decl)))
            ctx-decls))


(begin
  (trace/enable! 'alpha-equivalence)
  (trace/enable! 'type-checker)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic)))

(begin
  (trace/enable! 'context)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'mutex))
  (for-each sxml/pp (sal-context/declarations ctx)))

(begin
  (trace/enable! 'evaluator)
  (define env (make-sal-environment))
  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER val) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))))


(begin
;;  (trace/enable! 'evaluator)
;;  (trace/enable! 'type-checker)
;;  (trace/enable! 'context)
;;  (trace/enable! 'number-of-type-elements)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
;;  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER f1) (CONTEXTNAME (IDENTIFIER simple))))))
;;  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER val1d) (CONTEXTNAME (IDENTIFIER simple))))))
  

  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER r2) (CONTEXTNAME (IDENTIFIER simple))))))
  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER r) (CONTEXTNAME (IDENTIFIER simple))))))
  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER val1d) (CONTEXTNAME (IDENTIFIER simple))))))
  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER f1) (CONTEXTNAME (IDENTIFIER simple))))))
  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER val) (CONTEXTNAME (IDENTIFIER tst-pcp-generic))))))
  (sxml/pp (sal-env/evaluate env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER val2) (CONTEXTNAME (IDENTIFIER tst-pcp-generic))))))
  )


(begin
;;  (trace/enable! 'evaluator)
;;  (trace/enable! 'type-checker)
;;  (trace/enable! 'context)
;;  (trace/enable! 'number-of-type-elements)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (lsal/pp
   (sal/insert-implicit-assignments 
    env 
    (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER writer) (CONTEXTNAME (IDENTIFIER four-slot))))))
   200))


(begin
  (trace/enable! 'assignment-tracking)
  (trace/enable! 'implicit-assignments)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (sxml/pp
   (sal/insert-implicit-assignments 
    env 
    (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER writer) (CONTEXTNAME (IDENTIFIER four-slot))))))))


(begin
  (trace/enable! 'type-checker)
  (trace/enable! 'assignment-tracking)
  (trace/enable! 'implicit-assignments)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (lsal/pp
   (sal/insert-implicit-assignments 
    env 
    (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER arbiter) (CONTEXTNAME (IDENTIFIER arbiter))))))
   200))


(begin
  (trace/enable! 'expand-for-all-definition)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (lsal/pp
   (sal/expand-for-all-definitions
    env
    (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER tst) (CONTEXTNAME (IDENTIFIER for-all-tst2))))))
   200))



(begin
  (trace/enable! 'assignment-tracking)
  (trace/enable! 'implicit-assignments)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system) (CONTEXTNAME (IDENTIFIER four-slot)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (lsal/pp
        (sal-module-expander/expand-module expander mod)
        200)))))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system) (CONTEXTNAME (IDENTIFIER mutex)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (lsal/pp
        (sal-module-expander/expand-module expander mod)
        :demangle #t :interface #t)))))



(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (sal-module-expander/set-expand-multi-synch-composition! expander #t)
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER n-arbiter) (CONTEXTNAME (IDENTIFIER arbiter)
                                                                                                                               (ACTUALPARAMETERS
                                                                                                                                (ACTUALEXPRS
                                                                                                                                 (NUMERAL "10")))))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (lsal/pp
        (sal-module-expander/expand-module expander mod)
        '(demangle . #t)
        '(interface . #t)
        )))))



(begin
  (trace/enable! 'expand-modules)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  ;; (sal-module-expander/set-expand-multi-asynch-composition! expander #f)
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER tst-system) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (lsal/pp
         (sal-module-expander/expand-module expander mod)
         '(demangle . #t)
         '(interface . #t))))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system) (CONTEXTNAME (IDENTIFIER mutex)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (lsal/pp
        (sal-module-expander/expand-module expander mod)
        '(demangle . #t))))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system) (CONTEXTNAME (IDENTIFIER mutex)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (sal-transition-system/pp
        (make-sal-boolean-transition-system env
                                            (make-sal-transition-system env (sal-module-expander/expand-module expander mod))))))))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system) (CONTEXTNAME (IDENTIFIER four-slot)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (sal-transition-system/pp
        (make-sal-transition-system env (sal-module-expander/expand-module expander mod)))))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER n-arbiter) (CONTEXTNAME (IDENTIFIER arbiter)
                                                                                                                               (ACTUALPARAMETERS
                                                                                                                                (ACTUALEXPRS
                                                                                                                                 (NUMERAL "10")))))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (sal-transition-system/pp
        (make-sal-transition-system env (sal-module-expander/expand-module expander mod)))))))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  ;; (sal-module-expander/set-expand-multi-asynch-composition! expander #f)
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER tst-system) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (sal-transition-system/pp
        (make-sal-transition-system env (sal-module-expander/expand-module expander mod)))))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define manager (make-sal-finite-type-manager env))
  ;; (sal-module-expander/set-expand-multi-asynch-composition! expander #f)
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER tst-type) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))))
    (sxml/pp decl)
    (sxml/match-or-fail decl
      ((TYPEDECLARATION ?- ?body)
       (let ((result (sal-finite-type-manager/process manager body)))
         (sal-finite-type/pp result)
         (print "Number of bits = " (sal-finite-type/number-of-bits result))
         (print "Number of elements = " (mpq->string (sal-finite-type/number-of-elements result))))))))
    

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define manager (make-sal-finite-type-manager env))
  (define bit-manager (make-bit-manager))
  ;; (sal-module-expander/set-expand-multi-asynch-composition! expander #f)
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER tst-type) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))))
    (sxml/pp decl)
    (sxml/match-or-fail decl
      ((TYPEDECLARATION ?- ?body)
       (let ((result (sal-finite-type-manager/process manager body))
             (bv (bit-manager/make-bit-vector-var bit-manager 18)))
         (sal-finite-type/pp result)
         (print "Number of bits = " (sal-finite-type/number-of-bits result))
         (print "Number of elements = " (mpq->string (sal-finite-type/number-of-elements result)))
         (bit-vector/pp bv)
         (sxml/pp (sal-finite-type/membership-expression result bv))
         

         )))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define manager (make-sal-finite-type-manager env))
  (define bit-manager (make-bit-manager))
  ;; (sal-module-expander/set-expand-multi-asynch-composition! expander #f)
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER R) (CONTEXTNAME (IDENTIFIER type)))))))
    (sxml/pp decl)
    (sxml/match-or-fail decl
      ((TYPEDECLARATION ?- ?body)
       (let ((result (sal-finite-type-manager/process manager body))
             (bv27 (bit-manager/make-bit-vector-var bit-manager 27)))
         (sal-finite-type/pp result)
         (bit-vector/pp bv27)
         ;; (sxml/pp (sal-finite-expr-info/type-membership result bv27))
         )))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define it (sal-env/make-type-iterator env (template->sxml (QUALIFIEDTYPENAME (IDENTIFIER S2) (CONTEXTNAME (IDENTIFIER type))))))
  (iterator/for-each sxml/pp it))

(define (convert-to-bv sxml . env)
  (trace/enable! 'finite-expr)
  (let ((sal-env (make-sal-environment)))
    (sal-env/set-salpath! sal-env '("."))
    (let ((manager (make-simple-sal-finite-expr-manager sal-env))
          (empty-symbol-table (if (null? env) (make-symbol-table) (car env))))
      (sxml/pp
       (sal-finite-expr-info/bit-vector
        (sal-finite-expr-manager/process manager empty-symbol-table sxml)))
      (print "auxiliary equations: ")
      (for-each sxml/pp (sal-finite-expr-manager/auxiliary-equation-list manager))
      (print "validity conditions: ")
      (for-each sxml/pp (sal-finite-expr-manager/valid-expr-list manager))
      )))

(convert-to-bv (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER a) (CONTEXTNAME (IDENTIFIER type)))))
(convert-to-bv (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER id2) (CONTEXTNAME (IDENTIFIER type)))))
(convert-to-bv (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER id4) (CONTEXTNAME (IDENTIFIER type)))))
(convert-to-bv (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER id5) (CONTEXTNAME (IDENTIFIER type)))))
         
(convert-to-bv (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER r) (CONTEXTNAME (IDENTIFIER type)))))

(convert-to-bv (template->sxml (RECORDSELECTION
                                (QUALIFIEDNAMEEXPR (IDENTIFIER r) (CONTEXTNAME (IDENTIFIER type)))
                                (IDENTIFIER 'flag))))

(convert-to-bv (template->sxml (TUPLESELECTION
                                (RECORDSELECTION
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER r) (CONTEXTNAME (IDENTIFIER type)))
                                 (IDENTIFIER 'idx))
                                (NUMERAL "3"))))

(convert-to-bv (template->sxml (TUPLESELECTION
                                (RECORDSELECTION
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER r) (CONTEXTNAME (IDENTIFIER type)))
                                 (IDENTIFIER 'idx))
                                (NUMERAL "2"))))

(convert-to-bv (template->sxml (ARRAYSELECTION
                                (RECORDSELECTION
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER r) (CONTEXTNAME (IDENTIFIER type)))
                                 (IDENTIFIER 'arr))
                                (NUMERAL "5"))))

(convert-to-bv (template->sxml (TUPLESELECTION
                                (LETEXPRESSION
                                 (LETDECLARATIONS
                                  (LETDECLARATION (IDENTIFIER x) (SUBRANGE (NUMERAL "1") (NUMERAL "10")) (NUMERAL "3"))
                                  (LETDECLARATION (IDENTIFIER y) (TYPENAME BOOLEAN) (NAMEEXPR TRUE)))
                                 (CONDITIONAL (NAMEEXPR y)
                                              (TUPLELITERAL (NAMEEXPR x)  (NAMEEXPR x))
                                              (TUPLELITERAL (NUMERAL "0") (NUMERAL "1"))))
                                (NUMERAL "1"))))

         
(convert-to-bv (let ((c (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER my-array) (CONTEXTNAME (IDENTIFIER type))))))
                 (template->sxml (UPDATEEXPRESSION ,c
                                                   (ARRAYSELECTION
                                                    ,c
                                                    (NUMERAL "9"))
                                                   (NUMERAL "255")))))


(convert-to-bv (let ((c (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER my-array) (CONTEXTNAME (IDENTIFIER type))))))
                 (template->sxml (UPDATEEXPRESSION ,c
                                                   (ARRAYSELECTION
                                                    ,c
                                                    (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type))))
                                                   (NUMERAL "0")))))


(convert-to-bv (template->sxml 
                (ARRAYSELECTION
                 (QUALIFIEDNAMEEXPR (IDENTIFIER my-array) (CONTEXTNAME (IDENTIFIER type)))
                 (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type))))))


(convert-to-bv (template->sxml (TUPLELITERAL (NUMERAL "4") (NUMERAL "3"))))

(convert-to-bv (template->sxml (UPDATEEXPRESSION
                                (TUPLELITERAL (NUMERAL "4") (NUMERAL "3"))
                                (TUPLESELECTION
                                 (TUPLELITERAL (NUMERAL "4") (NUMERAL "3"))
                                 (NUMERAL "1"))
                                (NUMERAL "18"))))

(convert-to-bv (template->sxml (RECORDLITERAL
                                (RECORDENTRY (IDENTIFIER id)
                                             (TUPLELITERAL (NUMERAL "1") (NUMERAL "2")))
                                (RECORDENTRY (IDENTIFIER flag)
                                             (NAMEEXPR FALSE)))))


(convert-to-bv (let ((c (template->sxml (RECORDLITERAL
                                         (RECORDENTRY (IDENTIFIER id)
                                                      (TUPLELITERAL (NUMERAL "1") (NUMERAL "2")))
                                         (RECORDENTRY (IDENTIFIER flag)
                                                      (NAMEEXPR FALSE))))))
                 (template->sxml (UPDATEEXPRESSION ,c
                                                   (TUPLESELECTION 
                                                    (RECORDSELECTION 
                                                     ,c
                                                     (IDENTIFIER id))
                                                    (NUMERAL "2"))
                                                   (NUMERAL "20")))))

(convert-to-bv (template->sxml (FORALLEXPRESSION (VARDECLS (VARDECL (IDENTIFIER a) (TYPENAME BOOLEAN))) (NAMEEXPR a))))

(convert-to-bv (template->sxml (APPLICATION
                                (QUALIFIEDNAMEEXPR (IDENTIFIER msg) (CONTEXTNAME (IDENTIFIER type)))
                                (TUPLELITERAL
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER id4) (CONTEXTNAME (IDENTIFIER type)))
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type)))))))

(convert-to-bv (template->sxml (APPLICATION
                                (QUALIFIEDNAMEEXPR (IDENTIFIER msg2) (CONTEXTNAME (IDENTIFIER type)))
                                (TUPLELITERAL
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER id4) (CONTEXTNAME (IDENTIFIER type)))
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type)))))))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (sxml/pp
   (sal-env/evaluate env (template->sxml (APPLICATION
                                          (QUALIFIEDNAMEEXPR (IDENTIFIER union) (CONTEXTNAME (IDENTIFIER simple-set) (ACTUALPARAMETERS (ACTUALTYPES (TYPENAME BOOLEAN)))))
                                          (TUPLELITERAL
                                           (NAMEEXPR aaaa)
                                           (NAMEEXPR bbbb)))))))



(convert-to-bv (template->sxml (EXISTSEXPRESSION 
                                (VARDECLS (VARDECL (IDENTIFIER a) 
                                                   (QUALIFIEDTYPENAME (IDENTIFIER S1) (CONTEXTNAME (IDENTIFIER type)))))
                                (APPLICATION 
                                 (NAMEEXPR =)
                                 (TUPLELITERAL
                                  (NAMEEXPR a)
                                  (QUALIFIEDNAMEEXPR (IDENTIFIER pc1) (CONTEXTNAME (IDENTIFIER type))))))))


(convert-to-bv (template->sxml (EXISTSEXPRESSION 
                                (VARDECLS (VARDECL (IDENTIFIER a) 
                                                   (SUBRANGE (NUMERAL "1") (NUMERAL "8"))))
                                (APPLICATION 
                                 (NAMEEXPR =)
                                 (TUPLELITERAL
                                  (NAMEEXPR a)
                                  (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type))))))))

(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (sxml/pp (sal-env/accessor-constructor env (template->sxml (QUALIFIEDNAMEEXPR (IDENTIFIER msg2-id) (CONTEXTNAME (IDENTIFIER type)))))))


(convert-to-bv (template->sxml (APPLICATION
                                (QUALIFIEDNAMEEXPR (IDENTIFIER msg2) (CONTEXTNAME (IDENTIFIER type)))
                                (TUPLELITERAL
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER id4) (CONTEXTNAME (IDENTIFIER type)))
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type)))))))

(convert-to-bv (template->sxml (APPLICATION
                                (QUALIFIEDNAMEEXPR (IDENTIFIER msg2-id) (CONTEXTNAME (IDENTIFIER type)))
                                (APPLICATION
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER msg2) (CONTEXTNAME (IDENTIFIER type)))
                                 (TUPLELITERAL
                                  (QUALIFIEDNAMEEXPR (IDENTIFIER id4) (CONTEXTNAME (IDENTIFIER type)))
                                  (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type))))))))
               
(convert-to-bv (template->sxml (APPLICATION
                                (NAMEEXPR -)
                                (TUPLELITERAL
                                 (QUALIFIEDNAMEEXPR (IDENTIFIER b) (CONTEXTNAME (IDENTIFIER type)))
                                 (NUMERAL "15")))))


(begin
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER system) (CONTEXTNAME (IDENTIFIER four-slot)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (sal-transition-system/pp
        (make-sal-boolean-transition-system env
                                            (make-sal-transition-system env (sal-module-expander/expand-module expander mod))))))))

(begin
;;  (trace/enable! 'finite-type)
;;  (trace/enable! 'module-instance)
;;  (trace/enable! 'context)
  (define env (make-sal-environment))
  (sal-env/set-salpath! env '("."))
  (define expander (make-sal-module-expander env))
  (let ((decl (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER tst-system) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))))
    (sxml/match-or-fail decl
      ((MODULEDECLARATION ?- ?- ?mod)
       (sal-transition-system/pp
        (make-sal-boolean-transition-system env
                                            (make-sal-transition-system env (sal-module-expander/expand-module expander mod))))))))




(sxml/match-or-fail (sal-env/qualified-name-declaration env (template->sxml (QUALIFIEDMODULENAME (IDENTIFIER tst-system) (CONTEXTNAME (IDENTIFIER tst-pcp-generic)))))
  ((MODULEDECLARATION ?- ?- ?mod)
   (display-circle (sal-module-expander/expand-module expander mod))))



(begin
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'simple-set))
  (sal-ast/pp ctx))

(define-method (sal-ast->list :around (self <sal-ast>))
  (print (class-name-of (class-of self)))
  (call-next-method))

(sal-ast/pp ctx)


(define it (sal-type/make-iterator (make-ast-instance <sal-function-type> *sal-boolean-type*
                                                      :arg-type *sal-boolean-type*
                                                      :return-type *sal-boolean-type*)
                                   identity))
(for-each sal-ast/pp (iterator->list it))

(define it (sal-type/make-iterator (make-ast-instance <sal-function-type> *sal-boolean-type*
                                                      :arg-type (make-ast-instance <sal-tuple-type> *sal-boolean-type*
                                                                                   :types (list *sal-boolean-type* *sal-boolean-type*))
                                                      :return-type *sal-boolean-type*)
                                   identity))
(for-each sal-ast/pp (iterator->list it))


(begin
  (trace/enable! 'sal-module)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'mutex))
  (sal-ast/pp ctx))

(define-method (sal-ast/copy-using :around (self <sal-ast>) (env <primitive>) (proc <primitive>))
  (print (class-name-of (class-of self)))
  (call-next-method))

(define-method (sal-module/update-interface :around (self <sal-ast>))
  (print (class-name-of (class-of self)))
  (call-next-method))

(define-method (sal-ast->list :around (self <sal-ast>))
  (print (class-name-of (class-of self)))
  (call-next-method))


(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'expr)
  (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'four-slot))
  (sal-ast/pp ctx))

(define n 1)

(define-method (sal-type-check :around (self <sal-ast>) (info <sal-type-checker-info>))
  (print (make-string n) (sal-ast->list self))
  (print (make-string n) "+++" (class-name-of (class-of self)) " " (format-with-location self ""))
  (set! n (+ n 4))
  (call-next-method)
  (set! n (- n 4))
  (print (make-string n) "---" (class-name-of (class-of self))))

(define-method (sal-expr/update-type! :around (self <primitive>))
  (print (make-string n) (sal-ast->list self))
  (print (make-string n) "+++" (class-name-of (class-of self)) " " (format-with-location self ""))
  (set! n (+ n 4))
  (call-next-method)
  (set! n (- n 4))
  (print (make-string n) "---" (class-name-of (class-of self))))

(define n 0)   
(define-method (sal-ast/equivalent-core? :around (type1 <sal-type>) (type2 <sal-type>) (env <primitive>))
  (print (make-string n) "+++" (class-name-of (class-of type1)) ", " ;;  " at " (format-with-location type1 "") ", "
         (class-name-of (class-of type2))) ;;" at " (format-with-location type2 "") ", "
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "--- result: " result)
    result))

(define-method (sal-ast/equivalent-core? :around (type1 <sal-type>) (type2 <sal-type>) (env <primitive>))
  (print (make-string n) "+++" (sal-ast->list type1) ", " (sal-ast->list  type2))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "--- result: " result)
    result))

(define-method (sal-ast/equivalent? :around (type1 <sal-type-name>) (type2 <sal-type-name>) (env <primitive>))
  (print (sal-name-ref/name type1) ", " (sal-name-ref/name type2))
  (call-next-method))

(begin
  (trace/enable! 'sal-module)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'arbiter))
  (sal-ast/pp ctx))


(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'context)
  (trace/enable! 'importer)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic))
  (sal-ast/pp ctx))

(define n 0)
(define-method (sal-ast/instantiate-with :around (self <sal-ast>) (env <primitive>) (info <sal-instantiation-info>))
  (print (make-string n) "+++" (instance-class-name self) " " (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---" (instance-class-name self))
    result))



  (define ctx (sal-env/context env 'pcp-scheduler))
  (sal-ast/pp ctx))

(define-method (sal-builtin-constant-decl/update-application-type! (self <sal-builtin-constant-decl>) (app <sal-application>))
  (sal-ast/pp self)
  (call-next-method))

(print n 0)

(define-method (sal-expr/simplify-core (self <sal-ast>) (env <primitive>))
  (sal-ast/pp self)
  (print (make-string n) "+++" (class-name-of (class-of self)) " " (format-with-location self ""))
  (set! n (+ n 4))
  (call-next-method)
  (set! n (- n 4))
  (print (make-string n) "---" (class-name-of (class-of self))))

(define-method (sal-type-name/definition :around (self <sal-ast>))
  (print "+++" (class-name-of (class-of self)) " " (format-with-location self ""))
  (call-next-method))

(define n 0)

(define-method (sal-ast/equivalent? :around (type1 <sal-ast>) (type2 <sal-ast>) (env <primitive>))
  (print (make-string n) (sal-ast->list type1) " --- " (sal-ast->list type2))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
  (set! n (- n 4))
  (print (make-string n) "------ result = " result)
  result))

(define-method (sal-ast/equivalent? :around (type1 <sal-ast>) (type2 <sal-ast>) (env <primitive>))
  (call-next-method))

(define-method (sal-ast/equivalent? :around (type1 <sal-binary-op>) (type2 <sal-binary-op>) (env <primitive>))
  (print (make-string n) (sal-ast->list type1) " --- " (sal-ast->list type2))
  (set! n (+ n 4))
  (print "slot1 eq? = " (sal-ast/equivalent? (slot-value type1 :arg1) (slot-value type2 :arg1) env))
  (print "slot2 eq? = " (sal-ast/equivalent? (slot-value type1 :arg2) (slot-value type2 :arg2) env))
  (let ((result (call-next-method)))
  (set! n (- n 4))
  (print (make-string n) "------ result = " result)
  result))

  
(define-method (sal-type/super-type :around (self <sal-ast>))
  (print (make-string n) (sal-ast->list self))
  (set! n (+ n 4))
  (call-next-method)  
  (set! n (- n 4))
  (print (make-string n) "-----------------"))

(define-method (sal-ast->list :around (self <sal-identifier>))
  (list 'identifier (call-next-method)))

(define-method (sal-ast->list :around (self <sal-name-expr>))
  (list 'name-expr (call-next-method)))

(define-method (sal-ast->list :around (self <sal-subrange>))
  (sal-ast->list (change-class self <sal-bounded-subtype>)))

(define-method (sal-type/super-type :around (self <sal-subtype>))
  (print (make-string n) (sal-ast->list self))
  (print "expr: " (sal-ast->list (slot-value self :expr)))
  (print "expr type: " (sal-ast->list (sal-expr/type (slot-value self :expr))))
  (call-next-method))


(define-method (sal-type-check :around (self <sal-ast>) (info <sal-type-checker-info>))
  (pp (instance-class-name self))
  (call-next-method))

(define-method (sal-type/combine :around (type1 <primitive>) (type2 <primitive>) (union? <primitive>))
  (print (make-string n) (sal-ast->list type1) " --- " (sal-ast->list type2))
  (set! n (+ n 4))
  (when (next-method?)
    (call-next-method))
  (set! n (- n 4))
  (print (make-string n) "-------------"))

  
(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'expr)
  (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'mutex))
  (sal-ast/pp ctx))

(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'expr)
  (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'four-slot))
  (sal-ast/pp ctx))

(begin
  ;; (trace/enable! 'sal-module)
  ;; (trace/enable! 'expr)
  ;; (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'arbiter))
  (sal-ast/pp ctx))

(begin
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'abro))
  (sal-ast/pp ctx))

(begin
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'abro)))

(begin
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'arbiter2))
  (sal-ast/pp ctx))

(begin
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'scr001))
  (sal-ast/pp ctx))

(define-method (sal-ast->lsal-doc :around (self <primitive>) (pp-info <pp-info++>) (depth <primitive>))
  (unless (instance-of? self <sal-ast>)
    (display-circle self)
    (print ""))
  (print "processing ... " (instance-class-name self))
  (if (> depth (slot-value pp-info :max-depth))
    "..."
    (call-next-method)))



(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'expr)
  (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'simple-set))
  (sal-ast/pp ctx))

(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'expr)
  (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'math))
  (sal-ast/pp ctx))

(begin
  ;; (trace/enable! 'sal-module)
  ;; (trace/enable! 'expr)
  (trace/enable! 'expr)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-task))
  (sal-ast/pp ctx))

(define-method (sal-ast->list :around (self <sal-subrange>))
  (sal-ast->list (change-class self <sal-bounded-subtype>)))

(define-method (sal-ast->list :around (self <sal-ast>))
  (list (sal-ast/context-name self) (instance-class-name self) (call-next-method)))


(define n 0)

(define-method (sal-ast/instantiate-with :around (self <sal-var-param-name-expr>) (env <primitive>) (info <sal-instantiation-info>))
  (trace 'instantiate "~a instantiating var-param = ~a" (make-string n) (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (trace 'instantiate "~a----------" (make-string n))
    result))

(define-method (sal-ast/instantiate-with :around (self <sal-bounded-subtype>) (env <primitive>) (info <sal-instantiation-info>))
  (let ((result (shallow-copy self)))
    (print "before : "  (sal-ast->list result))
    (set-slot-value! result :expr (sal-ast/instantiate-with (slot-value result :expr) env info))
    (print "before2 : "  (sal-ast->list result))
    (set-slot-value! result :lower (sal-ast/instantiate-with (slot-value result :lower) env info))
    (print "before3 : "  (sal-ast->list result))
    (set-slot-value! result :upper (sal-ast/instantiate-with (slot-value result :upper) env info))
    (print "final : "  (sal-ast->list result))
    result))

(define-method (sal-ast/instantiate-with :around (self <sal-ast>) (env <primitive>) (info <sal-instantiation-info>))
  (trace 'instantiate "~a instantiating = ~a" (make-string n) (sal-ast->list self))
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (trace 'instantiate "~a ---------- result = ~a" (make-string n) (sal-ast->list result))
    result))


(begin
;;  (trace/enable! 'sal-module)
;;  (trace/enable! 'expr)
;;  (trace/enable! 'type-checker)
  (trace/enable! 'instantiate)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-rsrc))
  (sal-ast/pp ctx))

(begin
  (trace/enable! 'sal-module)
  (trace/enable! 'expr)
  (trace/enable! 'type)
  (trace/enable! 'type-checker)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'pcp-scheduler))
  (sal-ast/pp ctx))

(define-method (sal-ast->list :around (self <sal-ast>))
  (list (sal-ast/context-name self) (instance-class-name self) (call-next-method)))

(define-method (sal-ast->list :around (self <sal-ast>))
  (list (instance-class-name self) (call-next-method)))

(begin
  (trace/enable! 'type-checker)
  (trace/enable! 'type)
;;  (trace/enable! 'module)
;;  (trace/enable! 'instantiate)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst-pcp-generic))
  (sal-ast/pp ctx))

(define-method (sal-module-name/definition :around (self <sal-qualified-module-name>))
  



(begin
  (trace/enable! 'ls-parser)
  (define env (make-sal-env))
  (define prelude (sal-env/prelude env))
  (sal-ast/pp prelude)
  (define ctx (sal-env/context env 'pcp-task))
  (sal-ast/pp ctx)
  
  (define nat (sal-env/builtin-type-name env 'nat))
  (sal-ast/pp (sal-subtype/normalize (sal-type-name/definition nat))))


(define n 0)
(define-method (sal-ast->list :around (self <sal-ast>))
  (print (make-string n) "+++" (instance-class-name self))
  (set! n (+ n 4))
  (call-next-method)
  (set! n (- n 4))
  (print (make-string n) "---" (instance-class-name self)))


(define-method (sal-ast/instantiate-with :around (self <sal-internal-name-ref>) (env <primitive>) (info <sal-instantiation-info>))
  (let ((result (call-next-method)))
    (print "instantiating internal name " (sal-ast->list self) " ---- " (sal-ast->list result))
    result))
  
(define-method (sal-ast->list :around (self <sal-expr>))
  (if (slot-value self :type)
    (list 'typed-expr (call-next-method) (sal-ast->list (slot-value self :type)))
    (call-next-method)))

(define-method (sal-module-name/definition :around (self <sal-module-name>))
  (print "Computing module definition of " (sal-ast->list self))
  (call-next-method))


(begin
  (trace/enable! 'module)
  (trace/enable! 'instantiate)
  (define env (make-sal-env))
  (sal-env/set-salpath! env '("."))
  (define ctx (sal-env/context env 'tst))
  (define m (sal-context/module-declaration ctx 'm))
  (define body (slot-value m :parametric-module))
  (define instance (sal-ast/instantiate body '() (list (sal-env/expr-string->ast env "3"))))
  (sal-ast/pp instance)
  )

(define-method (sal-ast/copy-using :around (self <sal-base-module>) (env <primitive>) (proc <primitive>))
  (print "copying base module...")
  (print "before = " (sal-ast->list self))
  (let ((result (call-next-method)))
    (print "after = " (sal-ast->list result))
    result))

(define-method (sal-ast/instantiate-with :around (self <sal-var-param-name-expr>) (env <primitive>) (info <sal-instantiation-info>))
  (print "instantiating var param...")
  (let ((result (call-next-method)))
    (sal-ast/pp result)
    result))

(define-method (sal-ast/copy-using :around (self <sal-parametric-module>) (env <primitive>) (proc <primitive>))
  (let ((result (call-next-method)))
    result))
    
(define-method (sal-ast/copy-using :around (self <sal-simple-definition>) (env <primitive>) (proc <primitive>))
  (print "copying simple definition...")
  (print "before = " (sal-ast->list self))
  (let ((result (call-next-method)))
    (print "after = " (sal-ast->list result))
    result))

(define n 0)


(define-method (sal-expr/simplify-core :around (self <sal-ast>) (env <primitive>))
  (print (make-string n) "+++" (instance-class-name self) " " (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---" (instance-class-name self) " " (sal-ast->list result))
    result))

(define-method (sal-ast->lsal-doc :around (self <primitive>) (pp-info <pp-info++>) (depth <primitive>))
  (print (make-string n) "+++" (instance-class-name self) " " (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (if (> depth (slot-value pp-info :max-depth))
                  "..."
                  (call-next-method))))
    (set! n (- n 4))
    (print (make-string n) "---" (instance-class-name self))
    result))
    

(define-method (sal-ast->lsal-doc :around (self <sal-identifier>) (pp-info <pp-info++>) (depth <primitive>))
  (print "processing identifier = " (slot-value self :name) " string? = " (string? (slot-value self :name)))
  (let ((result (call-next-method)))
    
    result))


(begin
  (trace/enable! 'implicit-assignments)
  (sal/reset!)
  (sal/import "implicit")
  (sal-base-module/insert-implicit-assignments 
   (sal-module-instance/expand (sal/module "tst"))))

(begin
  (trace/enable! 'implicit-assignments)
  (sal/reset!)
  (sal/import "four-slot")
  (sal-base-module/insert-implicit-assignments 
   (sal-module-instance/expand (sal/module "writer"))))

(begin
  (trace/enable! 'implicit-assignments)
  (sal/reset!)
  (sal/import "(arbiter () (2))")
  (sal-base-module/insert-implicit-assignments 
   (sal-module-instance/expand (sal/module "(arbiter true)"))))
  

(begin
  (sal/reset!)
  (sal/import "mutex")
  (define x (sal-module/flat (sal/module "system")))
  x)

(begin
  (sal/reset!)
  (sal/import "arbiter2")
  (sal-module/flat (sal/module "system")))

(begin
  (sal/reset!)
  (sal/import "scr001")
  (sal-module/flat (sal/module "system")))

(begin
  (sal/reset!)
  (sal/import "tst-pcp-generic")
  (sal-module/flat (sal/module "tst-system")))
(begin
  ;; (trace/enable! 'expr)
  ;; (trace/enable! 'expand-modules)
  (sal/reset!)
  (sal/import "four-slot")
  (define x (sal-module/flat (sal/module "system")))
  x)

(begin
  ;; (trace/enable! 'expr)
  ;; (trace/enable! 'expand-modules)
  (sal/reset!)
  (sal/import "(arbiter () (10))")
  (define x (sal-module/flat (sal/module "n-arbiter")))
  x)

(define n 0)

(define-method (sal-ast/expand-modules :around (self <sal-ast>) (info <sal-module-expansion-info>) (env <primitive>))
  (print (make-string n) "+++" (instance-class-name self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---" (instance-class-name self))
    result))

(define-method (sal-ast/equivalent? :around (ast1 <sal-name-ref>) (ast2 <sal-name-ref>) (env <primitive>))
  (let ((result (call-next-method)))
    (print "------------------------------")
    (display* (instance-class-name ast1) "   ") (sal-ast/pp ast1)
    (display* (instance-class-name ast2) "   ") (sal-ast/pp ast2)
    (print "result = " result)
    (print "------------------------------")
    result))

(define-method (sal-ast/equivalent? :around (ast1 <sal-name-ref>) (ast2 <sal-name-ref>) (env <primitive>))
  (call-next-method))

(define-method (sal-ast/equivalent-core? :around (ast1 <sal-ast>) (ast2 <sal-ast>) (env <primitive>))
  (let ((result (call-next-method)))
    (print "------------------------------")
    (display* (instance-class-name ast1) "   ") (sal-ast/pp ast1)
    (display* (instance-class-name ast2) "   ") (sal-ast/pp ast2)
    (print "result = " result)
    (print "------------------------------")
    result))


(sal-ast/rename-variables-with-new-names (sal-module/flat (sal/module "(@ system (mutex () ()))")))
(sal-ast/rename-variables-with-local-new-names (sal-module/flat (sal/module "(@ system (mutex () ()))")))

(sal-module/flat (sal/module "(@ system (mutex () ()))"))

(sal-ast/rename-variables-with-local-new-names (sal-module/flat (sal/module "(@ system (four-slot () ()))")))

(sal-ast/rename-variables-with-local-new-names (sal-module/flat (sal/module "(@ n-arbiter (arbiter () (5)))")))

(sal-ast/rename-variables-with-local-new-names (sal-module-instance/expand (sal/module "(@ n-arbiter (arbiter () (5)))")))

(sal-ast/rename-variables-with-local-new-names (sal-ast/deep-copy (slot-value (sal-context/module-declaration (sal/context "arbiter") 'n-arbiter) :parametric-module)))

(sal-ast/rename-variables-with-local-new-names (sal-module/flat (sal/module "(@ tst-system (tst-pcp-generic () ()))")))

(sal-ast/rename-variables-with-local-new-names (sal-module/flat (sal/module "(@ system (mutex2 () ()))")))

(sal/context 'mutex2)

(sal-module/flat (sal/module "(@ system (mutex2 () ()))"))

(define (show-env env)
  (print "Env: ")
  (for-each (lambda (pair)
              (sal/pp (car pair))
              (display " --->  ")
              (sal/pp (cdr pair))
              (print ""))
            env)
  (print "-----------------------"))

(define-method (sal-ast/expand-modules :around (self <sal-module>) (info <sal-module-expansion-info>) (env <primitive>))
  (let ((result (call-next-method)))
    (print "processing module:")
    (print "before: ")
    (sal/pp self)
    (print "")
    (show-env env)
    (print "after: ")
    (sal/pp result)
    (print "\n==========================")
    result))

(sal-ast/rename-variables-with-local-new-names (sal-module/flat (sal/module "(@ n-arbiter (arbiter () (5)))")))
    
    
(define  n 0)   
(define-method (sal-ast/for-each-children :around (self <primitive>) (proc-before <primitive>))
  (print (make-string n) "+++" (instance-class-name self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---" (instance-class-name self))
    result))

(sal-flat-module/collect-dependencies (sal-module/flat (sal/module "(@ system (mutex () ()))")))

(sal-flat-module/collect-dependencies (sal-module/flat (sal/module "(@ system (mutex2 () ()))")))

(sal-flat-module/collect-dependencies (sal-module/flat (sal/module "(@ system (four-slot () ()))")))

(sal-flat-module/collect-dependencies (sal-module/flat (sal/module "(@ n-arbiter (arbiter () (5)))")))

(sal-flat-module/collect-dependencies (sal-module/flat (sal/module "(@ system (abro () ()))")))

(define (show-next-curr-dep flat-module)
  (let ((dep (sal-flat-module/collect-dependencies flat-module)))
    (sal-dependencies/closure! dep)
    (sal-dependencies/next-curr-dependencies dep)))

(show-next-curr-dep (sal-module/flat (sal/module "(@ n-arbiter (arbiter () (5)))")))

(show-next-curr-dep (sal-module/flat (sal/module "(@ system (abro () ()))")))

(show-next-curr-dep (sal-module/flat (sal/module "(@ system (arbiter2 () ()))")))

(sal-flat-module/next-vars (sal-module/flat (sal/module "(@ system (abro () ()))")))

(sal-dependencies/closure (sal-flat-module/collect-dependencies (sal-module/flat (sal/module "(@ n-arbiter (arbiter () (5)))"))))


(define  n 0)   
(define-method (sal-expr->dnf :around (self <primitive>))
  (print (make-string n) "+++" (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---" (sal-ast->list result))
    result))


(define  n 0)   
(define-method (sal-ast/expand-modules :around (self <primitive>) (info <sal-module-expansion-info>) (env <primitive>))
  (print (make-string n) "+++" (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---" (sal-ast->list result))
    result))

(define-method (sal-expr/evaluate-core :around (self <sal-ast>) (env <primitive>) (depth <primitive>))
  (print (make-string n) "+++v" (sal-ast->list self))
  (set! n (+ n 4))
  (let ((result (call-next-method)))
    (set! n (- n 4))
    (print (make-string n) "---v" (sal-ast->list result))
    result))

(begin
  (sal/reset!)
  (sal-env/disable-type-checker! *sal-env*)
  (define f1 (sal-module/flat (sal/module "(@ tst-system (tst-pcp-generic () ()))")))
  (define trans (slot-value f1 :transition))
  (define ex1 (cadddr (slot-value trans :exprs)))
  (define info (make-expr-expansion-info))
;;  (sal-expr/expand-core trans info (make-empty-env))
  (sal-ast/rename-variables-with-local-new-names (sal-expr/expand-core trans info (make-empty-env)))
  )
(define-method (sal-expr/expand-core :around (self <sal-ast>) (info <sal-expr-expansion-info>) (env <primitive>))
  (call-next-method))

(define aux #unspecified)

(define n 0)
(define-method (sal-ast/expand-core :around (self <sal-ast>) (env <primitive>) (depth <primitive>))
  (print (make-string n) "+++x" (sal-ast->list self))
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---x" (sal-ast->list result))
    result))

(define-method (sal-expr/lift :around (self <sal-ast>) (arg <sal-expr>) (info <sal-expr-expansion-info>) (env <primitive>))
  (call-next-method))

(define-method (sal-expr/lift :around (self <sal-name-expr>) (arg <sal-expr>) (info <sal-expr-expansion-info>) (env <primitive>))
  (when (eq? (sal-name-ref/name self) 'fun)
    (print "FOUND fun:")
    [assert (self arg info env) #f])
  (call-next-method))

(define-method (sal-expr/lift :around (self <sal-ast>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (print (make-string n) "+++l" (sal-ast->list self))
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---l" (sal-ast->list result))
    result))

(define-method (sal-ast/equivalent-core? :around (self <sal-ast>) (arg <sal-ast>) (env <primitive>))
  (print (make-string n) "+++l" (sal-ast->list self))
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---l" (sal-ast->list result))
    result))


(define-method (sal-ast->list :around (self <sal-qualified-name-ref>))
  (sal-name-ref/name self))

(define-method (sal-expr/evaluate-core :around (self <sal-name-expr>) (env <primitive>) (depth <primitive>))
  ;; (breakpoint "evaluate-core" (self env depth) (not (instance-of? (slot-value self :decl) <sal-decl>)))
  (call-next-method))

(define-method (sal-expr/expand-core :around (self <sal-conditional>) (info <sal-expr-expansion-info>) (env <primitive>))
  (print (make-string n) "+++l" (sal-ast->list self))
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---l" (sal-ast->list result))
    result))


(let ((new-cond (try-to-evaluate (slot-value self :cond-expr) env)))
    (cond
     ((sal-expr/true-constant? new-cond)
      (sal-expr/expand-core (slot-value self :then-expr) info env))
     ((sal-expr/false-constant? new-cond)
      (sal-expr/expand-core (slot-value self :else-expr) info env))
     (else
      (copy-instance self
                     :cond-expr (sal-expr/expand-core (slot-value self :cond-expr) info env)
                     :then-expr (sal-expr/expand-core (slot-value self :then-expr) info env)
                     :else-expr (sal-expr/expand-core (slot-value self :else-expr) info env)))))


(define-method (sal-expr/expand-core (self <sal-conditional>) (info <sal-expr-expansion-info>) (env <primitive>))
  (let ((new-cond (try-to-evaluate (slot-value self :cond-expr) env)))
    ;; (breakpoint "conditional" (self info env new-cond) #t)
    (cond
     ((sal-expr/true-constant? new-cond)
      (sal-expr/expand-core (slot-value self :then-expr) info env))
     ((sal-expr/false-constant? new-cond)
      (sal-expr/expand-core (slot-value self :else-expr) info env))
     (else
      (copy-instance self
                     :cond-expr (sal-expr/expand-core (slot-value self :cond-expr) info env)
                     :then-expr (sal-expr/expand-core (slot-value self :then-expr) info env)
                     :else-expr (sal-expr/expand-core (slot-value self :else-expr) info env))))))


(define-macro (trace fun before after)
  (let ((n (gensym))
        (old-fun (gensym)))
    `(begin
       (define ,n 0)
       (define ,old-fun ,fun)
       (define (,fun . %args)
         (print (make-string ,n) "calling " (quote ,fun) " with " (map instance-class-name %args))
         (apply ,before ,n %args)
         (set! ,n (+ ,n 2))
         (let ((%result (apply ,old-fun %args)))
           (set! ,n (- ,n 2))
           (print (make-string ,n) "returning from " (quote ,fun) " with " (map instance-class-name %args))
           (,after ,n %result)
           %result)))))

(define (dummy . args) #unspecified)

(define-macro (simple-trace fun)
  `(trace ,fun dummy dummy))

(simple-trace sal-expr/expand-core)



(define it (apply iterator/product 
             (lambda elems
               (display "elem = ") (sal/pp elems) (print "")
               (make-ast-instance <sal-application>
                                  :fun constructor-name
                                  :arg (make-application-argument elems)))
             accessor-type-list))

(begin
  (sal/reset!)
  (define type1 (sal/type "(subrange 1 4)"))
  (define type2 (sal/type "(subrange 1 6)"))
  (sal/import "bool-vars")
  (define x (sal/expr "x"))
  (define y (sal/expr "y"))
  (define z (sal/expr "z"))
  (define true (sal/expr "true"))
  (define false (sal/expr "false"))
  (define bl1 (list x false y))
  (define bl2 (list z false))
  (define data1 (make-sal-finite-data bl1 type1))
  (define data2 (make-sal-finite-data bl2 type2))
  (sal-le/gen-circuit-core data1 data2))


(begin
  (define m (sal/module "(@ system four-slot)"))
  (define f1 (sal-module/flat m))
  (define f2 (sal-ast/expand f1))
  (define f3 (sal-ast/expand-quantifiers f2))
  (sal-ast->boolean-ast f3))


(sal-expr->boolean-expr-core (sal/expr "(lambda (y::(subrange 1 3) x::bool) (if x (+ y 1) y))") (make-empty-env))
(sal-expr->boolean-expr-core (sal/expr "(lambda (y::(subrange 1 3)) (+ y 1))") (make-empty-env))

(sal-expr->boolean-expr-core (sal/expr "(+ 0 0)") (make-empty-env))

(begin
  (define m (sal/module "(@ n-arbiter (arbiter () (10)))"))
  (define f1 (sal-module/flat m))
  (define f2 (sal-ast/expand f1))
  (define f3 (sal-ast/expand-quantifiers f2))
  (define f4 (sal-ast->boolean-ast f3)))

(begin
  (define m (sal/module "(@ system four-slot)"))
  (define f1 (sal-module/flat m))
  (define f2 (sal-ast/expand f1))
  (define f3 (sal-ast/expand-quantifiers f2))
  (sal-ast->boolean-ast f3))


(define *sal-parser-tmp-file* "__sal_parser_tmp_file__.sal")
(define *sal-parser-tmp-result-file* "__sal_parser_tmp_file_result__.sal")

(define (sal/delete-tmp-files!)
  (system (string-append "rm -f " *sal-parser-tmp-file*))
  (system (string-append "rm -f " *sal-parser-tmp-result-file*)))

(define (sal/parse-expr-string string)
  (try
   (begin
     (with-output-to-file *sal-parser-tmp-file*
       (lambda ()
         (print string)))
     (let ((cmd (string-append "sal2xml -nt expression -o " *sal-parser-tmp-result-file* " "  *sal-parser-tmp-file*)))
       (unless (= (system cmd) 0)
         (sign-error "Errors parsing SAL expression string.")))
     (let ((result (sal/parse-xml *sal-parser-tmp-result-file*)))
       (sal/delete-tmp-files!)
       result))
   (finally
    (sal/delete-tmp-files!))))
     
               


(define n 0)
(define-method (sal-ast/expand-core :around (self <sal-ast>) (env <primitive>) (depth <primitive>))
  (print (make-string n) "+++x " (format-with-location self "") " " (instance-class-name self))
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---x")
    result))
(define-method (sal-expr/lift :around (self <sal-ast>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (print (make-string n) "+++l " (format-with-location self "") " " (format-with-location arg "") " depth= " depth)
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---l ")
    result))



(define (display-env env)
  (display "{")
  (wt-tree/for-each (lambda (key data) 
                      (sal/pp key) (display " -> ") (sal/pp data) (display "; "))
                    env)
  (display "}"))
   


(define n 0)
(define-method (sal-ast/expand-core :around (self <sal-ast>) (env <primitive>) (depth <primitive>))
  (display* (make-string n) "+++ " (sal-ast->list self) " " (instance-class-name self) " ")
  (display-env env)
  (print "")
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "--- " (sal-ast->list result))
    result))


(define ast (sal-ast/flat-modules (sal/assertion-name "set2!th1")))

(define-method (sal-type/finite-rep-membership-expr :around (self <sal-type>) (env <primitive>) (expr <sal-expr>))
  (print (make-string n) "+++m " (sal-ast->list self) " " (sal-ast->list expr))
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---m " (sal-ast->list result))
    result))
  

(define n 0)
(define-method (sal-ast/local-simplify-core :around (ast <sal-ast>) (env <primitive>) (proc <primitive>))
  (print (make-string n) "+++s " (sal-ast->list ast) " " (instance-class-name ast))
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---s " (sal-ast->list result))
    result))

(define-method (sal-expr/lift :around (self <sal-ast>) (arg <sal-expr>) (env <primitive>) (depth <primitive>))
  (print (make-string n) "+++l " (sal-ast->list self) " " (sal-ast->list arg) " depth= " depth)
  (dynamic-define 'aux self)
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---l " (sal-ast->list result))
    result))

(define-method (sal-expr->boolean-expr-core :around (ast <sal-expr>) (env <primitive>) (aux-decl-queue <primitive>))
  (print (make-string n) "+++s " (sal-ast->list ast) " " (instance-class-name ast))
  (set! n (+ n 2))
  (multiple-value-bind
      (bit-list type)
      (call-next-method)
    (set! n (- n 2))
    (print (make-string n) "---s ")
    (values bit-list type)))

(define-method (sal-ast/expand-quantifiers-core :around (ast <sal-ast>) (env <primitive>))
  (print (make-string n) "+++s " (sal-ast->list ast) " " (instance-class-name ast))
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---s " (sal-ast->list result))
    result))

(define-method (sal-ast->list :around (self <sal-qualified-name-ref>))
  (sal-name-ref/name self))
  
(define-method (sal-type/finite-rep-membership-expr :around (type <sal-type>) (env <primitive>) (expr <sal-expr>))
  (let ((result (call-next-method)))
    ;; (breakpoint "foo" (type expr result) #t)
    result))

(define n 0)
(define-method (sal-quantified-expr/local-simplify-core :around (expr <sal-ast>) (quantifier <sal-ast>) (var-decls <primitive>))
  (print (make-string n) "+++q " (sal-ast->list expr) " " (sal-ast->list quantifier))
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "---q " (sal-ast->list result))
    result))

(define n 0)
(define-method (sal-ast/collect-dependencies :around (ast <sal-ast>) (dep <sal-dependencies>))
  (print (make-string n) "+++ " (format-with-location ast ""))
  (set! n (+ n 2))
  (let ((result (call-next-method)))
    (set! n (- n 2))
    (print (make-string n) "--- ")
    result))
  
(define-method (sal-ast/expand-core (ast <sal-builtin-application>) (env <primitive>) (depth <primitive>))
  (let ((proc (lambda (ast new-env) (sal-ast/expand-core ast new-env depth))))
    (sal-ast/local-simplify-core ast env proc)))
