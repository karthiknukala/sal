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

(module ls-parser
        (include "utility.sch")
        (include "sxml-package.sch")
        (include "trace.sch")
        (import xformat queue symbol-table sal-parser-utility)
        (export
         (ls-parser/parse-string a-string)
         (ls-parser/parse-file file-name)
         (ls-parser/nary-operators))
        )


(define *ls-keyword-list*
  '(;; SAL keywords
    "context" "tuple" "array" "record" "arrow" "subrange" "subtype" "mod-state" "for-all" "exists" "lambda"
    "mk-array" "mk-tuple" "mk-record" "let" "let*" "if" "cond" "else" "update" "set-list" "set-pred" "mod-init"
    "mod-trans" "array-ref" "tuple-ref" "record-ref" "import" "define" "define-inline" "define-type" "define-module"
    "rename" "add-prefix" "add-suffix" "scalar" "scalar-set" "ring-set" "datatype" "observer" "hide" "output" "with"
    "begin" "definition" "transition" "initialization" "input" "output" "global" "local" "label"
    "implements" "predicate" "theorem" "lemma" "obligation" "claim"
    ;; SAL Esterel/Lustre extensions keywords
    "nothing" "pause" "halt" "emit" "sustain" "present" "not-present" "case" "loop"
    "repeat" "times" "positive" "weak" "abort" "when" "await" "each" "every" "suspend"
    "trap" "exit" "handle" "signal" "immediate" "pre" "current" "esterel-module" "lustre-module"
    ;; SAL Imperative extensions keywords
    "imperative-module" "while" "atomic" "break" ;; the other keywords were already defined in the previous sections
    ))

(define (boolean-op? op)
  (memq op '(and or xor not iff implies)))

;; The following binary operators can be used as nary operators,
;; that is, (+ a b c) == (+ (+ a b) c)
(define *ls-binary-op-which-can-be-used-as-nary*
  '(+ - * and or))
    
(define (ls-parser/nary-operators)
  *ls-binary-op-which-can-be-used-as-nary*)

(define (ls/init-lexer!)
  (for-each (lambda (word)
              (putprop! (string->symbol word) 'ls-reserved #t))
            *ls-keyword-list*))

(ls/init-lexer!)

(define *manager* (make-sal-parser-manager))

(define *curr-file-name* #f)

(define (ls-sign-parser-error place msg . args)
  (apply sign-parser-error *curr-file-name* place msg args))

(define (reset!)
  (sal-parser-manager/reset! *manager*))

(define (mk-place initial final)
  (make-sal-place-from initial final))

(define (mk-token id s)
  (sal-parser-manager/make-token *manager* id s))

(define (mk-simple-token s)
  (sal-parser-manager/make-simple-token *manager* s))

(define *ls-lexer*
  (regular-grammar
   ((nonzero-digit   (in ("19")))
    (first-ident-char (or #\_ alpha #\* #\+ #\- #\/ #\% #\= #\< #\> #\| #\! #\?))
    (next-ident-char (or digit first-ident-char))
    (ident (: first-ident-char (* next-ident-char))))
   ((: ";" (* (out #\newline)) #\newline)
    (sal-parser-manager/adjust-coord *manager* (the-string))
    (ignore))
   ((+ (in #\space #\newline #\tab #a012 #a013))
    (sal-parser-manager/adjust-coord *manager* (the-string))
    (ignore))
   ((: "{*" (* (: (* (out #\*)) (? (: #\* (out #\}))))) "*}")
    (cons 'ATTRIBUTE-VALUE (mk-token 'ATTRIBUTE-VALUE (trim (the-substring 2 (- (the-length) 2))))))
   ((: #\" (* (: (* (out #\" #\\)) (? (: #\\ all)))) #\")
    (cons 'STRING (mk-token 'STRING (the-substring 1 (- (the-length) 1)))))
   ;; numerals
   ((: (+ digit))
    (cons 'NUMERAL (mk-token 'NUMERAL (the-string))))
   ((: #\- (+ digit))
    (cons 'NUMERAL (mk-token 'NUMERAL (the-string))))
   ;; curly braces
   (#\{
    (cons 'LC (mk-token 'LC (the-string))))
   (#\}
    (cons 'RC (mk-token 'RC (the-string))))
   ;; parenthesis
   (#\(
    (cons 'LP (mk-token 'LP (the-string))))
   (#\)
    (cons 'RP (mk-token 'RP (the-string))))
   ;; square brackets
   (#\[
    (cons 'LB (mk-token 'LB (the-string))))
   (#\]
    (cons 'RB (mk-token 'RB (the-string))))
   ;; COLON-COLON
   ("::"
    (cons 'COLON-COLON (mk-token 'COLON-COLON (the-string))))
   (#\' 
    (cons 'NEXT (mk-token 'NEXT (the-string))))
   (#\@
    (cons 'AT (mk-token 'AT (the-string))))
   ("[]"
    (cons 'ASYNCH (mk-token 'ASYNCH (the-string))))
   ("||"
    (cons 'SYNCH (mk-token 'SYNCH (the-string))))
   (":="
    (cons 'ASSIGN (mk-token 'ASSIGN (the-string))))
   ("|-"
    (cons 'TURNSTYLE (mk-token 'TURNSTYLE (the-string))))
   ("->"
    (cons 'ARROW (mk-token 'ARROW (the-string))))
   ("-->"
    (cons 'LONGARROW (mk-token 'LONGARROW (the-string))))
   ("."
    (cons 'DOT (mk-token 'DOT (the-string))))
   ("top-expr"
    (cons 'TOP-EXPR (mk-token 'TOP-EXPR (the-string))))
   ("top-module"
    (cons 'TOP-MODULE (mk-token 'TOP-MODULE (the-string))))
   ("top-type"
    (cons 'TOP-TYPE (mk-token 'TOP-TYPE (the-string))))
   ("top-assertion-name"
    (cons 'TOP-ASSERTION-NAME (mk-token 'TOP-ASSERTION-NAME (the-string))))
   ("top-import"
    (cons 'TOP-IMPORT (mk-token 'TOP-IMPORT (the-string))))
   ;; identifier
   (ident
    (let* ((string   (the-string))
           (symbol   (the-symbol))
           (upsymbol (string->symbol (string-upcase string))))
      (cond
       ((getprop symbol 'ls-reserved)
        (cons upsymbol (mk-token upsymbol (the-symbol))))
       (else
        (cons 'IDENTIFIER (mk-token 'IDENTIFIER (the-symbol)))))))
   ;; error
   (else
    (let ((c (the-failure)))
      (if (eof-object? c)
        c
        (cons 'ERROR (sal-parser-manager/error-token *manager*)))))))

(define (identifier->sxml token)
  (template->sxml (IDENTIFIER PLACE: ,(sal-token/place token)
                              ,(string->symbol (sal-token/data token)))))

(define (add-attributes node attributes)
  (trace 'ls-parser "adding attributes ~a to ~a" attributes (sxml->list node))
  (for-each (lambda (name:value)
              (sxml/set-attribute! node (car name:value) (cdr name:value)))
            attributes)
  node)

(define (mk-selection expr position+)
  (let loop ((position+ position+)
             (curr-expr expr))
    (if (null? position+)
      curr-expr
      (loop (cdr position+)
            (let* ((curr-pos (car position+))
                   (children (sxml/children curr-pos)))
              [assert (children) (= (length children) 1)]
              (make-sxml-node-based-on 
               curr-pos
               (cons curr-expr children)))))))

(define (mk-numeral numeral-token)
  (template->sxml (NUMERAL PLACE: ,(sal-token/place numeral-token)
                           ,(sal-token/data numeral-token))))

(define (queue->place q)
  (if (queue/empty? q)
    #f
    (mk-place (queue/front q)
              (queue/rear q))))

(define (convert-assertion-expr assertion-expr)
  (let loop ((assertion-expr assertion-expr))
    (sxml/match assertion-expr
      ((or (MODULEMODELS . ?-) (MODULEIMPLEMENTS . ?-))
       (values assertion-expr #f))
      ((APPLICATION (as (NAMEEXPR (as (? boolean-op?) ?op)) ?name-expr) ?args)
       (let ((args (if (sxml/tag-equals? args 'TUPLELITERAL)
                     (sxml/children args)
                     (list args))))
         (let* ((simple? #t)
                (new-args (map (lambda (arg)
                                 (multiple-value-bind
                                     (new-arg s?)
                                     (loop arg)
                                   (unless s?
                                     (set! simple? #f))
                                   new-arg))
                               args)))
           (if simple?
             (values assertion-expr #t)
             (values (template->sxml (ASSERTIONPROPOSITION (<== assertion-expr) 
                                                           (ASSERTIONOPERATOR (<== name-expr) ,op)
                                                           ,@new-args))
                     #f)))))
      ((FORALLEXPRESSION ?vardecls ?expr)
       (multiple-value-bind
           (new-expr simple?)
           (loop expr)
         (if simple?
           (values assertion-expr #t)
           (values (template->sxml (FORALLASSERTION (<== assertion-expr)
                                                    ,vardecls
                                                    ,new-expr)) 
                   #f))))
      ((EXISTSEXPRESSION ?vardecls ?expr)
       (multiple-value-bind
           (new-expr simple?)
           (loop expr)
         (if simple?
           (values assertion-expr #t)
           (values (template->sxml (EXISTSASSERTION (<== assertion-expr)
                                                    ,vardecls
                                                    ,new-expr))
                   #f))))
      (?-
       (values assertion-expr #t)))))

(define *ls-parser*
  (lalr-grammar
   ;; tokens
   (LB RB LP RP LC RC DOT DEFINE DEFINE-INLINE DEFINE-TYPE DEFINE-MODULE THEOREM LEMMA OBLIGATION CLAIM
       CONTEXT TYPE TUPLE ARRAY RECORD ARROW SUBTYPE SCALAR DATATYPE SUBRANGE MOD-STATE
       IDENTIFIER COLON-COLON SET-PRED SET-LIST NEXT NUMERAL STRING ARRAY-REF TUPLE-REF
       RECORD-REF FOR-ALL EXISTS LET LET* MK-ARRAY MK-RECORD MK-TUPLE IF COND ELSE MOD-INIT LAMBDA MOD-TRANS BEGIN
       SYNCH ASYNCH HIDE OUTPUT LOCAL GLOBAL INPUT RENAME OBSERVE LABEL LONGARROW WITH
       TRANSITION DEFINITION INITIALIZATION TURNSTYLE UPDATE AT IMPORT ADD-PREFIX ADD-SUFFIX
       OBSERVER IMPLEMENTS PREDICATE ESTEREL-MODULE ATTRIBUTE-VALUE TOP-TYPE TOP-EXPR TOP-MODULE TOP-IMPORT
       TOP-ASSERTION-NAME SCALAR-SET RING-SET
       ;; Estrel/Lustre Tokens
       NOTHING PAUSE HALT EMIT SUSTAIN ASSIGN
       )
   [main
    ((context)
     context)
    ((TOP-EXPR expr)
     expr)
    ((TOP-MODULE module)
     module)
    ((TOP-TYPE type)
     type)
    ((TOP-ASSERTION-NAME top-assertion-name)
     top-assertion-name)
    ((TOP-IMPORT import-def)
     import-def)
    ]
   [context
    ((LC context attribute+ RC)
     (add-attributes context attribute+))
    ((LP@cb CONTEXT identifier LP@tb type-param* RP@te LP@vb var-decl* RP@ve declaration+ RP@ce)
     (template->sxml (CONTEXT PLACE: ,(mk-place cb ce)
                              ,identifier
                              (PARAMETERS PLACE: ,(mk-place tb ve)
                                          ,@(append (queue->list type-param*) (queue->list var-decl*)))
                              (CONTEXTBODY PLACE: ,(mk-place (queue/front declaration+) ce)
                                           ,@(queue->list declaration+)))))]
   [attribute+
    ((attribute)
     (list attribute))
    ((attribute+ attribute)
     (cons attribute attribute+))]
   [attribute
    ((LP IDENTIFIER ATTRIBUTE-VALUE RP)
     (cons (sal-token/data IDENTIFIER)
           (sal-token/data ATTRIBUTE-VALUE)))
    ((LP IDENTIFIER STRING RP)
     (cons (sal-token/data IDENTIFIER)
           (sal-token/data STRING)))
    ((LP IDENTIFIER@attr IDENTIFIER@value RP)
     (let* ((aux (sal-token/data value))
            (val (cond 
                  ((eq? aux 'true) #t)
                  ((eq? aux 'false) #f)
                  (else aux))))
       (cons (sal-token/data attr)
             val)))
    ((LP IDENTIFIER NUMERAL RP)
     (cons (sal-token/data IDENTIFIER)
           (string->integer (sal-token/data NUMERAL))))]
   [type-param*
    (()
     (make-queue))
    ((type-param* identifier)
     (queue/insert! type-param* (template->sxml (TYPEDECL PLACE: ,(extract-sal-place identifier)
                                                          ,identifier))))]
   [var-decl*
    (()
     (make-queue))
    ((var-decl* var-decl)
     (queue/insert! var-decl* var-decl))]
   [var-decl
    ((LC var-decl attribute+ RC)
     (add-attributes var-decl attribute+))
    ((identifier COLON-COLON type)
     (template->sxml (VARDECL PLACE: ,(mk-place identifier type)
                              ,identifier
                              ,type)))]
   [index-var-decl
    ((LC index-var-decl attribute+ RC)
     (add-attributes index-var-decl attribute+))
    ((identifier COLON-COLON index-type)
     (template->sxml (VARDECL PLACE: ,(mk-place identifier index-type)
                              ,identifier
                              ,index-type)))]
   [var-decl+
    ((var-decl)
     (make-queue var-decl))
    ((var-decl+ var-decl)
     (queue/insert! var-decl+ var-decl))]
   [type-name
    ((name)
     (cond
      ((sxml/tag-equals? name 'NAME)
       (sxml/set-tag! name 'TYPENAME))
      ((sxml/tag-equals? name 'QUALIFIEDNAME)
       (sxml/set-tag! name 'QUALIFIEDTYPENAME))
      (else
       (internal-error)))
     name)]
   [index-type
    ((LC index-type attribute+ RC)
     (add-attributes index-type attribute+))
    ((type-name)
     type-name)
    ((subrange)
     subrange)]
   [subrange
    ((LP SUBRANGE top-expr@low top-expr@high RP)
     (template->sxml (SUBRANGE PLACE: ,(mk-place LP RP)
                               ,low ,high)))]
   [type 
    ((type-name)
     type-name)
    ((LP TUPLE binded-type+ type RP)
     (template->sxml (TUPLETYPE PLACE: ,(mk-place LP RP)
                                ,@(queue->list binded-type+)
                                ,type)))
    ((LP ARRAY type@idx type@elem RP)
     (template->sxml (ARRAYTYPE PLACE: ,(mk-place LP RP)
                                ,idx
                                ,elem)))
    ((LP RECORD field+ RP)
     (template->sxml (RECORDTYPE PLACE: ,(mk-place LP RP)
                                 ,@(queue->list field+))))
    ((LP ARROW binded-type+ type RP)
     (let ((args (queue->list binded-type+)))
       (template->sxml (FUNCTIONTYPE PLACE: ,(mk-place LP RP)
                                     ,(if (null? (cdr args))
                                        (car args)
                                        (template->sxml (TUPLETYPE PLACE: ,(queue->place binded-type+)
                                                                   ,@args)))
                                     ,type))))
    ((subrange)
     subrange)
    ((LP SUBTYPE top-expr RP)
     (template->sxml (SUBTYPE PLACE: ,(mk-place LP RP)
                              ,top-expr)))
    ((LP@bs SUBTYPE LP var-decl RP top-expr RP@es)
     (template->sxml (SUBTYPE PLACE: ,(mk-place bs es)
                              (SETPREDEXPRESSION PLACE: ,(mk-place bs es)
                                                 ,var-decl
                                                 ,top-expr))))
    ((LP MOD-STATE top-module RP)
     (template->sxml (STATETYPE PLACE: ,(mk-place LP RP)
                                ,top-module)))
    ]
   [type*
    (()
     (make-queue))
    ((type* type)
     (queue/insert! type* type))]
   [binded-type+
    ((binded-type)
     (make-queue binded-type))
    ((binded-type+ binded-type)
     (queue/insert! binded-type+ binded-type))]
   [binded-type
    ((type)
     type)
    ((var-decl)
     var-decl)]
   [field+
    ((field-decl)
     (make-queue field-decl))
    ((field+ field-decl)
     (queue/insert! field+ field-decl))]
   [field-decl
    ((LC field-decl attribute+ RC)
     (add-attributes field-decl attribute+))
    ((identifier COLON-COLON type)
     (template->sxml (FIELDDECLARATION PLACE: ,(mk-place identifier type)
                                       ,identifier
                                       ,type)))]
   [quantifier
    ((FOR-ALL)
     'FORALLEXPRESSION)
    ((EXISTS)
     'EXISTSEXPRESSION)]
   [binding-op
    ((LAMBDA)
     'LAMBDAABSTRACTION)
    ((quantifier)
     quantifier)]
   [next-operator
    ((IDENTIFIER NEXT)
     (template->sxml (NEXTOPERATOR PLACE: ,(mk-place IDENTIFIER NEXT)
                                   (NAMEEXPR PLACE: ,(sal-token/place IDENTIFIER)
                                             ,(sal-token/data IDENTIFIER)))))]
   [position
    ((DOT identifier)
     (template->sxml (RECORDSELECTION PLACE: ,(mk-place DOT identifier) ,identifier)))
    ((DOT NUMERAL)
     (template->sxml (TUPLESELECTION PLACE: ,(mk-place DOT NUMERAL) ,(mk-numeral NUMERAL))))
    ((LB expr RB)
     (template->sxml (ARRAYSELECTION PLACE: ,(mk-place LB RB) ,expr)))
    ]
;    [update-position
;     ((DOT identifier)
;      (template->sxml (RECORDSELECTION PLACE: ,(mk-place DOT identifier) ,identifier)))
;     ((DOT NUMERAL)
;      (template->sxml (TUPLESELECTION PLACE: ,(mk-place DOT NUMERAL) ,(mk-numeral NUMERAL))))
;     ((LB expr RB)
;      (template->sxml (ARRAYSELECTION PLACE: ,(mk-place LB RB) ,expr)))
;     ((LP expr+ RP)
;      (let ((args (queue->list expr+)))
;        (template->sxml (APPLICATION PLACE: ,(mk-place LP RP)
;                                     ,(if (> (length args) 1)
;                                        (template->sxml (TUPLELITERAL PLACE: ,(queue->place expr+)
;                                                                      ,@args))
;                                        (car args))))))]
   [position+
    ((position)
     (list position))
    ((position position+)
     (cons position position+))]
;    [update-position+
;     ((update-position)
;      (list update-position))
;     ((update-position update-position+)
;      (cons update-position update-position+))]
   [top-expr
    ((LC expr attribute+ RC)
     (add-attributes expr attribute+))
    ((expr)
     expr)]
   [expr
    ((name)
     (cond
      ((sxml/tag-equals? name 'NAME)
       (sxml/set-tag! name 'NAMEEXPR))
      ((sxml/tag-equals? name 'QUALIFIEDNAME)
       (sxml/set-tag! name 'QUALIFIEDNAMEEXPR))
      (else
       (internal-error)))
     name)
    ((NUMERAL)
     (mk-numeral NUMERAL))
    ((STRING)
     (template->sxml (STRINGEXPR PLACE: ,(sal-token/place STRING)
                                 ,(sal-token/data STRING))))
    ((next-operator)
     next-operator)
    ((expr position+)
     (mk-selection expr position+))
    ((LP MK-TUPLE expr expr+ RP) ;; a tuple must have at least two elements
     (template->sxml (TUPLELITERAL PLACE: ,(mk-place LP RP)
                                   ,expr
                                   ,@(queue->list expr+))))
    ((LP MK-RECORD field-value+ RP)
     (template->sxml (RECORDLITERAL PLACE: ,(mk-place LP RP)
                                    ,@(queue->list field-value+))))
    ((LP@bb MK-ARRAY LP index-var-decl RP expr RP@eb)
     (template->sxml (ARRAYLITERAL PLACE: ,(mk-place bb eb)
                                   ,index-var-decl
                                   ,expr)))
    ((LP@bb SET-PRED LP var-decl RP expr RP@eb)
     (template->sxml (SETPREDEXPRESSION PLACE: ,(mk-place bb eb)
                                        ,var-decl
                                        ,expr)))
    ((LP@bb binding-op LP@bv var-decl+ RP@ev expr RP@eb)
     (template->sxml (,binding-op PLACE: ,(mk-place bb eb)
                                  (VARDECLS PLACE: ,(mk-place bv ev)
                                            ,@(queue->list var-decl+))
                                  ,expr)))
    ((LP@lb LET LP@db let-decl+ RP@de expr RP@le)
     (template->sxml (LETEXPRESSION PLACE: ,(mk-place lb le)
                                    (LETDECLARATIONS PLACE: ,(mk-place db de)
                                                     ,@(queue->list let-decl+))
                                    ,expr)))
    ((LP@lb LET* LP@db let-decl+ RP@de expr RP@le)
     (let loop ((let-decl-list (queue->list let-decl+)))
       (if (null? let-decl-list)
         expr
         (let ((curr (car let-decl-list))
               (rest (loop (cdr let-decl-list))))
           (template->sxml (LETEXPRESSION PLACE: ,(mk-place lb le)
                                          (LETDECLARATIONS PLACE: ,(mk-place db de)
                                                           ,curr)
                                          ,rest))))))
    ((LP IF expr@c expr@t expr@e RP)
     (template->sxml (CONDITIONAL PLACE: ,(mk-place LP RP)
                                  ,c ,t ,e)))
    ((LP expr expr+ RP)
     (let ((args (queue->list expr+)))
       (if (and (sxml/tag-equals? expr 'NAMEEXPR)
                (memq (sxml/first-child expr) *ls-binary-op-which-can-be-used-as-nary*)
                (> (length args) 2))
         (let loop ((curr (template->sxml (APPLICATION PLACE: ,(mk-place LP (cadr args))
                                                       ,expr
                                                       (TUPLELITERAL PLACE: ,(mk-place (car args) (cadr args))
                                                                     ,(car args) ,(cadr args)))))
                    (curr-args (cddr args)))
           (if (null? curr-args)
             curr
             (loop (template->sxml (APPLICATION PLACE: ,(mk-place LP (car curr-args))
                                                ,expr
                                                (TUPLELITERAL PLACE: ,(mk-place (car args) (car curr-args))
                                                              ,curr ,(car curr-args))))
                   (cdr curr-args))))
         (template->sxml (APPLICATION PLACE: ,(mk-place LP RP)
                                      ,expr
                                      ,(if (> (length args) 1)
                                         (template->sxml (TUPLELITERAL PLACE: ,(queue->place expr+)
                                                                       ,@args))
                                         (car args)))))))
    ((LP COND alternative+ else-alternative RP)
     (let ((alternatives (reverse (queue->list alternative+))))
       (let loop ((curr-else else-alternative)
		  (alternatives alternatives))
         (if (null? alternatives)
	     curr-else
           (let ((curr-alternative (car alternatives)))
             (sxml/match-or-fail curr-alternative
               ((ALTERNATIVE ?cond ?body)
                (loop (template->sxml (CONDITIONAL PLACE: ,(extract-sal-place curr-alternative)
                                                   ELSIF: ,(if (null? (cdr alternatives)) "NO" "YES")
                                                   ,cond
                                                   ,body
                                                   ,curr-else))
                      (cdr alternatives)))))))))
    ((array-ref)
     array-ref)
    ((tuple-ref)
     tuple-ref)
    ((record-ref)
     record-ref)
    ((LP UPDATE expr@target position+ expr@new-val RP)
     (template->sxml (UPDATEEXPRESSION PLACE: ,(mk-place LP RP)
                                       ,target
                                       ,(mk-selection target position+)
                                       ,new-val)))
    ((LP SET-LIST expr+ RP)
     (template->sxml (SETLISTEXPRESSION PLACE: ,(mk-place LP RP)
                                        ,@(queue->list expr+))))
    ((LP MOD-INIT top-module RP)
     (template->sxml (MODINIT PLACE: ,(mk-place LP RP)
                              ,top-module)))
    ((LP MOD-TRANS top-module RP)
     (template->sxml (MODTRANS PLACE: ,(mk-place LP RP)
                               ,top-module)))
    ;; TURNSTYLE and IMPLEMENTS are only allowed in assertion declarations.
    ((LP TURNSTYLE top-module expr RP)
     (template->sxml (MODULEMODELS PLACE: ,(mk-place LP RP)
                                   ,top-module
                                   ,expr)))
    ((LP IMPLEMENTS top-module@m1 top-module@m2 RP)
     (template->sxml (MODULEIMPLEMENTS PLACE: ,(mk-place LP RP)
                                       ,m1 ,m2)))]
   [field-value+
    ((field-value)
     (make-queue field-value))
    ((field-value+ field-value)
     (queue/insert! field-value+ field-value))]
   [field-value
    ((LC field-value attribute+ RC)
     (add-attributes field-value attribute+))
    ((identifier COLON-COLON expr)
     (template->sxml (RECORDENTRY PLACE: ,(mk-place identifier expr)
                                  ,identifier
                                  ,expr)))]
   [array-ref
    ((LP ARRAY-REF expr@array expr@idx RP)
     (template->sxml (ARRAYSELECTION PLACE: ,(mk-place LP RP)
                                     ,array
                                     ,idx)))]
   [tuple-ref
    ((LP TUPLE-REF expr NUMERAL RP)
     (template->sxml (TUPLESELECTION PLACE: ,(mk-place LP RP)
                                     ,expr
                                     ,(mk-numeral NUMERAL))))]
   [record-ref
    ((LP RECORD-REF expr identifier RP)
     (template->sxml (RECORDSELECTION PLACE: ,(mk-place LP RP)
                                      ,expr
                                      ,identifier)))]
   [lhs
    ((LC lhs attribute+ RC)
     (add-attributes lhs attribute+))
    ((IDENTIFIER)
     (template->sxml (NAMEEXPR PLACE: ,(sal-token/place IDENTIFIER)
                               ,(sal-token/data IDENTIFIER))))
    ((lhs position+)
     (mk-selection lhs position+))
    ((next-operator)
     next-operator)
    ((array-ref)
     array-ref)
    ((record-ref)
     record-ref)
    ((tuple-ref)
     tuple-ref)]
   [context-name
    ((identifier)
     (template->sxml (CONTEXTNAME PLACE: ,(extract-sal-place identifier)
                                  ,identifier)))
    ((LP@cb identifier LP@tb type* RP@te LP@eb expr* RP@ee RP@ce)
     (let ((types (queue->list type*))
           (exprs (queue->list expr*)))
       (template->sxml 
        (CONTEXTNAME PLACE: ,(mk-place cb ce)
                     ,identifier
                     ,@(if (and (null? types) (null? exprs))
                         '()
                         (list (template->sxml 
                                (ACTUALPARAMETERS PLACE: ,(mk-place tb ee)
                                                  ,@(append types exprs)))))))))]
   [qualified-name
    ((LP AT identifier context-name RP)
     (template->sxml (QUALIFIEDNAME PLACE: ,(mk-place LP RP)
                                    ,identifier
                                    ,context-name)))]
   [name
    ((IDENTIFIER)
     (template->sxml (NAME PLACE: ,(sal-token/place IDENTIFIER)
                           ,(sal-token/data IDENTIFIER))))
    ((qualified-name)
     qualified-name)]
   [expr*
    (()
     (make-queue))
    ((expr* expr)
     (queue/insert! expr* expr))]
   [expr+
    ((expr)
     (make-queue expr))
    ((expr+ expr)
     (queue/insert! expr+ expr))]
   [let-decl+
    ((let-decl)
     (make-queue let-decl))
    ((let-decl+ let-decl)
     (queue/insert! let-decl+ let-decl))]
   [let-decl
    ((LC let-decl attribute+ RC)
     (add-attributes let-decl attribute+))
    ((LP identifier COLON-COLON type expr RP)
     (template->sxml (LETDECLARATION PLACE: ,(mk-place LP RP)
                                     ,identifier
                                     ,type
                                     ,expr)))]
   [alternative+
    ((alternative)
     (make-queue alternative))
    ((alternative+ alternative)
     (queue/insert! alternative+ alternative))]
   [alternative
    ((LP expr@c expr@t RP)
     (template->sxml (ALTERNATIVE PLACE: ,(mk-place LP RP) ,c ,t)))]
   [else-alternative
    ((LP ELSE expr RP)
     expr)]
   [declaration+
    ((declaration)
     (make-queue declaration))
    ((declaration+ declaration)
     (queue/insert! declaration+ declaration))]
   [define-or-define-inline
     ((DEFINE) 'define)
     ((DEFINE-INLINE) 'define-inline)]
   [opt-expr
    (() '())
    ((top-expr) (list top-expr))]
   [declaration
    ((LC declaration attribute+ RC)
     (add-attributes declaration attribute+))
    ((import-def)
     import-def)
    ((LP define-or-define-inline identifier COLON-COLON type opt-expr RP)
     (template->sxml (CONSTANTDECLARATION PLACE: ,(mk-place LP RP) inline: ,(and (eq? define-or-define-inline 'define-inline)
                                                                                 (not (null? opt-expr)))
                                          ,identifier ,type ,@opt-expr)))
    ((LP@cb define-or-define-inline LP identifier COLON-COLON type var-decl+ RP expr RP@ce)
     (let* ((vardecls (queue->list var-decl+))
            (arg-types (map (sxml/match-lambda-or-fail
                             ((VARDECL ?- ?type)
                              type))
                            vardecls))
            (function-type (template->sxml (FUNCTIONTYPE PLACE: ,(mk-place type (queue/rear var-decl+))
                                                         ,(if (> (length arg-types) 1)
                                                            (template->sxml (TUPLETYPE PLACE: ,(queue->place var-decl+)
                                                                                       ,@arg-types))
                                                            (car arg-types))
                                                         ,type))))
       (template->sxml (CONSTANTDECLARATION PLACE: ,(mk-place cb ce) inline: ,(eq? define-or-define-inline 'define-inline)
                                            ,identifier
                                            ,function-type
                                            (LAMBDAABSTRACTION PLACE: ,(mk-place cb ce) 
                                                               (VARDECLS PLACE: ,(queue->place var-decl+)
                                                                         ,@vardecls)
                                                               ,expr)))))
    ((LP DEFINE-TYPE identifier type-def RP)
     (template->sxml (TYPEDECLARATION PLACE: ,(mk-place LP RP)
                                      ,identifier
                                      ,@type-def)))
    ((LP DEFINE-MODULE identifier top-module RP)
     (template->sxml (MODULEDECLARATION PLACE: ,(mk-place LP RP)
                                        ,identifier
                                        (VARDECLS PLACE: ,(extract-sal-place identifier))
                                        ,top-module)))
    ((LP@mb DEFINE-MODULE LP identifier var-decl+ RP top-module RP@me)
     (template->sxml (MODULEDECLARATION PLACE: ,(mk-place mb me)
                                        ,identifier
                                        (VARDECLS PLACE: ,(queue->place var-decl+)
                                                  ,@(queue->list var-decl+))
                                        ,top-module)))
    ((LP assertion-kind identifier expr RP)
     (template->sxml (ASSERTIONDECLARATION PLACE: ,(mk-place LP RP)
                                           ,identifier
                                           (ASSERTIONFORM PLACE: ,(sal-token/place assertion-kind)
                                                          ,(sal-token/data assertion-kind))
                                           ,(convert-assertion-expr expr))))]
   [top-assertion-name
    ((qualified-name)
     (sxml/set-tag! qualified-name 'QUALIFIEDASSERTIONNAME)
     qualified-name)]
   [import-def
    ((LP IMPORT context-ref RP)
     (template->sxml (IMPORT PLACE: ,(mk-place LP RP)
                             ,context-ref)))]
   [context-ref
    ((context-name)
     context-name)
    ((LP RENAME LP rename-decl+ RP context-ref RP)
     (template->sxml (IMPORTRENAME PLACE: ,(mk-place LP RP)
                                   (RENAMEDECLS PLACE: ,(queue->place rename-decl+)
                                                ,@(queue->list rename-decl+))
                                   ,context-ref)))
    ((LP ADD-PREFIX identifier context-ref RP)
     (template->sxml (ADDPREFIX PLACE: ,(mk-place LP RP)
                                ,identifier
                                ,context-ref)))
    ((LP ADD-SUFFIX identifier context-ref RP)
     (template->sxml (ADDSUFFIX PLACE: ,(mk-place LP RP)
                                ,identifier
                                ,context-ref)))]
   [rename-decl+
    ((rename-decl)
     (make-queue rename-decl))
    ((rename-decl+ rename-decl)
     (queue/insert! rename-decl+ rename-decl))]
   [rename-decl
    ((LP identifier@id1 identifier@id2 RP)
     (template->sxml (RENAMEDECL PLACE: ,(mk-place LP RP)
                                 ,id1
                                 ,id2)))]
   [type-def
    ((LC type-def attribute+ RC)
     (if (null? type-def)
       type-def
       (list (add-attributes (car type-def) attribute+))))
    (()
     '())
    ((type)
     (list type))
    ((LP SCALAR scalar-element+ RP)
     (list (template->sxml (SCALARTYPE PLACE: ,(mk-place LP RP)
                                       ,@(queue->list scalar-element+)))))
    ((LP SCALAR-SET expr RP)
     (list (template->sxml (SCALARSET PLACE: ,(mk-place LP RP)
                                      ,expr))))
    ((LP RING-SET expr RP)
     (list (template->sxml (RINGSET PLACE: ,(mk-place LP RP)
                                    ,expr))))
    ((LP DATATYPE constructor+ RP)
     (list (template->sxml (DATATYPE PLACE: ,(mk-place LP RP)
                                     ,@(queue->list constructor+)))))]
   [constructor+
    ((constructor)
     (make-queue constructor))
    ((constructor+ constructor)
     (queue/insert! constructor+ constructor))]
   [constructor
    ((LC constructor attribute+ RC)
     (add-attributes constructor attribute+))
    ((LP identifier accessor+ RP)
     (template->sxml (CONSTRUCTOR PLACE: ,(mk-place LP RP)
                                  ,identifier
                                  ,@(queue->list accessor+))))
    ((identifier)
     (template->sxml (CONSTRUCTOR PLACE: ,(extract-sal-place identifier)
                                  ,identifier)))]
   [accessor+
    ((accessor)
     (make-queue accessor))
    ((accessor+ accessor)
     (queue/insert! accessor+ accessor))]
   [accessor
    ((LC accessor attribute+ RC)
     (add-attributes accessor attribute+))
    ((identifier COLON-COLON type)
     (template->sxml (ACCESSOR PLACE: ,(mk-place identifier type)
                               ,identifier
                               ,type)))]
   [identifier
    ((IDENTIFIER)
     (template->sxml (IDENTIFIER PLACE: ,(sal-token/place IDENTIFIER)
                                 ,(sal-token/data IDENTIFIER))))]
   [identifier+
    ((identifier)
     (make-queue identifier))
    ((identifier+ identifier)
     (queue/insert! identifier+ identifier))]
   [scalar-element+
    ((scalar-element)
     (make-queue scalar-element))
    ((scalar-element+ scalar-element)
     (queue/insert! scalar-element+ scalar-element))]
   [scalar-element
    ((LC scalar-element attribute+ RC)
     (add-attributes scalar-element attribute+))
    ((IDENTIFIER)
     (template->sxml (SCALARELEMENT PLACE: ,(sal-token/place IDENTIFIER)
                                    ,(sal-token/data IDENTIFIER))))]
   [composition-op
    ((ASYNCH)
     'ASYNCHRONOUSCOMPOSITION)
    ((SYNCH)
     'SYNCHRONOUSCOMPOSITION)]
   [module-name
    ((name)
     (cond
      ((sxml/tag-equals? name 'NAME)
       (sxml/set-tag! name 'MODULENAME))
      ((sxml/tag-equals? name 'QUALIFIEDNAME)
       (sxml/set-tag! name 'QUALIFIEDMODULENAME))
      (else
       (internal-error)))
     name)]
   [top-module
    ((module)
     module)
    ((LC module attribute+ RC)
     (add-attributes module attribute+))]
   [module
    ((module-name)
     (template->sxml (MODULEINSTANCE PLACE: ,(extract-sal-place module-name)
                                     ,module-name
                                     (MODULEACTUALS PLACE: ,(extract-sal-place module-name)))))
    ((LP module-name expr+ RP)
     (template->sxml (MODULEINSTANCE PLACE: ,(mk-place LP RP)
                                     ,module-name
                                     (MODULEACTUALS PLACE: ,(queue->place expr+)
                                                    ,@(queue->list expr+)))))
    ((LP composition-op module module+ RP)
     (let ((modules (queue->list module+)))
       (let loop ((curr-module module)
                  (modules modules))
         (if (null? modules)
           curr-module
           (loop (template->sxml (,composition-op PLACE: ,(mk-place curr-module (car modules))
                                                  ,curr-module
                                                  ,(car modules)))
                 (cdr modules))))))
    ((LP OBSERVER module@mod1 module@mod2 RP)
     (template->sxml (OBSERVEMODULE PLACE: ,(mk-place LP RP)
                                    ,mod1 ,mod2)))
    ((LP@mb composition-op LP index-var-decl RP module RP@me)
     (template->sxml (,(if (eq? composition-op 'ASYNCHRONOUSCOMPOSITION)
                         'MULTIASYNCHRONOUS
                         'MULTISYNCHRONOUS)
                      PLACE: ,(mk-place mb me)
                      ,index-var-decl
                      ,module)))
    ((LP RENAME renames module RP)
     (template->sxml (RENAMING PLACE: ,(mk-place LP RP)
                               ,renames
                               ,module)))
    ((LP LABEL IDENTIFIER module RP)
     (sxml/set-attribute! module 'MODULELABEL (sal-token/data IDENTIFIER))
     module)
    ((LP HIDE identifiers module RP)
     (template->sxml (HIDING PLACE: ,(mk-place LP RP)
                             ,identifiers
                             ,module)))
    ((LP OUTPUT identifiers module RP)
     (template->sxml (NEWOUTPUT PLACE: ,(mk-place LP RP)
                                ,identifiers
                                ,module)))
    ((LP WITH new-var-decls module RP)
     (template->sxml (WITHMODULE PLACE: ,(mk-place LP RP)
                                 ,new-var-decls
                                 ,module)))
    ((base-module)
     base-module)
    ((esterel-module)
     esterel-module)]
   [new-var-decls
    ((LP state-var-decls+ RP)
     (template->sxml (NEWVARDECLS PLACE: ,(mk-place LP RP)
                                  ,@(queue->list state-var-decls+))))]
   [renames
    ((LP rename+ RP)
     (template->sxml (RENAMES PLACE: ,(mk-place LP RP)
                              ,@(queue->list rename+))))]
   [identifiers 
    ((LP identifier+ RP)
     (template->sxml (IDENTIFIERS PLACE: ,(mk-place LP RP)
                                  ,@(queue->list identifier+))))]
   [module+
    ((module)
     (make-queue module))
    ((module+ module)
     (queue/insert! module+ module))]
   [rename+
    ((rename)
     (make-queue rename))
    ((rename+ rename)
     (queue/insert! rename+ rename))]
   [rename
    ((LP lhs@l1 lhs@l2 RP)
     (template->sxml (RENAME PLACE: ,(mk-place LP RP)
                             ,l1 ,l2)))]
   [base-module
    ((LP BEGIN base-module-decl+ RP)
     (template->sxml (BASEMODULE PLACE: ,(mk-place LP RP)
                                 ,@(queue->list base-module-decl+))))]
   [esterel-module
    ((LP ESTEREL-MODULE esterel-module-interface esterel-statement RP)
     )]
   [esterel-module-interface
    (()
     )]
   [esterel-statement
    ((NOTHING)
     )
    ((PAUSE)
     )
    ((HALT)
     )
                                        ;             ((LP EMIT identifier RP)
                                        ;              )
                                        ;             ((LP EMIT identifier expr RP)
                                        ;              )
                                        ;             ((LP SUSTAIN identifier RP)
                                        ;              )
                                        ;             ((LP SUSTAIN identifier expr RP)
                                        ;              )
                                        ;             ((LP ASSIGN identifier epxr RP)
                                        ;              )
                                        ;             ((LP PRESENT signal-expression esterel-statement esterel-statement? RP)
                                        ;              )
                                        ;             ((LP NOT-PRESENT signal-expression esterel-statement RP)
                                        ;              )
                                        ;             ((LP PRESENT present-alternative+ present-else-alternative? RP)
                                        ;              )
                                        ;             ((LP IF expr esterel-statement esterel-statement? RP)
                                        ;              )
                                        ;             ((LP COND esterel-cond-alternatives+ esterel-else-alternative? RP)
                                        ;              )
                                        ;             ((LP LOOP esterel-statement RP)
                                        ;              )
                                        ;             ((LP LOOP-EACH esterel-statement esterel-delay-expr RP)
                                        ;              )
    ]
   [base-module-decl+
    ((base-module-decl)
     (make-queue base-module-decl))
    ((base-module-decl+ base-module-decl)
     (queue/insert! base-module-decl+ base-module-decl))]
   [base-module-decl
    ((state-var-decls)
     state-var-decls)
    ((LP DEFINITION definition+ RP)
     (template->sxml (DEFDECL PLACE: ,(mk-place LP RP)
                       ,@(queue->list definition+))))
    ((LP TRANSITION def-or-cmd+ RP)
     (template->sxml (TRANSDECL PLACE: ,(mk-place LP RP)
                                ,@(queue->list def-or-cmd+))))
    ((LP INITIALIZATION def-or-cmd+ RP)
     (template->sxml (INITDECL PLACE: ,(mk-place LP RP)
                               ,@(queue->list def-or-cmd+))))]
   [state-var-decl-kind
    ((INPUT)
     'INPUTDECL)
    ((OUTPUT)
     'OUTPUTDECL)
    ((GLOBAL)
     'GLOBALDECL)
    ((LOCAL)
     'LOCALDECL)]
   [state-var-decls
    ((LP state-var-decl-kind var-decl+ RP)
     (template->sxml (,state-var-decl-kind PLACE: ,(mk-place LP RP)
                                           ,@(queue->list var-decl+))))]
   [state-var-decls+
    ((state-var-decls)
     (make-queue state-var-decls))
    ((state-var-decls+ state-var-decls)
     (queue/insert! state-var-decls+ state-var-decls))]
   [definition+
     ((definition)
      (make-queue definition))
     ((definition+ definition)
      (queue/insert! definition+ definition))]
   [definition
     ((LC definition attribute+ RC)
      (add-attributes definition attribute+))
     ((LP@fb FOR-ALL LP@vb var-decl+ RP@ve definition+ RP@fe)
      (template->sxml (FORALLDEFINITION PLACE: ,(mk-place fb fe)
                                        (VARDECLS PLACE: ,(mk-place vb ve)
                                                  ,@(queue->list var-decl+))
                                        ,@(queue->list definition+))))
     ((simple-definition)
      simple-definition)]
   [simple-definition*
    (()
     (make-queue))
    ((simple-definition* simple-definition)
     (queue/insert! simple-definition* simple-definition))]
   [simple-definition
    ((LP IDENTIFIER lhs expr RP)
     (let ((op (sal-token/data IDENTIFIER)))
       (unless (or (eq? op '=) (eq? op 'in))
         (ls-sign-parser-error (sal-token/place IDENTIFIER) "Invalid simple definition, the allowed operators: `=' and `in'."))
       (template->sxml (SIMPLEDEFINITION PLACE: ,(mk-place LP RP)
                                         ,lhs
                                         (,(if (eq? op '=)
                                             'RHSEXPRESSION
                                             'RHSSELECTION)
                                          ,expr)))))]
   [def-or-cmd+
     ((def-or-cmd)
      (make-queue def-or-cmd))
     ((def-or-cmd+ def-or-cmd)
      (queue/insert! def-or-cmd+ def-or-cmd))]
   [def-or-cmd
     ((definition)
      definition)
     ((some-commands)
      some-commands)]
   [some-commands
    ((LC some-commands attribute+ RC)
     (add-attributes some-commands attribute+))
    ((LP ASYNCH some-command+ RP)
     (template->sxml (SOMECOMMANDS PLACE: ,(mk-place LP RP)
                                   ,@(queue->list some-command+))))]
   [some-command+
    ((some-command)
     (make-queue some-command))
    ((some-command+ some-command)
     (queue/insert! some-command+ some-command))]
   [some-command
    ((LC some-command attribute+ RC)
     (add-attributes some-command attribute+))
    ((LP LABEL IDENTIFIER some-command RP)
     (template->sxml (LABELEDCOMMAND PLACE: ,(mk-place LP RP)
                                     (LABEL PLACE: ,(sal-token/place LABEL)
                                            ,(sal-token/data IDENTIFIER))
                                     ,some-command)))
    ((LP@mb ASYNCH LP@vb var-decl+ RP@ve some-command RP@me)
     (template->sxml (MULTICOMMAND PLACE: ,(mk-place mb me)
                                   (VARDECLS PLACE: ,(mk-place vb ve)
                                             ,@(queue->list var-decl+))
                                   ,some-command)))
    ((LP ELSE LONGARROW simple-definition* RP)
     (template->sxml (ELSECOMMAND PLACE: ,(mk-place LP RP)
                                  (ASSIGNMENTS PLACE: ,(queue->place simple-definition*)
                                               ,@(queue->list simple-definition*)))))
    ((LP expr LONGARROW simple-definition* RP)
     (template->sxml (GUARDEDCOMMAND PLACE: ,(mk-place LP RP)
                                     (GUARD PLACE: ,(extract-sal-place expr)
                                            ,expr)
                                     (ASSIGNMENTS PLACE: ,(queue->place simple-definition*)
                                                  ,@(queue->list simple-definition*)))))]
   [assertion-kind
    ((THEOREM)
     THEOREM)
    ((LEMMA)
     LEMMA)
    ((OBLIGATION)
     OBLIGATION)
    ((CLAIM)
     CLAIM)]
   ))
      
(define (parse)
  (reset!)
  (try
   (read/lalrp
    *ls-parser*
    *ls-lexer*
    (current-input-port))
   (lambda (escape proc msg obj)
     (if (equal? proc "parser")
       (cond
        ((eof-object? obj)
         (sign-error "Unexpected end of input stream, when parsing `~a'." (if *curr-file-name* *curr-file-name* "<string>")))
        ((eq? (car obj) 'ERROR)
         (ls-sign-parser-error (sal-token/place (cdr obj)) "Invalid token symbol." (cdr obj)))
        (else
         (ls-sign-parser-error (sal-token/place (cdr obj)) "Unexpected token \"~a\"." (sal-token/data (cdr obj)))))
       (error proc msg obj)))))

(define (ls-parser/parse-string a-string)
  (set! *curr-file-name* #f)
  (with-input-from-string a-string
    parse))

(define (ls-parser/parse-file file-name)
  (set! *curr-file-name* file-name)
  (with-input-from-file file-name
    parse))

           
; fun/function const/constant init/initialization def/definition trans/transition
         
