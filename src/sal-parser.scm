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

(module sal-parser
        (include "utility.sch")
        (include "sxml-package.sch")
        (include "trace.sch")
        (include "fast-hash-table.sch")
        (include "lalr.macros")
        (import xformat queue symbol-table sal-parser-utility front-end
                sal-context scmobj sal-ast gmp-scheme)
        (export (sal-parser/set-uppercase-keywords! flag)
                (sal-parser/parse-string a-string context-provider-proc)
                (sal-parser/parse-file file-name context-provider-proc))
        )
 
;; (set! *debug-parser* #t)

(define *sal-keyword-list*
  '("type" "array" "of" "with" "lambda" "forall" "exists" "let" "in"  "if" "then" "else" "elsif" "endif" 
    "begin" "end" "rename" "to" "context" "module" "input" "output" "global" "local" "initialization" 
    "definition" "transition" "theorem" "lemma" "claim" "obligation" "observe" 
    "implements" "datatype" "state_type" "init_pred" "trans_pred" "scalarset" "ringset"
    "importing" "suffix" "prefix"))

(define *sal-uppercase-keywords?* #f)

(define (sal-parser/set-uppercase-keywords! flag)
  (set! *sal-uppercase-keywords?* flag))

(front-end/add-simple-option!
 "Misc" 
 "--uppercase-keywords" 
 "Parser only considers uppercase keywords. This option is provided for backward compatibility."
 (lambda ()
   (set! *sal-uppercase-keywords?* #t)))

(define *manager* #unspecified)
(define *curr-file-name* #f)
(define *external-context-name* #f) ;; stores the name of the contextname is the qualified name being parsed.
(define *context-provider-proc* #unspecified) ;; procedure which returns a context object of a given name
(define *prelude-ctx* #unspecified)

(define (sign-sal-parser-error place msg . args)
  (apply sign-parser-error *curr-file-name* place msg args))

(define (reset! file-name context-provider-proc)
  (set! *manager* (make-sal-parser-manager))
  (set! *curr-file-name* file-name)
  (set! *external-context-name* #f)
  (set! *context-provider-proc* context-provider-proc)
  (set! *prelude-ctx* (context-provider-proc 'prelude))) ;; <<< FIXME it will loop if the prelude is a SAL file

(define (mk-place initial final)
  (make-sal-place-from initial final))

(define (mk-token id s)
  (sal-parser-manager/make-token *manager* id s))

(define (mk-invisible-token id s)
  (sal-parser-manager/make-invisible-token id s))

(define (mk-simple-token s)
  (sal-parser-manager/make-simple-token *manager* s))

(define (queue->place q)
  (if (queue/empty? q)
    #f
    (mk-place (queue/front q)
              (queue/rear q))))

(define (sal/init-lexer!)
  (for-each (lambda (word)
              (putprop! (string->symbol (string-upcase word)) 'sal-reserved #t))
            *sal-keyword-list*))

(sal/init-lexer!)

(define (set-context-name-for-next-id! context-name-sxml)
  (set! *external-context-name* (sxml/first-child (sxml/first-child context-name-sxml))))

(define (fix-name-tag! name qualified-name-tag name-tag)
  (if (sxml/tag-equals? name 'QUALIFIEDNAME)
    (sxml/set-tag! name qualified-name-tag)
    (sxml/set-tag! name name-tag)))

;; Convert a string representing a number in the given base to the decimal base.
;; Remark: the result is a string.
(define (to-decimal str base)
  (mpq->string (make-mpq-integer str base)))
  
(define (sign-unknown-type ID)
  (sign-sal-parser-error ID "Unknown type \"~a\"." (sal-token/data ID)))

(define *sal-lexer*
  (regular-grammar
   ((opchar1 (or #\$ #\& #\@ #\^ #\~))
    (opchar (out ("azAZ") (#\0 #\9) #\( #\) #\[ #\] #\{ #\} #\% #\, #\. #\: #\; #\# #\\ #\! #\? #\_ #\| #\space #\tab #\newline)))
   ;; comments
   ((: "%" (* (out #\newline)) #\newline)
    (sal-parser-manager/adjust-coord *manager* (the-string))
    (ignore))
   ;; ignore spaces
   ((+ (in #\space #\newline #\tab #a012 #a013))
    (sal-parser-manager/adjust-coord *manager* (the-string))
    (ignore))
   ;; string
   ((: #\" (* (: (* (out #\" #\\)) (? (: #\\ all)))) #\")
    (cons 'STRING (mk-token 'STRING (the-substring 1 (- (the-length) 1)))))
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
   ;; curly braces
   (#\{
    (cons 'LC (mk-token 'LC (the-string))))
   (#\}
    (cons 'RC (mk-token 'RC (the-string))))
   ;; record type delimiters
   ("[#"
    (cons 'RECS (mk-token 'RECS (the-string))))
   ("#]"
    (cons 'RECE (mk-token 'RECE (the-string))))
   ;; record literal delimiters
   ("(#"
    (cons 'RECEXS (mk-token 'RECS (the-string))))
   ("#)"
    (cons 'RECEXE (mk-token 'RECE (the-string))))
   ;; punctuation
   (#\.
    (cons 'DOT (mk-token 'DOT (the-string))))
   (#\,
    (cons 'COMMA (mk-token 'COMMA (the-string))))
   (#\:
    (cons 'CLN (mk-token 'CLN (the-string))))
   (#\;
    (cons 'SEMI (mk-token 'SEMI (the-string))))
   (#\!
    (cons 'BANG (mk-token 'BANG (the-string))))
   (#\|
    (cons 'VBAR (mk-token 'VBAR (the-string))))
   ;; numeral
   ((: (+ digit))
    (cons 'NUMERAL (mk-token 'NUMERAL (the-string))))
   ;; Design decision: I have to convert the hex and binary
   ;; number here, because the SAL XML format doesn't support them.
   ;; hex number
   ((: "0" (or "x" "X") (+ xdigit))
    (cons 'NUMERAL (mk-token 'NUMERAL (to-decimal (the-substring 2 (the-length)) 16))))
   ;; binary number
   ((: "0" (or "b" "B") (+ (or "0" "1")))
    (cons 'NUMERAL (mk-token 'NUMERAL (to-decimal (the-substring 2 (the-length)) 2))))
   (":="
    (cons 'ASSIGN (mk-token 'ASSIGN (the-string))))
   (".."
    (cons 'DOTDOT (mk-token 'DOTDOT (the-string))))
   (#\=
    (cons 'EQ (mk-token 'EQ (the-string))))
   ("=>"
    (cons 'IMPLIES (mk-token 'IMPLIES (the-string))))
   (#\/
    (cons 'DIV (mk-token 'DIV (the-string))))
   ("div"
    (cons 'IDIV (mk-token 'IDIV (the-string))))
   ("DIV"
    (cons 'IDIV (mk-token 'IDIV (the-string))))
   ("mod"
    (cons 'MOD (mk-token 'MOD (the-string))))
   ("MOD"
    (cons 'MOD (mk-token 'MOD (the-string))))
   ("/="
    (cons 'NEQ (mk-token 'NEQ (the-string))))
   ("||"
    (cons 'SYNC (mk-token 'SYNC (the-string))))
   ("[]"
    (cons 'ASYNC (mk-token 'ASYNC (the-string))))
   ((or "or" "OR")
    (cons 'OR (mk-token 'OR (the-string))))
   ((or "and" "AND")
    (cons 'AND (mk-token 'AND (the-string))))
   ((or "not" "NOT")
    (cons 'NOT (mk-token 'NOT (the-string))))
   ((or "xor" "XOR")
    (cons 'XOR (mk-token 'XOR (the-string))))
   (#\+
    (cons 'PLUS (mk-token 'PLUS (the-string))))
   ("->"
    (cons 'ARROW (mk-token 'ARROW (the-string))))
   ("-->"
    (cons 'LONGARROW (mk-token 'LONGARROW (the-string))))
   ("-"
    (cons 'MINUS (mk-token 'MINUS (the-string))))
   ("*"
    (cons 'MULT (mk-token 'MULT (the-string))))
   ("<"
    (cons 'LT (mk-token 'LT (the-string))))
   ("<="
    (cons 'LE (mk-token 'LE (the-string))))
   ("<=>"
    (cons 'IFF (mk-token 'IFF (the-string))))
   (">"
    (cons 'GT (mk-token 'GT (the-string))))
   (">="
    (cons 'GE (mk-token 'GE (the-string))))
   ("|-"
    (cons 'TURNSTILE (mk-token 'TURNSTILE (the-string))))
   ("_"
    (cons 'UNBOUNDED (mk-token 'UNBOUNDED (the-string))))
   ("\'"
    (cons 'QUOTE (mk-token 'QUOTE (the-string))))
   ("@BTE@"
    (cons 'BTE (mk-invisible-token 'BTE (the-string))))
   ("@BTT@"
    (cons 'BTT (mk-invisible-token 'BTT (the-string))))
   ("@BTM@"
    (cons 'BTM (mk-invisible-token 'BTM (the-string))))
   ("@BTA@"
    (cons 'BTA (mk-invisible-token 'BTA (the-string))))
   ("@BTI@"
    (cons 'BTI (mk-invisible-token 'BTI (the-string))))
   ;; identifiers
   ((or (: (or alpha #\_) (* (or alpha digit #\? #\_)))
        (: opchar1 (* opchar)))
    (let* ((string   (the-string))
           (symbol   (the-symbol))
           (upsymbol (string->symbol (string-upcase string))))
      (cond
       ;; keywords...
       ((getprop symbol 'sal-reserved)
        (cons upsymbol (mk-token upsymbol (the-symbol))))
       ((and (not *sal-uppercase-keywords?*) (getprop upsymbol 'sal-reserved))
        (cons upsymbol (mk-token upsymbol (the-symbol))))
       (else
        ;; (print (the-symbol) " ID")
        (cons 'IDENTIFIER (mk-token 'IDENTIFIER (the-symbol)))))))
   ;; error
   (else
    (let ((c (the-failure)))
      (if (eof-object? c)
        c
        (cons 'ERROR (sal-parser-manager/error-token *manager*)))))))

(define (mk-identifier token)
  (template->sxml (IDENTIFIER PLACE: ,(sal-token/place token)
                              ,(sal-token/data token))))

(define (mk-numeral-core num-str place)
  (template->sxml (NUMERAL PLACE: ,place
                           ,num-str)))

(define (mk-numeral numeral-token)
  (mk-numeral-core (sal-token/data numeral-token) (sal-token/place numeral-token)))

(define (mk-vardecl ids type)
  (let ((result (make-queue)))
    (let loop ((ids ids))
         (cond 
          ((null? ids)
           (queue->list result))
          (else
           (let* ((curr-id (car ids))
                  (curr-var-decl (template->sxml (VARDECL 
                                                  PLACE: ,(mk-place curr-id type)
                                                  CHAIN: ,(if (null? ids) "NO" "YES") 
                                                  ,curr-id
                                                  ,type))))
             (queue/insert! result curr-var-decl)
             (loop (cdr ids))))))))

(define (mk-vardecls vardecl-queue)
  (template->sxml (VARDECLS PLACE: ,(queue->place vardecl-queue)
                            ,@(queue->list vardecl-queue))))

(define (mk-const-function-type vardecl-queue result-type)
  (let* ((domain-types (map (sxml/match-lambda-or-fail 
                             ((VARDECL ?- ?type)
                              type))
                            (queue->list vardecl-queue)))
         (domain-type (if (= (length domain-types) 1)
                        (car domain-types)
                        (template->sxml (TUPLETYPE PLACE: ,(queue->place vardecl-queue) 
                                                   ,@domain-types)))))
    (template->sxml (FUNCTIONTYPE ,domain-type ,result-type))))

(define (mk-infix-app op-token e1 e2)
  (template->sxml (APPLICATION PLACE: ,(mk-place e1 e2)
                               (NAMEEXPR PLACE: ,(sal-token/place op-token)
                                         ,(to-symbol (sal-token/data op-token)))
                               (TUPLELITERAL PLACE: ,(mk-place e1 e2)
                                             ,e1
                                             ,e2))))
(define (mk-unary-app op-token expr)
  (template->sxml (APPLICATION PLACE: ,(mk-place op-token expr)
                               (NAMEEXPR PLACE: ,(sal-token/place op-token)
                                         ,(to-symbol (sal-token/data op-token)))
                               ,expr)))

(define (inc-num-paren-attr! sxml)
  (let ((num-paren (sxml/attribute sxml 'PARENS)))
    (if num-paren
      (sxml/set-attribute! sxml 'PARENS (+ num-paren 1))
      (sxml/set-attribute! sxml 'PARENS 1))))

(define (var-decl+->index-var-decl var-decl+)
  (unless (= (queue/length var-decl+) 1)
    (sign-sal-parser-error var-decl+ "A single variable declaration is expected, but ~a declarations were provided." (queue/length var-decl+)))
  (let ((var-decl (queue/front var-decl+)))
    (sxml/match-or-fail var-decl
      ((VARDECL ?- ?type)
       (check-if-index-type type)))
    ;;(sxml/set-tag! var-decl 'INDEXVARDECL)
    var-decl))

(define (check-if-index-type type)
  (unless (sxml/tag-equals*? type '(TYPENAME QUALIFIEDTYPENAME SUBRANGE))
    (sign-sal-parser-error type "Invalid index type. Type must be a subrange or a type name.")))

(define (type-expr->type type-expr)
  (case (sxml/tag type-expr)
    ((SETPREDEXPRESSION)
     (template->sxml (SUBTYPE (<== type-expr)
                              ,type-expr)))
    ((QUALIFIEDNAMEEXPR)
     (sxml/set-tag! type-expr 'QUALIFIEDTYPENAME)
     type-expr)
    ((NAMEEXPR)
     (sxml/set-tag! type-expr 'TYPENAME)
     type-expr)
    ((TUPLETYPE RECORDTYPE ARRAYTYPE SUBRANGE FUNCTIONTYPE STATETYPE TYPENAME QUALIFIEDTYPENAME SUBTYPE)
     type-expr)
    (else
     (sign-sal-parser-error type-expr "Type expected."))))

(define (actual->expr actual)
  (case (sxml/tag actual)
    ((RECORDTYPE ARRAYTYPE SUBRANGE FUNCTIONTYPE STATETYPE TYPENAME QUALIFIEDTYPENAME SUBTYPE)
     (sign-sal-parser-error actual "Expression expected."))
    (else
     actual)))

(define (fix-actuals ctx-name actual-list ctx-place)
  (let* ((ctx-obj (*context-provider-proc* ctx-name))
         (param-list (slot-value ctx-obj :params)))
    (unless (= (length actual-list) (length param-list))
      (sign-sal-parser-error ctx-place "Wrong number of actual parameters."))
    (map (lambda (actual param)
           (if (instance-of? param <sal-type-param-decl>)
             (type-expr->type actual)
             (actual->expr actual)))
         actual-list
         param-list)))

(define (mk-context-name ctx-name ctx-place actual-queue)
  (if actual-queue
    (template->sxml (CONTEXTNAME PLACE: ,ctx-place
                                 (IDENTIFIER PLACE: ,ctx-place ,ctx-name)
                                 (ACTUALPARAMETERS PLACE: ,(queue->place actual-queue)
                                                   ,@(fix-actuals ctx-name (queue->list actual-queue) ctx-place))))
    (template->sxml (CONTEXTNAME PLACE: ,ctx-place
                                 (IDENTIFIER PLACE: ,ctx-place ,ctx-name)))))

(define (check-rename lhs)
  (when (sxml/contains-tag? lhs 'NEXTOPERATOR)
    (sign-sal-parser-error lhs "Invalid use of next operator in rename definition.")))

(define (mk-labeled-command id guarded-command)
  (sxml/set-tag! id 'LABEL)
  (sxml/match guarded-command
    ((GUARDEDCOMMAND . ?-)
     (template->sxml (LABELEDCOMMAND PLACE: ,(mk-place id guarded-command)
                                     ,id
                                     ,guarded-command)))
    ((ELSECOMMAND . ?-)
     (template->sxml (LABELEDELSECOMMAND PLACE: ,(mk-place id guarded-command)
                                         ,id
                                         ,guarded-command)))
    (?-
     (sign-sal-parser-error id "Invalid labeled command. Only guarded and else commands can be labeled."))))

(define (check-somecommands command-list)
  (let loop ((command-list command-list))
    (unless (null? command-list)
      (let ((curr-command (car command-list)))
        (when (and (not (null? (cdr command-list)))
                   (sxml/tag-equals*? curr-command '(ELSECOMMAND LABELEDELSECOMMAND)))
          (sign-sal-parser-error curr-command "Invalid occurrence of ELSE command. ELSE command must be the last command in the command list."))
        (loop (cdr command-list))))))

(define (mk-quantified-expr quantifier-token var-decl+ expr)
  (template->sxml (QUANTIFIEDEXPRESSION PLACE: ,(mk-place quantifier-token expr)
                                        (QUANTIFIER PLACE: ,(sal-token/place quantifier-token)
                                                    ,(to-symbol (sal-token/id quantifier-token)))
                                        ,(mk-vardecls var-decl+)
                                        ,expr)))

(define (mk-qualified-name tag context-name token-id)
  (template->sxml (,tag PLACE: ,(mk-place context-name token-id)
                        ,(mk-identifier token-id)
                        ,context-name)))

(define (mk-name tag toke-id)
  (template->sxml (,tag PLACE: ,(sal-token/place toke-id)
                        ,(to-symbol (sal-token/data toke-id)))))

(define (apply-suffix expr expr-suffix-list)
  (let loop ((expr expr)
             (expr-suffix-list expr-suffix-list))
    (if (null? expr-suffix-list)
      expr
      (let* ((curr-suffix (car expr-suffix-list))
             (curr-place (mk-place expr curr-suffix))
             (new-expr (sxml/match-or-fail curr-suffix
                         ((RECORDACCESS ?id)
                          (template->sxml (RECORDSELECTION PLACE: ,curr-place
                                                           ,expr
                                                           ,id)))
                         ((TUPLEACCESS ?num)
                          (template->sxml (TUPLESELECTION PLACE: ,curr-place
                                                          ,expr
                                                          ,num)))
                         ((ARRAYACCESS ?idx)
                          (template->sxml (ARRAYSELECTION PLACE: ,curr-place
                                                          ,expr
                                                          ,idx)))
                         ;; application
                         ((TUPLELITERAL . ?args)
                          (template->sxml (APPLICATION PLACE: ,curr-place
                                                       ,expr
                                                       ,(if (null? (cdr args))
                                                          (car args)
                                                          curr-suffix))))
                         )))
        (loop new-expr (cdr expr-suffix-list))))))

(define (compute-update-depth ex)
  (if (sxml/tag-equals*? ex '(APPLICATION RECORDSELECTION TUPLESELECTION ARRAYSELECTION))
    (+ 1 (compute-update-depth (sxml/first-child ex)))
    0))
  
(define (mk-lhs expr-id access-queue quote?)
  (let* ((name (mk-name 'NAMEEXPR expr-id))
         (base (if quote? 
                 (template->sxml (NEXTOPERATOR PLACE: ,(sal-token/place expr-id)
                                                        ,name))
                 name)))
    (apply-suffix base (queue->list access-queue))))

(define (mk-simple-expr expr-prefix expr-suffix-queue)
  (apply-suffix expr-prefix (queue->list expr-suffix-queue)))

(define (mk-set-pred identifier type expr LC RC)
  (template->sxml (SETPREDEXPRESSION PLACE: ,(mk-place LC RC)
                                     ,identifier
                                     ,type
                                     ,expr)))

(define (mk-conditional cond-expr then-expr elsif* else-expr start-token end-token)
  (let ((else-expr (let loop ((elsif* elsif*)
                              (curr-else else-expr))
                     (if (null? elsif*)
                       curr-else
                       (let ((curr-elsif (car elsif*)))
                         (sxml/match-or-fail curr-elsif
                           ((ELSIF ?curr-cond ?curr-then)
                            (loop (cdr elsif*)
                                  (template->sxml (CONDITIONAL PLACE: ,(mk-place curr-elsif end-token)
                                                               ELSIF: "YES"
                                                               ,curr-cond
                                                               ,curr-then
                                                               ,curr-else))))))))))
    (template->sxml (CONDITIONAL PLACE: ,(mk-place start-token end-token)
                                 ,cond-expr
                                 ,then-expr
                                 ,else-expr))))

(define (check-if-var-decls-only type-or-var-decl+)
  (for-each (lambda (type-or-var-decl)
              (unless (sxml/tag-equals? type-or-var-decl 'VARDECL)
                (sign-sal-parser-error type-or-var-decl "Variable declaration expected.")))
            (queue->list type-or-var-decl+)))

(define (mk-type-declaration identifier type-def)
  (template->sxml (TYPEDECLARATION PLACE: ,(mk-place identifier type-def)
                                   ,identifier
                                   ,type-def)))

(define *sal-parser*
  (lalr-grammar
   (
    (left: LP LB)
    (left: DOT)
    (left: UMINUS)
    (left: MULT DIV IDIV MOD)
    (left: PLUS MINUS)
    (left: WITH)
    (left: EQ NEQ LT GT LE GE)
    (none: NOT)
    (left: AND)
    (left: OR)
    (left: XOR)
    (right: IMPLIES)
    (left: IFF)
    (left: SYNC)
    (left: ASYNC) 
    (left: MODULE_WITH)
    (left: CLN)
    (left: IN)
    IDENTIFIER
    RP RB LC RC RECS RECE RECEXS RECEXE COMMA SEMI BANG VBAR
    NUMERAL ASSIGN DOTDOT ARROW LONGARROW TURNSTILE UNBOUNDED 
    TYPE ARRAY OF LAMBDA FORALL EXISTS LET IF THEN  ELSE ELSIF ENDIF  
    BEGIN END RENAME TO CONTEXT MODULE INPUT OUTPUT GLOBAL LOCAL INITIALIZATION
    DEFINITION TRANSITION THEOREM LEMMA CLAIM OBLIGATION OBSERVE QUOTE
    IMPLEMENTS DATATYPE STATE_TYPE INIT_PRED TRANS_PRED
    SCALARSET RINGSET STRING
    BTE BTT BTA BTI BTM
    IMPORTING SUFFIX PREFIX
    )
   [main
    ((context)
     context)
    ((BTE expr)
     expr)
    ((BTM module)
     module)
    ((BTT type-expr)
     ;; fix
     type-expr)
    ((BTA top-assertion-name)
     top-assertion-name)
    ((BTI import-def)
     import-def)
    ]
   [top-assertion-name
    ((name)
     (fix-name-tag! name 'QUALIFIEDASSERTIONNAME 'ASSERTIONAME)
     name)]
   [context
    ((identifier context-parameters CLN CONTEXT EQ context-body)
     (template->sxml (CONTEXT PLACE: ,(mk-place identifier context-body)
                              ,identifier
                              ,context-parameters
                              ,context-body)))]
   [context-parameters
    (()
     (template->sxml (PARAMETERS)))
    ((LC opt-semi param+ opt-semi RC)
     (template->sxml (PARAMETERS PLACE: ,(mk-place LC RC)
                                 ,@(queue->list param+))))
    ]
   [opt-semi
    (()
     )
    ((SEMI)
     )]
   [param+
    ((param)
     [assert (param) (list? param)]
     (list->queue param))
    ((param+ comma-or-semi param)
     [assert (param) (list? param)]
     (queue/append! param+ param))]
   [comma-or-semi
    ((COMMA)
     )
    ((SEMI)
      )]
   [param
    ((identifier+ CLN TYPE)
     (map (lambda (id)
            (template->sxml (TYPEDECL (<== id)
                                      ,id)))
          (queue->list identifier+)))
    ((var-decl)
     var-decl)]
   [identifier
    ((IDENTIFIER)
     (mk-identifier IDENTIFIER))]
   [identifier+
    ((identifier)
     (make-queue identifier))
    ((identifier+ COMMA identifier)
     (queue/insert! identifier+ identifier))]
   [var-decl+
    ((var-decl)
     (apply make-queue var-decl))
    ((var-decl+ COMMA var-decl)
     (queue/append! var-decl+ var-decl))]
   [var-decl
    ((identifier+ CLN type-expr)
     (let ((type type-expr)) ;; todo
       (mk-vardecl (queue->list identifier+) type)))
    ]
   [expr 
    ;; iff expressions
    ((expr@e1 IFF expr@e2)
     (mk-infix-app IFF e1 e2))
    ;; implies expressions
    ((expr@e1 IMPLIES expr@e2)
     (mk-infix-app IMPLIES e1 e2))
    ;; or expressions
    ((expr@e1 OR expr@e2)
     (mk-infix-app OR e1 e2))
    ((expr@e1 XOR expr@e2)
     (mk-infix-app XOR e1 e2))
    ;; and expressions
    ((expr@e1 AND expr@e2)
     (mk-infix-app AND e1 e2))
    ;; not expressions
    ((NOT expr)
     (mk-unary-app NOT expr))
    ;; eq expressions
    ((expr@e1 EQ expr@e2)
     (mk-infix-app EQ e1 e2))
    ((expr@e1 NEQ expr@e2)
     (mk-infix-app NEQ e1 e2))
    ;; relational expressions
    ((expr@e1 GT expr@e2)
     (mk-infix-app GT e1 e2))
    ((expr@e1 GE expr@e2)
     (mk-infix-app GE e1 e2))
    ((expr@e1 LT expr@e2)
     (mk-infix-app LT e1 e2))
    ((expr@e1 LE expr@e2)
     (mk-infix-app LE e1 e2))
    ;; additive expressions
    ((expr@e1 PLUS expr@e2)
     (mk-infix-app PLUS e1 e2))
    ((expr@e1 MINUS expr@e2)
     (mk-infix-app MINUS e1 e2))
    ;; multiplicative expressions
    ((expr@e1 MULT expr@e2)
     (mk-infix-app MULT e1 e2))
    ((expr@e1 DIV expr@e2)
     (mk-infix-app DIV e1 e2))
    ((expr@e1 IDIV expr@e2)
     (mk-infix-app IDIV e1 e2))
    ((expr@e1 MOD expr@e2)
     (mk-infix-app MOD e1 e2))
    ;; unary minus
    ((%prec (MINUS expr) UMINUS)
     (mk-unary-app MINUS expr))
    ((expr DOT numeral)
     (template->sxml (TUPLESELECTION PLACE: ,(mk-place expr numeral)
                                     ,expr
                                     ,numeral)))
    ((expr DOT identifier)
     (template->sxml (RECORDSELECTION PLACE: ,(mk-place expr identifier)
                                      ,expr
                                      ,identifier)))
    ((expr@array LB expr@idx RB)
     (template->sxml (ARRAYSELECTION PLACE: ,(mk-place array RB)
                                     ,array
                                     ,idx)))
    ((expr@target WITH access-argument+ ASSIGN expr@new-value)
     (let ((pos (apply-suffix target (queue->list access-argument+)))
           (depth (compute-update-depth target)))
       (template->sxml (UPDATEEXPRESSION PLACE: ,(mk-place WITH new-value)
                                         (DEPTH ,(integer->string depth))
                                         ,pos
                                         ,new-value))))
    ((expr LP expr+ RP)
     (template->sxml (APPLICATION PLACE: ,(mk-place expr RP)
                                  ,expr
                                  ,(if (= (queue/length expr+) 1)
                                     (queue/front expr+)
                                     (template->sxml (TUPLELITERAL PLACE: ,(mk-place LP RP)
                                                                   ,@(queue->list expr+)))))))
    ((name)
     (fix-name-tag! name 'QUALIFIEDNAMEEXPR 'NAMEEXPR)
     name)
    ;; nextoperator
    ((identifier QUOTE)
     (sxml/set-tag! identifier 'NAMEEXPR)
     (template->sxml (NEXTOPERATOR PLACE: ,(mk-place identifier QUOTE)
                                   ,identifier)))
    ;; numeral
    ((numeral)
     numeral)
    ;; float
    ((float)
     float)
    ;; string
    ((STRING)
     (template->sxml (STRINGEXPR PLACE: ,(sal-token/place STRING)
                                 ,(sal-token/data STRING))))
    ;; trans expr
    ((TRANS_PRED LP module RP)
     (template->sxml (MODTRANS PLACE: ,(mk-place TRANS_PRED RP)
                               ,module)))
    ;; init expr
    ((INIT_PRED LP module RP)
     (template->sxml (MODINIT PLACE: ,(mk-place INIT_PRED RP)
                              ,module)))
    ;; parenthesis...
    ((LP expr RP)
     (inc-num-paren-attr! expr)
     expr)
    ;; lambda expression
    ((LAMBDA LP var-decl+ RP CLN expr)
     (template->sxml (LAMBDAABSTRACTION PLACE: ,(mk-place LAMBDA expr)
                                        ,(mk-vardecls var-decl+)
                                        ,expr)))
    ;; forall expression
    ((FORALL LP var-decl+ RP CLN expr)
     (mk-quantified-expr FORALL var-decl+ expr))
    ;; exists expression
    ((EXISTS LP var-decl+ RP CLN expr)
     (mk-quantified-expr EXISTS var-decl+ expr))
    ;; let expression
    ((LET let-declaration+ IN expr)
     (template->sxml (LETEXPRESSION PLACE: ,(mk-place LET expr)
                                    (LETDECLARATIONS PLACE: ,(queue->place let-declaration+)
                                                     ,@(queue->list let-declaration+))
                                    ,expr)))
    ;; array literal
    ((LB@b LB identifier CLN type-expr RB expr RB@e) 
     (let ((type (type-expr->type type-expr)))
       (check-if-index-type type)
       (template->sxml (ARRAYLITERAL PLACE: ,(mk-place b e)
                                     (VARDECL PLACE: ,(mk-place identifier type-expr)
                                              ,identifier
                                              ,type)
                                     ,expr))))
    ;; record literal
    ((RECEXS record-entry+ RECEXE)
     (template->sxml (RECORDLITERAL PLACE: ,(mk-place RECEXS RECEXE)
                                    ,@(queue->list record-entry+))))
    ;; tuple literal
    ((LP expr COMMA expr+ RP)
     (template->sxml (TUPLELITERAL PLACE: ,(mk-place LP RP)
                                   ,expr
                                   ,@(queue->list expr+))))
    ;; set expression
    ((LC identifier CLN type-expr VBAR expr RC)
     (let ((type (type-expr->type type-expr)))
       (mk-set-pred identifier type expr LC RC)))
    ;; set list expression
    ((LC expr+ RC)
     (template->sxml (SETLISTEXPRESSION PLACE: ,(mk-place LC RC)
                                        ,@(queue->list expr+))))
    
    ;; conditional
    ((IF expr@cond THEN expr@then elsif* ELSE expr@else ENDIF)
     (mk-conditional cond then elsif* else IF ENDIF))

    ]
   [elsif*
    (() 
     '())
    ((elsif+)
     elsif+)]
   [elsif+
    ((elsif)
     (cons elsif '()))
    ((elsif+ elsif)
     (cons elsif elsif+))]
   [elsif
    ((ELSIF expr@cond THEN expr@then)
     (template->sxml (ELSIF PLACE: ,(mk-place ELSIF then)
                            ,cond
                            ,then)))]
   [record-entry+
    ((record-entry)
     (make-queue record-entry))
    ((record-entry+ COMMA record-entry)
     (queue/insert! record-entry+ record-entry))]
   [record-entry
    ((identifier ASSIGN expr)
     (template->sxml (RECORDENTRY PLACE: ,(mk-place identifier expr)
                                  ,identifier
                                  ,expr)))]
   [let-declaration+
    ((let-declaration)
     (make-queue let-declaration))
    ((let-declaration+ COMMA let-declaration)
     (queue/insert! let-declaration+ let-declaration))]
   [let-declaration
    ((identifier CLN type-expr EQ expr)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (LETDECLARATION PLACE: ,(mk-place identifier expr)
                                       ,identifier
                                       ,type
                                       ,expr))))]
   [access-argument+
    ((access-argument)
     (make-queue access-argument))
    ((access-argument+ access-argument)
     (queue/insert! access-argument+ access-argument))]
   [access-argument
    ((LB expr RB)
     (template->sxml (ARRAYACCESS PLACE: ,(mk-place LB RB)
                                  ,expr)))
    ((DOT identifier)
     (template->sxml (RECORDACCESS PLACE: ,(mk-place DOT identifier)
                                   ,identifier)))
    ((DOT numeral)
     (template->sxml (TUPLEACCESS PLACE: ,(mk-place DOT numeral)
                                  ,numeral)))
    ((LP expr+ RP)
     (template->sxml (TUPLELITERAL PLACE: ,(mk-place LP RP)
                                   ,@(queue->list expr+))))]
   [module
    ((module-instance)
     module-instance)
    ;; asynchronous composition
    ((module@m1 ASYNC module@m2)
     (template->sxml (ASYNCHRONOUSCOMPOSITION PLACE: ,(mk-place m1 m2)
                                              ,m1
                                              ,m2)))
    ;; synchronous composition 
    ((module@m1 SYNC module@m2)
     (template->sxml (SYNCHRONOUSCOMPOSITION PLACE: ,(mk-place m1 m2)
                                             ,m1
                                             ,m2)))
    ;; multi asynchronous composition
    ((LP ASYNC LP var-decl+ RP CLN module RP)
     (let ((var-decl (var-decl+->index-var-decl var-decl+)))
       (template->sxml (MULTIASYNCHRONOUS PLACE: ,(mk-place LP RP)
                                          ,var-decl
                                          ,module))))
    ;; multi synchronous composition
    ((LP SYNC LP var-decl+ RP CLN module RP)
     (let ((var-decl (var-decl+->index-var-decl var-decl+)))
       (template->sxml (MULTISYNCHRONOUS PLACE: ,(mk-place LP RP)
                                         ,var-decl
                                         ,module))))
    ;; hiding module
    ((LOCAL identifier+ IN module)
     (template->sxml (HIDING PLACE: ,(mk-place LOCAL module)
                             (IDENTIFIERS PLACE: ,(queue->place identifier+)
                                          ,@(queue->list identifier+))
                               ,module)))
    ;; new output module
    ((OUTPUT identifier+ IN module)
     (template->sxml (NEWOUTPUT PLACE: ,(mk-place OUTPUT module)
                                (IDENTIFIERS PLACE: ,(queue->place identifier+)
                                             ,@(queue->list identifier+))
                                ,module)))
    ;; renaming
    ((RENAME rename+ IN module)
     (template->sxml (RENAMING PLACE: ,(mk-place RENAME module)
                               (RENAMES PLACE: ,(queue->place rename+)
                                        ,@(queue->list rename+))
                               ,module)))
    ;; with module
    ((%prec (WITH new-var-decl+ module) MODULE_WITH)
     (template->sxml (WITHMODULE PLACE: ,(mk-place WITH module)
                                 (NEWVARDECLS PLACE: ,(queue->place new-var-decl+)
                                              ,@(queue->list new-var-decl+))
                                 ,module)))
    ;; observe module
    ((OBSERVE module@m1 WITH module@m2)
     (template->sxml (OBSERVEMODULE PLACE: ,(mk-place m1 m2)
                                      ,m1
                                      ,m2)))
    ;; base module
    ((base-module)
     base-module)
    ;; parenthesis...
    ((LP module RP)
     (inc-num-paren-attr! module)
     module)]
   [module-instance
    ((name)
     (fix-name-tag! name 'QUALIFIEDMODULENAME 'MODULENAME)
     (let ((module-name name))
       (template->sxml (MODULEINSTANCE (<== module-name)
                                       ,module-name
                                       (MODULEACTUALS)))))
    ((name LB expr+ RB)
      (fix-name-tag! name 'QUALIFIEDMODULENAME 'MODULENAME)
      (let ((module-name name))
        (template->sxml (MODULEINSTANCE PLACE: ,(mk-place module-name RB)
                                        ,module-name
                                        (MODULEACTUALS PLACE: ,(mk-place LB RB)
                                                       ,@(queue->list expr+))))))]
   [base-module
    ((BEGIN base-declaration* END)
     (template->sxml (BASEMODULE PLACE: ,(mk-place BEGIN END)
                                 ,@(queue->list base-declaration*))))]
   [base-declaration*
    (() 
     (make-queue))
    ((base-declaration* base-declaration)
     (queue/insert! base-declaration* base-declaration))]
   [base-declaration
    ((input-decl)
     input-decl)
    ((output-decl)
     output-decl)
    ((global-decl)
     global-decl)
    ((local-decl)
     local-decl)
    ((def-decl)
     def-decl)
    ((init-decl)
     init-decl)
    ((trans-decl)
     trans-decl)]
   [def-decl
     ((DEFINITION definition+)
      (template->sxml (DEFDECL PLACE: ,(mk-place DEFINITION (queue/rear definition+))
                        ,@(queue->list definition+))))
     ((DEFINITION definition+ SEMI)
      (template->sxml (DEFDECL PLACE: ,(mk-place DEFINITION SEMI)
                        ,@(queue->list definition+))))]
   [init-decl
    ((INITIALIZATION definition-or-command+)
     (template->sxml (INITDECL PLACE: ,(mk-place INITIALIZATION (queue/rear definition-or-command+))
                               ,@(queue->list definition-or-command+))))
    ((INITIALIZATION definition-or-command+ SEMI)
     (template->sxml (INITDECL PLACE: ,(mk-place INITIALIZATION SEMI)
                               ,@(queue->list definition-or-command+))))]
   [trans-decl
    ((TRANSITION definition-or-command+)
     (template->sxml (TRANSDECL PLACE: ,(mk-place TRANSITION (queue/rear definition-or-command+))
                                ,@(queue->list definition-or-command+))))
    ((TRANSITION definition-or-command+ SEMI)
     (template->sxml (TRANSDECL PLACE: ,(mk-place TRANSITION SEMI)
                                ,@(queue->list definition-or-command+))))]
   [definition+
     ((definition)
      (make-queue definition))
     ((definition+ SEMI definition)
      (queue/insert! definition+ definition))]
   [definition
     ((simple-definition)
      simple-definition)
     ((forall-definition)
      forall-definition)]
   [simple-definition
    ((lhs rhs-definition)
     (template->sxml (SIMPLEDEFINITION PLACE: ,(mk-place lhs rhs-definition)
                                       ,lhs
                                       ,rhs-definition)))]
   [rhs-definition
    ((rhs-expression)
     rhs-expression)
    ((rhs-selection)
     rhs-selection)]
   [rhs-expression
    ((EQ expr)
     (template->sxml (RHSEXPRESSION PLACE: ,(mk-place EQ expr)
                                    ,expr)))]
   [rhs-selection
    ((IN expr)
     (template->sxml (RHSSELECTION PLACE: ,(mk-place IN expr)
                                   ,expr)))]
   [forall-definition
    ((LP@b FORALL LP var-decl+ RP CLN definition+ RP@e)
     (template->sxml (FORALLDEFINITION PLACE: ,(mk-place b e)
                                       ,(mk-vardecls var-decl+)
                                       ,@(queue->list definition+))))
    ]
   [definition-or-command+
     ((definition-or-command)
      (make-queue definition-or-command))
     ((definition-or-command+ SEMI definition-or-command)
      (queue/insert! definition-or-command+ definition-or-command))]
   [definition-or-command
     ((definition)
      definition)
     ((somecommands)
      somecommands)]
   [somecommands
    ((LB somecommand+ RB)
     (check-somecommands (queue->list somecommand+))
     (template->sxml (SOMECOMMANDS PLACE: ,(mk-place LB RB)
                                   ,@(queue->list somecommand+))))]
   [somecommand+
    ((somecommand)
     (make-queue somecommand))
    ((somecommand+ ASYNC somecommand)
     (queue/insert! somecommand+ somecommand))]
   [somecommand
    ;; labeled command
    ((identifier CLN guarded-command)
     (mk-labeled-command identifier guarded-command))
    ((identifier CLN multi-command)
     (sign-sal-parser-error identifier "Only guarded commands can be labeled. Move the identifier to the nested guarded command. For instance, the command:\n\n  label1: ([] (idx : BOOLEAN): g(idx) --> x' = x + 1)\n\nMust be written as:\n\n  ([] (idx : BOOLEAN):\n    label1: g(idx) --> x' = x + 1)"))
    ((guarded-command)
     guarded-command)
    ((multi-command)
     multi-command)]
   [multi-command
    ((LP@b ASYNC LP var-decl+ RP CLN somecommand RP@e)
     (when (sxml/tag-equals*? somecommand '(LABELEDELSECOMMAND ELSECOMMAND))
       (sign-sal-parser-error b "Invalid multi-command. ELSE commands cannot be nested in multi-commands."))
     (template->sxml (MULTICOMMAND PLACE: ,(mk-place b e)
                                   ,(mk-vardecls var-decl+)
                                   ,somecommand)))]
   [guarded-command
    ((ELSE LONGARROW)
     (template->sxml (ELSECOMMAND PLACE: ,(mk-place ELSE LONGARROW)
                                  (ASSIGNMENTS PLACE: ,(sal-token/place LONGARROW)))))
    ((ELSE LONGARROW assignments)
     (template->sxml (ELSECOMMAND PLACE: ,(mk-place ELSE assignments)
                                  ,assignments)))
    ((expr LONGARROW)
     (template->sxml (GUARDEDCOMMAND PLACE: ,(mk-place expr LONGARROW)
                                     (GUARD (<== expr)
                                            ,expr)
                                     (ASSIGNMENTS PLACE: ,(sal-token/place LONGARROW)))))
    ((expr LONGARROW assignments)
     (template->sxml (GUARDEDCOMMAND PLACE: ,(mk-place expr assignments)
                                     (GUARD (<== expr)
                                            ,expr)
                                     ,assignments)))]
   [assignments
    ((assign-simple-definition+ SEMI)
     (template->sxml (ASSIGNMENTS PLACE: ,(mk-place (queue/front assign-simple-definition+) SEMI)
                                  ,@(queue->list assign-simple-definition+))))
    ((assign-simple-definition+)
     (template->sxml (ASSIGNMENTS PLACE: ,(queue->place assign-simple-definition+)
                                  ,@(queue->list assign-simple-definition+))))
    ]
   [input-decl
    ((INPUT var-decl+)
     (template->sxml (INPUTDECL PLACE: ,(mk-place INPUT (queue/rear var-decl+))
                                ,@(queue->list var-decl+))))]
   [output-decl
    ((OUTPUT var-decl+)
     (template->sxml (OUTPUTDECL PLACE: ,(mk-place OUTPUT (queue/rear var-decl+))
                                 ,@(queue->list var-decl+))))]
   [global-decl
    ((GLOBAL var-decl+)
     (template->sxml (GLOBALDECL PLACE: ,(mk-place GLOBAL (queue/rear var-decl+))
                                 ,@(queue->list var-decl+))))]
   [local-decl
    ((LOCAL var-decl+)
     (template->sxml (LOCALDECL PLACE: ,(mk-place LOCAL (queue/rear var-decl+))
                                ,@(queue->list var-decl+))))]
   [assign-definition
    ((simple-definition)
     simple-definition)
    ((forall-definition)
     forall-definition)]
   [assign-simple-definition+
    ((assign-definition)
     (make-queue assign-definition))
    ((assign-simple-definition+ SEMI assign-definition)
     (queue/insert! assign-simple-definition+ assign-definition))]
   [new-var-decl+
    ((new-var-decl)
     (make-queue new-var-decl))
    ((new-var-decl+ SEMI new-var-decl)
     (queue/insert! new-var-decl+ new-var-decl))]
   [new-var-decl
    ((input-decl)
     input-decl)
    ((output-decl)
     output-decl)
    ((global-decl)
     global-decl)]
   [rename+
    ((rename)
     (make-queue rename))
    ((rename+ COMMA rename)
     (queue/insert! rename+ rename))]
   [rename
    ((lhs@l1 TO lhs@l2)
     (check-rename l1)
     (check-rename l2)
     (template->sxml (RENAME PLACE: ,(mk-place l1 l2)
                             ,l1
                             ,l2)))]
   [lhs
    ((IDENTIFIER opt-quote access*)
     (mk-lhs IDENTIFIER access* opt-quote))]
   [opt-quote
    (() #f)
    ((QUOTE) #t)]
   [access*
    (()
     (make-queue))
    ((access+)
     access+)]
   [access+
    ((access)
     (make-queue access))
    ((access+ access)
     (queue/insert! access+ access))]
   [access
    ((LB expr RB)
     (template->sxml (ARRAYACCESS PLACE: ,(mk-place LB RB)
                                  ,expr)))
    ((DOT identifier)
     (template->sxml (RECORDACCESS PLACE: ,(mk-place DOT identifier)
                                   ,identifier)))
    ((DOT numeral)
     (template->sxml (TUPLEACCESS PLACE: ,(mk-place DOT numeral)
                                  ,numeral)))]
   [numeral
    ((NUMERAL)
     (mk-numeral NUMERAL))]
   [float
    ((NUMERAL@n DOT NUMERAL@d)
     (let* ((place (mk-place n d))
            (new-numerator (mk-numeral-core (string-append (sal-token/data n) (sal-token/data d)) place))
            (new-denominator (mk-numeral-core (string-append "1" (make-string (string-length (sal-token/data d)) #\0)) place)))
       (template->sxml (APPLICATION PLACE: ,place
                                    (NAMEEXPR PLACE: ,place '/)
                                    (TUPLELITERAL PLACE: ,place
                                                  ,new-numerator
                                                  ,new-denominator)))))]
   [context-body
    ((BEGIN declaration* END)
     (when (queue/empty? declaration*)
       (sign-sal-parser-error BEGIN "Invalid empty context."))
     (template->sxml (CONTEXTBODY PLACE: ,(mk-place BEGIN END)
                                  ,@(queue->list declaration*))))]
   [declaration*
    (()
     (make-queue))
    ((declaration* declaration SEMI)
     (queue/insert! declaration* declaration))]
   [type-declaration-prefix
    ((identifier CLN TYPE)
     (cons identifier TYPE))]
   [declaration
    ;; interpreted type declaration
    ((type-declaration-prefix EQ type-def)
     (mk-type-declaration (car type-declaration-prefix) type-def))
    ;; uninterpreted type declaration
    ((type-declaration-prefix)
     (template->sxml (TYPEDECLARATION PLACE: ,(mk-place (car type-declaration-prefix) (cdr type-declaration-prefix))
                                      ,(car type-declaration-prefix))))
    ;; context declaration
    ((identifier CLN CONTEXT EQ context-name)
     (template->sxml (CONTEXTDECLARATION PLACE: ,(mk-place identifier context-name)
                                         ,identifier
                                         ,context-name)))
    ;; parametric module declaration
    ((identifier LB var-decl+ RB CLN MODULE EQ module)
     (template->sxml (MODULEDECLARATION PLACE: ,(mk-place identifier module)
                                        ,identifier
                                        ,(mk-vardecls var-decl+)
                                        ,module)))
    ;; module declaration
    ((identifier CLN MODULE EQ module)
     (template->sxml (MODULEDECLARATION PLACE: ,(mk-place identifier module)
                                        ,identifier
                                        (VARDECLS)
                                        ,module)))
    ;; assertion declaration
    ((identifier CLN assertion-form assertion-expr)
     (template->sxml (ASSERTIONDECLARATION PLACE: ,(mk-place identifier assertion-expr)
                                           ,identifier
                                           ,assertion-form
                                           ,assertion-expr)))
    ;; function declaration
    ((identifier LP var-decl+ RP CLN type-expr EQ expr)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (CONSTANTDECLARATION PLACE: ,(mk-place identifier type)
                                            ,identifier
                                            ,(mk-const-function-type var-decl+ type)
                                            (LAMBDAABSTRACTION (<== expr)
                                                               ,(mk-vardecls var-decl+)
                                                               ,expr)))))
    ;; uninterpreted function declaration
    ((identifier LP var-decl+ RP CLN type-expr)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (CONSTANTDECLARATION PLACE: ,(mk-place identifier type)
                                            ,identifier
                                            ,(mk-const-function-type var-decl+ type)))))
    ;; constant declaration
    ((identifier CLN type-expr EQ expr)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (CONSTANTDECLARATION PLACE: ,(mk-place identifier expr)
                                            ,identifier
                                            ,type
                                            ,expr))))
    ;; uninterpreted constant declaration
    ((identifier CLN type-expr)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (CONSTANTDECLARATION PLACE: ,(mk-place identifier type)
                                            ,identifier
                                            ,type))))
    ((import-def)
     import-def)]
    [import-def
     ((IMPORTING context-ref)
      (template->sxml (IMPORT PLACE: ,(mk-place IMPORTING context-ref)
                              ,context-ref)))
     ]
    [context-ref
    ((context-name)
     context-name)
    ((context-name WITH rename-decl+)
     (template->sxml (IMPORTRENAME PLACE: ,(mk-place context-name (queue/rear rename-decl+))
                                   (RENAMEDECLS PLACE: ,(queue->place rename-decl+)
                                                ,@(queue->list rename-decl+))
                                   ,context-name)))
;     ((context-name WITH SUFFIX identifier)
;      (template->sxml (ADDSUFFIX PLACE: ,(mk-place context-name identifier)
;                                 ,identifier
;                                 ,context-name)))
;     ((context-name WITH PREFIX identifier)
;      (template->sxml (ADDPREFIX PLACE: ,(mk-place context-name identifier)
;                                 ,identifier
;                                 ,context-name)))
    ]
   [rename-decl+
    ((rename-decl)
     (make-queue rename-decl))
    ((rename-decl+ COMMA rename-decl)
     (queue/insert! rename-decl+ rename-decl))]
   [rename-decl
    ((identifier@id1 TO identifier@id2)
     (template->sxml (RENAMEDECL PLACE: ,(mk-place id1 id2)
                                 ,id1
                                 ,id2)))]
   [type-def
    ((type-expr) 
     (type-expr->type type-expr))
    ;; scalar type
    ((LC identifier+ RC)
     (template->sxml (SCALARTYPE PLACE: ,(mk-place LC RC)
                                 ,@(map (lambda (id)
                                          (sxml/set-tag! id 'SCALARELEMENT)
                                          id)
                                        (queue->list identifier+)))))
    ;; data type
    ((DATATYPE constructor+ END)
     (template->sxml (DATATYPE PLACE: ,(mk-place DATATYPE END)
                               ,@(queue->list constructor+))))
    ((SCALARSET LP expr RP)
     (template->sxml (SCALARSET PLACE: ,(mk-place SCALARSET RP)
                                ,expr)))
    ((RINGSET LP expr RP)
     (template->sxml (RINGSET PLACE: ,(mk-place RINGSET RP)
                              ,expr)))
    ]
   [name
    ((IDENTIFIER)
     (template->sxml (NAME PLACE: ,(sal-token/place IDENTIFIER)
                           ,(to-symbol (sal-token/data IDENTIFIER)))))
    ((IDENTIFIER@ctx BANG IDENTIFIER@name)
     (template->sxml (QUALIFIEDNAME PLACE: ,(mk-place ctx name)
                                    ,(mk-identifier name)
                                    ,(mk-context-name (to-symbol (sal-token/data ctx))
                                                      (sal-token/place ctx) #f))))
    ((IDENTIFIER@ctx LC actuals RC BANG IDENTIFIER@name)
     (let ((ctx-name (to-symbol (sal-token/data ctx))))
       (template->sxml (QUALIFIEDNAME PLACE: ,(mk-place ctx name)
                                      ,(mk-identifier name)
                                      ,(mk-context-name ctx-name (mk-place ctx RC) actuals)))))
    ]
   [actuals
    ((opt-semi actual+ opt-semi)
     actual+)]
   [actual+
    ((actual)
     (make-queue actual))
    ((actual+ comma-or-semi actual)
     (queue/insert! actual+ actual))]
   [actual
    ((expr)
     expr)
    ((comp-type-expr)
     comp-type-expr)]
   [type-expr
    ((name)
     (fix-name-tag! name 'QUALIFIEDTYPENAME 'TYPENAME)
     name)
    ((type-expr-sans-name)
     type-expr-sans-name)]
   [type-expr-sans-name
    ;; ((LP expr RP)
    ;; FUTURE EXTENSION...
    ;; subtype
    ;; )
    ;; subtype or setpred?
    ((LC identifier CLN type-expr VBAR expr RC)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (SUBTYPE PLACE: ,(mk-place LC RC)
                                ,(mk-set-pred identifier type expr LC RC)))))
    ((comp-type-expr)
     comp-type-expr)]
   [type-expr+
    ((type-expr)
     (make-queue type-expr))
    ((type-expr+ COMMA type-expr)
     (queue/insert! type-expr+ type-expr))]
   [comp-type-expr
    ((LB type-expr+ RB)
     (let ((type+ (map type-expr->type (queue->list type-expr+))))
       (template->sxml (TUPLETYPE PLACE: ,(mk-place LB RB)
                                  ,@type+))))
    ;; function type
    ((LB type-expr+ ARROW type-expr RB)
     (let* ((type+ (map type-expr->type (queue->list type-expr+)))
            (type (type-expr->type type-expr)))
       (unless (= (length type+) 1)
         (sign-sal-parser-error LB "Invalid function type, only unary functions are supported in SAL. Use a tuple argument if you desire to provide more than one argument."))
       (template->sxml (FUNCTIONTYPE PLACE: ,(mk-place LB RB)
                                     ,(car type+)
                                     ,type))))
    ;; subrange
    ((LB expr@lower DOTDOT expr@upper RB) 
     (template->sxml (SUBRANGE PLACE: ,(mk-place LB RB)
                               ,lower
                               ,upper)))
    ;; array type
    ((ARRAY type-expr@t1 OF type-expr@t2)
     (let ((t1 (type-expr->type t1))
           (t2 (type-expr->type t2)))
       (check-if-index-type t1)
       (template->sxml (ARRAYTYPE PLACE: ,(mk-place ARRAY t2)
                                  ,t1
                                  ,t2))))
    ;; record-type
    ((RECS field-declaration+ RECE)
     (template->sxml (RECORDTYPE PLACE: ,(mk-place RECS RECE)
                                 ,@(queue->list field-declaration+))))
    ;; state type
    ((STATE_TYPE LP module RP)
     (template->sxml (STATETYPE PLACE: ,(mk-place STATE_TYPE RP)
                                ,module)))
    ]
   [field-declaration+
    ((field-declaration)
     (make-queue field-declaration))
    ((field-declaration+ COMMA field-declaration)
     (queue/insert! field-declaration+ field-declaration))]
   [field-declaration
    ((identifier CLN type-expr)
     (let ((type (type-expr->type type-expr)))
       (template->sxml (FIELDDECLARATION PLACE: ,(mk-place identifier type)
                                         ,identifier
                                         ,type))))]
   [constructor+
    ((constructor)
     (make-queue constructor))
    ((constructor+ COMMA constructor)
     (queue/insert! constructor+ constructor))]
   [constructor
    ((identifier)
     (template->sxml (CONSTRUCTOR (<== identifier)
                                  ,identifier)))
    ((identifier LP var-decl+ RP)
     (template->sxml (CONSTRUCTOR PLACE: ,(mk-place identifier RP)
                                  ,identifier
                                  ,@(map (lambda (var-decl)
                                           (sxml/set-tag! var-decl 'ACCESSOR)
                                           var-decl)
                                         (queue->list var-decl+)))))]
   [qualified-name-context-name
    ((context-name)
     (set-context-name-for-next-id! context-name)
     context-name)]
   [context-name
    ((IDENTIFIER)
     (mk-context-name (to-symbol (sal-token/data IDENTIFIER))
                      (sal-token/place IDENTIFIER) #f))
    ((IDENTIFIER LC actuals RC)
     (mk-context-name (to-symbol (sal-token/data IDENTIFIER))
                      (mk-place IDENTIFIER RC)
                      actuals))
    ]
   [expr*
    (()
     (make-queue))
    ((expr+)
     expr+)]
   [expr+
    ((expr)
     (make-queue expr))
    ((expr+ COMMA expr)
     (queue/insert! expr+ expr))]
   [assertion-expr
    ((module TURNSTILE expr)
     (template->sxml (MODULEMODELS PLACE: ,(mk-place module expr)
                                   ,module
                                   ,expr)))
    ((module@m1 IMPLEMENTS module@m2)
     (template->sxml (MODULEIMPLEMENTS PLACE: ,(mk-place m1 m2)
                                       ,m1
                                       ,m2)))
    ]
   [assertion-form
    ((OBLIGATION)
     (template->sxml (ASSERTIONFORM obligation)))
    ((LEMMA)
     (template->sxml (ASSERTIONFORM lemma)))
    ((THEOREM)
     (template->sxml (ASSERTIONFORM theorem)))
    ((CLAIM)
     (template->sxml (ASSERTIONFORM claim)))]
   ))
   
(define (parse file-name context-provider-proc)
  (dlet ((*manager* (make-sal-parser-manager))
         (*curr-file-name* file-name)
         (*external-context-name* #f)
         (*context-provider-proc* context-provider-proc)
         (*prelude-ctx* (context-provider-proc 'prelude)))
    (try
     (read/lalrp *sal-parser* *sal-lexer* (current-input-port))
     (lambda (escape proc msg obj)
       (if (equal? proc "parser")
         (cond
          ((eof-object? obj)
           (sign-error "Unexpected end of input stream, when parsing `~a'." (if *curr-file-name* *curr-file-name* "<string>")))
          ((eq? (car obj) 'ERROR)
           (sign-sal-parser-error (sal-token/place (cdr obj)) "Invalid token symbol." (cdr obj)))
          (else
           (sign-sal-parser-error (sal-token/place (cdr obj)) "Unexpected token \"~a\"." (sal-token/data (cdr obj)))))
         (error proc msg obj))))))

(define (sal-parser/parse-string a-string context-provider-proc)
  (with-input-from-string a-string
    (lambda ()
      (parse #f context-provider-proc))))

(define (sal-parser/parse-file file-name context-provider-proc)
  (with-input-from-file file-name
    (lambda ()
      (parse file-name context-provider-proc))))



