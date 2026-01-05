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

(module sal-sxml-support
        (include "sal.sch")
        (include "sxml-package.sch")
        (import sal-parser-utility file-info ls-parser sal-parser runtime sal-environment)
        (export (sal/parse-xml xml-file-name)
                (sal->sxml sal-file-name sal-env)
                (lsal->sxml lsal-file-name sal-env)
                (lsal-string->sxml a-string)
                (sal->xml sal-file-name sal-env)
                (sal/simple-update-expression? sxml)
                (sal/replace-builtin-names-with-official-names sxml)
                (sal-place->string place)
                (sal/sxml-preprocessor sxml)
                (lsal->xml lsal-file-name sal-env))
        )

;###
; Parse a SAL XML file.
; The file @code{file-name} must contain a SAL context.
; Return a SXML object.
(define (sal/parse-xml file-name)
  [assert (file-name) (string? file-name)]
  (verbose-message 1 "  parsing XML file \"~a\"..." file-name)
  (unless (file-exists? file-name)
    (sign-error "File ~s does not exist." file-name))
  (let ((xml (display-runtime 3 "    XML parser time: ~a secs"
               (lambda ()
                 (sal/identifiers->symbols (cadddr (sxml/parse-xml file-name)))))))
    (verbose-message 2 "  XML preprocessor...")
    (display-runtime 3 "    preprocessor time: ~a secs"
      (lambda ()
        (sal/sxml-preprocessor xml)))))

;; Return a list of symbols that contains all identifiers in a SXML IDENTIFIERS node.
(define (sal-identifiers->symbols identifiers)
  (sxml/match-or-fail identifiers
    ((IDENTIFIERS . ?ids)
     (map (sxml/match-lambda-or-fail
           ((IDENTIFIER ?id)
            id))
          ids))))

(define (place-string->sal-place place)
  (let* ((first-space (sal-string-index place #\space))
         (second-space (if first-space (sal-string-index place #\space (+ first-space 1)) #f))
         (third-space (if second-space (sal-string-index place #\space (+ second-space 1)) #f)))
    (if third-space
      (let ((ini-line (string->integer (substring place 0 first-space)))
            (ini-column (string->integer (substring place (+ first-space 1) second-space)))
            (final-line (string->integer (substring place (+ second-space 1) third-space)))
            (final-column (string->integer (substring place (+ third-space 1) (string-length place)))))
        (make-sal-place ini-line ini-column final-line final-column))
      (error place-string->sal-place "Invalid PLACE attribute in abstract syntax tree. Please contact support." place))))

(define (selection-target selection depth)
  (bind-exit (exit)
    (let loop ((selection selection))
      (sxml/match-or-fail selection
        ;; I'm support function updates...
        (((or ARRAYSELECTION TUPLESELECTION RECORDSELECTION APPLICATION) ?target . ?-)
         (let* ((child-depth (loop target))
                (curr-depth (+ child-depth 1)))
           (when (= curr-depth depth)
             (exit selection))
           curr-depth))
        (?-
         (when (= depth 0)
           (exit selection))
         0)))
    (internal-error)))

;; Preprocess a SXML abstract syntax tree.
;; Basically, I'm converting all tuples (types and literals) with one argument
;; to the projection.
(define (sal/sxml-preprocessor sxml)
  (let ((result (sxml/bottom-up-rewriter 
                 sxml
                 ((ASSERTIONFORM ?form)
                  (let ((new-form (string->symbol (string-downcase (symbol->string form)))))
                    (make-sxml-node-based-on sxml-current-node
                                             (list new-form))))
                 ((TUPLETYPE ?arg)
                  arg)
                 ((TUPLELITERAL ?arg)
                  arg)
                 ((APPLICATION (as (NAMEEXPR -) ?sub) ?arg) ;; converts unary - in binary one
                  (if (sxml/tag-equals? arg 'TUPLELITERAL)
                    sxml-current-node
                    (make-sxml-node-based-on sxml-current-node
                                             (list sub (template->sxml (TUPLELITERAL (<== arg)
                                                                                     (NUMERAL (<== arg) "0")
                                                                                     ,arg))))))
                 ((INDEXVARDECL ?id ?type)
                  (template->sxml (VARDECL (<== sxml-current-node) ,id ,type)))
                 ((QUANTIFIEDEXPRESSION (QUANTIFIER FORALL) ?vardecls ?expr)
                  (template->sxml (FORALLEXPRESSION (<== sxml-current-node) ,vardecls ,expr)))
                 ((QUANTIFIEDEXPRESSION (QUANTIFIER EXISTS) ?vardecls ?expr)
                  (template->sxml (EXISTSEXPRESSION (<== sxml-current-node) ,vardecls ,expr)))
                 ((QUANTIFIEDASSERTION (QUANTIFIER FORALL) ?vardecls ?expr)
                  (template->sxml (FORALLASSERTION (<== sxml-current-node) ,vardecls ,expr)))
                 ((QUANTIFIEDASSERTION (QUANTIFIER EXISTS) ?vardecls ?expr)
                  (template->sxml (EXISTSASSERTION (<== sxml-current-node) ,vardecls ,expr)))
                 ((UPDATEEXPRESSION (DEPTH ?depth) ?selection ?new-value)
                  (let ((depth-val (string->integer depth)))
                    ;; (breakpoint "convert" (depth selection-target selection new-value depth-val sxml-current-node) #t)
                    (template->sxml (UPDATEEXPRESSION (<== sxml-current-node) ,(selection-target selection depth-val) ,selection ,new-value))))
                 ((SETPREDEXPRESSION ?id ?type ?body)
                  (template->sxml (SETPREDEXPRESSION (<== sxml-current-node) (VARDECL (<== id) ,id ,type) ,body))))))
    ;; convert the PLACE attributes in sal-place objects...
    (sxml/basic-traverse (lambda (n)
                           (when (sxml-node? n)
                             (let ((place (sxml/attribute n 'PLACE)))
                               (when (and place (string? place))
                                 (sxml/set-attribute! n 'PLACE (place-string->sal-place place))))))
                         result)
    result))

;; I (still) need these tables to convert LSAL->XML
(define *builtin-const-table*
  '((OR . or) (XOR . xor) (AND . and) (MOD . mod) (DIV . div) (=> . implies) (<=> . iff)
    (NOT . not) (TRUE . true) (FALSE . false)))

(define *builtin-type-table*
  '((BOOLEAN . bool) (NATURAL . nat) (INTEGER . int) (REAL . real) (NZREAL . nzreal)
    (NZINTEGER . nzint) (STRING . string) (CHAR . char)))

(define *builtin-const-table-inv*
  (invert-mapping *builtin-const-table*))

(define *builtin-type-table-inv*
  (invert-mapping *builtin-type-table*))

(define (sal/replace-builtin-names-with-official-names sxml)
  (sxml/bottom-up-rewriter 
   sxml
   ((TYPENAME ?name)
    (cond
     ((assq name *builtin-type-table-inv*) =>
      (lambda (pair)
        (make-sxml-node-based-on sxml-current-node
                                 (list (cdr pair)))))
     (else
      sxml-current-node)))
   ((NAMEEXPR ?name)
    (cond
     ((assq name *builtin-const-table-inv*) =>
      (lambda (pair)
        (make-sxml-node-based-on sxml-current-node
                                 (list (cdr pair)))))
     (else
      sxml-current-node)))))

(define (sal-place->string place)
  (string-append (integer->string (sal-place/initial-line place))
                 " "
                 (integer->string (sal-place/initial-column place))
                 " "
                 (integer->string (sal-place/final-line place))
                 " "
                 (integer->string (sal-place/final-column place))))

(define (sal-sxml->official-sxml sxml)
  (let* ((sxml (sal/replace-builtin-names-with-official-names sxml))
         (result (sxml/bottom-up-rewriter
                  sxml
                  ((ASSERTIONFORM ?form)
                   (let ((new-form (string->symbol (string-upcase (symbol->string form)))))
                     (make-sxml-node-based-on sxml-current-node
                                              (list new-form))))
                  (((or MULTISYNCHRONOUS MULTIASYNCHRONOUS ARRAYTYPE)
                    (as (VARDECL ?id ?type) ?var-decl)
                    ?body)
                   (make-sxml-node-based-on sxml-current-node
                                            (list
                                             (template->sxml (INDEXVARDECL (<== var-decl) ,id ,type))
                                             body)))
                  ((APPLICATION ?fun ?arg)
                   (if (sxml/tag-equals? arg 'TUPLELITERAL)
                     sxml-current-node
                     (make-sxml-node-based-on sxml-current-node
                                              (list fun
                                                    (template->sxml (TUPLELITERAL (<== arg) ,arg))))))
                  ((FORALLEXPRESSION ?vardecls ?expr)
                   (template->sxml (QUANTIFIEDEXPRESSION (<== sxml-current-node) (QUANTIFIER FORALL) ,vardecls ,expr)))
                  ((EXISTSEXPRESSION ?vardecls ?expr)
                   (template->sxml (QUANTIFIEDEXPRESSION (<== sxml-current-node) (QUANTIFIER EXISTS) ,vardecls ,expr)))
                  ((FORALLASSERTION ?vardecls ?expr)
                   (template->sxml (QUANTIFIEDASSERTION (<== sxml-current-node) (QUANTIFIER FORALL) ,vardecls ,expr)))
                  ((EXISTSASSERTION ?vardecls ?expr)
                   (template->sxml (QUANTIFIEDASSERTION (<== sxml-current-node) (QUANTIFIER EXISTS) ,vardecls ,expr)))
                  ((SETPREDEXPRESSION (VARDECL ?id ?type) ?body)
                   (template->sxml (SETPREDEXPRESSION (<== sxml-current-node) ,id ,type ,body))))))
    ;; convert the sal-place attributes in strings...
    (sxml/basic-traverse (lambda (n)
                           (when (sxml-node? n)
                             (let ((place (sxml/attribute n 'PLACE))
                                   (elsif (sxml/attribute n 'ELSIF)))
                               (sxml/set-attributes! n '()) ;; I don't want to send non standard attributes to the XML file...
                               (when (and place (sal-place? place))
                                 (sxml/set-attribute! n 'PLACE (sal-place->string place)))
                               (when (and elsif (equal? elsif "YES"))
                                 (sxml/set-attribute! n 'ELSIF "YES")))))
                         result)
    result))

(define (lsal->xml lsal-file-name sal-env)
  (let ((sxml (lsal->sxml lsal-file-name sal-env)))
    (sxml->xml (sal-sxml->official-sxml sxml))))

;; check restriction in the sal language
(define (sal/check-restrictions sxml)
  #t)

;###
; Convert a SAL file (<name>.sal) in a SXML datastructure.
(define (sal->sxml file-name sal-env)
  (verbose-message 1 "parsing SAL file \"~a\"..." file-name)
  (display-runtime 10 "  parser time: ~a secs"
    (lambda ()
      (let* ((context-proc (lambda (ctx-name)
                             (sal-env/context sal-env ctx-name)))
             (sxml (sal-parser/parse-file file-name context-proc))
             (result (sal/sxml-preprocessor sxml)))
        ;; (sxml/pp result)
        result))))
    
(define *sal/identifier-tags*
  '(IDENTIFIER NAMEEXPR TYPENAME MODULENAME LABEL SCALARELEMENT QUANTIFIER ASSERTIONFORM ASSERTIONOPERATOR QUANTIFIER))

;; the following function maps strings inside the SAL AST to symbols
;; Imports external context references. A SAL context may contain external references, i.e., it may
;; use context-names.
(define (sal/identifiers->symbols sxml)
  [assert (sxml) (sxml-node? sxml)]
  (sxml/basic-traverse
   (lambda (sxml)
     (if (sxml/tag-equals*? sxml *sal/identifier-tags*)
       (let ((children (sxml/children sxml)))
         (set-car! children (string->symbol (car children))))))
   sxml)
  sxml)

;###
; Convert a LSAL file (<name>.lsal) in a SXML datastructure.
; @code{file-name} is a string which contains the full name of the
; LSAL file to be parsed.
(define (lsal->sxml file-name sal-env)
  (verbose-message 1 "parsing LSAL file \"~a\"..." file-name)
  (display-runtime 10 "  parser time: ~a secs"
    (lambda ()
      (ls-parser/parse-file file-name))))

(define (sal->xml file-name sal-env)
  (verbose-message 1 "parsing SAL file \"~a\"..." file-name)
  (display-runtime 2 "  parser time: ~ secs"
    (lambda ()
      (sxml->xml (sal-sxml->official-sxml (sal->sxml file-name sal-env))))))
  

;###
; Convert a LSAL string in a SXML datastructure.
; @code{a-string} is a string which contains LSAL code.
(define (lsal-string->sxml a-string)
  (ls-parser/parse-string a-string))

;###
; Check if @code{sxml} is a simple update expression, that is,
; the position does not specifies a nested element.
; @lisp
; (sal/simple-update-expression? 
;  (template->sxml (UPDATEEXPRESSION
;                   (NAMEEXPR c)
;                   (RECORDSELECTION
;                    (NAMEEXPR c)
;                    (IDENTIFIER idx))
;                   (NAMEEXPR val))))
; @result{} #t
; (sal/simple-update-expression? 
;  (template->sxml (UPDATEEXPRESSION
;                   (NAMEEXPR c)
;                   (TUPLESELECTION
;                    (RECORDSELECTION
;                     (NAMEEXPR c)
;                     (IDENTIFIER idx))
;                    (NUMERAL "1"))
;                   (NAMEEXPR val))))
; @result{} #f
; (sal/simple-update-expression? 
;  (template->sxml (UPDATEEXPRESSION
;                   (RECORDSELECTION
;                    (NAMEEXPR c)
;                    (IDENTIFIER idx))
;                   (TUPLESELECTION
;                    (RECORDSELECTION
;                     (NAMEEXPR c)
;                     (IDENTIFIER idx))
;                    (NUMERAL "1"))
;                   (NAMEEXPR val))))
; @result{} #t
; @end lisp
(define (sal/simple-update-expression? sxml)
  (sxml/match-or-fail sxml
    ((UPDATEEXPRESSION ?collection1 ?position ?value)
     (sxml/match-or-fail position
       (((or ARRAYSELECTION TUPLESELECTION RECORDSELECTION APPLICATION) ?collection2 ?-)
        (sxml/equal? collection1 collection2))))))

