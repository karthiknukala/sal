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


;; BD replaced try by sxmltry everywhere to avoid
;; clashing with bigloo's (try ...) form

(module sxml-package
        (import utility)
        (export *sxml-void*
                (make-simple-sxml-node tag children)
                (make-sxml-node tag attributes children)
                (make-sxml-node-based-on sxml-node children)
                (sxml-node? sxml)
                (list->sxml alist)
                (sxml->list sxml)
                (sxml/parse-xml file)
                (string->goodXML str)
                (sxml->xml data)
                (sxml/equal? sxml1 sxml2)
                (sxml/lt? sxml1 sxml2)
                (sxml/hash sxml)
                (sxml/size sxml)
                ;; Tag related functions
                (sxml/tag sxml)
                (sxml/tag-equals? sxml tag)
                (sxml/tag-equals*? sxml tag-list)
                (sxml/contains-tag? sxml tag)
                (sxml/contains-tag*? sxml tag-list)
                (sxml/find-child-with-tag sxml tag)
                (sxml/find-child-with-tag* sxml tag-list)
                (sxml/set-tag! sxml new-tag)
                ;; Children Manipulation functions
                (sxml/leaf? sxml)
                (sxml/children sxml)
                (sxml/child sxml idx)
                (sxml/first-child sxml)
                (sxml/last-child sxml)
                (sxml/set-children! sxml children)
                (sxml/add-child! n child)
                (sxml/add-child-before n child)
                (sxml/add-child-after n child)
                (sxml/find-child proc sxml)
                ;; Attributes
                (sxml/attributes sxml)
                (sxml/set-attributes! n attrs) 
                (sxml/attribute sxml key)
                (sxml/set-attribute! n key value)
                ;; Tree walkers
                (sxml/basic-traverse proc sxml)
                (sxml/traverse proc-before sxml . proc-after)
                (sxml/find-node proc sxml)
                ;; Rest
                (sxml/pp sxml)
                (sxml/copy sxml))
        )

;==================================
; SXML 
; @cindex XML
; This module provides functions to manipulate XML
; documents in Scheme.

;-----------------------------------------
; Main functions

;###
; Make a simple new SXML node, attributes are
; not specified.
; @code{tag} must be a symbol.
; @code{children} must be a list of sxml-nodes or atoms.
(define (make-simple-sxml-node tag children)
  [assert (tag) (symbol? tag)]
  (internal-make-sxml-node tag '() children))

(define *sxml-void* (make-simple-sxml-node 'VOID (list)))

;###
; Make a new SXML node.
(define (make-sxml-node tag attributes children)
  (internal-make-sxml-node tag attributes children))

;###
; Make a new SXML node based on the given SXML node.
; The new node will have the same tag, and attributes
; of the given node. 
(define (make-sxml-node-based-on sxml-node children)
  (make-sxml-node (sxml/tag sxml-node)
                  (sxml/attributes sxml-node)
                  children))

(define-record-type sxml-node
  (internal-make-sxml-node tag attributes children)
  sxml-node?
  (tag sxml/tag sxml/set-tag!)
  (attributes sxml/attributes sxml/set-attributes!)
  (children sxml/children sxml/set-children!))


;###
; Return true if @code{n} is a SXML node.
;C(define (sxml-node? n) 

;----

;------------------------------------------------------
; Tags
; @cindex SXML Tags
; @cindex XML Tags

;###
; Return the tag associated with a SXML node @code{n}.
;C(define (sxml/tag n) 


;###
; Return true if @code{sxml} node tag is equals to @code{tag}.
(define (sxml/tag-equals? sxml tag)
  (and (sxml-node? sxml)
       (eq? tag (sxml/tag sxml))))

;###
; Return true if @code{sxml} node tag is equals to a element in the list of tags @code{tag-list}.
(define (sxml/tag-equals*? sxml tag-list)
  (and (sxml-node? sxml)
       (memq (sxml/tag sxml) tag-list)))

;###
; Modify the tag associated with a SXML node @code{n}.
;C(define (sxml/set-tag! n new-tag) 

;###
; Return true if the @code{sxml} node contains a node such that its tag is equals to @code{tag}.
; @code{tag} is a symbol
(define (sxml/contains-tag? sxml tag)
  (sxml/find-node (lambda (node) (if (sxml/tag-equals? node tag) node #f)) sxml))

;###
; Return true if the @code{sxml} node contains a node such that its tag is a member of the list @code{tag-list}.
; @code{tag-list} is a list of symbols.
(define (sxml/contains-tag*? sxml tag-list)
  (sxml/find-node (lambda (node) (if (sxml/tag-equals*? node tag-list) node #f)) sxml))

;###
; Return the first child with the tag @code{tag}.
(define (sxml/find-child-with-tag sxml tag)
  (sxml/find-child (lambda (n) (sxml/tag-equals? n tag)) sxml))

;###
; Return the first child which the tag is a element of the list @code{tag-list}.
(define (sxml/find-child-with-tag* sxml tag-list)
  (sxml/find-child (lambda (n) (sxml/tag-equals*? n tag-list)) sxml))

;---

;----------------------------------------------------
; Attributes
; @cindex XML Attributes
; @cindex SXML Attributes
; Every SXML node may contain attributes.

;###
; Return an association list of attributes of the node @code{n}.
;C(define (sxml/attributes n) 


;###
; Set the attributes of a SXML node @code{n}.
; @code{attrs} is an association list.
;C(define (sxml/set-attributes! n attrs) 

;###
; Return the value of an attribute @code{key} in the SXML node @code{n}.
(define (sxml/attribute n key)
  [assert (n) (sxml-node? n)]
  [assert (key) (symbol? key)]
  (let ((result (assq key (sxml/attributes n))))
    (if result
      (cdr result)
      result)))

;###
; Set an attribute in a SXML node
(define (sxml/set-attribute! n key value)
  [assert (n) (sxml-node? n)]
  [assert (key) (symbol? key)]
  (let ((attr (assq key (sxml/attributes n))))
    (when attr
      (sxml/set-attributes! n (remq attr (sxml/attributes n))))
    (sxml/set-attributes! n (cons (cons key value) (sxml/attributes n)))))

;------- SXML attributes section

;----------------------------------------------------
; Children Manipulation Functions
; @cindex SXML/XML Children Manipulation Functions

;###
; Return the children of the node @code{n}.
;C(define (sxml/children n) 

;###
; Set the children of the node @code{n}.
; @code{children} is a list that contains the new children.
;C(define (sxml/set-children! n children) 

;###
; Add a new child @code{n} to the SXML node @code{n}.
; The new child is added in the end of the child list of @code{n}.
(define (sxml/add-child! n child)
  [assert (n) (sxml-node? n)]
  (sxml/set-children! n (append (sxml/children n) (list child))))

;###
; Create a new node equals to @code{n}, but contains a new child @code{child}.
; The new child is added in the end of the child list.
(define (sxml/add-child-after n child)
  (make-sxml-node-based-on n (append (sxml/children n) (list child))))

;###
; Create a new node equals to @code{n}, but contains a new child @code{child}.
; The new child is added in the begin of the child list.
(define (sxml/add-child-before n child)
  (make-sxml-node-based-on n (cons child (sxml/children n))))

;###
; Return a child in the position @code{idx}.
; @code{(sxml/child n idx)} is equivalent to @code{(list-ref (sxml/children n) idx)}, when
; an error is not signed.
(define (sxml/child n idx)
  [assert (n) (sxml-node? n)]
  (let ((children (sxml/children n)))
    (if (< idx (length children))
      (list-ref children idx)
      #f)))
;###
; Return the first child of a SXML node.
; Return false if the SXML node does not contain any children.
(define (sxml/first-child sxml)
  [assert (sxml) (sxml-node? sxml)]
  (let ((children (sxml/children sxml)))
    (if (null? children)
      #f
      (car (sxml/children sxml)))))

;###
; Return the last child of a SXML node.
; Return false if the SXML node does not contain any children.
(define (sxml/last-child sxml)
  [assert (sxml) (sxml-node? sxml)]
  (let ((children (sxml/children sxml)))
    (if (null? children)
      #f
      (sxml/child sxml (- (length (sxml/children sxml)) 1)))))

;###
; Return true if the SXML node is a leaf, that is, it does not contain children or
; it is not a SXML node.
(define (sxml/leaf? sxml)
  (if (sxml-node? sxml)
    (null? (sxml/children sxml))
    #t))

;###
; Return the first child such that (proc child) returns true
(define (sxml/find-child proc sxml)
  (bind-exit (exit)
    (for-each (lambda(n) (if (proc n) (exit n))) (sxml/children sxml))
    #f))

;----- End of the SXML Children Manipulation Section

;----------------------------------
; Encoding/Decoding
; @cindex XML Encoding
; @cindex XML Decoding
;

(define xml-char-decoding
  '((lt . #\<) (gt . #\>) (amp . #\&) (quot . #\")))

(define xml-char-encoding
  '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

;###
; Build a SXML object using the XML file @code{file-name}.
(define (sxml/parse-xml file-name)
  (with-input-from-file file-name  
    (lambda () 
      (letrec
          ([simple-error 
            (lambda (msg) 
              (error 'sxml/parse-xml msg #f))]
           [sxmltry 
            (lambda (proc exception)
              ;; Exception mechanism
              ;; Emitting #f will be considered as an exception for procedures
              ;; that are supposed to return a non-boolean
              (let ((result proc))
                (if result result (eval exception))))]
           [get-char 
            (lambda ()
              ;; Reads one char from the current port
              ;; returns: char
              ;; exception: #f
              (let ((char (read-char )))
                (if (eof-object? char)
                  #f
                  char)))]
           [lookahead 
            (lambda () (peek-char))]
           [skip-char 
            (lambda ()
              ;; skips one char from the current port
              ;; returns: empty list
              ;; exception: #f
              (let ((char (read-char)))
                (if (eof-object? char)
                  #f
                  '())))] 
           [skip-if-char 
            (lambda (ch)
              ;; Skips one char from the current port
              ;; returns: empty list
              ;; no exception needed
              (cond ((eqv? ch (lookahead)) (skip-char))
                    (else '())))]
           [eof-doc? 
            (lambda ()
              ;; Checks inport for End of File
              ;; returns: boolean
              (eof-object? (peek-char )))]
           [get-while 
            (lambda (fn)
              ;; Reads characters from inport while fn
              ;; returns: list of char, or empty list when nothing 's been found
              ;; no exception needed
              (cond ((eof-object? (lookahead)) '())
                    ((fn (lookahead)) 
                     (let* ((c (get-char)))
                       (cons c (get-while fn))))
                    (else '())))]
           [skip-while 
            (lambda (fn)
              ;; Reads characters from the current port while fn
              ;; returns: empty list
              ;; no exception needed
              (cond ((eof-object? (lookahead)) '())
                    ((fn (lookahead)) (begin (skip-char) (skip-while fn)))
                    (else '())))]
           [get-upto-and-skip 
            (lambda (ch)
              ;; Reads characters from the current port upto ch
              ;; returns: list of char
              ;; exception: #f
              (cond ((eof-object? (lookahead)) #f)
                    ((eqv? ch (lookahead)) (skip-char))
                    (else (let* ((c (get-char)))
                            (cons c (get-upto-and-skip ch))))))]
           [get-upto-and-skip-2 
            (lambda (ch1 ch2)
              ;; Reads characters from the current port upto ch1 ch2
              ;; returns: list of char
              ;; exception: #f
              (cond ((eof-object? (lookahead)) #f)
                    ((eqv? ch1 (lookahead))   (begin
                                                (skip-char)
                                                (if (eqv? ch2 (lookahead))
                                                  (get-upto-and-skip ch2)
                                                  (cons ch1 (get-upto-and-skip-2 ch1 ch2)))))
                    (else (let* ((c (get-char))) (cons c (get-upto-and-skip-2 ch1 ch2))))))]
           [expect? 
            (lambda (ch)
              ;; Checks next char on the current port against parameter Ch
              ;; returns: boolean
              ;; no exception needed
              (cond ((eof-object? (lookahead)) #f)
                    ((eqv? ch (lookahead)) (begin (read-char) #t))
                    (else #f)))]
           [expect-ls? 
            (lambda (ls)
              ;; Checks next chars on the current port against chars in parameter ls
              ;; returns: boolean
              ;; no exception needed
              (cond ((null? ls) #t)
                    ((expect? (car ls)) (expect-ls? (cdr ls)))
                    (else #f)))]
           [expect-string? 
            (lambda (str)
              ;; Checks the current port against string Str
              ;; returns: boolean
              ;; no exception needed
              (expect-ls? (string->list str)))]
           [char-pcdata? 
            (lambda (ch)
              (not (or (eqv? #\< ch) (eqv? #\& ch))))]
           [char-upper? 
            (lambda (ch)
              (and (char? ch) (char>=? ch #\A) (char<=? ch #\Z)))]
           [char-lower? 
            (lambda (ch)
              (and (char? ch) (char>=? ch #\a) (char<=? ch #\z)))]
           [char-alpha? 
            (lambda (ch)
              (or (char-upper? ch) (char-lower? ch)))]
           [char-blank? 
            (lambda (ch)
              (or (eqv? #\space ch)
                  (eqv? #\return ch)
                  (eqv? #\newline ch)
                  (eqv? #a012 ch)
                  (eqv? #\tab ch)))]
           [char-return? 
            (lambda (ch)
              (or (eqv? #\return ch)
                  (eqv? #\newline ch)))]
           [char-namechar? 
            (lambda (ch)
              (or (char-alpha? ch) (eqv? #\. ch) (eqv? #\- ch) (eqv? #\_ ch) (eqv? #\: ch)))]
           [parse-comment 
            (lambda ()
              (let* ((hyp (if (expect? #\-)
                            '() ;; ok
                            (simple-error "malformed comment")))
                     (comment (list (list->string (sxmltry (get-upto-and-skip-2 #\- #\-) 
						       '(simple-error "comment never ends")))))
                     (fin (if (expect? #\>)
                            '() ;; ok
                            (simple-error "malformed comment"))))
                (list (cons 'comment: comment))))]
           [parse-name 
            (lambda ()
              ;; [5] Name ::= (Letter | '_' | ':') (NameChar)*
              (let* ((tagname  (get-while char-namechar?))
                     (white    (skip-while char-blank?)))
                (list->string tagname)))]

           ;; ****** I stopped to use parse-pcdata... check parce-pcdata-and-references
           ;;       (define (parse-pcdata)
           ;;       ;; Parse PCData (= Parsed Character Data)
           ;;       ;; [16] PCData ::= [^<&]*
           ;;       ;; (list (cons 'pcdata:
           ;;       ;;            (list (list->string (get-while char-pcdata?))))))
           ;;       (list->string (get-while char-pcdata?)))

           ;; ****** I stopped to use parse-reference... check parce-pcdata-and-references
           ;;     (define (parse-reference)
           ;;       ;; Parse a reference
           ;;       ;; NB. Needs differentiation
           ;;       ;; EBNF rule [67-69]:
           ;;       ;; [68] Reference ::= EntityRef | CharRef
           ;;       ;; [67] CharRef   ::= '&#' [0-9]+ ';' | '&#x' [0-9A-Fa-f]+ ';'
           ;;       ;; [69] EntityRef ::= '&' Name ';'
           ;;       (let*  ((refname (list->string (get-while char-namechar?)))
           ;;               (end-ok? (expect? #\;)))
           ;;         (list (cons 'ref: (list refname)))))

           [parse-pcdata-and-references 
            (lambda ()
              (list->string 
               (let loop ((result '()))
                 (let* ((curr (cond 
                               ((expect? #\&) 
                                (let* ((refname (list->string (get-while char-namechar?)))
                                       (refname-symbol (string->symbol refname))
                                       (end-ok? (expect? #\;))
                                       (decoded-char-pair (assq refname-symbol xml-char-decoding)))
                                  (if (and end-ok? decoded-char-pair)
                                    (list (cdr decoded-char-pair))
                                    (simple-error (string-append "invalid xml reference char \"" refname "\"")))))
                               (else (get-while char-pcdata?))))
                        (new-result (append result curr)))
                   (if (not (eqv? #\< (lookahead)))
                     (loop new-result)
                     new-result)))))]
           
           [parse-white 
            (lambda ()
              ;; Parse nonsignificant white
              ;; eg. this token starts with a return
              ;; Cf. XML Spec section 2.8 White Space Handling
              (skip-while char-blank?))]
           [parse-quoted 
            (lambda ()
              ;; Parse a quoted value
              ;; Returns containing a string ===> in the previous version was a list
              ;; [10] AttValue ::= '"' ([^<&"] | Reference)* '"' 
              ;;                 | "'" ([^<&'] | Reference)* "'"
              (cond  ((expect? #\") (list->string
                                     (sxmltry (get-upto-and-skip #\")
                                          '(simple-error "quote \" never ends"))))
                     ((expect? #\') (list->string
                                     (sxmltry (get-upto-and-skip #\')
                                          '(simple-error "single quote never ends"))))
                     (else #f)))]
           [parse-attr 
            (lambda ()
              ;; Parse an attribute
              ;; [41] Attribute ::= Name Eq AttValue
              ;; [25] Eq        ::= Sp? '=' Sp?
              (let*  ((attrname (list->string (get-while char-alpha?)))
                      (sp       (skip-while char-blank?))
                      (eq       (if (expect? #\=)
                                  (skip-while char-blank?)
                                  (simple-error "malformed attribute")))
                      (attrval  (sxmltry (parse-quoted)
                                     '(simple-error "attribute has no value")))
                      (sp2       (skip-while char-blank?)))
                (append ;; one or more attributes
                 (list (cons (string->symbol attrname) attrval))
                 (if (char-alpha? (lookahead)) 
                   (parse-attr) ;; next attribute
                   '()))))]
           [parse-endtag 
            (lambda (currelem)
              ;; Parse an endtag
              ;; [42] EndTag  ::= '</' Name Sp? '>'
              (let* ((tagname   (parse-name))
                     (sp        (skip-while char-blank?))
                     (end-ok?   (expect? #\>)))
                (if (and (string=? tagname currelem) end-ok?)
                  '()
                  (simple-error "wrong endtag"))))]
           [parse-starttag 
            (lambda (currelem)
              ;; Parse a starttag
              ;; [39] element ::= EmptyElemTag | StartTag content EndTag
              ;; [40] StartTag     ::= '<' Name (Sp Attribute)* Sp? '>'
              ;; [44] EmptyElemTag ::= '<' Name (Sp Attribute)* Sp? '/>'
              (let* ((tagname (parse-name))
                     (result (internal-make-sxml-node (string->symbol tagname)
                                                      (if (char-alpha? (lookahead)) (parse-attr) '()) ; check if there are attributes to be parsed.
                                                      '())) 
                     (empty?  (expect? #\/))
                     (end-ok? (expect? #\>)))
                (cond ((and empty? end-ok?)  result)
                      (end-ok? (parse-content result tagname))
                      (else (simple-error "malformed starttag")))))]
           [parse-pi 
            (lambda ()
              ;; Parse a Processing Instr. "<?...?>"
              ;; PI  ::was:: '<?' Name Sp (Char*  -(Char* '?>' Char*)) '?>'
              ;; [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
              ;; [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
              (simple-error "processing instructions are not supported yet."))]
           [parse-cdata 
            (lambda ()
              ;; Parse CData
              ;; [18] CDSect ::= CDStart CData CDEnd
              ;; [19] CDStart ::= '<![CDATA['
              ;; [20] CData  ::= (Char* -(Char* ']]>' Char*))
              ;; [21] CDEnd  ::= ']]>'
              (let*  ((start (if (char-alpha? (lookahead))
                               (if (expect-string? "CDATA[")
                                 '() ;; OK
                                 (simple-error "Expressions like these <![% blocks don't exist in XML"))
                               ;; else (it doesn't read "CDATA")
                               (simple-error "This expression <![ should be followed by CDATA")))
                      (cdata (list (list->string (append
                                                  (sxmltry (get-upto-and-skip-2 #\] #\])
                                                       '(simple-error "CData block never ends"))))))
                      (fin   (if (expect? #\>)
                               '() ;; OK
                               (simple-error "wrong CDATA-block"))))
                (list (cons 'cdata: cdata ))))]
           [parse-content 
            (lambda (result currelem)
              ;; Parse one expression, then loops recursively
              ;; returns: list of strings
              ;; [43] content  ::= (element | CharData | Reference | CDSect | PI | Comment)*
              ;; [39] element  ::= EmptyElemTag | StartTag content EndTag
              ;; ;; (EmptyElement | ( StartTag content EndTag) | PCData
              ;; ;; | EntityRef | CharRef | CDSect | PI | Comment)*
              (letrec ((loop (lambda (proc)
                               (parse-content
                                (begin (if (not (null? proc)) (sxml/add-child! result proc)) result)
                                currelem))))
                (cond 
                 ((eof-doc?)  result)
                 ((expect? #\<)                     ;;; Tags: "<__>"
                  (cond ((char-alpha? (lookahead)) (loop (parse-starttag currelem)))
                        ((expect? #\/)             (begin (parse-endtag currelem) result))
                        ((expect? #\!)       
                         (cond ((expect? #\-)   (loop (parse-comment)))
                               ((expect? #\[)   (loop (parse-cdata)))
                               (else            (simple-error "wrong markup at <!"))))
                        ((expect? #\?)          (loop (parse-pi)))
                        (else                    (simple-error "wrong markup")))) ;; ends 'expect? #\< section
                 ;; Now, I'm handling references and pcdata together!
                 ;; ((expect? #\&)                 (loop (parse-reference))) ;; &__ 
                 ((char-blank? (lookahead))    (loop (parse-white)))
                 (else                          (loop (parse-pcdata-and-references))))))]
           [parse-dtd 
            (lambda (filename)
              ;; Placeholder for 'parse-xml-file
              ;; Parse an XML DTD-file and return a Scheme tree-structure
              ;; (DTD = Document Type Definition)
              '())] ;; DO NOTHING IN THE CURRENT IMPLEMENTATION
           [parse-dtd-decls 
            (lambda (result lst)
              (simple-error "dtd declarations are not supported yet."))]
           [parse-doctype-decl 
            (lambda (result)
              ;; Parse a document type declaration tag
              ;; [28] doctypedecl ::= '<!DOCTYPE' Sp Name (Sp  ExternalID)? Sp?
              ;;                      ('[' (markupdecl | PEReference | Sp)* ']' Sp? )? '>'
              ;; [75] ExternalID  ::= 'SYSTEM' Sp SystemLiteral
              ;;                      | 'PUBLIC' PubIdLiteral Sp SystemLiteral
              (let* ((decl-ok? (cond ((expect-string? "DOCTYPE")   #t)
                                     ((expect-string? "doctype")   #t)
                                     (else            (simple-error "expecting <!DOCTYPE"))))
                     (sp       (skip-while char-blank?))
                     (doctype  (parse-name)))
                ;; (print "doctype = " doctype)
                (cond ((expect? #\[)  (cons (list 'doctype: doctype)
                                            (parse-dtd-decls result '())))
                      ((expect-string? "SYSTEM")  
                       (let* ((sp2 (skip-while char-blank?))
                              (dtd-ident (sxmltry (parse-quoted) '(simple-error "malformed doctype")))
                              (sp3 (skip-while char-blank?))
                              (end-ok? (expect? #\>)))
                         (if end-ok? (cons (list 'doctype: doctype) (parse-dtd dtd-ident))
                             (simple-error "malformed doctype"))))
                      ((expect-string? "PUBLIC")     
                       (let* ((sp2 (skip-while char-blank?))
                              (dtd-ident (sxmltry (parse-quoted)
                                              '(simple-error "malformed doctype"))]
                              (sp3 (skip-while char-blank?))
                              (end-ok? (expect? #\>)))
                         (if end-ok? (cons (list 'doctype: doctype) dtd-ident)
                             (simple-error "malformed doctype"))))
                      (else   (simple-error "malformed doctype")))))]
           [parse-misc 
            (lambda (result dtd expect-dtd?)
              ;; Misc may come before and/or after the DTD
              ;; [27] Misc        ::= Comment | PI | Sp
              (letrec ((loop (lambda (proc)
                               (parse-misc
                                (append result proc) dtd #t))))
                (cond 
                 ((eof-doc?)                         result)
                 ((expect? #\<)                      ;;; Tags: "<__>"
                  (cond ((char-alpha? (lookahead)) (append result
                                                           (list (cons 'dtd: dtd))
                                                           (list (parse-starttag ""))))                                          
                        ((expect? #\/)            (simple-error "unexpected end tag in prolog"))
                        ((expect? #\!)      (cond ((expect? #\-)
                                                   (loop (parse-comment)))
                                                  ((char-alpha? (lookahead))
                                                   (if expect-dtd?
                                                     (parse-misc result (parse-doctype-decl result) #f)
                                                     (simple-error "DTD finished")))
                                                  (else   (simple-error "wrong markup at <!")))) ;; ends this 'expect? #\! section
                        ((expect? #\?)            (loop (parse-pi)))
                        (else                     (simple-error "wrong markup in prolog")))) ;; ends 'expect? #\< section
                 ((char-blank? (lookahead))       (loop (parse-white)))
                 (else                            (simple-error "PCdata in prolog")))))]
           [parse-document 
            (lambda ()
              (if (expect? #\<)
                (cond ((and (expect? #\?) 
                            (or (expect-string? "XML")
                                (expect-string? "xml")))
                       ;; let* ensures sequential parsing on the current port
                       (let* ((white (skip-while char-blank?))
                              (attrs (if (char-alpha? (lookahead))
                                       (list (parse-attr)) ;; list all attributes
                                       '()))
                              (fin (if (expect-string? "?>")
                                     '() ;; ok
                                     (simple-error "malformed XML prolog"))))
                         (cons (append '(xml:) attrs)
                               (parse-misc '() '() #t)))) ;; ends let*, ends ?XML section
                      ((expect? #\!) (cond ((expect? #\-)
                                            (append '(xml:) (parse-misc (parse-comment) '() #t)))
                                           ((char-alpha? (lookahead))
                                            (append '(xml:) (parse-misc '() (parse-doctype-decl '()) #f)))))
                      ((char-alpha? (lookahead)) (append '(xml: (dtd:)) (list (parse-starttag ""))))) ;; ends 'cond
                (simple-error "not an XML document")))])
        (parse-document)))))

;###
; Convert a string to a XML string, that is, the XML representation that will be used in a XML file.
(define (string->goodXML str)
  (let* ((bad-chars (map car xml-char-encoding)))
    ;; Check to see if str contains one of the characters in charset,
    ;; from the position i onward. If so, return that character's index.
    ;; otherwise, return #f
    (let* ([index-cset 
            (lambda (str i charset)
              (let loop ((i i))
                (and (< i (string-length str))
                     (if (memv (string-ref str i) charset) i
                         (loop (+ i 1))))))]
           ;; The body of the function
           (bad-pos (index-cset str 0 bad-chars)))
      (if (not bad-pos) str   ; str had all good chars
          (let loop ((from 0) (to bad-pos))
            (cond
             ((>= from (string-length str)) '())
             ((not to)
              (cons (substring str from (string-length str)) '()))
             (else
              (let ((tail (cons
                           (cdr (assv (string-ref str to) xml-char-encoding))
                           (loop (+ to 1) (index-cset str (+ to 1) bad-chars)))))
                (if (< from to) 
                  (cons
                   (substring str from to) tail)
                  tail)))))))))

;###
; Produce a XML representation in the current output port of the given SXML node.
; Remark: Only the attributes that are associated with
;         strings, symbols and numbers are considered.
(define (sxml->xml sxml)
  (letrec 
      ([display-xml-string 
        (lambda (str)
          (let* ((str (to-string str))
                 (result (string->goodXML str)))
            (if (list? result)
              (for-each display (string->goodXML str))
              (display result))))]
       [print-attributes 
        (lambda (attrs)
          (for-each (lambda (attr)
                      (when (or (pair? attr) (string? (cdr attr)) (symbol? (cdr attr)) (number? (cdr attr)))
                        (let ((attr (cond 
                                     ((symbol? attr)
                                      (symbol->string attr))
                                     ((number? attr)
                                      (number->string attr))
                                     (else
                                      attr))))
                          (display " ")
                          (display (car attr)) 
                          (display "=\"") 
                          (display-xml-string (cdr attr)) 
                          (display "\"")))) 
                    attrs))]
       [print-aux 
        (lambda (data) 
          (cond ((sxml-node? data) 
                 (display "<") 
                 (display (sxml/tag data)) 
                 (print-attributes (sxml/attributes data))
                 (display ">")
                 (for-each print-aux (sxml/children data))
                 (display "</")
                 (display (sxml/tag data)) 
                 (display ">"))
                (else (display-xml-string data))))])
    (print-aux sxml)
    #unspecified))
;-----

;----------------------------------
; Auxiliary Functions
; @cindex SXML Auxiliary Functions

;###
; Return the number of SXML nodes in @code{sxml}.
; @lisp
; (sxml/size (list->sxml '(TUPLETYPE (TYPENAME NATURAL)
;                                    (TYPENAME NATURAL))))
;   @result{} 5
; @end lisp
(define (sxml/size sxml)
  (if (sxml-node? sxml)
    (fold-left (lambda (accumulator child)
                                   (+ accumulator (sxml/size child)))
                                 1
                                 (sxml/children sxml))
    1))

; (define (sxml/size sxml)
;   (if (sxml-node? sxml)
;     (let ((result (sxml/attribute sxml 'SXML-SIZE)))
;       (if result
;         result
;         (let ((result (fold-left (lambda (accumulator child)
;                                    (+ accumulator (sxml/size child)))
;                                  1
;                                  (sxml/children sxml))))
;           (sxml/set-attribute! sxml 'SXML-SIZE result)
;           result)))
;     1))


;###
;  Create a deep copy of a sxml node. 
;  IMPORTANT: we do not perform deep copy over the attributes or use data.
(define (sxml/copy sxml)
  (cond 
   ((sxml-node? sxml) 
    (let ((new-children (map sxml/copy (sxml/children sxml))))
      (internal-make-sxml-node (sxml/tag sxml) (sxml/attributes sxml) new-children)))
   ((symbol? sxml) sxml)
   ((string? sxml) (string-copy sxml))
   (else (error 'sxml/copy "Unexpected node type." sxml))))

;###
; Print a SXML node in current output port. Attributes and User data are not printed.
(define (sxml/pp sxml)
  (pp (sxml->list sxml)) 
  #unspecified)

;###
; Convert a list data structure to a SXML tree.
; Example:
; @lisp
; (list->sxml '(TUPLETYPE (TYPENAME NATURAL) (TYPENAME NATURAL))) 
; @end lisp
(define (list->sxml alist)
  [assert (alist) (list? alist)]
  (if (list? alist)
    (internal-make-sxml-node (car alist) '() (map list->sxml (cdr alist)))
    alist))

;###
; Convert a SXML node in a list structure. Attributes and User data are ignored.
; Example:
; @lisp
; (define n (list->sxml '(TUPLETYPE (TYPENAME NATURAL) 
;                                   (TYPENAME NATURAL))))
; (sxml/set-user-datum! n 'id 10)
; (sxml->list n)
;   @result{} (TUPLETYPE (TYPENAME NATURAL) 
;   @result{}            (TYPENAME NATURAL))
; @end lisp
(define (sxml->list sxml)
  (cond ((sxml-node? sxml)
         `(,(sxml/tag sxml) ,@(map sxml->list (sxml/children sxml))))
        (else sxml)))

;###
; Return true if @code{sxm1} and @code{sxml2} are equal. This function does @emph{not}
; compare attributes or user data.
; @lisp
; (define n1 (list->sxml '(TUPLETYPE (TYPENAME NATURAL) 
;                                    (TYPENAME NATURAL))))
; (define n2 (list->sxml '(TUPLETYPE (TYPENAME INTEGER) 
;                                    (TYPENAME NATURAL))))
; (define n3 (list->sxml '(TUPLETYPE (TYPENAME NATURAL) 
;                                    (TYPENAME NATURAL))))
; (sxml/equal? n1 n2)
;   @result{} #f
; (sxml/equal? n1 n3)
;   @result{} #t
; @end lisp
(define (sxml/equal? sxml1 sxml2)
  (or (eq? sxml1 sxml2)
      (let ((sxml-node1? (sxml-node? sxml1))
            (sxml-node2? (sxml-node? sxml2)))
        (cond ((and sxml-node1? sxml-node2?)
               (if (eq? (sxml/tag sxml1) (sxml/tag sxml2))
                 (for-all sxml/equal? (sxml/children sxml1) (sxml/children sxml2))
                 #f))
              ((and (not sxml-node1?) (not sxml-node2?))
               (equal? sxml1 sxml2)) ;; equal? is used instead of eq?, since a leaf can be a symbol or string.
              (else #f)))))

(define (symbol-hash tag)
  (get-hashnumber tag))

;###
; Return the hash code of a SXML node.
(define (sxml/hash sxml)
  (cond 
   ((sxml-node? sxml)
    (+ (symbol-hash (sxml/tag sxml))
       (fold-left  (lambda (curr child) (+ (sxml/hash child) (* 3 curr)))
                   0 
                   (sxml/children sxml))))
   ((symbol? sxml)
    (symbol-hash sxml))
   (else
    1)))

;###
; Define a total order over SXML nodes. This function is useful to create
; balanced tree or ordered sets of SXML nodes.
(define (sxml/lt? sxml1 sxml2)
  (cond
   ((sxml-node? sxml1)
    (if (sxml-node? sxml2)
      (or (symbol<? (sxml/tag sxml1) (sxml/tag sxml2))
          (and (eq? (sxml/tag sxml1) (sxml/tag sxml2))
               (or (< (length (sxml/children sxml1)) (length (sxml/children sxml2)))
                   (and (= (length (sxml/children sxml1)) (length (sxml/children sxml2)))
                        (for-all sxml/lt? (sxml/children sxml1) (sxml/children sxml2))))))
      #f))
   ((symbol? sxml1)
    (if (symbol? sxml2)
      (symbol<? sxml1 sxml2)
      (not (or (string? sxml2) (number? sxml2)))))
   ((string? sxml1)
    (if (string? sxml2)
      (string<? sxml1 sxml2)
      (not (number? sxml2))))
   ((number? sxml1)
    (and
     (number? sxml2)
     (< sxml1 sxml2)))
   (else
    (internal-error))))

;###
; Create an user data entry that points to the parent node.
; Every node in the tree rooted by @code{sxml} will be updated.
; See the function @code{(sxml/parent sxml)}.
(define (sxml/set-parent! sxml)
  (let loop ((parent #f)
             (sxml sxml))
    (when (sxml-node? sxml)
      (when parent
        (sxml/set-attribute! sxml 'parent parent))
      (for-each (lambda (child)
                  (loop sxml child))
                (sxml/children sxml)))))

;###
; Return the parent of the node @code{sxml}.
; You have to use the function @code{(sxml/set-parent! sxml)}, before
; calling this function. If the information is not available, the function
; will return false.
; @lisp
; (define n (list->sxml '(TUPLETYPE (TUPLETYPE (TYPENAME NATURAL) 
;                                              (TYPENAME INTEGER)) 
;                                   (TYPENAME NATURAL))))
; (sxml/set-parent! n)
; (sxml/pp n)
;   @result{} (TUPLETYPE (TUPLETYPE (TYPENAME NATURAL) 
;   @result{}                       (TYPENAME INTEGER)) 
;   @result{}            (TYPENAME NATURAL))
; (sxml/pp (sxml/first-child n))
;   @result{} (TUPLETYPE (TYPENAME NATURAL) (TYPENAME INTEGER))
; (sxml/pp (sxml/parent (sxml/first-child n)))
;   @result{} (TUPLETYPE (TUPLETYPE (TYPENAME NATURAL) 
;   @result{}                       (TYPENAME INTEGER)) 
;   @result{}            (TYPENAME NATURAL))
; (sxml/parent n)
;   @result{} #f
; @end lisp
(define (sxml/parent sxml)
  (sxml/attribute sxml 'parent))

;--- End of SXML Auxiliary Functions


;-----------------------------------
; Tree traversal functions
; @cindex SXML tree traversal functions 

;###
; Traverse the @code{sxml} tree applying @code{proc} to each node.
(define (sxml/basic-traverse proc sxml)
  (let loop ((sxml sxml))
    (unless (eq? (proc sxml) 'do-not-traverse-children) ; if (proc sxml) returns 'do-not-traverse-children then I must not traverse sxml children.
      (if (sxml-node? sxml)
        (for-each loop (sxml/children sxml))))))

;###
; SXML tree walker.
; Semantics: 
; @itemize @bullet
; @item For each reached node in the tree apply the function @code{proc-before}. 
; @item If @code{proc-before} returns a value different from 'do-not-traverse-children, then traverse
;       the children nodes, after that apply the function @code{proc-after}. 
; @end itemize
(define (sxml/traverse proc-before sxml . proc-after)
  [assert (proc-after) (or (null? proc-after) (null? (cdr proc-after)))]
  (if (null? proc-after) 
    (sxml/basic-traverse proc-before sxml)
    (let ((proc-after (car proc-after)))
      (let loop ((sxml sxml))
        ;; if (proc-before sxml) returns 'do-not-traverse-children then I must not traverse sxml children.
        (unless (eq? (proc-before sxml) 'do-not-traverse-children) 
          (if (sxml-node? sxml)
            (for-each loop (sxml/children sxml))))
        (proc-after sxml)))))

;###
; Find a node @code{n} such that @code{(proc n)} returns true.
; The result is @code{n}, or false is the node was not found.
(define (sxml/find-node proc sxml)
  (bind-exit (exit)
    (sxml/traverse (lambda (n) (if (proc n) (exit n))) sxml)
    #f))


;--- End of SXML tree traverse functions

;=== End of SXML Chapter

