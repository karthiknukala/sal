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

;; Quick and dirty implementation of Literate programming...
;; This is an one hour implementation, don't try to use it
;; in other projects. Don't expect any kind of documentation or
;; support.
(module scmdoc
        (main scmdoc-main))

(define (trim astring)
  (if (equal? astring "")
    astring
    (let* ((length (string-length astring))
           (first-non-blank 
            (let loop ((pos 0))
              (if (< pos length)
                (if (not (memq (string-ref astring pos) '(#\space #\newline #\tab #a012)))
                  pos
                  (loop (+ pos 1)))
                pos)))
           (astring1 (substring astring first-non-blank length))
           (length1 (string-length astring1))
           (last-non-blank
            (let loop ((pos 0)
                       (last 0))
              (if (< pos length1)
                (if (not (memq (string-ref astring1 pos) '(#\space #\newline #\tab #a012)))
                  (loop (+ pos 1) pos)
                  (loop (+ pos 1) last))
                last))))
      (substring astring1 0 (+ last-non-blank 1)))))

(define *curr-line* 0)

(define-struct coord
   fname  ;; string  : the file of the coord
   line)

(define (adjust-char c)
  (when (eq? c #\newline)
    (set! *curr-line* (+ *curr-line* 1))))

(define (adjust-coord data)
  (cond 
   ((number? data)
    (set! *curr-line* (+ *curr-line* data)))
   ((string? data)
    (let ((len (string-length data)))
      (let loop ((pos 0))
        (when (< pos len)
          (adjust-char (string-ref data pos))
          (loop (+ pos 1))))))
   ((char? data)
    (adjust-char data))))

(define (the-coord input-port data)
  (let ((result (coord (input-port-name input-port)
                       *curr-line*)))
    (adjust-coord data)
    result))

(define *scmdoc-lexer*
  (regular-grammar
   ()
   ((: "(define-operation" (* (out #\newline)) #\newline)
    (list 'OPERATION (the-coord input-port 1) (the-string)))
   ((: "(define-macro" (* (out #\newline)) #\newline)
    (list 'MACRO (the-coord input-port 1) (the-string)))
   ((: "(define" (* (out #\newline)) #\newline)
    (list 'FUNCTION (the-coord input-port 1) (the-string)))
   ((: ";C(define-operation" (* (out #\newline)) #\newline)
    (list 'OPERATION (the-coord input-port 1) (the-substring 2 (the-length))))
   ((: ";C(define-macro" (* (out #\newline)) #\newline)
    (list 'MACRO (the-coord input-port 1) (the-substring 2 (the-length))))
   ((: ";C(define" (* (out #\newline)) #\newline)
    (list 'FUNCTION (the-coord input-port 1) (the-substring 2 (the-length))))
   ((: #\; (* (out #\newline)) #\newline)
    (list 'COMMENT (the-coord input-port 1) (the-string)))
   ((: #\" (* (or (out #\\ #\") (: #\\ all))) #\")
    (adjust-coord (the-string))
    (ignore))
   ((: #\# #\\ all)
    (ignore))
   ((bol (: (* (in #\space #\newline #\tab #a012)) #\newline))
    (list 'BLANK-LINE (the-coord input-port (the-string))))
   (else
    (let ((c (the-failure)))
      (if (eof-object? c)
        c
        (begin
          (adjust-coord (the-string))
          (ignore)))))))

(define (scmdoc/extract-documentation)
  (set! *curr-line* 1)
  (let ((read-code (lambda () (read/rp *scmdoc-lexer*
                                       (current-input-port)))))
    (let loop ((result '()))
      (let ((curr (read-code)))
        ;; (print "curr = " curr)
        (if (eof-object? curr)
          (reverse! result)
          (loop (cons curr result)))))))
      
(define (scmdoc/extract-documentation-from-string a-string)
  (with-input-from-string
      a-string
    scmdoc/extract-documentation))

(define (scmdoc/extract-documentation-from-file file-name)
  (with-input-from-file
      file-name
    scmdoc/extract-documentation))


(define (scmdoc/extract-documentation-from-files file-name-list)
  (let ((result '()))
    (for-each (lambda (file-name)
                (set! result (append! result (scmdoc/extract-documentation-from-file file-name))))
              file-name-list)
    result))

(define (doc-error msg elem)
  (let* ((c (cadr elem))
         (fname (coord-fname c))
         (line (coord-line c)))
    (error 'doc-error (string-append "[" fname ":line(" (number->string line) ")]: " msg) #unspecified)))

(define (scmdoc/chapter-title a-chapter)
  (let* ((header (car a-chapter))
         (title  (car (cadr header))))
    (trim title)))

(define (scmdoc/print-line-list line-list)
  (for-each (lambda (line)
              (display line))
            line-list))

(define (scmdoc/print-function fun-doc)
  (let ((elem-id (car fun-doc))
        (decl (trim (cadr fun-doc)))
        (body (cddr fun-doc)))
    (print "")
    (display* "@deffn {" (string-downcase (symbol->string elem-id)) "} ")
    (let ((len (string-length decl)))
      (let loop ((pos 0))
        (when (< pos len)
          (let ((c (string-ref decl pos)))
            (unless (memq c '(#\( #\)))
              (display c)))
          (loop (+ pos 1)))))
    (print "")
    (scmdoc/print-line-list body)
    (print "@end deffn")
    (print "")))

(define (scmdoc/gen-texi-doc a-list)
  (let ((chapters '())
        (curr-sections '())
        (curr-chapter #unspecified)
        (curr-section #unspecified)
        (curr-header #unspecified)
        (in-chapter? #f)
        (in-section? #f)
        (in-header? #f)
        (header-kind #unspecified))
    (for-each (lambda (elem)
                (let ((kind (car elem)))
                  (case kind
                    ((COMMENT)
                     (let* ((comment (caddr elem))
                            (id (string-ref comment 1)))
                       (case id
                         ((#\=)
                          (when (or in-section? in-header?)
                            (doc-error "Invalid chapter decl in the middle of section/header..." elem))
                          (if in-chapter?
                            (begin
                              ;; close the chapter
                              ;; (print "CLOSING...." elem)
                              (set! in-chapter? #f)
                              (set! curr-sections (reverse! curr-sections))
                              (unless (null? curr-sections)
                                (set! curr-chapter (cons `(SECTIONS ,curr-sections) curr-chapter)))
                              (set! curr-chapter (reverse! curr-chapter))
                              (set! chapters (cons curr-chapter chapters)))
                            (begin
                              ;; open a new chapter
                              ;; (print "OPENING...." elem)
                              (set! in-chapter? #t)
                              (set! in-header? #t)
                              (set! curr-chapter '())
                              (set! curr-header '())
                              (set! curr-sections '())
                              (set! header-kind 'CHAPTER))))
                         ((#\;)
                          ;; do nothing...
                          #unspecified
                          )
                         ((#\-)
                          (when in-header?
                            (doc-error "Invalid section decl in the middle of header..." elem))
                          (if in-section?
                            (begin
                              ;; close section
                              (set! in-section? #f)
                              (set! curr-section (reverse! curr-section))
                              (set! curr-sections (cons curr-section curr-sections)))
                            (begin
                              ;; open a new section
                              (set! in-section? #t)
                              (set! in-header? #t)
                              (set! curr-section '())
                              (set! curr-header '())
                              (set! header-kind 'SECTION))))
                         ((#\!)
                          (when in-header?
                            (doc-error "Invalid function decl in the middle of header..." elem))
                          (set! in-header? #t)
                          (set! header-kind 'EXTRA)
                          (set! curr-header '()))
                         ((#\#)
                          (when in-header?
                            (doc-error "Invalid function decl in the middle of header..." elem))
                          (set! in-header? #t)
                          (set! header-kind 'FUNCTION)
                          (set! curr-header '()))
                         ((#\*)
                          (when in-header?
                            (doc-error "Invalid operation decl in the middle of header..." elem))
                          (set! in-header? #t)
                          (set! header-kind 'OPERATION)
                          (set! curr-header '()))
                         ((#\+)
                          (when in-header?
                            (doc-error "Invalid macro decl in the middle of header..." elem))
                          (set! in-header? #t)
                          (set! header-kind 'MACRO)
                          (set! curr-header '()))
                         (else
                          (when in-header?
                            (let ((s (substring comment 2 (string-length comment))))
                              (set! curr-header (cons s curr-header)))))
                          )))
                    ((BLANK-LINE)
                     (when in-header?
                       (unless (memq header-kind '(SECTION CHAPTER EXTRA))
                         (doc-error "Invalid header terminator..." elem))
                       (set! curr-header (reverse! curr-header))
                       (case header-kind
                        ((SECTION)
                         (set! curr-section (list `(HEADER ,curr-header))))
                        ((CHAPTER)
                         (set! curr-chapter (list `(HEADER ,curr-header))))
                        ((EXTRA)
                         (set! curr-header `(EXTRA ,curr-header))
                         ;; (print "EXTRA = " curr-header)
                         (cond 
                          (in-section?
                           (set! curr-section (cons curr-header curr-section)))
                          (in-chapter?
                           (set! curr-chapter (cons curr-header curr-chapter)))
                          (else
                           ;; (pp chapters)
                           ;; (print in-chapter? " " in-section? " " in-header? " " header-kind " " curr-header)
                           (doc-error "Header out of Chapter/Section..." elem))))
                        (else 
                         (error 'scmdoc/gen-texi-doc "Invalid header kind..." elem)))
                       (set! in-header? #f)))
                    ((OPERATION FUNCTION MACRO)
                     (when in-header?
                       (unless (eq? header-kind kind)
                         (doc-error "Invalid header terminator..." elem))
                       (let* ((str (caddr elem))
                              (decl (substring str
                                               (case kind
                                                 ((OPERATION) 17)
                                                 ((FUNCTION) 7)
                                                 ((MACRO) 13))
                                               (string-length str))))
                         (set! curr-header (reverse! curr-header))
                         (set! curr-header `(,kind ,decl ,@curr-header))
                         (cond 
                          (in-section?
                           (set! curr-section (cons curr-header curr-section)))
                          (in-chapter?
                           (set! curr-chapter (cons curr-header curr-chapter)))
                          (else
                           ;; (pp chapters)
                           ;; (print in-chapter? " " in-section? " " in-header? " " header-kind " " curr-header)
                           (doc-error "Header out of Chapter/Section..." elem)))
                         )
                       (set! in-header? #f)
                       ))
                    (else
                     (error 'scmdoc/gen-texi-doc "Invalid doc kind" kind)))))
              a-list)
    (set! chapters (reverse! chapters))
    ;; gen top level menu
    (print "@menu")
    (for-each (lambda (chapter)
                (print "* " (scmdoc/chapter-title chapter) "::"))
              chapters)
    (print "@end menu")
    (print "")
    ;; print chapters
    (let ((num-chapters (length chapters)))
      (let loop ((curr-chapter-id 0))
        (when (< curr-chapter-id num-chapters)
          (let* ((chapter (list-ref chapters curr-chapter-id))
                 (title (scmdoc/chapter-title chapter))
                 (header-body (cdr (cadr (car chapter))))
                 (chapter-body (cdr chapter))
                 (prev-title (if (= curr-chapter-id 0)
                               ""
                               (scmdoc/chapter-title (list-ref chapters (- curr-chapter-id 1)))))
                 (next-title (if (= curr-chapter-id (- num-chapters 1))
                               ""
                               (scmdoc/chapter-title (list-ref chapters (+ curr-chapter-id 1)))))
                 )
            (print "@node " title ", " next-title ", " prev-title ", Top")
            (print "@chapter " title)
            (print "@cindex " title)
            ;; print header body
            (scmdoc/print-line-list header-body)
            ;; print chapter body
            (for-each (lambda (elem)
                        (let ((elem-id (car elem)))
                          (case elem-id
                            ((FUNCTION OPERATION MACRO)
                             (scmdoc/print-function elem))
                            ((EXTRA)
                             (scmdoc/print-line-list (cadr elem)))
                            ((SECTIONS)
                             (let ((sections (cadr elem)))
                               ;; gen menu
                               (print "@menu")
                               (for-each (lambda (section)
                                           (print "* " (scmdoc/chapter-title section) "::"))
                                         sections)
                               (print "@end menu")
                               (print "")
                               ;; print sections
                               (let ((num-sections (length sections)))
                                 (let loop ((curr-section-id 0))
                                   (when (< curr-section-id num-sections)
                                     (let* ((section (list-ref sections curr-section-id))
                                            (chapter-title title)
                                            (title (scmdoc/chapter-title section))
                                            (prev-title (if (= curr-section-id 0)
                                                          ""
                                                          (scmdoc/chapter-title (list-ref sections (- curr-section-id 1)))))
                                            (next-title (if (= curr-section-id (- num-sections 1))
                                                          ""
                                                          (scmdoc/chapter-title (list-ref sections (+ curr-section-id 1)))))
                                            (header-body (cdr (cadr (car section))))
                                            (section-body (cdr section)))
                                       (print "@node " title ", " next-title ", " prev-title ", " chapter-title)
                                       (print "@section " title)
                                       (print "@cindex " title)
                                       ;; print header-body
                                       (scmdoc/print-line-list header-body)
                                       ;; print section body
                                       (for-each (lambda (elem)
                                                   (let ((elem-id (car elem)))
                                                     (case elem-id
                                                       ((FUNCTION OPERATION MACRO)
                                                        (scmdoc/print-function elem))
                                                       ((EXTRA)
                                                        (scmdoc/print-line-list (cadr elem)))
                                                       (else
                                                        (error 'scmdoc/gen-texi-doc "Invalid section element..." elem)))))
                                                 section-body))
                                     (loop (+ curr-section-id 1)))))
                               ))
                            (else
                             (error 'scmdoc/gen-texi-doc "Invalid chapter element" #unspecified)))))
                      chapter-body)
            (print "")
            )
          (loop (+ curr-chapter-id 1)))))
    ))

(define (scmdoc/gen-doc-db a-list)
  (let ((result '())
        (in-header? #f)
        (header-kind #unspecified)
        (curr-header '())
        (kind-table '((#\# . FUNCTION) (#\* . OPERATION) (#\+ . MACRO))))
    (for-each (lambda (elem)
                (let ((kind (car elem)))
                  (case kind
                    ((COMMENT)
                     (let* ((comment (caddr elem))
                            (id (string-ref comment 1)))
                       (case id
                         ((#\= #\; #\- #\!)
                          ;; do nothing
                          #unspecified)
                         ((#\# #\* #\+)
                          (when in-header?
                            (doc-error "Invalid decl in the middle of header..." elem))
                          (set! in-header? #t)
                          (set! header-kind (cdr (assq id kind-table)))
                          (set! curr-header '()))
                         (else
                          (when in-header?
                            (let ((s (substring comment 2 (string-length comment))))
                              (set! curr-header (cons s curr-header)))))
                          )))
                    ((BLANK-LINE)
                     (when in-header?
                       (doc-error "Invalid header terminator..." elem))
                     #unspecified)
                    ((OPERATION FUNCTION MACRO)
                     (when in-header?
                       (unless (eq? header-kind kind)
                         (doc-error "Invalid header terminator..." elem))
                       (let* ((str (caddr elem))
                              (decl (substring str
                                               (case kind
                                                 ((OPERATION) 17)
                                                 ((FUNCTION) 7)
                                                 ((MACRO) 13))
                                               (string-length str))))
                         (set! curr-header (reverse! curr-header))
                         (set! curr-header `(,kind ,(trim decl) ,@curr-header))
                         (set! result (cons curr-header result)))
                       (set! in-header? #f)
                       ))
                    (else
                     (error 'scmdoc/gen-texi-doc "Invalid doc kind" kind)))))
              a-list)
    (for-each (lambda (header)
                (let ((kind (car header))
                      (decl (cadr header))
                      (lines (cddr header)))
                  (print kind)
                  (print (trim decl))
                  (scmdoc/print-line-list lines)
                  (print "@@@END@@@")))
              (sort result
                    (lambda (header-1 header-2)
                      (string<? (cadr header-1)
                                (cadr header-2)))))))


;;
;; (scmdoc/gen-doc-db (scmdoc/extract-documentation-from-files '("/homes/demoura/project/sal/salenv2/src/yasos.macros")))
;; (scmdoc/gen-texi-doc (scmdoc/extract-documentation-from-files '("/homes/demoura/project/sal/salenv2/src/yasos.macros")))
;;                                                                "/homes/demoura/project/sal/salenv2/src/sal-environment.scm")))
                                          

(define (scmdoc-main argv)
  (let ((input-files (cdr argv)))
    (if (eq? (string->number (car input-files)) 0)
      (scmdoc/gen-doc-db (scmdoc/extract-documentation-from-files (cdr input-files)))
      (scmdoc/gen-texi-doc (scmdoc/extract-documentation-from-files input-files)))))
    
