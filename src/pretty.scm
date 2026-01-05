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

(module pretty
        (include "sal.sch")
        (include "pretty.macros")
        (import gmp-scheme symbol-set symbol-table sxml-package) 
        (export <pp-info> <doc-nil> <doc-concat> <doc-nest> <doc-text> <doc-line> <doc-line++> <doc-union>
                (make-default-pp-info)
                *doc-nil* 
                *doc-line* 
                (pp/line . str) 
                (pp/text str) 
                (pp/concat . docs) 
                (pp/nest i doc)
                (pp/nest* i . docs)
                (pp/union . docs)
                (pp/group doc)
                (pp/pack . docs)
                (pp/list-style1-core tag op cp . doc-lst)
                (pp/list-style1 tag . doc-lst)
                (pp/list-style2-core tag op cp indent . doc-lst)
                (pp/list-style2 tag indent . doc-lst)
                (pp/list-style3-core tag op cp indent i . doc-list)
                (pp/list-style3 tag indent i . doc-lst)
                (pp/list-style4-core tag op cp indent . doc-lst)
                (pp/list-style4 tag indent . doc-lst)
                (pp/list-style5-core op cp indent . doc-lst)
                (pp/list-style5 indent . doc-lst)
                (pp/list-style6-core op cp . doc-lst)
                (pp/list-style6 . doc-lst)
                (pp/flatten doc)
                (pp/pretty doc pp-info)
                <pp-info++>
                (make-default-pp-info++)
                (object->doc obj pp-info depth)
                (pp/object obj pp-info)
                )
        )

(define-class <pp-info> () (:max-width :max-ribbon :max-num-lines :max-indent :single-line? :bounded?))

(define *big-number* 1048576)

(define (make-default-pp-info) 
  (make-instance <pp-info>
                 :max-width 80
                 :max-ribbon 30
                 :max-num-lines *big-number*
                 :max-indent *big-number*
                 :single-line? #f
                 :bounded? #f))

(define-class <doc> () ())
(define-class <doc-nil> (<doc>) ())
(define-class <doc-concat> (<doc>) (:docs))
(define-class <doc-nest> (<doc>) (:i :doc))
(define-class <doc-text> (<doc>) (:str))
(define-class <doc-line> (<doc>) ())
(define-class <doc-line++> (<doc-line>) (:str))
(define-class <doc-union> (<doc>) (:docs))

(define *doc-nil* (make-instance <doc-nil>))
(define *doc-line* (make-instance <doc-line>))

(define (pp/line . str)
  (if (null? str)
    *doc-line*
    (make-instance <doc-line++> :str (car str))))

(define (pp/text str)
  (make-instance <doc-text> :str str))

(define (pp/concat . docs)
  (cond 
   ((null? docs)
    *doc-nil*)
   ((null? (cdr docs))
    (car docs))
   (else
    (make-instance <doc-concat> :docs docs))))

(define (pp/nest i doc) 
  (make-instance <doc-nest> :i i :doc doc))

(define (pp/nest* i . docs)
  (pp/nest i (apply pp/concat docs)))

(define (pp/union . docs)
  (cond 
   ((null? docs)
    *doc-nil*)
   ((null? (cdr docs))
    (car docs))
   (else
    (make-instance <doc-union> :docs docs))))

(define (pp/group doc)
  (pp/union (pp/flatten doc) doc))

(define-generic (pp/flatten doc))
(define-method (pp/flatten (doc <primitive>))
  doc)
(define-method (pp/flatten (doc <doc-concat>))
  (copy-instance doc :docs (map pp/flatten (slot-value doc :docs))))
(define-method (pp/flatten (doc <doc-nest>))
  (copy-instance doc :doc (pp/flatten (slot-value doc :doc))))
(define-method (pp/flatten (doc <doc-union>))
  (pp/flatten (car (slot-value doc :docs))))
(define-method (pp/flatten (doc <doc-line>))
  " ")
(define-method (pp/flatten (doc <doc-line++>))
  (slot-value doc :str))

(define-class <idoc> () ())
(define-class <idoc-nil> (<idoc>) ())
(define-class <idoc-text> (<idoc>) (:str :idoc))
(define-class <idoc-line> (<idoc>) (:i :idoc))

(define *idoc-nil* (make-instance <idoc-nil>))

(define-generic (pp/layout doc pp-info line))
(define-method (pp/layout (doc <idoc-nil>) (pp-info <pp-info>) (line <primitive>))
  #unspecified)
(define-method (pp/layout (doc <idoc-text>) (pp-info <pp-info>) (line <primitive>))
  (display (slot-value doc :str))
  (pp/layout (force (slot-value doc :idoc)) pp-info line))
(define-method (pp/layout (doc <idoc-line>) (pp-info <pp-info>) (line <primitive>))
  (cond
   ((<= line (slot-value pp-info :max-num-lines))
    (print "")
    (display (make-string (slot-value doc :i)))
    (pp/layout (force (slot-value doc :idoc)) pp-info (+ line 1)))
   (else
    (print " ..."))))

(define-generic (fits doc w))
(define-method (fits (doc <idoc>) (space-left <primitive>))
  (>= space-left 0))
(define-method (fits (doc <idoc-text>) (space-left <primitive>))
  (and (call-next-method)
       (fits (force (slot-value doc :idoc))
             (- space-left (string-length (slot-value doc :str))))))

(define (pp/best doc pp-info pos ribbon-pos)
  (be (list (cons 0 doc)) pp-info pos ribbon-pos))

(define (be to-do-list pp-info pos ribbon-pos)
  (if (null? to-do-list)
    *idoc-nil*
    (let* ((curr (car to-do-list))
           (indent (car curr))
           (doc (cdr curr)))
      (be-next doc pp-info indent (cdr to-do-list) pos ribbon-pos))))

(define-generic (be-next doc pp-info indent to-do-list pos ribbon-pos))
(define-method (be-next (doc <primitive>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>)  (pos <primitive>) (ribbon-pos <primitive>))
  (let* ((bounded? (slot-value pp-info :bounded?))
         (str (object->string doc))
         (len (string-length str))
         (default (lambda (str delta)
                    (make-instance <idoc-text>
                                   :str str
                                   :idoc (delay (be to-do-list pp-info (+ pos delta) (+ ribbon-pos delta)))))))
    (if bounded?
      (let ((max-width (slot-value pp-info :max-width)))
        (cond
         ((> pos max-width)
          (be to-do-list pp-info pos ribbon-pos))
         ((> (+ pos len) max-width)
          (default " ..." len))
         (else
          (default str len))))
      (default str len))))
(define-method (be-next (doc <doc-nil>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>) (pos <primitive>) (ribbon-pos <primitive>))
  (be to-do-list pp-info pos ribbon-pos))
(define-method (be-next (doc <doc-concat>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>) (pos <primitive>) (ribbon-pos <primitive>))
  (be (append (map (cut cons indent <>) (slot-value doc :docs)) to-do-list)
      pp-info pos ribbon-pos))
(define-method (be-next (doc <doc-nest>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>) (pos <primitive>) (ribbon-pos <primitive>))
  (let ((new-indent (min (+ indent (slot-value doc :i))
                         (slot-value pp-info :max-indent))))
    (be (cons (cons new-indent (slot-value doc :doc)) to-do-list)
        pp-info
        pos
        ribbon-pos)))
(define-method (be-next (doc <doc-text>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>) (pos <primitive>) (ribbon-pos <primitive>))
  (be-next (slot-value doc :str) pp-info indent to-do-list pos ribbon-pos))
(define-method (be-next (doc <doc-line>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>) (pos <primitive>) (ribbon-pos <primitive>))
  (if (slot-value pp-info :single-line?) ;; ignore line breaks
    (be-next " " pp-info indent to-do-list pos ribbon-pos)
    (make-instance <idoc-line>
                   :i indent
                   :idoc (delay (be to-do-list pp-info indent 0)))))
(define-method (be-next (doc <doc-union>) (pp-info <pp-info>) (indent <primitive>) (to-do-list <primitive>) (pos <primitive>) (ribbon-pos <primitive>))
  (let loop ((docs (slot-value doc :docs)))
    [assert (docs) (not (null? docs))]
    (let* ((curr-doc (car docs))
           (curr-layout (be (cons (cons indent curr-doc)
                                  to-do-list)
                            pp-info
                            pos
                            ribbon-pos)))
      (if (or (null? (cdr docs))
              (slot-value pp-info :single-line?) ;; since we are ignoring line breaks
              (fits curr-layout 
                    (min (- (slot-value pp-info :max-width) pos) 
                         (- (slot-value pp-info :max-ribbon) ribbon-pos))))
        curr-layout
        (loop (cdr docs))))))

(define (pp/pretty doc pp-info)
  (pp/layout (pp/best doc pp-info 0 0) pp-info 0))

(define (pp/pack . docs)
  (if (null? docs)
    *doc-nil*
    (let* ((doc-vect (list->vector docs))
           (size (vector-length doc-vect)))
      (let loop ((l 0)
                 (u (- size 1)))
        (cond
         ((< u l)
          *doc-nil*)
         ((= u l)
          (vector-ref doc-vect u))
         (else
          (let* ((l1 l)
                 (u1 (+ l (/fx (- u l) 2)))
                 (l2 (+ u1 1))
                 (u2 u))
            (pp/group (pp/concat (loop l1 u1) *doc-line* (loop l2 u2))))))))))

;; auxiliary function:
;;   child1
;;   child2
;;   child3
(define (pp/list-aux lst)
  (apply pp/concat (map-and-filter (lambda (d)
                                     (and (not (eq? d *doc-nil*))
                                          (pp/concat *doc-line* d)))
                                   lst)))

;; Style 1:
;; (tag child1
;;      child2
;;      child3)
(define (pp/list-style1-core tag op cp . doc-lst)
  (trace 'pretty "tag = ~a" tag)
  (let ((tag (to-string tag)))
    (if (null? doc-lst)
      (pp/concat op tag cp)
      (pp/group (pp/concat op tag 
                           (pp/nest* (+ (string-length tag) (string-length op) 1) 
                                     " " (car doc-lst) (pp/list-aux (cdr doc-lst)))
                           cp)))))

(define (pp/list-style1 tag . doc-lst)
  (apply pp/list-style1-core tag "(" ")" doc-lst))

;; Style 2:
;; (tag
;;   child1
;;   child2
;;   child3)
(define (pp/list-style2-core tag op cp indent . doc-lst)
  (if (null? doc-lst)
    (pp/concat op tag cp)
    (pp/group (pp/concat op tag 
                         (pp/nest indent (pp/list-aux doc-lst))
                         cp))))

(define (pp/list-style2 tag indent . doc-lst)
  (apply pp/list-style2-core tag "(" ")" indent doc-lst))

;; Style 3:
;; (tag child_1 
;;      ...
;;      child_i
;;   child_i+1
;;   child_i+2
;;   ...)
(define (pp/list-style3-core tag op cp indent i . doc-list)
  [assert (doc-list i) (<= i (length doc-list))]
  (let ((tag (to-string tag))
        (l1 (list-head doc-list i))
        (l2 (list-tail doc-list i)))
    (pp/group (pp/concat op tag 
                         (pp/group (pp/nest* (+ (string-length tag) (string-length op) 1) 
                                             " " (car l1) (pp/list-aux (cdr l1))))
                         (pp/nest indent (pp/list-aux l2))
                         cp))))

(define (pp/list-style3 tag indent i . doc-list)
  (apply pp/list-style3-core tag "(" ")" indent i doc-list))

;; Style 4:
;; (tag child1
;;   child2
;;   child3)
(define (pp/list-style4-core tag op cp indent . doc-lst)
  (apply pp/list-style3-core tag op cp indent 1 doc-lst))

(define (pp/list-style4 tag indent . doc-lst)
  (apply pp/list-style3 tag indent 1 doc-lst))
  
;; Style 5:
;; (child1
;;   child2
;;   child3)
(define (pp/list-style5-core op cp indent . doc-lst)
  (if (null? doc-lst)
    (pp/concat op cp)
    (pp/group (pp/concat op (car doc-lst)
                         (pp/nest indent (pp/list-aux (cdr doc-lst)))
                         cp))))

(define (pp/list-style5 indent . doc-lst)
  (apply pp/list-style5-core "(" ")" indent doc-lst))

;; Style 6:
;; (child1
;;  child2
;;  child3)
(define (pp/list-style6-core op cp . doc-lst)
  (apply pp/list-style5-core op cp (string-length (to-string op)) doc-lst))

(define (pp/list-style6 . doc-lst)
  (apply pp/list-style5 1 doc-lst))
    
(define-class <pp-info++> (<pp-info>) (:max-depth :default-indent :simplify-qualified-names?))

(define (make-default-pp-info++)
  (let ((tmp (make-default-pp-info)))
    [assert (tmp) (instance? tmp)]
    (copy-instance (change-class tmp <pp-info++>) 
                   :max-depth *big-number* :default-indent 2)))

(define-generic (object->doc doc pp-info depth))

(define (pp/symbol-set doc pp-info depth)
  (cond
   ((>= depth (slot-value pp-info :max-depth))
    (pp/concat "{...}"))
   ((symbol-set/empty? doc)
    "{}")
   (else
    (pp/concat "{" (pp/nest 1 (apply pp/pack (symbol-set->list doc)))
               "}"))))

(define (pp/symbol-table doc pp-info depth)
  (cond 
   ((>= depth (slot-value pp-info :max-depth))
    "{...}")
   ((symbol-table/empty? doc)
    "{}")
   (else
    (let ((first? #t))
      (pp/concat "{" (pp/nest 1 (apply pp/concat (map-symbol-table->list
                                                  (lambda (key value)
                                                    (pp/concat (if first? (begin (set! first? #f) *doc-nil*) (pp/concat "," *doc-line*))
                                                               key " |-> " (pp/nest (+ (symbol-length key) 5)
                                                                                    (object->doc value pp-info (+ depth 1)))))
                                                  doc)))
                 "}")))))

(define (pp/struct doc pp-info depth)
  (cond
   ((>= depth (slot-value pp-info :max-depth))
    (pp/concat "#{" (struct-key doc) " ...}"))
   (else
    (let* ((depth (+ depth 1))
           (doc-lst (map (cut object->doc <> pp-info depth) (sal_struct->list doc))))
      (apply pp/list-style1-core (struct-key doc) "#{" "}" doc-lst)))))
   
(define (pp/vector doc pp-info depth)
  (pp/concat "#" (object->doc (vector->list doc) pp-info depth)))

(define (pp/scheme-list doc pp-info depth)
  (cond
   ((null? doc)
    "()")
   ((>= depth (slot-value pp-info :max-depth))
    "(...)")
   ((null? (cdr doc))
    (pp/concat "(" (object->doc (car doc) pp-info (+ depth 1)) ")"))
   (else
    (let* ((depth (+ depth 1))
           (first (car doc))
           (second (cadr doc))
           (rest (cddr doc))
           (body (apply pp/concat
                        (map (lambda (e)
                               (pp/concat *doc-line* (object->doc e pp-info depth)))
                             rest))))
      (pp/group
       (pp/concat
        (pp/group (pp/concat "(" 
                             (object->doc first pp-info depth) 
                             (pp/nest* (slot-value pp-info :default-indent) *doc-line* 
                                       (object->doc second pp-info depth))))
        (pp/nest (slot-value pp-info :default-indent) body)
        ")"))))))
    
(define (pp/scheme-pair doc pp-info depth)
  (cond 
   ((>= depth (slot-value pp-info :max-depth))
    "(...)")
   (else
    (let ((depth (+ depth 1)))
      (pp/group 
       (pp/concat "("
                  (object->doc (car doc) pp-info depth)
                  (let loop ((rest (cdr doc)))
                    (if (pair? rest)
                      (pp/concat (pp/nest* (slot-value pp-info :default-indent) *doc-line* (object->doc (car rest) pp-info depth))
                                 (loop (cdr rest)))
                      (pp/concat (pp/nest* (slot-value pp-info :default-indent) *doc-line* ". " (object->doc rest pp-info depth)))))
                  ")"))))))

(define (pp/instance doc pp-info depth)
  (let ((c (class-of doc)))
    (cond
     ((>= depth (slot-value pp-info :max-depth))
      (pp/concat "[" (slot-value c :class-name) " ...]"))
     (else
      (let* ((slots (slot-value c :slots))
             (depth (+ depth 1))
             (c-name (to-string (slot-value c :class-name)))
             (c-name-len (string-length c-name))
             (slot-docs (map (lambda (slot-id)
                               (pp/concat (object->doc slot-id pp-info depth)
                                          (pp/nest* (+ (string-length (keyword->string slot-id)) 1)
                                                    " "
                                                    (object->doc (slot-value doc slot-id) pp-info depth))))
                             slots)))            
        (pp/concat "[" c-name (if (null? slot-docs) "" " ") (pp/nest (+ 2 c-name-len) (apply pp/pack slot-docs)) "]"))))))
  
(define-method (object->doc (obj <primitive>) (pp-info <pp-info++>) (depth <primitive>))
  (let ((c (class-of obj)))
    (cond
     ((not (eq? c <primitive>))
      (pp/instance obj pp-info depth))
     ((sxml-node? obj)
      (object->doc (sxml->list obj) pp-info depth))
     ((symbol-set? obj)
      (pp/symbol-set obj pp-info depth))
     ((symbol-table? obj)
      (pp/symbol-table obj pp-info depth))
     ((struct? obj)
      (pp/struct obj pp-info depth))
     ((vector? obj)
      (pp/vector obj pp-info depth))
     ((list? obj)
      (pp/scheme-list obj pp-info depth))
     ((pair? obj)
      (pp/scheme-pair obj pp-info depth))
     ((string? obj)
      obj)
      ;; (string-append "\"" obj "\""))
     ((symbol? obj)
      (symbol->string obj))
     ((keyword? obj)
      (keyword->string obj))
     ((number? obj)
      (number->string obj))
     ((char? obj)
      (make-string 1 obj))
     ((mpq? obj)
      (mpq->string obj))
     ((procedure? obj)
      "#<procedure>")
     ((boolean? obj)
      (if obj "#t" "#f"))
     ((eq? obj #unspecified)
      "") ;;"#unspecified")
     ((input-port? obj)
      "#[input-port]")
     ((output-port? obj)
      "#[output-port]")
     ((eof-object? obj)
      "#[eof-object]")
     (else
      "#unknown"))))

(define-method (object->doc :around (obj <primitive>) (pp-info <pp-info++>) (depth <primitive>))
  (if (> depth (slot-value pp-info :max-depth))
    "..."
    (call-next-method)))

(define (pp/object obj pp-info)
  (pp/pretty (object->doc obj pp-info 0) pp-info))
    

