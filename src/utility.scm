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

(module utility
        (include "utility.macros")
        (extern 
         ;; Modern Bigloo (4.x) GC interface
         ;; These functions are part of the Boehm GC used by Bigloo
         (GC_gcollect::void () "GC_gcollect")
         (GC_invoke_finalizers::int () "GC_invoke_finalizers"))
        (import xformat gmp-scheme)
        (export (force-gc!)
                (natural? obj)
                (identity x)
                (compose f g)
                (flip f)
                (curry f)
                (for-each->fold for-each-proc)
                (for-each->find for-each-proc)
                (for-each->exists for-each-proc)
                (for-each->for-all for-each-proc)
                (for-all-1 proc lst)
                (for-all-2 proc lst1 lst2)
                (for-all proc . lst)
                (for-all-sublist-1 proc lst)
                (for-all-sublist proc . lst)
                (exists proc lst)
                (fold-left proc base lst)               
                (fold-right proc lst base)
                (list-max proc lst)
                (map-and-filter proc lst)
                (conservative-map-1 proc lst)
                (conservative-map-2 proc lst1 lst2)
                (conservative-map proc . lsts)
                (conservative-filter proc lst)
                (conservative-map-filter proc lst)
                (list-copy lst)
                (list-head alist k)
                (list-last-element list)
                (sal_struct->list struct)
                (find proc lst)
                (replace old new lst)
                (replace! old new lst)
                (generate-list proc num-elems)
                (generic-member-pos alist obj eq?)
                (split-list lst pred?)
                (member-pos alist obj)
                (memq-pos alist obj)
                (map-with-pos proc list)
                (associated elem mapping is-equal?)
                (inv-associated elem mapping is-equal?)
                (inv-assoc elem mapping)
                (inv-assq elem mapping)
                (adjoin obj lst)
                (union lst1 lst2)
                (difference lst1 lst2)
                (subset? lst1 lst2)
                (set-eq? lst1 lst2)
                (invert-mapping mapping)
                (intersection lst1 lst2)
                (product proc lst1 lst2)
                (remove-if pred lst)
                (delete-duplicates l)
                (portable-string->symbol str)
                (to-symbol symbol-or-string)
                (to-string symbol-or-string)
                (string-or-symbol? obj)
                (symbol-length symb)
                (object->string obj)
                (object->symbol obj)
                (sal-string-index-ci str a-char . start-pos)
                (sal-string-index str a-char . start-pos)
                (string-last-index str a-char . end-pos)
                (string-last-index-ci str a-char . end-pos)
                (char-pos achar astring)
                (inline space? char)
                (trim astring)
                (delimited-string->list delimiters a-string)
                (load-config-file file)
                (vector/for-each proc vector)
                (vector/fold proc init vector)
                (vector/find proc vector)
                (inline vector/swap-elements! vect i j)
                (vector/fill-from-to! vect from to value)
                (obj->boolean obj)
                (gen-temp-file prefix)
                (add-temp-dir-prefix file-name)
                (closest-greater-power-of-2 num)
                ;; fast symbol comparison functions
                (symbol<? s1 s2)
                (symbol<=? s1 s2)
                (symbol>? s1 s2)
                (symbol>=? s1 s2)
                ;; error messages
                (register-app-error! kind)
                (app-error? kind)
                (sign-error msg . args)
                (sign-usage-error proc msg . args)
                (fail)
                (internal-error)
                (unreachable-code)
                (warning-message msg . args)
                (verbose-message-core msg . args)
                (status-message-core . args)
                (set-verbosity! new-lv)
                (verbosity-level)
                *verbosity-level*
                *status-messages?*
                (status-messages-enabled?)
                (enable-status-messages! flag)
                *global-def-aux*
                (dynamic-define name value)
                *util-trace-stack-depth*
                (dump-exec-stack)
                (svsv/value svsv key default-value)
                (svsv/for-each proc svsv)
                (indent n)
                (display-indented n . args)
                (print-indented level . args)
                (display-boxed begin-column end-column . args)
                (print-boxed begin-column end-column . args)
                (print-file file-name)
                (grep/contains? file str)))


(define (force-gc!)
  (GC_gcollect)
  (GC_invoke_finalizers)
  #unspecified)

;; NOTE: On ARM64 (Apple Silicon), finalization is disabled in cudd_c_utils.c
;; to prevent bus errors. See cudd_new_manager() for details.

(define (natural? obj)
  (and (integer? obj) (>= obj 0)))

(define (identity x) x)

(define (compose f g) (lambda (x) (f (g x))))

(define (flip f)
  (lambda (x y)
    (f y x)))

(define (curry f)
  (lambda (x)
    (lambda (. args)
      (apply f (cons x args)))))

;; (define (make-list n . init)
;;  (if (pair? init) (set! init (car init)))
;;  (let loop ((answer '())
;;             (n n))
;;    (if (<= n 0)
;;      answer
;;      (loop (cons init answer) (- n 1)))))

(define (make-gc-buffer n)
  (let ((buffer (make-vector n #f))
        (pos 0))
    (lambda (next)
      (when (>= pos n)
        (set! pos 0))
      (vector-set! buffer pos next)
      (set! pos (+ pos 1)))))

(define (for-each->fold for-each-proc)
  (lambda (proc initial col)
    (let ((result initial))
      (for-each-proc 
       (lambda (curr)
         (set! result (proc result curr)))
       col)
      result)))

(define (for-each->find for-each-proc)
  (lambda (proc col)
    (bind-exit (exit)
      (for-each-proc
       (lambda (n)
         (when (proc n)
           (exit n)))
       col)
      #f)))

(define (for-each->exists for-each-proc)
  (let ((find (for-each->find for-each-proc)))
    (lambda (proc col)
      (and (find proc col) #t))))

(define (for-each->for-all for-each-proc)
  (lambda (proc col)
    (bind-exit (exit)
      (for-each-proc
       (lambda (n)
         (unless (proc n)
           (exit #f)))
       col)
      #t)))

(define (for-all-1 proc lst)
  (let loop ((lst lst))
    [assert (lst) (list? lst)]
    (or (null? lst)
        (and (proc (car lst)) (loop (cdr lst))))))

(define (for-all-2 proc lst1 lst2)
  (let loop ((lst1 lst1) (lst2 lst2))
    [assert (lst1) (list? lst1)]
    [assert (lst2) (list? lst2)]
    (if (and (not (null? lst1)) (not (null? lst2)))
      (and (apply proc (list (car lst1) (car lst2))) (loop (cdr lst1) (cdr lst2)))
      (and (null? lst1) (null? lst2)))))

(define (for-all proc . lsts)
  (cond ((null? lsts)
         #unspecified)
        ((null? (cdr lsts))
         (for-all-1 proc (car lsts)))
        ((null? (cddr lsts))
         (for-all-2 proc (car lsts) (cadr lsts)))
        (else (let loop ((lsts lsts))
                (if (for-all (lambda (x) (not (null? x))) lsts) ;; check if all lists are not null!
                  (and (apply proc (map car lsts)) (loop (map cdr lsts)))
                  (for-all (lambda (x) (null? x)) lsts))))))

(define (for-all-sublist-1 proc lst)
  (let loop ((lst lst))
    (or (null? lst)
        (and (proc lst) (loop (cdr lst))))))

(define (for-all-sublist proc . lsts)
  (cond ((null? lsts)
         #unspecified)
        ((null? (cdr lsts))
         (for-all-sublist-1 proc (car lsts)))
        (else (let loop ((lsts lsts))
                (if (for-all (lambda (x) (not (null? x))) lsts) ;; check if all lists are not null!
                  (and (apply proc lsts) (loop (map cdr lsts)))
                  (for-all (lambda (x) (null? x)) lsts))))))
  

(define (exists proc lst)
  (not (for-all (lambda (elem) (not (proc elem))) lst)))

(define (fold-right proc lst  base)
  (let loop ((lst lst))
    (if (null? lst)
      base
      (proc (car lst) (loop (cdr lst))))))

(define (fold-left proc base lst)
  (let loop ((curr base)
             (lst lst))
    (if (null? lst)
      curr
      (loop (proc curr (car lst)) (cdr lst)))))

(define (list-max proc lst)
  (fold-left (lambda (val elem)
               (max val (proc elem)))
             0
             lst))

; Now, Filter is a builtin
; (define (filter proc lst)
;   (let loop ((lst lst)
;              (result '()))
;     (if (null? lst)
;         (reverse! result)
;         (loop (cdr lst) (if (proc (car lst)) (cons (car lst) result) result)))))

(define (map-and-filter proc lst)
  (let loop ((lst lst)
             (result '()))
    (if (null? lst)
      (reverse! result)
      (loop (cdr lst) (let ((new-elem (proc (car lst))))
                        (if new-elem
                          (cons new-elem result)
                          result))))))
    
(define (conservative-map-1 proc lst)
  (let loop ((lst lst))
    (if (null? lst)
      lst
      (let* ((curr (car lst))
             (new-curr (proc curr))
             (rest (cdr lst))
             (new-rest (loop rest)))
        (if (and (eq? curr new-curr)
                 (eq? rest new-rest))
          lst
          (cons new-curr new-rest))))))

(define (conservative-map-2 proc lst1 lst2)
  (let loop ((lst1 lst1)
             (lst2 lst2))
    [assert (lst1 lst2) (eq? (null? lst1) (null? lst2))]
    (if (and (null? lst1)
             (null? lst2))
      lst1
      (let* ((curr1 (car lst1))
             (curr2 (car lst2))
             (new-curr (proc curr1 curr2))
             (rest1 (cdr lst1))
             (rest2 (cdr lst2))
             (new-rest (loop rest1 rest2)))
        (if (and (eq? curr1 new-curr)
                 (eq? rest1 new-rest))
          lst1
          (cons new-curr new-rest))))))

(define (conservative-map proc . lsts)
  (cond
   ((null? lsts)
    #unspecified)
   ((null? (cdr lsts))
    (conservative-map-1 proc (car lsts)))
   ((null? (cddr lsts))
    (conservative-map-2 proc (car lsts) (cadr lsts)))
   (else
    (error 'conservative-map "Not implemented yet." #unspecified))))

(define (conservative-filter proc lst)
  (let loop ((lst lst))
    (if (null? lst)
      lst
      (let* ((curr (car lst))
             (rest (cdr lst))
             (new-rest (loop rest)))
        (if (proc curr)
          (if (eq? rest new-rest)
            lst
            (cons curr new-rest))
          new-rest)))))

(define (conservative-map-filter proc lst)
  (let loop ((lst lst))
    (if (null? lst)
      lst
      (let* ((curr (car lst))
             (new-curr (proc curr))
             (rest (cdr lst))
             (new-rest (loop rest)))
        (cond
         ((not new-curr)
          new-rest)
         ((and (eq? curr new-curr)
               (eq? rest new-rest))
          lst)
         (else
          (cons new-curr new-rest)))))))

(define (list-copy lst)
  (let loop ((result '())
             (lst lst))
    (if (null? lst)
        (reverse! result)
        (loop (cons (car lst) result) (cdr lst)))))
         
(define (list-head lst k)
  (let loop ((result '())
             (i 0)
             (lst lst))
    (if (or (null? lst) (>= i k))
      (reverse! result)
      (loop (cons (car lst) result)
            (+ i 1)
            (cdr lst)))))

(define (list-last-element lst)
  (if (null? lst)
    #unspecified
    (let loop ((lst lst))
      (if (null? (cdr lst))
        (car lst)
        (loop (cdr lst))))))

;; struct->list conflicts with a library function  in bigloo2.8
(define (sal_struct->list struct)
  (let ((key (struct-key struct))
        (len (struct-length struct)))
    (let loop ((i 0))
      (if (>= i len)
        '()
        (cons (struct-ref struct i) (loop (+ i 1)))))))

(define *find-proc* (for-each->find for-each))
    
(define (find proc lst)
  (*find-proc* proc lst))

(define (replace old new lst)
  (let loop ((lst lst))
    [assert (lst) (list? lst)]
    (if (null? lst)
        lst
        (let ((elem (car lst))
              (tail (cdr lst)))
          (let ((new-tail (loop tail)))
            (cond 
             ((equal? old elem) (cons new new-tail))
             ((eq? new-tail tail) lst)
             (else (cons elem new-tail))))))))

(define (replace! old new lst)
  (let loop ((lst lst))
    [assert (lst) (list? lst)]
    (when (not (null? lst))
          (if (equal? old (car lst))
              (set-car! lst new))
          (loop (cdr lst))))
  lst)

(define (remove-if pred lst)
  (let loop ((lst lst))
    (cond 
     ((null? lst) '())
     ((pred (car lst)) (loop (cdr lst)))
     (else (cons (car lst) (loop (cdr lst)))))))

(define (delete-duplicates l)
  (let loop ((l l)
             (r '()))
    (if (pair? l)
      (let ((e (car l)))
        (if (memq e r)
          (loop (cdr l) r)
          (loop (cdr l) (cons e r))))
      (reverse! r))))

(define (generate-list proc num-elems)
  (let loop ((i (- num-elems 1))
             (result '()))
    (if (>= i 0)
      (loop (- i 1) (cons (proc i) result))
      result)))

;; returns the position of element "obj" in the list "alist"
;; returns #f is "obj" is not in the list.
(define (generic-member-pos alist obj eq?)
  (let loop ((pos 0)
             (alist alist))
    (cond 
     ((null? alist) #f)
     ((eq? obj (car alist)) pos)
     (else (loop (+ pos 1) (cdr alist))))))

;; split the list in two using the predicate pred?
(define (split-list lst pred?)
  (let loop ((lst lst)
             (valid-lst '())
             (invalid-lst '()))
    (if (null? lst)
      (values (reverse! valid-lst) (reverse! invalid-lst))
      (let ((curr (car lst)))
        (if (pred? curr)
          (loop (cdr lst) (cons curr valid-lst) invalid-lst)
          (loop (cdr lst) valid-lst (cons curr invalid-lst)))))))

(define (member-pos alist obj)
  (generic-member-pos alist obj equal?))

(define (memq-pos alist obj)
  (generic-member-pos alist obj eq?))

(define (map-with-pos proc list)
  (let ((len (length list)))
    (let loop ((i 0)
               (list list)
               (result '()))
      (if (null? list)
        (reverse! result)
        (loop (+ i 1) (cdr list) (cons (proc (car list) i) result))))))

;; similar to assoc, but you can specify your notion of equality
(define (associated elem mapping is-equal?)
  (let loop ((mapping mapping))
    (cond ((null? mapping) #f)
          ((is-equal? (caar mapping) elem) (car mapping))
          (else (loop (cdr mapping))))))
  
(define (inv-associated elem mapping is-equal?)
  (let loop ((mapping mapping))
    (cond ((null? mapping) #f)
          ((is-equal? (cdar mapping) elem) (car mapping))
          (else (loop (cdr mapping))))))

(define (inv-assoc elem mapping)
  (inv-associated elem mapping equal?))

(define (inv-assq elem mapping)
  (inv-associated elem mapping eq?))

(define (adjoin obj lst) 
  (if (memq obj lst) 
    lst 
    (cons obj lst)))

(define (union lst1 lst2)
  (cond 
   ((null? lst2) lst1)
   ((null? lst1) lst2)
   (else 
    (let ((ans lst1))
      (for-each (lambda (elt) 
                  (set! ans (adjoin elt ans)))
                lst2)
        ans))))

(define (intersection lst1 lst2)
  (if (null? lst2)
    lst2
    (let build-intersection ((lst1 lst1)
                             (result '()))
      (cond ((null? lst1) 
             result)
            ((memq (car lst1) lst2)
             (build-intersection (cdr lst1) (cons (car lst1) result)))
            (else
             (build-intersection (cdr lst1) result))))))

(define (difference lst1 lst2)
  (let loop ((lst2 lst2)
             (result lst1))
    (cond ((null? lst2)
           result)
          ((memq (car lst2) result)
           (loop (cdr lst2) (delete (car lst2) result)))
          (else
           (loop (cdr lst2) result)))))
            
(define (subset? lst1 lst2)
  (for-all (lambda (e1) (exists (lambda (e2) (eq? e1 e2)) lst2)) lst1))

(define (set-eq? lst1 lst2)
  (and (subset? lst1 lst2)
       (subset? lst2 lst1)))

(define (product proc alist1 alist2)
  (let ((result '()))
    (for-each (lambda (e1) (for-each (lambda (e2) (push! (proc e1 e2) result)) alist2)) alist1)
    result))

(define (invert-mapping mapping)
  (map (lambda (pair) (cons (cdr pair) (car pair))) mapping))

;;
;; The following function should be used to convert string to symbols, in
;; a portable way.
;;
;; In guile (and possibly other "schemes"), this function should be implemented
;; as
;; 
;; (define (portable-string->symbol str)
;;  (string->symbol (string-downcase str)))
;;
;; (define (portable-string->symbol str)
;;  (string->symbol-ci str))
;; 
;; Biglo is now case sensitive
(define (portable-string->symbol str)
  (string->symbol (string-downcase str)))

(define (to-symbol symbol-or-string)
  (if (symbol? symbol-or-string)
      symbol-or-string
      (string->symbol symbol-or-string)))

(define (to-string symbol-or-string)
  (if (string? symbol-or-string)
    symbol-or-string
    (symbol->string symbol-or-string)))

(define (string-or-symbol? obj)
  (or (string? obj) (symbol? obj)))

(define (symbol-length symb)
  (string-length (symbol->string symb)))

(define (object->string obj)
  (with-output-to-string
    (lambda () (display obj))))

(define (object->symbol obj)
  (string->symbol (object->string obj)))

;;; Return the index of the first occurrence of a-char in str, or #f
(define (sal-string-index str a-char . start-pos)
  (let loop ((pos (if (null? start-pos) 0 (car start-pos))))
    (cond
     ;; whole string has been searched, in vain
     ((>= pos (string-length str)) #f)
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

(define (sal-string-index-ci str a-char . start-pos)
  (let loop ((pos (if (null? start-pos) 0 (car start-pos))))
    (cond
     ;; whole string has been searched, in vain
     ((>= pos (string-length str)) #f)
     ((char-ci=? a-char (string-ref str pos)) pos)
     (else (loop (+ 1 pos))))))

;;; Return the index of the last occurrence of a-char in str, or #f
(define (string-last-index str a-char . end-pos)
  (let loop ((pos (if (null? end-pos) (- (string-length str) 1) (car end-pos))))
    (cond
     ((< pos 0) #f)
     ((char=? a-char (string-ref str pos)) pos)
     (else (loop (- pos 1))))))

(define (string-last-index-ci str a-char . end-pos)
  (let loop ((pos (if (null? end-pos) (- (string-length str) 1) (car end-pos))))
    (cond
     ((< pos 0) #f)
     ((char-ci=? a-char (string-ref str pos)) pos)
     (else (loop (- pos 1))))))

(define (char-pos achar astring)
  (let ((length (string-length astring)))
    (let loop ((pos 0))
      (if (< pos length)
        (if (eq? (string-ref astring pos) achar)
          pos
          (loop (+ pos 1)))
        #f))))

(define-inline (space? char) 
  (memq char '(#\space #\newline #\tab #a012 #a013)))

(define (trim astring)
  (if (equal? astring "")
    astring
    (let* ((length (string-length astring))
           (first-non-blank 
            (let loop ((pos 0))
              (if (< pos length)
                (if (not (space? (string-ref astring pos)))
                  pos
                  (loop (+ pos 1)))
                pos)))
           (astring1 (substring astring first-non-blank length))
           (length1 (string-length astring1))
           (last-non-blank
            (let loop ((pos 0)
                       (last 0))
              (if (< pos length1)
                (if (not (space? (string-ref astring1 pos)))
                  (loop (+ pos 1) pos)
                  (loop (+ pos 1) last))
                last))))
      (substring astring1 0 (+ last-non-blank 1)))))

(define (delimited-string->list delimiters a-string)
  (let ((len (string-length a-string)))
    (let loop ((i 0)
               (result '()))
      (if (< i len)
        (let* ((c (string-ref a-string i))
               (car-res (if (null? result) #f (car result)))
               (new-result (if (memq c delimiters)
                             (if (equal? car-res "")
                               result
                               (cons "" result))
                             (if car-res
                               (begin
                                 (set-car! result (string-append car-res (string c)))
                                 result)
                               (cons (string c) result)))))
          (loop (+ i 1) new-result))
        (cond
         ((null? result)
          '())
         ((equal? (car result) "")
          (reverse! (cdr result)))
         (else
          (reverse! result)))))))
  
(define (load-config-file file-name)
  (let ((home (getenv "HOME")))
    (when home
      (let ((salrc (string-append home (make-string 1 (file-separator)) file-name)))
        (try
         (when (file-exists? salrc)
           (loadq salrc))
         (lambda (escape proc msg obj)
           (with-output-to-port (current-error-port)
             (lambda ()
               (print "Error loading configuration script: " salrc)
               (print "Please fix (or erase) the configuration file.")
               (exit -1)))))))))

;;; Vector functions
(define (vector/for-each proc vector)
  (let ((len (vector-length vector)))
    (let loop ((pos 0))
      (when (< pos len)
        (proc (vector-ref vector pos))
        (loop (+ pos 1))))))

(define *vector-fold* (for-each->fold vector/for-each))

(define (vector/fold proc initial vector)
  (*vector-fold* proc initial vector))

(define *vector-find* (for-each->find vector/for-each))

(define (vector/find proc vector)
  (*vector-find* proc vector))

(define-inline (vector/swap-elements! vect i j)
  (let ((aux (vector-ref vect i)))
    (vector-set! vect i (vector-ref vect j))
    (vector-set! vect j aux)))

(define (vector/fill-from-to! vect from to value)
  (let loop ((i from))
    (when (< i (min to (vector-length vect)))
      (vector-set! vect i value)
      (loop (+ i 1)))))

(define (obj->boolean obj)
  (if obj
    #t
    #f))

;;
;; Temporary files
;;
(define (gen-temp-file prefix)
  (let* ((proc (run-process "mktemp" (string-append prefix ".XXXXXXXX") output: pipe:))
         (port (process-output-port proc)))
    (read-line port)))

(define (add-temp-dir-prefix file-name)
  (let ((tmpdir (getenv "TMPDIR")))
    (unless tmpdir
      (set! tmpdir "/tmp"))
    (string-append tmpdir "/" file-name)))
    
(define (closest-greater-power-of-2 num)
  (let loop ((value 1)
             (result 0))
    (if (< num value)
      (values value result)
      (loop (* 2 value) (+ result 1)))))

;;
;; Defines an order on symbols
;;
(define *symbol-order-next-pos* 0)

(define (symbol-get-pos! s)
  [assert (s) (symbol? s)]
  (let ((pos (getprop s 'sym-pos)))
    (unless pos
      (set! pos *symbol-order-next-pos*)
      (set! *symbol-order-next-pos* (+ *symbol-order-next-pos* 1))
      (putprop! s 'sym-pos pos))
    pos))

(define (symbol<? s1 s2)
  [assert (s1 s2) (and (symbol? s1) (symbol? s2))]
  (string<? (symbol->string s1) (symbol->string s2))) ;; <-- making symbol<? more predictable
;;  (< (symbol-get-pos! s1) (symbol-get-pos! s2)))

(define (symbol<=? s1 s2)
  (or (eq? s1 s2) (symbol<? s1 s2)))

(define (symbol>? s1 s2)
  (not (symbol<=? s1 s2)))

(define (symbol>=? s1 s2)
  (not (symbol<? s1 s2)))

;;
;; Error msgs
;;

(define (register-app-error! kind)
  [assert (kind) (symbol? kind)]
  (putprop! kind 'app-error #t))

(define (app-error? kind)
  (and (symbol? kind)
       (getprop kind 'app-error)))

(define (sign-error msg . args)
  (error 'sign-error (apply xformat #f msg args) #unspecified))

(define (sign-usage-error proc msg . args)
  (error proc (apply xformat #f msg args) #unspecified))

(define (fail)
  (sign-error "failed"))

;;
;; New error API starting from bigloo2.7:
;; notify-error not defined anymore
;; error-notify needs an error object
;;
;; BD: removed support for old versions of bigloo (2007/12/13)
;;
;;
(define (internal-error)
  (when (sal-check-mode) 
	(error-notify (mk-error 'internal-error 'internal-error 'internal-error)))
  (sign-error "BUG: Internal error. Contact support."))
;
; (define (internal-error)
;  (when (sal-check-mode)
;	(cond-expand
;	 (bigloo2.7
;	  (error-notify (make-&error #f #f 'internal-error 
;				     'internal-error 'internal-error)))	 
;	 (bigloo2.8
;	  (error-notify (make-&error #f #f 'internal-error 
;				     'internal-error 'internal-error)))
;	 (bigloo2.9
;	  (error-notify (make-&error #f #f 'internal-error 
;				     'internal-error 'internal-error)))
;	 (else
;	  (notify-error 'internal-error 'internal-error 'internal-error))))
;  (sign-error "BUG: Internal error. Contact support."))
;

(define (unreachable-code)
  (sign-error "BUG: \"unreachable\" code was reached! Please contact support."))

(define (warning-message msg . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print (apply xformat (cons* #f (string-append "WARNING: " msg) args))))))

(register-app-error! 'sign-error)

;;
;; Verbose messages
;;
(define *verbosity-level* 0)

(define (set-verbosity! new-lv)
  (set! *verbosity-level* new-lv))

(define (verbosity-level)
  *verbosity-level*)

(define (verbose-message-core msg . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (display (apply xformat (cons* #f msg args)))
      (newline)
      (flush-output-port (current-error-port))))
  #unspecified)

;;
;; Status messages
;;
;; Remark: machine processable messages

(define *status-messages?* #f)

(define (status-messages-enabled?)
  *status-messages?*)

(define (status-message-core . args)
  (with-output-to-port (current-error-port)
    (lambda ()
      (let ((key? #f))
        (for-each (lambda (arg)
                    (cond
                     ((or (keyword? arg)
                          (symbol? arg)
                          (number? arg))
                      (display arg))
                     ((string? arg)
                      (display* "'" arg "'"))
                     (else
                      (sign-error "Invalid status information: ~a" arg)))
                    (display " "))
                  args)
        (print ""))))
  ;; the following line was included to solve a problem when compiling SAL with mingw
  (pragma "fflush(stderr);")
  #unspecified)

(define (enable-status-messages! flag)
  (set! *status-messages?* flag))

;;
;; breakpoint
;;
(define *global-def-aux* #unspecified)

(define (dynamic-define name value)
  (set! *global-def-aux* value)
  (with-error-to-string
    (lambda ()
      (eval `(define ,name *global-def-aux*)))))

;;
;; Dump stack
;;
(define *util-trace-stack-depth* 20)
(define (dump-exec-stack)
  (dump-trace-stack (current-output-port) *util-trace-stack-depth*))

;;
;; svsv
;;
(define (svsv/value svsv slot-id default-value)
  (let loop ((svsv svsv))
    (if (null? svsv)
      default-value
      (if (eq? (car svsv) slot-id)
        (cadr svsv)
        (loop (cddr svsv))))))

(define (svsv/for-each proc svsv)
  (let loop ((svsv svsv))
    (unless (null? svsv)
      (proc (car svsv) (cadr svsv))
      (loop (cddr svsv)))))

;;
;; indentation
;; Poor's man pretty printing 
(define (indent n)
  (display (make-string n)))

(define (display-indented n . args)
  (let loop ((args args))
    (unless (null? args)
      (let ((curr-arg (car args)))
        (cond
         ((string? curr-arg)
          (let ((len (string-length curr-arg)))
            (let inner-loop ((i 0))
              (when (< i len)
                (let ((c (string-ref curr-arg i)))
                  (display c)
                  (when (eq? c #\newline)
                    (indent n))
                  (inner-loop (+ i 1)))))))
         (else 
          (display curr-arg)))
        (loop (cdr args))))))

(define (print-indented n . args)
  (apply display-indented n args)
  (print ""))

(define *boxed-grammar*
  (regular-grammar ()
   ((in #\space #\tab)
    'space)
   ((in #\newline #a012 #a013)
    'line)
   ((+ (out #\space #\tab #\newline #a012 #a013))
    (cons 'data (the-string)))
   (else
    (if (eof-object? (the-failure))
      'eof
      (internal-error)))))

(define (display-boxed begin-column end-column . args)
  (let loop ((args args)
             (pos begin-column))
    (unless (null? args)
      (let* ((curr-arg (car args))
             (curr-str (object->string curr-arg))
             (str-port (open-input-string curr-str))
             (read-next-data (lambda () (read/rp *boxed-grammar* str-port)))
             (curr-pos pos)
             (display-nl (lambda ()
                           (display "\n")
                           (indent begin-column)
                           (set! curr-pos begin-column)))
             (display-data (lambda (str)
                             (let ((len (string-length str)))
                               (cond
                                ((< (+ len curr-pos) end-column)
                                 (set! curr-pos (+ len curr-pos))
                                 (display str))
                                (else
                                 (display-nl)
                                 (unless (equal? str " ")
                                   (set! curr-pos (+ len curr-pos))
                                   (display str))))))))
                                   
        (let inner-loop ()
          (let ((data (read-next-data)))
            ;; (breakpoint "aaa" (data) #t)
            (cond
             ((eq? data 'eof)
              ;; do nothing... end of string
              #unspecified)
             ((eq? data 'space)
              (display-data " ")
              (inner-loop))
             ((eq? data 'line)
              (display-nl)
              (inner-loop))
             ((and (pair? data) (eq? (car data) 'data))
              (display-data (cdr data))
              (inner-loop))
             (else 
              (internal-error)))))
        (loop (cdr args) curr-pos)))))

(define (print-boxed begin-column end-column . args)
  (apply display-boxed begin-column end-column args)
  (print ""))

(define (print-file file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((line (read-line (current-input-port))))
        (if (not (eof-object? line))
          (begin
            (print line)
            (loop (read-line (current-input-port)))))))))

;;
;; Grep interface
;;
(define (grep/contains? file str)
  (= 0 (system (string-append "grep -i -n -e \"" str "\" \"" file "\" > /dev/null 2> /dev/null"))))

