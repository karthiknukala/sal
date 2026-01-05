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

(module scmobj
        (import collect-info)
        (include "utility.macros")
        (include "scmobj.macros")
        (include "fast-hash-table.sch")
        (import utility xformat)
        (export *sal-class-info-file*
                *scm:instance-tag*
                (inline instance? obj)
                <standard-class>
                (inline class-idx class)
                (inline class-name-of class)
                (inline class-precedence-list class)
                (inline class-slots class)
                (inline class-slot-idxs class)
                (inline class-instance-size class)
                <primitive> <procedure> <integer> <natural> <string> <symbol> <list> <queue>
                (inline class-of obj)
                (inline instance-class-name instance)
                (inline instance-class-idx instance)
                (inline slot-name->slot-idx instance slot-name)
                (slot-value instance slot-name)
                (set-slot-value! instance slot-name new-value)
                (inline make-instance-core class)
                (make-instance++ class . svsv)
                (scm:make-class class-name super-classes defined-slots)
                (inline shallow-copy instance)
                (subclass? c1 c2)
                (inline instance-of? instance class)                
                (inline quick-change-class! instance class)
                (change-class instance class)
                (scm:update-method-table method-table specializer-classes method)
                (scm:register-generic-procedure! name add-primary-proc! add-around-proc!)
                (scm:add-method! name kind class-list method-proc)
                (scm:compute-all-applicable-methods classes around-method-table primary-method-table)
                (make-multi-dispatch-table num-discriminators)
                (multi-dispatch-table/reset! table)
                (multi-dispatch-table/applicable-methods-in-cache table class-idxs)
                (multi-dispatch-table/applicable-methods table classes class-idxs around-method-table primary-method-table)
                (sign-no-method-defined name)))


;; -------------------------------------------------------------
;; Instance representation:
;;
;; Vector of size num-slots + 2
;; pos 0: unique tag
;; pos 1: class ref
;; pos 2-n: slot values
;;
;; -------------------------------------------------------------

;;
;; BD: sal-collect-info is a compilation option
;; first phase: compilation with (define-macro (sal-collect-info) #t)
;; second pahase: compilation with (define-macro (sal-collect-info) #f)
;;
;; (collect-class-info?) is defined in collect-info.scm
;; it's true if environment variable SAL_COLLECT_CLASS_INFO is set
;;
;; When collect info is enabled, then data about classes and methods,
;; is stored in file 'sal-class-info.data'
;;

(define *sal-class-info-file* (compile-if (sal-collect-info)
                                 (when (collect-class-info?)          
                                   (open-output-file  "sal-class-info.data"))
                                 #unspecified))

(compile-if (sal-collect-info)
  (register-exit-function! 
   (lambda (status) 
     (when (collect-class-info?)          
       (close-output-port *sal-class-info-file*))
     status)))

(define *next-class-idx* 2) ;; save space for <standard-class> and <primitive>

(define (next-class-idx)
  (let ((result *next-class-idx*))
    (set! *next-class-idx* (+ *next-class-idx* 1))
    result))

;; -------------------------------------------------------------
;; Define instance indentification tag
;;
;; -------------------------------------------------------------
(define *scm:instance-tag* (gensym 'instance))

(define-inline (instance? obj)
  (and (vector? obj) (eq? (vector-ref obj 0) *scm:instance-tag*)))

;; -------------------------------------------------------------
;; Create Class of Classes
;;
;; -------------------------------------------------------------
(define <standard-class>
  (vector *scm:instance-tag*
          #unspecified
          0
          '<standard-class>
          '()
          '(:class-idx :class-name :class-precedence-list :slots :slot-idxs :instance-size)
          '((:class-idx . 2) (:class-name . 3) (:class-precedence-list . 4) (:slots . 5) (:slot-idxs . 6) (:instance-size . 7))
          8))

(vector-set! <standard-class> 1 <standard-class>)

(define-inline (class-idx class)
  (vector-ref class 2))
(define-inline (class-name-of class)
  (vector-ref class 3))
(define-inline (class-precedence-list class)
  (vector-ref class 4))
(define-inline (class-slots class)
  (vector-ref class 5))
(define-inline (class-slot-idxs class)
  (vector-ref class 6))
(define-inline (class-instance-size class)
  (vector-ref class 7))

;; -------------------------------------------------------------
;; Primitive class
;;
;; -------------------------------------------------------------
(define <primitive>
  (vector *scm:instance-tag*
          <standard-class>
          1
          '<primitive>
          '()
          '()
          '()
          0))

;; some aliases for the <primitive> class
(define <procedure> <primitive>)
(define <integer> <primitive>)
(define <natural> <primitive>)
(define <string> <primitive>)
(define <symbol> <primitive>)
(define <keyword> <primitive>)
(define <list> <primitive>)
(define <queue> <primitive>)

;; -------------------------------------------------------------
;; auxiliary functions
;;
;; -------------------------------------------------------------
(define-inline (class-of obj)
  (if (instance? obj)
    (vector-ref obj 1)
    <primitive>))

(define-inline (instance-class-name instance)
  (class-name-of (class-of instance)))

(define-inline (instance-class-idx instance)
  (class-idx (class-of instance)))

(define-inline (slot-name->slot-idx instance slot-name)
  (let ((entry (assq slot-name (class-slot-idxs (class-of instance)))))
    (and entry
         (cdr entry))))

;; -------------------------------------------------------------
;; Inneficient version of slot-value and slot-value-core.
;; It is used when the scmobj optimizations (close-world)
;; are not used.
;; -------------------------------------------------------------
(define (slot-value instance slot-name)
  [assert (instance) (instance? instance)]
  (let ((slot-idx (slot-name->slot-idx instance slot-name)))
    (cond
     (slot-idx
      (vector-ref instance slot-idx))
     (else
      (sal-assert "slot-value: slot not found" (instance slot-name) #f)
      (error 'slot-value (xformat #f "slot ~a not found in ~a" slot-name (instance-class-name instance)) #unspecified)))))

(define (set-slot-value! instance slot-name new-value)
  [assert (instance) (instance? instance)]
  (let ((slot-idx (slot-name->slot-idx instance slot-name)))
    (cond
     (slot-idx
      (vector-set! instance slot-idx new-value))
     (else
      (sal-assert "set-slot-value!: slot not found" (instance slot-name new-value) #f)
      (error 'set-slot-value! "trying to set nonexistent slot" slot-name)))))

;; -------------------------------------------------------------
;; make-instance support
;;
;; -------------------------------------------------------------
(define-inline (make-instance-core class)
  (let ((result (make-vector (class-instance-size class) #f)))
    (vector-set! result 0 *scm:instance-tag*)
    (vector-set! result 1 class)
    result))

(define (make-instance++ class . svsv)
  (let ((result (make-instance-core class)))
    (svsv/for-each (lambda (slot-name value)
                     (set-slot-value! result slot-name value))
                   svsv)
    result))

;; -------------------------------------------------------------
;; Class creation
;;
;; -------------------------------------------------------------
(define (compute-class-precedence-list super-classes)
  (delete-duplicates
   (fold-left (lambda (result super-class)
                (append result (cons super-class (class-precedence-list super-class))))
              '()
              super-classes)))

(define (compute-class-slot-info super-classes defined-slots)
  (delete-duplicates
   (append (fold-left (lambda (result super-class)
                        (append result (class-slots super-class)))
                      '()
                      super-classes)
           defined-slots)))

(define (scm:make-class class-name super-classes defined-slots)
  (let* ((class-idx (next-class-idx))
         (slots (compute-class-slot-info super-classes defined-slots))
         (precedence-list (compute-class-precedence-list super-classes))
         (slot-idx 2)
         (result (vector *scm:instance-tag*
                         <standard-class>
                         class-idx
                         class-name
                         precedence-list
                         slots
                         (map (lambda (slot-name)
                                (let ((pair (cons slot-name slot-idx)))
                                  (set! slot-idx (+ slot-idx 1))
                                  pair))
                              slots)
                         (+fx (length slots) 2))))
    (compile-if (sal-collect-info)
      (when (collect-class-info?)           
        (with-output-to-port *sal-class-info-file*
          (lambda ()
            (print "(class " class-name  " " (map class-name-of precedence-list) " " slots ")")))))
    result))

;; -------------------------------------------------------------
;; Copy operation
;;
;; -------------------------------------------------------------
(define-inline (shallow-copy instance)
  [assert (instance) (instance? instance)]
  (copy-vector instance (vector-length instance)))


;; -------------------------------------------------------------
;; Generic procedure support
;;
;; -------------------------------------------------------------
(define (scm:more-specific-method m1 m2 cc)
  (let loop ((cc1 (car m1)) 
             (cc2 (car m2))
             (cc cc))
    (if (null? cc)
      (error 'scm:more-specific-method "scm:more-specific-method" #unspecified)
      (let ((c1 (car cc1))
            (c2 (car cc2)))
        (cond ((eq? c1 c2)
               (loop (cdr cc1) (cdr cc2) (cdr cc)))
              ((subclass? c1 c2) #t)
              ((subclass? c2 c1) #f)
              (else
               (let* ((c (car cc))
                      (cpl (if (eq? c <primitive>)
                             '()
                             (class-precedence-list c)))
                      (i1 (memq-pos cpl c1))
                      (i2 (memq-pos cpl c2)))
                 (if (and i1 i2)
                   (< i1 i2)
                   (error 'scm:more-specific-method "scm:more-specific-method" #unspecified)))))))))

(define (scm:compute-applicable-methods list-of-classes method-table)
  (let loop ((methods method-table)
             (the-applicable-methods '()))
    (if (null? methods)
      (map cdr
           (sort the-applicable-methods
                 (lambda (m1 m2)
                   (scm:more-specific-method m1 m2 list-of-classes))))
      (loop (cdr methods)
            (let ((method (car methods)))
              (if (for-all subclass? list-of-classes (car method))
                (cons method the-applicable-methods)
                the-applicable-methods))))))

(define (scm:compute-all-applicable-methods classes around-method-table primary-method-table)
  (let* ((applicable-around-methods (scm:compute-applicable-methods classes around-method-table))
         (applicable-primary-methods (scm:compute-applicable-methods classes primary-method-table))
         (applicable-methods (append applicable-around-methods applicable-primary-methods)))
    applicable-methods))

;; -------------------------------------------------------------
;; Subclass? and instance-of?
;;
;; -------------------------------------------------------------
(define (subclass? c1 c2)
  (cond 
   ((eq? c1 c2) #t)
   ((eq? c1 <primitive>) #f)
   ((eq? c2 <primitive>) #t)
   ((memq c2 (class-precedence-list c1)) 
    ;; (print "precedence-list size: " (length (class-precedence-list c1)))
    ;; (print "memq size: " (length (memq c2 (class-precedence-list c1))))
    #t)
   (else #f)))

(define-inline (instance-of? instance class)
  (subclass? (class-of instance) class))

;; -------------------------------------------------------------
;; Instance metamorphosis
;;
;; -------------------------------------------------------------
(define-inline (quick-change-class! instance class)
  [assert (instance class) (= (vector-length instance) (class-instance-size class))]
  [assert (instance class) (for-all eq? (class-slots (class-of instance)) (class-slots class))]
  (vector-set! instance 1 class)
  instance)

(define (change-class instance class)
  (let ((r (make-instance-core class)))
    (let loop ((idx 2) ;; first two positions are reserved: tag and class reference
               (slot-names (class-slots class)))
      (unless (null? slot-names)
        (let ((source-slot-idx (slot-name->slot-idx instance (car slot-names))))
          (when source-slot-idx
            (vector-set! r idx (vector-ref instance source-slot-idx))))
        (loop (+ idx 1)
              (cdr slot-names))))
    r))

;; -------------------------------------------------------------
;; Generic procedure, method-table manipulation functions.
;;
;; A method table is a list of pairs. 
;; Each pair is composed of:
;;   1- a list of classes
;;   2- a procedure
;;
;; -------------------------------------------------------------
(define (scm:update-method-table method-table specializer-classes method)
  (if (exists (lambda (c)
                (and (for-all eq? specializer-classes (car c))
                     (begin
                       (set-cdr! c method)
                       #t)))
              method-table)
    method-table
    (cons (cons specializer-classes method) method-table)))

;; -------------------------------------------------------------
;; Generic procedure registration
;;
;; -------------------------------------------------------------
(define *scm-generic-procedures* (make-eq-hash-table))

(define  (scm:register-generic-procedure! name add-primary-proc! add-around-proc!)
  (eq-hash-table/put! *scm-generic-procedures* name (cons add-around-proc! add-primary-proc!)))

(define (scm:add-method! name kind class-list method-proc)
  (cond
   ((eq-hash-table/get *scm-generic-procedures* name) =>
    (lambda (entry)
      (let* ((pair (cdr entry))
             (add-around-proc! (car pair))
             (add-primary-proc! (cdr pair)))
        (case kind
          ((:around)
           (add-around-proc! class-list method-proc))
          ((:primary)
           (add-primary-proc! class-list method-proc))
          (else
           (error 'scm:add-method! "Unknown method kind" kind))))))
   (else
    (error 'scm:add-method! "Unknown generic procedure" name))))

;; -------------------------------------------------------------
;; Method cache tables
;;
;; -------------------------------------------------------------
(define-inline (multi-dispatch-table-key/hash class-idx-vect)
  (if (>fx (vector-length class-idx-vect) 1)
    (+fx (vector-ref class-idx-vect 0) (*fx (vector-ref class-idx-vect 1) 3))
    (vector-ref class-idx-vect 0)))

(define *multi-dispatch-table-initial-size* 64)
(define *multi-dispatch-table-load-factor* 40)

(define (make-multi-dispatch-table num-discriminators)
  (cons 0 (make-vector *multi-dispatch-table-initial-size* #f)))

(define (multi-dispatch-table/reset! table)
  (unless (=fx (car table) 0)
    (set-car! table 0)
    (set-cdr! table (make-vector *multi-dispatch-table-initial-size* #f))))

(define (multi-dispatch-table/applicable-methods-in-cache table class-idxs)
  (let* ((vect (cdr table))
         (n (vector-length vect))
         (j (remainder (multi-dispatch-table-key/hash class-idxs) n)))
    (let loop ((j j))
      (let ((curr (vector-ref vect j)))
        (cond
         ((not curr) #f)
         ((equal? (car curr) class-idxs)
          (cdr curr))
         (else (loop (remainder (+fx j 1) n))))))))

(define (insert-core! vect class-idxs applicable-methods)
  (let* ((n (vector-length vect))
         (j (remainder (multi-dispatch-table-key/hash class-idxs) n)))
    (let loop ((j j))
      (let ((curr (vector-ref vect j)))
        (cond
         ((not curr) 
          (vector-set! vect j (cons class-idxs applicable-methods)))
         (else (loop (remainder (+fx j 1) n))))))))

(define (multi-dispatch-table/expand-if-needed table)
  (let* ((vect (cdr table))
         (capacity (vector-length vect))
         (size (car table)))
    (when (> (*fx size 100) (*fx capacity *multi-dispatch-table-load-factor*))
      (let ((new-vect (make-vector (*fx capacity 2) #f)))
        (let loop ((i 0))
          (when (< i capacity)
            (let ((curr (vector-ref vect i)))
              (when curr
                (insert-core! new-vect (car curr) (cdr curr))))
            (loop (+fx i 1))))
        (set-cdr! table new-vect)))))

(define (multi-dispatch-table/insert! table class-idxs applicable-methods)
  (multi-dispatch-table/expand-if-needed table)
  (insert-core! (cdr table) class-idxs applicable-methods)
  (set-car! table (+fx (car table) 1)))

(define (multi-dispatch-table/applicable-methods table classes class-idxs around-method-table primary-method-table)
  (let* ((applicable-methods (scm:compute-all-applicable-methods classes around-method-table primary-method-table))
         (class-idxs (copy-vector class-idxs (vector-length class-idxs))))
    (multi-dispatch-table/insert! table class-idxs applicable-methods)
    applicable-methods))

;; -------------------------------------------------------------
;; Error signals
;;
;; -------------------------------------------------------------
(define (sign-no-method-defined name)
  (error 'define-generic (string-append "no method defined for " (symbol->string name)) #unspecified))


  
