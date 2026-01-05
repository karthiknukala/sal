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

(module code-analyzer
        (main main))

(define-record-type class-info
  (mk-class-info name idx slots precedence-list subclasses fast-slot-access?)
  class-info?
  (name class-info/name)
  (idx class-info/idx)
  (slots class-info/slots)
  (precedence-list class-info/precedence-list)
  (subclasses class-info/subclasses class-info/set-subclasses!)
  (fast-slot-access? class-info/fast-slot-access? class-info/set-fast-slot-access!))

(define-record-type generic-info
  (mk-generic-info name idx arg-type-info num-discriminators discriminator-pos-list)
  generic-info?
  (name generic-info/name)
  (idx generic-info/idx)
  (arg-type-info generic-info/arg-type-info)
  (num-discriminators generic-info/num-discriminators generic-info/set-num-discriminators!)
  (discriminator-pos-list generic-info/discriminator-pos-list generic-info/set-discriminator-pos-list!))
  
(define *next-class-idx* 1)
(define *next-generic-idx* 0)
(define *class-table* (make-hashtable))
(define *generic-table* (make-hashtable))
(define *classes* '())
(define *generics* '())

(define (name->class-info name)
  (cond
   ((hashtable-get *class-table* name) =>
    (lambda (info) info))
   (else
    (error 'register-subclass! "Unknown class" name))))

(define (register-subclass! class-info subclass-info)
  (class-info/set-subclasses! class-info (cons subclass-info (class-info/subclasses class-info))))

(define (add-class! name slots precedence-list)
  (let* ((precedence-list (map name->class-info precedence-list))
         (info (mk-class-info name *next-class-idx* slots precedence-list '() 'undef)))
    (set! *next-class-idx* (+ *next-class-idx* 1))
    (for-each (lambda (superclass)
                (register-subclass! superclass info))
              precedence-list)
    (set! *classes* (cons info *classes*))
    (hashtable-put! *class-table* name info)))

(define (name->generic-info name num-args)
  (cond
   ((hashtable-get *generic-table* name) =>
    (lambda (info) 
      (unless (= (vector-length (generic-info/arg-type-info info)) num-args)
        (error 'name->generic-info "Invalid method arity" name))
      info))
   (else
    (let ((info (mk-generic-info name *next-generic-idx* (make-vector num-args '()) 0 '())))
      (set! *next-generic-idx* (+ *next-generic-idx* 1))
      (set! *generics* (cons info *generics*))
      (hashtable-put! *generic-table* name info)
      info))))

(define (append-types! arg-type-info types num-args)
  [assert (num-args arg-type-info) 
          (and (= (vector-length arg-type-info) num-args)
               (= (length types) num-args))]
  (let loop ((i 0)
             (types types))
    (when (< i num-args)
      (let ((type-list (vector-ref arg-type-info i))
            (curr-type (car types)))
        (unless (memq curr-type type-list)
          (vector-set! arg-type-info i (cons curr-type type-list)))
        (loop (+ i 1)
              (cdr types))))))

(define (add-method! name args)
  (let* ((num-args (length args))
         (types (map cadr args))
         (info (name->generic-info name num-args))
         (arg-type-info (generic-info/arg-type-info info)))
    (append-types! arg-type-info types num-args)))

(define (eq-prefix? l1 l2 n)
  (let loop ((l1 l1)
             (l2 l2)
             (i 0))
    (if (< i n)
      (if (eq? (car l1) (car l2))
        (loop (cdr l1) (cdr l2) (+ i 1))
        #f)
      #t)))

(define (set-fast-slot-access! info)
  (when (eq? (class-info/fast-slot-access? info) 'undef)
    (let* ((support? #t)
           (slots (class-info/slots info))
           (n (length slots)))
      (for-each (lambda (subclass)
                  (unless (eq-prefix? slots (class-info/slots subclass) n)
                    (print "WARNING: Fast slot access is not supported by: " (class-info/name info))
                    (print "REASON: the following subclass has a different slot prefix: " (class-info/name subclass))
                    (print "class slots: " slots)
                    (print "subclass slots: " (class-info/slots subclass))
                    (print "-----------------------------")
                    (set! support? #f)))
                (class-info/subclasses info))
      (class-info/set-fast-slot-access! info support?)))
  (class-info/fast-slot-access? info))

(define *num-classes-fast-slot-access* 0)
    
(define (fast-slot-access-optimization!)
  (for-each (lambda (class)
              (when (set-fast-slot-access! class)
                (set! *num-classes-fast-slot-access* (+ *num-classes-fast-slot-access* 1))))
            *classes*))

(define *num-zero-dispatch* 0)
(define *num-single-dispatch* 0)
(define *num-double-dispatch* 0)
(define *num-multi-dispatch* 0)

(define (set-num-discriminators! gen-info)
  (let* ((arg-type-info (generic-info/arg-type-info gen-info))
         (n (vector-length arg-type-info))
         (discriminator-pos-list '())
         (num-discriminators 0))
    (let loop ((i 0))
      (when (< i n)
        (let ((type-list (vector-ref arg-type-info i)))
          [assert (type-list) (not (null? type-list))]
          (when (> (length type-list) 1)
            (set! num-discriminators (+ num-discriminators 1))
            (set! discriminator-pos-list (cons i discriminator-pos-list))))
        (loop (+ i 1))))
    (cond
     ((= num-discriminators 0)
      (set! *num-zero-dispatch* (+ *num-zero-dispatch* 1)))
     ((= num-discriminators 1)
      (set! *num-single-dispatch* (+ *num-single-dispatch* 1)))
     ((= num-discriminators 2)
      (set! *num-double-dispatch* (+ *num-double-dispatch* 1)))
     (else
      (set! *num-multi-dispatch* (+ *num-multi-dispatch* 1))))
    (generic-info/set-num-discriminators! gen-info num-discriminators)
    (generic-info/set-discriminator-pos-list! gen-info (reverse! discriminator-pos-list))))

(define (dispatch-optimization!)
  (for-each set-num-discriminators! *generics*))

(define (show-classes)
  (for-each (lambda (info)
              (print "CLASS: " (class-info/name info))
              (print "FAST SLOT ACCESS: " (class-info/fast-slot-access? info))
              (print "IDX: " (class-info/idx info))
              (print "SLOTS: " (class-info/slots info))
              (print "SUBCLASSES: " (map class-info/name (class-info/subclasses info)))
              (print "---------------------------"))
            *classes*))

(define (show-generics)
  (for-each (lambda (info)
              (print "GENERIC: " (generic-info/name info))
              (print "IDX: " (generic-info/idx info))
              (print "NUM DISCRIMINATORS: " (generic-info/num-discriminators info))
              (print "DISCRIMINATORS POSITIONS: " (generic-info/discriminator-pos-list info))
              (print "----------------------------"))
            *generics*))

(define (show-database)
  (print "")
  (print "SUMMARY:")
  (show-classes)
  (show-generics))

(define (show-statistics)
  (print "STATISTICS:")
  (print "number of classes: " (length *classes*))
  (print "number of classes which support fast slot access: " *num-classes-fast-slot-access*)
  (print "number of generic methods: " (length *generics*))
  (print "number of static dispatch generic methods: " *num-zero-dispatch*)
  (print "number of single dispatch generic methods: " *num-single-dispatch*)
  (print "number of double dispatch generic methods: " *num-double-dispatch*)
  (print "number of multi dispatch generic methods: " *num-multi-dispatch*))

(define (read-info! file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((sexpr (read)))
        (unless (eof-object? sexpr)
          (match-case sexpr
            ((class ?name (. ?precedence-list) (. ?slots))
             ;; (print "name: " name " precedence-list: " precedence-list " slots: " slots)
             (add-class! name slots precedence-list))
            ((method (?name :around . ?args))
             (add-method! name args))
            ((method (?name . ?args))
             (add-method! name args))
            )
          (loop (read))))
      (set! *classes* (reverse! *classes*))
      (set! *generics* (reverse! *generics*)))))

(define (generate-compilation-support-code)
  (pp `(define *class-global-info*
         (quote ,(map (lambda (class-info)
                        (cons* (class-info/name class-info) 
                               (class-info/idx class-info) 
                               (if (class-info/fast-slot-access? class-info)
                                 (class-info/slots class-info)
                                 '())))
                      *classes*))))
  (pp '(define (opt-info/class-info name)
         (cond 
          ((assq name *class-global-info*) =>
           cdr)
          (else 
           #f))))
  (pp '(define (opt-class-info/class-idx info)
         (car info)))
  (pp '(define (opt-class-info/slot->idx info slot-name)
         (let loop ((pos 2) ;; the first to slot-positions are reserved for instance tag and class reference
                    (slots (cdr info)))
           (cond 
            ((null? slots) #f)
            ((eq? slot-name (car slots)) pos)
            (else (loop (+ pos 1) (cdr slots)))))))
  (pp '(define (opt-class-info/slots info)
         (cdr info)))
  (pp `(define *generic-global-info*
         (quote ,(map (lambda (gen-info)
                        (cons* (generic-info/name gen-info)
                               (generic-info/idx gen-info)
                               (generic-info/discriminator-pos-list gen-info)))
                      *generics*))))
  (pp '(define (opt-info/generic-info name)
         (cond
          ((assq name *generic-global-info*) =>
           cdr)
          (else
           #f))))
  (pp '(define (opt-generic-info/idx info)
         (car info)))
  (pp '(define (opt-generic-info/discriminator-pos-list info)
         (cdr info)))
  (pp `(define (opt-info/num-classes)
         ,(length *classes*)))
  (pp `(define (opt-info/num-generics)
         ,(length *generics*)))
  )

(define (main argv)
  (let* ((args (cdr argv))
         (info-file (if (null? args) "sal-class-info.data" (car args))))
    (with-output-to-file "code-analyzer.out"
      (lambda ()
        (read-info! info-file)
        (fast-slot-access-optimization!)
        (dispatch-optimization!)
        (show-database)
        (show-statistics)))
    (with-output-to-file "compilation-support-code.scm"
      (lambda ()
        (generate-compilation-support-code)))))

