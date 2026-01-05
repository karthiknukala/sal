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

(module sal2scm-type
        (include "sal.sch")
        (import sal2scm-core sal-type unique-names sal-ast-table
                sal-decls gmp-scheme sal-expression iterators queue
                sal2scm-runtime sal-scm-obj-table)
        (export (sal-scm/product-idx alt-size-list idx . do-not-include-size-assignments?)
                (sal-scm/mk-product-list num-list)
                (sal-type/simple-idx-val? type)
                (sal-type/idx->val type ctx env arg)
                (sal-type/idx->val-proc type ctx env)
                (sal-type/val->idx type ctx env arg)
                (sal-type/val->idx-proc type ctx env)
                (sal-data-type/pre-allocated-arguments type)
                (sal-type/setup-code type))
        )

;; ----------------------------------------------------------------------
;;
;; Return #t, if the type can be easily converted to/from an integer index.
;;
;; ----------------------------------------------------------------------
(define-generic (sal-type/simple-idx-val? type))
(define-method (sal-type/simple-idx-val? (type <sal-type>)) #f)
(define-method (sal-type/simple-idx-val? (type <sal-scalar-type>)) #t)
(define-method (sal-type/simple-idx-val? (type <sal-subrange>))
  (or (instance-of? (slot-value type :lower) <sal-numeral>)
      (instance-of? (slot-value type :lower) <sal-name-expr>)))
(define-method (sal-type/simple-idx-val? (type <sal-type-name>))
  (let ((definition (sal-type-name/definition type)))
    (and definition
         (sal-type/simple-idx-val? definition))))

;; ----------------------------------------------------------------------
;;
;; Convert an index (number) is a runtime value.
;; This procedure is mainly used to enumerate the elements of a type.
;;
;;
;; ----------------------------------------------------------------------
(define-generic (sal-type/idx->val type ctx env arg))

(define-method (sal-type/idx->val (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  `(,(sal-type/idx->val-proc type ctx env) ,arg))

(define-method (sal-type/idx->val (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  (if (not (sal-type/simple-idx-val? type))
    (call-next-method)
    (let ((lower (sal->scm (slot-value type :lower) ctx env)))
      (cond
       ((slot-value ctx :gmp?)
        `(+mpq (integer->mpq ,arg) ,lower ctx env))
       ((eq? lower 0)
        arg)
       (else
        `(+fx ,arg ,lower))))))
    
(define-method (sal-type/idx->val (type <sal-scalar-type>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  arg)

(define-method (sal-type/idx->val (type <sal-bool-type>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  `(if (=fx ,arg 0) #f #t))

(define-method (sal-type/idx->val (type <sal-type-name>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  (sal-type/idx->val (sal-type-name/definition type) ctx env arg))

;; Given a list of numbers alt-size-list: (s_1 ... s_n)
;; and a symbol idx:
;; 
;; return two results
;;
;; - a list of new indexes: (idx!1 ... idx!n)
;; - a definition list:
;;   ((idx!1    (/fx       idx       (* s_2 ... s_n)))
;;    (r_idx!1  (remainder idx       (* s_2 ... s_n)))
;;    (idx!2    (/fx       r_idx!1   (* s_3 ... s_n)))
;;    (r_idx!2  (remainder r_idx!1   (* s_3 ... s_n)))
;;    (idx!3    (/fx       r_idx!2   (* s_4 ... s_n)))
;;    (r_idx!3  (remainder r_idx!2   (* s_4 ... s_n)))
;;    ...
;;    (idx!n    r_idx!n-1))
;; 
;; Remark: when do-not-include-size-assignments? is #t
;; Then a idx!i is only included if s_i > 1
(define (sal-scm/product-idx alt-size-list idx . do-not-include-size-assignments?)
  [assert (alt-size-list) (for-all (cut > <> 0) alt-size-list)]
  (let ((do-not-include-size-assignments? (optional-arg do-not-include-size-assignments? #f))
        (product-size-list (let loop ((alt-size-list (cdr alt-size-list))
                                  (result '()))
                         (if (null? alt-size-list)
                           (reverse! result)
                           (let* ((new-sum (fold-left * 1 alt-size-list))
                                  (new-result (cons new-sum result)))
                             (loop (cdr alt-size-list)
                                   new-result))))))
    (let loop ((product-size-list product-size-list)
               (alt-size-list alt-size-list)
               (idx idx)
               (idx-name-list '())
               (result '()))
      (cond
       ((not (null? product-size-list))
        (if (=fx (car alt-size-list) 1)
          ;; size = 1 case
          (let* ((idx-i (gen-unique-name 'idx))
                 (assignment `(,idx-i 0)))
            (loop (cdr product-size-list)
                  (cdr alt-size-list)
                  idx
                  (cons idx-i idx-name-list)
                  (if do-not-include-size-assignments? 
                    result
                    (cons assignment result))))
          ;; size > 1 case
          (let* ((idx-i (gen-unique-name 'idx))
                 (r-idx-i (gen-unique-name 'r-idx))
                 (assignment1 `(,idx-i (/fx ,idx ,(car product-size-list))))
                 (assignment2 `(,r-idx-i (remainder ,idx ,(car product-size-list)))))
            (loop (cdr product-size-list)
                  (cdr alt-size-list)
                  r-idx-i
                  (cons idx-i idx-name-list)
                  (cons* assignment2 assignment1 result)))))
       (else
        (let* ((idx-i (gen-unique-name 'idx))
               (assignment `(,idx-i ,idx)))
          (values (reverse! (cons idx-i idx-name-list))
                  (reverse! (cons assignment result)))))))))

(define (gen-cached-proc-core type ctx proc table-slot id-prefix)
  (let* ((table (slot-value ctx table-slot))
         (proc (cond
                ((and (not (sal-ast/contains-open-references? type))
                      (sal-ast-table/get table type)) =>
                      cdr)
                (else
                 (let ((name (gen-unique-name id-prefix)))
                   (sal-ast-table/put! table type name)
                   (let ((new-scm-decl `(define ,name ,(proc))))
                     (sal-scm-context/add-decl! ctx new-scm-decl)
                     name))))))
    proc))


(define (gen-cached-idx->val-proc type ctx proc)
  (gen-cached-proc-core type ctx proc :idx-val-table 'idx->val))

(define (gen-cached-val->idx-proc type ctx proc)
  (gen-cached-proc-core type ctx proc :val-idx-table 'val->idx))

(define-generic (sal-type/idx->val-proc type ctx env))

(define (type-product-idx->val-proc-core type-list ctx env idx empty-vect init-idx)
  (let ((alt-size-list (map sal-type/number-of-elements-as-integer type-list))
        (result (gen-unique-name 'result)))
    (multiple-value-bind
        (child-idx-list assignment-list)
        (sal-scm/product-idx alt-size-list idx)
      `(let* (,@assignment-list
              (,result ,empty-vect))
         ,@(let ((curr-idx init-idx))
             (map (lambda (type child-idx)
                    (let ((i curr-idx))
                      (set! curr-idx (+ curr-idx 1))
                      `(vector-set! ,result ,i ,(sal-type/idx->val type ctx env child-idx))))
                type-list
                child-idx-list))
         ,result))))

(define (type-product-idx->val-proc type-list ctx env)
  (let ((idx (gen-unique-name 'idx))
        (empty-elem `(make-vector ,(length type-list))))
    `(lambda (,idx)
       ,(type-product-idx->val-proc-core type-list ctx env idx empty-elem 0))))

(define-method (sal-type/idx->val-proc (type <sal-type-name>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-type/idx->val-proc (sal-type-name/definition type) ctx env))

(define-method (sal-type/idx->val-proc (type <sal-tuple-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-idx->val-proc 
   type ctx 
   (lambda ()
     (type-product-idx->val-proc (slot-value type :types) ctx env))))
   
(define-method (sal-type/idx->val-proc (type <sal-record-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-idx->val-proc
   type ctx
   (lambda ()
     (let ((types (map (cut slot-value <> :type) (sal-record-type/sorted-fields type))))
       (type-product-idx->val-proc types ctx env)))))

(define-method (sal-type/idx->val-proc (type <sal-scalar-type>) (ctx <sal-scm-context>) (env <primitive>))
  `(lambda (idx) idx))

(define-method (sal-type/idx->val-proc (type <sal-bool-type>) (ctx <sal-scm-context>) (env <primitive>))
  `(lambda (idx) (if (=fx idx 0) #f #t)))

(define (sal-subrange/val-proc-core type ctx env mpq? inv?)
  (let ((+proc (if inv?
                 (if mpq? -mpq -fx)
                 (if mpq? +mpq +fx)))
        (base (if mpq? '(integer->mpq idx) 'idx)))
    (if (sal-type/simple-idx-val? type)
      (let ((lower (sal->scm (slot-value type :lower) ctx env)))
        (if (eq? lower 0)
          `(lambda (idx) ,base)
          `(lambda (idx) (+proc ,base ,(sal->scm (slot-value type :lower) ctx env)))))
      (gen-cached-idx->val-proc 
       type ctx 
       (lambda ()
         (let* ((lower (sal->scm (slot-value type :lower) ctx env))
                (lower-name (gen-unique-name 'lower))
                (lower-decl `(define ,lower-name ,lower)))
           (sal-scm-context/add-decl! ctx lower-decl)
           `(lambda (idx) (+proc ,base ,lower-name))))))))

(define (sal-subrange/idx->val-proc-core type ctx env mpq?)
  (sal-subrange/val-proc-core type ctx env mpq? #f))

(define (sal-subrange/val->idx-proc-core type ctx env mpq?)
  (sal-subrange/val-proc-core type ctx env mpq? #t))

(define-method (sal-type/idx->val-proc (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-subrange/idx->val-proc-core type ctx env (slot-value ctx :gmp?)))

(define (sal-scm/sum-idx size-list idx)
  [assert (size-list) (> (length size-list) 1)]
  (let loop ((size-list (cdr size-list))
             (acc (car size-list))
             (idx-list `(,idx))
             (cond-list `((<fx ,idx ,(car size-list)))))
    (if (null? (cdr size-list))
      (values (reverse! (cons `(-fx ,idx ,acc) idx-list))
              (reverse! (cons 'else cond-list)))
      (let* ((curr-size (car size-list))
             (new-idx `(-fx ,idx ,acc))
             (new-acc (+fx acc curr-size))
             (new-cond `(and (>=fx ,idx ,acc) (<fx ,idx ,new-acc))))
        (loop (cdr size-list)
              new-acc
              (cons new-idx idx-list)
              (cons new-cond cond-list))))))

(define-method (sal-type/idx->val-proc (type <sal-data-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-idx->val-proc 
   type ctx 
   (lambda ()
     (let* ((constructors (slot-value type :constructors))
            (max-constructor-size (sal-data-type/max-constructor-size type))
            [constructor/idx->val-proc 
             (lambda (constructor idx tag-idx)
               (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
                      (accessor-type-list (map sal-name-expr/accessor-type accessor-list))
                      (empty-elem `(vector ,tag-idx ,@(generate-list (lambda (_) #f) max-constructor-size))))
                 (if (null? accessor-type-list)
                   empty-elem
                   (type-product-idx->val-proc-core accessor-type-list 
                                                    ctx env idx empty-elem 1))))])
       (if (null? (cdr constructors))
         `(lambda (idx)
            ,(constructor/idx->val-proc (car constructors) 'idx 0))
         (let ((constructor-sizes (map sal-constructor/number-of-elements-as-integer constructors))
               (tag-idx 0))
           (multiple-value-bind
               (idx-list cond-list)
               (sal-scm/sum-idx constructor-sizes 'idx)
             `(lambda (idx)
                (cond
                 ,@(map (lambda (constructor idx cond)
                          (let ((result `(,cond 
                                          ,(constructor/idx->val-proc constructor idx tag-idx))))
                            (set! tag-idx (+fx tag-idx 1))
                            result))
                        constructors
                        idx-list
                        cond-list))))))))))

(define-method (sal-type/idx->val-proc (type <sal-function-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-idx->val-proc 
   type ctx 
   (lambda ()
     (let* ((domain (slot-value type :domain))
            (range (slot-value type :range))
            (proc-child (sal-type/idx->val range ctx env '(/fx idx n)))
            (range-size (sal-type/number-of-elements-as-integer range))
            (domain-size (sal-type/number-of-elements-as-integer domain))
            (init-n (expt range-size (-fx domain-size 1))))
       `(lambda (idx)
          (let ((v (make-vector ,domain-size)))
            (let loop ((idx idx)
                       (n ,init-n)
                       (i 0))
              (when (<fx i ,domain-size)
                (vector-set! v i ,proc-child)
                (loop (remainder idx n)
                      (/fx n ,range-size)
                      (+fx i 1))))
            v))))))

;; Remark: if the type cannot be handled by the previous methods, then
;; I create a mapping (array) with the element types.
(define-method (sal-type/idx->val-proc (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-idx->val-proc 
   type ctx 
   (lambda ()
     (try
      (let* ((it (sal-type/make-iterator type))
             (map-queue (make-queue)))
        (iterator/for-each (lambda (value)
                             (queue/insert! map-queue (sal->scm value ctx env)))
                           it)
        (let* ((map-vect (list->vector (queue->list map-queue)))
               (map-id (gen-unique-name 'idx->val-map))
               (new-decl `(define ,map-id (quote ,map-vect))))
          (sal-scm-context/add-decl! ctx new-decl)
          `(lambda (idx)
             (vector-ref ,map-id idx))))
      (catch 'type-iterator
             (lambda (msg)
               (sign-unsupported-feature type "Failed to create a mapping from index to type element, reason: ~a" msg)))))))


;; ----------------------------------------------------------------------
;;
;; Convert a runtime value in an index.
;; This procedure is mainly used to produce indexes to access arrays.
;; Remark: sal-type/val->idx is the inverse of sal-type/idx->val
;;
;;
;; ----------------------------------------------------------------------
(define-generic (sal-type/val->idx type ctx env arg))

(define-method (sal-type/val->idx (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  `(,(sal-type/val->idx-proc type ctx env) ,arg))

(define-method (sal-type/val->idx (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  (if (not (sal-type/simple-idx-val? type))
    (call-next-method)
    (let ((lower (sal->scm (slot-value type :lower) ctx env)))
      (cond 
       ((slot-value ctx :gmp?)
        `(-mpq (integer->mpq ,arg) ,(sal->scm (slot-value type :lower) ctx env)))
       ((eq? lower 0)
        arg)
       (else
        `(-fx ,arg ,lower))))))
    
(define-method (sal-type/val->idx (type <sal-scalar-type>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  arg)

(define-method (sal-type/val->idx (type <sal-bool-type>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  `(if ,arg 1 0))

(define-method (sal-type/val->idx (type <sal-type-name>) (ctx <sal-scm-context>) (env <primitive>) (arg <primitive>))
  (sal-type/val->idx (sal-type-name/definition type) ctx env arg))
          
(define-generic (sal-type/val->idx-proc type ctx env))

(define-method (sal-type/val->idx-proc (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-subrange/val->idx-proc-core type ctx env (slot-value ctx :gmp?)))

(define-method (sal-type/val->idx-proc (type <sal-scalar-type>) (ctx <sal-scm-context>) (env <primitive>))
  `(lambda (val) val))

(define-method (sal-type/val->idx-proc (type <sal-bool-type>) (ctx <sal-scm-context>) (env <primitive>))
  `(lambda (val) (if val 1 0)))

(define-method (sal-type/val->idx-proc (type <sal-type-name>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-type/val->idx-proc (sal-type-name/definition type) ctx env))

;; Given the list (n1 n2 ... ni)
;; return the list (n2*...*ni n3*...*ni ... 1)
(define (sal-scm/mk-product-list num-list)
  (let loop ((num-list (cdr num-list))
             (result '()))
    (if (null? num-list)
      (reverse! (cons 1 result))
      (let ((new-elem (fold-left * 1 num-list)))
        (loop (cdr num-list)
              (cons new-elem result))))))
  
(define (type-product-val->idx-proc-core type-list ctx env val first-idx)
  (let* ((alt-size-list (map sal-type/number-of-elements-as-integer type-list))
         (product-list (sal-scm/mk-product-list alt-size-list))
         (idx 0))
    (let loop ((type-list type-list)
               (product-list product-list)
               (idx first-idx)
               (result #f))
      (if (null? type-list)
        result
        (let* ((type (car type-list))
               (coefficient (car product-list))
               (factor (sal-type/val->idx type ctx env `(vector-ref ,val ,idx)))
               (new-elem `(*fx ,coefficient ,factor)))
          (loop (cdr type-list)
                (cdr product-list)
                (+ idx 1)
                (if result
                  `(+fx ,result ,new-elem)
                  new-elem)))))))

(define (type-product-val->idx-proc type-list ctx env)
  (let ((val (gen-unique-name 'val)))
    `(lambda (,val)
       ,(type-product-val->idx-proc-core type-list ctx env val 0))))

(define-method (sal-type/val->idx-proc (type <sal-tuple-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-val->idx-proc 
   type ctx 
   (lambda ()
     (type-product-val->idx-proc (slot-value type :types) ctx env))))
   
(define-method (sal-type/val->idx-proc (type <sal-record-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-val->idx-proc
   type ctx
   (lambda ()
     (let ((types (map (cut slot-value <> :type) (sal-record-type/sorted-fields type))))
       (type-product-val->idx-proc types ctx env)))))


;; Given the list (n1 n2 ... ni)
;; return the list (0 n1 n1+n2 ... n1+...+ni-1)
(define (mk-sum-list num-list)
  (let loop ((num-list num-list)
             (sum 0)
             (result '()))
    (if (null? num-list)
      (reverse! result)
      (loop (cdr num-list)
            (+ sum (car num-list))
            (cons sum result)))))

(define-method (sal-type/val->idx-proc (type <sal-data-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-val->idx-proc 
   type ctx 
   (lambda ()
     (let* ((constructors (slot-value type :constructors))
            (num-constructors (length constructors)))
       (if (null? (cdr constructors))
         `(lambda (val)
            ,(constructor->idx-body (car constructors) ctx env 'val))
         (let* ((constructor-sizes (map sal-constructor/number-of-elements-as-integer constructors))
                (sum-list (mk-sum-list constructor-sizes))
                (tag-idx 0))
           `(lambda (val)
              (let ((tag (vector-ref val 0)))
                (cond
                 ,@(map (lambda (constructor delta)
                          (let ((alt `(,(if (<fx tag-idx (- num-constructors 1))
                                          `(=fx tag ,tag-idx)
                                          'else)
                                       (+fx ,delta ,(constructor->idx-body constructor ctx env 'val)))))
                            (set! tag-idx (+fx tag-idx 1))
                            alt))
                        constructors
                        sum-list))))))))))
  
(define (constructor->idx-body constructor ctx env val)
  (let* ((accessor-list (sal-name-expr/constructor-accessors constructor))
         (accessor-type-list (map sal-name-expr/accessor-type accessor-list)))
    (if (null? accessor-list)
      0
      (type-product-val->idx-proc-core accessor-type-list ctx env val 1))))

(define (sal-function-type/val->idx-proc type ctx env vector-ref-proc-name)
  (gen-cached-val->idx-proc 
   type ctx 
   (lambda ()
     (let* ((domain (slot-value type :domain))
            (range (slot-value type :range))
            (proc-child (sal-type/val->idx range ctx env `(,vector-ref-proc-name val i)))
            (range-size (sal-type/number-of-elements-as-integer range))
            (domain-size (sal-type/number-of-elements-as-integer domain))
            (init-n (expt range-size (-fx domain-size 1))))
       `(lambda (val)
          (let loop ((i 0)
                     (idx 0)
                     (n ,init-n))
            (if (<fx i ,domain-size)
              (loop (+fx i 1)
                    (+fx idx (*fx n ,proc-child))
                    (/fx n ,range-size))
              idx)))))))

(define-method (sal-type/val->idx-proc (type <sal-function-type>) (ctx <sal-scm-context>) (env <primitive>))
  (sal-function-type/val->idx-proc type ctx env 'vector-ref))

;; Remark: if the type cannot be handled by the previous methods, then
;; I create a mapping (sal-scm-obj-table) from val to idx
(define-method (sal-type/val->idx-proc (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-val->idx-proc 
   type ctx 
   (lambda ()
     (try
      (let* ((it (sal-type/make-iterator type))
             (val->idx-table-id (gen-unique-name 'val->idx-map))
             ;; (val->idx-table (make-sal-scm-obj-table))
             (assignments (make-queue))
             (idx 0))
        (iterator/for-each (lambda (value)
                             (queue/insert! assignments `(sal-scm-obj-table/put! ,val->idx-table-id ,(sal->scm value ctx env) ,idx))
                             (set! idx (+ idx 1)))
                           it)
        (let ((new-decl `(define ,val->idx-table-id (let ((,val->idx-table-id (make-sal-scm-obj-table)))
                                                      ,@(queue->list assignments)
                                                      ,val->idx-table-id))))
          (sal-scm-context/add-decl! ctx new-decl)
          `(lambda (val)
             (cdr (sal-scm-obj-table/get ,val->idx-table-id val)))))
      (catch 'type-iterator
             (lambda (msg)
               (sign-unsupported-feature type "Failed to create a mapping from index to type element, reason: ~a" msg)))))))
   
;; Return a list of type/boolean. The length of the list is equals
;; to the maximum constructor size. A value is a type in pos idx 
;; when all constructors agree (or don't care) about
;; the type of the argument on that position, and #f when at least
;; two constructors don't agree about the type at that position.
;; The list is used to decide which children type will be prealloacted
;; for a datatype.
(define (sal-data-type/pre-allocated-arguments type)
  [assert (type) (instance-of? type <sal-data-type>)]
  (ensure ((type <sal-data-type>))
    (let* ((constructors (slot-value type :constructors))
           (size (sal-data-type/max-constructor-size type)))
      (let loop ((i 0)
                 (result '()))
        (if (< i size)
          (let inner-loop ((constructors constructors)
                           (curr-type #f))
            (if (null? constructors)
              (loop (+ i 1) (cons curr-type result))
              (let* ((curr-constructor (car constructors))
                     (accessor-list (sal-name-expr/constructor-accessors curr-constructor)))
                (if (>= i (length accessor-list))
                  ;; constructor has less than i arguments, so it doesn't care about position i.
                  (inner-loop (cdr constructors) curr-type)
                  (let* ((curr-accessor (list-ref accessor-list i))
                         (curr-accessor-type (sal-name-expr/accessor-type curr-accessor)))
                    (cond
                     ((not curr-type)
                      (inner-loop (cdr constructors) curr-accessor-type))
                     ((sal-type/equivalent? curr-type curr-accessor-type)
                      ;; Possible improvement: the curr-type and curr-accessor-type doesn't need to be syntatically equivalent, it is sufficient
                      ;; if their Scheme memory representation is equal. So, in a future version, I should implement a method that test whether
                      ;; the scheme representation of two types is equivalent.
                      ;; Examples:
                      ;;   Scheme memory equivalent types:
                      ;;       - nat and bool
                      ;;       - [1..3] and [2..7]
                      ;;       - scalar and [2..4]
                      ;;       - [bool, [1..3]]  and [nat, int]
                      ;;  Scheme memory incompatible types:
                      ;;       - [nat, bool] and bool
                      ;;       - [# idx : bool #] and [bool, bool]
                      (inner-loop (cdr constructors) curr-type))
                     (else
                      ;; at least two constructors don't agree about the type of the element at position i.
                      (loop (+ i 1) (cons #f result)))))))))
          (reverse! result))))))

;; ----------------------------------------------------------------------
;;
;; Create the template for an element of the given type.
;; This procedure is mainly used to preallocate memory 
;; for runtime values of a given type.
;;
;; ----------------------------------------------------------------------
(define-generic (sal-type/setup-code type))

(define-method (sal-type/setup-code (type <sal-type>)) 'not-assigned)
(define-method (sal-type/setup-code (type <sal-subrange>)) 'not-assigned)
(define-method (sal-type/setup-code (type <sal-int-type>)) 'not-assigned)
(define-method (sal-type/setup-code (type <sal-real-type>)) 'not-assigned)
(define-method (sal-type/setup-code (type <sal-tuple-type>))
  (apply vector (map sal-type/setup-code (slot-value type :types))))
(define-method (sal-type/setup-code (type <sal-record-type>))
  (apply vector (map (lambda (field)
                       (sal-type/setup-code (slot-value field :type)))
                     (sal-record-type/sorted-fields type))))
(define-method (sal-type/setup-code (type <sal-type-name>))
  (if (sal-decl/recursive? (slot-value type :decl))
    'not-assigned
    (let ((definition (sal-type-name/definition type)))
      (if definition
        (sal-type/setup-code definition)
        'not-assigned))))
(define-method (sal-type/setup-code (type <sal-data-type>))
  ;; For datatypes I only setup a child type if all constructors agree (or don't care) of that
  ;; type on that position
  (let* ((pre-allocated-children (sal-data-type/pre-allocated-arguments type))
         (result (make-vector (+ (length pre-allocated-children) 1) 'not-assigned))
         (idx 1))
    (for-each (lambda (type-or-false)
                (when type-or-false
                  (vector-set! result idx (sal-type/setup-code type-or-false)))
                (set! idx (+ idx 1)))
              pre-allocated-children)
    result))
(define-method (sal-type/setup-code (type <sal-function-type>))
  (let* ((n (sal-type/number-of-elements-as-integer (slot-value type :domain)))
         (result (make-vector n 'not-assigned))
         (range (slot-value type :range)))
    (let loop ((i 0))
      (when (< i n)
        (vector-set! result i (sal-type/setup-code range))
        (loop (+ i 1))))
    result))
(define-method (sal-type/setup-code (type <sal-subtype>))
  (sal-type/setup-code (sal-subtype/immediate-super-type type)))

  
