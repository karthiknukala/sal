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

(module sal2scm-random
        (include "sal.sch")
        (import sal2scm-core sal2scm queue sal-type unique-names sal-ast-table sal-decls)
        (export (sal-type/gen-pick-random-elem type ctx env))
        )

(define-generic (sal-type/gen-pick-random-elem type ctx env))

(define (gen-cached-pick-random-proc type ctx env proc)
  (let* ((pick-random-table (slot-value ctx :pick-random-table))
         (proc (cond
                ((and (not (sal-ast/contains-open-references? type))
                      (sal-ast-table/get pick-random-table type)) =>
                      cdr)
                (else
                 (let ((name (gen-unique-name 'pick-random)))
                   (sal-ast-table/put! pick-random-table type name)
                   (let ((new-scm-decl `(define (,name) ,(proc))))
                     (sal-scm-context/add-decl! ctx new-scm-decl)
                     name))))))
    `(,proc)))

;; REMARK: I didn't implement code for datatypes.

;; this covers all cases not covered by the other methods...
(define-method (sal-type/gen-pick-random-elem (type <sal-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-pick-random-proc 
   type ctx env 
   (lambda ()
     (let* ((it (sal-type->scm-iterator type ctx env))
            (elem-vect (gen-unique-name 'elem-vect))
            (get-random-elem (gen-unique-name 'get-random))
            (elem-vect-def `(define ,elem-vect #f))
            (get-random-def `(define (,get-random-elem)
                               (unless ,elem-vect
                                 (set! ,elem-vect (list->vector (iterator->list ,it))))
                               (vector-ref ,elem-vect (rand (vector-length ,elem-vect))))))
       (sal-scm-context/add-decl! ctx elem-vect-def)
       (sal-scm-context/add-decl! ctx get-random-def)
       `(,get-random-elem)))))

(define-method (sal-type/gen-pick-random-elem (type <sal-type-name>) (ctx <sal-scm-context>) (env <primitive>))
  (let ((definition (sal-type-name/definition type)))
    (unless definition
      (sign-unsupported-feature type "Uninterpreted types are not supported by this tool."))
    (sal-type/gen-pick-random-elem definition ctx env)))

(define-method (sal-type/gen-pick-random-elem (type <sal-bool-type>) (ctx <sal-scm-context>) (env <primitive>))
  '(if (= (rand 2) 1) #t #f))

(define-method (sal-type/gen-pick-random-elem (type <sal-subrange>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-pick-random-proc type ctx env 
                               (lambda ()
                                 (let ((lower (sal->scm (slot-value type :lower) ctx env))
                                       (upper (sal->scm (slot-value type :upper) ctx env))
                                       (lower-var (gen-unique-name 'lower)))
                                   (if (slot-value ctx :gmp?)
                                     `(let ((,lower-var ,lower))
                                        (+mpq ,lower-var (integer->mpq (rand (+ (mpq->integer (-mpq ,upper ,lower-var)) 1)))))
                                     `(let ((,lower-var ,lower))
                                        (+fx ,lower-var (rand (+fx (-fx ,upper ,lower-var) 1)))))))))
  
(define-method (sal-type/gen-pick-random-elem (type <sal-tuple-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-pick-random-proc type ctx env 
                               (lambda ()
                                 `(vector ,@(map (cut sal-type/gen-pick-random-elem <> ctx env)
                                                 (slot-value type :types))))))
                               
(define-method (sal-type/gen-pick-random-elem (type <sal-record-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-pick-random-proc type ctx env
                               (lambda ()
                                 `(vector ,@(map (lambda (field)
                                                   (sal-type/gen-pick-random-elem (slot-value field :type) ctx env))
                                                 (sal-record-type/sorted-fields type))))))

(define *int-random-lower* (- 0 16777216))
(define *int-random-upper* 16777215)
(define *int-random-range* (+ (- *int-random-upper* *int-random-lower*) 1))

(define-method (sal-type/gen-pick-random-elem (type <sal-nat-type>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :gmp?)
    `(integer->mpq (rand ,*int-random-upper*))
    `(rand ,*int-random-upper*)))

(define-method (sal-type/gen-pick-random-elem (type <sal-int-type>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :gmp?)
    `(integer->mpq (+fx ,*int-random-lower* (rand ,*int-random-range*)))
    `(+fx ,*int-random-lower* (rand ,*int-random-range*))))

(define-method (sal-type/gen-pick-random-elem (type <sal-real-type>) (ctx <sal-scm-context>) (env <primitive>))
  (if (slot-value ctx :gmp?)
    `(integer->mpq (+fx ,*int-random-lower* (rand ,*int-random-range*)))
    `(+fx ,*int-random-lower* (rand ,*int-random-range*))))

(define-method (sal-type/gen-pick-random-elem (type <sal-function-type>) (ctx <sal-scm-context>) (env <primitive>))
  (gen-cached-pick-random-proc 
   type ctx env 
   (lambda ()
     (let ((domain (slot-value type :domain))
           (range (slot-value type :range)))
       (let ((domain-size (sal-type/number-of-elements-as-integer domain))
             (res-name (gen-unique-name 'random-array))
             (idx (gen-unique-name 'idx)))
         `(let ((,res-name (make-vector ,domain-size)))
            (let loop ((,idx 0))
              (when (< ,idx ,domain-size)
                (vector-set! ,res-name ,idx ,(sal-type/gen-pick-random-elem range ctx env))
                (loop (+ ,idx 1))))
            ,res-name))))))
   
           



  
         
         
