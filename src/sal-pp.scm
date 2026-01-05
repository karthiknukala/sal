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

(module sal-pp
        (include "sal.sch")
        (include "pretty.macros")
        (import front-end pretty lsal-pretty-printer sal-pretty-printer sal-value-pretty-printer sal-context)
        (export (sal/enable-ast-pretty-printer! flag)
                (sal/set-sal-pp-proc! proc)
                (sal/initialize-pp!)
                (sal/set-pp-max-depth! max)
                (sal/set-pp-max-width! max)
                (sal/set-pp-max-ribbon! max)
                (sal/set-pp-max-num-lines! max)
                (sal/set-pp-simplify-qualified-names-flag! flag)
                (sal->doc obj . depth)
                (sal-value->doc obj . depth)
                (sal/pretty doc)
                (sal-value/pp obj)
                (sal/pp obj . indentation)
                (sal/pp-simple obj)
                (sal/pp-detailed obj)
                (sal/pp-single-line obj)
                (sal/pp-instance obj)
                (sal-application/infix? app)
                (sal-application/precedence app)
                (sal-application/associativity app))
        )

(define *pp-info* #unspecified)
(define *pp-value-info* #unspecified)
(define *pretty-print-ast?* #t)

(define (sal/enable-ast-pretty-printer! flag)
  (set! *pretty-print-ast?* flag))

(define (update-pp-value-info!)
  (set! *pp-value-info* (change-class *pp-info* <pp-value-info++>)))

(define *pp-sal-ast-proc* sal-ast->sal-doc)

(define-api (sal/set-sal-pp-proc! (proc procedure?))
  (set! *pp-sal-ast-proc* proc))
  
(define-method (object->doc (ast <sal-ast>) (pp-info <pp-info++>) (depth <primitive>))
  (if *pretty-print-ast?*
    (*pp-sal-ast-proc* ast pp-info depth)
    (call-next-method)))
  
(define-api (sal/set-pp-max-depth! (max natural?))
  (set-slot-value! *pp-info* :max-depth max)
  (update-pp-value-info!))
(define-api (sal/set-pp-max-width! (max natural?))
  (set-slot-value! *pp-info* :max-width max)
  (update-pp-value-info!))
(define-api (sal/set-pp-max-ribbon! (max natural?))
  (set-slot-value! *pp-info* :max-ribbon max)
  (update-pp-value-info!))
(define-api (sal/set-pp-max-num-lines! (max natural?))
  (set-slot-value! *pp-info* :max-num-lines max)
  (update-pp-value-info!))
(define-api (sal/set-pp-simplify-qualified-names-flag! (flag boolean?))
  (set-slot-value! *pp-info* :simplify-qualified-names? flag)
  (update-pp-value-info!))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--pp-max-depth=<num>"
                                      :description "Maximum number of nested structures before ellipsis (default: 10)."
                                      :proc (front-end-adapter/nz-nat-arg sal/set-pp-max-depth!)))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--pp-max-width=<num>"
                                      :description "Maximum line width (default: 120)."
                                      :proc (front-end-adapter/nz-nat-arg sal/set-pp-max-width!)))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--pp-max-ribbon=<num>"
                                      :description "Maximum line `ribbon' width (default: 60), where `ribbon' width = width - indentation."
                                      :proc (front-end-adapter/nz-nat-arg sal/set-pp-max-ribbon!)))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--pp-num-lines=<num>"
                                      :description "Maximum number of line before ellipsis (default: 100)."
                                      :proc (front-end-adapter/nz-nat-arg sal/set-pp-max-num-lines!)))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--full-qualified-names"
                                      :description "The default behavior is to suppress the context name associated with qualified names. This option will force to print the context name associated with every qualified name."
                                      :proc (lambda () (sal/set-pp-simplify-qualified-names-flag! #f))))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--sal-syntax"
                                      :description "The error messages and counter-examples are printed using the SAL official concrete syntax (default)."
                                      :proc (lambda () (sal/set-sal-pp-proc! sal-ast->sal-doc))))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Pretty Printing"
                                      :long-opt-name "--lsal-syntax"
                                      :description "The error messages and counter-examples are printed using the LSAL (lisp-like) syntax."
                                      :proc (lambda () (sal/set-sal-pp-proc! sal-ast->lsal-doc))))
                                      

(define-api (sal->doc obj . depth)
  (let ((depth (optional-arg depth 0)))
    (object->doc obj *pp-info* depth)))

(define-api (sal-value->doc obj . depth)
  (let ((depth (optional-arg depth 0)))
    (object->doc obj *pp-value-info* depth)))

(define-api (sal/pretty doc)
  (pp/pretty doc *pp-info*))

(define (sal/initialize-pp!)
  (set! *pp-info* (make-pp-info++ :max-width 120 :max-ribbon 60 :max-depth 10 :max-num-lines 100 :simplify-qualified-names? #t))
  (update-pp-value-info!))

(define-api (sal/pp obj . indent)
  (if (null? indent)
    (pp/object obj *pp-info*)
    (pp/pretty (pp/nest (car indent) (object->doc obj *pp-info* 0)) *pp-info*)))

(define-api (sal-value/pp obj)
  (pp/object obj *pp-value-info*))

(define (sal/pp-simple obj)
  (pp/object obj (make-pp-info++ :max-width 120 :max-ribbon 60 :max-depth 100 :max-num-lines 100 :simplify-qualified-names? #t)))

(define (sal/pp-detailed obj)
  (pp/object obj (make-pp-info++ :max-width 120 :max-ribbon 60 :max-depth 1000 :max-num-lines 1000 :simplify-qualified-names? #f)))

(define (sal/pp-single-line obj)
  (pp/object obj (make-pp-info++ :max-width 10000 :max-ribbon 10000 :max-depth 10000 :max-num-lines 1 :simplify-qualified-names? #t)))

(define (sal/pp-instance obj)
  (cond
   ((instance? obj)
    (print ":class " (class-name-of (class-of obj)))
    (for-each (lambda (slot-id)
                (display slot-id)
                (display " ")
                (cond
                 ((eq? slot-id :context)
                  (print "[" (sal-context/name (slot-value obj slot-id)) "]"))
                 (else
                  (let ((val (slot-value obj slot-id))
                        (indent (+ (string-length (keyword->string slot-id)) 1)))
                    (cond
                     ((list? val)
                      (let ((doc-lst (map (lambda (val) (object->doc val *pp-info* 0)) val)))
                        (sal/pretty (pp/nest (+ indent 1) (pp/concat "(" (doc-list-with-sep doc-lst ",") ")")))))
                     (else
                      (sal/pp val indent)))
                    (print "")))))
              (slot-value (class-of obj) :slots)))
   (else
    (sal/pp obj))))

(define-generic (sal-application/infix? app))
(define-method (sal-application/infix? (app <sal-application>)) #f)
(define-method (sal-application/infix? (app <sal-infix-application>)) #t)

(define-generic (sal-application/precedence app))
(define-method (sal-application/precedence (app <sal-and>)) 40)
(define-method (sal-application/precedence (app <sal-or>)) 30)
(define-method (sal-application/precedence (app <sal-implies>)) 20)
(define-method (sal-application/precedence (app <sal-eq>)) 10)
(define-method (sal-application/precedence (app <sal-diseq>)) 15)
(define-method (sal-application/precedence (app <sal-iff>)) 10)
(define-method (sal-application/precedence (app <sal-xor>)) 15)
(define-method (sal-application/precedence (app <sal-add>)) 80)
(define-method (sal-application/precedence (app <sal-sub>)) 80)
(define-method (sal-application/precedence (app <sal-mul>)) 90)
(define-method (sal-application/precedence (app <sal-div>)) 90)
(define-method (sal-application/precedence (app <sal-arith-relation>)) 60)
(define-method (sal-application/precedence (app <sal-idiv>)) 90)
(define-method (sal-application/precedence (app <sal-mod>)) 90)

(define-generic (sal-application/associativity app))
(define-method (sal-application/associativity (app <sal-application>)) 'none)
(define-method (sal-application/associativity (app <sal-and>)) 'left-right)
(define-method (sal-application/associativity (app <sal-not>)) 'left-right)
(define-method (sal-application/associativity (app <sal-implies>)) 'right)
(define-method (sal-application/associativity (app <sal-iff>)) 'left-right)
(define-method (sal-application/associativity (app <sal-xor>)) 'left-right)
(define-method (sal-application/associativity (app <sal-add>)) 'left-right)
(define-method (sal-application/associativity (app <sal-sub>)) 'left)
(define-method (sal-application/associativity (app <sal-mul>)) 'left-right)
(define-method (sal-application/associativity (app <sal-div>)) 'left)






