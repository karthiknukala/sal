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

(module sal-rename-variables
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal-ast-env symbol-set sal-ast-copy sal-module 
                symbol-table unique-names)
        (export <sal-var-rename-info>
                <sal-var-rename-info++>
                (sal-var-rename-info/new-decl info decl)
                (sal-ast/rename-variables-core ast info env)
                (sal-ast/rename-variables ast)
                (sal-ast/rename-variables-with-new-names ast)
                (sal-ast/rename-variables-with-local-new-names ast))
        )

(define-class <sal-var-rename-info> () (:defined-vars))
(define-class <sal-var-rename-info++> (<sal-var-rename-info>) (:preserve?))

(define-generic (sal-var-rename-info/new-decl info decl))
(define-method (sal-var-rename-info/new-decl (info <sal-var-rename-info>) (decl <sal-decl>))
  (let ((id (slot-value decl :id)))
    (copy-ast decl 
              :id (copy-ast id
                            :name (let ((curr-name (slot-value id :name)))
                                    (if (unique-name? curr-name)
                                      curr-name
                                      (gen-unique-name curr-name)))))))

(define-method (sal-var-rename-info/new-decl (info <sal-var-rename-info++>) (decl <sal-decl>))
  (let* ((id (slot-value decl :id))
         (aux-name (slot-value id :name))
         (name (cond
                ((unique-name? aux-name) =>
                 identity)
                (else
                 aux-name)))
         (already-defined-vars (slot-value info :defined-vars))
         (new-name (let loop ((i 0))
                     (let ((curr-name (if (and (slot-value info :preserve?) (= i 0))
                                        name
                                        (symbol-append name '_ (string->symbol (integer->string i))))))
                       (if (symbol-set/member? curr-name already-defined-vars)
                         (loop (+ i 1))
                         curr-name)))))
    (symbol-set/add! already-defined-vars new-name)
    (copy-ast decl
              :id (copy-ast id
                            :name new-name))))

(define (sal-ast/rename-variables-with-new-names ast)
  (sal-ast/rename-variables-core ast (make-instance <sal-var-rename-info> :defined-vars (make-symbol-set)) (make-empty-env)))

(define (sal-ast/rename-variables-with-local-new-names ast)
  (sal-ast/rename-variables-core ast (make-instance <sal-var-rename-info++> :defined-vars (make-symbol-set)) (make-empty-env)))

(define (sal-ast/rename-variables ast)
  (sal-ast/rename-variables-core ast (make-instance <sal-var-rename-info++> :defined-vars (make-symbol-set) :preserve? #t) (make-empty-env)))

(define (rename-slot ast slot-id info env)
  (sal-ast/rename-variables-core (slot-value ast slot-id) info env))

(define (rename-optional-slot ast slot-id info env)
  (let ((val (slot-value ast slot-id)))
    (if val
      (sal-ast/rename-variables-core (slot-value ast slot-id) info env)
      val)))

(define (rename-list lst info env)
  (conservative-map-1 (cut sal-ast/rename-variables-core <> info env) lst))

(define (rename-list-slot ast slot-id info env)
  (rename-list (slot-value ast slot-id) info env))

(define (sal-local-binds-ast/rename-core ast info env decl-slot-id body-slot-id map-body-proc)
  (let* ((local-decls (slot-value ast decl-slot-id))
         (new-local-decls (rename-list local-decls info env))
         (env (update-env* env local-decls new-local-decls)))
    (update-ast-slots ast
                      decl-slot-id new-local-decls 
                      body-slot-id (map-body-proc
                                    ast 
                                    body-slot-id
                                    info
                                    env))))

(define (sal-local-binds-ast/rename ast info env decl-slot-id body-slot-id)
  (sal-local-binds-ast/rename-core ast info env decl-slot-id body-slot-id rename-slot))

(define (sal-for-all-definition/rename ast info env)
  (sal-local-binds-ast/rename-core ast info env :local-decls :definitions rename-list-slot))

(define (sal-base-module/rename-core ast info env body-proc)
  (let* ((state-vars (slot-value ast :state-vars))
         (new-state-vars (rename-list state-vars info env))
         (env (update-env* env state-vars new-state-vars)))
    (body-proc info env new-state-vars)))

(define (sal-base-module/rename ast info env)
  (sal-base-module/rename-core 
   ast info env 
   (lambda (info env new-state-vars)
     (update-ast-slots ast
                       :state-vars new-state-vars
                       :definitions (rename-list-slot ast :definitions info env)
                       :initialization-definitions (rename-list-slot ast :initialization-definitions info env)
                       :transition-definitions (rename-list-slot ast :transition-definitions info env)
                       :initialization-command-section (rename-optional-slot ast :initialization-command-section info env)
                       :transition-command-section (rename-optional-slot ast :transition-command-section info env)))))

(define (sal-flat-module/rename ast info env)
  (sal-base-module/rename-core 
   ast info env
   (lambda (info env new-state-vars)
     (update-ast-slots ast
                       :state-vars new-state-vars
                       :definition (rename-slot ast :definition info env)
                       :initialization (rename-slot ast :initialization info env)
                       :transition (rename-slot ast :transition info env)
                       :skip (rename-slot ast :skip info env)
                       :component-info (rename-slot ast :component-info info env)
                       :valid-input-expr (rename-slot ast :valid-input-expr info env)
                       :valid-state-expr (rename-slot ast :valid-state-expr info env)
                       :valid-constant-expr (rename-slot ast :valid-constant-expr info env)))))

(define (rename-esm-slot ast slot-name info env)
  (and
   (slot-value ast slot-name)
   (rename-slot ast slot-name info env)))

(define (sal-esm-module/rename ast info env)
  (sal-base-module/rename-core 
   ast info env
   (lambda (info env new-state-vars)
     (update-ast-slots ast
                       :state-vars new-state-vars
                       :definition (rename-esm-slot ast :definition info env)
                       :initialization (rename-esm-slot ast :initialization info env)
                       :transition (rename-esm-slot ast :transition info env)))))

(define-generic (sal-ast/rename-variables-core ast info env))

(define-method (sal-ast/rename-variables-core (ast <sal-ast>) (info <sal-var-rename-info>) (env <primitive>))
  (let ((proc (lambda (n e)
                (sal-ast/rename-variables-core n info e))))
    (sal-ast/map ast env proc)))

(define-method (sal-ast/rename-variables-core (ast <sal-top-decl>) (info <sal-var-rename-info>) (env <primitive>))
  (sign-error "Method sal-ast/rename-variables-core cannot be used on top level declarations."))

(define-method (sal-ast/rename-variables-core (ast <sal-context>) (info <sal-var-rename-info>) (env <primitive>))
  (sign-error "Method sal-ast/rename-variables-core cannot be used on context declarations."))

(define-method (sal-ast/rename-variables-core (ast <sal-decl>) (info <sal-var-rename-info>) (env <primitive>))
  (let ((new-ast (call-next-method)))
    (sal-var-rename-info/new-decl info new-ast)))

(define-method (sal-ast/rename-variables-core (ast <sal-parametric-module>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-local-binds-ast/rename ast info env :local-decls :module))

(define-method (sal-ast/rename-variables-core (ast <sal-local-binds-expr>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-local-binds-ast/rename ast info env :local-decls :expr))

(define-method (sal-ast/rename-variables-core (ast <sal-for-all-definition>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-for-all-definition/rename ast info env))

(define-method (sal-ast/rename-variables-core (ast <sal-multi-command>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-local-binds-ast/rename ast info env :local-decls :command))

(define-method (sal-ast/rename-variables-core (ast <sal-multi-composition>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-local-binds-ast/rename ast info env :local-decls :module))

(define-method (sal-ast/rename-variables-core (ast <sal-with-module>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-local-binds-ast/rename ast info env :new-state-vars :module))

(define-method (sal-ast/rename-variables-core (ast <sal-multi-component-info>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-local-binds-ast/rename ast info env :local-decls :component))

(define-method (sal-ast/rename-variables-core (ast <sal-base-module>) (info <sal-var-rename-info>) (env <primitive>))
  (sal-base-module/rename ast info env))

(define-generic (sal-trace-info/rename info old->new))
(define-method (sal-trace-info/rename (info <sal-trace-info>) (old->new <primitive>))
  info)
(define-method (sal-trace-info/rename (info <sal-nested-trace-info>) (old->new <primitive>))
  (update-ast-slots info
                    :info (sal-trace-info/rename (slot-value info :info) old->new)))
(define-method (sal-trace-info/rename (info <sal-nested-list-trace-info>) (old->new <primitive>))
  (update-ast-slots info
                    :info-list (conservative-map-1 (cut sal-trace-info/rename <> old->new) (slot-value info :info-list))))
(define-method (sal-trace-info/rename (info <sal-multi-choice-trace-info>) (old->new <primitive>))
  (update-ast-slots info
                    :choice-var-names (conservative-map-1 (cut symbol-table/lookup old->new <>) (slot-value info :choice-var-names))
                    :info (sal-trace-info/rename (slot-value info :info) old->new)))
(define-method (sal-trace-info/rename (info <sal-choice-trace-info>) (old->new <primitive>))
  (update-ast-slots info
                    :choice-var-name (symbol-table/lookup old->new (slot-value info :choice-var-name))
                    :info-list (conservative-map-1 (cut sal-trace-info/rename <> old->new) (slot-value info :info-list))))
                                                           
(define (make-old->new ast new-ast)
  (let ((old-state-var-decls (slot-value ast :state-vars))
        (new-state-var-decls (slot-value new-ast :state-vars))
        (old->new (make-symbol-table)))
    (for-each (lambda (old-state-var-decl new-state-var-decl)
                (let ((old-state-var-name (sal-decl/name old-state-var-decl))
                      (new-state-var-name (sal-decl/name new-state-var-decl)))
                  (symbol-table/add! old->new old-state-var-name new-state-var-name)))
              old-state-var-decls new-state-var-decls)
    old->new))

(define (flat-module/rename-transition-trace-info! new-ast old->new)
  (when (slot-value new-ast :transition-trace-info)
    (set-slot-value! new-ast :transition-trace-info (sal-trace-info/rename (slot-value new-ast :transition-trace-info) old->new))))
  

(define-method (sal-ast/rename-variables-core (ast <sal-flat-module>) (info <sal-var-rename-info>) (env <primitive>))
  (let* ((new-ast (sal-flat-module/rename ast info env))
         (old->new (if (slot-value new-ast :transition-trace-info)
                     (make-old->new ast new-ast)
                     #unspecified)))
    (flat-module/rename-transition-trace-info! new-ast old->new)
    new-ast))

(define-method (sal-ast/rename-variables-core (ast <sal-esm-module>) (info <sal-var-rename-info>) (env <primitive>))
  (let* ((new-ast (sal-esm-module/rename ast info env))
         (old->new (if (slot-value new-ast :transition-trace-info)
                     (make-old->new ast new-ast)
                     #unspecified)))
    (flat-module/rename-transition-trace-info! new-ast old->new)
    new-ast))

(define-method (sal-ast/rename-variables-core (ast <sal-derived-flat-module>) (info <sal-var-rename-info>) (env <primitive>))
  (let* ((new-ast (sal-flat-module/rename ast info env))
         (old-state-var-decls (slot-value ast :state-vars))
         (new-state-var-decls (slot-value new-ast :state-vars))
         (old->new (make-old->new ast new-ast)))
    (flat-module/rename-transition-trace-info! new-ast old->new)
    (let ((new-traceability-table (make-eq-hash-table))
          (mapping (update-env* (make-empty-env) old-state-var-decls new-state-var-decls)))
      (eq-hash-table/for-each (lambda (original-var-decl old-var-decl-list)
                                (eq-hash-table/put! new-traceability-table
                                                    original-var-decl
                                                    (map (lambda (old-var-decl)
                                                           (cond ((lookup-env old-var-decl mapping) => identity)
                                                                 (else 
                                                                  ;; the variable was probably sliced keep it
                                                                  old-var-decl)))
                                                         old-var-decl-list)))
                              (slot-value ast :var-trace-info))
      (set-slot-value! new-ast :var-trace-info new-traceability-table)
      new-ast)))

(define-method (sal-ast/rename-variables-core (ast <sal-module-models>) (info <sal-var-rename-info>) (env <primitive>))
  (let* ((module (slot-value ast :module))
         (new-module (sal-ast/rename-variables-core module info env))
         (env (update-env* env (sal-module/state-variables module) (sal-module/state-variables new-module))))
    (update-ast-slots ast
                      :module new-module
                      :expr (rename-slot ast :expr info env))))

