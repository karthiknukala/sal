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

(module sal-importer
        (include "sal.sch")
        (import sal-context)
        (export (make-context-name-importer ctx-ref actuals)
                (make-rename-importer renames importer)
                (make-add-prefix-importer prefix importer)
                (make-add-suffix-importer suffix importer))
        )

;;
;; Implements the support for the "import" statement.
;;
;; An importer is a function that receives four arguments:
;; - kind (symbol): specifies the kind of object we are
;;            trying to import (possible values: constant, type, module, assertion
;; - name (symbol): the name to be imported
;; - ctx : the context that will own the new node.
;; - place (sal-place): a place object (line and column info)

(define (make-context-name-importer ctx-ref actuals)
  [assert (ctx-ref) (instance-of? ctx-ref <sal-context>)]
  (lambda (kind name ctx place)
    (let ((decl (case kind
                  ((constant)
                   (sal-context/constant-declaration ctx-ref name))
                  ((type)
                   (sal-context/type-declaration ctx-ref name))
                  ((module)
                   (sal-context/module-declaration ctx-ref name))
                  ((assertion)
                   (sal-context/assertion-declaration ctx-ref name))
                  (else 
                   (trace 'importer "unknown kind ~a" kind)
                   (internal-error)))))
      (and decl
           (make-instance (sal-decl/name-class decl)
                          :context ctx :place place 
                          :decl decl 
                          :context-ref ctx-ref 
                          :actuals actuals)))))

(define (make-rename-importer renames importer)
  (let ((inv-renames (invert-mapping renames)))
    (lambda (kind id ctx place)
      ;; (print "request = " id)
      ;; (print "renames = " renames)
      ;; (print "inv-renames = " inv-renames)
      (cond
       ((assq id inv-renames) =>
        (lambda (pair)
          (importer kind (cdr pair) ctx place)))
       ((not (assq id renames))
        (importer kind id ctx place))
       (else
        #f)))))

(define (make-add-prefix-importer prefix importer)
  [assert (prefix) (symbol? prefix)]
  (let* ((prefix-str (to-string prefix))
         (prefix-len (string-length prefix-str)))
    (lambda (kind id ctx place)
      (let ((id-str (to-string id)))
        (and (substring=? prefix-str id-str prefix-len)
             (importer kind (string->symbol (substring id-str prefix-len (string-length id-str))) ctx place))))))

(define (make-add-suffix-importer suffix importer)
  [assert (suffix) (symbol? suffix)]
  (let* ((suffix-str (to-string suffix))
         (suffix-len (string-length suffix-str)))
    (lambda (kind id ctx place)
      (let* ((id-str (to-string id))
             (id-len (string-length id-str)))
        (and (< suffix-len id-len)
             (string=? suffix-str (substring id-str (- id-len suffix-len) id-len))
             (importer kind (string->symbol (substring id-str 0 (- id-len suffix-len))) ctx place))))))


