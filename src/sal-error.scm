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

(module sal-error
        (include "utility.sch")
        (include "trace.sch")
        (import sal-ast-support sal-parser-utility sxml-package xformat)
        (export (format-with-location node msg . args)
                (sign-sal-error kind node error-msg arg-list)
                (sign-source-error node error-msg . args)
                (sign-unsupported-feature node error-msg . args)
                (sal/source-error? kind))
        )

(define (format-with-location node msg . args)
  (multiple-value-bind
      (ctx-name place)
      (if (sxml-node? node)
        (values (sxml/attribute node 'CONTEXT-NAME)
                (sxml/attribute node 'PLACE))
        (values (sal-ast/context-name node)
                (sal-ast/place node)))
    (let* ((line (if place 
                   (sal-place/initial-line place)
                   #f))
           (column (if place
                     (sal-place/initial-column place)
                     #f))
           (msg (if (equal? msg "") msg (string-append ": " msg)))
           (result-msg (cond ((and ctx-name line column)
                              (apply xformat (cons* #f (string-append "[Context: ~a, line(~d), column(~d)]" msg) ctx-name line column args)))
                             ((and ctx-name line)
                              (apply xformat (cons* #f (string-append "[Context: ~a, line(~d)]" msg) ctx-name line args)))
                             ((and line column)
                              (apply xformat (cons* #f (string-append "[line(~d), column(~d)]" msg) line column args)))
                             (line
                              (apply xformat (cons* #f (string-append "[line(~d)]" msg) line args)))
                             (ctx-name
                              (apply xformat (cons* #f (string-append "[Context: ~a]" msg) ctx-name args)))
                             (else 
                              (apply xformat (cons* #f msg args))))))
      result-msg)))

;###
; Generate an error message with context and line information.
; The context name and line number information used in the error message are extracted from
; @code{node}. If this AST or SXML node does not provied such information, the
; line number and the context name will not be printed. 
; @code{error-msg} is the template error message that will be instantiated
; with arguments @code{args}.
; @code{kind} is a symbol which describes the error category.
; @lisp
; (sign-sal-error 'sal-source-error
;                  sxml-node 
;                 "Invalid variable name \"~a\"" 
;                 var-name)
; @end lisp
(define (sign-sal-error kind node error-msg arg-list)
  (with-output-to-trace 
   'error
   (print "\"" kind "\" Error detected.")
   (display-circle node)
   (print ""))
  (error kind (apply format-with-location node error-msg arg-list) #unspecified))

;###
; Generate an error that signs an invalid SAL specification.
; This functions uses the function @code{sign-sal-error}.
; It defines the category @code{'sal-source-error}.
(define (sign-source-error node error-msg . args)
  (sign-sal-error 'sal-source-error node error-msg args))

(register-app-error! 'sal-source-error)

;###
; Generate an error that signs the use of an unsupported SAL feature.
; This functions uses the function @code{sign-sal-error}.
; It defines the category @code{'sal-unsupported}.
(define (sign-unsupported-feature node error-msg . args)
  (sign-sal-error 'sal-unsupported node error-msg args))

(register-app-error! 'sal-unsupported)

;###
; Return true, if @code{kind} is a @code{'sal-source-error}.
(define (sal/source-error? kind)
  (eq? kind 'sal-source-error))

