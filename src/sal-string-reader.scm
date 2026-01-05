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

(module sal-string-reader
        (include "sal.sch")
        (import sal-sxml-support sxml-to-sal-ast sal-context sal-environment
                sal-parser)
        (export <sal-string-reader>
                (sal-string-reader/read-expr reader expr)
                (sal-string-reader/read-expr-using reader expr sym-tab)
                (sal-string-reader/read-type reader type)
                (sal-string-reader/read-module reader module)
                (sal-string-reader/read-import reader import)
                (sal-string-reader/read-assertion-name reader assertion)
                (sal-string-reader/scratch-context reader)
                (make-sal-string-reader sal-env))
        )

(define-class <sal-string-reader> () (:scratch-context :sal-env))

(define-macro (protected-string-parse str kind)
  `(try
    (call-next-method)
    (lambda (e p m o)
      (if (memq p '(sal-source-error parser))
        (sign-error "Processing the ~a `~a'. Reason: ~a" ,kind ,str m)
        (error p m o)))))

(define-generic (sal-string-reader/read-expr reader expr))
(define-generic (sal-string-reader/read-expr-using reader expr sym-tab))
(define-generic (sal-string-reader/read-type reader type))
(define-generic (sal-string-reader/read-module reader module))
(define-generic (sal-string-reader/read-import reader import))
(define-generic (sal-string-reader/read-assertion-name reader assertion))
(define-generic (sal-string-reader/scratch-context reader))
(define-method (sal-string-reader/scratch-context (reader <sal-string-reader>))
  (slot-value reader :scratch-context))

(define-method (sal-string-reader/read-expr :around (reader <sal-string-reader>) (str <string>))
  (protected-string-parse str "expression"))
(define-method (sal-string-reader/read-expr-using :around (reader <sal-string-reader>) (str <string>) (sym-tab <primitive>))
  (protected-string-parse str "expression"))
(define-method (sal-string-reader/read-type :around (reader <sal-string-reader>) (str <string>))
  (protected-string-parse str "type"))
(define-method (sal-string-reader/read-module :around (reader <sal-string-reader>) (str <string>))
  (protected-string-parse str "module"))
(define-method (sal-string-reader/read-import :around (reader <sal-string-reader>) (str <string>))
  (protected-string-parse str "import clause"))
(define-method (sal-string-reader/read-assertion-name :around (reader <sal-string-reader>) (str <string>))
  (protected-string-parse str "assertion"))

(define (make-sal-string-reader sal-env)
  [assert (sal-env) (instance-of? sal-env <sal-env>)]
  (make-instance <sal-string-reader> 
                 :scratch-context (make-context sal-env 'scratch)
                 :sal-env sal-env))

(define (parse-str str reader)
  (sal/sxml-preprocessor (sal-parser/parse-string str (lambda (ctx-id) (sal-env/context (slot-value reader :sal-env) ctx-id)))))
  
(define-method (sal-string-reader/read-expr (reader <sal-string-reader>) (str <string>))
  (let* ((str (string-append "@BTE@ " str))
         (sxml (parse-str str reader)))
    (convert-top-expr (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-expr-using (reader <sal-string-reader>) (str <string>) (sym-tab <primitive>))
  (let* ((str (string-append "@BTE@ " str))
         (sxml (parse-str str reader)))
    (convert-expr (slot-value reader :scratch-context) sym-tab sxml)))

(define-method (sal-string-reader/read-type (reader <sal-string-reader>) (str <string>))
  (let* ((str (string-append "@BTT@ " str))
         (sxml (parse-str str reader)))
    (convert-top-type (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-module (reader <sal-string-reader>) (str <string>))
  (let* ((str (string-append "@BTM@ " str))
         (sxml (parse-str str reader)))
    (convert-top-module (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-assertion-name (reader <sal-string-reader>) (str <string>))
  (let* ((str (string-append "@BTA@ " str))
         (sxml (parse-str str reader)))
    (convert-top-assertion-name (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-import (reader <sal-string-reader>) (str <string>))
  (print str)
  (let* ((str (string-append "@BTI@ IMPORTING " str))
         (sxml (parse-str str reader)))
    (process-import (slot-value reader :scratch-context) sxml)))
