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

(module lsal-string-reader
        (include "sal.sch")
        (import sal-sxml-support sxml-to-sal-ast sal-string-reader sal-context sal-environment)
        (export <lsal-string-reader>
                (make-lsal-string-reader sal-env))
        )

(define-class <lsal-string-reader> (<sal-string-reader>) ())

(define (make-lsal-string-reader sal-env)
  [assert (sal-env) (instance-of? sal-env <sal-env>)]
  (make-instance <lsal-string-reader> 
                 :scratch-context (make-context sal-env 'scratch)
                 :sal-env sal-env))

(define-method (sal-string-reader/read-expr (reader <lsal-string-reader>) (str <string>))
  (let* ((str (string-append "top-expr " str))
         (sxml (lsal-string->sxml str)))
    (convert-top-expr (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-expr-using (reader <lsal-string-reader>) (str <string>) (sym-tab <primitive>))
  (let* ((str (string-append "top-expr " str))
         (sxml (lsal-string->sxml str)))
    (convert-expr (slot-value reader :scratch-context) sym-tab sxml)))

(define-method (sal-string-reader/read-type (reader <lsal-string-reader>) (str <string>))
  (let* ((str (string-append "top-type " str))
         (sxml (lsal-string->sxml str)))
    (convert-top-type (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-module (reader <lsal-string-reader>) (str <string>))
  (let* ((str (string-append "top-module " str))
         (sxml (lsal-string->sxml str)))
    (convert-top-module (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-assertion-name (reader <lsal-string-reader>) (str <string>))
  (let* ((str (string-append "top-assertion-name " str))
         (sxml (lsal-string->sxml str)))
    (convert-top-assertion-name (slot-value reader :scratch-context) sxml)))

(define-method (sal-string-reader/read-import (reader <lsal-string-reader>) (str <string>))
  (let* ((str (string-append "top-import (import " str ")"))
         (sxml (lsal-string->sxml str)))
    (process-import (slot-value reader :scratch-context) sxml)))

