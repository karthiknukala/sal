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

(module simple-pretty-printer
        (include "utility.macros")
        (include "sxml-package.macros")
        (include "trace.macros")
        (import trace utility sxml-package sal-environment)
        )


(define (convert sxml demangle?)
  (sxml/match-or-fail sxml
    ((VARDECL (IDENTIFIER ?n) ?type)
     (if demangle?
       (sal/original-name n)
       n))
    (((or LABEL TYPENAME MODULENAME NAMEEXPR IDENTIFIER SCALARELEMENT) ?n)
     (cond 
      ((assq n *pp-id-table*) =>
       (lambda (pair)
         (pp/text (cdr pair))))
      (else 
       (if demangle
         (pp/text (sal/original-name n))
         (pp/text n)))))
        

