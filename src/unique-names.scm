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

(module unique-names
        (include "utility.sch")
        (import symbol-table)
        (export (gen-unique-name prefix . sep)
                (unique-name? name)
                (unique-name/prefix name)
                (unique-name/reset!))
        )

;;
;; unique names
;;
(define *unique-names* (make-symbol-table))

(define (unique-name? name)
  (getprop name 'sal-unique))

(define (unique-name/prefix name)
  (getprop name 'sal-unique))

(define (gen-unique-name prefix . sep)
  (let* ((sep (optional-arg sep '!))
         (prefix (or (unique-name/prefix prefix) prefix))
         (idx (cond 
               ((symbol-table/lookup *unique-names* prefix) =>
                identity)
               (else 0)))
         (next-idx (+ idx 1))
         (new-name (symbol-append prefix sep (string->symbol (integer->string next-idx)))))
    (symbol-table/add! *unique-names* prefix next-idx)
    (putprop! new-name 'sal-unique prefix)
    new-name))

(define (unique-name/reset!)
  (set! *unique-names* (make-symbol-table)))
          
