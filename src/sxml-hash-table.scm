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

(module sxml-hash-table
        (include "fast-hash-table.macros")
        (import fast-hash-table sxml-package)
        (export (make-sxml-hash-table . initial-size-and-load-factor)
                (sxml-hash-table/rehash! ht)
                (sxml-hash-table/put! ht k v)
                (sxml-hash-table/contains? ht k)
                (sxml-hash-table/get ht k)
                (sxml-hash-table/delete! ht k)
                (sxml-hash-table/size ht))
        )

;; Defines a Mapping from SXML nodes to qualified name instances
(make-fast-hash-table-type sxml-hash-table sxml/hash sxml/equal?)

(define *sxml-hash-table-default-size* 1024)

(define *sxml-hash-table-default-load-balance* 0.8)

