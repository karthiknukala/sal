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

(module sal-scm-obj-table
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import sal2scm-runtime gmp-scheme)
        (export (make-sal-scm-obj-table . initial-size-and-load-factor)
                (sal-scm-obj-table/rehash! ht)
                (sal-scm-obj-table/put! ht k v)
                (sal-scm-obj-table/contains? ht k)
                (sal-scm-obj-table/get ht k)
                (sal-scm-obj-table/delete! ht k)  
                (sal-scm-obj-table/delete-all! ht)
                (sal-scm-obj-table/size ht)
                (sal-scm-obj-table/for-each proc ht)
                (sal-scm-obj-table/for-each-key proc htable)
                (sal-scm-obj-table/fold-keys proc init htable)
                (sal-scm-obj-table/exists-key proc htable)
                (sal-scm-obj-table/for-all-keys proc htable)
                (sal-scm-obj-table/find-key proc htable))
        )

(make-fast-hash-table-type sal-scm-obj-table sal-scm/hash sal-scm/eq?)
