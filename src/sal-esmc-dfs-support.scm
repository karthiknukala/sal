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

(module sal-esmc-dfs-support
        (include "sal.sch")
        (import state-to-do-list)
        (export (sal-esmc/pop-states! to-do-stack depth))
        )

;; return a new depth
(define (sal-esmc/pop-states! to-do-stack depth)
  (let loop ((idx (stdl/top to-do-stack)))
    [assert (idx) (> idx 0)]
    (let ((parent-idx (stdl/parent-entry-idx to-do-stack idx)))
      ;; (print "idx: " idx " parent-idx: " parent-idx)
      (stdl/remove-top! to-do-stack)
      (when (and (= parent-idx (- idx 1)) (> parent-idx 0))
        (set! depth (- depth 1))
        (loop parent-idx))))
  depth)

