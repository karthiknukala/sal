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

(module fast-cache
        (import utility)
        (export (fast-cache/for-each cache proc)
                (fast-cache/size cache))
        )

(define (fast-cache/for-each cache proc)
  (let ((capacity (vector-length cache)))
    (let loop ((i 0))
      (when (< i capacity)
        (let ((val (vector-ref cache i)))
          (if val (proc val)))
        (loop (+ i 1))))))

(define (fast-cache/size cache)
  (let ((capacity (vector-length cache)))
    (let loop ((i 0)
               (result 0))
      (if (< i capacity)
        (let ((val (vector-ref cache i)))
          (loop (+ i 1)
                (+ result (if val 1 0))))
        result))))
  
