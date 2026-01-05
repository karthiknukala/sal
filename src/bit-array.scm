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

(module bit-array
        (include "utility.macros")
        (extern
         (type bitarray (opaque) "void *")
         (BA_make::bitarray (::int) "BA_make")
         (BA_zero_array::int (::bitarray) "BA_zero_array")
         (BA_copy::bitarray (::bitarray) "BA_copy")
         (BA_get_num_bits::int (::bitarray) "BA_get_num_bits")
         (BA_get_size::int (::bitarray) "BA_get_size")
         (BA_get_bit::int (::bitarray ::int) "BA_get_bit")
         (BA_set_bit::void (::bitarray ::int) "BA_set_bit")
         (BA_reset_bit::void (::bitarray ::int) "BA_reset_bit")
         (BA_append0::void (::bitarray) "BA_append0")
         (BA_append1::void (::bitarray) "BA_append1")
         (BA_reset::void (::bitarray) "BA_append1")
         (BA_next::int (::bitarray) "BA_next")
         (BA_get_next::bitarray (::bitarray) "BA_get_next"))
        (export 
         (make-bit-array . size)
         (bit-array/empty? bit-array)
         (bit-array/copy source)
         (bit-array/num-bits bit-array)
         (bit-array/size bit-array)
         (bit-array/get-bit bit-array pos)
         (bit-array/set-bit! bit-array pos)
         (bit-array/set! bit-array pos val)
         (bit-array/reset-bit! bit-array pos)
         (bit-array/append-0! bit-array)
         (bit-array/append-1! bit-array)
         (bit-array/next! bit-array)
         (bit-array/get-next! bit-array)
         (bit-array->string bit-array)
         (bit-array->vector bit-array)
         (bit-array->list bit-array)
         (vector->bit-array vector)
         (list->bit-array alist))
        )

(define *bit-array-default-size* 4)

(define (make-bit-array . size)
  (let ((size (optional-arg size *bit-array-default-size*)))
    (BA_make size)))

(define (bit-array/empty? bit-array)
  (= (BA_zero_array bit-array) 1))

(define (bit-array/copy source)
  (BA_copy source))

(define (bit-array/num-bits bit-array)
  (BA_get_num_bits bit-array))

(define (bit-array/size bit-array)
  (BA_get_size bit-array))

(define (bit-array/get-bit bit-array pos)
  (= (BA_get_bit bit-array pos) 1))
   
(define (bit-array/set-bit! bit-array pos)
  (BA_set_bit bit-array pos)
  #unspecified)

(define (bit-array/reset-bit! bit-array pos)
  (BA_reset_bit bit-array pos)
  #unspecified)

(define (bit-array/set! bit-array pos val)
  (if val
    (BA_set_bit bit-array pos)
    (BA_reset_bit bit-array pos))
  #unspecified)
    
(define (bit-array/append-0! bit-array)
  (BA_append0 bit-array)
  #unspecified)

(define (bit-array/append-1! bit-array)
  (BA_append1 bit-array)
  #unspecified)

(define (bit-array/next! bit-array)
  (= (BA_next bit-array) 1))

(define (bit-array/get-next! bit-array)
  (let ((result (BA_get_next bit-array)))
    (if (void*-null? bit-array)
      #f
      result)))

(define (bit-array->string bit-array)
  (with-output-to-string
    (lambda ()
      (let ((size (bit-array/num-bits bit-array)))
        (let loop ((pos 0))
          (when (< pos size)
            (display (bit-array/get-bit bit-array pos))
            (loop (+ pos 1))))))))
      
(define (bit-array->vector bit-array)
  (let* ((size (bit-array/num-bits bit-array))
         (result (make-vector size)))
    (let loop ((pos 0))
      (cond
       ((< pos size)
        (vector-set! result pos (bit-array/get-bit bit-array pos))
        (loop (+ pos 1)))
       (else result)))))

(define (bit-array->list bit-array)
  (vector->list (bit-array->vector bit-array)))

(define (vector->bit-array vector)
  (let ((result (make-bit-array))
        (size (vector-length vector)))
    (let loop ((pos 0))
      (cond
       ((< pos size)
        (if (vector-ref vector pos)
          (bit-array/append-1! result)
          (bit-array/append-0! result))
        (loop (+ pos 1)))
       (else result)))))

(define (list->bit-array lst)
  (let ((result (make-bit-array)))
    (let loop ((lst lst))
      (cond
       ((null? lst)
        result)
       ((car lst)
        (bit-array/append-1! result)
        (loop (cdr lst)))
       (else
        (bit-array/append-0! result)
        (loop (cdr lst)))))))
    
      
        

