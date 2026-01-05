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

(module state-entry-channel
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (include "state-entry-channel.sch")
        (import gmp-scheme utility sal2scm-runtime front-end sal-scm-obj-table)
        (export 
         (sec/set-obj-num-bits! num)
         (sec/obj-num-bits)
         (make-state-entry-channel num-bits num-entries)
         (sec/num-bits channel)
         (sec/get-bit channel pos)
         (sec/display-entry channel)
         (sec/input-reset! channel)
         (sec/output-reset! channel)
         (sec/add-bit! channel bit?)
         (sec/add-num! channel val num-bits)
         (sec/add-obj! channel obj)
         (sec/read-bit! channel)
         (sec/read-num! channel num-bits)
         (sec/read-obj! channel)
         (sec/write-to-file! binary-port channel)
         (sec/read-from-file! binary-port channel)
         (sec/equal? channel1 channel2)
         (sec/copy! target-channel source-channel)
         (sec/copy source-channel))
        )

(define *sec-obj-num-bits* 20)
(define *sec-num-max-objs* (expt 2 *sec-obj-num-bits*))

(define (sec/obj-num-bits)
  *sec-obj-num-bits*)

(define-api (sec/set-obj-num-bits! num)
  [assert (num) (and (number? num) (>fx num 0) (<fx num 30))]
  (set! *sec-obj-num-bits* num)
  (set! *sec-num-max-objs* (expt 2 *sec-obj-num-bits*)))

(front-end/add-full-option!
 "Explicit State"
 "-ot <num>"
 "--obj-table-size=<num>"
 "Set the size of the object table (default:2^20). The object table is used to store values of infinite types such as: recursive datatypes and multi precision numbers."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sec/set-obj-num-bits! (mpq/num-bits arg)))))

(define *sec-obj-table* (make-sal-scm-obj-table))
(define *sec-inv-obj-table* (make-eq-hash-table))

(define *sec-obj-next-idx* 0)
(define (sec/reset-obj-table!)
  (set! *sec-obj-table* (make-sal-scm-obj-table))
  (set! *sec-inv-obj-table* (make-eq-hash-table))
  (set! *sec-obj-next-idx* 0))

(define (sec/obj->idx obj)
  (cond
   ((sal-scm-obj-table/get *sec-obj-table* obj) =>
    cdr)
   (else
    (let ((result *sec-obj-next-idx*))
      (set! *sec-obj-next-idx* (+ *sec-obj-next-idx* 1))
      (unless (< result *sec-num-max-objs*)
        (sign-error "Object table exhausted. Use option --obj-table-size=<nun> to increase the capacity of the table."))
      (sal-scm-obj-table/put! *sec-obj-table* obj result)
      (eq-hash-table/put! *sec-inv-obj-table* result obj)
      result))))

(define (sec/idx->obj idx)
  (let ((obj (eq-hash-table/get *sec-inv-obj-table* idx)))
    [assert (obj) obj]
    (cdr obj)))

(define (make-state-entry-channel num-bits num-entries)
  (SEC_make (+ num-bits (* num-entries *sec-obj-num-bits*))))

(define (sec/num-bits channel)
  (SEC_get_num_bits channel))

(define (sec/get-bit channel pos)
  (= (SEC_get_entry_bit channel pos) 1))

(define (sec/display-entry channel)
  (SEC_print_state_entry channel)
  #unspecified)

(define (sec/input-reset! channel)
  (SEC_input_reset channel)
  #unspecified)

(define (sec/output-reset! channel)
  (SEC_output_reset channel)
  #unspecified)

(define (sec/add-bit! channel bit?)
  (SEC_add_bit channel (if bit? 1 0))
  #unspecified)

(define (sec/add-num! channel val num-bits)
  (SEC_add_num channel val num-bits)
  #unspecified)

(define (sec/add-obj! channel obj)
  (SEC_add_num channel (sec/obj->idx obj) *sec-obj-num-bits*)
  #unspecified)

(define (sec/read-bit! channel)
  (= (SEC_read_bit channel) 1))

(define (sec/read-num! channel num-bits)
  (SEC_read_num channel num-bits))

(define (sec/read-obj! channel)
  (sec/idx->obj (SEC_read_num channel *sec-obj-num-bits*)))

(define (sec/write-to-file! binary-port channel)
  (let* ((num-bits (sec/num-bits channel))
         (_ [assert (num-bits) (>= num-bits 0)])
         (num-shorts (+ (/fx num-bits 16) (if (> (remainder num-bits 16) 0) 1 0))))
    (sec/output-reset! channel)
    (let loop ((idx 0))
      (when (< idx num-shorts)
        (output-obj binary-port (sec/read-num! channel 16))
        (loop (+ idx 1))))))

(define (sec/read-from-file! binary-port channel)
  (let* ((num-bits (sec/num-bits channel))
         (_ [assert (num-bits) (>= num-bits 0)])
         (num-shorts (+ (/fx num-bits 16) (if (> (remainder num-bits 16) 0) 1 0))))
    (sec/input-reset! channel)
    (let loop ((idx 0))
      (when (< idx num-shorts)
        (sec/add-num! channel (input-obj binary-port) 16)
        (loop (+ idx 1))))
    (sec/output-reset! channel)))

(define (sec/equal? channel1 channel2)
  (= (SEC_equal channel1 channel2) 1))

(define (sec/copy! target-channel source-channel)
  (SEC_copy target-channel source-channel)
  #unspecified)

(define (sec/copy source-channel)
  (SEC_make_copy source-channel))
