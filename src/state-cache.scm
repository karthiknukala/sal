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

(module state-cache
        (include "sal.sch")
        (import state-entry-channel front-end)
        (extern
         (type state-cache (opaque) "void *")
         (type int-array (array int) "int *")
         (SC_make::state-cache (::uint) "SC_make")
         (SC_reset_input_channel::state-entry-channel (::state-cache) "SC_reset_input_channel")
         (SC_reset_output_channel::state-entry-channel (::state-cache ::uint) "SC_reset_output_channel")
         (SC_insert_channel::int (::state-cache ::state-entry-channel) "SC_insert_channel")
         (SC_size::int (::state-cache) "SC_size")
         (SC_set_load_balance::void (::uint) "SC_set_load_balance")
         (SC_set_initial_cache_capacity::void (::uint) "SC_set_initial_cache_capacity")
         (SC_get_num_colisions::uint () "SC_get_num_colisions")
         (export sc/idx-notification-handler "sc_idx_notification_handler"))
        (export
         (sc/set-load-balance! val)
         (sc/set-initial-cache-capacity! val)
         (sc/num-collisions)
         (make-state-cache num-bits num-externals)
         (sc/reset-input-channel! cache)
         (sc/reset-output-channel! cache idx)
         (sc/insert-channel! cache channel proc)
         (sc/size cache)
         (sc/idx-notification-handler ::int-array ::uint))
        )

(define-api (sc/set-load-balance! val)
  :doc "Set the load balance for the table used to cache states."
  (unless (and (> val 0) (<= val 100))
    (sign-error "Invalid cache table load balace. It must be a value in (0,100]."))
  (SC_set_load_balance val)
  #unspecified)

(define-api (sc/set-initial-cache-capacity! val)
  :doc "Set initial cache capacity (number of states)."
  [sal-assert "sc/set-load-balance!" (val) (> val 0)]
  (SC_set_initial_cache_capacity (closest-greater-power-of-2 val))
  #unspecified)

(front-end/add-full-option!
 "Explicit State"
 "-t <num>"
 "--cache-table-size=<num>"
 "Set the initial size of the state cache table (default:2^16 states)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sc/set-initial-cache-capacity! arg))))

(front-end/add-full-option!
 "Explicit State"
 "-cl <num>"
 "--cache-table-load-balance=<num>"
 "Set the load balance for the state cache table (default: 70%). It must be a value in (0, 100]. Small (big) values increase (decrease) performance and memory usuage."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sc/set-load-balance! arg))))


(define-api (sc/num-collisions)
  :doc "Return the number of collisions in the state cache."
  (SC_get_num_colisions))

(define *curr-idx-mapping* #f)
(define *curr-idx-mapping-size* #f)

(define (sc/idx-notification-handler array size)
  (set! *curr-idx-mapping* array)
  (set! *curr-idx-mapping-size* size))
  
(define (make-state-cache num-bits num-externals)
  (SC_make (+ num-bits 
              (* num-externals (sec/obj-num-bits)))))

(define (sc/reset-input-channel! cache)
  (SC_reset_input_channel cache))

(define (sc/reset-output-channel! cache idx)
  (SC_reset_output_channel cache idx))

(define (sc/insert-channel! cache channel proc)
  [assert (*curr-idx-mapping*) (not *curr-idx-mapping*)]
  (let ((result (SC_insert_channel cache channel)))
    (when *curr-idx-mapping*
      (proc (lambda (curr-idx)
              (int-array-ref *curr-idx-mapping* curr-idx)))
      (set! *curr-idx-mapping* #f))
;     (display "inserting: ")
;     (sec/print-entry channel)
;     (print "")
;     (print "result : " result)
    (if (< result 0)
      #f
      result)))

(define (sc/size cache)
  (SC_size cache))
