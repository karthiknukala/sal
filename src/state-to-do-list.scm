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

(module state-to-do-list
        (include "state-entry-channel.sch")
        (include "utility.macros")
        (extern
         (type state-to-do-list (opaque) "void *")
         (STDL_make::state-to-do-list (::uint ::int) "STDL_make")
         (STDL_empty::int (::state-to-do-list) "STDL_empty")
         (STDL_size::uint (::state-to-do-list) "STDL_size")
         (STDL_reset_input_channel::state-entry-channel (::state-to-do-list ::uint ::uint) "STDL_reset_input_channel")
         (STDL_reset_output_channel::state-entry-channel (::state-to-do-list ::uint) "STDL_reset_output_channel")
         (STDL_reset_input_channel_with_depth::state-entry-channel (::state-to-do-list ::uint ::uint ::uint) "STDL_reset_input_channel_with_depth")
         (STDL_reset_output_channel_with_depth::state-entry-channel (::state-to-do-list ::uint) "STDL_reset_output_channel_with_depth")
         (STDL_get_next_idx::uint (::state-to-do-list) "STDL_get_next_idx")
         (STDL_get_state_idx::uint (::state-to-do-list ::uint) "STDL_get_state_idx")
         (STDL_set_state_idx::void (::state-to-do-list ::uint ::uint) "STDL_set_state_idx")
         (STDL_get_parent_idx::uint (::state-to-do-list ::uint) "STDL_get_parent_idx")
         (STDL_get_depth::uint (::state-to-do-list ::uint) "STDL_get_depth")
         (STDL_insert::uint (::state-to-do-list ::state-entry-channel) "STDL_insert")
         (STDL_remove_top::uint (::state-to-do-list) "STDL_remove_top")
         (STDL_remove_front::uint (::state-to-do-list) "STDL_remove_front"))
        (export (make-state-to-do-list num-aux-bits . store-depth?)
                (stdl/empty? lst)
                (stdl/size lst)
                (stdl/reset-input-channel! lst state-idx parent-entry-idx)
                (stdl/reset-input-channel-with-depth! lst state-idx parent-entry-idx depth)
                (stdl/reset-output-channel! lst entry-idx)
                (stdl/next-idx lst)
                (stdl/state-idx lst entry-idx)
                (stdl/depth lst entry-idx)
                (stdl/set-state-idx! lst entry-idx state-idx)
                (stdl/parent-entry-idx lst entry-idx)
                (stdl/insert-channel! lst channel)
                (stdl/remove-top! lst)
                (stdl/top lst)
                (stdl/remove-front! lst))
        )

(define (make-state-to-do-list num-aux-bits . store-depth?)
  (let ((store-depth? (optional-arg store-depth? #f)))
    (STDL_make num-aux-bits (if store-depth? 1 0))))

(define (stdl/empty? lst)
  (= (STDL_empty lst) 1))

(define (stdl/size lst)
  (STDL_size lst))

(define (stdl/next-idx lst)
  (STDL_get_next_idx lst))

(define (stdl/top lst)
  (-fx (STDL_get_next_idx lst) 1))

(define (stdl/reset-input-channel! lst state-idx parent-entry-idx)
  (STDL_reset_input_channel lst state-idx parent-entry-idx))

(define (stdl/reset-output-channel! lst entry-idx)
  (STDL_reset_output_channel lst entry-idx))

(define (stdl/reset-input-channel-with-depth! lst state-idx parent-entry-idx depth)
  (STDL_reset_input_channel_with_depth lst state-idx parent-entry-idx depth))

(define (stdl/state-idx lst entry-idx)
  (STDL_get_state_idx lst entry-idx))

(define (stdl/set-state-idx! lst entry-idx state-idx)
  (STDL_set_state_idx lst entry-idx state-idx)
  #unspecified)

(define (stdl/parent-entry-idx lst entry-idx)
  (STDL_get_parent_idx lst entry-idx))

(define (stdl/depth lst entry-idx)
  (STDL_get_depth lst entry-idx))

(define (stdl/insert-channel! lst channel)
  (STDL_insert lst channel))

(define (stdl/remove-top! lst)
  (STDL_remove_top lst))

(define (stdl/remove-front! lst)
  (STDL_remove_front lst))

