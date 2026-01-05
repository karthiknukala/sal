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

(directives (extern
             (type state-entry-channel (opaque) "void *")
             (SEC_make::state-entry-channel (::uint) "SEC_make")
             (SEC_get_entry_bit::int (::state-entry-channel ::uint) "SEC_get_entry_bit")
             (SEC_print_state_entry::void (::state-entry-channel) "SEC_print_state_entry")
             (SEC_get_num_bits::uint (::state-entry-channel) "SEC_get_num_bits")
             (SEC_input_reset::void (::state-entry-channel) "SEC_input_reset")
             (SEC_output_reset::void (::state-entry-channel) "SEC_output_reset")
             (SEC_add_bit::void (::state-entry-channel ::int) "SEC_add_bit")
             (SEC_add_num::void (::state-entry-channel ::uint ::uint) "SEC_add_num")
             (SEC_read_bit::int (::state-entry-channel) "SEC_read_bit")
             (SEC_read_num::uint (::state-entry-channel ::uint) "SEC_read_num")
             (SEC_equal::int (::state-entry-channel ::state-entry-channel) "SEC_equal")
             (SEC_copy::void (::state-entry-channel ::state-entry-channel) "SEC_copy")
             (SEC_make_copy::state-entry-channel (::state-entry-channel) "SEC_make_copy")))


