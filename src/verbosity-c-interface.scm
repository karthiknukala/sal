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

(module verbosity-c-interface
        (include "sal.sch")
        (export (c-simple-verbose-message::int ::int ::string))
        (export (c-int-verbose-message::int ::int ::string ::int))
        (extern (export c-simple-verbose-message "c_simple_verbose_message"))
        (extern (export c-int-verbose-message "c_int_verbose_message"))
        )

(define (c-simple-verbose-message lvl msg)
  (verbose-message lvl msg)
  1)

(define (c-int-verbose-message lvl msg val)
  (verbose-message lvl msg val)
  1)
