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

(define (opt-info/class-info name)
  #f)

(define (opt-class-info/class-idx info)
  (error 'opt-class-info/class-idx "Not available" #f))

(define (opt-class-info/slot->idx info slot-name)
  (error 'opt-class-info/slot->idx "Not available" #f))

(define (opt-info/generic-info name)
  #f)

(define (opt-generic-info/idx info) 
  (error 'opt-generic-info/idx "Not available" #f))

(define (opt-generic-info/discriminator-pos-list info)
  (error 'opt-generic-info/discriminator-pos-list "Not available" #f))

(define (opt-info/num-classes) 0)
(define (opt-info/num-generics) 0)
