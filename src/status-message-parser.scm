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

(module status-message-parser
        (include "sal.sch")
        (export (parse-status-message msg))
        )

(define *curr-key* #f)
(define *status-info-list* '())

(define (sign-invalid-status-message)
  ;; (warning-message "Invalid status message.")
  (set! *status-info-list* #f))

(define *status-message-lexer*
  (regular-grammar
   ()
   ((+ (in #\Space #\Tab #\newline))
    (ignore))
   ((: #\: alpha (* (or alpha digit #\-)))
    (when *curr-key*
      (push! *curr-key* *status-info-list*))
    (set! *curr-key* (the-keyword))
    (ignore))
   ((: alpha (* (or alpha digit #\-)))
    (cond
     (*curr-key*
      (push! (cons *curr-key* (the-symbol)) *status-info-list*)
      (set! *curr-key* #f)
      (ignore))
     (else
      (sign-invalid-status-message))))
   ((or (: #\" (* (: (* (out #\" #\\)) (? (: #\\ all)))) #\")
        (: #\' (* (: (* (out #\' #\\)) (? (: #\\ all)))) #\'))
    (cond
     (*curr-key*
      (push! (cons *curr-key* (the-substring 1 (- (the-length) 1))) *status-info-list*) 
      (set! *curr-key* #f)
      (ignore))
     (else
      (sign-invalid-status-message))))
   ((+ digit)
    (cond
     (*curr-key*
      (push! (cons *curr-key* (the-fixnum)) *status-info-list*)
      (set! *curr-key* #f)
      (ignore))
     (else
      (sign-invalid-status-message))))
   ((: (+ digit) (? (: #\. (+ digit))))
    (cond
     (*curr-key*
      (push! (cons *curr-key* (the-flonum)) *status-info-list*)
      (set! *curr-key* #f)
      (ignore))
     (else
      (sign-invalid-status-message))))
   ("#t"
    (cond
     (*curr-key*
      (push! (cons *curr-key* #t) *status-info-list*)
      (set! *curr-key* #f)
      (ignore))
     (else
      (sign-invalid-status-message))))
   ("#f"
    (cond
     (*curr-key*
      (push! (cons *curr-key* #f) *status-info-list*)
      (set! *curr-key* #f)
      (ignore))
     (else
      (sign-invalid-status-message))))
   (else
    (when *curr-key*
      (push! *curr-key* *status-info-list*))
    (unless (eof-object? (the-failure))
      (sign-invalid-status-message)))))

(define (parse-status-message msg)
  (dlet ((*curr-key* #f)
         (*status-info-list* '()))
    (let ((port (open-input-string msg)))
      (read/rp *status-message-lexer* port)
      *status-info-list*)))
