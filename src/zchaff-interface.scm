;;
;; SAL 3.1 Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
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

(module zchaff-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *zchaff-command*
                *sal-tmp-in-file-to-zchaff*
                *sal-tmp-out-file-to-zchaff*
                (sal/set-zchaff-in-tmp-file!)
                (sal/set-zchaff-out-tmp-file!)
                (sal/set-zchaff-command! cmd-name)
                (zchaff/execute in-file))
        )

(define *zchaff-command* "zchaff")
(define *sal-tmp-in-file-to-zchaff* #f)
(define *sal-tmp-out-file-to-zchaff* #f)

(define-api (sal/set-zchaff-command! (cmd-name string?))
  (set! *zchaff-command* cmd-name))
  
(define (sal/set-zchaff-in-tmp-file!)
  (set! *sal-tmp-in-file-to-zchaff* (sal/setup-tmp-file! "input.cnf")))

(define (sal/set-zchaff-out-tmp-file!)
  (set! *sal-tmp-out-file-to-zchaff* (sal/setup-tmp-file! "output.zchaff")))

(define (zchaff/execute in-file)
  (sal/set-zchaff-out-tmp-file!)
  (let* ((cmd (string-append *zchaff-command* " \"" in-file "\" > \"" *sal-tmp-out-file-to-zchaff* "\""))
         (_ (verbose-message 5 "  ZCHAFF command: ~a" cmd))
         (_ (status-message :executing-zchaff))
         (result (display-runtime 3 "  ZCHAFF execution time: ~a secs"
                   (lambda ()
                     (system cmd))
                   :zchaff-time)))
    (unless (= result 0)
      (sign-error "Executing ZCHAFF. The following command was used to execute ZCHAFF:\n~a\nIf this is not the correct command to execute ZCHAFF, it can be changed using the statement:\n\n  (sal/set-zchaff-command! \"<path-to-zchaff>/zchaff\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (unwind-protect
     (with-input-from-file *sal-tmp-out-file-to-zchaff*
       (lambda ()
         (zchaff/filter-output)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-zchaff*))))

  
(define (sign-unknown-zchaff)
  (sign-error "ZCHAFF produced an unexpected output. SALenv was tested using ZChaff 2001.2.17 and 2004.5.13"))

(define (zchaff/filter-output)
  (try
   (let loop ((z-chaff-header-found? #f))
     (let ((curr-line (read-line)))
       (when (eof-object? curr-line)
         (sign-unknown-zchaff))
       (string-case 
        (string-downcase curr-line)
        ((: "z-chaff version" (* all))
         (loop #t))
        ((: (* all) "instance satisfiable")
         (unless z-chaff-header-found?
           (sign-unknown-zchaff))
         (let* ((line (read-line))
                (tmp-list (delimited-string->list '(#\space) line)))
           (map-and-filter (lambda (str)
                             (let ((num (string->integer str)))
                               (and (not (= num 0)) ;; failed to read number
                                    num)))
                           tmp-list)))
        ((: (* all) "instance unsatisfiable")
         (unless z-chaff-header-found?
           (sign-unknown-zchaff))
         #f)
        (else 
         (loop z-chaff-header-found?)))))
   (lambda (escape proc msg obj)
     (sign-unknown-zchaff))))
        
