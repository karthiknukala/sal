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

(module svc-interface
        (include "sal.sch")
        (import tmp-files)
        (export *svc-command*
                *sal-tmp-in-file-to-svc*
                *sal-tmp-out-file-to-svc*
                (sal/set-svc-in-tmp-file!)
                (sal/set-svc-out-tmp-file!)
                (sal/set-svc-command! cmd-name)
                (svc/execute  in-file))
        )

(define *svc-command* "svc")
(define *sal-tmp-in-file-to-svc* #f)
(define *sal-tmp-out-file-to-svc* #f)

(define-api (sal/set-svc-command! (cmd-name string?))
  (set! *svc-command* cmd-name))
  
(define (sal/set-svc-in-tmp-file!)
  (set! *sal-tmp-in-file-to-svc* (sal/setup-tmp-file! "input.svc")))

(define (sal/set-svc-out-tmp-file!)
  (set! *sal-tmp-out-file-to-svc* (sal/setup-tmp-file! "output.svc")))

(define (sign-error-executing-svc cmd)
  (sign-error "Error executing SVC. The following command was used to execute svc:\n~a\nIf this is not the correct command to execute SVC, it can be changed using the statement:\n\n  (sal/set-svc-command! \"<path-to-svc>/svc <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))

(define (svc/execute  in-file)
  (sal/set-svc-out-tmp-file!)
  (let* ((cmd (string-append *svc-command* " \"" in-file "\" > \"" *sal-tmp-out-file-to-svc* "\""))
         (_ (verbose-message 5 "  SVC command: ~a" cmd))
         (result (system cmd)))
    (unless (= result 0)
      (sign-error-executing-svc cmd))
    (when (grep/contains? *sal-tmp-out-file-to-svc* "error")
      (sign-error-executing-svc cmd))
    (unwind-protect
     (cond 
      ((grep/contains? *sal-tmp-out-file-to-svc* "invalid")
       #t)
      ((grep/contains? *sal-tmp-out-file-to-svc* "valid")
       #f)
      (else
       (sign-error "Unexpected output produced by SVC. The following command was used to execute svc:\n~a\nIf this is not the correct command to execute SVC, it can be changed using the statement:\n\n  (sal/set-svc-command! \"<path-to-svc>/svc <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-svc*))))

