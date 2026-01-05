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

(module cvcl-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *cvcl-command*
                *cvc-command*
                *sal-tmp-in-file-to-cvcl*
                *sal-tmp-out-file-to-cvcl*
                (sal/set-cvcl-in-tmp-file!)
                (sal/set-cvcl-out-tmp-file!)
                (sal/set-cvcl-command! cmd-name)
                (sal/set-cvc-command! cmd-name)
                (cvcl/execute  in-file)
                (cvc/execute  in-file))
        )

(define *cvcl-command* "cvcl +sat")
(define *cvc-command* "cvc +sat") ;; legacy support for CVC
(define *sal-tmp-in-file-to-cvcl* #f)
(define *sal-tmp-out-file-to-cvcl* #f)

(define-api (sal/set-cvcl-command! (cmd-name string?))
  (set! *cvcl-command* cmd-name))

(define-api (sal/set-cvc-command! (cmd-name string?))
  (set! *cvc-command* cmd-name))
  
(define (sal/set-cvcl-in-tmp-file!)
  (set! *sal-tmp-in-file-to-cvcl* (sal/setup-tmp-file! "input.cvcl")))

(define (sal/set-cvcl-out-tmp-file!)
  (set! *sal-tmp-out-file-to-cvcl* (sal/setup-tmp-file! "output.cvcl")))

(define (cvcl/execute  in-file)
  (sal/set-cvcl-out-tmp-file!)
  (let* ((error-file (sal/setup-tmp-file! "error.cvc"))
         (cmd (string-append *cvcl-command* " < \"" in-file "\" > \"" *sal-tmp-out-file-to-cvcl* "\" 2> \"" error-file "\""))
         (_ (verbose-message 5 "  CVC Lite command: ~a" cmd))
         (_ (status-message :executing-cvcl))
         (result (display-runtime 3 "  CVC Lite execution time: ~a secs"
                   (lambda () 
                     (system cmd))
                   :cvcl-time)))
    (unless (and (= result 0)
                 (not (grep/contains? error-file "error")))
      (sign-error "Error executing CVC Lite. The following command was used to execute CVC Lite:\n~a\nIf this is not the correct command to execute CVC Lite, it can be changed using the statement:\n\n  (sal/set-cvcl-command! \"<path-to-cvcl>/cvcl <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (sal/delete-tmp-file! error-file)
    (unwind-protect
     (cond
      ((grep/contains? *sal-tmp-out-file-to-cvcl* "invalid")
       #t)
      ((grep/contains? *sal-tmp-out-file-to-cvcl* "valid")
       #f)
      (else
       (sign-error "Unexpected output produced by CVC Lite. The following command was used to execute CVC Lite:\n~a\nIf this is not the correct command to execute CVC Lite, it can be changed using the statement:\n\n  (sal/set-cvcl-command! \"<path-to-cvcl>/cvcl <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-cvcl*))))
  
(define (cvc/execute  in-file)
  (sal/set-cvcl-out-tmp-file!)
  (let* ((error-file (sal/setup-tmp-file! "error.cvc"))
         (cmd (string-append *cvc-command* " < \"" in-file "\" > \"" *sal-tmp-out-file-to-cvcl* "\" 2> \"" error-file "\""))
         (_ (verbose-message 5 "  CVC command: ~a" cmd))
         (_ (status-message :executing-cvc))
         (result (display-runtime 3 "  CVC execution time: ~a secs"
                   (lambda ()
                     (system cmd))
                   :cvc-time)))
    (unless (and (= result 0)
                 (not (grep/contains? error-file "error")))
      (sign-error "Error executing CVC. The following command was used to execute CVC:\n~a\nIf this is not the correct command to execute CVC, it can be changed using the statement:\n\n  (sal/set-cvc-command! \"<path-to-cvc>/cvc <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (sal/delete-tmp-file! error-file)
    (unwind-protect
     (cond
      ((grep/contains? *sal-tmp-out-file-to-cvcl* "invalid")
       #t)
      ((grep/contains? *sal-tmp-out-file-to-cvcl* "valid")
       #f)
      (else
       (sign-error "Unexpected output produced by CVC. The following command was used to execute CVC:\n~a\nIf this is not the correct command to execute CVC, it can be changed using the statement:\n\n  (sal/set-cvc-command! \"<path-to-cvc>/cvc <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-cvcl*))))


