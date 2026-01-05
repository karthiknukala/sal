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

(module mathsat-interface
        (include "sal.sch")
        (import tmp-files)
        (export *mathsat-command*
                *sal-tmp-in-file-to-mathsat*
                *sal-tmp-out-file-to-mathsat*
                (sal/set-mathsat-in-tmp-file!)
                (sal/set-mathsat-out-tmp-file!)
                (sal/set-mathsat-command! cmd-name)
                (mathsat/execute  in-file))
        )

(define *mathsat-command* "mathsat")
(define *sal-tmp-in-file-to-mathsat* #f)
(define *sal-tmp-out-file-to-mathsat* #f)

(define-api (sal/set-mathsat-command! (cmd-name string?))
  (set! *mathsat-command* cmd-name))
  
(define (sal/set-mathsat-in-tmp-file!)
  (set! *sal-tmp-in-file-to-mathsat* (sal/setup-tmp-file! "input.mathsat")))

(define (sal/set-mathsat-out-tmp-file!)
  (set! *sal-tmp-out-file-to-mathsat* (sal/setup-tmp-file! "output.mathsat")))

(define (sign-error-executing-mathsat cmd)
  (sign-error "Error executing MATHSAT. The following command was used to execute mathsat:\n~a\nIf this is not the correct command to execute MATHSAT, it can be changed using the statement:\n\n  (sal/set-mathsat-command! \"<path-to-mathsat>/mathsat <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))

(define (mathsat/execute  in-file)
  (sal/set-mathsat-out-tmp-file!)
  (let* ((cmd (string-append *mathsat-command* " \"" in-file "\" > \"" *sal-tmp-out-file-to-mathsat* "\""))
         (_ (verbose-message 5 "  MATHSAT command: ~a" cmd))
         (result (system cmd)))
    (unless (= result 0)
      (sign-error-executing-mathsat cmd))
    (unwind-protect
     (cond 
      ((grep/contains? *sal-tmp-out-file-to-mathsat* "Result = 1")
       #t)
      ((grep/contains? *sal-tmp-out-file-to-mathsat* "Result = 0")
       #f)
      (else
       (sign-error "Unexpected output produced by MATHSAT. The following command was used to execute mathsat:\n~a\nIf this is not the correct command to execute MATHSAT, it can be changed using the statement:\n\n  (sal/set-mathsat-command! \"<path-to-mathsat>/mathsat <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-mathsat*))))

