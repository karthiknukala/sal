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

(module smt-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *smt-command*
                *sal-tmp-in-file-to-smt*
                *sal-tmp-out-file-to-smt*
                (sal/set-smt-out-tmp-file!)
                (sal/set-smt-command! cmd-name)
                (smt/execute in-file))
        )

(define *smt-command* "yices -v 2 -smt")
(define *sal-tmp-in-file-to-smt* #f)
(define *sal-tmp-out-file-to-smt* #f)

(define-api (sal/set-smt-command! (cmd-name string?))
  (set! *smt-command* cmd-name))
  
(define (sal/set-smt-out-tmp-file!)
  (set! *sal-tmp-out-file-to-smt* (sal/setup-tmp-file! "output.smt")))

(define (smt/execute in-file)
  (sal/set-smt-out-tmp-file!)
  (let* ((cmd (string-append *smt-command* " \"" in-file "\" > \"" *sal-tmp-out-file-to-smt* "\""))
         (_ (verbose-message 5 "  SMT command: ~a" cmd))
         (_ (status-message :executing-smt))
         (result (display-runtime 3 "  SMT execution time: ~a secs"
                   (lambda ()
                     (system cmd))
                   :smt-time)))
    (unless (= result 0)
      (sign-error "Executing SMT. The following command was used to execute SMT:\n~a\nIf this is not the correct command to execute SMT, it can be changed using the statement:\n\n  (sal/set-smt-command! \"<path-to-smt>/smt-solver\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (unwind-protect
     (cond
      ((grep/contains? *sal-tmp-out-file-to-smt* "unsat")
       #f)
      ((grep/contains? *sal-tmp-out-file-to-smt* "sat")
       #t)
      (else
       (sign-error "Unexpected output produced by SMT. The following command was used to execute SMT solver:\n~a\nIf this is not the correct command to execute SMT solver, it can be changed using the statement:\n\n  (sal/set-smt-command! \"<path-to-smt>/smt <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-smt*))))
