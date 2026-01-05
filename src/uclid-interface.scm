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

(module uclid-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *uclid-command*
                *sal-tmp-in-file-to-uclid*
                *sal-tmp-out-file-to-uclid*
                (sal/set-uclid-in-tmp-file!)
                (sal/set-uclid-out-tmp-file!)
                (sal/set-uclid-command! cmd-name)
                (sal/set-uclid-boolean-mode! mode)
                (sal/set-uclid-sat-checker! checker)
                (uclid/execute  in-file))
        )

(define *uclid-command* "uclid")
(define *sal-tmp-in-file-to-uclid* #f)
(define *sal-tmp-out-file-to-uclid* #f)
(define *uclid-boolean-mode* 'sat)
(define *uclid-sat-checker* 'zchaff)

(define-api (sal/set-uclid-command! (cmd-name string?))
  (set! *uclid-command* cmd-name))
  
(define (sal/set-uclid-in-tmp-file!)
  (set! *sal-tmp-in-file-to-uclid* (sal/setup-tmp-file! "input.uclid")))

(define (sal/set-uclid-out-tmp-file!)
  (set! *sal-tmp-out-file-to-uclid* (sal/setup-tmp-file! "output.uclid")))

(define (sal/set-uclid-boolean-mode! mode)
  [assert (mode) (memq mode '(sat bdd))]
  (set! *uclid-boolean-mode* mode))

(define (sal/set-uclid-sat-checker! checker)
  [assert (checker) (memq checker '(zchaff chaff grasp))]
  (set! *uclid-sat-checker* checker))

(define (uclid/execute  in-file)
  (when (file-exists? "salenv_output.cnf_result")
    (delete-file "salenv_output.cnf_result"))
  (when (file-exists? "salenv_tmp_output.cnf_result")
    (delete-file "salenv_tmp_output.cnf_result"))
  (sal/set-uclid-out-tmp-file!)
  (let* ((cmd (string-append *uclid-command* " \"" in-file "\" " 
                             (object->string *uclid-boolean-mode*) " 0 " 
                             (object->string *uclid-sat-checker*) 
                             " > \"" *sal-tmp-out-file-to-uclid* "\""))
         (_ (verbose-message 5 "  UCLID command: ~a" cmd))
         (_ (status-message :executing-uclid))
         (result (display-runtime 3 "  UCLID execution time: ~a secs"
                   (lambda () 
                     (system cmd))
                   :uclid-time)))
    (unless (= result 0)
      (sign-error "Error executing UCLID. The following command was used to execute uclid:\n~a\nIf this is not the correct command to execute UCLID, it can be changed using the statement:\n\n  (sal/set-uclid-command! \"<path-to-uclid>/uclid\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (unwind-protect
     (cond
      ((grep/contains? *sal-tmp-out-file-to-uclid* "COUNTER-EXAMPLE GENERATED")
       #t)
      ((grep/contains? *sal-tmp-out-file-to-uclid* "Formula Valid")
       #f)
      (else
       (sign-error "Unexpected output produced by UCLID. The following command was used to execute uclid:\n~a\nIf this is not the correct command to execute UCLID, it can be changed using the statement:\n\n  (sal/set-uclid-command! \"<path-to-uclid>/uclid\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-uclid*))))

