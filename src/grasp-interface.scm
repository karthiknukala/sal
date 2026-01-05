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

(module grasp-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *grasp-command*
                *sal-tmp-in-file-to-grasp*
                *sal-tmp-out-file-to-grasp*
                (sal/set-grasp-in-tmp-file!)
                (sal/set-grasp-out-tmp-file!)
                (sal/set-grasp-command! cmd-name)
                (grasp/execute in-file))
        )

(define *grasp-command* "sat-grasp")
(define *sal-tmp-in-file-to-grasp* #f)
(define *sal-tmp-out-file-to-grasp* #f)

(define-api (sal/set-grasp-command! (cmd-name string?))
  (set! *grasp-command* cmd-name))
  
(define (sal/set-grasp-in-tmp-file!)
  (set! *sal-tmp-in-file-to-grasp* (sal/setup-tmp-file! "input.cnf")))

(define (sal/set-grasp-out-tmp-file!)
  (set! *sal-tmp-out-file-to-grasp* (sal/setup-tmp-file! "output.grasp")))

(define (grasp/execute in-file)
  (sal/set-grasp-out-tmp-file!)
  (let* ((cmd (string-append *grasp-command* " +V0 +O \"" in-file "\" > \"" *sal-tmp-out-file-to-grasp* "\""))
         (_ (verbose-message 5 "  GRASP command: ~a" cmd))
         (_ (status-message :executing-grasp))
         (result (display-runtime 3 "  GRASP execution time: ~a secs"
                   (lambda ()
                     (system cmd))
                   :grasp-time)))
    (unless (= result 0)
      (sign-error "Executing GRASP. The following command was used to execute GRASP:\n~a\nIf this is not the correct command to execute GRASP, it can be changed using the statement:\n\n  (sal/set-grasp-command! \"<path-to-grasp>/sat-grasp <extra-args>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (unwind-protect
     (with-input-from-file *sal-tmp-out-file-to-grasp*
       (lambda ()
         (grasp/filter-output)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-grasp*))))


(define (sign-unknown-grasp)
  (sign-error "GRASP produced an unexpected output."))

(define (grasp/filter-output)
  (try 
   (let loop ()
     (let ((curr-line (read-line)))
       (when (eof-object? curr-line)
         (sign-unknown-grasp))
       (string-case 
        curr-line
        ((: (* all) "UNSATISFIABLE" (* all))
         #f)
        ((: (* all) "Variable Assignments Satisfying" (* all))
         (let* ((line (read-line))
                (tmp-list (delimited-string->list '(#\space) line)))
           (map (lambda (str)
                  (let ((num (string->integer str)))
                    (when (= num 0)
                      (sign-unknown-grasp))
                    num))
                tmp-list)))
        (else
         (loop)))))
   (lambda (escape proc msg obj)
     (sign-unknown-grasp))))
        
