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

(module berkmin-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *berkmin-command*
                *sal-tmp-in-file-to-berkmin*
                (sal/set-berkmin-in-tmp-file!)
                (sal/set-berkmin-command! cmd-name)
                (berkmin/execute in-file))
        )

(define *berkmin-command* "BerkMin561-linux")

(define *sal-tmp-in-file-to-berkmin* #f)

(define-api (sal/set-berkmin-command! (cmd-name string?))
  (set! *berkmin-command* cmd-name))
  
(define (sal/set-berkmin-in-tmp-file!)
  (set! *sal-tmp-in-file-to-berkmin* (sal/setup-tmp-file! "input.cnf")))

(define (berkmin/execute in-file)
  (status-message :executing-berkmin)
  (verbose-message 5 "  BerkMin command: ~a ~a" *berkmin-command* in-file)
   (let ((proc (run-process *berkmin-command* in-file output: pipe:)))
     (unwind-protect
      (berkmin/filter-output proc)
      (process-kill proc))))

(define (sign-unknown-berkmin)
  (sign-error "BerkMin produced an unexpected output. SAL was tested using BerkMin561."))

(define (check-status proc)
  (unless (process-alive? proc)
    (let ((status (process-exit-status proc)))
      (when status
        (unless (= status 0)
          (sign-error "Executing BerkMin. The following command was used to execute BerkMin:\n~a\nIf this is not the correct command to execute BerkMin, it can be changed using the statement:\n\n  (sal/set-berkmin-command! \"<path-to-berkmin>/<berkmin-executable>\")\n\nThis statement should be included in your `.salrc' file in your home directory." *berkmin-command*))))))


(define (berkmin/filter-output proc)
  (check-status proc)
  (let ((port (process-output-port proc)))
    (let loop ()
      (let ((curr-line (try (read-line port)
                            (lambda (escape proc msg obj)
                              (sign-unknown-berkmin)))))
        (check-status proc)
        (when (eof-object? curr-line)
          (sign-unknown-berkmin))
        (string-case 
         curr-line
         ((bol (: "SOFTWARE LICENSE AGREEMENT" (* all)))
          (sign-error "BerkMin is requesting the license file (license.txt). The license file should be in the current directory. You can generate the license file by executing BerkMin without arguments in the current directory. The following command was used to execute BerkMin:\n~a" *berkmin-command*))
         ((bol (: "UNSATISFIABLE" (* all)))
          #f)
         ((bol (: "solution = " (* all)))
          (let* ((assignment (substring curr-line 11 (string-length curr-line)))
                 (tmp-list (delimited-string->list '(#\space) assignment)))
            (map (lambda (str)
                   (let ((num (string->integer str)))
                     (when (= num 0)
                       (sign-unknown-berkmin))
                     num))
                 tmp-list)))
         (else 
          (loop)))))))
  
