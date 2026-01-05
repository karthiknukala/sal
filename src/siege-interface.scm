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

(module siege-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *siege-command*
                *sal-tmp-in-file-to-siege*
                (sal/set-siege-in-tmp-file!)
                (sal/set-siege-command! cmd-name)
                (siege/execute in-file))
        )

(define *siege-command* "siege_v4")

(define *siege-result-file* "siege.results")

(define *sal-tmp-in-file-to-siege* #f)

(define-api (sal/set-siege-command! (cmd-name string?))
  (set! *siege-command* cmd-name))
  
(define (sal/set-siege-in-tmp-file!)
  (set! *sal-tmp-in-file-to-siege* (sal/setup-tmp-file! "input.cnf")))

(define (siege/execute in-file)
  (when (file-exists? *siege-result-file*)
    (delete-file *siege-result-file*))
  (let* ((cmd (string-append *siege-command* " \"" in-file "\""))
         (_ (verbose-message 5 "  SIEGE command: ~a" cmd))
         (_ (status-message :executing-siege))
         (result (display-runtime 3 "  SIEGE execution time: ~a secs"
                   (lambda ()
                     (system cmd))
                   :siege-time)))
    (unless (= result 0)
      (sign-error "Executing SIEGE. The following command was used to execute SIEGE:\n~a\nIf this is not the correct command to execute SIEGE, it can be changed using the statement:\n\n  (sal/set-siege-command! \"<path-to-siege>/siege\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))
    (unless (file-exists? *siege-result-file*)
      (sign-error "Result file `~a' produced by SIEGE is missing. SAL was tested using SIEGE V4." *siege-result-file*))
    (unwind-protect
     (siege/filter-output in-file *siege-result-file*)
     (sal/delete-tmp-file! *siege-result-file*))))

(define (sign-unknown-siege)
  (sign-error "SIEGE produced an unexpected output. SAL was tested using SIEGE V4"))

(define (siege/filter-output in-file result-file)
  (with-input-from-file result-file
    (lambda ()
      (try
       (let ((curr-line (read-line))
             (in-file-len (string-length in-file)))
         (when (eof-object? curr-line)
           (sign-unknown-siege))
         (let ((curr-line-len (string-length curr-line)))
           (when (< curr-line-len (+ in-file-len 4))
             (sign-unknown-siege))
           (string-case
            curr-line
            ((: (* all) "unsatisfiable)")
             #f)
            (else
             (let* ((assignment-substring (substring curr-line (+ in-file-len 5) (- curr-line-len 3)))
                    (tmp-list (delimited-string->list '(#\space) assignment-substring)))
               (map (lambda (str)
                      (let ((num (string->integer str)))
                        (when (= num 0)
                          (sign-unknown-siege))
                        num))
                    tmp-list))))))
       (lambda (escape proc msg obj)
         (sign-unknown-siege))))))
        
