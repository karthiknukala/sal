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

(module tmp-files
        (extern 
         (get-pid::int () "sal_get_pid"))
        (import utility front-end)
        (export (sal/setup-tmp-file! basename)
                (sal/set-tmp-file-dir! dir-name)
                (sal/delete-tmp-file! file))
        )

(define *sal-tmp-file-dir* (os-tmp))

(define (sal/set-tmp-file-dir! dir-name)
  (set! *sal-tmp-file-dir* dir-name))

(define *sal-proc-id* (object->string (get-pid)))

(define *sal-tmp-file-prefix* (string-append "sal-" (let ((user (getenv "USER")))
                                                      (if (not (string? user))
                                                        "unknown"
                                                        user))
                                             "-" *sal-proc-id* "-"))

(define *sal-delete-tmp-files* #t)

(front-end/add-simple-option!
 "Misc" 
 "--preserve-tmp-files" 
 "Preserve (do not delete) temporary files. This option is useful for bug reporting."
 (lambda ()
   (set! *sal-delete-tmp-files* #f)))

(define (delete-tmp-files!)
  (when *sal-delete-tmp-files*
    (let ((cmd (string-append "rm -f " (make-file-name *sal-tmp-file-dir* *sal-tmp-file-prefix*) "*")))
      (system cmd))))

(define (sal/delete-tmp-file! file)
  (when *sal-delete-tmp-files*
    (system (string-append "rm -r -f \"" file "\""))))

(register-exit-function! 
 (lambda (status)
   (delete-tmp-files!)
   status))

(letrec ((intrhdl 
          (lambda (n)
            (exit 130))))
  (signal sigint intrhdl))

(define (sal/setup-tmp-file! basename)
  ;; check if temporary file exists
  (unless (file-exists? *sal-tmp-file-dir*)
    (sign-error "Temporary files directory `~a\` does not exist. Please you should create it, or specify a different one. If you want to specify a new temporary files directory, then you should add the following statement in your `.salrc' file in your home directory:\n\n  (sal/set-tmp-file-dir! \"<path-to-tmp-dir>\")\n\nFor instance, the following command sets `/homes/demoura/tmp` as your temporary files directory:\n\n (sal/set-tmp-file-dir! \"/homes/demoura/tmp\")" *sal-tmp-file-dir*))
  (let ((tmp-file-name (make-file-name *sal-tmp-file-dir* (string-append *sal-tmp-file-prefix* basename))))
    (let loop ((i 0))
      (let ((tmp-file-name (if (> i 0) (string-append tmp-file-name ".t" (object->string i)) tmp-file-name)))
        (try 
         (if (file-exists? tmp-file-name)
           (loop (+ i 1))
           (let ((p (open-output-file tmp-file-name)))
             (close-output-port p)
             tmp-file-name))
         (lambda (escape proc msg obj)
           (sign-error "Temporary file `~a` could not be created. Please check your access privileges, or specify a different temporary files directory. If you want to specify a new temporary files directory, then you should add the following statement in your `.salrc' file in your home directory:\n\n  (sal/set-tmp-file-dir! \"<path-to-tmp-dir>\")\n\nFor instance, the following command sets `/homes/demoura/tmp` as your temporary files directory:\n\n (sal/set-tmp-file-dir! \"/homes/demoura/tmp\")" tmp-file-name)))))))



