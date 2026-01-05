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

(module compile-and-load
        (include "sal.sch")
        (import tmp-files runtime sal-environment front-end)
        (export (set-optimization-level! n)
                (compile-and-load definitions . lib-file-name))
        )

(define *optimization-level* 1)

(define (set-optimization-level! n)
  (set! *optimization-level* n))

(front-end/add-full-option!
 "Dynamic Compilation"
 "-opt <num>"
 "--optimization-level=<num>"
 "Set code optimization level for dynamic compiled code (default: 1). Level 0: includes debugging information, Level 1: optimize, Level 2: more expensive optimization, Level 3: unsafe optimizations (they produce segmentation faults if the program has a type error)."
 (front-end-adapter/nat-arg 
  (lambda (arg)
    (set-optimization-level! arg))))

(define (generate-code-file definitions)
  (let ((file-name (string-append (sal/setup-tmp-file! "action-code") ".scm"))
        (sal-src-dir (sal/src-directory)))
    (with-output-to-file file-name
      (lambda ()
        (print "(module " (prefix (basename file-name)))
        (print "(library sal)")
        (print "  (export ")
        (for-each (lambda (def)
                    (print "     " (cadr def)))
                  definitions)
        (print "      )")
        (print "  )")
        (print "")
        (for-each (lambda (def)
                    (pp def))
                  definitions)))
    file-name))

(define (compile-file file-name lib-file-name)
  (let* ((obj-file-name (string-append (prefix file-name) ".o"))
         (lib-file-name (if lib-file-name lib-file-name (string-append (prefix file-name) ".so")))
         (sal-src-dir (sal/src-directory))
         (comp-cmd (string-append "sal-sc --dynamic -O" (number->string *optimization-level*) " -o \"" obj-file-name "\" \"" file-name "\""))
         (link-cmd (string-append "sal-sld --shared -o \"" lib-file-name "\" \"" obj-file-name "\"")))
    (status-message :compiling obj-file-name)
    (verbose-message 2 "  generating object file '~a'..." obj-file-name)
    (verbose-message 3 "    executing command: ~a" comp-cmd)
    (let ((result (display-runtime 3 "   compilation time: ~a secs"
                    (lambda () 
                      (system comp-cmd))
                    :compilation-time)))
      (unless (= result 0)
        (sign-error "Compiling file `~a'. The following command was used:\n~a" file-name comp-cmd)))
    (status-message :linking lib-file-name)
    (verbose-message 2 "  generating dynamic link library '~a'..." lib-file-name)
    (verbose-message 3 "    executing command: ~a" link-cmd)
    (let ((result (display-runtime 3 "   linking time: ~a secs"
                    (lambda ()
                      (system link-cmd))
                    :linking-time)))
      (unless (= result 0)
        (sign-error "Generating dynamic link library `~a'. The following command was used:\n~a" lib-file-name link-cmd)))
    lib-file-name))

(define (load-dynamic-library lib-file-name)
  (status-message :loading-so lib-file-name)
  (verbose-message 2 "  loading dynamic library `~a'..." lib-file-name)
  (try
   (dynamic-load lib-file-name)
   (lambda (escape proc msg obj)
     (sign-error "Loading dynamic link library `~a'. Reason: ~a" lib-file-name msg))))

(compile-if (dynamic-enabled?)
            (define (compile-and-load definitions . lib-file-name)
              (let* ((lib-file-name (optional-arg lib-file-name #f))
                     (file-name (generate-code-file definitions))
                     (lib-file-name (compile-file file-name lib-file-name)))
                (load-dynamic-library lib-file-name)))
            (define (compile-and-load definitions . lib-file-name)
              (warning-message "Dynamic compilation of source code is not supported in this version of the binaries. So, the code will be INTERPRETED. Download a different binary from the SAL website if you want to use dynamic compilation.")
                (let ((saved-level (bigloo-warning)))
		  (bigloo-warning-set! 0)
		  (for-each eval definitions)
		  (bigloo-warning-set! saved-level))))
