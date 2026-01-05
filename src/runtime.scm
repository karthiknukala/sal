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

(module runtime
        (extern 
         (initialize-runtime::void () "initialize_runtime")
         (collect-runtime::double () "collect_runtime")
         (collect-elapsed::double () "collect_elapsed")
	 ;;
	 ;; functions to inspect the status value returned by (display-runtime ..)
	 ;; (man 2 wait explains what these are about).
	 ;;
	 (wifexited::int (::int) "wifexited")
	 (wifsignaled::int (::int) "wifsignaled")
	 (wifstopped::int (::int) "wifstopped")
	 (wexitstatus::int (::int) "wexitstatus")
	 (wtermsig::int (::int) "wtermsig")
	 (wcoredump::int (::int) "wcoredump")
	 (wstopsig::int (::int) "wstopsig"))
	 
        (include "utility.macros")
        (import utility)
        (export (display-runtime vl msg proc . key-name)
		(if-exited c)
		(if-signaled c)
		(if-stopped c)
		(exit-status c)
		(term-sig c)
		(stop-sig c)
		(core-dump c))
        )

(define (display-runtime vl msg proc . key-name)
  (let ((key-name (optional-arg key-name #f)))
    [assert (key-name) (or (not key-name) (keyword? key-name))]
    (if (or (>= (verbosity-level) vl) (and key-name (status-messages-enabled?)))
      (begin
        (initialize-runtime)
        ;; Use call-with-values to properly handle multiple return values
        (call-with-values
            proc
            (lambda results
              (let ((elapsed-time (collect-runtime)))
                (cond
                 ((<fl elapsed-time 0.0)
                  (status-message key-name 'not-available)
                  (verbose-message vl msg "- (not available)"))
                 (else
                  (status-message key-name elapsed-time)
                  (verbose-message vl msg elapsed-time)))
                ;; Return all values
                (apply values results)))))
      (proc))))

    

;;
;; Query the termination conditions for a process
;;
(define (if-exited c) (not (= (wifexited c) 0)))

(define (if-signaled c) (not (= (wifsignaled c) 0)))

(define (if-stopped c) (not (= (wifstopped c) 0)))

;;
;; More info depending on the condition above
;;
(define (exit-status c) (wexitstatus c))

(define (term-sig c) (wtermsig c))

(define (stop-sig c) (wstopsig c))

(define (core-dump c) (not (= (wcoredump c) 0)))

