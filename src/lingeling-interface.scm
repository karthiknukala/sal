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

;;
;; Interface to the lingeling SAT solver
;; - lingeling is available at Armin's Biere webpage:
;; - it's distributed with a GPL license
;; This interface should be compatible with SAT solvers in
;; the same family as lingeling (e.g., plingeling, precosat?).
;; 
(module lingeling-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *lingeling-command*
                *sal-tmp-in-file-to-lingeling*
		*sal-tmp-out-file-to-lingeling*
                (sal/set-lingeling-in-tmp-file!)
                (sal/set-lingeling-out-tmp-file!)
                (sal/set-lingeling-command! cmd-name)
                (lingeling/execute in-file)
		(lingeling/filter-output))
)


;; command: option -w force generation of a witness
(define *lingeling-command* "lingeling -w ")
(define *sal-tmp-in-file-to-lingeling* #f)
(define *sal-tmp-out-file-to-lingeling* #f)

(define-api (sal/set-lingeling-command! (cmd-name string?))
  (set! *lingeling-command* cmd-name))
  
(define (sal/set-lingeling-in-tmp-file!)
  (set! *sal-tmp-in-file-to-lingeling* (sal/setup-tmp-file! "input.cnf")))

(define (sal/set-lingeling-out-tmp-file!)
  (set! *sal-tmp-out-file-to-lingeling* (sal/setup-tmp-file! "output.lgl")))

;;
;; verbosity flag to pass to lingeling
;; depending on sal's verbosity level verbo
;;
(define (lingeling/verbo-option verbo)
  (cond ((> verbo 5) " -v -v -v ")
	((> verbo 4) " -v -v ")
	((> verbo 3) " -v ")
	(else " ")))



;;
;; Errors when runnig lingeling or when reading the output
;;
(define (sign-lingeling-died x)
  (cond ((if-signaled x)
	 (sign-error "Lingeling killed by signal ~a" (term-sig x)))
	((if-stopped x)
	 (sign-error "Lingeling stopped by signal ~a" (stop-sig x)))
	(else
	 (sign-error "Lingeling terminated unexpectedly"))))

(define (sign-error-running-lingeling cmd)
  (sign-error "The following command was used to execute Lingeling:\n~a\nIf this is not the correct command to execute lingeling, it can be changed using the statement:\n\n  (sal/set-lingeling-command! \"<path-to-lingeling>/<lingeling-executable>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))


(define (sign-bad-output-lingeling)
  (sign-error "Lingeling produced an unexpected output."))

(define (sign-bad-line-lingeling line)
  (sign-error "Can't process the following Lingeling output:\n  ~a" line))   

(define (sign-unknown-lingeling)
  (sign-error "Lingeling returned status UNKNOWN."))

(define (sign-conflicting-stat-lingeling)
  (sign-error "Lingeling returned both SATISFIABLE and UNSATISFIABLE."))

(define (sign-missing-stat-lingeling)
  (sign-error "Lingeling did not return a recognized status."))

(define (sign-io-error-lingeling msg)
  (sign-error "Error in Lingeling's output file\n~a" msg))


;;
;; exit code from lingeling:
;; 1 means an error somewhere
;;
;; 0 means status UNKNOWN
;; 10 means SAT
;; 20 means UNSAT
;;

;; Check for a normal exit code from lingeling
;; (we include UNKNOWN here, so we can parse the output file)
;;
(define (lingeling-exit-normal x) 
  (if (and (>= x 0) (if-exited x)) ;; normal exit
      (let ((exit-code (exit-status x)))
	(or (= exit-code 10) (= exit-code 20) (= exit-code 0)))))

;; Check for an error code from lingeling
(define (lingeling-exit-with-error x)
  (and (>= x 0) (if-exited x) (= (exit-status x) 1)))

;; Check for killed or other termination
(define (lingeling-terminated x)
  (or (if-signaled x) (if-stopped x)))



;;
;; Call lingeling on the given input file
;; - if all goes well parse the ouput and return it
;;
(define (lingeling/execute in-file)
  (sal/set-lingeling-out-tmp-file!)
  (let* ((cmd (string-append *lingeling-command* 
			    (lingeling/verbo-option (verbosity-level))
			    "\"" in-file "\" > \"" *sal-tmp-out-file-to-lingeling* "\""))
	 (_ (verbose-message 3 "  Lingeling command: ~a" cmd))
	 (_ (status-message :executing-lingeling))
	 (result (display-runtime 3 "  Lingeling execution time: ~a secs"
				  (lambda () (system cmd)) :lingeling-time))
	 (_ (verbose-message 1 "  Lingeling code: ~a" result)))
    (unwind-protect 
     (cond 
      ;; normal exit --> read the output
      ((lingeling-exit-normal result)
       (with-input-from-file *sal-tmp-out-file-to-lingeling*
	 (lambda () (lingeling/filter-output))))
      ;; exit code = 1 --> error from lingeling (e.g., incorrect option)
      ((lingeling-exit-with-error result)
       (sign-error-running-lingeling cmd))
      ;; lingeling was killed or crashed
      ((lingeling-terminated result)
       (sign-lingeling-died result))
      ;; any other code: assume we couldn't run lingeling
      (else
       (sign-error-running-lingeling cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-lingeling*))))


;; 
;; Helper function: num-list is a list of strings
;; parse all elements as integers and add the result to model
;;
(define (lingeling/add-to-model num-list model)
  (if (null? num-list) model
      (let ((num (string->integer (car num-list))))
	(if (= num 0) model
	    (lingeling/add-to-model (cdr num-list) (cons num model))))))


;;
;; Output format from lingeling and friends:
;; - a series of lines starting with 'c' --> statistics/comments (just ignore them)
;; - a line of the form  's <status>' where <status> can be 
;;   SATISFIABLE, UNSATISFIABLE, or UNKNOWN
;; - if <status> is SATISFIABLE, the Boolean assignment is printed as a 
;;   series of lines starting with 'v'. In each of these lines, there's
;;   a series on integers identifying the true literals as in:
;;     v -12842 12843 12844 -12845 ...
;;   A positive integer  denotes a positive literal;
;;   a negative integer denotes a negative literal
;; - The whole assignemnt is terminated by 'v 0' or by 'v .... 0'
;;
(define (lingeling/filter-output)
  (try 
    (let loop ((stat #unspecified) (model '()))
      (let ((curr-line (read-line)))
;;	(verbose-message 1 " curr-line: ~a" curr-line)  ;; for testing
        (cond ((eof-object? curr-line)
	       (cond ((eq? stat #unspecified) (sign-missing-stat-lingeling))
		     ((eq? stat #t) model)
		     ((eq? stat #f) #f)
		     (else (sign-bad-output-lingeling))))
	      ((substring=? "c" curr-line 1) (loop stat model)) ;; comment line
	      ((substring=? "s UNKNOWN" curr-line 9) (sign-unknown-lingeling)) ;; status unknown
	      ((substring=? "s UNSATISFIABLE" curr-line 15) ;; status unsat
	       (if (eq? stat #unspecified) (loop #f model) (sign-conflicting-stat-lingeling)))
	      ((substring=? "s SATISFIABLE" curr-line 13)   ;; status sat
	       (if (eq? stat #unspecified) (loop #t model) (sign-conflicting-stat-lingeling)))
	      ((substring=? "v " curr-line 2) ;; truth-assignment output
	       (let* ((rest-of-line (substring curr-line 2 (string-length curr-line)))
		      (num-list (delimited-string->list '(#\space) rest-of-line)))
		 (loop stat (lingeling/add-to-model num-list model))))
	      (else 
	       (sign-bad-line-lingeling curr-line)))))
    (lambda (escape proc msg obj)
      (sign-io-error-lingeling msg))))
