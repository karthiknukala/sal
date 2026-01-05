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
;; Interface to the Minisat SAT solver
;; - minisat is available at ???
;; This interface should be compatible with all solvers that
;; use the same calling conventions as minisat:
;;   minisat <input.cnf> <output>
;; 
;; The result is copied in the ouput file:
;; first line is either 'SAT' or 'UNSAT'
;; if the first line is 'SAT' the rest of the file
;; is the Boolean assignment as a list of numbers:
;; positive number +x --> variable x is true
;; negative number -x --> varaible x is false
;; The list is terminated by 0
;;
(module minisat-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *minisat-command*
                *sal-tmp-in-file-to-minisat*
		*sal-tmp-out-file-to-minisat*
                (sal/set-minisat-in-tmp-file!)
                (sal/set-minisat-out-tmp-file!)
                (sal/set-minisat-command! cmd-name)
                (minisat/execute in-file))
        )


;; command: no special option
(define *minisat-command* "minisat ")
(define *sal-tmp-in-file-to-minisat* #f)
(define *sal-tmp-out-file-to-minisat* #f)

(define-api (sal/set-minisat-command! (cmd-name string?))
  (set! *minisat-command* cmd-name))
  
(define (sal/set-minisat-in-tmp-file!)
  (set! *sal-tmp-in-file-to-minisat* (sal/setup-tmp-file! "input.cnf")))

(define (sal/set-minisat-out-tmp-file!)
  (set! *sal-tmp-out-file-to-minisat* (sal/setup-tmp-file! "output.lgl")))

;;
;; verbosity flag to pass to minisat
;; depending on sal's verbosity level verbo
;; (minisat's default is -verb=1)
;;
(define (minisat/verbo-option verbo)
  (cond ((> verbo 5) " -verb=2 ")
	((> verbo 3) " -verb=1 ")
	(else " -verb=0 ")))



;;
;; Errors when runnig minisat or when reading the output
;;
(define (sign-minisat-died x)
  (cond ((if-signaled x)
	 (sign-error "Minisat killed by signal ~a" (term-sig x)))
	((if-stopped x)
	 (sign-error "Minisat stopped by signal ~a" (stop-sig x)))
	(else
	 (sign-error "Minisat terminated unexpectedly"))))

(define (sign-bad-output-minisat)
  (sign-error "Minisat produced an unexpected output."))

(define (sign-unknown-minisat)
  (sign-error "Minisat returned UNKNOWN status."))

(define (sign-missing-stat-minisat)
  (sign-error "Minisat did not return a recognized status."))

(define (sign-io-error-minisat)
  (sign-error "IO error when reading Minisat's output file."))


(define (sign-error-running-minisat cmd)
  (sign-error "The following command was used to execute Minisat:\n~a\nIf this is not the correct command to execute Minisat, it can be changed using the statement:\n\n  (sal/set-minisat-command! \"<path-to-minisat>/<minisat-executable>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))


;;
;; exit code from minisat:
;; 1 means an error somewhere
;;
;; 0 means status UNKNOWN
;; 10 means SAT
;; 20 means UNSAT
;;

;; Check for a normal exit code from minisat
;; (we include UNKNOWN here, so we can parse the output file)
;;
(define (minisat-exit-normal x) 
  (if (and (>= x 0) (if-exited x)) ;; normal exit
      (let ((exit-code (exit-status x)))
	(or (= exit-code 10) (= exit-code 20) (= exit-code 0)))))

;; Check for an error code from minisat
(define (minisat-exit-with-error x)
  (and (>= x 0) (if-exited x) (= (exit-status x) 1)))

;; Check for killed or other termination
(define (minisat-terminated x)
  (or (if-signaled x) (if-stopped x)))



;;
;; Call minisat on the given input file
;; - if all goes well parse the ouput and return it
;;
(define (minisat/execute in-file)
  (sal/set-minisat-out-tmp-file!)
  (let* ((cmd (string-append *minisat-command* 
			    (minisat/verbo-option (verbosity-level))
			    "\"" in-file "\"  \"" *sal-tmp-out-file-to-minisat* "\""))
	 (_ (verbose-message 3 "  Minisat command: ~a" cmd))
	 (_ (status-message :executing-minisat))
	 (result (display-runtime 3 "  Minisat execution time: ~a secs"
				  (lambda () (system cmd)) :minisat-time))
	 (_ (verbose-message 1 "  Minisat code: ~a" result)))
    (unwind-protect 
     (cond 
      ;; normal exit --> read the output
      ((minisat-exit-normal result)
       (with-input-from-file *sal-tmp-out-file-to-minisat*
	 (lambda () (minisat/filter-output))))
      ;; exit code = 1 --> error from minisat (e.g., incorrect option)
      ((minisat-exit-with-error result)
       (sign-error-running-minisat cmd))
      ;; minisat was killed or crashed
      ((minisat-terminated result)
       (sign-minisat-died result))
      ;; any other code: assume we couldn't run minisat
      (else
       (sign-error-running-minisat cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-minisat*))))


;; 
;; Helper function: num-list is a list of strings
;; parse all elements as integers then add them to model
;; stop when '0' is read.
;;
(define (minisat/parse-model num-list model)
  (if (null? num-list) model
      (let ((num (string->integer (car num-list))))
	(if (= num 0) model
	    (minisat/parse-model (cdr num-list) (cons num model))))))



;;
;; Output format from minisat:
;; - the first line is either 'SAT' or 'UNSAT' or 'INDET'
;; - if it's 'SAT' the second line contains the model as a list
;;   of integers terminated by 0
;;
(define (minisat/filter-output)
  (try 
   (let ((first-line (read-line)))
     (cond 
      ((eof-object? first-line) (sign-bad-output-minisat)) ;; empty file
      ((substring=? "INDET" first-line 5) (sign-unknown-minisat)) ;; status: unknown
      ((substring=? "UNSAT" first-line 5) #f) ;; status: unsat
      ((substring=? "SAT" first-line 3) ;; status sat: get the model
       (let ((sol-line (read-line))) ;; assignment should be all on one line
	 (if (eof-object? sol-line)
	     (sign-bad-output-minisat)  ;; missing assignment
	     (let ((num-list (delimited-string->list '(#\space) sol-line)))
	       (minisat/parse-model num-list '())))))))
    ;; exception 
   (lambda (escape proc msg obj)
     (sign-io-error-minisat))))
