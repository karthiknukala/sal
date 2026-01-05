;;
;; SAL 3.3, Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
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
;; Interface to the Kissat SAT solver
;; - Kissat is available at https://github.com/arminbiere/kissat
;; - It's distributed under the MIT license
;; - This interface follows the SAT competition output format
;; 
(module kissat-interface
        (include "sal.sch")
        (import tmp-files runtime)
        (export *kissat-command*
                *sal-tmp-in-file-to-kissat*
		*sal-tmp-out-file-to-kissat*
                (sal/set-kissat-in-tmp-file!)
                (sal/set-kissat-out-tmp-file!)
                (sal/set-kissat-command! cmd-name)
                (kissat/execute in-file)
		(kissat/filter-output))
)


;; Default command - kissat with quiet mode (-q) disabled to get assignment output
(define *kissat-command* "kissat ")
(define *sal-tmp-in-file-to-kissat* #f)
(define *sal-tmp-out-file-to-kissat* #f)

(define-api (sal/set-kissat-command! (cmd-name string?))
  (set! *kissat-command* cmd-name))
  
(define (sal/set-kissat-in-tmp-file!)
  (set! *sal-tmp-in-file-to-kissat* (sal/setup-tmp-file! "input.cnf")))

(define (sal/set-kissat-out-tmp-file!)
  (set! *sal-tmp-out-file-to-kissat* (sal/setup-tmp-file! "output.kissat")))

;;
;; Verbosity flag to pass to kissat
;; depending on sal's verbosity level
;;
(define (kissat/verbo-option verbo)
  (cond ((> verbo 5) " -v -v -v ")
	((> verbo 4) " -v -v ")
	((> verbo 3) " -v ")
	((< verbo 2) " -q ")  ;; quiet mode for low verbosity
	(else " ")))



;;
;; Errors when running kissat or when reading the output
;;
(define (sign-kissat-died x)
  (cond ((if-signaled x)
	 (sign-error "Kissat killed by signal ~a" (term-sig x)))
	((if-stopped x)
	 (sign-error "Kissat stopped by signal ~a" (stop-sig x)))
	(else
	 (sign-error "Kissat terminated unexpectedly"))))

(define (sign-error-running-kissat cmd)
  (sign-error "The following command was used to execute Kissat:\n~a\nIf this is not the correct command to execute kissat, it can be changed using the statement:\n\n  (sal/set-kissat-command! \"<path-to-kissat>/<kissat-executable>\")\n\nThis statement should be included in your `.salrc' file in your home directory." cmd))


(define (sign-bad-output-kissat)
  (sign-error "Kissat produced an unexpected output."))

(define (sign-bad-line-kissat line)
  (sign-error "Can't process the following Kissat output:\n  ~a" line))   

(define (sign-unknown-kissat)
  (sign-error "Kissat returned status UNKNOWN."))

(define (sign-conflicting-stat-kissat)
  (sign-error "Kissat returned both SATISFIABLE and UNSATISFIABLE."))

(define (sign-missing-stat-kissat)
  (sign-error "Kissat did not return a recognized status."))

(define (sign-io-error-kissat msg)
  (sign-error "Error in Kissat's output file\n~a" msg))


;;
;; Exit code from kissat:
;; - 0 means status UNKNOWN (e.g., time limit, resource limit)
;; - 10 means SAT
;; - 20 means UNSAT
;; - 1 means an error (e.g., invalid option, file not found)
;;

;; Check for a normal exit code from kissat
;; (we include UNKNOWN here, so we can parse the output file)
;;
(define (kissat-exit-normal x) 
  (if (and (>= x 0) (if-exited x)) ;; normal exit
      (let ((exit-code (exit-status x)))
	(or (= exit-code 10) (= exit-code 20) (= exit-code 0)))))

;; Check for an error code from kissat
(define (kissat-exit-with-error x)
  (and (>= x 0) (if-exited x) (= (exit-status x) 1)))

;; Check for killed or other termination
(define (kissat-terminated x)
  (or (if-signaled x) (if-stopped x)))



;;
;; Call kissat on the given input file
;; - if all goes well parse the output and return it
;;
(define (kissat/execute in-file)
  (sal/set-kissat-out-tmp-file!)
  (let* ((cmd (string-append *kissat-command* 
			    (kissat/verbo-option (verbosity-level))
			    "\"" in-file "\" > \"" *sal-tmp-out-file-to-kissat* "\""))
	 (_ (verbose-message 3 "  Kissat command: ~a" cmd))
	 (_ (status-message :executing-kissat))
	 (result (display-runtime 3 "  Kissat execution time: ~a secs"
				  (lambda () (system cmd)) :kissat-time))
	 (_ (verbose-message 1 "  Kissat exit code: ~a" result)))
    (unwind-protect 
     (cond 
      ;; normal exit --> read the output
      ((kissat-exit-normal result)
       (with-input-from-file *sal-tmp-out-file-to-kissat*
	 (lambda () (kissat/filter-output))))
      ;; exit code = 1 --> error from kissat (e.g., incorrect option)
      ((kissat-exit-with-error result)
       (sign-error-running-kissat cmd))
      ;; kissat was killed or crashed
      ((kissat-terminated result)
       (sign-kissat-died result))
      ;; any other code: assume we couldn't run kissat
      (else
       (sign-error-running-kissat cmd)))
     (sal/delete-tmp-file! *sal-tmp-out-file-to-kissat*))))


;; 
;; Helper function: num-list is a list of strings
;; parse all elements as integers and add the result to model
;;
(define (kissat/add-to-model num-list model)
  (if (null? num-list) model
      (let ((num (string->integer (car num-list))))
	(if (= num 0) model
	    (kissat/add-to-model (cdr num-list) (cons num model))))))


;;
;; Output format from kissat:
;; - lines starting with 'c' --> comments/statistics (ignore them)
;; - a line of the form  's <status>' where <status> can be 
;;   SATISFIABLE, UNSATISFIABLE, or UNKNOWN
;; - if <status> is SATISFIABLE, the Boolean assignment is printed as a 
;;   series of lines starting with 'v'. In each of these lines, there's
;;   a series of integers identifying the true literals as in:
;;     v 1 -2 3 -4 ...
;;   A positive integer denotes a positive literal;
;;   a negative integer denotes a negative literal
;; - The whole assignment is terminated by '0'
;;
;; Helper: check if a line starts with a given prefix
(define (kissat/line-starts-with? line prefix)
  (and (>= (string-length line) (string-length prefix))
       (substring=? prefix line (string-length prefix))))

(define (kissat/filter-output)
  (try 
    (let loop ((stat #unspecified) (model '()))
      (let ((curr-line (read-line)))
;;	(verbose-message 1 " curr-line: ~a" curr-line)  ;; for testing
        (cond ((eof-object? curr-line)
	       (cond ((eq? stat #unspecified) (sign-missing-stat-kissat))
		     ((eq? stat #t) model)
		     ((eq? stat #f) #f)
		     (else (sign-bad-output-kissat))))
	      ;; comment lines: start with 'c' (possibly just "c" or "c ...")
	      ((kissat/line-starts-with? curr-line "c") (loop stat model))
	      ;; status lines
	      ((kissat/line-starts-with? curr-line "s UNKNOWN") (sign-unknown-kissat))
	      ((kissat/line-starts-with? curr-line "s UNSATISFIABLE")
	       (if (eq? stat #unspecified) (loop #f model) (sign-conflicting-stat-kissat)))
	      ((kissat/line-starts-with? curr-line "s SATISFIABLE")
	       (if (eq? stat #unspecified) (loop #t model) (sign-conflicting-stat-kissat)))
	      ;; truth-assignment output
	      ((kissat/line-starts-with? curr-line "v ")
	       (let* ((rest-of-line (substring curr-line 2 (string-length curr-line)))
		      (num-list (delimited-string->list '(#\space) rest-of-line)))
		 (loop stat (kissat/add-to-model num-list model))))
	      ;; empty lines should be ignored
	      ((= (string-length curr-line) 0) (loop stat model))
	      (else 
	       (sign-bad-line-kissat curr-line)))))
    (lambda (escape proc msg obj)
      (sign-io-error-kissat msg))))

