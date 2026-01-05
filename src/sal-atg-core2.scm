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

;; BD: hacked this from sal-atg-core.scm to dump the paths
;; as soon as they are discovered (rather than wait until
;; the end).
 
; main function is testgen -- toward the bottom

(define (disj goals)
  (when (list? goals)
  (string-append "(" 
           (if (string? (car goals)) (car goals) (conj (car goals)))
           (apply string-append
            (map (lambda (g) (string-append " OR "
              (if (string? g) g (conj g)))) (cdr goals)))
           ")")))

(define (conj goals)
  (when (list? goals)
  (string-append "(" 
           (if (string? (car goals)) (car goals) (disj (car goals)))
           (apply string-append
            (map (lambda (g) (string-append " AND "
              (if (string? g) g (disj g)))) (cdr goals)))
           ")")))

(define (mk-expr expr-str module)
  (let ((real-mk-expr (if (instance-of? module <sal-boolean-flat-module>) 
			  make-boolean-state-expression make-simple-data-state-expression)))
    (real-mk-expr expr-str module)))
    
; creates the conjunction of the purpose-list with the disjunction of the goal-list

(define (list->goal goal-list purpose-list module)
  (if purpose-list
      (mk-expr
       (string-append "(" (conj purpose-list) " AND " (disj goal-list) ")") module)
      (mk-expr (disj goal-list) module)))

(define (goal-reduce scan goal-list path prevlen)
  (let ((len (sal-path/length path)))
    (let loop ((acc '())
         (gl goal-list))
      (if (null? gl)
    acc
    (loop
     (if (boolean-value-at scan path (car gl) len prevlen)
         (begin
;        (display "----- Discharged ")(display (car gl)) (newline)
     (verbose-message 1 "ATG: Discharged test goal \"~a\" at depth ~a."
          (if (list? (car gl)) (conj (car gl)) (car gl)) len)
     acc
     )
         (cons (car gl) acc))
     (cdr gl))))))

(define (minimal-goal-reduce scan goal-list path prevlen)
(let ((len (sal-path/length path)))
  (let loop ((acc '())
       (gl goal-list))
    (cond ((null? gl) goal-list)
    ((boolean-value-at scan path (car gl) len prevlen)
     (begin
       (verbose-message 1 "ATG: Discharged test goal \"~a\" at depth ~a."
            (if (list? (car gl)) (conj (car gl)) (car gl)) len)
       (append acc (cdr gl))))
    (else (loop (cons (car gl) acc) (cdr gl)))))))

(define (sal-path/evaluate-string-at path str idx)
        (let* ((boolean-module (slot-value path :flat-module))
                           (bool-expr (mk-expr str boolean-module)))
     (sal-path/evaluate-expr-at path bool-expr idx)))

(define (boolean-value-at-core path v id)
  (if (list? v)
      (let loop ((vv v))
  (if (let ((val (sal-path/evaluate-string-at path (car vv) (- id 1))))
        (if val (sal-expr/true? val) #f))
      (if (null? (cdr vv)) #t (loop (cdr vv)))
      #f))
      (let ((val (if *simplenames?*
         (sal-path/state-variable-value-at path (string->symbol v) (- id 1))
         (sal-path/evaluate-string-at path v (- id 1)))))
      (if val (sal-expr/true? val) #f))))

(define (boolean-value-at scan path v id prevlen)
  (if scan
      (let loop ((pos id))
  (if (> pos prevlen)
      (if (boolean-value-at-core path v pos) #t (loop (- pos 1)))
      #f))
      (boolean-value-at-core path v id)))

(define (extend-search module goal-list purpose-list path scan prune innerslice branch incr init ext mind)
  (cond ((null? goal-list) (values '() path))
	((= ext 0) (values goal-list path))
	(else
	 (verbose-message 1 "ATG: Searching for extension to current path; ~a goals left."
			  (length goal-list))
					; need the following on my laptop 'cos short of tmp space
					;  (system "rm -f /tmp/sal-rushby-*")
	 (let* ((goal (list->goal goal-list purpose-list module))
		(mod (if innerslice (sal-module/slice-for module goal) module))
		(prevlen (sal-path/length path))
		(new-path
		 (let loop ((depth (if incr (max 1 (quotient ext 10) mind) (max ext mind))))
		   (cond ((> depth ext) '())
			 ((if *infbmc?* (sal-inf-bmc/extend-path+ path mod goal mind depth *solver-id*)
			      (sal-bmc/extend-path+ path mod goal mind depth *solver-id*)))
			 (else
			  (loop (+ depth (max 1 (quotient ext 10)))))))))
					;    (breakpoint "foo" (new-path goal mind) #t)
	   (if (instance-of? new-path <sal-path>)
	       (begin 
		 (test-path/pp (sal-derived-input-seq->original-input-seq new-path) 0)
		 (extend-search mod 
				(if prune
				    (goal-reduce scan goal-list new-path prevlen)
				    (minimal-goal-reduce scan goal-list new-path prevlen))
				purpose-list new-path scan prune innerslice branch incr init ext mind))
	       (begin
		 (verbose-message 1 "ATG: No new goals found at depth ~a, abandoning this path"
				  (+ (sal-path/length path) ext))
		 (values goal-list path)))))))

; discharges as many goals as possible from a single initial segment

(define (iterative-search module goal-list purpose-list scan prune slice innerslice branch smcinit iincr eincr init ext mind)
  (let* ((goal (list->goal goal-list purpose-list module))
         (mod (if slice (sal-module/slice-for module goal) module))
; find an initial segment
         (path (if smcinit
                   (if (= init 0)
                       (sal-smc/find-path-from-initial-state mod goal)
                       (sal-smc/find-path-from-initial-state-with-at-most mod goal (+ 1 init)))
                   (let iloop ((depth (if iincr (max 1 (quotient init 10)) init)))
                     (cond ((> depth init) #f)
                           ((if *infbmc?* (sal-inf-bmc/find-path-from-initial-state mod goal depth *solver-id*)
				(sal-bmc/find-path-from-initial-state mod goal depth *solver-id*)))
                           (else
                            (iloop (+ depth (max 1 (quotient init 10))))))))))
; if successful, extend it
    (if path
	(begin 
	  (test-path/pp (sal-derived-input-seq->original-input-seq path) 0)
	  (let bloop ((goals 
		       (if prune
			   (goal-reduce scan goal-list path 0)
			   (minimal-goal-reduce scan goal-list path 0)))
		      (paths '())
		      (first #t))
	    (multiple-value-bind
	     (newgoals newpath)
	     (extend-search mod goals purpose-list path scan prune innerslice branch eincr init ext mind)
	     (if branch
		 (if (and (< 0 (length newgoals))
			  (< (length newgoals) (length goals)))
		     (begin
		       (verbose-message 1 "ATG: exploring new branch; ~a goals left."
					(length newgoals))
		       (bloop newgoals (cons newpath paths) #f))
		     (values newgoals (if first (cons newpath '()) paths)))
		 (values newgoals (cons newpath '()))))))
					; nothing found
        (values #f #f))))

; Arguments to testgen (mostly passed on to subsidiary functions)
; module: the (boolean-)flat-module to be analyzed
; goal-list: list of trap variables
; purpose-list: list of trap variables for test purposes or #f if none
; scan: whether full path should be scanned to see if a trap variable has become true
;   (if trap variables latch, leave this false and only the last state will be checked)
; prune: whether the goal list should be purged of all trap variables that have become true
;   (otherwise only the one targeted is removed)
; slice: whether to slice before starting a new path
; innerslice: whether to slice before each extension to the current path
; branch: whether to explore multiple branches after the initial search
; smcinit: if #t, use SMC to start each path; otherwise use BMC (must be #f if *infbmc?* is #t)
; iincr: if #t, step the depth of initial BMC incrementally from 1 to its limit
; eincr: if #t, step the depth of extension BMC incrementally from 1 to its limit
; init: the depth to search when starting a new path (0 means no limit for SMC)
; ext: the depth to search when extending a path
; mind is the minimum depth to be used when extending a path

; the global *infbmc?* causes the inf-bmc to be used if #t, otherwise finite bmc

; output is a multiple-value pair: undischarged goals, and a null-terminated list of paths.

; (multiple-value-bind (a b) (blah)) gets the two outputs of (blah) in a and b
; (values x y) produces two outputs


(define (testgen module goal-list purpose-list scan prune slice innerslice branch smcinit iincr eincr init ext mind)
  (let loop ((result '())
       (goals goal-list))
       (if (null? goals) (values goals result)
     (begin
       (verbose-message 1 "ATG: Starting a new path; ~a goals left." (length goals))
       (multiple-value-bind
        (new-goals new-result)
        (iterative-search module goals purpose-list scan prune slice innerslice
			  branch smcinit iincr eincr init ext mind)
        (if new-result
	    (loop (append new-result result) new-goals)
	    (values goals result)))))))

(sal/set-sal-pp-proc! sal-ast->sal-doc)

(define (test-path/pp path i)
  (sal-path/pp path *inputsonly?*))

(define (print-tests test-list)
  (let loop ((test test-list)
       (i 1))
    (unless (null? test)
      (test-path/pp (sal-derived-input-seq->original-input-seq (car test)) i)
      (loop (cdr test) (+ i 1)))))

(define (count-tests test-list)
  (let loop ((acc 0)
       (l test-list))
    (if (null? l)
	acc
	(loop (+ acc (- (sal-path/length (car l)) 1)) (cdr l)))))

(define (goal-product goal-list1 goal-list2)
  (let loop ((res '())
       (g1 goal-list1))
    (if (null? g1) res
	(loop (append (map (lambda (g) (list (car g1) g)) goal-list2) res)
	      (cdr g1)))))

(define (except goal-list1 goal-list2)
  (let loop ((res '())
       (g1 goal-list1))
    (if (null? g1) res
	(loop (if (member (car g1) goal-list2) res (cons (car g1) res)) (cdr g1)))))
