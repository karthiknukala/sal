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

(module sal-esm-options
        (include "sal.sch")
        (import front-end sal2scm-runtime gmp-scheme sal2scm-core)
        (export (sal-esm/set-report-progress-frequency! val)
                (sal-esm/enable-infinite-loop-detection! flag)
                (sal-esm/enable-debug-code! flag)
                (sal-esm/enable-dynamic-compilation! flag)
                (sal-esm/enable-gmp! flag)
                (sal-esmc/set-idfs-increment! num)
                (sal-esmc/set-traversal-strategy! name)
                (sal-esm/enable-static-scheduler! flag)
                (sal-esm/set-max-search-depth! max)
                (sal-esm/enable-expand-multi-commands! flag)
                (sal-esmc/enable-randomization! flag)
                (sal-esm/set-cacheless-num-paths! arg)
                (sal-esm/enable-deadlock-detection! flag)
                (sal-esmc/enable-symmetry-reduction! flag)
                (sal-esmc/set-show-visited-states! flag)
                (sal-esmc/set-show-fired-transitions! flag)
                (sal-path-explorer/enable-silent! flag)
                (sal-path-explorer/enable-guided! flag)
                (sal-esm/set-state-weight-function! function)
                *esm-report-frequency*
                *esm-infinite-loop-detection?*
                *esm-debug?*
                *esm-dynamic-compilation?*
                *esm-use-gmp?*
                *esm-max-depth*
                *esm-static-scheduler?*
                *esm-expand-multi-commands?*
                *esm-cacheless-num-paths*
                *esm-detect-deadlocks?*
                *esm-state-weight-function*

                *esmc-symmetry-reduction?*
                *esmc-show-visited-states?*
                *esmc-show-fired-transitions?*
                *esmc-randomize?*
                *esmc-traversal-strategy*
                *esmc-idfs-increment*

                *path-explorer-silent?*
                *path-explorer-guided?*)
        )

(define *esm-report-frequency* 1000)
(define *esm-infinite-loop-detection?* #f)
(define *esm-debug?* #f)
(define *esm-dynamic-compilation?* #f)
(define *esm-use-gmp?* #f)
(define *esm-max-depth* #f)
(define *esm-static-scheduler?* #f)
(define *esm-expand-multi-commands?* #f)
(define *esm-cacheless-num-paths* #f)
(define *esm-detect-deadlocks?* #f)
(define *esm-state-weight-function* #f)

(define *esm-supertrace-predicates* '())

(define *esmc-traversal-strategy* 'dfs)
(define *esmc-idfs-increment* 10)
(define *esmc-randomize?* #f)
(define *esmc-symmetry-reduction?* #f)
(define *esmc-show-visited-states?* #f)
(define *esmc-show-fired-transitions?* #f)

(define *path-explorer-silent?* #f)
(define *path-explorer-guided?* #f)

(define-api (sal-esm/set-report-progress-frequency! val)
  (set! *esm-report-frequency* val))

(define-api (sal-esm/enable-infinite-loop-detection! flag)
  :doc "Turn on/off infinite loop detection in recursive functions."
  (set! *esm-infinite-loop-detection?* flag))

(define-api (sal-esm/enable-debug-code! flag)
  :doc "Turn on/off debugging information in the code produced by the explicit state model checker."
  (set! *esm-debug?* flag))

(define-api (sal-esm/enable-dynamic-compilation! flag)
  :doc "Turn on/off dynamic compilation of code produced byt the explicit state model checker."
  (set! *esm-dynamic-compilation?* flag))

(define-api (sal-esm/enable-gmp! flag)
  :doc "Turn on/off GMP in the code produced by the explicit state model checker."
  (set! *esm-use-gmp?* flag))

(define-api (sal-esmc/set-idfs-increment! num)
  (set! *esmc-idfs-increment* num))

(define-api (sal-esmc/set-traversal-strategy! name)
  :doc "Set the state space traversal strategy. Available strategies: dfs, bfs, idfs, guided, guided-cacheless, and cacheless."
  (let ((name (to-symbol name)))
    (unless (memq name '(dfs bfs idfs cacheless guided-cacheless guided))
      (sign-error "Unknown state space traversal strategy `~a'." name))
    (set! *esmc-traversal-strategy* name)))
  
(define-api (sal-esm/enable-static-scheduler! flag)
  (set! *esm-static-scheduler?* flag))

(define-api (sal-esm/set-max-search-depth! max)
  (set! *esm-max-depth* max))

(define-api (sal-esm/enable-expand-multi-commands! flag)
  (set! *esm-expand-multi-commands?* flag))

(define-api (sal-esmc/enable-randomization! flag)
  (set! *esmc-randomize?* flag))

(define-api (sal-esm/set-cacheless-num-paths! arg)
  (set! *esm-cacheless-num-paths* arg))

(define-api (sal-esm/enable-deadlock-detection! flag)
  (set! *esm-detect-deadlocks?* flag))

(define-api (sal-esmc/enable-symmetry-reduction! flag)
  (set! *esmc-symmetry-reduction?* flag))

(define-api (sal-esmc/set-show-visited-states! flag)
  (set! *esmc-show-visited-states?* flag))

(define-api (sal-esmc/set-show-fired-transitions! flag)
  (set! *esmc-show-fired-transitions?* flag))

(define-api (sal-path-explorer/enable-silent! flag)
  (set! *path-explorer-silent?* flag))

(define-api (sal-path-explorer/enable-guided! flag)
  (set! *path-explorer-guided?* flag))

(define-api (sal-esm/set-state-weight-function! function)
  (set! *esm-state-weight-function* function))

(front-end/add-full-option!
 "Explicit State"
 "-p <list>"
 "--predicates=<list>"
 "Set a list of predicates for a supertracing search."
 (lambda (arg)
   (set! *esm-supertrace-predicates* (with-input-from-string arg read))))

(front-end/add-full-option!
 "Explicit State"
 "-f <num>"
 "--frequency=<num>"
 "Report progress every <num> states (default: 10000)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sal-esm/set-report-progress-frequency! arg))))


(front-end/add-full-option!
 "Explicit State"
 "-l"
 "--infinite-loop-detection"
 "Enable infinite loop detection in recursive functions (default: disabled)"
 (lambda ()
   (sal-esm/enable-infinite-loop-detection! #t)))

(front-end/add-full-option!
 "Explicit State"
 "-mrd <num>"
 "--max-recursion-depth=<num>"
 "Set the maximum recursion depth for recursive functions (depth: 1024). An error is produced when this threshold is reached. See option `--infinite-loop-detection'."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sal-esm/enable-infinite-loop-detection! #t)
    (sal-scm/set-max-loop-depth! arg))))

(front-end/add-full-option!
 "Explicit State"
 "-g"
 "--debug"
 "Add debugging information in the generated code (depth: disabled). Runtime errors will contain line number information when this option is used."
 (lambda ()
   (sal-esm/enable-debug-code! #t)))

(front-end/add-full-option!
 "Explicit State"
 "-c"
 "--compile"
 "Enable dynamic compilation of the code produced by the model checker (default: disabled). The default behavior is to use an interpreter."
 (lambda ()
   (sal-esm/enable-dynamic-compilation! #t)))

(front-end/add-full-option!
 "Explicit State"
 "-m"
 "--gmp"
 "Use GMP to handle arithmetic (default: disabled). GMP is the GNU multi-precision package for integer and rational numbers. It must be used to model check specifications that use big numbers."
 (lambda ()
   (sal-esm/enable-gmp! #t)))

(front-end/add-full-option!
 "ESMC"
 "-s <name>"
 "--traversal-strategy=<name>"
 "The state space traversal strategy (default: dfs). The available strategies are: dfs (depth-first search), bfs (breadth-first search), idfs (iterative depth-first search), guided (state with highest weight first), cacheless (random search without a cache), guided-cacheless (guided search without a cache)."
 (lambda (name)
   (sal-esmc/set-traversal-strategy! name)))

(front-end/add-full-option!
 "ESMC"
 "-i <num>"
 "--idfs-increment=<num>"
 "The increment used in each iteration of the iterative deepening depth-first search strategy (default: 10)."
 (front-end-adapter/nz-nat-arg 
  (lambda (num)
    (sal-esmc/set-traversal-strategy! 'idfs)
    (sal-esmc/set-idfs-increment! num))))

(front-end/add-full-option!
 "Explicit State"
 "-d <num>"
 "--max-depth=<num>"
 "Maximum execution depth (default: unbounded)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sal-esm/set-max-search-depth! arg))))

(front-end/add-full-option!
 "Explicit State"
 "-ss"
 "--static-scheduler"
 "Schedule the operations at compilation time, this option usually improves the performance for synchronous systems (default: disabled)."
 (lambda ()
   (sal-esm/enable-static-scheduler! #t)))

(front-end/add-full-option!
 "Explicit State"
 "-emc"
 "--expand-multi-commands"
 "Expand multi commands in the specification. This option improves the static scheduling of operations (see option --static-scheduler)."
 (lambda ()
   (sal-esm/enable-expand-multi-commands! #t)))

(front-end/add-full-option!
 "ESMC"
 "-r"
 "--randomize"
 "Randomize the state space traversal."
 (lambda ()
   (sal-esmc/enable-randomization! #t)))

(front-end/add-full-option!
 "Explicit State"
 "-n <num>"
 "--num-paths=<num>"
 "Number of paths to explore in the cacheless state traversal strategy and in the sal-path-explorer (default: unbounded)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sal-esm/set-cacheless-num-paths! arg))))

(front-end/add-full-option!
 "Explicit State"
 "-k"
 "--detect-deadlocks"
 "Enable deadlock detection."
 (lambda ()
   (sal-esm/enable-deadlock-detection! #t)))

(front-end/add-full-option!
 "Explicit State"
 "-mvs <num>"
 "--max-vector-size=<num>"
 "Set the maximum vector size that can be used in the generated code."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg)
    (sal-scm/set-max-vector-size! arg))))

(front-end/add-full-option!
 "ESMC"
 "-y"
 "--symmetry"
 "Enable symmetry reduction."
 (lambda ()
   (sal-esmc/enable-symmetry-reduction! #t)))

(front-end/add-full-option!
 "ESMC"
 "-sv"
 "--show-visited-states"
 "Show visited states."
 (lambda ()
   (sal-esmc/set-show-visited-states! #t)))

(front-end/add-full-option!
 "ESMC"
 "-st"
 "--show-fired-transitions"
 "Show fired transitions"
 (lambda ()
   (sal-esmc/set-show-fired-transitions! #t)))

(front-end/add-full-option!
 "Path Explorer"
 "-s"
 "--silent"
 "Do not print states/transitions."
 (lambda ()
   (sal-path-explorer/enable-silent! #t)))

