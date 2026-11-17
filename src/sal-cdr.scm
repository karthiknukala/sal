;; Counterexample-directed reachability with property-directed k-induction.
;;
;; Algorithm: Jovanovic and Dutertre, Property-Directed k-Induction,
;; FMCAD 2016, Algorithms 1--3 (the supplied fmcad2016 PDF, pp. 3--6).
;; The two components of an obligation must remain distinct: A is a forward
;; overapproximation, C is a backward region with a path to the bad states.
;; This implementation uses SAL's existing flattened AST and Yices2 FFI.
;; The solver runs in process. SMT-LIB output is an optional log of its queries.

(module sal-cdr
  (include "sal.sch")
  (include "fast-hash-table.sch")
  (import (yices2-ffi "yices2_ffi.scm")
          sal-expression sal-type sal-type-membership sal-ast-simplify
          sal-module sal-assertion sal-path gmp-scheme sal-ast-table sal-ast-for-each
          sal-flat-data-structures sal-environment sal-ast-copy sal-ast-env queue)
  (export (sal-cdr/invariant assertion max-depth)
          (sal-cdr/reset!)
          (sal-cdr/display-statistics)
          (sal-cdr/statistics)
          (sal-cdr/with-statistics-observer observer thunk)
          (sal-cdr/set-option! key value)
          (sal-cdr/display-invariant))
  (eval (export-exports)))

;; Engine data

(define-struct cdr-solver native assertions interpolator mcsat? fallback incomplete?)
(define-struct cdr-step variables mapping cache transition buffer projection)
(define-struct cdr-projection variables elimination-buffer count formulas)
(define-struct cdr-successor mapping goals)
(define-struct cdr-system module declarations decl-map type-map globals
  steps native-types base-count base-buffer initial transition domain nonlinear?
  solvers models initial-solver frames interpolation-b interpolation-record expression-cache transient-inputs
  minimization-solver graph roots word-encoding successor integrality? definitions trace-declarations smt2-output
  projection-result)

(define-struct cdr-word-encoding reject requirements constants)

(define-struct cdr-frame solver excluded facts)

(define-struct cdr-obligation forward backward distance score)

(define-struct cdr-heap data size)

;; Property-directed k-induction

(define (sal-cdr/invariant assertion max-depth)
  (unless (sal-module-models/invariant? assertion)
    (sign-error "sal-cdr supports safety invariants only."))
  (unless (and (integer? max-depth) (>= max-depth 0))
    (sign-error "sal-cdr requires a nonnegative maximum induction depth."))
  (unless *cdr-initialized?*
    (yices-init)
    (set! *cdr-initialized?* #t))
  (unless (= (yices-has-mcsat) 1)
    (sign-error "sal-cdr requires a Yices2 library with MCSAT and interpolation support. Rebuild with YICES2_PREFIX pointing to that installation."))
  (vector-fill! *cdr-statistics* 0)
  (set! *cdr-invariant* #f)
  (set! *cdr-query-id* (+ *cdr-query-id* 1))
  (let ((sys #f))
    (unwind-protect
      (bind-exit (abort)
        (set! *cdr-abort* (lambda (status)
                          (when (and sys (cdr-option 'cex-graph))
                            (cdr-write-graph! sys (cdr-option 'cex-graph)))
                          (abort (values status #unspecified))))
        (let* ((definitions (cdr-state-definitions (slot-value assertion :module)))
               (packed
                 (and (cdr-word-candidate? assertion definitions)
                      (bind-exit (fallback)
                        (set! sys (cdr-empty-system (make-cdr-word-encoding fallback '() (make-hashtable)) definitions))
                        (cdr-setup! sys assertion))))
               (property
                 (or packed
                     (begin
                       (when sys (cdr-cleanup! sys))
                       (set! sys (cdr-empty-system #f definitions))
                       (cdr-setup! sys assertion))))
               (result (cdr-search sys property max-depth)))
          (when (cdr-option 'cex-graph)
            (cdr-write-graph! sys (cdr-option 'cex-graph)))
          (cond ((eq? result 'valid) (values 'valid #unspecified))
                ((number? result)
                 (vector-set! *cdr-statistics* cdr-stat-max-cex-depth result)
                 (values 'invalid (cdr-counterexample sys property result)))
                (else (values 'unknown #unspecified)))))
      (begin (when sys (cdr-cleanup! sys)) (set! *cdr-abort* #f)))))

;; Return 'valid, 'unknown, or the length of a counterexample.
(define (cdr-search sys property limit)
  (let ((bad (cdr-not property)))
    (cdr-record-edge! sys bad bad 0)
    (if (cdr-initial-reachable? sys bad)
      (begin (cdr-mark-root! sys bad) 0)
      (let rounds ((n 0) (k 1) (frame (list (make-cdr-obligation property bad 0 1.0))))
        (vector-set! *cdr-statistics* 4 n)
        (vector-set! *cdr-statistics* 5 k)
        (vector-set! *cdr-statistics* cdr-stat-frame-size (length frame))
        (vector-set! *cdr-statistics* cdr-stat-frame-pushed 0)
        (verbose-message 2 "  PDKIND frame ~a, induction depth ~a, ~a obligations"
                         n k (vector-ref *cdr-statistics* cdr-stat-frame-size))
        (if (and (> limit 0) (> k limit)) 'unknown
          (let* ((transitions (let loop ((i 0) (result '()))
                                (if (= i k) (reverse! result)
                                    (loop (+ i 1) (cons (cdr-transition-at sys i) result)))))
                 (solver (cdr-new-solver sys transitions)))
            (multiple-value-bind (result pushed next-index)
                (cdr-push-frame sys solver frame n k)
              (if (number? result) result
                (begin
                  (cdr-retire-solver! sys solver)
                  (cond
                    ((eq? result 'valid)
                     (cdr-save-invariant! sys pushed k)
                     'valid)
                    ((and (> (cdr-option 'max-frame) 0) (>= n (cdr-option 'max-frame)))
                     'unknown)
                    (else
                     (let ((next (cdr-minimize-frame sys (reverse! pushed)))
                           (cap (cdr-option 'induction-max)))
                       (for-each (lambda (o)
                                   (cdr-obligation-score-set! o (+ (/ (cdr-obligation-score o) 2) 0.5))) next)
                       ;; The SALLY induction cap keeps searching at fixed k.
                       (rounds next-index (if (> cap 0) (min (+ k 1) cap) (+ k 1)) next)))))))))))))

;; Push each forward obligation under k-step induction. A counterexample to
;; induction either leads backward to the bad states or tells us where the
;; forward frame needs strengthening.
(define (cdr-push-frame sys solver frame n k)
  ;; A holds through frame n. A region projected k steps back from a
  ;; violation cannot be reached before n-k+1 in this search.
  (let ((earliest (max 0 (+ n 1 (- k))))
        (heap (make-cdr-heap (make-vector (max 8 (vector-ref *cdr-statistics* cdr-stat-frame-size))) 0)))
    (for-each (lambda (o)
                (cdr-add-assumption! sys solver (cdr-obligation-forward o) k)
                (cdr-enqueue! heap o)) frame)
    (let push ((pushed '()) (failed? #f) (next-index (+ n k)))
      (if (= (cdr-heap-size heap) 0)
        (values (if failed? 'advance 'valid) pushed next-index)
        (let* ((o (cdr-dequeue! heap)) (a (cdr-obligation-forward o))
               (c (cdr-obligation-backward o)) (d (cdr-obligation-distance o))
               (not-a (cdr-not a))
               (cti (cdr-query sys solver (cdr-successor-goal sys not-a k))))
          (cond
            ((not cti)
             (cdr-count! cdr-stat-frame-pushed)
             (push (cons o pushed) failed? next-index))
            (else
             (let* ((already-bad? (or (= c not-a)
                                     (= 1 (cdr-ok (yices-formula-true-in-model cti (cdr-at sys c k))))))
                    (cex (if already-bad? cti (cdr-query sys solver (cdr-successor-goal sys c k)))))
               (if cex
                 (let* ((g (cdr-project sys cex c k))
                        (reach (cdr-reachable sys n g #t earliest)))
                   (cdr-record-edge! sys g c k)
                   (unless already-bad? (cdr-release-model! sys cex))
                   (cdr-release-model! sys cti)
                   (if reach (values (+ reach k d) #f #f)
                     (let* ((h (cdr-explain sys n g))
                            (new (make-cdr-obligation h g (+ d k) 1.0)))
                       (cdr-add-assumption! sys solver h k)
                       (cdr-obligation-score-set! o (* 0.9 (cdr-obligation-score o)))
                       (cdr-enqueue! heap new)
                       (cdr-enqueue! heap o)
                       (push pushed failed? next-index))))
                 (let* ((g (cdr-project sys cti not-a k))
                        (reach (cdr-reachable sys n g #f earliest)))
                   (cdr-release-model! sys cti)
                   (if reach
                     ;; The model gives an upper bound. Find the first
                     ;; reachable violation before advancing the frame.
                     (let ((first
                             (let bisect ((lo (+ n 1)) (hi (min next-index (+ reach k))))
                               (if (>= lo hi) lo
                                 (let* ((mid (quotient (+ lo hi) 2))
                                        (r (cdr-reachable sys mid not-a)))
                                   (if r (bisect lo mid) (bisect (+ mid 1) hi)))))))
                       (cdr-count! cdr-stat-frame-pushed)
                       (push (cons (make-cdr-obligation (cdr-not c) c d (exact->inexact d)) pushed)
                             #t (min next-index first)))
                     (let* ((h (cdr-explain sys n g))
                            (stronger (cdr-and (list a h))))
                       (cdr-add-assumption! sys solver h k)
                       (cdr-obligation-forward-set! o stronger)
                       (cdr-obligation-score-set! o (* 0.9 (cdr-obligation-score o)))
                       (cdr-enqueue! heap o)
                       (push pushed failed? next-index)))))))))))))

;; Reachability in AT MOST n transitions. Checking initial states explicitly
;; avoids the earliest-unreachable precondition of the paper's exact-depth
;; subroutine and handles deadlocks without adding artificial stuttering.
;; Return an actual witness length, or #f. An explicit work stack keeps long
;; counterexamples from consuming the Scheme/C call stack.
(define (cdr-reachable sys n goal #!optional (record? #f) (minimum 0))
  (let loop ((pending (list (cons n goal))))
    (if (null? pending)
      (begin (when (> n 0) (cdr-count! cdr-stat-unreachable)) #f)
      (let* ((obligation (car pending)) (level (car obligation)) (target (cdr obligation))
             (frame (cdr-frame-at sys level))
             (cache (cdr-frame-excluded frame))
             (distance (- n level)))
        (cond
          ((cdr-term-table-ref cache target) (loop (cdr pending)))
          ((and (>= distance minimum) (cdr-initial-reachable? sys target))
           (when (> n 0) (cdr-count! cdr-stat-reachable))
           (when record? (cdr-mark-root! sys target))
           distance)
          ((= level 0)
           (cdr-term-table-set! cache target #t)
           (loop (cdr pending)))
          (else
           (cdr-count! cdr-stat-reachability-queries)
           (let ((model (cdr-query sys (cdr-reach-solver sys (- level 1))
                                  (cdr-successor-goal sys target 1))))
             (if model
               (let ((predecessor (cdr-project sys model target 1)))
                 (cdr-release-model! sys model)
                 (when record? (cdr-record-edge! sys predecessor target 1))
                 (loop (cons (cons (- level 1) predecessor) pending)))
               (begin
                 (cdr-explain sys level target)
                 (loop (cdr pending)))))))))))

(define (cdr-frame-at sys n)
  (let ((frames (cdr-system-frames sys)))
    (when (>= n (vector-length frames))
      (set! frames (copy-vector frames (max (+ n 2) (* 2 (vector-length frames)))))
      (cdr-system-frames-set! sys frames))
    (when (eq? (vector-ref frames n) #unspecified)
      (vector-set! frames n
        (make-cdr-frame #f (make-eq-hash-table 17) '())))
    (vector-ref frames n)))

(define (cdr-reach-solver sys n)
  (let ((frame (cdr-frame-at sys n)))
    (unless (cdr-frame-solver frame)
      (cdr-frame-solver-set! frame
        (cdr-new-solver sys
          (append (list (cdr-system-transition sys))
                  (if (= n 0) (list (cdr-system-initial sys)) '())
                  (reverse (cdr-frame-facts frame))))))
    (cdr-frame-solver frame)))

(define (cdr-check-consistency! sys solver where)
  (when (and (cdr-option 'check-deadlock)
             (cdr-unsat? sys solver (yices-true)))
    ;; Like SALLY's diagnostic this checks the current abstraction/transition
    ;; context. It is not a complete reachable-deadlock verification query.
    (sign-error "sal-cdr: deadlock diagnostic: inconsistent ~a context." where)))

(define (cdr-learn! sys n goal learned)
  ;; Reachability here is up-to-depth, so a fact valid through n is valid at
  ;; every earlier frame. Installing it there cannot remove a reachable state.
  (let loop ((level n))
    (let* ((frame (cdr-frame-at sys level)) (cache (cdr-frame-excluded frame)))
      (unless (number? (cdr-term-table-ref cache goal))
        (cdr-term-table-set! cache goal learned)
        (when (> level 0)
          (cdr-frame-facts-set! frame (cons learned (cdr-frame-facts frame)))
          (when (cdr-frame-solver frame) (cdr-assert! (cdr-frame-solver frame) learned))
          (when (cdr-option 'check-deadlock)
            (cdr-check-consistency! sys (cdr-reach-solver sys level) "reachability")))))
    (when (and (cdr-option 'add-backward) (> level 0)) (loop (- level 1)))))

(define (cdr-initial-reachable? sys goal)
  (let ((cache (cdr-frame-excluded (cdr-frame-at sys 0))))
    (cond ((cdr-term-table-ref cache goal) #f)
          ((cdr-unsat? sys (cdr-system-initial-solver sys) goal)
           (cdr-term-table-set! cache goal #t)
           #f)
          (else #t))))

;; Explain the exclusion of G through n, including initial states even when
;; the system deadlocks. Each disjunct refutes G; together they cover I and
;; every successor of R[n-1]. Frames need not imply one another.
(define (cdr-explain sys n goal)
  (let* ((frame (cdr-frame-at sys n))
         (cache (cdr-frame-excluded frame))
         (known (cdr-term-table-ref cache goal)))
    (if (number? known) known
      (let* ((minimize? (cdr-option 'minimize-interpolants))
             (target (if minimize? (cdr-minimize-interpolant sys n goal #f) goal))
             (initial (if (= n 0)
                       (cdr-interpolate sys (cdr-system-initial-solver sys) target)
                       ;; I is independent of the reachability level. Keep
                       ;; its explanation in the level-zero cache as well.
                       (cdr-explain sys 0 target)))
             (explanation
               (if (= n 0) initial
                 (cdr-or (list initial
                   (cdr-back sys
                     (cdr-interpolate sys (cdr-reach-solver sys (- n 1))
                                      (cdr-at sys target 1)) 1)))))
             (learned (if minimize? (cdr-minimize-interpolant sys n explanation #t) explanation)))
        (cdr-learn! sys n goal learned)
        (cdr-count! 3)
        learned))))

;; The same ordering as SALLY: favor shallow backward regions relative to
;; their score, then larger scores, with native term identity breaking ties.
(define (cdr-obligation-better? a b)
  (let ((left (* (cdr-obligation-distance a) (cdr-obligation-score b)))
        (right (* (cdr-obligation-distance b) (cdr-obligation-score a))))
    (cond ((not (= left right)) (< left right))
          ((not (= (cdr-obligation-score a) (cdr-obligation-score b)))
           (> (cdr-obligation-score a) (cdr-obligation-score b)))
          ((not (= (cdr-obligation-backward a) (cdr-obligation-backward b)))
           (< (cdr-obligation-backward a) (cdr-obligation-backward b)))
          (else (< (cdr-obligation-forward a) (cdr-obligation-forward b))))))

;; Only the removed obligation is rescored. Queued keys never change.
;; The heap can keep its entries in an array.
(define (cdr-enqueue! heap obligation)
  (let ((data (cdr-heap-data heap)) (size (cdr-heap-size heap)))
    (when (= size (vector-length data))
      (set! data (copy-vector data (* 2 (vector-length data))))
      (cdr-heap-data-set! heap data))
    (cdr-heap-size-set! heap (+ size 1))
    (vector-set! *cdr-statistics* cdr-stat-queue-size (+ size 1))
    (let rise ((i size))
      (let ((parent (quotient (- i 1) 2)))
        (if (and (> i 0) (cdr-obligation-better? obligation (vector-ref data parent)))
          (begin (vector-set! data i (vector-ref data parent)) (rise parent))
          (vector-set! data i obligation))))))

(define (cdr-dequeue! heap)
  (let* ((data (cdr-heap-data heap)) (size (- (cdr-heap-size heap) 1))
         (result (vector-ref data 0)) (last (vector-ref data size)))
    (cdr-heap-size-set! heap size)
    (vector-set! *cdr-statistics* cdr-stat-queue-size size)
    (vector-set! data size #unspecified)
    (when (> size 0)
      (let sink ((i 0))
        (let ((left (+ (* 2 i) 1)))
          (if (>= left size) (vector-set! data i last)
            (let* ((right (+ left 1))
                   (child (if (and (< right size)
                                   (cdr-obligation-better? (vector-ref data right) (vector-ref data left)))
                            right left)))
              (if (cdr-obligation-better? (vector-ref data child) last)
                (begin (vector-set! data i (vector-ref data child)) (sink child))
                (vector-set! data i last)))))))
    result))

(define (cdr-add-assumption! sys solver formula depth)
  (let loop ((i 0))
    (when (< i depth)
      (cdr-assert! solver (cdr-at sys formula i))
      (loop (+ i 1))))
  (cdr-check-consistency! sys solver "induction"))

;; Yices2 storage and marshalling

;; The generated FFI binds the Yices2 calls and exposes their pointer types
;; as opaque. This section supplies storage and access for those arguments.
(define-macro (yices-enum name)
  `(pragma::int ,(symbol->string name)))

(define-macro (yices-allocate kind c-type count)
  (let ((size (gensym 'size)))
    `(let ((,(symbol-append size '::long) ,count))
       (,(symbol-append 'pragma:: kind)
        ,(string-append "(" c-type " *) GC_MALLOC(sizeof(" c-type ") * $1)") ,size))))

(define-macro (yices-pointer-present? pointer)
  `(pragma::bool "$1 != NULL" ,pointer))

(define-macro (yices-default-config) '(pragma::yices-ctx-config-pointer "NULL"))
(define-macro (yices-default-parameters) '(pragma::yices-param-pointer "NULL"))

(define-inline (yices-config-present? config::yices-ctx-config-pointer)
  (yices-pointer-present? config))

(define-inline (yices-context-unsat? context::yices-context-pointer)
  (= (yices-context-status context) (yices-enum YICES_STATUS_UNSAT)))

(define-inline (yices-arithmetic-constant? term::int32)
  (= (yices-term-constructor term) (yices-enum YICES_ARITH_CONSTANT)))

(define-inline (make-yices-term-array::yices-term-pointer size::long)
  (yices-allocate yices-term-pointer "term_t" size))

(define-inline (make-yices-int32-array::yices-int32-pointer size::long)
  (yices-allocate yices-int32-pointer "int32_t" size))

(define-inline (make-yices-uint32-cell::yices-uint32-pointer)
  (yices-allocate yices-uint32-pointer "uint32_t" 1))

(define-inline (yices-term-array-ref::int32 array::yices-term-pointer i::long)
  (pragma::int32 "$1[$2]" array i))

(define-inline (yices-term-array-set! array::yices-term-pointer i::long term::int32)
  (pragma::void "$1[$2] = $3" array i term))

(define-inline (yices-int32-array-ref::int32 array::yices-int32-pointer i::long)
  (pragma::int32 "$1[$2]" array i))

(define-inline (yices-uint32-cell-ref::uint32 cell::yices-uint32-pointer)
  (pragma::uint32 "$1[0]" cell))

(define (list->yices-term-array values)
  (let ((array::yices-term-pointer (make-yices-term-array (max 1 (length values)))))
    (let loop ((rest values) (i::long 0))
      (unless (null? rest)
        (yices-term-array-set! array i (car rest))
        (loop (cdr rest) (+ i 1))))
    array))

(define-inline (make-yices-term-vector::yices-term-vector-pointer)
  (let ((vector::yices-term-vector-pointer (yices-allocate yices-term-vector-pointer "term_vector_t" 1)))
    (yices-init-term-vector vector)
    vector))

(define (yices-term-vector-conjunction vector::yices-term-vector-pointer)
  (cdr-ok (yices-and (pragma::uint32 "$1->size" vector)
                    (pragma::yices-term-pointer "$1->data" vector))))

(define-inline (make-yices-interpolation-record::yices-interpolation-context-pointer)
  (yices-allocate yices-interpolation-context-pointer "interpolation_context_t" 1))

(define-inline (yices-set-interpolation-partitions! record::yices-interpolation-context-pointer
                                                   a::yices-context-pointer b::yices-context-pointer)
  (pragma::void "$1->ctx_A = $2; $1->ctx_B = $3" record a b))

(define-inline (yices-interpolant::int32 record::yices-interpolation-context-pointer)
  (pragma::int32 "$1->interpolant" record))

;; A native model can have different Scheme boxes at different call sites.
(define-inline (yices-same-model? a::yices-model-pointer b::yices-model-pointer)
  (pragma::bool "$1 == $2" a b))

(define (yices-take-string pointer::yices-string)
  (let ((result (yices-string-copy pointer)))
    (yices-free-string pointer)
    result))

(define (yices-term-text term #!optional (width 4294967295))
  (yices-take-string (yices-term-to-string term width 4294967295 0)))

(define (cdr-fail operation)
  (sign-error "sal-cdr: ~a: ~a" operation (yices-take-string (yices-error-string))))

(define (cdr-ok value)
  (when (< value 0) (cdr-fail "Yices2 operation failed"))
  value)

;; Keep native int32 boxing out of the hash keys. SAL's eq table hashes the
;; numeric fixnum IDs directly even when a context owns many native terms.
(define-inline (cdr-term-table-ref table term)
  (cond ((eq-hash-table/get table (int32->fixnum term)) => cdr)
        (else #f)))

(define-inline (cdr-term-table-set! table term value)
  (eq-hash-table/put! table (int32->fixnum term) value))

(define (cdr-and values)
  (cond ((null? values) (yices-true))
        ((null? (cdr values)) (car values))
        ((null? (cddr values)) (cdr-ok (yices-and2 (car values) (cadr values))))
        (else (cdr-ok (yices-and (length values) (list->yices-term-array values))))))

(define (cdr-or values)
  (cond ((null? values) (yices-false))
        ((null? (cdr values)) (car values))
        ((null? (cddr values)) (cdr-ok (yices-or2 (car values) (cadr values))))
        (else (cdr-ok (yices-or (length values) (list->yices-term-array values))))))

(define (cdr-not t) (cdr-ok (yices-not t)))

;; Solver contexts and queries

;; A logical solver owns one native context. An interpolation-capable copy
;; is allocated only if it is needed. Models never leave the Yices heap.
(define (cdr-new-native mcsat? logic #!optional (interpolation? #t))
  (let* ((tags (cdr-option 'solver-trace-tags))
         (ctx
          (if (or mcsat? logic tags)
            (let ((config (yices-new-config)))
              (unless (yices-config-present? config) (cdr-fail "creating solver configuration"))
              (unwind-protect
                (begin
                  (when logic
                    (when (< (yices-default-config-for-logic config logic) 0)
                      (cdr-fail (string-append "configuring solver logic " logic))))
                  (when tags (cdr-ok (yices-set-config config "trace" tags)))
                  (when mcsat?
                    (cdr-ok (yices-set-config config "solver-type" "mcsat"))
                    (when interpolation?
                      (cdr-ok (yices-set-config config "model-interpolation" "true"))))
                  (yices-new-context config))
                (yices-free-config config)))
            ;; Yices' default configuration already supports push/pop.
            (yices-new-context (yices-default-config)))))
    (unless (yices-pointer-present? ctx) (cdr-fail "creating solver"))
    (cdr-count! 6)
    ctx))

(define (cdr-new-solver sys formulas)
  (cdr-new-solver-mode sys formulas
    (case (cdr-option 'solver-mode)
      ((mcsat) #t)
      ((dpllt hybrid) #f)
      (else (and (not (cdr-solver-logic)) (cdr-system-nonlinear? sys))))))

(define (cdr-new-solver-mode sys formulas mcsat?)
  (let* ((ctx (cdr-new-native mcsat? (and (not mcsat?) (cdr-solver-logic))))
         (solver (make-cdr-solver ctx '() #f mcsat? #f #f)))
    (cdr-system-solvers-set! sys (cons solver (cdr-system-solvers sys)))
    (for-each (lambda (f) (cdr-assert! solver f)) formulas)
    solver))

(define (cdr-assert-native! solver formula)
  (unless (yices-context-unsat? (cdr-solver-native solver))
    (when (< (yices-assert-formula (cdr-solver-native solver) formula) 0)
      ;; A partial theory solver may still refute its supported subset. It
      ;; must never establish SAT after dropping an unsupported assertion.
      (let ((code (yices-error-code)))
        (if (or (= code (yices-enum CTX_NONLINEAR_ARITH_NOT_SUPPORTED))
                (= code (yices-enum MCSAT_ERROR_UNSUPPORTED_THEORY)))
          (begin
            (cdr-solver-incomplete?-set! solver #t)
            (yices-clear-error))
          (cdr-fail "asserting solver formula"))))))

(define (cdr-assert! solver formula)
  (cdr-assert-native! solver formula)
  (when (cdr-solver-fallback solver)
    (cdr-assert! (cdr-solver-fallback solver) formula))
  (when (cdr-solver-interpolator solver)
    (unless (yices-context-unsat? (cdr-solver-interpolator solver))
      (cdr-ok (yices-assert-formula (cdr-solver-interpolator solver) formula))))
  (cdr-solver-assertions-set! solver (cons formula (cdr-solver-assertions solver))))

(define (cdr-model sys ctx)
  (let ((model (yices-get-model ctx 1)))
    (unless (yices-pointer-present? model) (cdr-fail "getting model"))
    (cdr-system-models-set! sys (cons model (cdr-system-models sys)))
    model))

(define (cdr-release-model! sys model)
  (cdr-system-models-set! sys
    (filter (lambda (other) (not (yices-same-model? other model))) (cdr-system-models sys)))
  (yices-free-model model)
  #unspecified)

(define (cdr-close-solver! solver)
  (when (cdr-solver-native solver)
    (yices-free-context (cdr-solver-native solver))
    (cdr-solver-native-set! solver #f))
  (when (cdr-solver-interpolator solver)
    (yices-free-context (cdr-solver-interpolator solver))
    (cdr-solver-interpolator-set! solver #f))
  (when (cdr-solver-fallback solver) (cdr-close-solver! (cdr-solver-fallback solver))))

(define (cdr-retire-solver! sys solver)
  (when (cdr-solver-fallback solver) (cdr-retire-solver! sys (cdr-solver-fallback solver)))
  (cdr-close-solver! solver)
  (cdr-system-solvers-set! sys (remq solver (cdr-system-solvers sys))))

(define (cdr-cleanup! sys)
  (for-each yices-free-model (cdr-system-models sys))
  (for-each cdr-close-solver! (cdr-system-solvers sys))
  (when (cdr-system-projection-result sys)
    (yices-delete-term-vector (cdr-system-projection-result sys))
    (cdr-system-projection-result-set! sys #f))
  (when (cdr-system-interpolation-b sys) (yices-free-context (cdr-system-interpolation-b sys))))

(define (cdr-log-check-status! port solver status)
  (fprint port "; yices2 "
    (cond ((= status (yices-enum YICES_STATUS_SAT))
           (if (cdr-solver-incomplete? solver) 'unknown 'sat))
          ((= status (yices-enum YICES_STATUS_UNSAT)) 'unsat)
          ((or (= status (yices-enum YICES_STATUS_UNKNOWN))
               (= status (yices-enum YICES_STATUS_INTERRUPTED))) 'unknown)
          (else 'error))))

(define-macro (cdr-logged-check sys solver goal interpolate? check)
  (let ((port (gensym 'port)) (status (gensym 'status)))
    `(begin
       (when *cdr-statistics-observer* (*cdr-statistics-observer*))
       (if (cdr-system-smt2-output ,sys)
         (let ((,port (cdr-open-query-log ,sys ,solver ,goal ,interpolate?)))
           (unwind-protect
             (let ((,status ,check))
               (cdr-log-check-status! ,port ,solver ,status)
               ,status)
             (close-output-port ,port)))
         ,check))))

;; Return a native model on SAT, #f on UNSAT. The caller owns the model.
(define (cdr-query-native sys solver formula model?)
  (let ((ctx::yices-context-pointer (cdr-solver-native solver)))
    (if (yices-context-unsat? ctx) #f
      (let ((incomplete? (cdr-solver-incomplete? solver)))
        (cdr-ok (yices-push ctx))
        (unwind-protect
          (begin
            (cdr-assert-native! solver formula)
            (cdr-count! 0)
            (verbose-message 5 "  check ~a" (vector-ref *cdr-statistics* 0))
            (let ((status (cdr-logged-check sys solver formula #f
                             (yices-check-context ctx (yices-default-parameters)))))
              (cond ((= status (yices-enum YICES_STATUS_SAT))
                     (if (cdr-solver-incomplete? solver) 'unknown
                       (if model? (cdr-model sys ctx) #t)))
                    ((= status (yices-enum YICES_STATUS_UNSAT)) #f)
                    ((= status (yices-enum YICES_STATUS_UNKNOWN)) 'unknown)
                    ((= status (yices-enum YICES_STATUS_INTERRUPTED)) (*cdr-abort* 'unknown))
                    (else (cdr-fail "solver check")))))
          (begin
            (cdr-ok (yices-pop ctx))
            (cdr-solver-incomplete?-set! solver incomplete?)))))))

(define (cdr-query sys solver formula #!optional (model? #t))
  (let ((result (and (not (= formula (yices-false))) (cdr-query-native sys solver formula model?))))
    (if (not (eq? result 'unknown)) result
      (if (and (eq? (cdr-option 'solver-mode) 'hybrid)
               (not (cdr-solver-mcsat? solver)))
        (begin
          (unless (cdr-solver-fallback solver)
            (cdr-solver-fallback-set! solver
              (cdr-new-solver-mode sys (reverse (cdr-solver-assertions solver)) #t)))
          (cdr-query sys (cdr-solver-fallback solver) formula model?))
        (*cdr-abort* 'unknown)))))

(define (cdr-unsat? sys solver formula)
  ;; Reachability tests and minimization need only a Boolean answer. Building
  ;; and immediately freeing a complete model is avoidable on these hot paths.
  (not (cdr-query sys solver formula #f)))

(define (cdr-interpolation-context solver)
  (let ((mcsat (if (cdr-solver-mcsat? solver) solver (cdr-solver-fallback solver))))
    (cond
      ;; Ordinary MCSAT contexts already support model interpolation. The
      ;; API uses its own push/pop scope, so there is no need to replay A.
      ((and mcsat (not (cdr-solver-incomplete? mcsat))) (cdr-solver-native mcsat))
      ((cdr-solver-interpolator solver) => (lambda (ctx) ctx))
      (else
       (let ((ctx (cdr-new-native #t #f)))
         (cdr-solver-interpolator-set! solver ctx)
         (for-each (lambda (f)
                     (unless (yices-context-unsat? ctx)
                       (cdr-ok (yices-assert-formula ctx f))))
                   (reverse (cdr-solver-assertions solver)))
         ctx)))))

(define (cdr-interpolate sys solver goal)
  (bind-exit (done)
    (when (yices-context-unsat? (cdr-solver-native solver))
      (done (yices-false)))
    ;; Native model interpolation can turn scalar equalities into arithmetic
    ;; atoms. The reachability check has already proved A AND goal unsatisfiable,
    ;; so the complement of goal gives a well-typed exclusion lemma.
    (when (> (sal-ast-table/size (cdr-system-type-map sys)) 0)
      (done (cdr-not goal)))
    (unless (cdr-system-interpolation-b sys)
      ;; Partition B supplies models of the goal. The interpolation API
      ;; computes the model interpolants in A.
      (cdr-system-interpolation-b-set! sys (cdr-new-native (cdr-system-nonlinear? sys) #f #f)))
    (let ((a::yices-context-pointer (cdr-interpolation-context solver))
          (b::yices-context-pointer (cdr-system-interpolation-b sys))
          (record::yices-interpolation-context-pointer (cdr-system-interpolation-record sys)))
      (yices-reset-context b)
      (cdr-ok (yices-assert-formula b goal))
      (when (yices-context-unsat? a) (done (yices-false)))
      (when (yices-context-unsat? b) (done (yices-true)))
      (yices-set-interpolation-partitions! record a b)
      (cdr-count! 2)
      (cdr-count! 0)
      (verbose-message 5 "  interpolate ~a" (vector-ref *cdr-statistics* 2))
      (let ((status (cdr-logged-check sys solver goal #t
                      (yices-check-context-with-interpolation record (yices-default-parameters) 0))))
        (cond ((= status (yices-enum YICES_STATUS_UNSAT)) #unspecified)
              ((= status (yices-enum YICES_STATUS_SAT))
               (sign-error "sal-cdr: inconsistent reachability explanation."))
              ((or (= status (yices-enum YICES_STATUS_UNKNOWN))
                   (= status (yices-enum YICES_STATUS_INTERRUPTED))) (*cdr-abort* 'unknown))
              (else (cdr-fail "solver check"))))
      (let ((lemma (cdr-ok (yices-interpolant record))))
        (if (and (cdr-solver-logic) (not (cdr-solver-mcsat? solver)))
          ;; MCSAT can express a lemma using a theory unavailable in the
          ;; configured logic. A implies NOT goal, which uses the goal's logic.
          (let ((ctx::yices-context-pointer (cdr-solver-native solver)))
            (cdr-ok (yices-push ctx))
            (unwind-protect
              (if (>= (yices-assert-formula ctx lemma) 0) lemma
                (begin (yices-clear-error) (cdr-not goal)))
              (cdr-ok (yices-pop ctx))))
          lemma)))))

;; Time shifts and model projection

;; Step variables and substitutions are allocated once and shared by every
;; reachability, induction, interpolation, and trace query.
(define (cdr-step-at sys n)
  (let ((steps (cdr-system-steps sys)))
    (when (>= n (vector-length steps))
      (set! steps (copy-vector steps (max (+ n 2) (* 2 (vector-length steps)))))
      (cdr-system-steps-set! sys steps))
    (when (eq? (vector-ref steps n) #unspecified)
      (vector-set! steps n
        (make-cdr-step
          (map (lambda (type) (cdr-ok (yices-new-uninterpreted-term type)))
               (cdr-system-native-types sys))
          #f (make-eq-hash-table 17) #f #f #f)))
    (vector-ref steps n)))

(define (cdr-at sys term n)
  (if (= n 0) term
      (let* ((step (cdr-step-at sys n)) (cache (cdr-step-cache step)))
        (or (cdr-term-table-ref cache term)
            (begin
              (unless (cdr-step-mapping step)
                (cdr-step-mapping-set! step
                  (list->yices-term-array (append (cdr-step-variables step)
                                     (cdr-step-variables (cdr-step-at sys (+ n 1)))))))
              (let ((result (cdr-ok (yices-subst-term
                                     (fixnum->uint32 (cdr-system-base-count sys))
                                     (cdr-system-base-buffer sys) (cdr-step-mapping step) term))))
                (cdr-term-table-set! cache term result)
                result))))))

(define (cdr-step-buffer! step)
  (or (cdr-step-buffer step)
      (let ((buffer (list->yices-term-array (cdr-step-variables step))))
        (cdr-step-buffer-set! step buffer)
        buffer)))

(define (cdr-back sys term n)
  (if (= n 0) term
      (cdr-ok (yices-subst-term
               (fixnum->uint32 (quotient (cdr-system-base-count sys) 2))
               (cdr-step-buffer! (cdr-step-at sys n))
               (cdr-step-buffer! (cdr-step-at sys 0)) term))))

(define (cdr-transition-at sys n)
  (let ((step (cdr-step-at sys n)))
    (or (cdr-step-transition step)
        (let ((term (cdr-at sys (cdr-system-transition sys) n)))
          (cdr-step-transition-set! step term)
          term))))

(define (cdr-transition-substitution sys transition)
  (let ((assignments (make-eq-hash-table)) (integral-facts '()))
    (define (record left right)
      (when (instance-of? left <sal-next-operator>)
        (let* ((decl (slot-value (slot-value left :name-expr) :decl))
               (entry (eq-hash-table/get (cdr-system-decl-map sys) decl)))
          (when entry
            (let* ((next (cddr entry)) (value (cdr-encode sys right '() #f))
                   (replacement
                    (if (= (yices-term-is-bitvector next) 1)
                      (cdr-word-resize value (cdr-word-width next)) value)))
              (when (and (not (= replacement next))
                         (= (yices-test-subtype (yices-type-of-term replacement)
                                               (yices-type-of-term next)) 1))
                (if (and (cdr-system-integrality? sys) (= (yices-term-is-real next) 1)
                         (not (= (yices-term-is-int next) 1)))
                  (let ((operand (cdr-integral-shift-core right)))
                    (unless (and (eq? operand right) (not (= (yices-term-is-int value) 1)))
                      ;; Preserve the original atoms so the solver can use
                      ;; this congruence without an unbounded integer search.
                      (set! integral-facts
                        (cons (cdr-ok (yices-iff (yices-is-int-atom next)
                                        (yices-is-int-atom (cdr-encode sys operand '() #f))))
                              integral-facts))))
                  (eq-hash-table/put! assignments decl replacement))))))))
    (define (scan expression)
      (cond
        ((instance-of? expression <sal-and>)
         (for-each scan (sal-application/argument-list expression)))
        ((instance-of? expression <sal-definition-expression>)
         (scan (slot-value expression :expr)))
        ((instance-of? expression <sal-eq>)
         (let ((args (sal-application/argument-list expression)))
           (record (car args) (cadr args))
           (record (cadr args) (car args))))))
    (scan transition)
    (when (> (eq-hash-table/size assignments) 0)
      (cdr-system-successor-set! sys
        (make-cdr-successor
          (list->yices-term-array
            (map (lambda (decl next)
                   (cond ((eq-hash-table/get assignments decl) => cdr) (else next)))
                 (cdr-system-declarations sys) (cdr-step-variables (cdr-step-at sys 1))))
          (make-eq-hash-table 17))))
    integral-facts))

(define (cdr-successor-goal sys goal depth)
  (let ((successor (cdr-system-successor sys)))
    (if (not successor) (cdr-at sys goal depth)
      ;; T entails each of these unconditional equations. Replace only
      ;; the last state's occurrence in ordinary SAT queries. Projection
      ;; and interpolation still see the original state partition.
      (let* ((goals (cdr-successor-goals successor))
             (substituted
               (or (cdr-term-table-ref goals goal)
                   (let ((result
                           (cdr-ok (yices-subst-term (fixnum->uint32 (quotient (cdr-system-base-count sys) 2))
                                     (cdr-step-buffer! (cdr-step-at sys 0))
                                     (cdr-successor-mapping successor) goal))))
                     (cdr-term-table-set! goals goal result)
                     result))))
        (cdr-at sys substituted (- depth 1))))))

;; Project T^k /\ goal[k], never the induction assumptions. Including the
;; latter would destroy the existential path guarantee on the resulting G.
(define (cdr-projection-data sys depth)
  (let ((step (cdr-step-at sys depth)))
    (or (cdr-step-projection step)
        (let* ((inputs (cdr-system-transient-inputs sys))
               (variables (cdr-step-variables (cdr-step-at sys 0)))
               (state (if (null? inputs) variables
                        (filter (lambda (var) (not (exists (lambda (input) (= var input)) inputs))) variables)))
               (reversed '()))
          (define (collect-at vars time)
            (for-each (lambda (var) (set! reversed (cons (cdr-at sys var time) reversed))) vars))
          ;; Eliminate intermediate states and transition inputs before the
          ;; final state. Keep the same order for all projections at this k.
          (let loop ((i 1))
            (when (<= i depth)
              (when (< i depth) (collect-at state i))
              (collect-at inputs (- i 1))
              (loop (+ i 1))))
          (collect-at state depth)
          (let* ((vars (reverse! reversed))
                 (formulas::yices-term-pointer (make-yices-term-array (+ depth 1)))
                 (result (make-cdr-projection vars (list->yices-term-array vars) (length vars) formulas)))
            ;; The transition prefix is fixed at this depth. Only the last
            ;; slot changes from one blocking obligation to the next.
            (let loop ((i::long 0))
              (when (< i depth)
                (yices-term-array-set! formulas i (cdr-transition-at sys i))
                (loop (+ i 1))))
            (cdr-step-projection-set! step result)
            result)))))

(define (cdr-projection-result!::yices-term-vector-pointer sys)
  (or (cdr-system-projection-result sys)
      (let ((buffer::yices-term-vector-pointer (make-yices-term-vector)))
        (cdr-system-projection-result-set! sys buffer)
        buffer)))

(define (cdr-project sys model goal depth::long)
  (let* ((target::int32 (cdr-at sys goal depth))
         (projection (cdr-projection-data sys depth))
         (count (cdr-projection-count projection))
         (eliminate (cdr-projection-elimination-buffer projection))
         (formulas::yices-term-pointer (cdr-projection-formulas projection))
         (size (fixnum->uint32 (+ depth 1)))
         (buffer::yices-term-vector-pointer (cdr-projection-result! sys)))
    (yices-term-array-set! formulas depth target)
    (yices-reset-term-vector buffer)
    (cdr-count! 1)
    (verbose-message 5 "  project ~a: ~a variables" (vector-ref *cdr-statistics* 1) count)
    (cdr-minimize-generalization sys
      (if (= (yices-generalize-model-array model size formulas (fixnum->uint32 count) eliminate
                    (yices-enum YICES_GEN_DEFAULT) buffer) 0)
        (yices-term-vector-conjunction buffer)
        ;; Some theories require exact witness substitution in place of
        ;; the native projection. Either result guarantees a predecessor.
        (let ((values (map (lambda (var) (yices-get-value-as-term model var))
                           (cdr-projection-variables projection))))
          (when (exists (lambda (v) (< v 0)) values)
            (cdr-fail "exact model projection is unavailable for this theory"))
          (yices-clear-error)
          (cdr-count! 7)
          (cdr-ok (yices-subst-term (fixnum->uint32 count) eliminate
                                   (list->yices-term-array values) (cdr-ok (yices-and size formulas)))))))))

;; Obligation minimization

;; Native Yices represents conjunction by De Morgan's law. Traverse only
;; Boolean AND/OR structure, retaining every other expression as an atom.
;; Header constants avoid depending on enum values across Yices versions.
(define (cdr-boolean-components term conjunction?)
  (let ((seen (make-eq-hash-table 17))
        (identity (if conjunction? (yices-true) (yices-false))))
    (let walk ((pending (list (cons term #f))) (result '()))
      (if (null? pending) (sort result <)
        (let* ((entry (car pending)) (node (car entry)) (negative? (cdr entry))
               (kind (cdr-ok (yices-term-constructor node))))
          (cond
            ((= kind (yices-enum YICES_NOT_TERM))
             (walk (cons (cons (cdr-ok (yices-term-child node 0)) (not negative?))
                         (cdr pending)) result))
            ((and (= kind (yices-enum YICES_OR_TERM))
                  (eq? negative? conjunction?))
             (let children ((i 0) (n (cdr-ok (yices-term-num-children node)))
                            (rest (cdr pending)))
               (if (= i n) (walk rest result)
                 (children (+ i 1) n
                   (cons (cons (cdr-ok (yices-term-child node (fixnum->int32 i))) negative?) rest)))))
            (else
             (let ((atom (if negative? (cdr-not node) node)))
               (if (or (= atom identity) (cdr-term-table-ref seen atom))
                 (walk (cdr pending) result)
                 (begin
                   (cdr-term-table-set! seen atom #t)
                   (walk (cdr pending) (cons atom result))))))))))))

;; QuickXplain over a monotone sufficiency predicate. The complete candidate
;; set must be sufficient; recursive calls preserve this premise. Testing
;; whole subsets through cdr-query keeps temporary assertions out of the
;; logical solver's permanent assertion list and restores scopes on errors.
(define (cdr-minimize-subset candidates sufficient?)
  (let ((items (list->vector candidates)))
    (define (extend base begin end)
      (let loop ((i (- end 1)) (result base))
        (if (< i begin) result
          (loop (- i 1) (cons (vector-ref items i) result)))))
    (define (minimize base begin end)
      (cond
        ((= begin end) '())
        ((sufficient? base) '())
        ((= (+ begin 1) end) (list (vector-ref items begin)))
        (else
         (let* ((middle (+ begin (quotient (- end begin) 2)))
                (right (minimize (extend base begin middle) middle end))
                (left (minimize (append right base) begin middle)))
           (append left right)))))
    (minimize '() 0 (vector-length items))))

(define (cdr-minimization-solver sys)
  (or (cdr-system-minimization-solver sys)
      (let ((solver (cdr-new-solver sys '())))
        (cdr-system-minimization-solver-set! sys solver)
        solver)))

;; Removing a projection conjunct is permitted only when the remainder
;; implies the original projection. Thus the result stays equivalent to G,
;; preserving both its model witness and its existential path guarantee.
(define (cdr-minimize-generalization sys goal)
  (if (not (cdr-option 'minimize-generalizations)) goal
    (let ((parts (cdr-boolean-components goal #t))
          (negated (cdr-not goal)))
      (cdr-and
        (cdr-minimize-subset parts
          (lambda (subset)
            (cdr-unsat? sys (cdr-minimization-solver sys)
              (cdr-and (cons negated subset)))))))))

;; Before interpolation, discard goal conjuncts only while both antecedents
;; refute the weakened goal. Afterwards, discard explanation disjuncts only
;; while both antecedents imply the stronger explanation. Each retained
;; disjunct already refutes G, so this second pass preserves exclusion too.
(define (cdr-minimize-interpolant sys n formula disjunction?)
  (let ((initial (cdr-system-initial-solver sys))
        (previous (and (> n 0) (cdr-reach-solver sys (- n 1)))))
    (let* ((parts (cdr-boolean-components formula (not disjunction?)))
           (kept
             (cdr-minimize-subset parts
               (lambda (subset)
                 (let ((test (if disjunction? (cdr-not (cdr-or subset)) (cdr-and subset))))
                   (and (cdr-unsat? sys initial test)
                        (or (not previous)
                            (cdr-unsat? sys previous (cdr-at sys test 1)))))))))
      (if disjunction? (cdr-or kept) (cdr-and kept)))))

;; Keep every original property (distance zero), and remove other obligations
;; only if the retained forward formulas imply the complete original frame.
;; The resulting conjunction is equivalent, so neither the induction premise
;; nor the link from the learned invariant to the property is weakened.
(define (cdr-minimize-frame sys obligations)
  (if (not (cdr-option 'minimize-frames)) obligations
    (let* ((ordered (sort (append obligations '()) cdr-obligation-better?))
           (properties (filter (lambda (o) (= (cdr-obligation-distance o) 0)) ordered))
           (candidates (filter (lambda (o) (> (cdr-obligation-distance o) 0)) ordered))
           (background
             (cons (cdr-not (cdr-and (map cdr-obligation-forward ordered)))
                   (map cdr-obligation-forward properties)))
           (kept
             (cdr-minimize-subset candidates
               (lambda (subset)
                 (cdr-unsat? sys (cdr-minimization-solver sys)
                   (cdr-and (append background (map cdr-obligation-forward subset))))))))
      (sort (append properties kept) cdr-obligation-better?))))

;; Encoding a flattened SAL assertion

(define (cdr-word-reject sys)
  ((cdr-word-encoding-reject (cdr-system-word-encoding sys)) #f))

(define (cdr-word-width-for lower upper)
  ;; Two's complement contains [-2^(w-1), 2^(w-1)-1].
  (+ 1 (mpq/num-bits (mpq/max (-mpq *mpq-zero* lower) (+mpq upper *mpq-one*)))))

(define (cdr-word-bound-width interval)
  (and interval (cdr-word-width-for (car interval) (cdr interval))))

(define (cdr-type sys type)
  (cond
    ((sal-type/boolean? type) (yices-bool-type))
    ((sal-type/integer? type)
     (if (cdr-system-word-encoding sys)
       (let ((width (cdr-word-bound-width (cdr-type-interval type))))
         (if (and width (<= width 257))
           (cdr-ok (yices-bv-type (fixnum->uint32 width)))
           (cdr-word-reject sys)))
       (yices-int-type)))
    ((sal-type/number? type)
     (when (cdr-system-word-encoding sys) (cdr-word-reject sys))
     (yices-real-type))
    ((and (sal-type/scalar? type) (sal-type/cast type <sal-scalar-type>)) =>
     (lambda (scalar)
       ;; Qualified aliases are instantiated afresh by sal-type/cast.
       ;; Structural identity retains their context actuals while sharing
       ;; the native sort across equivalent instances and aliases.
       (cond ((sal-ast-table/get (cdr-system-type-map sys) scalar) => cdr)
             (else
              (let ((result (cdr-ok (yices-new-scalar-type (length (sal-scalar-type/elements scalar))))))
                (sal-ast-table/put! (cdr-system-type-map sys) scalar result)
                result)))))
    (else (sign-unsupported-feature type "sal-cdr requires Boolean, arithmetic, or scalar state variables after SAL data flattening."))))

(define (cdr-variable sys decl next?)
  (cond
    ((eq-hash-table/get (cdr-system-decl-map sys) decl) =>
     (lambda (entry) (if next? (cddr entry) (cadr entry))))
    ((eq-hash-table/get (cdr-system-definitions sys) decl) =>
     (lambda (entry) (cdr-encode sys (cddr entry) '() next?)))
    (else
     (let ((type (cdr-type sys (slot-value decl :type))))
       (let ((term (cdr-ok (yices-new-uninterpreted-term type))))
         ;; A free context constant is shared across the entire path.
         (eq-hash-table/put! (cdr-system-decl-map sys) decl (cons term term))
         (cdr-system-globals-set! sys (cons decl (cdr-system-globals sys)))
         term)))))

(define (cdr-global sys name)
  ;; Subrange bounds and named subtype predicates can still refer to the
  ;; original context constant after SAL has flattened its other uses.
  ;; Reuse SAL's qualified-name mapping, including context actuals, rather
  ;; than assigning another native variable to the original declaration.
  (let ((canonical
         (if (and (instance-of? name <sal-qualified-name-expr>)
                  (not (eq? (slot-value name :context-ref)
                            (sal-env/auxiliary-context (sal-ast/sal-env name)))))
           (let* ((aux (make-queue))
                  (names (sal-expr/flat-data-core name (make-empty-env) aux
                           (make-sal-df-info-using (cdr-system-module sys)))))
             (unless (and (queue/empty? aux) (pair? names) (null? (cdr names))
                          (instance-of? (car names) <sal-name-expr>))
               (sign-unsupported-feature name "sal-cdr requires atomic context constants after SAL data flattening."))
             (car names))
           name)))
    (cdr-variable sys (slot-value canonical :decl) #f)))

(define (cdr-encoded-node sys ast env next? build)
  (if (null? env)
    (let ((cache ((if next? cdr car) (cdr-system-expression-cache sys))))
      (cond ((eq-hash-table/get cache ast) => cdr)
            (else
             (let ((encoded (build)))
               (eq-hash-table/put! cache ast encoded)
               encoded))))
    (build)))

(define (cdr-word-require! encoding condition)
  (unless (= condition (yices-true))
    (cdr-word-encoding-requirements-set! encoding
      (cons condition (cdr-word-encoding-requirements encoding)))))

(define (cdr-word-capture encoding build)
  (let ((outer (cdr-word-encoding-requirements encoding)))
    (cdr-word-encoding-requirements-set! encoding '())
    (let* ((term (build)) (defined (cdr-and (cdr-word-encoding-requirements encoding))))
      (cdr-word-encoding-requirements-set! encoding outer)
      (cons term defined))))

(define (cdr-word-connective encoding operands conjunction?)
  ;; SAL simplification can turn a guarded conditional into AND, OR or
  ;; implication. A known absorbing operand determines the result even if
  ;; another operand contains a division that the guard makes irrelevant.
  (let ((defined (cdr-and (map cdr operands))))
    (unless (= defined (yices-true))
      (cdr-word-require! encoding
        (cdr-or
          (cons defined
            (map (lambda (operand)
                   (cdr-and (list (cdr operand) (if conjunction? (cdr-not (car operand)) (car operand)))))
                 operands))))))
  ((if conjunction? cdr-and cdr-or) (map car operands)))

(define (cdr-encode sys ast env next?)
  ;; Cache closed AST translations by identity and time polarity. For packed
  ;; integers also cache the condition under which that expression is defined.
  (let ((encoding (cdr-system-word-encoding sys)))
    (define (build) (cdr-encode-core sys ast env next?))
    (if encoding
      (let ((node (cdr-encoded-node sys ast env next?
                    (lambda () (cdr-word-capture encoding build)))))
        (cdr-word-require! encoding (cdr node))
        (car node))
      (cdr-encoded-node sys ast env next? build))))

(define (cdr-integer-literal expr)
  (and (instance-of? expr <sal-numeral>)
       (let ((number (slot-value expr :num)))
         (and (mpq/integer? number) number))))

(define (cdr-type-interval type)
  (and (sal-type/integer? type) (sal-type/bounded-subtype? type)
       (let ((lower (cdr-integer-literal (sal-ast/simplify (sal-bounded-subtype/lower type))))
             (upper (cdr-integer-literal (sal-ast/simplify (sal-bounded-subtype/upper type)))))
         (and lower upper (<=mpq lower upper) (cons lower upper)))))

(define (cdr-integer-interval expr)
  ;; A declared state range is available because I and T assert membership.
  ;; Other intervals follow from the operator, independently of any model.
  (cond
    ((cdr-integer-literal expr) => (lambda (number) (cons number number)))
    ((instance-of? expr <sal-next-operator>) (cdr-integer-interval (slot-value expr :name-expr)))
    ((instance-of? expr <sal-name-expr>)
     (let ((decl (slot-value expr :decl)))
       (and (instance-of? decl <sal-state-var-decl>) (cdr-type-interval (slot-value decl :type)))))
    ((instance-of? expr <sal-definition-expression>) (cdr-integer-interval (slot-value expr :expr)))
    ((instance-of? expr <sal-conditional>)
     (let ((left (cdr-integer-interval (slot-value expr :then-expr)))
           (right (cdr-integer-interval (slot-value expr :else-expr))))
       (and left right (cons (mpq/min (car left) (car right)) (mpq/max (cdr left) (cdr right))))))
    ((instance-of? expr <sal-mod>)
     (let ((number (cdr-integer-literal (cadr (sal-application/argument-list expr)))))
       (and number (not (=mpq number *mpq-zero*))
            (cons *mpq-zero* (-mpq (mpq/absolute number) *mpq-one*)))))
    ((instance-of? expr <sal-idiv>)
     (let* ((arguments (sal-application/argument-list expr))
            (divisor (cdr-integer-literal (cadr arguments)))
            (interval (and divisor (not (=mpq divisor *mpq-zero*))
                           (cdr-integer-interval (car arguments)))))
       (and interval
            (let ((left (div-mpq (car interval) divisor)) (right (div-mpq (cdr interval) divisor)))
              (cons (mpq/min left right) (mpq/max left right))))))
    ((instance-of? expr <sal-sub>)
     (let* ((arguments (sal-application/argument-list expr))
            (left (cdr-integer-interval (car arguments))))
       (and left
            (if (null? (cdr arguments))
              (cons (-mpq *mpq-zero* (cdr left)) (-mpq *mpq-zero* (car left)))
              (let ((right (cdr-integer-interval (cadr arguments))))
                (and right (cons (-mpq (car left) (cdr right)) (-mpq (cdr left) (car right)))))))))
    (else #f)))

(define (cdr-integer-range expr)
  (let ((interval (cdr-integer-interval expr)))
    (and interval
         (let ((bits (mpq/num-bits (+mpq (-mpq (cdr interval) (car interval)) *mpq-one*))))
           (and (<= bits 128) (cons (car interval) bits))))))

(define (cdr-product sys operands)
  ;; A bounded integer x=L+sum(2^i*b_i) multiplies any arithmetic term
  ;; through linear ITEs. The residual and selected contributions form a
  ;; shared DAG of O(log(U-L+1)) nodes, including signed and singleton ranges.
  ;; Recursing on the remaining operands also handles flattened n-ary products.
  (define (native-product)
    (let ((terms (map car operands)))
      (cdr-ok (if (and (pair? terms) (pair? (cdr terms)) (null? (cddr terms)))
                 (yices-mul (car terms) (cadr terms))
                 (yices-product (length terms) (list->yices-term-array terms))))))
  (let ((dynamic (filter (lambda (entry) (not (yices-arithmetic-constant? (car entry)))) operands)))
    (if (< (length dynamic) 2) (native-product)
      (let ((selected
              (let choose ((entries dynamic) (best #f))
                (cond ((null? entries) best)
                      ((and (cdar entries)
                            (or (not best) (< (cddar entries) (cddr best))))
                       (choose (cdr entries) (car entries)))
                      (else (choose (cdr entries) best))))))
        (if (not selected)
          (begin (cdr-system-nonlinear?-set! sys #t) (native-product))
          (let* ((lower (cadr selected)) (bits (cddr selected))
                 (factor (cdr-product sys (remq selected operands)))
                 (offset (cdr-ok (yices-parse-rational (mpq->string lower))))
                 (initial (cdr-ok (yices-mul offset factor))))
            (if (= bits 0) initial
              (let loop ((weight (mpq/exp *mpq-two* (integer->mpq (- bits 1))))
                         (remaining (cdr-ok (yices-sub (car selected) offset)))
                         (result initial))
                (let* ((coefficient (cdr-ok (yices-parse-rational (mpq->string weight))))
                       (set? (cdr-ok (yices-arith-geq-atom remaining coefficient)))
                       (contribution (cdr-ok (yices-ite set? (yices-mul coefficient factor) (yices-zero))))
                       (sum (cdr-ok (yices-add result contribution))))
                  (if (=mpq weight *mpq-one*) sum
                    (loop (/mpq weight *mpq-two*)
                          (cdr-ok (yices-ite set? (yices-sub remaining coefficient) remaining))
                          sum)))))))))))

(define (cdr-word-constant sys number)
  (let* ((cache (cdr-word-encoding-constants (cdr-system-word-encoding sys)))
         (key (mpq->string number)))
    (or (hashtable-get cache key)
        (let* ((width (cdr-word-width-for number number))
               (term
                (cond
                  ((> width 1024) (cdr-word-reject sys))
                  ((<= width 32)
                   (cdr-ok (yices-bvconst-int32 (fixnum->uint32 width) (fixnum->int32 (mpq->integer number)))))
                  (else
                   (let ((bits (make-string width #\0)) (magnitude (mpq/absolute number)))
                     (let fill ((i (- width 1)) (value magnitude))
                       (when (>= i 0)
                         (unless (=mpq (%mpq value *mpq-two*) *mpq-zero*)
                           (string-set! bits i #\1))
                         (fill (- i 1) (div-mpq value *mpq-two*))))
                     (let ((value (cdr-ok (yices-parse-bvbin bits))))
                       (if (<mpq number *mpq-zero*) (cdr-ok (yices-bvneg value)) value)))))))
          (hashtable-put! cache key term)
          term))))

(define (cdr-word-width term) (uint32->fixnum (yices-term-bitsize term)))

(define (cdr-word-resize term width)
  (let ((old (cdr-word-width term)))
    (cond ((= old width) term)
          ((< old width) (cdr-ok (yices-sign-extend term (fixnum->uint32 (- width old)))))
          (else (cdr-ok (yices-bvextract term 0 (fixnum->uint32 (- width 1))))))))

(define (cdr-word-binary procedure left right extra)
  (let ((width (+ extra (max (cdr-word-width left) (cdr-word-width right)))))
    (cdr-ok (procedure (cdr-word-resize left width) (cdr-word-resize right width)))))

(define (cdr-word-select condition then else)
  (if (= (yices-term-is-bitvector then) 1)
    (cdr-word-binary (lambda (left right) (yices-ite condition left right)) then else 0)
    (cdr-ok (yices-ite condition then else))))

(define (cdr-word-arithmetic sys terms product?)
  (cond
    ((null? terms) (if product? (yices-bvconst-one 2) (yices-bvconst-zero 1)))
    ((null? (cdr terms)) (car terms))
    (else
     (let* ((widths (map cdr-word-width terms))
            (count (length terms))
            (width (if product? (apply + widths)
                     (+ (apply max widths) (mpq/num-bits (integer->mpq count))))))
       (when (> width 1024) (cdr-word-reject sys))
       (let ((buffer (list->yices-term-array (map (lambda (term) (cdr-word-resize term width)) terms))))
         (cdr-ok ((if product? yices-bvproduct yices-bvsum) (fixnum->uint32 count) buffer)))))))

(define (cdr-positive-power-of-two number)
  (and number (>mpq number *mpq-zero*)
       (let ((exponent (mpq/num-bits number)))
         (and (=mpq number (mpq/exp *mpq-two* (integer->mpq exponent))) exponent))))

(define (cdr-word-division sys arguments left right modulo?)
  (let* ((divisor (cdr-integer-literal (cadr arguments)))
         (left-range (cdr-integer-interval (car arguments)))
         (right-range (cdr-integer-interval (cadr arguments)))
         (left-nonnegative? (and left-range (>=mpq (car left-range) *mpq-zero*)))
         (right-nonnegative? (and right-range (>=mpq (car right-range) *mpq-zero*)))
         (power (cdr-positive-power-of-two (and divisor (mpq/absolute divisor)))))
    (cdr-word-require! (cdr-system-word-encoding sys)
      (cdr-ok (yices-bvneq-atom right (yices-bvconst-zero (fixnum->uint32 (cdr-word-width right))))))
    (cond
      ((and modulo? power)
       ;; The unsigned low bits are the Euclidean remainder, including for
       ;; negative dividends. The added zero sign bit keeps that value signed.
       (if (= power 0) (yices-bvconst-zero 1)
         (cdr-ok (yices-zero-extend (cdr-word-resize left power) 1))))
      ((and (not modulo?) power left-nonnegative? right-nonnegative?)
       (let ((width (cdr-word-width left)))
         (if (>= power width) (yices-bvconst-zero 1)
           (cdr-ok (yices-bvextract left (fixnum->uint32 power) (fixnum->uint32 (- width 1)))))))
      ((and left-nonnegative? right-nonnegative?)
       (cdr-word-binary (if modulo? yices-bvrem yices-bvdiv) left right 0))
      (modulo?
       (cdr-word-binary
         (lambda (x y)
           ;; Widen before taking abs to avoid a signed minimum overflow.
           (let ((magnitude (yices-ite (yices-bitextract y (fixnum->uint32 (- (cdr-word-width y) 1)))
                                       (yices-bvneg y) y)))
             (yices-bvsmod x magnitude))) left right 1))
      (else (cdr-word-binary yices-bvsdiv left right 1)))))

(define (cdr-integer-division sys arguments terms modulo?)
  (let* ((left (car terms)) (divisor (cadr terms))
         (left-range (and (not modulo?) (cdr-integer-interval (car arguments))))
         (nonnegative? (and left-range (>=mpq (car left-range) *mpq-zero*))))
    (unless (yices-arithmetic-constant? divisor)
      (cdr-system-nonlinear?-set! sys #t))
    (if modulo?
      (cdr-ok (yices-imod left divisor))
      ;; The SAL evaluator truncates toward zero. Yices uses Euclidean
      ;; division, so divide the magnitudes before restoring the sign.
      (let* ((magnitude (if nonnegative? left (yices-abs left)))
             (quotient (cdr-ok (yices-idiv magnitude (yices-abs divisor))))
             (negative (yices-arith-lt0-atom divisor)))
        (cdr-ok (yices-ite (if nonnegative? negative
                            (yices-xor2 (yices-arith-lt0-atom left) negative))
                           (yices-neg quotient) quotient))))))

(define (cdr-integral-shift-core expression)
  ;; z+t and z-t have the same integrality as t for integral z. Return
  ;; the one unbounded summand, when this decomposition is explicit.
  (if (or (instance-of? expression <sal-add>) (instance-of? expression <sal-sub>))
    (let ((nonintegers (filter (lambda (arg) (not (sal-type/integer? (sal-expr/type arg))))
                              (sal-application/argument-list expression))))
      (if (and (pair? nonintegers) (null? (cdr nonintegers)))
        (cdr-integral-shift-core (car nonintegers))
        expression))
    expression))

(define (cdr-encode-core sys ast env next?)
  (define encoding (cdr-system-word-encoding sys))
  (define (translate-child expr) (cdr-encode sys expr env next?))
  (define (args) (map translate-child (sal-application/argument-list ast)))
  (define (word-args)
    (map (lambda (child) (cdr-word-capture encoding (lambda () (translate-child child))))
         (sal-application/argument-list ast)))
  (define (binary proc #!optional (word-proc #f))
    (let ((a (args)))
      (if (and encoding word-proc (= (yices-term-is-bitvector (car a)) 1))
        (cdr-word-binary word-proc (car a) (cadr a) 0)
        (cdr-ok (proc (car a) (cadr a))))))
  (cond
    ((instance-of? ast <sal-true>) (yices-true))
    ((instance-of? ast <sal-false>) (yices-false))
    ((instance-of? ast <sal-numeral>)
     (let ((number (slot-value ast :num)))
       (if encoding
         (begin (unless (mpq/integer? number) (cdr-word-reject sys)) (cdr-word-constant sys number))
         (cdr-ok (yices-parse-rational (mpq->string number))))))
    ((instance-of? ast <sal-scalar>)
     (let* ((type (cdr-type sys (sal-expr/type ast)))
            (index (sal-scalar-element/idx ast))
            (term (cdr-ok (yices-constant type (fixnum->int32 index)))))
       (verbose-message 5 "  scalar ~a: type ~a, index ~a, term ~a" (sal-decl/name (slot-value ast :decl)) type index term)
       term))
    ((instance-of? ast <sal-next-operator>)
     (when next? (sign-unsupported-feature ast "Nested next-state expression."))
     (cdr-encode sys (slot-value ast :name-expr) env #t))
    ((instance-of? ast <sal-definition-expression>) (translate-child (slot-value ast :expr)))
    ((instance-of? ast <sal-name-expr>)
     (let ((decl (slot-value ast :decl)))
       (cond ((assq decl env) => cdr)
             ((instance-of? decl <sal-state-var-decl>) (cdr-variable sys decl next?))
             ((sal-name-expr/definition ast) => translate-child)
             ((instance-of? decl <sal-constant-decl>) (cdr-global sys ast))
             (else (sign-unsupported-feature ast "Unbound name in sal-cdr term encoding.")))))
    ((instance-of? ast <sal-let-expr>)
     (let* ((decls (slot-value ast :local-decls))
            (bindings (map (lambda (d) (cons d (translate-child (slot-value d :value)))) decls)))
       (cdr-encode sys (slot-value ast :expr) (append bindings env) next?)))
    ((instance-of? ast <sal-conditional>)
     (let ((condition (translate-child (slot-value ast :cond-expr))))
       (if encoding
         (let* ((left (cdr-word-capture encoding (lambda () (translate-child (slot-value ast :then-expr)))))
                (right (cdr-word-capture encoding (lambda () (translate-child (slot-value ast :else-expr))))))
           ;; A branch only has to be defined when the condition selects it.
           (cdr-word-require! encoding (cdr-ok (yices-ite condition (cdr left) (cdr right))))
           (cdr-word-select condition (car left) (car right)))
         (cdr-ok (yices-ite condition
                   (translate-child (slot-value ast :then-expr)) (translate-child (slot-value ast :else-expr)))))))
    ((instance-of? ast <sal-and>)
     (if encoding (cdr-word-connective encoding (word-args) #t) (cdr-and (args))))
    ((instance-of? ast <sal-or>)
     (if encoding (cdr-word-connective encoding (word-args) #f) (cdr-or (args))))
    ((instance-of? ast <sal-not>) (cdr-not (translate-child (slot-value ast :arg))))
    ((instance-of? ast <sal-implies>)
     (if encoding
       (let* ((parts (word-args)) (left (car parts)))
         (cdr-word-connective encoding (cons (cons (cdr-not (car left)) (cdr left)) (cdr parts)) #f))
       (binary yices-implies)))
    ((instance-of? ast <sal-iff>) (binary yices-iff))
    ((instance-of? ast <sal-xor>) (binary yices-xor2))
    ((instance-of? ast <sal-eq>)
     (let* ((left (car (sal-application/argument-list ast)))
            (definition (and (instance-of? left <sal-name-expr>)
                             (eq-hash-table/get (cdr-system-definitions sys) (slot-value left :decl)))))
       (if (and definition (eq? ast (cadr definition)))
         ;; The replaced equality is true by construction. Still encode the
         ;; operands so globals and packed division requirements are retained.
         (begin (args) (yices-true))
         (binary yices-eq yices-bveq-atom))))
    ((instance-of? ast <sal-diseq>) (binary yices-neq yices-bvneq-atom))
    ((instance-of? ast <sal-lt>) (binary yices-arith-lt-atom yices-bvslt-atom))
    ((instance-of? ast <sal-le>) (binary yices-arith-leq-atom yices-bvsle-atom))
    ((instance-of? ast <sal-gt>) (binary yices-arith-gt-atom yices-bvsgt-atom))
    ((instance-of? ast <sal-ge>) (binary yices-arith-geq-atom yices-bvsge-atom))
    ((instance-of? ast <sal-add>)
     (let ((a (args)))
       (cond (encoding (cdr-word-arithmetic sys a #f))
             ((and (pair? a) (pair? (cdr a)) (null? (cddr a)))
              (cdr-ok (yices-add (car a) (cadr a))))
             (else (cdr-ok (yices-sum (length a) (list->yices-term-array a)))))))
    ((instance-of? ast <sal-sub>)
     (let ((a (args)))
       (if encoding
         (if (null? (cdr a)) (cdr-ok (yices-bvneg (cdr-ok (yices-sign-extend (car a) 1))))
           (cdr-word-binary yices-bvsub (car a) (cadr a) 1))
         (cdr-ok (if (null? (cdr a)) (yices-neg (car a)) (yices-sub (car a) (cadr a)))))))
    ((instance-of? ast <sal-mul>)
     (if encoding (cdr-word-arithmetic sys (args) #t)
       (let ((expressions (sal-application/argument-list ast)))
         (cdr-product sys
           (map (lambda (expr) (cons (translate-child expr) (cdr-integer-range expr))) expressions)))))
    ((instance-of? ast <sal-div>)
     (when encoding (cdr-word-reject sys))
     (let ((a (args)))
       (unless (yices-arithmetic-constant? (cadr a)) (cdr-system-nonlinear?-set! sys #t))
       (cdr-ok (yices-division (car a) (cadr a)))))
    ((or (instance-of? ast <sal-idiv>) (instance-of? ast <sal-mod>))
     (let ((arguments (sal-application/argument-list ast)) (terms (args))
           (modulo? (instance-of? ast <sal-mod>)))
       (if encoding (cdr-word-division sys arguments (car terms) (cadr terms) modulo?)
         (cdr-integer-division sys arguments terms modulo?))))
    ((or (instance-of? ast <sal-min>) (instance-of? ast <sal-max>))
     (let* ((a (args)) (left (car a)) (right (cadr a)))
       (if encoding
         (cdr-word-binary (lambda (x y)
                           (yices-ite ((if (instance-of? ast <sal-min>) yices-bvsle-atom yices-bvsge-atom) x y) x y))
                          left right 0)
         (cdr-ok (yices-ite ((if (instance-of? ast <sal-min>) yices-arith-leq-atom yices-arith-geq-atom) left right)
                            left right)))))
    ((instance-of? ast <sal-int-pred>)
     (if encoding (begin (args) (yices-true))
       (let* ((operand (car (sal-application/argument-list ast)))
              (original (translate-child operand))
              (core (cdr-integral-shift-core operand))
              (term (if (eq? core operand) original (translate-child core))))
         (unless (= (yices-term-is-int term) 1) (cdr-system-integrality?-set! sys #t))
         (cdr-ok (yices-is-int-atom term)))))
    ((instance-of? ast <sal-real-pred>) (args) (yices-true))
    (else
     (when encoding (cdr-word-reject sys))
     (sign-unsupported-feature ast "Unsupported residual SAL expression in sal-cdr."))))

(define (cdr-membership-expr decl)
  (let ((type (slot-value decl :type)))
    (and (sal-type/nontrivial-membership-expr? type)
         (sal-ast/simplify (sal-type/membership-expr type (make-sal-name-expr decl))))))

(define (cdr-membership sys decl)
  (let ((expr (cdr-membership-expr decl)))
    (and expr
         (let ((term (cdr-encode sys expr '() #f)))
           (and (not (= term (yices-true))) term)))))

(define (cdr-find-transient-inputs sys assertion)
  ;; Quotient away only transition-local, freely chosen current inputs.
  ;; Neither I, P, state constraints, nor a primed transition occurrence may
  ;; observe them. Then an incoming edge's endpoint input can be replaced by
  ;; the outgoing edge's witness, preserving all retained state variables.
  ;; Projection and Craig interpolation consequently keep every A/C input-free.
  ;; Inputs used by a property, initialization, dependent type, or next-state
  ;; constraint remain ordinary state variables. This deliberately conservative
  ;; criterion also retains inputs with nontrivial domain constraints.
  (let* ((module (cdr-system-module sys))
         (blocked (make-eq-hash-table))
         (seen (make-eq-hash-table))
         (definitions (cdr-system-definitions sys))
         (discarded (make-eq-hash-table))
         (decls (cdr-system-declarations sys)))
    (define (block-names ast #!optional (definition? #f))
      (sal-ast/for-each+
        (lambda (node)
          (cond
            ((or (and definition? (eq-hash-table/get discarded node))
                 (and (not definition?) (eq-hash-table/get seen node))) #f)
            (else
             (unless definition? (eq-hash-table/put! seen node #t))
             (when (instance-of? node <sal-name-expr>)
               (let* ((decl (slot-value node :decl)) (definition (eq-hash-table/get definitions decl)))
                 (if definition (block-names (cddr definition)) (eq-hash-table/put! blocked decl #t))))
             #t))) ast (lambda (node) #unspecified)))
    (eq-hash-table/for-each
      (lambda (decl definition) (eq-hash-table/put! discarded (car definition) #t)) definitions)
    (block-names (sal-module-models/invariant-body assertion))
    (for-each (lambda (slot) (block-names (slot-value module slot)))
              '(:initialization :valid-state-expr :valid-input-expr :valid-constant-expr))
    ;; An inlined equality imposes no constraint on an input. Constraints
    ;; that remain, including declared ranges on the alias, still protect it.
    (block-names (slot-value module :definition) #t)
    (for-each (lambda (decl)
                (let ((membership (cdr-membership-expr decl)))
                  ;; SAL can leave REAL?(x)/INTEGER?(x) here even when
                  ;; the native sort already makes that predicate true.
                  (when (and membership (not (= (cdr-encode sys membership '() #f) (yices-true))))
                    (block-names membership)))) (cdr-system-trace-declarations sys))
    (sal-ast/for-each
      (lambda (node)
        (when (instance-of? node <sal-next-operator>) (block-names (slot-value node :name-expr))))
      (slot-value module :transition))
    (map-and-filter
      (lambda (decl)
        (and (instance-of? decl <sal-input-state-var-decl>)
             (not (eq-hash-table/get blocked decl))
             (cdr-variable sys decl #f))) decls)))

(define (cdr-state-definitions module)
  (let ((candidates (make-eq-hash-table)) (accepted (make-eq-hash-table))
        (marks (make-eq-hash-table)))
    (define (collect expression env)
      (cond
        ((instance-of? expression <sal-and>)
         (for-each (lambda (arg) (collect arg env)) (sal-application/argument-list expression)))
        ((instance-of? expression <sal-definition-expression>)
         (collect (slot-value expression :expr) env))
        ((instance-of? expression <sal-let-expr>)
         (let* ((decls (slot-value expression :local-decls))
                (values (map (lambda (decl) (sal-ast/substitute (slot-value decl :value) env)) decls)))
           (collect (slot-value expression :expr) (update-env* env decls values))))
        ((instance-of? expression <sal-eq>)
         (let* ((args (sal-application/argument-list expression)) (left (car args)))
           (when (instance-of? left <sal-name-expr>)
             (let ((decl (slot-value left :decl)))
               (when (and (instance-of? decl <sal-state-var-decl>)
                          (not (instance-of? decl <sal-input-state-var-decl>)))
                 (let ((right (sal-ast/substitute (cadr args) env)))
                   (eq-hash-table/put! candidates decl
                     (and (not (eq-hash-table/get candidates decl))
                          (or (not (sal-type/integer? (slot-value decl :type)))
                              (sal-type/integer? (sal-expr/type right)))
                          (cons expression right)))))))))))
    (define (acyclic? decl source)
      (cond
        ((eq-hash-table/get marks decl) => (lambda (entry) (eq? (cdr entry) 'accepted)))
        (else
         (eq-hash-table/put! marks decl 'visiting)
         (let* ((visited (make-eq-hash-table))
                (ok?
                 (bind-exit (reject)
                   (sal-ast/for-each+
                     (lambda (node)
                       (cond
                         ((eq-hash-table/get visited node) #f)
                         ((instance-of? node <sal-next-operator>) (reject #f))
                         (else
                          (eq-hash-table/put! visited node #t)
                          (when (instance-of? node <sal-name-expr>)
                            (let* ((dep (slot-value node :decl)) (candidate (eq-hash-table/get candidates dep)))
                              (when (and candidate (cdr candidate) (not (acyclic? dep (cdr candidate))))
                                (reject #f))))
                          #t)))
                     (cdr source) (lambda (node) #unspecified))
                   #t)))
           (eq-hash-table/put! marks decl (if ok? 'accepted 'rejected))
           (when ok? (eq-hash-table/put! accepted decl source))
           ok?))))
    ;; Only unconditional, functional equations are candidates. A cycle or
    ;; a second definition leaves the original variable and equations intact.
    (collect (slot-value module :definition) (make-empty-env))
    (eq-hash-table/for-each (lambda (decl source) (when source (acyclic? decl source))) candidates)
    accepted))

(define (cdr-word-candidate? assertion definitions)
  (let* ((logic (cdr-solver-logic))
         (bitvector-logic?
           (and logic (>= (string-length logic) 2)
                (string=? (substring logic (- (string-length logic) 2) (string-length logic)) "BV")))
         (module (slot-value assertion :module))
         (integers (filter (lambda (decl) (and (not (eq-hash-table/get definitions decl))
                                               (sal-type/number? (slot-value decl :type))))
                           (sal-module/state-variables module))))
    (and (or (not logic) bitvector-logic?)
         (pair? integers)
         (every (lambda (decl)
                  (let ((width (cdr-word-bound-width (cdr-type-interval (slot-value decl :type)))))
                    (and width (<= width 257)))) integers)
         (or bitvector-logic?
             (bind-exit (found)
               (define (inspect node)
                 (when (or (instance-of? node <sal-idiv>) (instance-of? node <sal-mod>)
                           (and (instance-of? node <sal-mul>)
                                (> (length (filter (lambda (arg) (not (instance-of? arg <sal-numeral>)))
                                                   (sal-application/argument-list node))) 1)))
                   (found #t)))
               (sal-ast/for-each inspect (sal-module-models/invariant-body assertion))
               (sal-ast/for-each inspect (slot-value module :initialization))
               (sal-ast/for-each inspect (slot-value module :transition))
               #f)))))

(define (cdr-check-word-definedness! sys)
  (let ((encoding (cdr-system-word-encoding sys)))
    (when (and encoding (pair? (cdr-word-encoding-requirements encoding)))
      ;; Restrict the check by declared integer ranges only. In particular,
      ;; a possibly undefined user definition cannot justify its own value.
      (let* ((decls (append (cdr-system-declarations sys) (cdr-system-globals sys)))
             (range
              (cdr-and
                (map-and-filter
                  (lambda (decl)
                    (let ((var (cdr-variable sys decl #f)))
                      (and (= (yices-term-is-bitvector var) 1)
                           (let ((bounds (cdr-type-interval (slot-value decl :type))))
                             (cdr-and
                               (list (cdr-word-binary yices-bvsge-atom var (cdr-word-constant sys (car bounds)) 0)
                                     (cdr-word-binary yices-bvsle-atom var (cdr-word-constant sys (cdr bounds)) 0)))))))
                  decls)))
             (bad (cdr-and (list range (cdr-at sys range 1)
                                 (cdr-not (cdr-and (cdr-word-encoding-requirements encoding))))))
             (check (cdr-new-solver-mode sys '() #f))
             (result (cdr-query-native sys check bad #f)))
        (cdr-retire-solver! sys check)
        (when result (cdr-word-reject sys))))))

(define (cdr-setup! sys assertion)
  (let* ((module (slot-value assertion :module))
         (source-decls (sal-module/state-variables module))
         (definitions (cdr-system-definitions sys))
         (decls (filter (lambda (decl) (not (eq-hash-table/get definitions decl))) source-decls)))
    (cdr-system-module-set! sys module)
    (cdr-system-declarations-set! sys decls)
    (cdr-system-trace-declarations-set! sys source-decls)
    (cdr-system-native-types-set! sys (map (lambda (decl) (cdr-type sys (slot-value decl :type))) decls))
    (let ((current (cdr-step-variables (cdr-step-at sys 0)))
          (next (cdr-step-variables (cdr-step-at sys 1))))
      (for-each (lambda (decl a b) (eq-hash-table/put! (cdr-system-decl-map sys) decl (cons a b)))
                decls current next)
      (cdr-system-base-count-set! sys (* 2 (length decls)))
      (cdr-system-base-buffer-set! sys (list->yices-term-array (append current next))))
    (when (exists (lambda (decl) (instance-of? decl <sal-input-state-var-decl>)) decls)
      (cdr-system-transient-inputs-set! sys (cdr-find-transient-inputs sys assertion)))
    (let* ((property (cdr-encode sys (sal-module-models/invariant-body assertion) '() #f))
           (initial (cdr-encode sys (slot-value module :initialization) '() #f))
           (transition (cdr-encode sys (slot-value module :transition) '() #f))
           (domain
             (append (map-and-filter (lambda (decl) (cdr-membership sys decl)) source-decls)
               (map (lambda (slot) (cdr-encode sys (slot-value module slot) '() #f))
                    '(:definition :valid-state-expr :valid-input-expr :valid-constant-expr)))))
      ;; Memberships may themselves discover additional global parameters.
      (let loop ((done '()))
        (let ((todo (filter (lambda (d) (not (memq d done))) (cdr-system-globals sys))))
          (unless (null? todo)
            (set! domain (append (map-and-filter (lambda (d) (cdr-membership sys d)) todo) domain))
            (loop (append todo done)))))
      (let ((domain (cdr-and domain))
            (integral-facts (cdr-transition-substitution sys (slot-value module :transition))))
        (cdr-system-domain-set! sys domain)
        (cdr-system-initial-set! sys (cdr-and (list domain initial)))
        (cdr-system-transition-set! sys (cdr-and (append (list domain transition (cdr-at sys domain 1)) integral-facts))))
      (cdr-check-word-definedness! sys)
      (cdr-system-initial-solver-set! sys (cdr-new-solver sys (list (cdr-system-initial sys))))
      (when (cdr-system-word-encoding sys) (verbose-message 3 "  Using packed bounded integers."))
      (when (> (eq-hash-table/size definitions) 0)
        (verbose-message 3 "  Inlined ~a state definitions." (eq-hash-table/size definitions)))
      (when (>= (verbosity-level) 5)
        (for-each (lambda (name term)
                    (print name ":")
                    (flush-output-port (current-output-port))
                    (cdr-ok (yices-pp-term-fd 1 term 120 100 0)))
                  '(initial transition property)
                  (list (cdr-system-initial sys) (cdr-system-transition sys) property)))
      property)))

(define (cdr-empty-system encoding definitions)
  (make-cdr-system #f '() (make-eq-hash-table) (make-sal-ast-table) '()
    (make-vector 8) '() 0 #f #f #f #f #f '() '() #f (make-vector 8) #f
    (make-yices-interpolation-record)
    (cons (make-eq-hash-table) (make-eq-hash-table)) '()
    #f (and (cdr-option 'cex-graph) (make-eq-hash-table 17))
    (and (cdr-option 'cex-graph) (make-eq-hash-table 17))
    encoding #f #f definitions '() (cdr-option 'smt2-output) #f))

;; Invariants and counterexamples

(define (sal-cdr/display-invariant)
  (unless *cdr-invariant* (sign-error "No proved sal-cdr invariant is available."))
  ;; Give the native symbols stable, unambiguous names and retain their SAL
  ;; origin. Naming affects printing only, never terms or solver semantics.
  (let loop ((bindings (vector-ref *cdr-invariant* 2)) (i 0))
    (unless (null? bindings)
      (let* ((binding (car bindings))
             (name (string-append "cdr" (number->string (vector-ref *cdr-invariant* 3))
                                 "_" (number->string i))))
        (cdr-ok (yices-set-term-name (cdr binding) name))
        (print "; " name " = " (sal-decl/name (car binding)))
        (when (= (yices-term-is-bitvector (cdr binding)) 1)
          (print "; " name " has a signed " (cdr-word-width (cdr binding)) "-bit encoding.")))
      (loop (cdr bindings) (+ i 1))))
  (print "(invariant " (vector-ref *cdr-invariant* 0) " "
         (yices-term-text (vector-ref *cdr-invariant* 1)) ")"))

(define (cdr-save-invariant! sys obligations depth)
  (set! *cdr-invariant*
    ;; Native integer sorts do not intrinsically encode SAL subranges. Keep
    ;; their domain in the exported certificate, including when arithmetic
    ;; has been lowered using those bounds. T guarantees it at every step.
    (vector depth (cdr-and (cons (cdr-system-domain sys)
                                (map cdr-obligation-forward obligations)))
      (append (map cons (cdr-system-declarations sys)
                        (cdr-step-variables (cdr-step-at sys 0)))
              (map (lambda (decl) (cons decl (cdr-variable sys decl #f)))
                   (cdr-system-globals sys)))
      *cdr-query-id*)))

(define (cdr-value model decl term)
  (let* ((type (slot-value decl :type)) (value (yices-get-value-as-term model term)))
    (cond
      ((< value 0) #f)                    ; e.g., an algebraic irrational
      ((sal-type/boolean? type)
       (if (= value (yices-true)) (make-sal-true decl) (make-sal-false decl)))
      ((= (yices-term-is-bitvector term) 1)
       (make-sal-numeral (cdr-word-value model term) decl))
      ((sal-type/number? type)
       (make-sal-numeral (make-mpq (yices-term-text value)) decl))
      ((and (sal-type/scalar? type) (sal-type/cast type <sal-scalar-type>)) =>
       (lambda (scalar)
         (let ((index (make-yices-int32-array 1)))
           (cdr-ok (yices-get-scalar-value model term index))
           (list-ref (sal-scalar-type/elements scalar)
                     (int32->fixnum (yices-int32-array-ref index 0))))))
      (else #f))))

(define (cdr-word-value model term)
  (let* ((width (cdr-word-width term))
         (bits::yices-int32-pointer (make-yices-int32-array width)))
    (cdr-ok (yices-get-bv-value model term bits))
    (let ((sign::long (- width 1)))
      (let loop ((i::long (- sign 1))
                 (number (if (= (yices-int32-array-ref bits sign) 0) *mpq-zero*
                            (-mpq *mpq-zero* *mpq-one*))))
        (if (< i 0) number
          (let ((shifted (*mpq number *mpq-two*)))
            (loop (- i 1)
              (if (= (yices-int32-array-ref bits i) 0) shifted (+mpq shifted *mpq-one*)))))))))

;; Re-solve the complete witness against I,T,!P before reporting INVALID.
;; This also checks the composition of all existential generalizations.
(define (cdr-counterexample sys property depth)
  (let ((solver (cdr-new-solver sys (list (cdr-system-initial sys)))))
    (let loop ((i 0))
      (when (< i depth)
        (cdr-assert! solver (cdr-transition-at sys i))
        (loop (+ i 1))))
    (let ((model (cdr-query sys solver (cdr-at sys (cdr-not property) depth))))
      (unless model (sign-error "sal-cdr: counterexample reconstruction failed."))
      (let* ((exact-sal? #t)
             (declarations (cdr-system-trace-declarations sys))
             (base-terms (map (lambda (decl) (cdr-variable sys decl #f)) declarations))
             (steps
              (let loop ((i 0) (result '()))
                (if (> i depth) (reverse! result)
                  (let ((table (make-eq-hash-table)))
                    (for-each
                      (lambda (decl term)
                        (let ((value (cdr-value model decl (cdr-at sys term i))))
                          (if value (eq-hash-table/put! table decl value) (set! exact-sal? #f))))
                      declarations base-terms)
                    (loop (+ i 1)
                      (cons (make-instance <sal-step> :assignment-table table
                                :constraint-list '() :transition-step #f) result))))))
             (global-constraints
               (map-and-filter
                 (lambda (decl)
                   (let ((value (cdr-value model decl (cdr-variable sys decl #f))))
                     (if value (make-sal-equality (make-sal-name-expr decl) value)
                       (begin (set! exact-sal? #f) #f))))
                 (cdr-system-globals sys))))
        (let ((result
          (if exact-sal?
            (make-instance <sal-concrete-path> :flat-module (cdr-system-module sys)
              :step-info-list steps :auxiliary-decls '() :global-constraint-list global-constraints)
            (begin
              ;; The normal Yices pretty-printer rounds algebraics to doubles.
              ;; The non-pretty FFI printer preserves defining polynomials and
              ;; isolating intervals. Keep the solver's exact witness here.
              (print "Counterexample (exact Yices2 values):")
              (for-each (lambda (decl)
                          (display* "Constant " (sal-decl/name decl) " = ")
                          (flush-output-port (current-output-port))
                          (cdr-ok (yices-print-term-values-fd 1 model 1
                                   (list->yices-term-array (list (cdr-variable sys decl #f)))))
                          (newline))
                        (cdr-system-globals sys))
              (let loop ((i 0))
                (when (<= i depth)
                  (print "Step " i ":")
                  (for-each (lambda (decl term)
                              (display* "  " (sal-decl/name decl) " = ")
                              (flush-output-port (current-output-port))
                              (cdr-ok (yices-print-term-values-fd 1 model 1 (list->yices-term-array (list (cdr-at sys term i)))))
                              (newline))
                            declarations base-terms)
                  (loop (+ i 1))))
              #unspecified))))
          (cdr-release-model! sys model)
          result)))))

;; Options and statistics

(define *cdr-initialized?* #f)
(define *cdr-statistic-fields*
  '(("checks" . "checks") ("projections" . "projections")
    ("interpolations" . "interpolations") ("refinements" . "refinements")
    ("frames" . "pdkfi") ("induction-depth" . "pdkid")
    ("contexts" . "contexts") ("projection-substitutions" . "projection-substitutions")
    ("frame-size" . "pdkfs") ("frame-pushed" . "pdkfp")
    ("queue-size" . "pdkqs") ("max-cex-depth" . "pdkmcd")
    ("reachable" . "pdkrr") ("unreachable" . "pdkru") ("reachability-queries" . "pdkrq")))
(define *cdr-statistics* (make-vector (length *cdr-statistic-fields*) 0))
(define cdr-stat-frame-size 8)
(define cdr-stat-frame-pushed 9)
(define cdr-stat-queue-size 10)
(define cdr-stat-max-cex-depth 11)
(define cdr-stat-reachable 12)
(define cdr-stat-unreachable 13)
(define cdr-stat-reachability-queries 14)
(define *cdr-statistics-observer* #f)
(define *cdr-abort* #f)
(define *cdr-options*
  (map (lambda (entry) (cons (car entry) (cdr entry)))
  '((max-frame . 0) (induction-max . 0) (solver-mode . auto)
    (solver-logic . #f) (solver-trace-tags . #f) (smt2-output . #f)
    (add-backward . #f) (check-deadlock . #f)
    (minimize-interpolants . #f) (minimize-generalizations . #f)
    (minimize-frames . #f) (cex-graph . #f))))
(define *cdr-invariant* #f)
(define *cdr-query-id* 0)
(define *cdr-smt2-query-index* 0)

(define (cdr-option key) (cdr (assq key *cdr-options*)))

(define (cdr-solver-logic)
  (and (not (eq? (cdr-option 'solver-mode) 'mcsat))
       (cdr-option 'solver-logic)))

(define (sal-cdr/set-option! key value)
  (let ((entry (assq key *cdr-options*)))
    (unless entry (sign-error "Unknown sal-cdr option: ~a" key))
    (cond
      ((memq key '(max-frame induction-max))
       (unless (and (integer? value) (>= value 0))
         (sign-error "sal-cdr option ~a requires a nonnegative integer." key)))
      ((eq? key 'solver-mode)
       (when (string? value) (set! value (string->symbol value)))
       (unless (memq value '(auto dpllt mcsat hybrid))
         (sign-error "sal-cdr solver mode must be auto, dpllt, mcsat, or hybrid.")))
      ((memq key '(cex-graph solver-logic solver-trace-tags smt2-output))
       (unless (or (not value) (and (string? value) (> (string-length value) 0)))
         (sign-error "sal-cdr option ~a requires a nonempty string." key)))
      (else
       (unless (boolean? value) (sign-error "sal-cdr option ~a requires a Boolean." key))))
    (set-cdr! entry value)))

(define (cdr-count! index)
  (vector-set! *cdr-statistics* index (+ 1 (vector-ref *cdr-statistics* index))))

(define (sal-cdr/statistics)
  (map (lambda (field value) (list (car field) (cdr field) value))
       *cdr-statistic-fields* (vector->list *cdr-statistics*)))

(define (sal-cdr/display-statistics)
  (for-each (lambda (field) (print "cdr::" (car field) " = " (caddr field)))
            (sal-cdr/statistics)))

(define (sal-cdr/with-statistics-observer observer thunk)
  (let ((previous *cdr-statistics-observer*))
    (unwind-protect
      (begin (set! *cdr-statistics-observer* observer) (thunk))
      (set! *cdr-statistics-observer* previous))))

(define (sal-cdr/reset!)
  ;; The native front end owns the Yices instance and calls this between
  ;; batch queries, after printing the previous result and its invariant.
  (set! *cdr-invariant* #f)
  (vector-fill! *cdr-statistics* 0)
  (when *cdr-initialized?* (yices-reset)))

;; Counterexample graph

;; An edge records a symbolic guarantee: every source state has some path
;; of the given length to the target region. It is not a shortest concrete
;; witness. Multiple guarantees between the same regions retain the shortest
;; recorded length; this does not alter the proof search or its witnesses.
(define (cdr-record-edge! sys from to depth)
  (when (cdr-option 'cex-graph)
    (let* ((graph (cdr-system-graph sys))
           (edges (or (cdr-term-table-ref graph from) '())))
      (unless (cdr-term-table-ref graph to) (cdr-term-table-set! graph to '()))
      (let find ((rest edges))
        (cond
          ((null? rest) (cdr-term-table-set! graph from (cons (cons to depth) edges)))
          ((= (caar rest) to)
           (when (< depth (cdar rest)) (set-cdr! (car rest) depth)))
          (else (find (cdr rest))))))))

(define (cdr-mark-root! sys region)
  (when (cdr-option 'cex-graph)
    (cdr-term-table-set! (cdr-system-roots sys) region #t)
    (unless (cdr-term-table-ref (cdr-system-graph sys) region)
      (cdr-term-table-set! (cdr-system-graph sys) region '()))))

(define (cdr-dot-escape text)
  (let ((port (open-output-string)))
    (let loop ((i 0))
      (when (< i (string-length text))
        (let ((ch (string-ref text i)))
          (case ch
            ((#\\) (display "\\\\" port))
            ((#\") (display "\\\"" port))
            ((#\newline) (display "\\n" port))
            ((#\return) (display "\\r" port))
            ;; DOT accepts literal tabs; \t is not a DOT label escape.
            (else (write-char ch port))))
        (loop (+ i 1))))
    (close-output-port port)))

(define (cdr-write-graph! sys path)
  (when (cdr-option 'cex-graph)
    (let* ((graph (cdr-system-graph sys))
           (nodes (sort (eq-hash-table/fold-keys (lambda (ids id) (cons id ids)) '() graph) <)))
      (call-with-output-file path
        (lambda (port)
          (fprint port "digraph sal_cdr {")
          (fprint port "  rankdir=LR;")
          (for-each
            (lambda (id)
              (let ((term (fixnum->int32 id)))
                (fprint port "  n" id
                  " [shape=" (if (cdr-term-table-ref (cdr-system-roots sys) term) "doublecircle" "box")
                  ", label=\""
                  (cdr-dot-escape (yices-term-text term 120))
                  "\"];")
                (for-each
                  (lambda (edge)
                    (fprint port "  n" id " -> n" (int32->fixnum (car edge))
                      " [label=\"" (cdr edge) "\"];"))
                  (sort (append (cdr-term-table-ref graph term) '())
                        (lambda (a b) (< (car a) (car b)))))))
            nodes)
          (fprint port "}"))))))

;; SMT-LIB query logs

;; Yices prints canonical terms as S-expressions, including exact rationals
;; and polynomial coefficients. Translate that notation for query replay.
(define (cdr-smt2-protect-integers text)
  ;; SAL's GMP setup can change how Bigloo reads bignums. Read long decimal
  ;; literals as strings and write their original digits into the replay.
  (let ((length (string-length text)))
    (define (separator? i)
      (or (< i 0) (= i length)
          (let ((ch (string-ref text i)))
            (or (char-whitespace? ch) (char=? ch #\() (char=? ch #\))))))
    (let loop ((i 0) (start 0) (port #f))
      (cond
        ((= i length)
         (if port
           (begin (display (substring text start length) port) (close-output-port port))
           text))
        ((separator? (- i 1))
         (let* ((digits (if (char=? (string-ref text i) #\-) (+ i 1) i))
                (end (let scan ((j digits))
                       (if (and (< j length) (char-numeric? (string-ref text j))) (scan (+ j 1)) j))))
           (if (and (>= (- end digits) 10) (separator? end))
             (let ((output (or port (open-output-string))))
               (display (substring text start i) output)
               (write (substring text i end) output)
               (loop end end output))
             (loop (max (+ i 1) end) start port))))
        (else (loop (+ i 1) start port))))))

(define (cdr-smt2-read term)
  (with-input-from-string
    (cdr-smt2-protect-integers
      (yices-term-text term)) read))

(define (cdr-smt2-identifiers sys)
  (let ((ids (make-hashtable)))
    (define (register term value)
      (hashtable-put! ids (cdr-smt2-read term)
        (cons (cdr-ok (yices-type-of-term term)) value)))
    (vector/for-each
      (lambda (step)
        (unless (eq? step #unspecified)
          (for-each (lambda (term) (register term #f)) (cdr-step-variables step))))
      (cdr-system-steps sys))
    (for-each (lambda (decl) (register (cdr-variable sys decl #f) #f)) (cdr-system-globals sys))
    (sal-ast-table/for-each
      (lambda (decl type)
        (let ((card (uint32->fixnum (yices-scalar-type-card type))))
          (let loop ((i 0))
            (when (< i card)
              (register (cdr-ok (yices-constant type (fixnum->int32 i))) i)
              (loop (+ i 1))))))
      (cdr-system-type-map sys))
    ids))

(define (cdr-smt2-sort type)
  (cond
    ((= (yices-type-is-bool type) 1) 'Bool)
    ((or (= (yices-type-is-int type) 1) (= (yices-type-is-scalar type) 1)) 'Int)
    ((= (yices-type-is-real type) 1) 'Real)
    ((= (yices-type-is-bitvector type) 1) (list '_ 'BitVec (uint32->fixnum (yices-bvtype-size type))))
    (else (sign-error "sal-cdr: cannot export native sort ~a to SMT-LIB." type))))

(define (cdr-smt2-integer value)
  (if (< value 0) (list '- (- value)) value))

(define (cdr-smt2-decimal text)
  (if (char=? (string-ref text 0) #\-)
    (list '- (substring text 1 (string-length text)))
    text))

(define (cdr-smt2-real operand)
  (if (eq? (cdr operand) 'Int) (list 'to_real (car operand)) (car operand)))

(define (cdr-smt2-atom atom resolve)
  (cond
    ((and (integer? atom) (exact? atom)) (cons (cdr-smt2-integer atom) 'Int))
    ((string? atom) (cons (cdr-smt2-decimal atom) 'Int))
    ((memq atom '(true false)) (cons atom 'Bool))
    ((symbol? atom)
     (let ((text (symbol->string atom)))
       (cond
         ((or (substring=? text "0b" 2) (substring=? text "0x" 2))
          (cons (string-append "#" (substring text 1 (string-length text))) 'BitVec))
         ((sal-string-index text #\/) =>
          (lambda (i)
            (let ((a (substring text 0 i))
                  (b (substring text (+ i 1) (string-length text))))
              (unless (and (> i 0) (> (string-length b) 0)
                           (or (char-numeric? (string-ref a 0)) (char=? (string-ref a 0) #\-))
                           (char-numeric? (string-ref b 0)))
                (sign-error "sal-cdr: malformed native rational ~a." atom))
              (cons (list '/ (list 'to_real (cdr-smt2-decimal a)) (list 'to_real b)) 'Real))))
         ((resolve atom) => identity)
         (else (sign-error "sal-cdr: cannot export native term ~a to SMT-LIB." atom)))))
    (else (sign-error "sal-cdr: cannot export native term ~a to SMT-LIB." atom))))

(define (cdr-smt2-fold op values)
  (let loop ((form (car values)) (rest (cdr values)))
    (if (null? rest) form (loop (list op form (car rest)) (cdr rest)))))

(define (cdr-smt2-power op value degree)
  (let ((base (gensym 'base!))
        (degree (if (string? degree) (string->integer degree) degree)))
    (define (power n)
      (if (= n 1) base
        (let* ((half (gensym 'half!))
               (square (list op half half)))
          (list 'let (list (list half (power (quotient n 2))))
                (if (odd? n) (list op base square) square)))))
    (list 'let (list (list base value)) (power degree))))

(define (cdr-smt2-indexed-root operands resolve)
  (let* ((op (if (eq? (car operands) '!=) 'distinct (car operands)))
         (rank (cadr operands))
         (index (if (string? rank) (string->integer rank) rank))
         (x (caddr operands))
         (polynomial (cadddr operands)))
    (unless (and (integer? index) (>= index 0) (symbol? x))
      (sign-error "sal-cdr: malformed native root atom ~a." operands))
    ;; Yices counts distinct real roots from zero. Select index + 1 ordered
    ;; zeros and require every zero below the last one to appear in the list.
    (let* ((earlier (generate-list (lambda (i) (gensym 'root!)) index))
           (last-root (gensym 'root!))
           (probe (gensym 'root!))
           (roots (append earlier (list last-root))))
      (define (zero-at var)
        (list '= (car (cdr-smt2-form polynomial
                       (lambda (name) (if (eq? name x) (cons var 'Real) (resolve name))))) 0))
      (let ((preceding (map (lambda (root) (list '= probe root)) earlier)))
        (cons
          (list 'exists (map (lambda (root) (list root 'Real)) roots)
                (cons 'and
                      (append (map zero-at roots)
                        (list (if (null? earlier) 'true (cons '< roots))
                              (list 'forall (list (list probe 'Real))
                                    (list '=> (list 'and (list '< probe last-root) (zero-at probe))
                                          (cond ((null? preceding) 'false)
                                                ((null? (cdr preceding)) (car preceding))
                                                (else (cons 'or preceding)))))
                              (list op (cdr-smt2-real (cdr-smt2-form x resolve)) last-root)))))
          'Bool)))))

(define (cdr-smt2-apply op args)
  (let ((terms (map car args))
        (real? (exists (lambda (arg) (eq? (cdr arg) 'Real)) args)))
    (case op
      ((not or and xor =>) (cons (cons op terms) 'Bool))
      ((= /= distinct < <= >= >)
       (cons (cons (if (eq? op '/=) 'distinct op)
                   (if real? (map cdr-smt2-real args) terms)) 'Bool))
      ((+ - *) (if real?
                 (cons (cons op (map cdr-smt2-real args)) 'Real)
                 (cons (cons op terms) 'Int)))
      ((/) (cons (cons '/ (map cdr-smt2-real args)) 'Real))
      ((div mod) (cons (cons op terms) 'Int))
      ((is-int) (cons (list 'is_int (cdr-smt2-real (car args))) 'Bool))
      ((floor) (cons (list 'to_int (cdr-smt2-real (car args))) 'Int))
      ((ceil) (cons (list '- (list 'to_int (list '- (cdr-smt2-real (car args))))) 'Int))
      ((abs)
       (let ((name (gensym 'abs!)))
         (cons (list 'let (list (list name (car terms)))
                     (list 'ite (list '< name 0) (list '- name) name))
               (cdar args))))
      ((divides)
       (cons (list 'is_int (list '/ (cdr-smt2-real (cadr args)) (cdr-smt2-real (car args)))) 'Bool))
      ((ite)
       (cons (cons 'ite (if real? (cons (car terms) (map cdr-smt2-real (cdr args))) terms))
             (if real? 'Real (cdadr args))))
      ((^ bv-pow)
       (cons (cdr-smt2-power (if (eq? op '^) '* 'bvmul) (car terms) (cadr terms))
             (cdar args)))
      ((bit)
       (cons (list '= (list (list '_ 'extract (cadr terms) (cadr terms)) (car terms)) "#b1")
             'Bool))
      ((bool-to-bv)
       (cons (cdr-smt2-fold 'concat
               (map (lambda (value) (list 'ite value "#b1" "#b0")) (reverse terms))) 'BitVec))
      ((bv-extract)
       (cons (list (list '_ 'extract (car terms) (cadr terms)) (caddr terms)) 'BitVec))
      ((bv-sign-extend bv-zero-extend)
       (cons (list (list '_ (if (eq? op 'bv-sign-extend) 'sign_extend 'zero_extend)
                         (cadr terms)) (car terms)) 'BitVec))
      (else
       (let ((word-op (assq op '((bv-add . bvadd) (bv-mul . bvmul) (bv-concat . concat)
                                 (bv-div . bvudiv) (bv-rem . bvurem)
                                 (bv-sdiv . bvsdiv) (bv-srem . bvsrem) (bv-smod . bvsmod)
                                 (bv-shl . bvshl) (bv-lshr . bvlshr) (bv-ashr . bvashr)
                                 (bv-ge . bvuge) (bv-lt . bvult) (bv-sge . bvsge) (bv-slt . bvslt)))))
         (unless word-op (sign-error "sal-cdr: cannot export native operator ~a to SMT-LIB." op))
         (cons (cdr-smt2-fold (cdr word-op) terms)
               (if (memq op '(bv-ge bv-lt bv-sge bv-slt)) 'Bool 'BitVec)))))))

(define (cdr-smt2-form form resolve)
  (cond
    ((not (pair? form)) (cdr-smt2-atom form resolve))
    ((eq? (car form) 'arith-root-atom) (cdr-smt2-indexed-root (cdr form) resolve))
    (else (cdr-smt2-apply (car form) (map (lambda (arg) (cdr-smt2-form arg resolve)) (cdr form))))))

(define (cdr-smt2-native-operators)
  (list (cons (yices-enum YICES_NOT_TERM) 'not)
        (cons (yices-enum YICES_OR_TERM) 'or)
        (cons (yices-enum YICES_XOR_TERM) 'xor)
        (cons (yices-enum YICES_EQ_TERM) '=)
        (cons (yices-enum YICES_DISTINCT_TERM) 'distinct)
        (cons (yices-enum YICES_ITE_TERM) 'ite)
        (cons (yices-enum YICES_ARITH_GE_ATOM) '>=)
        (cons (yices-enum YICES_RDIV) '/)
        (cons (yices-enum YICES_IDIV) 'div)
        (cons (yices-enum YICES_IMOD) 'mod)
        (cons (yices-enum YICES_ABS) 'abs)
        (cons (yices-enum YICES_FLOOR) 'floor)
        (cons (yices-enum YICES_CEIL) 'ceil)
        (cons (yices-enum YICES_IS_INT_ATOM) 'is-int)
        (cons (yices-enum YICES_DIVIDES_ATOM) 'divides)
        (cons (yices-enum YICES_BV_ARRAY) 'bool-to-bv)
        (cons (yices-enum YICES_BV_DIV) 'bv-div)
        (cons (yices-enum YICES_BV_REM) 'bv-rem)
        (cons (yices-enum YICES_BV_SDIV) 'bv-sdiv)
        (cons (yices-enum YICES_BV_SREM) 'bv-srem)
        (cons (yices-enum YICES_BV_SMOD) 'bv-smod)
        (cons (yices-enum YICES_BV_SHL) 'bv-shl)
        (cons (yices-enum YICES_BV_LSHR) 'bv-lshr)
        (cons (yices-enum YICES_BV_ASHR) 'bv-ashr)
        (cons (yices-enum YICES_BV_GE_ATOM) 'bv-ge)
        (cons (yices-enum YICES_BV_SGE_ATOM) 'bv-sge)))

(define (cdr-smt2-bv-literal bits::yices-int32-pointer width)
  (let ((text (make-string (+ width 2) #\0)))
    (string-set! text 0 #\#)
    (string-set! text 1 #\b)
    (let loop ((i::long 0))
      (when (< i width)
        (unless (= (yices-int32-array-ref bits i) 0)
          (string-set! text (- (+ width 1) i) #\1))
        (loop (+ i 1))))
    text))

(define (cdr-smt2-bv-sum term encode)
  (let* ((width (cdr-word-width term))
         (n (cdr-ok (yices-term-num-children term)))
         (bits::yices-int32-pointer (make-yices-int32-array width))
         (component::yices-term-pointer (make-yices-term-array 1)))
    (let loop ((i 0) (monomials '()))
      (if (= i n)
        (cons (cdr-smt2-fold 'bvadd (reverse! monomials)) 'BitVec)
        (begin
          (cdr-ok (yices-bvsum-component term (fixnum->int32 i) bits component))
          (let ((a (cdr-smt2-bv-literal bits width))
                (x (yices-term-array-ref component 0)))
            (loop (+ i 1) (cons (if (< x 0) a (list 'bvmul a (car (encode x)))) monomials))))))))

(define (cdr-smt2-product term sort encode)
  (let* ((op (if (pair? sort) 'bvmul '*))
         (n (cdr-ok (yices-term-num-children term)))
         (component::yices-term-pointer (make-yices-term-array 1))
         (exponent::yices-uint32-pointer (make-yices-uint32-cell)))
    (let loop ((i 0) (factors '()))
      (if (= i n)
        (cons (cdr-smt2-fold op (reverse! factors)) sort)
        (begin
          (cdr-ok (yices-product-component term (fixnum->int32 i) component exponent))
          (let* ((degree (uint32->fixnum (yices-uint32-cell-ref exponent)))
                 (factor (encode (yices-term-array-ref component 0)))
                 (value (if (eq? sort 'Real) (cdr-smt2-real factor) (car factor))))
            (loop (+ i 1)
              (cons (if (= degree 1) value (cdr-smt2-power op value degree)) factors))))))))

(define (cdr-smt2-write form port)
  (if (not (pair? form)) (display form port)
    (begin
      (display "(" port)
      (cdr-smt2-write (car form) port)
      (for-each (lambda (item) (display " " port) (cdr-smt2-write item port)) (cdr form))
      (display ")" port))))

(define (cdr-open-query-log sys solver goal interpolate?)
  (let ((ids #f) (used (make-hashtable)) (encoded (make-eq-hash-table))
        (operators (cdr-smt2-native-operators)) (declarations '()) (definitions '()))
    (define (declare name type sort)
      (unless (hashtable-get used name)
        (hashtable-put! used name #t)
        (set! declarations (cons (list name type sort) declarations)))
      (cons name sort))
    (define (resolve name)
      (unless ids (set! ids (cdr-smt2-identifiers sys)))
      (cond
        ((hashtable-get ids name) =>
         (lambda (binding)
           (let ((type (car binding)) (value (cdr binding)))
             (if value (cons value 'Int)
               (declare name type (cdr-smt2-sort type))))))
        (else #f)))
    (define (encode-arguments term)
      (generate-list
        (lambda (i) (encode (cdr-ok (yices-term-child term (fixnum->int32 i)))))
        (int32->fixnum (cdr-ok (yices-term-num-children term)))))
    (define (encode-form term kind sort)
      (cond
        ((assv kind operators) => (lambda (entry) (cdr-smt2-apply (cdr entry) (encode-arguments term))))
        ((= kind (yices-enum YICES_BIT_TERM))
         (cdr-smt2-apply 'bit (list (encode (cdr-ok (yices-proj-arg term)))
                               (cons (int32->fixnum (cdr-ok (yices-proj-index term))) 'Int))))
        ((= kind (yices-enum YICES_BV_SUM)) (cdr-smt2-bv-sum term encode))
        ((= kind (yices-enum YICES_POWER_PRODUCT)) (cdr-smt2-product term sort encode))
        ;; Use the printed form where the API does not provide the root
        ;; relation/index or expose rational coefficients as native terms.
        (else (cdr-smt2-form (cdr-smt2-read term) resolve))))
    (define (encode term)
      (or (cdr-term-table-ref encoded term)
          (let* ((kind (cdr-ok (yices-term-constructor term)))
                 (type (cdr-ok (yices-type-of-term term)))
                 (sort (cdr-smt2-sort type))
                 (result
                   (cond
                     ((= kind (yices-enum YICES_UNINTERPRETED_TERM))
                      (declare (cdr-smt2-read term) type sort))
                     ((= kind (yices-enum YICES_SCALAR_CONSTANT))
                      (let ((index::yices-int32-pointer (make-yices-int32-array 1)))
                        (cdr-ok (yices-scalar-const-value term index))
                        (cons (int32->fixnum (yices-int32-array-ref index 0)) 'Int)))
                     (else
                      (let* ((form (encode-form term kind sort))
                             (body (if (eq? sort 'Real) (cdr-smt2-real form) (car form))))
                        (if (= (yices-term-is-atomic term) 1) (cons body sort)
                          ;; Definitions are emitted in dependency order. A
                          ;; shared subterm is serialized once per query.
                          (let ((name (gensym 'term!)))
                            (set! definitions (cons (list 'define-fun name '() sort body) definitions))
                            (cons name sort))))))))
            (cdr-term-table-set! encoded term result)
            result)))
    (let ((background (map (lambda (term) (car (encode term))) (reverse (cdr-solver-assertions solver))))
          (target (car (encode goal))))
      (set! *cdr-smt2-query-index* (+ *cdr-smt2-query-index* 1))
      (let* ((path (string-append (cdr-system-smt2-output sys) "."
                                  (number->string *cdr-smt2-query-index*) ".smt2"))
             (port (open-output-file path))
             (ready? #f))
        (define (command form)
          (cdr-smt2-write form port)
          (newline port))
        (unless (output-port? port) (sign-error "sal-cdr: cannot write SMT-LIB query to ~a." path))
        (unwind-protect
          (begin
            (fprint port "(set-option :produce-models true)")
            (fprint port "(set-logic ALL)")
            (for-each
              (lambda (binding)
                (let ((name (car binding)) (type (cadr binding)) (sort (caddr binding)))
                  (command (list 'declare-const name sort))
                  (when (= (yices-type-is-scalar type) 1)
                    (command (list 'assert
                                   (list 'and (list '<= 0 name)
                                         (list '< name (uint32->fixnum (yices-scalar-type-card type)))))))))
              (reverse declarations))
            (for-each command (reverse definitions))
            (fprint port (if interpolate? "; Interpolation partition A" "; Solver assertions"))
            (for-each (lambda (form) (command (list 'assert form))) background)
            (fprint port (if interpolate? "; Interpolation partition B" "; Query goal"))
            (command (list 'assert target))
            (fprint port "(check-sat)")
            (flush-output-port port)
            (set! ready? #t)
            port)
          (unless ready? (close-output-port port)))))))
