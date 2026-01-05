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

(module sal-esm-alt
        (include "sal.sch")
        (import unique-names sal-esm-support gmp-scheme sal-esm-engine-scm-context
                sal2scm-type sal-ast-env sal-type sal2scm-core)
        (export (sal-esm->next-alt-proc esm ctx env idx check-given-idx? produce-new-idx?)
                (sal-esm->exec-alt-proc esm ctx env idx))
        )

(define (size-list->sum-size-list size-list)
  (let loop ((size-list size-list)
             (acc 0)
             (result '()))
    (if (null? size-list)
      (reverse! result)
      (let* ((curr-size (car size-list))
             (new-acc (+fx acc curr-size)))
        (loop (cdr size-list)
              new-acc
              (cons new-acc result))))))

(define (mk-if c then else)
  (cond
   ((or (eq? c #t) (number? c)) then)
   ((eq? c #f) else)
   (else `(if ,c ,then ,else))))

;; We ignore the :no-delay? slot for processing guards. We perform a
;; much more conservative check for guards.
;;
;; INITIALIZATION section: assume they delay when they use any state
;; variable
;;
;; DEFINITION section: assume they delay when they use other defined
;; variables.
;;
;; TRANSITION section: assume they delay when they use next variables.
;;
;; We do not try to execute guards that can delay in the next-alt
;; proc. They are executed after exec-alt.
;;
;; Remark: a well behaved spec does not produce delays, because there
;; are no guards in the definition or initialization section, and next
;; variables are not used in the guards.
;;
;; Remark: Protection (undefinedness detection) code doesn't need to
;; be include in guards.
;;

(define (sal-esm/only-one-alternative? esm)
  (=mpq (sal-esm/num-alternatives esm) *mpq-one*))

;; I define a next-alt procedure for each class of esm statement.
;;
;; A next-alt procedure receives a alternative idx, and returns a
;; idx' >= idx such that the guards of the alternative idx' is
;; (possibly) true. We say possibly because the guards may use next
;; variables, and these values are not available at the time the
;; procedure is called. If there isn't such idx', then next-alt
;; procedure must return #f.
;;
;; The mk-next-alt procedure is defined inductively.
;;
;; Remark: The first idx is 0. 
;;
;; Definition of the mk-next-alt procedure.  Let (size s) denote the
;; number of alternatives in the esm statement s.
;;
;;
;; - guards:
;;   (lambda (idx)
;;     (and (= idx 0) <guard-expr> 0))
;;
;; - assignments
;;   (lambda (idx)
;;      (and (= idx 0) 0))
;;
;; - sequence (s-1 ... s-n)
;;   (lambda (idx)
;;     (and (< idx (* (size s-1) ... (size s-n)))
;;       (let* ((idx-1   (/          idx (* (size s-2) ... (size s-n))))
;;              (r-idx-1 (remainder  idx (* (size s-2) ... (size s-n))))
;;              (idx-2   (/          idx (* (size s-3) ... (size s-n))))
;;              (r-idx-2 (remainder  idx (* (size s-3) ... (size s-n))))
;;              ...
;;              (idx-n   r-idx-n-1)
;;              (child-idx #unspecified)
;;              (result-idx 0))
;;         (and 
;;              (set! child-idx ((mk-next-alt s-1) idx-1))
;;              child-idx
;;              (set! result-idx (* child-idx (size s-2) ... (size s-n)))
;;              (set! child-idx ((mk-next-alt s-2) idx-2))
;;              child-idx
;;              (set! result-idx (+ result-idx (* child-idx (size s-3) ... (size s-n))))
;;              ...
;;              (set! child-idx ((mk-next-alt s-n) idx-n))
;;              child-idx
;;              (set! result-idx (+ result-idx child-idx))
;;              result-idx))))
;;              
;; - choice (s-1 ... s-n)
;;   (lambda (idx)
;;           (or (and
;;                  (< idx (size s-1))
;;                  (let ((new-child-idx ((mk-next-alt s-1) idx)))
;;                     (if new-child-idx
;;                         new-child-idx
;;                        (and (set! idx (size s-1)) #f)))) 
;;               (and
;;                  (>= idx (size s-1))
;;                  (<  idx (+ (size s-1) (size s-2)))
;;                  (let ((new-child-idx ((mk-next-alt s-1) (- idx (size s-1)))))
;;                     (if new-child-idx
;;                         (+ new-child-idx (size s-1))
;;                         (and (set! idx (+ (size s-1) (size s-2)) #f)))) 
;;               ... 
;;               (and
;;                  (>= idx (+ (size s-1) ... (size s-n-1)))
;;                  (<  idx (+ (size s-1) ... (size s-n)))
;;                  (let ((new-child-idx ((mk-next-alt s-1) (- idx (+ (size s-1) ... (size s-n-1))))))
;;                     (if new-child-idx
;;                         (+ new-child-idx (size s-1) ... (size s-n-1))
;;                         #f)))))
;;
;;
;; - multi-sequence and multi-choice are similar to sequence and choice. 
;;
;; Remarks:
;; - the next alt for the empty choice is
;;   (lambda (idx) #f)
;;
;; - the next alt for the empty sequence is
;;   (lambda (idx) (and (= idx 0) idx))
;; 
;; Optimizations:
;;
;; 1) Unfold the nested next-alt procedures. In this case several simplifications
;;    can be performed, since in most of the cases the tests of caller and callee 
;;    can be combined. To perform these simplications we use two extra variables
;;    when we generate the next-alt procedure: check-given-idx? and produce-last-idx?.
;;    The next-alt code for an esm doesn't need to check the given idx when
;;    check-given-idx? is #f. The next-alt code for an esm doesn't need to produce
;;    the idx for its last alternative when produce-last-idx? is #f.
;;
;; 2) If an esm statement 's' does not contain guards, then its next-alt procedure is:
;;    (lambda (idx)
;;      (and (< idx (size s)) idx))
;;
;; 3) The code for sequence and choice can be simplified when the nested statements
;;    contains only one alternative. 
;;    Remark: If a statement contains just an alternative, then its next-alt proc
;;    returns 0 or #f.
;;
;; 4) When we process choice and sequence statements, we can ignore the esm assignments
;;    in the slot :statements.
;;
;; Example: suppose we have a sequence where all nested statements contains just one
;; alternative. Using the definition above, we will produce:
;;
;;   (lambda (idx)
;;     (and (< idx 1)
;;       (let* ((idx-1   (/          idx 1))
;;              (r-idx-1 (remainder  idx 1))
;;              (idx-2   (/          idx 1))
;;              (r-idx-2 (remainder  idx 1))
;;              ...
;;              (idx-n   r-idx-n-1)
;;              (child-idx #unspecified)
;;              (result-idx 0))
;;         (and 
;;              (set! child-idx ((mk-next-alt s-1) idx-1))
;;              child-idx
;;              (set! result-idx (* child-idx 1))
;;              (set! child-idx ((mk-next-alt s-2) idx-2))
;;              child-idx
;;              (set! result-idx (+ result-idx (* child-idx 1)))
;;              ...
;;              (set! child-idx ((mk-next-alt s-n) idx-n))
;;              child-idx
;;              (set! result-idx (+ result-idx child-idx))
;;              result-idx))))
;;
;;  This code if far from optimal. Using the optimization decribed above, we can transform
;;  the code to:
;;  (lambda (idx)
;;    (and (= idx 0)
;;         ((mk-next-alt s-1) 0)
;;         ...
;;         ((mk-next-alt s-n) 0)
;;         0)))
;;
;;  Considering optimization 1, check-given-idx? and produce-last-idx? are extra arguments for mk-next-alt.
;;  Then we have
;;
;;  (lambda (idx)
;;    (and ((mk-next-alt s-1 check-given-idx? #f) 0)
;;         ...
;;         ((mk-next-alt s-n #f produce-last-idx?) 0)))
;;
;;

(define-generic (sal-esm->next-alt-proc esm ctx env idx check-given-idx? produce-last-idx?))

(define-method (sal-esm->next-alt-proc :around (esm <sal-esm-statement>) (ctx <sal-esm-engine-scm-context>) (env <primitive>) (idx <primitive>)
                                       (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (if (not (sal-esm/has-guards? esm))
    (let ((check-given-test (when check-given-idx?
                              (let ((num-alts (mpq->integer (sal-esm/num-alternatives esm))))
                                (if (number? idx)
                                  (<fx idx num-alts)
                                  `(<fx ,idx ,num-alts))))))
      (cond
       ((and (not check-given-idx?) (not produce-last-idx?))
        #t)
       ((not check-given-idx?)
        idx)
       ((not produce-last-idx?)
        check-given-test)
       (else
        `(and ,check-given-test ,idx))))
    (call-next-method)))

(define (mk-do-nothing-next-alt-proc idx check-given-idx? produce-last-idx?)
  (let ((check-given-test (when check-given-idx?
                            (if (number? idx)
                              (=fx idx 0)
                              `(=fx ,idx 0)))))
    (cond
     ((and (not check-given-idx?) (not produce-last-idx?))
      #t)
     ((not check-given-idx?)
      0)
     ((not produce-last-idx?)
      check-given-test)
     (else
      `(and ,check-given-test 0)))))

(define-method (sal-esm->next-alt-proc (esm <primitive>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (mk-do-nothing-next-alt-proc idx check-given-idx? produce-last-idx?))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-guard>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (let ((check-given-test (when check-given-idx?
                            (if (number? idx)
                              (=fx idx 0)
                              `(=fx ,idx 0))))
        (guard-expr 'todo))
    (cond
     ((and (not check-given-idx?) (not produce-last-idx?))
      guard-expr)
     ((not check-given-idx?)
      `(and ,guard-expr 0))
     ((not produce-last-idx?)
      `(and ,check-given-test ,guard-expr))
     (else
      `(and ,check-given-test ,guard-expr 0)))))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-assignment>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (mk-do-nothing-next-alt-proc idx check-given-idx? produce-last-idx?))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-choice>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (let* ((statement-list (slot-value esm :statements))
         (size-list (map (lambda (stmt)
                           (mpq->integer (sal-esm/num-alternatives stmt)))
                         statement-list))
         (sum-size-list (size-list->sum-size-list size-list)))
    (cond
     ((null? statement-list)
      #f)
     ((null? (cdr statement-list))
      (sal-esm->next-alt-proc (car statement-list) ctx env idx check-given-idx? produce-last-idx?))
     (else
      `(or ,@(let loop ((prev-sum 0)
                        (statement-list statement-list)
                        (sum-size-list sum-size-list)
                        (result '()))
               (if (null? statement-list)
                 (reverse! result)
                 (let* ((curr-statement (car statement-list))
                        (curr-sum (car sum-size-list))
                        (child-idx (gen-unique-name 'child-idx))
                        (new-child-idx (gen-unique-name 'new-child-idx))
                        (last? (null? (cdr statement-list)))
                        (first? (=fx prev-sum 0))
                        ;; it is not possible to be the first and the last... the
                        ;; this case was handled in the top conditional
                        (_ [assert (last? first? esm) (not (and first? last?))])  
                        (new-alternative
                         `(and ,@(if (sal-esm/only-one-alternative? curr-statement)
                                   ;; child contains only 1 alternative
                                   `((=fx ,idx ,prev-sum)
                                     ,(cond
                                       ((and last? produce-last-idx?)
                                        `(and ,(sal-esm->next-alt-proc curr-statement ctx env 0 #f #f) ,prev-sum))
                                       (last?
                                        [assert (produce-last-idx?) (not produce-last-idx?)]
                                        (sal-esm->next-alt-proc curr-statement ctx env 0 #f #f))
                                       (else
                                        (mk-if (sal-esm->next-alt-proc curr-statement ctx env 0 #f #f)
                                               idx
                                               `(begin (set! ,idx ,curr-sum) #f)))))
                                   ;; child contains several alternatives
                                   `((>=fx ,idx ,prev-sum)
                                     (<fx ,idx ,curr-sum)
                                     ,(cond
                                       ;; it is the first
                                       (first?
                                        ;; I don't have to adjust the given and returned indexes.
                                        `(or ,(sal-esm->next-alt-proc curr-statement ctx env idx #f #t)
                                             (and (set! ,idx ,curr-sum) #f)))
                                       ;; it is the last
                                       (last?
                                        `(let* ((,child-idx (-fx ,idx ,prev-sum))
                                                (,new-child-idx ,(sal-esm->next-alt-proc curr-statement ctx env child-idx #f #t)))
                                           (and ,new-child-idx (+fx ,new-child-idx ,prev-sum))))
                                       ;; it not the first nor the last
                                       (else
                                        `(let* ((,child-idx (-fx ,idx ,prev-sum))
                                                (,new-child-idx ,(sal-esm->next-alt-proc curr-statement ctx env child-idx #f #t)))
                                           (if ,new-child-idx
                                             (+fx ,new-child-idx ,prev-sum)
                                             (begin (set! ,idx ,curr-sum) #f))))))))))
                   (loop curr-sum
                         (cdr statement-list)
                         (cdr sum-size-list)
                         (cons new-alternative result)))))))))) 

(define-method (sal-esm->next-alt-proc (esm <sal-esm-case>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  `(case todo ;; ,(sal->scm (slot-value esm :expr) ctx env)
     ,@(map (lambda (case-entry)
              `((,(sal->scm (slot-value case-entry :value) ctx env))
                ,(sal-esm->next-alt-proc (slot-value case-entry :statement) ctx env idx #t produce-last-idx?)))
            (slot-value esm :case-entries))
     (else #f)))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-when-undefined>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (mk-do-nothing-next-alt-proc idx check-given-idx? produce-last-idx?))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-seq>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (let ((statement-list (filter (lambda (esm)
                                  (not (instance-of? esm <sal-esm-assignment>)))
                                (slot-value esm :statements))))
    (cond
     ((null? statement-list)
      (mk-do-nothing-next-alt-proc idx check-given-idx? produce-last-idx?))
     ((null? (cdr statement-list))
      (sal-esm->next-alt-proc (car statement-list) ctx env idx check-given-idx? produce-last-idx?))
     ((sal-esm/only-one-alternative? esm)
      ;; simple case  
      `(and ,@(let loop ((first? #t)
                         (statement-list statement-list)
                         (result-list '()))
                (cond
                 ((null? statement-list)
                  [assert (not (null? result-list))]
                  (reverse! result-list))
                 (else
                  (let ((last? (null? (cdr statement-list))))
                    (loop #f
                          (cdr statement-list)
                          (let ((child-code (sal-esm->next-alt-proc (car statement-list) ctx env 0 
                                                                    (and first? check-given-idx?)
                                                                    (and produce-last-idx? last?))))
                            (if (eq? child-code #t)
                              result-list
                              (cons child-code result-list))))))))))
     (else 
      ;; generic case
      (let* ((size-list (map (lambda (stmt)
                               (mpq->integer (sal-esm/num-alternatives stmt)))
                             statement-list))
             (product-list (sal-scm/mk-product-list size-list))
             (new-child-idx (gen-unique-name 'new-child-idx))
             (new-idx (gen-unique-name 'new-idx)))
        (multiple-value-bind
            (children-idxs children-idxs-assignments)
            (sal-scm/product-idx size-list idx #t)
          `(let* (,@children-idxs-assignments
                  (,new-idx 0)
                  (,new-child-idx 0))
             (and ,@(if check-given-idx?
                      `((<fx ,idx ,(mpq->integer (sal-esm/num-alternatives esm))))
                      '())
                  ,@(let loop ((first? #t)
                               (statement-list statement-list)
                               (children-idxs children-idxs)
                               (product-list product-list)
                               (result-list '()))
                      (if (null? statement-list)
                        (reverse! result-list)
                        (let* ((child-statement (car statement-list))
                               (child-idx (car children-idxs))
                               (factor (car product-list))
                               (last? (null? (cdr statement-list)))
                               (code  (cond
                                       ((sal-esm/only-one-alternative? child-statement)
                                        (sal-esm->next-alt-proc child-statement ctx env 0 #f #f))
                                       (else
                                        ;; the child doesn't need to check the given idx, since we added the
                                        ;; test (<fx idx num-seq-alternatives), and this test guarantees that
                                        ;; child-idx is a valid idx for the child.
                                        `(and (set! ,new-child-idx ,(sal-esm->next-alt-proc child-statement ctx env child-idx #f #t))
                                              ,new-child-idx
                                              (set! ,new-idx (+fx ,new-idx (*fx ,factor ,new-child-idx)))))))
                               (new-result-list (cons code result-list))
                               (new-result-list (if last? (cons new-idx new-result-list) new-result-list)))
                          (loop #f
                                (cdr statement-list)
                                (cdr children-idxs)
                                (cdr product-list)
                                new-result-list))))))))))))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-multi-choice>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (let* ((statement (slot-value esm :statement))
         (local-decls (slot-value esm :local-decls))
         (local-sizes (map (lambda (var-decl)
                             (sal-type/number-of-elements-as-integer (slot-value var-decl :type)))
                           local-decls))
         (n (mpq->integer (sal-esm/num-alternatives statement))) ;; number of alts in the nested statement
         (vn (fold-left * 1 local-sizes)) ;; number of assignments for the local decls
         (new-vars (map (lambda (decl) (gen-unique-name (sal-decl/name decl))) local-decls))
         (new-env (update-env* env local-decls new-vars))
         ;; auxiliary variables
         (var-idx (gen-unique-name 'var-idx))
         (child-idx (gen-unique-name 'child-idx))
         (new-child-idx (gen-unique-name 'new-child-idx))
         (multi-choice-loop (gen-unique-name 'multi-choice-loop)))         
    (multiple-value-bind
        (local-idxs local-idxs-assignments)
        (sal-scm/product-idx local-sizes var-idx)
      (let (;; new-vars assignments
            (new-vars-assignments (map (lambda (new-var decl idx)
                                         `(,new-var ,(sal-type/idx->val (slot-value decl :type) ctx env idx)))
                                       new-vars
                                       local-decls
                                       local-idxs)))
        (if (=fx n 1) ;; little optimization
          ;; there is only one nested alternative
          `(let ,multi-choice-loop ((,var-idx ,idx))   
                (if (<fx ,var-idx ,vn)
                  (let* (,@local-idxs-assignments
                         ,@new-vars-assignments)
                    ,(mk-if (sal-esm->next-alt-proc statement ctx new-env 0 #f #f)
                            var-idx
                            `(,multi-choice-loop (+fx ,var-idx 1))))
                  #f))
          ;; there are more than one nested alternative
          `(let ,multi-choice-loop ((,var-idx (/fx ,idx ,n))
                                    (,child-idx (remainder ,idx ,n)))
                (if (<fx ,var-idx ,vn)
                  (let* (,@local-idxs-assignments
                         ,@new-vars-assignments
                         (,new-child-idx ,(sal-esm->next-alt-proc statement ctx new-env child-idx #f #t)))
                    (if ,new-child-idx
                      (+fx ,new-child-idx (*fx ,var-idx ,n))
                      (,multi-choice-loop (+fx ,var-idx 1)
                                          0)))
                  #f)))))))

(define-method (sal-esm->next-alt-proc (esm <sal-esm-multi-seq>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>) (check-given-idx? <primitive>) (produce-last-idx? <primitive>))
  (let* ((statement (slot-value esm :statement))
         (local-decls (slot-value esm :local-decls))
         (local-sizes (map (lambda (var-decl)
                             (sal-type/number-of-elements-as-integer (slot-value var-decl :type)))
                           local-decls))
         (n (mpq->integer (sal-esm/num-alternatives statement))) ;; number of alts in the nested statement
         (vn (fold-left * 1 local-sizes)) ;; number of assignments for the local decls
         (new-vars (map (lambda (decl) (gen-unique-name (sal-decl/name decl))) local-decls))
         (new-env (update-env* env local-decls new-vars))
         (num-alts (expt n vn))
         ;; auxiliary variables
         (var-idx (gen-unique-name 'var-idx))
         (curr-idx (gen-unique-name 'curr-idx))
         (r-idx (gen-unique-name 'r-idx))
         (factor (gen-unique-name 'factor))
         (child-idx (gen-unique-name 'child-idx))
         (new-child-idx (gen-unique-name 'new-child-idx))
         (result-idx (gen-unique-name 'result-idx))
         (multi-seq-main-loop (gen-unique-name 'multi-seq-main-loop))
         (multi-seq-inner-loop (gen-unique-name 'multi-seq-inner-loop)))
    (multiple-value-bind
        (local-idxs local-idxs-assignments)
        (sal-scm/product-idx local-sizes var-idx)
      (let ((new-vars-assignments (map (lambda (new-var decl idx)
                                         `(,new-var ,(sal-type/idx->val (slot-value decl :type) ctx env idx)))
                                       new-vars
                                       local-decls
                                       local-idxs)))
        `(let ,multi-seq-main-loop ((,curr-idx ,idx))
              (if (<fx ,curr-idx ,num-alts)
                (let ,multi-seq-inner-loop ((,var-idx 0)
                                            (,r-idx ,curr-idx)
                                            (,factor ,(expt n (- vn 1)))
                                            (,result-idx 0))
                     (if (<fx ,var-idx ,vn)
                       (let* (,@local-idxs-assignments
                              ,@new-vars-assignments
                              (,child-idx (/fx ,r-idx ,factor))
                              (,new-child-idx ,(sal-esm->next-alt-proc statement ctx new-env child-idx #f #t)))
                         (if ,new-child-idx
                           (,multi-seq-inner-loop (+fx ,var-idx 1)
                                                  (remainder ,r-idx ,factor)
                                                  (/fx factor ,n)
                                                  (+fx ,result-idx (*fx ,new-child-idx ,factor)))
                           (,multi-seq-main-loop (+fx ,curr-idx 1))))
                       ,result-idx))
                #f))))))


;; I define a exec-alt procedure for each class of esm statement.
;;
;; - guards:
;;   (lambda (idx)
;;     #unspecified)
;;
;; - assignments
;;   (lambda (idx)
;;     <assignment-code>)
;;
;; - sequence  (s-1 ... s-n)
;;   (lambda (idx)
;;       (let* ((idx-1   (/          idx (* (size s-2) ... (size s-n))))
;;              (r-idx-1 (remainder  idx (* (size s-2) ... (size s-n))))
;;              (idx-2   (/          idx (* (size s-3) ... (size s-n))))
;;              (r-idx-2 (remainder  idx (* (size s-3) ... (size s-n))))
;;              ...
;;              (idx-n   r-idx-n-1))
;;     (when (< idx (* (size s-1) ... (size s-n)))
;;         (begin ((mk-exec-alt s-1) idx-1)
;;                ...
;;                ((mk-exec-alt s-n) idx-n)))
;;
;; - choice (s-1 ... s-n)
;;   (lambda (idx)
;;     (cond
;;       ((< idx (size s-1))
;;        ((mk-exec-alt s-1) idx))
;;       ...
;;       ((>= idx (+ (size s-1) ... (size s-n-1)))
;;        (< idx (+ (size s-1) ... (size s-n)))
;;        ((mk-exec-alt s-n) (- idx (+ (size s-1) ... (size s-n-1)))))))
;;
(define-generic (sal-esm->exec-alt-proc esm ctx env idx))

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-guard>) (ctx <sal-esm-engine-scm-context>) (env <primitive>) 
                                       (idx <primitive>))
  #unspecified)

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-assignment>) (ctx <sal-esm-engine-scm-context>) (env <primitive>) 
                                       (idx <primitive>))
  'todo)

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-choice-assignment>) (ctx <sal-esm-engine-scm-context>) (env <primitive>) 
                                       (idx <primitive>))
  #unspecified)

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-choice>) (ctx <sal-esm-engine-scm-context>) (env <primitive>) 
                                       (idx <primitive>))
  (let* ((statement-list (slot-value esm :statements))
         (size-list (map (lambda (stmt)
                           (mpq->integer (sal-esm/num-alternatives stmt)))
                         statement-list))
         (sum-size-list (size-list->sum-size-list size-list)))
    (cond
     ((null? statement-list)
      #f)
     ((null? (cdr statement-list))
      (sal-esm->exec-alt-proc (car statement-list) ctx env idx))
     (else
      `(cond ,@(let loop ((prev-sum 0)
                          (statement-list statement-list)
                          (sum-size-list sum-size-list)
                          (result '()))
                 (if (null? statement-list)
                   (reverse! result)
                   (let* ((curr-statement (car statement-list))
                          (curr-sum (car sum-size-list))
                          (child-idx (gen-unique-name 'child-idx))
                          (new-alternative
                           (if (sal-esm/only-one-alternative? curr-statement)
                             ;; child contains only 1 alternative
                             `((=fx ,idx ,prev-sum)
                               ,(sal-esm->exec-alt-proc curr-statement ctx env 0))
                             ;; child contains several alternatives
                             `((and (>=fx ,idx ,prev-sum)
                                    (<fx ,idx ,curr-sum))
                               (let ((,child-idx (-fx ,idx ,prev-sum)))
                                 ,(sal-esm->exec-alt-proc curr-statement ctx env child-idx))))))
                     (loop curr-sum
                           (cdr statement-list)
                           (cdr sum-size-list)
                           (cons new-alternative result))))))))))

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-case>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>))
  `(case 'todo ;; ,(sal->scm (slot-value esm :expr) ctx env)
     ,@(map (lambda (case-entry)
              `((,(sal->scm (slot-value case-entry :value) ctx env))
                ,(sal-esm->exec-alt-proc (slot-value case-entry :statement) ctx env idx)))
            (slot-value esm :case-entries))))

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-when-undefined>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>))
  `(when todo
     ,(sal-esm->exec-alt-proc (slot-value esm :statement) ctx env idx)))

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-seq>) (ctx <sal-esm-engine-scm-context>) (env <primitive>) 
                                       (idx <primitive>))
  (let ((statement-list (filter (lambda (esm)
                                  (and (not (instance-of? esm <sal-esm-guard>))
                                       (not (instance-of? esm <sal-esm-choice-assignment>))))
                                (slot-value esm :statements))))
    (cond
     ((null? statement-list)
      #unspecified)
     ((null? (cdr statement-list))
      (sal-esm->exec-alt-proc (car statement-list) ctx env idx))
     ((sal-esm/only-one-alternative? esm)
      ;; simple case  
      (let ((children-code (map (cut sal-esm->exec-alt-proc <> ctx env 0) statement-list)))
        `(begin ,@children-code)))
     (else
      (let* ((size-list (map (lambda (stmt)
                               (mpq->integer (sal-esm/num-alternatives stmt)))
                             statement-list))
             (product-list (sal-scm/mk-product-list size-list)))
        (multiple-value-bind
            (children-idxs children-idxs-assignments)
            (sal-scm/product-idx size-list idx #t)
          `(let* ,children-idxs-assignments
             ,@(let loop ((statement-list statement-list)
                          (children-idxs children-idxs)
                          (result-list '()))
                 (if (null? statement-list)
                   (reverse! result-list)
                   (let* ((child-statement (car statement-list))
                          (child-idx (car children-idxs))
                          (code (if (sal-esm/only-one-alternative? child-statement)
                                  (sal-esm->exec-alt-proc child-statement ctx env 0)
                                  (sal-esm->exec-alt-proc child-statement ctx env child-idx))))
                     (loop (cdr statement-list)
                           (cdr children-idxs)
                           (cons code result-list))))))))))))

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-multi-choice>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>))
  (let* ((statement (slot-value esm :statement))
         (local-decls (slot-value esm :local-decls))
         (local-sizes (map (lambda (var-decl)
                             (sal-type/number-of-elements-as-integer (slot-value var-decl :type)))
                           local-decls))
         (n (mpq->integer (sal-esm/num-alternatives statement))) ;; number of alts in the nested statement
         (vn (fold-left * 1 local-sizes)) ;; number of assignments for the local decls
         (new-vars (map (lambda (decl) (gen-unique-name (sal-decl/name decl))) local-decls))
         (new-env (update-env* env local-decls new-vars))
         ;; auxiliary variables
         (var-idx (gen-unique-name 'var-idx))
         (child-idx (gen-unique-name 'child-idx)))
    (multiple-value-bind
        (local-idxs local-idxs-assignments)
        (sal-scm/product-idx local-sizes var-idx)
      (let ((new-vars-assignments (map (lambda (new-var decl idx)
                                         `(,new-var ,(sal-type/idx->val (slot-value decl :type) ctx env idx)))
                                       new-vars
                                       local-decls
                                       local-idxs)))
        (if (=fx n 1) ;; little optimization
          ;; there is only one nested alternative
          `(let* ((,var-idx ,idx)
                  ,@local-idxs-assignments
                  ,@new-vars-assignments)
             ,(sal-esm->exec-alt-proc statement ctx new-env 0))
          ;; there are more than one nested alternative
          `(let* ((,var-idx (/fx ,idx ,n))
                  (,child-idx (remainder ,idx ,n))
                  ,@local-idxs-assignments
                  ,@new-vars-assignments)
             ,(sal-esm->exec-alt-proc statement ctx new-env child-idx)))))))

(define-method (sal-esm->exec-alt-proc (esm <sal-esm-multi-seq>) (ctx <sal-esm-engine-scm-context>) (env <primitive>)
                                       (idx <primitive>))
  (let* ((statement (slot-value esm :statement))
         (local-decls (slot-value esm :local-decls))
         (local-sizes (map (lambda (var-decl)
                             (sal-type/number-of-elements-as-integer (slot-value var-decl :type)))
                           local-decls))
         (n (mpq->integer (sal-esm/num-alternatives statement))) ;; number of alts in the nested statement
         (vn (fold-left * 1 local-sizes)) ;; number of assignments for the local decls
         (new-vars (map (lambda (decl) (gen-unique-name (sal-decl/name decl))) local-decls))
         (new-env (update-env* env local-decls new-vars))
         (num-alts (expt n vn))
         ;; auxiliary variables
         (var-idx (gen-unique-name 'var-idx))
         (r-idx (gen-unique-name 'r-idx))
         (factor (gen-unique-name 'factor))
         (child-idx (gen-unique-name 'child-idx))
         (multi-seq-loop (gen-unique-name 'multi-seq-loop)))
    (multiple-value-bind
        (local-idxs local-idxs-assignments)
        (sal-scm/product-idx local-sizes var-idx)
      (let ((new-vars-assignments (map (lambda (new-var decl idx)
                                         `(,new-var ,(sal-type/idx->val (slot-value decl :type) ctx env idx)))
                                       new-vars
                                       local-decls
                                       local-idxs)))
        `(let ,multi-seq-loop ((,var-idx 0)
                               (,r-idx ,idx)
                               (,factor ,(expt n (- vn 1))))
              (when (<fx ,var-idx ,vn)
                (let* (,@local-idxs-assignments
                       ,@new-vars-assignments
                       (,child-idx (/fx ,r-idx ,factor)))
                  ,(sal-esm->exec-alt-proc statement ctx new-env child-idx)
                  (,multi-seq-loop (+fx ,var-idx 1)
                                   (remainder ,r-idx ,factor)
                                   (/fx factor ,n)))))))))
