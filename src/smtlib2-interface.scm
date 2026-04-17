;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module smtlib2-interface
        (include "sal.sch")
        (import tmp-files runtime gmp-scheme sal-expression sal-type sal-environment xformat)
        (export *smtlib2-command*
                *smtlib2-profile*
                *sal-tmp-in-file-to-smtlib2*
                *sal-tmp-out-file-to-smtlib2*
                (sal/set-smtlib2-in-tmp-file!)
                (sal/set-smtlib2-out-tmp-file!)
                (sal/set-smtlib2-command! cmd-name)
                (sal/set-smtlib2-profile! profile)
                (sal/smtlib2-command)
                (smtlib2/execute-core in-file)
                (smtlib2/execute in-file smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
                (smtlib2/read-forms-from-string str)
                (smtlib2/model-forms->constraints forms smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
                (smtlib2/form->sal-expr form smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)))

(define *smtlib2-profile-commands*
  '((yices2 . "yices-smt2")
    (z3 . "z3 -smt2 pp.decimal=true pp.decimal_precision=20")
    (cvc5 . "cvc5 --lang=smt2 --output-lang=smt2 --produce-models --no-condense-function-values")))

(define *smtlib2-profile* 'yices2)
(define *smtlib2-command* (cdr (assq *smtlib2-profile* *smtlib2-profile-commands*)))
(define *smtlib2-env-configured?* #f)

(define *sal-tmp-in-file-to-smtlib2* #f)
(define *sal-tmp-out-file-to-smtlib2* #f)

(define-api (sal/set-smtlib2-command! (cmd-name string?))
  (set! *smtlib2-profile* 'custom)
  (set! *smtlib2-command* cmd-name))

(define-api (sal/set-smtlib2-profile! profile)
  (let ((profile-id (cond
                     ((symbol? profile) profile)
                     ((string? profile) (string->symbol profile))
                     (else
                      (sign-error "Expected SMT-LIB2 solver profile to be a symbol or string, found ~a." profile)))))
    (cond
     ((assq profile-id *smtlib2-profile-commands*) =>
      (lambda (entry)
        (set! *smtlib2-profile* profile-id)
        (set! *smtlib2-command* (cdr entry))))
     (else
      (sign-error "Unknown SMT-LIB2 solver profile `~a'. Available profiles: ~a"
                  profile-id
                  (map car *smtlib2-profile-commands*))))))

(define (sal/smtlib2-command)
  (smtlib2/configure-from-env!)
  *smtlib2-command*)

(define (smtlib2/configure-from-env!)
  (unless *smtlib2-env-configured?*
    (set! *smtlib2-env-configured?* #t)
    (let ((cmd (getenv "SAL_SMTLIB2_COMMAND"))
          (profile (getenv "SAL_SMTLIB2_PROFILE")))
      (cond
       ((and cmd (> (string-length cmd) 0))
        (sal/set-smtlib2-command! cmd))
       ((and profile (> (string-length profile) 0))
        (sal/set-smtlib2-profile! profile))))))

(define (sal/set-smtlib2-in-tmp-file!)
  (set! *sal-tmp-in-file-to-smtlib2* (sal/setup-tmp-file! "input.smt2")))

(define (sal/set-smtlib2-out-tmp-file!)
  (set! *sal-tmp-out-file-to-smtlib2* (sal/setup-tmp-file! "output.smt2")))

(define *smtlib2-id->sal-decl* (lambda (decl) #f))
(define *smtlib2-sort->sal-type* (lambda (sort-id) #f))
(define *place-provider* #unspecified)
(define *model-helper-defs* #f)
(define *model-helper-values* #f)
(define *model-aux-decls* #f)
(define *model-in-progress* '())

(define (report-error-running-smtlib2 cmd)
  (sign-error "Error running the SMT-LIB2 solver.\nThe following command was used:\n   ~a\nYou can change this command by adding\n\n  (sal/set-smtlib2-command! \"<solver> <args>\")\n\nor select one of the built-in profiles with\n\n  (sal/set-smtlib2-profile! 'z3)\n  (sal/set-smtlib2-profile! 'yices2)\n  (sal/set-smtlib2-profile! 'cvc5)\n\nto a `.salrc' file in your home directory." cmd))

(define (file-has-content? file-name)
  (and file-name
       (file-exists? file-name)
       (with-input-from-file file-name
         (lambda ()
           (not (eof-object? (read-char)))))))

(define (smtlib2/execute-core in-file)
  (smtlib2/configure-from-env!)
  (sal/set-smtlib2-out-tmp-file!)
  (let* ((cmd (string-append *smtlib2-command* " \"" in-file "\" > \"" *sal-tmp-out-file-to-smtlib2* "\""))
         (_ (verbose-message 3 "  SMT-LIB2 command: ~a" cmd))
         (_ (status-message :executing-smt))
         (result (display-runtime 3 "  SMT-LIB2 execution time: ~a secs"
                   (lambda ()
                     (system cmd))
                   :smt-time)))
    (unless (or (= result 0)
                (file-has-content? *sal-tmp-out-file-to-smtlib2*))
      (report-error-running-smtlib2 cmd))
    *sal-tmp-out-file-to-smtlib2*))

(define (smtlib2/execute in-file smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (smtlib2/execute-core in-file)
  (unwind-protect
   (parse-smtlib2-output-file *sal-tmp-out-file-to-smtlib2*
                              smtlib2-id->sal-decl-proc
                              smtlib2-sort->sal-type-proc
                              place-provider)
   (sal/delete-tmp-file! *sal-tmp-out-file-to-smtlib2*)))

(define *mpq-ten* (make-mpq "10"))

(define (decimal-string->mpq str)
  (let* ((len (string-length str))
         (exp-pos (let loop ((i 0))
                    (cond
                     ((= i len) #f)
                     ((or (char=? (string-ref str i) #\e)
                          (char=? (string-ref str i) #\E))
                      i)
                     (else
                      (loop (+ i 1))))))
         (mantissa (if exp-pos (substring str 0 exp-pos) str))
         (exponent (if exp-pos
                     (string->integer (substring str (+ exp-pos 1) len))
                     0))
         (mantissa-len (string-length mantissa))
         (negative? (and (> mantissa-len 0)
                         (char=? (string-ref mantissa 0) #\-)))
         (unsigned (if negative?
                     (substring mantissa 1 mantissa-len)
                     mantissa))
         (dot-pos (let loop ((i 0))
                    (cond
                     ((= i (string-length unsigned)) #f)
                     ((char=? (string-ref unsigned i) #\. ) i)
                     (else
                      (loop (+ i 1))))))
         (whole (if dot-pos (substring unsigned 0 dot-pos) unsigned))
         (fraction (if dot-pos
                     (substring unsigned (+ dot-pos 1) (string-length unsigned))
                     ""))
         (digits (let ((raw (string-append whole fraction)))
                   (if (= (string-length raw) 0) "0" raw)))
         (scale (string-length fraction))
         (base (make-mpq digits))
         (scaled (if (= scale 0)
                   base
                   (/mpq base (mpq/exp *mpq-ten* (integer->mpq scale)))))
         (shifted (cond
                   ((= exponent 0) scaled)
                   ((> exponent 0)
                    (*mpq scaled (mpq/exp *mpq-ten* (integer->mpq exponent))))
                   (else
                    (/mpq scaled (mpq/exp *mpq-ten* (integer->mpq (- 0 exponent))))))))
    (if negative?
      (-mpq *mpq-zero* shifted)
      shifted)))

(define (strip-inexact-marker token)
  (let ((len (string-length token)))
    (if (and (> len 0)
             (char=? (string-ref token (- len 1)) #\?))
      (substring token 0 (- len 1))
      token)))

(define (token->mpq token)
  (let ((clean (strip-inexact-marker token)))
    (if (or (string-index clean #\.)
            (string-index clean #\e)
            (string-index clean #\E))
      (decimal-string->mpq clean)
      (make-mpq clean))))

(define (string-index str chr)
  (let loop ((i 0)
             (len (string-length str)))
    (cond
     ((= i len) #f)
     ((char=? (string-ref str i) chr) i)
     (else
      (loop (+ i 1) len)))))

(define (digit-char? c)
  (and (char>=? c #\0)
       (char<=? c #\9)))

(define (number-token? token)
  (let* ((clean (strip-inexact-marker token))
         (len (string-length clean)))
    (and (> len 0)
         (let loop ((i 0)
                    (saw-digit? #f))
           (if (= i len)
             saw-digit?
             (let ((c (string-ref clean i)))
               (cond
                ((digit-char? c)
                 (loop (+ i 1) #t))
                ((or (char=? c #\+)
                     (char=? c #\-)
                     (char=? c #\/)
                     (char=? c #\.)
                     (char=? c #\e)
                     (char=? c #\E))
                 (loop (+ i 1) saw-digit?))
                (else
                 #f))))))))

(define (whitespace-char? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

(define (skip-comment)
  (let loop ()
    (let ((c (read-char)))
      (unless (or (eof-object? c)
                  (char=? c #\newline))
        (loop)))))

(define (read-quoted-token)
  (let loop ((chars '()))
    (let ((c (read-char)))
      (cond
       ((eof-object? c)
        (sign-error "Unexpected end-of-file while reading a quoted SMT-LIB2 symbol."))
       ((char=? c #\|)
        (list->string (reverse! chars)))
       (else
        (loop (cons c chars)))))))

(define (read-string-token)
  (let loop ((chars '())
             (escaped? #f))
    (let ((c (read-char)))
      (cond
       ((eof-object? c)
        (sign-error "Unexpected end-of-file while reading an SMT-LIB2 string."))
       ((and (not escaped?) (char=? c #\"))
        (list->string (reverse! chars)))
       ((and (not escaped?) (char=? c #\\))
        (loop (cons c chars) #t))
       (else
        (loop (cons c chars) #f))))))

(define (read-plain-token first-char)
  (let loop ((chars (list first-char)))
    (let ((c (peek-char)))
      (cond
       ((or (eof-object? c)
            (whitespace-char? c)
            (char=? c #\()
            (char=? c #\))
            (char=? c #\;))
        (list->string (reverse! chars)))
       (else
        (read-char)
        (loop (cons c chars)))))))

(define (read-next-token)
  (let loop ()
    (let ((c (read-char)))
      (cond
       ((eof-object? c) c)
       ((whitespace-char? c)
        (loop))
       ((char=? c #\;)
        (skip-comment)
        (loop))
       ((char=? c #\()
        'LP)
       ((char=? c #\))
        'RP)
       ((char=? c #\|)
        (read-quoted-token))
       ((char=? c #\")
        (read-string-token))
       (else
        (read-plain-token c))))))

(define (read-smtlib2-form token)
  (cond
   ((eq? token 'LP)
    (let loop ((forms '()))
      (let ((next-token (read-next-token)))
        (cond
         ((eof-object? next-token)
          (sign-error "Unexpected end-of-file while parsing an SMT-LIB2 list."))
         ((eq? next-token 'RP)
          (reverse! forms))
         (else
          (loop (cons (read-smtlib2-form next-token) forms)))))))
   ((eq? token 'RP)
    (sign-error "Unexpected ')' while parsing SMT-LIB2 output."))
   (else
    token)))

(define (read-smtlib2-forms)
  (let loop ((forms '()))
    (let ((token (read-next-token)))
      (if (eof-object? token)
        (reverse! forms)
        (loop (cons (read-smtlib2-form token) forms))))))

(define (smtlib2/read-forms-from-string str)
  (with-input-from-string str
    (lambda ()
      (read-smtlib2-forms))))

(define (symbol-from-string str)
  (string->symbol str))

(define (model-id->decl id)
  (and *smtlib2-id->sal-decl*
       (*smtlib2-id->sal-decl* (symbol-from-string id))))

(define (model-sort->type sort-id)
  (or (and *smtlib2-sort->sal-type*
           (*smtlib2-sort->sal-type* (symbol-from-string sort-id)))
      (make-sal-builtin-name <sal-any-type> *place-provider*)))

(define (make-model-aux-decl atom type)
  (let ((key atom))
    (cond
     ((hashtable-get *model-aux-decls* key) =>
      identity)
     (else
      (let ((decl (make-ast-instance <sal-var-decl> *place-provider*
                                     :id (make-sal-identifier *place-provider* (symbol-from-string atom))
                                     :type (or type
                                               (make-sal-builtin-name <sal-any-type> *place-provider*)))))
        (hashtable-put! *model-aux-decls* key decl)
        decl)))))

(define (make-model-name-expr atom type)
  (make-sal-name-expr (make-model-aux-decl atom type) *place-provider*))

(define (term-value? expr)
  (or (instance-of? expr <sal-lambda>)
      (instance-of? expr <sal-array-update>)
      (instance-of? expr <sal-function-update>)
      (instance-of? expr <sal-array-literal>)))

(define (eta-expand expr expected-type)
  (if (and expected-type
           (sal-type/function? expected-type)
           (not (term-value? expr)))
    (let* ((domain-types (sal-function-type/domain-types expected-type))
           (decls (let loop ((types domain-types)
                             (idx 0)
                             (result '()))
                    (if (null? types)
                      (reverse! result)
                      (loop (cdr types)
                            (+ idx 1)
                            (cons (make-ast-instance <sal-var-decl> *place-provider*
                                                     :id (make-sal-identifier *place-provider*
                                                                              (symbol-append 'arg_ (object->symbol idx)))
                                                     :type (car types))
                                  result)))))
           (args (map (lambda (decl) (make-sal-name-expr decl *place-provider*)) decls))
           (body (make-sal-application expr
                                       (apply make-application-argument args)
                                       *place-provider*))
           (class (if (sal-type/array? expected-type) <sal-array-literal> <sal-lambda>)))
      (make-ast-instance class *place-provider*
                         :local-decls decls
                         :expr body))
    expr))

(define (smtlib2-sort->sal-type sort-expr)
  (cond
   ((string? sort-expr)
    (cond
     ((string=? sort-expr "Bool")
      (make-sal-builtin-name <sal-bool-type> *place-provider*))
     ((string=? sort-expr "Int")
      (make-sal-builtin-name <sal-int-type> *place-provider*))
     ((string=? sort-expr "Real")
      (make-sal-builtin-name <sal-number-type> *place-provider*))
     (else
      (model-sort->type sort-expr))))
   ((and (pair? sort-expr)
         (string? (car sort-expr))
         (string=? (car sort-expr) "Array"))
    (let ((domain-type (smtlib2-sort->sal-type (cadr sort-expr)))
          (range-type (smtlib2-sort->sal-type (caddr sort-expr))))
      (make-ast-instance <sal-array-type> *place-provider*
                         :domain domain-type
                         :range range-type)))
   (else
    (sign-error "Unsupported SMT-LIB2 sort in model: ~a" sort-expr))))

(define (binding-list->env binding-list env)
  (let ((new-env env))
    (for-each (lambda (binding)
                (unless (and (pair? binding) (= (length binding) 2))
                  (sign-error "Unexpected SMT-LIB2 binding: ~a" binding))
                (let ((id (car binding))
                      (value-expr (cadr binding)))
                  (set! new-env
                        (cons (cons id (smtlib2-term->sal-expr value-expr env #f))
                              new-env))))
              binding-list)
    new-env))

(define (lookup-env-value atom env)
  (cond
   ((assoc atom env) => cdr)
   (else #f)))

(define (register-helper-value! atom value)
  (hashtable-put! *model-helper-values* atom value)
  value)

(define (lookup-helper-value atom)
  (cond
   ((hashtable-get *model-helper-values* atom) => identity)
   ((hashtable-get *model-helper-defs* atom) =>
    (lambda (def)
      (verbose-message 5 "      helper-value(~a): expanding definition" atom)
      (when (member atom *model-in-progress*)
        (sign-error "Cyclic helper definition encountered while decoding the SMT-LIB2 model: ~a" atom))
      (push! atom *model-in-progress*)
      (let* ((params (car def))
             (result-type (cadr def))
             (body-expr (caddr def))
             (_ (verbose-message 5 "      helper-value(~a): body ~a" atom body-expr))
             (value (if (null? params)
                      (begin
                        (verbose-message 5 "      helper-value(~a): constant definition" atom)
                        (eta-expand (smtlib2-term->sal-expr body-expr '() result-type) result-type))
                      (smtlib2-function-value params result-type body-expr))))
        (verbose-message 5 "      helper-value(~a): decoded body" atom)
        (set! *model-in-progress*
              (let loop ((remaining *model-in-progress*)
                         (result '()))
                (cond
                 ((null? remaining)
                  (reverse! result))
                 ((equal? (car remaining) atom)
                  (loop (cdr remaining) result))
                 (else
                  (loop (cdr remaining) (cons (car remaining) result))))))
        (verbose-message 5 "      helper-value(~a): caching decoded value" atom)
        (register-helper-value! atom value))))
   (else #f)))

(define (smtlib2-number-atom->expr atom)
  (make-sal-numeral (token->mpq atom) *place-provider*))

(define (smtlib2-identifier->expr atom env expected-type)
  (cond
   ((string=? atom "true")
    (make-sal-true *place-provider*))
   ((string=? atom "false")
    (make-sal-false *place-provider*))
   ((number-token? atom)
    (smtlib2-number-atom->expr atom))
   ((lookup-env-value atom env) =>
    (lambda (expr) expr))
   ((model-id->decl atom) =>
    (lambda (decl)
      (make-sal-name-expr decl *place-provider*)))
   ((lookup-helper-value atom) =>
    (lambda (expr) expr))
   (else
    (make-model-name-expr atom (or expected-type
                                   (make-sal-builtin-name <sal-any-type> *place-provider*))))))

(define (make-update-expr target idx-list new-value)
  (if (instance-of? (sal-expr/type target) <sal-array-type>)
    (make-ast-instance <sal-array-update> *place-provider*
                       :target target
                       :idx (if (= (length idx-list) 1)
                              (car idx-list)
                              (apply make-application-argument idx-list))
                       :new-value new-value)
    (make-ast-instance <sal-function-update> *place-provider*
                       :target target
                       :idx (apply make-application-argument idx-list)
                       :new-value new-value)))

(define (make-constant-array default-value array-type)
  (let* ((domain-type (sal-function-type/domain array-type))
         (decl (make-ast-instance <sal-var-decl> *place-provider*
                                  :id (make-sal-identifier *place-provider* 'idx)
                                  :type domain-type)))
    (make-ast-instance <sal-array-literal> *place-provider*
                       :local-decls (list decl)
                       :expr default-value)))

(define (qualified-id? expr name)
  (and (pair? expr)
       (= (length expr) 3)
       (string? (car expr))
       (string=? (car expr) "as")
       (string? (cadr expr))
       (string=? (cadr expr) name)))

(define (qualified-constant? expr)
  (and (pair? expr)
       (= (length expr) 3)
       (string? (car expr))
       (string=? (car expr) "as")
       (string? (cadr expr))
       (not (string=? (cadr expr) "const"))))

(define (make-builtin-app class args)
  (apply make-sal-builtin-application class *place-provider* args))

(define (smtlib2-natural-exponent term env)
  (let ((expr (smtlib2-term->sal-expr term env #f)))
    (and (instance-of? expr <sal-numeral>)
         (let ((num (slot-value expr :num)))
           (and (mpq/integer? num)
                (>=mpq num *mpq-zero*)
                (mpq->integer num))))))

(define (make-power-expr base exponent)
  (cond
   ((= exponent 0)
   (make-sal-numeral 1 *place-provider*))
   ((= exponent 1)
    base)
   (else
    (apply make-sal-builtin-application
           <sal-mul>
           *place-provider*
           (let loop ((remaining exponent)
                      (result '()))
             (if (= remaining 0)
               (reverse! result)
               (loop (- remaining 1) (cons base result))))))))

(define (smtlib2-function-value params result-type body)
  (let* ((param-decls (map (lambda (param)
                             (unless (and (pair? param) (= (length param) 2))
                               (sign-error "Unexpected SMT-LIB2 function parameter: ~a" param))
                             (let ((name (car param))
                                   (type (smtlib2-sort->sal-type (cadr param))))
                               (make-ast-instance <sal-var-decl> *place-provider*
                                                  :id (make-sal-identifier *place-provider* (symbol-from-string name))
                                                  :type type)))
                           params))
         (param-env (map (lambda (decl)
                           (cons (symbol->string (sal-decl/name decl))
                                 (make-sal-name-expr decl *place-provider*)))
                         param-decls))
         (body-expr (smtlib2-term->sal-expr body param-env result-type))
         (class (if (and result-type (sal-type/array? result-type)) <sal-array-literal> <sal-lambda>)))
    (make-ast-instance class *place-provider*
                       :local-decls param-decls
                       :expr body-expr)))

(define (smtlib2-term->sal-expr term env expected-type)
  (cond
   ((string? term)
    (smtlib2-identifier->expr term env expected-type))
   ((null? term)
    (sign-error "Unexpected empty SMT-LIB2 term in model."))
   (else
    (let ((head (car term))
          (args (cdr term)))
      (cond
       ((and (string? head) (string=? head "ite"))
        (make-ast-instance <sal-conditional> *place-provider*
                           :cond-expr (smtlib2-term->sal-expr (car args) env (make-sal-builtin-name <sal-bool-type> *place-provider*))
                           :then-expr (smtlib2-term->sal-expr (cadr args) env expected-type)
                           :else-expr (smtlib2-term->sal-expr (caddr args) env expected-type)))
       ((and (string? head) (string=? head "let"))
        (let ((extended-env (binding-list->env (car args) env)))
          (smtlib2-term->sal-expr (cadr args) extended-env expected-type)))
       ((and (string? head) (string=? head "lambda"))
        (smtlib2-function-value (car args) expected-type (cadr args)))
       ((and (string? head) (string=? head "="))
        (make-sal-equality (smtlib2-term->sal-expr (car args) env #f)
                           (smtlib2-term->sal-expr (cadr args) env #f)))
       ((and (string? head)
             (or (string=? head "distinct")
                 (string=? head "/=")))
        (make-sal-and*
         (let loop ((remaining args)
                    (result '()))
           (if (null? remaining)
             (reverse! result)
             (let ((lhs (smtlib2-term->sal-expr (car remaining) env #f)))
               (loop (cdr remaining)
                     (append (map (lambda (rhs)
                                    (make-builtin-app <sal-diseq> (list lhs (smtlib2-term->sal-expr rhs env #f))))
                                  (cdr remaining))
                             result)))))
         *place-provider*))
       ((and (string? head) (string=? head "and"))
        (make-sal-and* (map (lambda (arg) (smtlib2-term->sal-expr arg env (make-sal-builtin-name <sal-bool-type> *place-provider*))) args)
                       *place-provider*))
       ((and (string? head) (string=? head "or"))
        (make-sal-or* (map (lambda (arg) (smtlib2-term->sal-expr arg env (make-sal-builtin-name <sal-bool-type> *place-provider*))) args)
                      *place-provider*))
       ((and (string? head) (string=? head "not"))
        (make-sal-not (smtlib2-term->sal-expr (car args) env (make-sal-builtin-name <sal-bool-type> *place-provider*))))
       ((and (string? head) (string=? head "=>"))
        (make-builtin-app <sal-implies>
                          (map (lambda (arg) (smtlib2-term->sal-expr arg env (make-sal-builtin-name <sal-bool-type> *place-provider*))) args)))
       ((and (string? head) (string=? head "+"))
        (apply make-sal-builtin-application <sal-add> *place-provider*
               (map (lambda (arg) (smtlib2-term->sal-expr arg env expected-type)) args)))
       ((and (string? head) (string=? head "-"))
        (let ((converted (map (lambda (arg) (smtlib2-term->sal-expr arg env expected-type)) args)))
          (if (= (length converted) 1)
            (make-builtin-app <sal-sub> (list (make-sal-numeral 0 *place-provider*) (car converted)))
            (apply make-sal-builtin-application <sal-sub> *place-provider* converted))))
       ((and (string? head) (string=? head "*"))
        (apply make-sal-builtin-application <sal-mul> *place-provider*
               (map (lambda (arg) (smtlib2-term->sal-expr arg env expected-type)) args)))
       ((and (string? head) (string=? head "^"))
        (let ((exponent (and (= (length args) 2)
                             (smtlib2-natural-exponent (cadr args) env))))
          (unless exponent
            (sign-error "Unsupported SMT-LIB2 power term in model/interpolant: ~a" term))
          (make-power-expr (smtlib2-term->sal-expr (car args) env expected-type)
                           exponent)))
       ((and (string? head) (string=? head "/"))
        (apply make-sal-builtin-application <sal-div> *place-provider*
               (map (lambda (arg) (smtlib2-term->sal-expr arg env expected-type)) args)))
       ((and (string? head) (string=? head "div"))
        (apply make-sal-builtin-application <sal-idiv> *place-provider*
               (map (lambda (arg) (smtlib2-term->sal-expr arg env expected-type)) args)))
       ((and (string? head) (string=? head "mod"))
        (apply make-sal-builtin-application <sal-mod> *place-provider*
               (map (lambda (arg) (smtlib2-term->sal-expr arg env expected-type)) args)))
       ((and (string? head) (string=? head "<"))
        (make-builtin-app <sal-lt> (map (lambda (arg) (smtlib2-term->sal-expr arg env #f)) args)))
       ((and (string? head) (string=? head "<="))
        (make-builtin-app <sal-le> (map (lambda (arg) (smtlib2-term->sal-expr arg env #f)) args)))
       ((and (string? head) (string=? head ">"))
        (make-builtin-app <sal-gt> (map (lambda (arg) (smtlib2-term->sal-expr arg env #f)) args)))
       ((and (string? head) (string=? head ">="))
        (make-builtin-app <sal-ge> (map (lambda (arg) (smtlib2-term->sal-expr arg env #f)) args)))
       ((and (string? head) (string=? head "select"))
        (let ((target (smtlib2-term->sal-expr (car args) env #f))
              (idx (map (lambda (arg) (smtlib2-term->sal-expr arg env #f)) (cdr args))))
          (make-sal-application target
                                (apply make-application-argument idx)
                                *place-provider*)))
       ((and (string? head) (string=? head "store"))
        (let ((target (smtlib2-term->sal-expr (car args) env expected-type))
              (idx-list (map (lambda (arg) (smtlib2-term->sal-expr arg env #f))
                             (cdr (reverse (cdr (reverse args))))))
              (new-value (smtlib2-term->sal-expr (car (reverse args)) env #f)))
          (make-update-expr target idx-list new-value)))
       ((and (list? head) (qualified-id? head "const"))
        (let ((array-type (smtlib2-sort->sal-type (caddr head)))
              (default-value (smtlib2-term->sal-expr (car args) env (sal-function-type/range (smtlib2-sort->sal-type (caddr head))))))
          (make-constant-array default-value array-type)))
       ((qualified-constant? term)
        (make-model-name-expr (cadr term) (smtlib2-sort->sal-type (caddr term))))
       ((and (pair? head)
             (= (length head) 3)
             (string? (car head))
             (string=? (car head) "_")
             (string? (cadr head))
             (string=? (cadr head) "as-array"))
        (eta-expand (smtlib2-identifier->expr (caddr head) env expected-type) expected-type))
       (else
        (let ((fun-expr (smtlib2-term->sal-expr head env #f))
              (arg-exprs (map (lambda (arg) (smtlib2-term->sal-expr arg env #f)) args)))
          (make-sal-application fun-expr
                                (apply make-application-argument arg-exprs)
                                *place-provider*))))))))

(define (normalize-model-forms forms)
  (cond
   ((null? forms)
    (sign-error "Unexpected empty output produced by the SMT-LIB2 solver."))
   ((and (string? (car forms)) (string=? (car forms) "unsat"))
    #f)
   ((and (string? (car forms)) (string=? (car forms) "unknown"))
    (warning-message "The counterexample produced by the SMT-LIB2 solver may not be a true counterexample due to incompleteness.")
    (normalize-model-forms (cons "sat" (cdr forms))))
   ((and (string? (car forms)) (string=? (car forms) "sat"))
    (let ((payload (cdr forms)))
      (cond
       ((and (= (length payload) 1)
             (pair? (car payload))
             (not (null? (car payload)))
             (string? (caar payload))
             (string=? (caar payload) "model"))
        (cdar payload))
       ((and (= (length payload) 1)
             (list? (car payload)))
        (car payload))
       (else
        payload))))
   (else
    (sign-error "Unexpected SMT-LIB2 solver output: ~a" forms))))

(define (register-declare-fun! form)
  (unless (and (= (length form) 4)
               (string? (cadr form))
               (list? (caddr form)))
    (sign-error "Unexpected SMT-LIB2 declare-fun model entry: ~a" form))
  (let ((name (cadr form))
        (param-sorts (caddr form))
        (result-sort (cadddr form)))
    (unless (null? param-sorts)
      (sign-error "Unexpected non-constant SMT-LIB2 declare-fun in model: ~a" form))
    (register-helper-value! name (make-model-name-expr name (smtlib2-sort->sal-type result-sort)))))

(define (register-define-fun! form)
  (unless (and (= (length form) 5)
               (string? (cadr form))
               (list? (caddr form)))
    (sign-error "Unexpected SMT-LIB2 define-fun model entry: ~a" form))
  (let ((name (cadr form))
        (params (caddr form))
        (result-type (smtlib2-sort->sal-type (cadddr form)))
        (body (car (cddddr form))))
    (hashtable-put! *model-helper-defs* name (list params result-type body))))

(define (model-entry->constraint form)
  (cond
   ((and (pair? form)
         (string? (car form))
         (string=? (car form) "declare-fun"))
    (verbose-message 5 "    SMT-LIB2 model entry: declare-fun ~a" (cadr form))
    (register-declare-fun! form)
    '())
   ((and (pair? form)
         (string? (car form))
         (string=? (car form) "define-fun"))
    (verbose-message 5 "    SMT-LIB2 model entry: define-fun ~a" (cadr form))
    (register-define-fun! form)
    (let* ((name (cadr form))
           (decl (model-id->decl name)))
      (verbose-message 5 "      model-id->decl(~a): ~a" name (if decl 'found 'missing))
      (if decl
        (let ((value (lookup-helper-value name)))
          (verbose-message 5 "      lookup-helper-value(~a): ok" name)
          (list (make-sal-equality (make-sal-name-expr decl *place-provider*)
                                   value)))
        '())))
   (else
    '())))

(define (setup-smtlib2-decode-context! smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (set! *smtlib2-id->sal-decl* smtlib2-id->sal-decl-proc)
  (set! *smtlib2-sort->sal-type* smtlib2-sort->sal-type-proc)
  (set! *place-provider* place-provider)
  (set! *model-helper-defs* (make-hashtable))
  (set! *model-helper-values* (make-hashtable))
  (set! *model-aux-decls* (make-hashtable))
  (set! *model-in-progress* '()))

(define (coerce-model-forms forms)
  (cond
   ((null? forms)
    (normalize-model-forms forms))
   ((string? (car forms))
    (normalize-model-forms forms))
   ((and (= (length forms) 1)
         (pair? (car forms))
         (not (null? (car forms)))
         (pair? (caar forms)))
    (car forms))
   (else
    forms)))

(define (smtlib2/model-forms->constraints forms smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (setup-smtlib2-decode-context! smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (let ((model-forms (coerce-model-forms forms)))
    (and model-forms
         (apply append (map model-entry->constraint model-forms)))))

(define (smtlib2/form->sal-expr form smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (setup-smtlib2-decode-context! smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (smtlib2-term->sal-expr form '() #f))

(define (parse-smtlib2-output-file file-name smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (setup-smtlib2-decode-context! smtlib2-id->sal-decl-proc smtlib2-sort->sal-type-proc place-provider)
  (with-input-from-file file-name
    (lambda ()
      (status-message :parsing-yices-output)
      (verbose-message 3 "  parsing output produced by the SMT-LIB2 solver...")
      (let ((model-forms (normalize-model-forms (read-smtlib2-forms))))
        (verbose-message 4 "  SMT-LIB2 parser normalized ~a model entries" (if model-forms (length model-forms) 0))
        (and model-forms
             (let ((result (apply append (map model-entry->constraint model-forms))))
               (verbose-message 4 "  SMT-LIB2 parser produced ~a SAL constraints" (length result))
               result)))))))
