(module yices2-ffi-generator
        (main main))

(define yices2-api-scalars
  '(("void" . void) ("int" . int)
    ("int32_t" . int32) ("uint32_t" . uint32)
    ("int64_t" . int64) ("uint64_t" . uint64)
    ("float" . float) ("double" . double)
    ("term_t" . int32) ("type_t" . int32)
    ("error_code_t" . int) ("smt_status_t" . int)
    ("term_constructor_t" . int) ("yices_gen_mode_t" . int)))

(define yices2-api-pointer-types
  '(("char *" yices-string "char *")
    ("const char *" yices-const-string "const char *")
    ("mpz_t" yices-mpz "mpz_ptr")
    ("mpq_t" yices-mpq "mpq_ptr")
    ("mpz_t *" yices-mpz-array "mpz_t *")
    ("mpq_t *" yices-mpq-array "mpq_t *")
    ("lp_algebraic_number_t *" yices-algebraic-number "void *")))

(define yices2-optional-symbols
  '("yices_finitefield_const_value"
    "yices_model_make_tuple"
    "yices_model_set_tuple"
    "yices_model_make_mapping"
    "yices_model_make_function"
    "yices_model_set_function"
    "yices_model_set_algebraic_number"
    "yices_get_algebraic_number_value"
    "yices_val_get_algebraic_number"))

(define (yices2-trim text)
  (pregexp-replace* "^\\s+|\\s+$" text ""))

(define (yices2-scheme-name name)
  (string->symbol (string-replace name #\_ #\-)))

(define (yices2-register-type! types name spelling)
  (hashtable-put! types name spelling)
  name)

(define (yices2-api-type c-type result? types)
  (let* ((normalized (pregexp-replace* "\\s*\\*\\s*" (yices2-trim c-type) "*"))
         (name (pregexp-replace "([^*])\\*" normalized "\\1 *"))
         (unqualified (pregexp-replace "^const " name ""))
         (scalar (assoc unqualified yices2-api-scalars))
         (pointer (or (assoc name yices2-api-pointer-types)
                      (assoc unqualified yices2-api-pointer-types))))
    (cond ((and (not result?) (string=? name "const char *")) 'string)
          (scalar (cdr scalar))
          (pointer (yices2-register-type! types (cadr pointer) (caddr pointer)))
          (else
           (match-case (pregexp-match "^([a-zA-Z0-9_]+) (\\*\\*?)$" unqualified)
             ((?- ?base ?stars)
              (let* ((base-name (pregexp-replace "_t$" base ""))
                     (pointer-name (if (string=? stars "*") "-pointer" "-pointer-pointer"))
                     (scheme-name (yices2-scheme-name (string-append "yices-" base-name pointer-name))))
                (yices2-register-type! types scheme-name unqualified)))
             (else (error 'yices2-api "Unsupported C type" c-type)))))))

(define (yices2-api-parameter parameter types)
  (let ((source (yices2-trim parameter)))
    (cond
      ((pregexp-match "^void\\s*\\(\\s*\\*\\s*[a-zA-Z0-9_]+\\s*\\)\\s*\\(void\\)$" source)
       (yices2-register-type! types 'yices-oom-callback "void (*$)(void)"))
      (else
       (match-case (pregexp-match "^(.+?[ *])[a-zA-Z0-9_]+(\\[\\])?$" source)
         ((?- ?type ?array)
          (yices2-api-type (if array (string-append type "*") type) #f types))
         (else (error 'yices2-api "Unsupported C parameter" source)))))))

(define (yices2-read-api path types)
  (call-with-input-file path
    (lambda (port)
      (let loop ((line (read-line port)) (declarations '()))
        (cond
          ((eof-object? line) (reverse! declarations))
          ((string=? (yices2-trim line) "") (loop (read-line port) declarations))
          (else
           (match-case (pregexp-match "^(.+?)(yices_[a-zA-Z0-9_]+)(?:\\((.*)\\))?;$" (yices2-trim line))
             ((?- ?type ?name ?parameters)
              (let ((result (yices2-api-type type #t types))
                    (arguments (and parameters
                                    (if (member (yices2-trim parameters) '("void" ""))
                                        '()
                                        (map (lambda (parameter) (yices2-api-parameter parameter types))
                                             (pregexp-split "," parameters))))))
                (loop (read-line port) (cons (list name result arguments) declarations))))
             (else (error 'yices2-api "Unsupported declaration" line)))))))))

;; Test the declarations with the same compiler flags and libraries as the client.
;; A reference held in a volatile variable survives through the link step.
(define (yices2-probe-error source output link?)
  (let* ((command (string-append "exec " (or (getenv "YICES2_FFI_CC") "cc") " -x c - "
                                 (if link?
                                     (string-append "-x none -o \"$1\" "
                                                    (or (getenv "YICES2_FFI_LINK") "-lyices -lgmp"))
                                     "-fsyntax-only")))
         (process (run-process "/bin/sh" "-c" command "yices2-probe" output
                               input: pipe: error: pipe:))
         (input (process-input-port process))
         (errors (process-error-port process)))
    (display source input)
    (close-output-port input)
    (let ((diagnostics (read-string errors)))
      (close-input-port errors)
      (process-wait process)
      (and (not (equal? (process-exit-status process) 0)) diagnostics))))

(define (yices2-probe-api)
  (let* ((process (run-process "mktemp" (make-file-name (os-tmp) "sal-yices2-ffi.XXXXXXXX")
                               output: pipe:))
         (port (process-output-port process))
         (output (read-line port)))
    (close-input-port port)
    (process-wait process)
    (unless (and (string? output) (equal? (process-exit-status process) 0))
      (error 'yices2-api "Could not create a temporary linker output" output))
    (unwind-protect
      (let* ((headers (append '("stdio.h" "gmp.h")
                              (if (yices2-probe-error "#include <poly/poly.h>\n" output #f)
                                  '()
                                  '("poly/poly.h"))
                              '("yices.h")))
             (includes (apply string-append
                              (map (lambda (header)
                                     (string-append "#include <" header ">\n"))
                                   headers)))
             (diagnostics (yices2-probe-error
                            (string-append includes "int main(void) { yices_init(); yices_exit(); return 0; }\n")
                            output #t)))
        (when diagnostics
          (error 'yices2-api "Cannot compile and link against Yices2" diagnostics))
        (cons headers
              (filter (lambda (name)
                        (yices2-probe-error
                          (string-append includes "int main(void) { __typeof__(&" name
                                         ") volatile symbol = &" name "; return symbol == 0; }\n")
                          output #t))
                      yices2-optional-symbols)))
      (delete-file output))))

(define (yices2-module-forms path)
  (let ((types (make-hashtable)))
    (let* ((declarations (yices2-read-api path types))
           (availability (yices2-probe-api))
           (missing (cdr availability))
           (headers (map (lambda (header) `(include ,header)) (car availability)))
           (foreign-types
            (sort (lambda (a b) (string<? (symbol->string (cadr a)) (symbol->string (cadr b))))
                  (hashtable-map types
                    (lambda (name spelling) `(type ,name (opaque) ,spelling)))))
           (externs '())
           (exports '())
           (definitions '()))
      (for-each
        (lambda (declaration)
          (match-case declaration
            ((?c-name ?result ?arguments)
             (let* ((name (yices2-scheme-name c-name))
                    (typed-name (symbol-append name ':: result)))
               (cond
                 ((member c-name missing)
                  (let ((signature
                         (cons typed-name
                               (map (lambda (type index)
                                      (symbol-append 'arg (string->symbol (number->string index)) ':: type))
                                    arguments (iota (length arguments))))))
                    (set! exports (cons signature exports))
                    (set! definitions
                          (cons `(define ,signature
                                   (error ',name "Unavailable in this Yices2 build" #f))
                                definitions))))
                 (else
                  (let ((formals (if arguments
                                     (list (map (lambda (type) (symbol-append ':: type)) arguments))
                                     '())))
                    (set! externs (cons `(macro ,typed-name ,@formals ,c-name) externs)))))))))
        declarations)
      `((module yices2-ffi
          (extern ,@headers ,@foreign-types ,@(reverse! externs))
          (export (yices-api-missing-functions)
                  (yices-string-copy value::yices-string)
                  (yices-const-string-copy value::yices-const-string)
                  ,@(reverse! exports)))
        (define (yices-api-missing-functions) ',(map yices2-scheme-name missing))
        ,@(reverse! definitions)
        ;; These conversions copy the string without taking ownership.
        (define (yices-string-copy value::yices-string)
          (pragma::obj "($1 ? string_to_bstring($1) : BFALSE)" value))
        (define (yices-const-string-copy value::yices-const-string)
          (pragma::obj "($1 ? string_to_bstring((char *)$1) : BFALSE)" value))))))

(define (yices2-write-module forms)
  (let ((temporary "yices2_ffi.scm.tmp")
        (output "yices2_ffi.scm"))
    (call-with-output-file temporary
      (lambda (port)
        (display ";; Generated from yices2_api.spec by yices2-ffi-generator.\n" port)
        (for-each (lambda (form) (pp form port)) forms)))
    (unless (rename-file temporary output)
      (error 'yices2-api "Cannot write expanded FFI module" output))))

(define (main argv)
  (match-case (cdr argv)
    ((?spec)
     (yices2-write-module (yices2-module-forms spec))
     0)
    (else (error 'yices2-ffi-generator "Expected the path to yices2_api.spec" (cdr argv)))))
