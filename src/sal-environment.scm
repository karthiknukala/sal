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

(module sal-environment
        (include "sal.sch")
        (include "sxml-package.sch")
        (include "fast-hash-table.sch")
        (import xformat symbol-table symbol-set sal-sxml-support sal-ast-attributes
                queue sal-parser-utility sal-importer sal-type-checker
                sxml-to-sal-ast sal-context sal-string-reader lsal-string-reader
                runtime sal-decls sal-module sal-prelude)
        (export <sal-env>
                (make-sal-env)
                *make-sal-string-reader-proc*
                (sal-env/initialize-string-reader! sal-env)
                (sal-env/string-reader sal-env)
                (sal-env/with-string-reader sal-env make-reader-proc thub)
                (sal-env/expr-string->ast sal-env str)
                (sal-env/type-string->ast sal-env str)
                (sal-env/module-string->ast sal-env str)
                (sal-env/assertion-name-string->ast sal-env str)
                (sal-env/read-import-string sal-env str)
                (sal-env/enable-type-checker! sal-env)
                (sal-env/disable-type-checker! sal-env)
                (sal-env/append-to-sal-context-path! sal-env to-append)
                (sal-env/set-sal-context-path! sal-env a-salpath)
                (sal-env/add-language! sal-env extension parser)
                (sal-env/file-source-extensions sal-env)
                (sal-env/find-context-source sal-env ctx-name)
                (sal-env/change-language-parser! sal-env extension parser)
                (sal-env/parse sal-env ctx-name)
                (sal-env/parse-file sal-env ctx-file-name)
                (sal-env/reset! sal-env)
                (sal-env/import-context! sal-env ctx-name)
                (sal-env/import-context-from-file! sal-env ctx-file-name)
                (sal-env/context sal-env ctx-name)
                (sal-env/context-from-file sal-env ctx-file-name)
                (sal-env/prelude sal-env)
                (sal-env/auxiliary-context sal-env)
                (sal-env/builtin-type-decl sal-env type-name)
                (sal-env/builtin-constant-decl sal-env const-name)
                (sal-env/builtin-decl-of-class sal-env class)
                (sal/set-directory!)
                (sal/src-directory)
                (sal/directory)
                (file-name->sal-context-name ctx-file-name))
        )

(define *sal-directory* #unspecified)
(define *sal-src-directory* #unspecified)

(define *make-sal-string-reader-proc* make-sal-string-reader)

(define (sal-env/with-string-reader sal-env make-reader-proc thub)
  (let ((old-proc *make-sal-string-reader-proc*))
    (set! *make-sal-string-reader-proc* make-reader-proc)
    (sal-env/initialize-string-reader! sal-env)
    (unwind-protect
     (thub)
     (begin
       (set! *make-sal-string-reader-proc* old-proc)
       (sal-env/initialize-string-reader! sal-env)))))

(define (sal/set-directory!)
  (let ((sal-dir (getenv "SALENV_DIR")))
    (unless sal-dir
      (sign-error "SALENV_DIR environment variable is not defined. Please set it before executing SALenv."))
    (set! *sal-directory* sal-dir)
    (set! *sal-src-directory* (string-append sal-dir (make-string 1 (file-separator)) "src"))
    ))

(define-api (sal/directory)
  :doc  "Return the directory where SALenv is installed."
  *sal-directory*)

(define-api (sal/src-directory)
  :doc "Return the directory where the SALenv scripts are located."
  *sal-src-directory*)

(define (read-sal-context-path)
  (let* ((pc (path-separator))
         (convert-list (lambda (source new-list result-so-far)
                         (let loop ((source source)
                                    (new-list new-list)
                                    (result-so-far result-so-far))
                           (if (null? source) 
                             (cons new-list result-so-far)
                             (if (equal? (car source) pc)
                               (loop (cdr source) '() (cons new-list result-so-far))
                               (loop (cdr source) (cons (car source) new-list) result-so-far))))))
         (shell-path-to-list (lambda (shell-path)
                               (if shell-path
                                 (let* ((tmplist (reverse! (string->list shell-path)))
                                        (resultlist (convert-list tmplist '() '())))
                                   (map list->string resultlist))
                                 '())))
         (result (shell-path-to-list 
                  (let ((sal-context-path (getenv "SALCONTEXTPATH")))
                    (cond 
                     ((and sal-context-path (not (equal? (trim sal-context-path) "")))
                      sal-context-path)
                     (else
                      (let ((sal-path (getenv "SALPATH")))
                        (when (and sal-path (not (equal? (trim sal-path) "")))
                          (warning-message "SALPATH is now called SALCONTEXTPATH."))
;;                        (warning-message "The environment variable SALCONTEXTPATH is undefined, assuming  \".\" (i.e., just the current directory). The SALCONTEXTPATH specifies which directories will be searched for SAL context files. This is NOT a problem, but if you want a different search path, then you need to set the environment variable SALCONTEXTPATH."))
			(verbose-message 1 "SALCONTEXTPATH not defined (reading files in the current directory only)."))
                      "."))))))
    (unless (member "." result)
      (warning-message "The environment variable SALCONTEXTPATH does not include the current directory. Context files in the current directory will not be considered by SAL."))
    ;; append the source directory in the path
    (append result (list (sal/src-directory)))))
    
(define-class <sal-env> () (:contexts        ;; mapping (hashtable) from symbols to <sal-context> objects
                            :sal-context-path        ;; list of strings, each string represents a directory (SALCONTEXTPATH)
                            :extension-parser-table ;; association list of file extension names and functions
                                                    ;; each function is a parser for the given file type.
                            :type-check?            ;; flag    
                            :string-reader          ;; parser for strings 
                            :prelude                ;; reference to the prelude context (<sal-context> object)
                            :ast-attributes         ;; mapping from (ast,key) --> attr-value, this mapping is
                                                    ;; used to store extra node information.
                            :class-to-decl          
                            :auxiliary-context      ;; auxiliary context used to store expanded (flattened) definitions
                            ))

(define-api (make-sal-env)
  :doc "Create a new SAL environment object."
  (let ((result (make-instance <sal-env> 
                               :sal-context-path (read-sal-context-path)
                               :type-check? #t
                               :extension-parser-table `(("sal" . ,sal->sxml) ("lsal" . ,lsal->sxml)))))
    (sal-env/reset! result)
    result))

(define-api (sal-env/reset! (sal-env <sal-env>))
  :doc "Reinitialize a SAL environment object."
  (set-slot-value! sal-env :ast-attributes (make-sal-ast-attribute-table))
  (set-slot-value! sal-env :class-to-decl (make-eq-hash-table))
  (set-slot-value! sal-env :contexts (make-eq-hash-table))
  (set-slot-value! sal-env :prelude #f)
  (set-slot-value! sal-env :string-reader (*make-sal-string-reader-proc* sal-env))
  (import-prelude! sal-env)
  (make-auxiliary-context! sal-env))

(define (sal-env/initialize-string-reader! sal-env)
  (set-slot-value! sal-env :string-reader (*make-sal-string-reader-proc* sal-env)))

(define-api (sal-env/string-reader (sal-env <sal-env>))
  :doc "Get the current string reader used to parse fragments of SAL specification."
  (slot-value sal-env :string-reader))

(define-api (sal-env/expr-string->ast (sal-env <sal-env>) (str string?))
  :doc "Convert @code{str} in a SAL expression abstract syntax tree.
The context @code{scratch} owns the nodes of the new abstract syntax tree."
  (let ((result (sal-string-reader/read-expr (slot-value sal-env :string-reader) str)))
    (sal-ast/type-check result)
    result))

(define-api (sal-env/type-string->ast (sal-env <sal-env>) (str string?))
  :doc "Convert @code{str} in a SAL type abstract syntax tree.
The context @code{scratch} owns the nodes of the new abstract syntax tree."
  (let ((result (sal-string-reader/read-type (slot-value sal-env :string-reader) str)))
    (sal-ast/type-check result)
    result))

(define-api (sal-env/module-string->ast (sal-env <sal-env>) (str string?))
  :doc "Convert @code{str} in a SAL module abstract syntax tree.
The context @code{scratch} owns the nodes of the new abstract syntax tree."
  (let ((result (sal-string-reader/read-module (slot-value sal-env :string-reader) str)))
    (sal-ast/type-check result)
    result))

(define-api (sal-env/assertion-name-string->ast (sal-env <sal-env>) (str string?))
  :doc "Convert @code{str} in a SAL assertion name abstract syntax tree.
The context @code{scratch} owns the nodes of the new abstract syntax tree."
  (let ((result (sal-string-reader/read-assertion-name (slot-value sal-env :string-reader) str)))
    (sal-ast/type-check result)
    result))

(define-api (sal-env/read-import-string (sal-env <sal-env>) (str string?))
  :doc "Import a context in the @code{scratch} context. All binds will be available
on calls to @code{sal-env/expr-string->ast}, @code{sal-env/type-string->ast},
and @code{sal-env/module-string->ast}."
  :examples '((begin   
                (sal-env/read-import-string *sal-env* "list{nat}")
                (sal-env/expr-string->ast *sal-env* "cons(10,nil)")))
  (sal-string-reader/read-import (slot-value sal-env :string-reader) str))
              
(define-api (sal-env/enable-type-checker! (sal-env <sal-env>))
  :doc "Enable type checking. All contexts will be type checked when imported."
  (set-slot-value! sal-env :type-check? #t))

(define-api (sal-env/disable-type-checker! (sal-env <sal-env>))
  :doc "Disable type checking when loading contexts."
  (set-slot-value! sal-env :type-check? #f))

(define-api (sal-env/append-to-sal-context-path! (sal-env <sal-env>) (to-append list?))
  :doc "Append new directories to the search path. SALenv uses SALCONTEXTPATH to locate
SAL contexts."
  (set-slot-value! sal-env :sal-context-path (append (slot-value sal-env :sal-context-path) to-append))
  (sal-env/reset! sal-env))

(define-api (sal-env/set-sal-context-path! (sal-env <sal-env>) (a-path list?))
  :doc "Set a new search path. SALenv uses SALCONTEXTPATH to locate SAL contexts."
  (set-slot-value! sal-env :sal-context-path a-path)
  (sal-env/reset! sal-env))

(define-api (sal-env/add-language! (sal-env <sal-env>) (extension string?) (parser procedure?))
  :doc "Add a new parser for a given extension. @code{parser} must be a procedure that receives
a file name, and returns a SXML data structure that represents abstract syntax tree 
associated with the given file."
  (when (assoc extension (slot-value sal-env :extension-parser-table))
    (sign-error "Extension \"~a\" is already in use." extension))
  (set-slot-value! sal-env :extension-parser-table 
                   (cons (cons extension parser) 
                         (slot-value sal-env :extension-parser-table)))
  (sal-env/reset! sal-env))

(define-api (sal-env/file-source-extensions (sal-env <sal-env>))
  :doc "Return all file extensions supported by the given sal-env object."
  (map car (slot-value sal-env :extension-parser-table)))

(define-api (sal-env/find-context-source (sal-env <sal-env>) (ctx-name string-or-symbol?))
  :doc "Locate a context in the SALCONTEXTPATH. The result is the file name that specifies the
given context name."
  (let ((extensions (append (sal-env/file-source-extensions sal-env) (list "xml"))))
    (bind-exit (exit)
      (for-each (lambda (ext)
                  (let ((result (find-file/path (string-append (to-string ctx-name) "." ext) 
                                                (slot-value sal-env :sal-context-path))))
                    (when result
                      (exit result))))
                extensions)
      #f)))

(define-api (sal-env/change-language-parser! (sal-env <sal-env>) (extension string?) (parser procedure?))
  :doc "Change the parser used to parse files with a given extension. Check function @code{sal-env/add-language!}."
  (let ((pair (assoc extension (slot-value sal-env :extension-parser-table))))
    (unless pair
      (sign-error "Undefined extension \"~a\"." extension))
    (set-cdr! pair parser)))

(define-api (sal-env/parse (sal-env <sal-env>) (ctx-name string-or-symbol?))
  :doc "Parse the file that specifies the given context. The function 
@code{sal-env/find-context-source is used to locate the SAL context."
  (let ((file-name (sal-env/find-context-source sal-env ctx-name)))
    (unless file-name
      (sign-error "Source file for the context \"~a\" was not found." ctx-name))
    (if (equal? (suffix file-name) "xml")
      (values (sal/parse-xml file-name) file-name)
      (let ((parser (assoc (suffix file-name) (slot-value sal-env :extension-parser-table))))
        (unless parser
          (internal-error))
        (values ((cdr parser) file-name sal-env) file-name)))))

(define-api (sal-env/parse-file (sal-env <sal-env>) (ctx-file-name string?))
  :doc "Parse the context file."
  (unless (find-file/path ctx-file-name '("."))
    (sign-error "File \"~a\" was not found." ctx-file-name))
  (if (equal? (suffix ctx-file-name) "xml")
    (values (sal/parse-xml ctx-file-name) ctx-file-name)
    (let ((parser (assoc (suffix ctx-file-name) (slot-value sal-env :extension-parser-table))))
      (unless parser
        (sign-error "There is no registered parser for the following file extension: '~a'.\nThe function (sal-env/add-language! sal-env extension parser) can be used to register new parsers. For instance, the following command can be included in the file .salrc in your home directory to instruct SAL to use the default parser to process '.xsal' files.\n\n   (sal-env/add-language! *sal-env* \".xsal\" sal->sxml)\n" (suffix ctx-file-name)))
      (values ((cdr parser) ctx-file-name sal-env) ctx-file-name))))

(define-api (sal-env/import-context! (sal-env <sal-env>) (ctx-name string-or-symbol?))
  :doc "Import a SAL context in the sal-env object. The context will be parsed, converted
in an abstract syntax tree, and type checked (if enabled)."
  :examples '((sal-env/import-context! *sal-env* 'list))
  (let ((ctx-name (to-symbol ctx-name)))
    (unless (eq-hash-table/get (slot-value sal-env :contexts) ctx-name)
      (status-message :importing ctx-name)
      (verbose-message 1 "importing context \"~a\"..." ctx-name)
      (multiple-value-bind
          (ctx-sxml ctx-file-name)
          (sal-env/parse sal-env ctx-name)
        (import-context! sal-env ctx-sxml ctx-name ctx-file-name)))))

(define (file-name->sal-context-name ctx-file-name)
  (let loop ((ctx-name (basename ctx-file-name)))
    (let ((new-ctx-name (prefix ctx-name)))
      (if (equal? new-ctx-name ctx-name)
        ctx-name
        (loop new-ctx-name)))))

(define-api (sal-env/import-context-from-file! (sal-env <sal-env>) (ctx-file-name string?))
  :doc "Import a SAL context file in the sal-env object. The context will be parsed, converted
in an abstract syntax tree, and type checked (if enabled)."
  :examples '((sal-env/import-context! *sal-env* "/homes/demoura/tmp/peterson.sal"))
  (let ((ctx-name (file-name->sal-context-name ctx-file-name)))
    (unless (eq-hash-table/get (slot-value sal-env :contexts) ctx-name)
      (status-message :importing ctx-name)
      (verbose-message 1 "importing context \"~a\"..." ctx-name)
      (multiple-value-bind
          (ctx-sxml ctx-file-name)
          (sal-env/parse-file sal-env ctx-file-name)
        (import-context! sal-env ctx-sxml (to-symbol ctx-name) ctx-file-name)))))
      
(define (import-context! sal-env ctx-sxml ctx-name ctx-file-name)
  (try
   (let ((new-context (make-empty-context sal-env)))
     (eq-hash-table/put! (slot-value sal-env :contexts) ctx-name new-context)
     (set-slot-value! new-context :file-name ctx-file-name)
     (convert-context! ctx-sxml new-context)
     (unless (eq? ctx-name (sal-decl/name new-context))
       (sign-source-error (sxml/first-child ctx-sxml) 
                          "Invalid context name \"~a\". The context declared name and the file name must be the same." ctx-name))
     (when (slot-value sal-env :type-check?)
       (status-message :type-checking ctx-name)
       (verbose-message 1 "type checking context \"~a\"..." ctx-name)
       (display-runtime 2 "  type-checker time: ~a secs"
         (lambda () (sal-ast/type-check new-context))
         :type-checking-tyime)))
   (finally (eq-hash-table/delete! (slot-value sal-env :contexts) ctx-name))))

(define (fill-class-to-decl-table! sal-env)
  (let ((prelude (sal-env/prelude sal-env))
        (class-to-decl (slot-value sal-env :class-to-decl)))
    ;; fill the class-to-decl table
    (for-each (lambda (decl)
                (let ((name-class (sal-decl/name-class decl))
                      (app-class (and (instance-of? decl <sal-constant-decl>) (sal-decl/app-class decl))))
                  (unless (eq-hash-table/get class-to-decl app-class) ;; use the first definition
                    (when (and app-class (not (eq? app-class <sal-application>)))
                      (eq-hash-table/put! class-to-decl app-class decl))
                    (when name-class
                      (eq-hash-table/put! class-to-decl name-class decl)))))
              (sal-context/declarations prelude))))
  
(define (import-prelude! sal-env)
  (status-message :importing-prelude)
  (verbose-message 1 "importing SAL prelude...")
  (let ((ctx (lsal-string->sxml *sal-prelude*)))
    (import-context! sal-env ctx 'prelude #f)
    (set-slot-value! sal-env :prelude (cdr (eq-hash-table/get (slot-value sal-env :contexts) 'prelude)))
    (fill-class-to-decl-table! sal-env)))
  
(define (make-auxiliary-context! sal-env)
  (let ((aux (make-empty-context sal-env))
        (ctx-name 'sal-aux-ctx))
    (set-slot-value! aux :id (make-instance <sal-identifier> 
                                            :context aux
                                            :name ctx-name))
    (set-slot-value! sal-env :auxiliary-context aux)
    (eq-hash-table/put! (slot-value sal-env :contexts) ctx-name aux)))

(define-api (sal-env/context (sal-env <sal-env>) (ctx-name string-or-symbol?))
  :doc "Return a reference to a context object. The context will be imported if it was not
already loaded in memory."
  (let ((ctx-name (to-symbol ctx-name)))
    (sal-env/import-context! sal-env ctx-name)
    (cdr (eq-hash-table/get (slot-value sal-env :contexts) ctx-name))))

(define-api (sal-env/context-from-file (sal-env <sal-env>) (ctx-file-name string?))
  :doc "Return a reference to a context object. The context will be imported if it was not
already loaded in memory."
  (sal-env/import-context-from-file! sal-env ctx-file-name)
  (cdr (eq-hash-table/get (slot-value sal-env :contexts) (to-symbol (file-name->sal-context-name ctx-file-name)))))

(define-api (sal-env/prelude (sal-env <sal-env>))
  :doc "Return a reference to the Prelude context. Prelude is a builtin context that
defines the builtin types and constants of the SAL language."
  (cond
   ((slot-value sal-env :prelude) =>
    identity)
   (else 
    (sal-env/context sal-env 'prelude))))

(define (sal-env/auxiliary-context sal-env)
  (slot-value sal-env :auxiliary-context))

(define-api (sal-env/builtin-type-decl (sal-env <sal-env>) (type-name symbol?))
  (sal-context/type-declaration (sal-env/prelude sal-env) type-name))

(define-api (sal-env/builtin-constant-decl (sal-env <sal-env>) (const-name symbol?))
  (sal-context/constant-declaration (sal-env/prelude sal-env) const-name))

(define-api (sal-env/builtin-decl-of-class (sal-env <sal-env>) class)
  (cond
   ((eq-hash-table/get (slot-value sal-env :class-to-decl) class) =>
    cdr)
   (else
    ;; try to fill the table... and try again... this code is only executed when loading the prelude...
    (fill-class-to-decl-table! sal-env)
    (cond 
     ((eq-hash-table/get (slot-value sal-env :class-to-decl) class) =>
      cdr)
     (else
      (sign-error "There isn't a declaration associated with the class ~a." (class-name-of class)))))))

