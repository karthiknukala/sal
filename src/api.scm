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

(module api
        (import collect-info)
        (include "utility.sch")
        (include "scmobj.sch")
        (import symbol-table queue)
        (export <api-entry>
                <api-named-entry>
                <api-group>
                <api-function-entry>
                <api-generic-entry>
                <api-class-entry>
                (sign-invalid-argument proc-name arg-name pred-name)
                (sign-invalid-instance-argument proc-name arg-name class-name)
                (insert-api-entry! entry)
                (insert-api-named-entry! entry)
                (insert-method-api-entry! generic-name method-header)
                (open-api-group! . svsv)
                (close-api-group!)
                (api-entry/match entry matcher)
                (api/find-entry name)
                (api/show-entries prefix)
                (api/show-entry-names prefix)
                (api/show-entry name)
                (api/apropos regex)
                (api-entry/print entry level simple?)
                (api-entry/print-header entry)
                *api-doc-table*))


(define (sign-invalid-instance-argument proc-name arg-name class-name)
  (sign-error "function `~a': argument `~a' is not an instance of `~a'." proc-name arg-name class-name))

(define (sign-invalid-argument proc-name arg-name pred-name)
  (sign-error "function `~a': argument `~a' does not satisfy predicate ~a." proc-name arg-name pred-name))

(define *tab-size* 2)

(define *filter*
  (regular-grammar
   ((blank (in #\space #\newline #\tab)))
   ((: "@code{" (* (out #\})) "}")
    (string-append "`" (the-substring 6 (- (the-length) 1)) "'"))
   ((: "@uref{" (* (out #\})) "}")
    (string-append "`" (the-substring 6 (- (the-length) 1)) "'"))
   ("@cindex"
    (ignore))
   ("@lisp"
    (ignore))
   ((: "@end" (* #\space) "lisp")
    (ignore))
   ("@result{}"
    "==>")
   (else
    (the-failure))))

(define (filter-string str)
  (with-input-from-string str
    (lambda ()
      (with-output-to-string 
        (lambda ()
          (let loop ()
            (let ((curr (read/rp *filter* (current-input-port))))
              (unless (eof-object? curr)
                (display curr)
                (loop)))))))))

(define (api-print-indented level . args)
  (indent (* *tab-size* level))
  (apply print-indented (* *tab-size* level) args))

(define (print-hiding-marks level str)
  (api-print-indented level (filter-string str)))

(define (pp-indented level obj)
  (if (= level 0)
    (pp obj)
    (let ((str-obj (with-output-to-string
                     (lambda () (pp obj)))))
      (api-print-indented level str-obj))))

(define-class <api-entry> () (:doc :owner :file-name))
(define-class <api-named-entry> (<api-entry>) (:name))
(define-class <api-group> (<api-named-entry>) (:entries))
(define-class <api-entry-with-header> (<api-named-entry>) (:header :examples))
(define-class <api-function-entry> (<api-entry-with-header>) ())
(define-class <api-generic-entry> (<api-function-entry>) (:methods))
(define-class <api-class-entry> (<api-entry-with-header>) ())

(define *api-doc-table* (make-symbol-table))
(define *api-doc* (make-instance <api-group> 
                                 :name (string->symbol "SALenv documentation")
                                 :doc "SALenv documentation"
                                 :entries (make-queue)))
(define *api-group-stack* (list *api-doc*))

(define (print-doc entry level)
  (when (slot-value entry :file-name)
    (print "Defined at: \"" (slot-value entry :file-name) "\""))
  (if (slot-value entry :doc)
    (print-hiding-marks level (slot-value entry :doc))
    (api-print-indented level "<undocumented>")))

(define-generic (api-entry/print entry level simple?))
(define-method (api-entry/print (entry <api-entry>) (level <primitive>) (simple? <primitive>))
  (print-doc entry level))
(define-method (api-entry/print (entry <api-named-entry>) (level <primitive>) (simple? <primitive>))
  (api-print-indented level (slot-value entry :name))
  (call-next-method))
(define-method (api-entry/print (entry <api-group>) (level <primitive>) (simple? <primitive>))
  (call-next-method)
  (for-each (lambda (entry)
              (print "")
              (api-entry/print entry (+ level 1) simple?))
            (queue->list (slot-value entry :entries))))

(define (api-function-entry/print entry level simple? tag)
  (api-print-indented level tag " " (slot-value entry :header))
  (print-doc entry level)
  (when (and (not simple?) 
             (slot-value entry :examples) 
             (not (null? (slot-value entry :examples))))
    (api-print-indented level "Example(s):")
    (let ((examples (slot-value entry :examples)))
      (pp-indented (+ level 1) (car examples))
      (for-each (lambda (example)
                  (print "")
                  (pp-indented (+ level 1) example))
                (cdr examples)))))
              
(define-method (api-entry/print (entry <api-function-entry>) (level <primitive>) (simple? <primitive>))
  (api-function-entry/print entry level simple? "[function]"))
  
(define-method (api-entry/print (entry <api-generic-entry>) (level <primitive>) (simple? <primitive>))
  (api-function-entry/print entry level simple? "[generic]")
  (when (and (not simple?)
             (slot-value entry :methods)
             (not (queue/empty? (slot-value entry :methods))))
    (api-print-indented level "methods:")
    (for-each (lambda (method)
                (api-print-indented (+ level 1) method))
              (queue->list (slot-value entry :methods)))))

(define-method (api-entry/print (entry <api-class-entry>) (level <primitive>) (simple? <primitive>))
  (api-print-indented level "[class] " (slot-value entry :header))
  (print-doc entry level))

(define-generic (api-entry/print-header entry))

(define-method (api-entry/print-header (entry <api-entry>))
  ;; do nothing
  #unspecified)

(define-method (api-entry/print-header (entry <api-entry-with-header>))
  (print (slot-value entry :header)))

(define-method (api-entry/print-header (entry <api-function-entry>))
  (display "[function] ")
  (call-next-method))

(define-method (api-entry/print-header (entry <api-generic-entry>))
  (display "[generic] ")
  (call-next-method))

(define-method (api-entry/print-header (entry <api-class-entry>))
  (display "[class] ")
  (call-next-method))
  
(define-generic (api-entry/match entry matcher))
(define-method (api-entry/match (entry <api-entry>) (matcher <primitive>))
  #f)
(define-method (api-entry/match (entry <api-entry-with-header>) (matcher <primitive>))
  (or 
   (and (slot-value entry :header) (matcher (slot-value entry :header)))
   (and (slot-value entry :doc) (matcher (slot-value entry :doc)))))
  
(define-macro (define-api-generator header . body)
  `(compile-if
    (sal-check-mode)
    (define ,header ,@body)
    (define ,header #unspecified)))

(define (current-owner)
  (car *api-group-stack*))

(define (insert-api-entry! entry)
  (let ((owner (current-owner)))
    (set-slot-value! entry :owner owner)
    (queue/insert! (slot-value owner :entries) entry)))

(define-api-generator (insert-api-named-entry! entry)
  (insert-api-entry! entry)
  (when (slot-value entry :name)
    (symbol-table/add! *api-doc-table* (slot-value entry :name) entry))
  #unspecified)

(define-api-generator (insert-method-api-entry! generic-name method-header)
  (let ((entry (symbol-table/lookup *api-doc-table* generic-name)))
    (when entry
      (unless (slot-value entry :methods)
        (set-slot-value! entry :methods (make-queue)))
      (queue/insert! (slot-value entry :methods) method-header))
    #unspecified))

(define-api-generator (open-api-group! . svsv)
  (let ((new-group (apply make-instance++ <api-group>
                          :entries (make-queue)
                          svsv)))
    (set-slot-value! new-group :name (to-symbol (slot-value new-group :name)))
    (insert-api-named-entry! new-group)
    (set! *api-group-stack* (cons new-group *api-group-stack*))))

(define-api-generator (close-api-group!)
  (when (null? (cdr *api-group-stack*))
    (sign-error "The top level API document cannot be closed."))
  (set! *api-group-stack* (cdr *api-group-stack*)))

(define-macro (define-api-access header . body)
  ;; Note: removed broken [assert (error-msg) (string? error-msg)] - error-msg
  ;; does not exist at macro expansion time. This was dead code.
  `(compile-if
    (sal-check-mode)
    (define ,header ,@body)
    (define ,header (sign-error "The executable was not built with online documentation."))))

(define-api-access (api/find-entry name) 
  (unless (string-or-symbol? name)
    (sign-error "Invalid API entry name, the name must be a string or symbol."))
  (let ((entry (symbol-table/lookup *api-doc-table* (to-symbol name))))
    (unless entry
      (sign-error "There isn't an API entry for `~a'." name))
    entry))

(define (for-each-entry proc prefix)
  (let* ((prefix (to-string prefix))
         (len (string-length prefix))
         (found? #f))
    (symbol-table/for-each (lambda (entry-name entry)
                             (let ((entry-name (to-string entry-name)))
                               (when (substring=? entry-name prefix len)
                                 (set! found? #t)
                                 (proc entry)
                                 (print ""))))
                           *api-doc-table*)
    (unless found?
      (print "No entry found."))))

(define-api-access (api/show-entries prefix)
  (for-each-entry (cut api-entry/print <> 0 #f) prefix)
  "")

(define-api-access (api/show-entry-names prefix)
  (for-each-entry api-entry/print-header prefix)
  "")

(define-api-access (api/show-entry name)
  (let ((entry (api/find-entry name)))
    (api-entry/print entry 0 #f))
  "")

(define-api-access (api/apropos regex)
  (unless (string? regex)
    (sign-error "Invalid regular expression, argument must be a string for api/apropos."))
  (let ((port (current-output-port)))
    (try
     (eval `(let ((matcher (lambda (str)
                             (let ((found #f))
                               (string-case 
                                str
                                ((posix ,regex)
                                 (set! found #t)
                                 "")
                                ((or all #\newline)
                                 (ignore)
                                 ""))
                               found)))
                  (found? #f))
              (symbol-table/for-each 
               (lambda (entry-name entry)
                 (when (api-entry/match entry matcher)
                   (api-entry/print entry 0 #f)
                   (print "")
                   (set! found? #t)))
               *api-doc-table*)
              (unless found?
                (print "No entry found."))
              ""))
     (lambda (escape proc msg obj)
;;       (error-notify (make-&error #f #f proc msg obj))
       (error-notify (mk-error proc msg obj))
       (with-output-to-port port
         (lambda ()
           (sign-error "Invalid regular expression \"~a\"." regex)))))))

