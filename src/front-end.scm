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


;; BD: Added comments.
;;
;; This modules provide functions and global variables to define
;; command line options, and to parse the command line of all sal
;; tools.
;; 

(module front-end
        (include "utility.sch")
        (include "scmobj.sch")
        (import xformat sal-version)
        (export *arguments*
                <sal-cmd-line-option>
                (front-end/set-categories! categories)
                (front-end/add-option! option-info)
                (front-end/show-options)
                (front-end-adapter/nat-arg proc)
                (front-end-adapter/nz-nat-arg proc)
                (sign-invalid-arg msg . args)
                (front-end/parse-args arguments else-proc)
                (front-end/add-toggle-option! category base-name desc proc)
                (front-end/add-simple-option! category name desc proc)
                (front-end/add-full-option! category short-name name desc proc)
                (front-end/set-help-proc! proc)
                (sal/header prog-name)))

(define (sal/header prog-name)
  (string-append prog-name " (Version " (salenv/version) "). Copyright (c) 2003-2011 SRI International.\nBuild date: " (salenv/build-date)))

(define *arguments* '())

(define-class <sal-cmd-line-option> () (:category :short-opt-name :long-opt-name :description :proc))

(define *options* '())

(define *active-categories* '())

(define (front-end/set-categories! categories)
  (set! *active-categories* categories))

(define (front-end/add-option! option-info)
  (push! option-info *options*))

(define (option/name-size opt)
  (if (slot-value opt :short-opt-name)
    (max (+ (string-length (slot-value opt :short-opt-name)) 1)
         (string-length (slot-value opt :long-opt-name)))
    (string-length (slot-value opt :long-opt-name))))

(define (option/full-name-size opt)
  (if (slot-value opt :short-opt-name)
    (+ (string-length (slot-value opt :short-opt-name)) 2 (string-length (slot-value opt :long-opt-name)))
    (string-length (slot-value opt :long-opt-name))))

(define (option/long-name-size opt)
  (string-length (slot-value opt :long-opt-name)))

(define (option/display-full-name opt)
  (when (slot-value opt :short-opt-name)
    (display (slot-value opt :short-opt-name))
    (display ", "))
  (display (slot-value opt :long-opt-name)))

(define (option/display-short-name opt)
  (display (slot-value opt :short-opt-name)))
    
(define (option/display-long-name opt)
  (display (slot-value opt :long-opt-name)))

(define *max-column-number* 80)
  
(define (front-end/show-category category max-opt-name-size)
  (let* ((category-options (filter (lambda (option-info) (equal? category (slot-value option-info :category))) *options*))
         (sorted-options (sort category-options (lambda (opt1 opt2)
                                                  (string<? (slot-value opt1 :long-opt-name) (slot-value opt2 :long-opt-name)))))
         (indent 0))
    (print category ":")
    (set! indent (+ max-opt-name-size 6)) ;; save space for '--' and initial tab
    (for-each (lambda (opt)
                (display "  ")
                (cond 
                 ((> (option/full-name-size opt) max-opt-name-size)
                  [assert (opt) (slot-value opt :short-opt-name)]
                  (option/display-short-name opt)
                  (print ",")
                  (display "  ")
                  (option/display-long-name opt)
                  (display (make-string (- max-opt-name-size (option/long-name-size opt)))))
                 (else
                  (option/display-full-name opt)
                  (display (make-string (- max-opt-name-size (option/full-name-size opt))))))
                (display " -- ")
                (print-boxed indent *max-column-number* (slot-value opt :description)))
              sorted-options)
    (print "")))

(define (front-end/active-options)
  (filter (lambda (opt)
            (member (slot-value opt :category) *active-categories*))
          *options*))

(define (front-end/show-options)
  (let ((max-opt-name-size 0))
    (for-each (lambda (opt)
                (when (member (slot-value opt :category) *active-categories*)
                  (set! max-opt-name-size (max (option/name-size opt) max-opt-name-size))))
              *options*)
    (for-each (lambda (category)
                (front-end/show-category category max-opt-name-size))
              *active-categories*)))
  
(define (sign-invalid-arg msg . args)
  (error 'front-end-invalid-arg (apply xformat #f msg args) #unspecified))

(define (front-end-adapter/nat-arg proc)
  (lambda (str)
    (let ((num (string->integer str)))
      (when (< num 0)
        (sign-invalid-arg "Argument must be a non negative number."))
      (proc num))))

(define (front-end-adapter/nz-nat-arg proc)
  (lambda (str)
    (let ((num (string->integer str)))
      (when (<= num 0)
        (sign-invalid-arg "Argument must be a positive number."))
      (proc num))))

(define (default-help-proc)
  (with-output-to-port (current-error-port)
    (lambda ()
      (front-end/show-options)
      (exit -1))))

(define *help-proc* default-help-proc)

(define (front-end/set-help-proc! proc)
  (set! *help-proc* proc))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Help"
                                      :short-opt-name "-?"
                                      :long-opt-name "--help"
                                      :description "This help message."
                                      :proc (lambda () 
                                              (*help-proc*))))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Misc"
                                      :short-opt-name "-V"
                                      :long-opt-name "--version"
                                      :description "Display the version number."
                                      :proc (lambda ()
                                              (print (salenv/version))
                                              (exit 0))))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Misc"
                                      :short-opt-name "-v <num>"
                                      :long-opt-name "--verbose=<num>"
                                      :description "Be verbose, where <num> is the verbosity level (default 0)."
                                      :proc (front-end-adapter/nat-arg set-verbosity!)))

(front-end/add-option! (make-instance <sal-cmd-line-option>
                                      :category "Misc"
                                      :long-opt-name "--status-messages"
                                      :description "Enable machine processable messages (default disable)."
                                      :proc (lambda ()
                                              (enable-status-messages! #t))))


(define (option/consume-arg opt arguments)
  (let ((curr-arg (car arguments)))
    (cond
     ((substring=? curr-arg "--" 2)
      (let* ((long-opt (slot-value opt :long-opt-name))
             (eq-pos (sal-string-index long-opt #\=)))
        (if eq-pos
          (if (substring=? curr-arg long-opt (+ eq-pos 1))
            (let ((arg-val (substring curr-arg (+ eq-pos 1) (string-length curr-arg))))
              ((slot-value opt :proc) arg-val)
              (cdr arguments))
            #f)
          (if (equal? curr-arg long-opt)
            (begin
              ((slot-value opt :proc))
              (cdr arguments))
            #f))))
     ((and (substring=? curr-arg "-" 1) (slot-value opt :short-opt-name))
      (let* ((short-opt (slot-value opt :short-opt-name))
             (space-pos (sal-string-index short-opt #\space)))
        (if space-pos
          (if (substring=? curr-arg short-opt space-pos)
            (if (null? (cdr arguments))
              (sign-invalid-arg "Missing argument for option `~a'." short-opt)
              (let ((arg-val (cadr arguments)))
                ((slot-value opt :proc) arg-val)
                (cddr arguments)))
            #f)
          (if (equal? curr-arg short-opt)
            (begin
              ((slot-value opt :proc))
              (cdr arguments))
            #f))))
     (else
      #f))))

(define (front-end/parse-args arguments else-proc)
  (let ((active-options (front-end/active-options)))
    (try
     (let loop ((arguments arguments))
       (if (null? arguments)
         #t
         (if (substring=? (car arguments) "-" 1)
           (loop (bind-exit (exit)
                   (for-each (lambda (opt)
                               (cond
                                ((option/consume-arg opt arguments) =>
                                 exit)))
                             active-options)
                   (sign-invalid-arg "Invalid option: ~a." (car arguments))))
           (begin (else-proc (car arguments))
                  (loop (cdr arguments))))))
     (catch 'front-end-invalid-arg
            (lambda (msg)
              (with-output-to-port (current-error-port)
                (lambda ()
                  (print msg)
                  #f)))))))

(define (front-end/add-toggle-option! category base-name desc proc)
  (front-end/add-option! (make-instance <sal-cmd-line-option>
                                        :category category
                                        :long-opt-name (string-append "--enable-" base-name)
                                        :description (string-append "Enable " desc)
                                        :proc (lambda () 
                                                (proc #t))))
  (front-end/add-option! (make-instance <sal-cmd-line-option>
                                        :category category
                                        :long-opt-name (string-append "--disable-" base-name)
                                        :description (string-append "Disable " desc)
                                        :proc (lambda () 
                                                (proc #f)))))

(define (front-end/add-simple-option! category name desc proc)
  (front-end/add-option! (make-instance <sal-cmd-line-option>
                                        :category category
                                        :long-opt-name name
                                        :description desc
                                        :proc proc)))


(define (front-end/add-full-option! category short-name name desc proc)
  (front-end/add-option! (make-instance <sal-cmd-line-option>
                                        :category category
                                        :short-opt-name short-name
                                        :long-opt-name name
                                        :description desc
                                        :proc proc)))
