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

(module sal-parser-utility
        (include "utility.sch")
        (include "scmobj.sch")
        (include "api.sch")
        (import xformat sxml-package)
        (export
         (make-sal-place initial-line initial-column final-line final-column)
         (sal-place? obj)
         (sal-place/initial-line p)
         (sal-place/initial-column p)
         (sal-place/final-line p)
         (sal-place/final-column p)
         (sign-parser-error file-name place msg . args)
         (make-sal-token place id str)
         (sal-token? obj)
         (sal-token/place t)
         (sal-token/id t)
         (sal-token/data t)
         (has-place? obj)
         (extract-sal-place obj)
         (make-sal-place-from-places initial-place final-place)
         (make-sal-place-from initial final)
         (sal-parser-manager/set-tab-size! new-tab-size)
         (sal-parser-manager/reset! parser-manager)
         (sal-parser-manager/adjust-char parser-manager s)
         (sal-parser-manager/adjust-coord parser-manager s)
         (sal-parser-manager/the-place parser-manager s)
         (sal-parser-manager/error-token parser-manager)
         (sal-parser-manager/make-simple-token parser-manager s)
         (sal-parser-manager/make-token parser-manager id s)
         (sal-parser-manager/make-invisible-token id data)
         (make-sal-parser-manager))
        )

(define-record-type sal-place 
  (make-sal-place initial-line initial-column final-line final-column)
  sal-place?
  (initial-line sal-place/initial-line)
  (initial-column sal-place/initial-column)
  (final-line sal-place/final-line)
  (final-column sal-place/final-column))

(define-record-type sal-token
  (make-sal-token place id data)
  sal-token?
  (place sal-token/place)
  (id sal-token/id)
  (data sal-token/data))

(define (has-place? obj)
  (or (sal-token? obj)
      (sal-place? obj)
      (sxml-node? obj)))

(define (extract-sal-place obj)
  (cond
   ((sal-token? obj) (sal-token/place obj))
   ((sal-place? obj) obj)
   ((sxml-node? obj) (sxml/attribute obj 'PLACE))
   (else
    (internal-error))))

(define (make-sal-place-from-places initial-place final-place)
  (make-sal-place
   (sal-place/initial-line initial-place)
   (sal-place/initial-column initial-place)
   (sal-place/final-line final-place)
   (sal-place/final-column final-place)))

(define (make-sal-place-from initial final)
  [assert (initial final) (has-place? initial) (has-place? final)]
  (make-sal-place-from-places
   (extract-sal-place initial)
   (extract-sal-place final)))

(define (sign-parser-error file-name place msg . args)
  (let* ((place (extract-sal-place place))
         (line (sal-place/initial-line place))
         (column (sal-place/initial-column place)))
    (error 'parser 
           (if file-name 
             (apply xformat (cons* #f (string-append "[~a, line(~d), column(~d)] " msg) file-name line column args))
             (apply xformat (cons* #f (string-append "[line(~d), column(~d)] " msg) line column args)))
           #unspecified)))

(define *tab-size* 2)

(define (sal-parser-manager/set-tab-size! new-tab-size)
  (set! *tab-size* new-tab-size))

(define-class <sal-parser-manager> () (:curr-line :curr-column))

(define (make-sal-parser-manager)
  (let ((i (make-instance <sal-parser-manager>)))
    (sal-parser-manager/reset! i)
    i))

(define (sal-parser-manager/reset! parser-manager)
  (set-slot-value! parser-manager :curr-line 1)
  (set-slot-value! parser-manager :curr-column 0))

(define (sal-parser-manager/adjust-char parser-manager c)
  (cond
   ((eq? c #\newline)
    (set-slot-value! parser-manager :curr-line (+ (slot-value parser-manager :curr-line) 1))
    (set-slot-value! parser-manager :curr-column 0))
   ((eq? c #\tab)
    (set-slot-value! parser-manager :curr-column (+ (slot-value parser-manager :curr-column) *tab-size*)))
   (else
    (set-slot-value! parser-manager :curr-column (+ (slot-value parser-manager :curr-column) 1)))))

(define (sal-parser-manager/adjust-string parser-manager str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (when (< i len)
        (sal-parser-manager/adjust-char parser-manager (string-ref str i))
        (loop (+ i 1))))))

(define (sal-parser-manager/adjust-coord parser-manager obj)
  (cond 
   ((string? obj)
    (sal-parser-manager/adjust-string parser-manager obj))
   ((symbol? obj)
    (sal-parser-manager/adjust-string parser-manager (symbol->string obj)))
   ((char? obj)
    (sal-parser-manager/adjust-char parser-manager obj))
   (else
    (internal-error))))

(define (sal-parser-manager/the-place parser-manager data)
  (let ((ini-line (slot-value parser-manager :curr-line))
        (ini-col (slot-value parser-manager :curr-column)))
    (sal-parser-manager/adjust-coord parser-manager data)
    (make-sal-place ini-line ini-col (slot-value parser-manager :curr-line) (slot-value parser-manager :curr-column))))

(define (sal-parser-manager/error-token parser-manager)
  (make-sal-token (sal-parser-manager/the-place parser-manager "") 'ERROR 'ERROR))

(define (sal-parser-manager/make-simple-token parser-manager data)
  [assert (data) (or (symbol? data) (string? data))]
  (make-sal-token (sal-parser-manager/the-place parser-manager data)
                  (if (string? data) (string->symbol data) data)
                  data))
  
(define (sal-parser-manager/make-token parser-manager id data)
  [assert (data) (or (symbol? data) (string? data))]
  (make-sal-token (sal-parser-manager/the-place parser-manager data)
                  id
                  data))

(define (sal-parser-manager/make-invisible-token id data)
  [assert (data) (or (symbol? data) (string? data))]
  (make-sal-token #f id data))
  
