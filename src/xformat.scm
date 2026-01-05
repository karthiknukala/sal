; Copyright (C) Kenneth A Dickey (2003). All Rights Reserved.

; Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in
; the Software without restriction, including without limitation the rights to use,
; copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
; Software, and to permit persons to whom the Software is furnished to do so, 
; subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all 
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

; FILE: "xformat.scm" IMPLEMENTS: Xformat function {Scheme} -- see
;documentation below.  AUTHOR: Ken Dickey DATE: 1988 LAST UPDATED:
;2001 July 12 (Leonardo) -- adapt to use with bigloo compiler 1992
;January 8 -- now implements ~& option 1991 November 25 -- now uses
;string ports

; NOTES:        Imports PRETTY-PRINT (~g) and OBJECT->STRING

;               Pretty print and various other code is available via ftp
;               from the Scheme Repository on nexus.yorku.ca [130.63.9.1] 
;               under pub/scheme.  Contact: Ozan Yigit: oz@nexus.yorku.ca.
(module xformat
        (export (xformat <output-port> <format-string> . <args>))
        )
;;  ========
;;  FUNCTION: (XFORMAT <port> <xformat-string> . <args>)
;;  ========
;;  
;;  RESULT: returns zero-length symbol or a string; has side effect of
;;  printing according to <xformat-string>.  If <port> is #t the output is
;;  to the current output port.  If <port> is #f, a xformatted string is
;;  returned as the result of the call.  Otherwise <port> must be an
;;  output port.  <xformat-string> must be a string.  Characters are output
;;  as if the string were output by the DISPLAY function with the
;;  exception of those prefixed by a tilde (~) as follows [note that options
;;  which take arguments remove them from the argument list (they are said to
;;  be `consumed')]:
;;
;;option  mnemonic: description
;;------  ------------------------
;;   ~a  any: display the argument (as for humans).
;;   ~s  slashified: write the argument (as for parsers).
;;   ~d  decimal: the integer argument is output in decimal xformat.
;;   ~x  hexadecimal: the integer argument is output in hexadecimal xformat.
;;   ~o  octal: the integer argument is output in octal xformat.
;;   ~b  binary: the integer argument is output in binary xformat.
;;   ~p  plural: if the argument is > than 1, a lower case 's' is printed.
;;   ~c  character: the next argument is displayed as a character.
;;   ~_  space: output a space character.
;;   ~%  newline: output a newline character.
;;   ~&  freshline: unless at the beginning of a line, same as ~%, else ignored
;;   ~~  tilde: output a tilde.
;;   ~t  tab: output a tab charcter. **implemented, but system dependent**
;;   ~g  glorify: pretty print the argument (typically an s-expression).
;;   ~|  page seperator: output a page seperator.
;;   ~?  indirection: take the next argument as a xformat string and consume
;;       further arguments as appropriate, then continue to process the current
;;       xformat string.
;;
;---------- XFORMAT

(define (xformat <output-port> <xformat-string> . <args>)
  (let ((last-was-newline #f)  ; state shared between invocations
        (ascii-tab   (integer->char  9))
        (ascii-ff    (integer->char 12))
        (dont-print  (string->symbol "")))  ;; a zero character symbol
    (let* ((port (cond ((output-port? <output-port>) <output-port>)
                       ((eq? <output-port> #t) (current-output-port))
                       ((eq? <output-port> #f) (open-output-string))
                       (else (error 'xformat "xformat: bad port -> " <output-port>))))
           (return-value  (if (eq? <output-port> #f)  ;; xformat to a string
                            (lambda () (get-output-string port))
                            (lambda () dont-print))))
      (letrec ([xformat-help 
                (lambda (xformat-strg arglyst)
                  (letrec ((length-of-xformat-string (string-length xformat-strg))
                           [anychar-dispatch 
                            (lambda (pos arglist last-char-was-nl)
                              (if (>= pos length-of-xformat-string)
                                (begin
                                  (set! last-was-newline last-char-was-nl)
                                  arglist) ; used for ~? continuance
                                (let ((char (string-ref xformat-strg pos)))
                                  (cond
                                   ((eq? char #\~)
                                    (tilde-dispatch (+ pos 1) arglist last-char-was-nl))
                                   (else
                                    (write-char char port)
                                    (anychar-dispatch (+ pos 1) arglist #f))))))] ; end anychar-dispatch
                           [tilde-dispatch 
                            (lambda (pos arglist last-char-was-nl)
                              (cond
                               ((>= pos length-of-xformat-string)
                                (write-char #\~ port) ; tilde at end of string is just output
                                (set! last-was-newline last-char-was-nl)
                                arglist) ; used for ~? continuance
                               (else
                                (case (char-upcase (string-ref xformat-strg pos))
                                  ((#\A)       ; Any -- for humans
                                   (display (car arglist) port)
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\S)       ; Slashified -- for parsers
                                   (write (car arglist) port)
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\D)       ; Decimal
                                   (display (number->string (car arglist) 10) port)
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\X)       ; Hexadecimal
                                   (display (number->string (car arglist) 16) port)
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\O)       ; Octal
                                   (display (number->string (car arglist)  8) port)
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\B)       ; Binary
                                   (display (number->string (car arglist)  2) port)
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\C)       ; Character
                                   (write-char (car arglist) port) 
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\P)       ; Plural
                                   (if (= (car arglist) 1)
                                     #f ; no action
                                     (write-char #\s port))
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                  ((#\~)       ; Tilde
                                   (write-char #\~ port)
                                   (anychar-dispatch (+ pos 1) arglist #f))
                                  ((#\%)       ; Newline
                                   (write-char #\newline port)
                                   (anychar-dispatch (+ pos 1) arglist #t))
                                  ((#\_)       ; Space
                                   (write-char #\space port)
                                   (anychar-dispatch (+ pos 1) arglist #f))
                                  ((#\&)       ; Freshline
                                   (if (not last-char-was-nl)
                                     (write-char #\newline port))
                                   (anychar-dispatch (+ pos 1) arglist #t))
                                  ((#\T)       ; Tab -- Implementation dependent
                                   (write-char ascii-tab port) 
                                   (anychar-dispatch (+ pos 1) arglist #f))
                                  ((#\|)       ; Page Seperator -- Implementation dependent
                                   (write-char ascii-ff port) ;; use form-feed char
                                   (anychar-dispatch (+ pos 1) arglist #t)) ; counts as newline
                                  ((#\G)       ; Pretty-print {T}
                                   (if (eq? port #f)
                                     (pp (car arglist) (current-output-port)) ; (display (pretty-print-to-string (car arglist)) port)
                                     (pp (car arglist) port))
                                   (anychar-dispatch (+ pos 1) (cdr arglist) #t)) ; check this!
                                  ;; {"~?" in Common Lisp is "~K" in T}
                                  ((#\?)       ; indirection -- take next arg as xformat string.
                                   (set! last-was-newline last-char-was-nl)
                                   (anychar-dispatch (+ pos 1) 
                                                     (xformat-help (car arglist) (cdr arglist))
                                                     last-was-newline))
                                        ; Note: xformat-help returns unused args
                                  (else
                                   (error 'xformat "XFORMAT: unknown tilde escape" 
                                          (string-ref xformat-strg pos)))))))]) ; end tilde-dispatch
                    ;; XFORMAT-HELP MAIN
                    (anychar-dispatch 0 arglyst last-was-newline)))]) ; end xformat-help
        ;; XFORMAT MAIN
        (xformat-help <xformat-string> <args>)
        (return-value))))) ; end xformat

