;;
;; SAL 3.1.  Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
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

(require 'lisp-mode)

(defvar lsal-mode-syntax-table nil)
(if (not lsal-mode-syntax-table)
    (let ((i 0))
      (setq lsal-mode-syntax-table (make-syntax-table))
      (set-syntax-table lsal-mode-syntax-table)
      ;; Default is atom-constituent.
      (while (< i 256)
        (modify-syntax-entry i "_   ")
        (setq i (1+ i)))
      ;; Word components.
      (setq i ?0)
      (while (<= i ?9)
        (modify-syntax-entry i "w   ")
        (setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
        (modify-syntax-entry i "w   ")
        (setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
        (modify-syntax-entry i "w   ")
        (setq i (1+ i)))
      
      ;; Whitespace
      (modify-syntax-entry ?\. ".   ")
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\r "    ")
      (modify-syntax-entry ?  "    ")
      (modify-syntax-entry ?: "    ")

      (modify-syntax-entry ?{ "(}  ")
      (modify-syntax-entry ?} "){  ")
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")
      (modify-syntax-entry ?\[ "(]  ")
      (modify-syntax-entry ?\] ")[  ")
      (modify-syntax-entry ?\; "<   ")
      (modify-syntax-entry ?\" "\"    ")
      (modify-syntax-entry ?\\ "\\   ")

      ))

(defvar lsal-mode-abbrev-table nil)
(define-abbrev-table 'lsal-mode-abbrev-table ())

(defvar lsal-imenu-generic-expression
  '(("Constants"
     "(define\\s-+(?\\(\\sw+\\)" 1)
    ("Types"
     "(define-type\\s-+\\(\\sw+\\)" 1)
    ("Modules"
     "(define-module\\s-+(?\\(\\sw+\\)" 1)
    ("Assertions"
     "(\\(theorem\\|lemma\\|obligation\\|claim\\)\\s-+\\(\\sw+\\)" 2))
  "Imenu generic expression for LSAL mode.  See `imenu-generic-expression'.")

(defconst lsal-font-lock-keywords-1
  (eval-when-compile
    (list
     (list (concat "(\\(\\(define\\("
                   ;; modules and types.
                   "-module\\|-type\\|-inline\\)?\\)\\|context\\|theorem\\|lemma\\|obligation\\|claim\\|label\\)\\>"
                   ;; Any whitespace and declared object.
                   "[ \t]*(?"
                   "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(4 font-lock-function-name-face))
     ))
  "Subdued expressions to highlight in LSAL modes.")


(defconst lsal-font-lock-keywords-2
  (append lsal-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt
             '("begin" "input" "global" "output" "local" 
               "initialization" "transition" "definition"
               "scalar" "tuple" "array" "record" "datatype" 
               "mk-tuple" "mk-array" "mk-record" 
               "rename" "theorem" "lemma" "obligation" "claim"
               "hide" "with" "update" "lambda" "else" "subtype"
               "for-all" "exists" "in" "subrange" "set-list" "set-pred" 
               "cond" "if" "let" "let*" "import" "array-ref" "tuple-ref"
               "record-ref") t)
        "\\>") 1)
      (cons 
       (concat
        "\\<" (regexp-opt
               '("with") 
               t)
        "\\>") 1)
      (cons 
       (concat
        "\\<" (regexp-opt
               '("and" "not" "or" "xor" "implies" "iff" "false" "true"
                 "int" "integer" "nat" "natural" "nzint" "nzinteger" "number"
                 "real" "nzreal" "boolean" "bool") t)
        "\\>") font-lock-variable-name-face)
      
      )))
  "Expressions to highlight in LSAL modes.")

(defvar lsal-font-lock-keywords lsal-font-lock-keywords-1
  "Default expressions to highlight in LSAL modes.")

(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-indent-function, but with gets of
;; lsal-indent-{function,hook}.
(defun lsal-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'lsal-indent-function)
                         (get (intern-soft function) 'lsal-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`define" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

;;; Let is different in LSAL

(defun lsal-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'lsal-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'lsal-indent-function 0)
(put 'lambda 'lsal-indent-function 1)
(put 'for-all 'lsal-indent-function 1)
(put 'exists 'lsal-indent-function 1)
(put 'let 'lsal-indent-function 'lsal-let-indent)
(put 'let* 'lsal-indent-function 'lsal-let-indent)
(put 'if 'lsal-indent-function 1)
(put 'context 'lsal-indent-function 3)
(put 'theorem 'lsal-indent-function 1)
(put 'lemma 'lsal-indent-function 1)
(put 'obligation 'lsal-indent-function 1)
(put 'claim 'lsal-indent-function 1)
(put 'label 'lsal-indent-function 1)
(put 'rename 'lsal-indent-function 1)

(defun lsal-mode-variables ()
  (set-syntax-table lsal-mode-syntax-table)
  (setq local-abbrev-table lsal-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'lsal-indent-function)
  (setq mode-line-process '("" lsal-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression lsal-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
  '(("+-*/.<>=?!$%_&~^" . "w")))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((lsal-font-lock-keywords
           lsal-font-lock-keywords-1 lsal-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun))))

(defvar lsal-mode-line-process "")

(defvar lsal-mode-map nil
  "Keymap for LSAL mode.")

(unless lsal-mode-map
  (let ((map (make-sparse-keymap "LSAL")))
    (setq lsal-mode-map (make-sparse-keymap))
    (define-key lsal-mode-map [menu-bar] (make-sparse-keymap))
    (define-key lsal-mode-map [menu-bar lsal]
      (cons "LSAL" map))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))

(defun lsal-mode-commands (map)
  t
  )

(defun lsal-mode ()
  "Major mode for editing LSAL code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior SALenv process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all LSAL buffers.
Entry to this mode calls the value of `lsal-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (lsal-mode-initialize)
  (lsal-mode-variables)
  (run-hooks 'lsal-mode-hook))


(defun lsal-mode-initialize ()
  (use-local-map lsal-mode-map)
  (setq major-mode 'lsal-mode)
  (setq mode-name "LSAL"))

(defgroup lsal nil
  "Editing LSAL code"
  :group 'SAL)

(defcustom lsal-mode-hook nil
  "Normal hook run when entering `lsal-mode'.
See `run-hooks'."
  :type 'hook
  :group 'lsal)


(provide 'lsal-mode)
