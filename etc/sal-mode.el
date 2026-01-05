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

(require 'derived)
(require 'cl)

(defvar sal-font-lock-keywords
  `(
   ("%[^\n]*" . 'font-lock-comment-face)
   ("\\<\\(context\\|type\\|begin\\|end\\|datatype\\|module\\|context\\|array\\|of\\|with\\|to\\|lambda\\|forall\\|exists\\|let\\|in\\|if\\|then\\|else\\|elsif\\|endif\\|input\\|output\\|global\\|local\\|definition\\|initialization\\|transition\\|rename\\|module\\|theorem\\|lemma\\|claim\\|\\importing\\|prefix\\|suffix\\|ringset\\|scalarset\\|implements\\|observe\\|CONTEXT\\|TYPE\\|BEGIN\\|END\\|DATATYPE\\|MODULE\\|CONTEXT\\|ARRAY\\|OF\\|WITH\\|TO\\|LAMBDA\\|FORALL\\|EXISTS\\|LET\\|IN\\|IF\\|THEN\\|ELSE\\|ELSIF\\|ENDIF\\|INPUT\\|OUTPUT\\|GLOBAL\\|LOCAL\\|DEFINITION\\|INITIALIZATION\\|TRANSITION\\|RENAME\\|MODULE\\|THEOREM\\|LEMMA\\|CLAIM\\|\\IMPORTING\\|PREFIX\\|SUFFIX\\|RINGSET\\|SCALARSET\\|IMPLEMENTS\\|OBSERVE\\)\\>" . 'font-lock-keyword-face)
   ("\\<\\(and\\|not\\|or\\|xor\\|false\\|true\\|mod\\|div\\|AND\\|NOT\\|OR\\|XOR\\|FALSE\\|TRUE\\|MOD\\|DIV\\|ring_succ\\|ring_pred\\)\\>" . 'font-lock-variable-name-face)
   ("\\<\\(integer\\|natural\\|nzinteger\\|real\\|nzreal\\|boolean\\|int\\|nat\\|bool\\|INTEGER\\|NATURAL\\|NZINTEGER\\|REAL\\|NZREAL\\|BOOLEAN\\)\\>" . 'font-lock-variable-name-face)
    "Default font-lock-keywords for sal mode."
   )
)

(defvar sal-mode-map ()
  "Local keymap used for SAL mode.")

(defvar sal-mode-hook nil
  "*List of functions to call when SAL mode is invoked.
This is a good place to add SAL environment specific bindings.")

(defvar sal-mode-syntax-table nil
  "Syntax table for SAL.")

(defun sal-create-syntax-table ()
  "Create the syntax table for SAL mode."
  (setq sal-mode-syntax-table (make-syntax-table))
  
  ; A % starts a comment
  (modify-syntax-entry ?% "<" sal-mode-syntax-table)
  ; A \f and \n end a comment
  (modify-syntax-entry ?\n ">" sal-mode-syntax-table)

  (modify-syntax-entry ?:  "." sal-mode-syntax-table)
  (modify-syntax-entry ?\; "." sal-mode-syntax-table)
  (modify-syntax-entry ?\|  "." sal-mode-syntax-table)
  (modify-syntax-entry ?+  "." sal-mode-syntax-table)
  (modify-syntax-entry ?-  "." sal-mode-syntax-table)
  (modify-syntax-entry ?*  "." sal-mode-syntax-table)
  (modify-syntax-entry ?/  "." sal-mode-syntax-table)
  (modify-syntax-entry ?=  "." sal-mode-syntax-table)
  (modify-syntax-entry ?<  "." sal-mode-syntax-table)
  (modify-syntax-entry ?>  "." sal-mode-syntax-table)
  (modify-syntax-entry ?. "." sal-mode-syntax-table)
  (modify-syntax-entry ?\\ "." sal-mode-syntax-table)
  (modify-syntax-entry ?\' "." sal-mode-syntax-table)
  (modify-syntax-entry ?# "." sal-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" sal-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" sal-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" sal-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" sal-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" sal-mode-syntax-table)
  (modify-syntax-entry ?\} "){" sal-mode-syntax-table)
  (set-syntax-table sal-mode-syntax-table))

;;;###autoload
(defun sal-mode ()
  "SAL mode is a major mode for editing SAL code."
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'comment-start)
  (setq comment-start "%")

  ;; comment end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before. RE
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'comment-start-skip) ;; used by autofill
  (setq comment-start-skip "%+[ \t]*")

;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'ada-indent-current-function)

  (make-local-variable 'fill-column)
  (setq fill-column 75)
  
  (make-local-variable 'comment-column)
  (setq comment-column 40)

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)

  (setq major-mode 'sal-mode)
  (setq mode-name "SAL")

  (use-local-map sal-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sal-font-lock-keywords nil nil ((?_ . "w"))))

  (if sal-mode-syntax-table
    (set-syntax-table sal-mode-syntax-table)
  (sal-create-syntax-table))

  (run-hooks 'sal-mode-hook))

(provide 'sal-mode)
