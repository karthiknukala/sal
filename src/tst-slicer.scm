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

(loadq "sal-eqf.scm")
(sal-env/disable-type-checker! *sal-env*)
(set-verbosity! 10)
(let ((num-args (length *arguments*))) 
  (unless (or (= num-args 3) (= num-args 2))
    (sign-error "Invalid number of arguments for SAL slicer: usage sal-slicer context assertion-name [num-steps]..."))
  (let* ((context (sal/context (car *arguments*)))
         (assertion (string->symbol (cadr *arguments*)))
         (num-steps (if (= num-args 2) #f (string->integer (caddr *arguments*)))))
    (unless (null? (sal-context/params context))
      (sign-error "The slicer does not handle parametric contexts..."))
    (let ((assertion-decl (sal-context/assertion-declaration context assertion)))
      (unless assertion-decl
        (sign-error "Unknown assertion declaration ~a" assertion))
      (let ((assertion-expr (sal-ast/instantiate (slot-value assertion-decl :assertion-expr)
                                                 '() '())))
        (unless (instance-of? assertion-expr <sal-module-models>)
          (sign-error "The slicer only handles module-models assertions..."))
        (let* ((module (slot-value assertion-expr :module))
               (property (slot-value assertion-expr :expr))
               (flat-module (sal-module/flat module))
               (sliced-module (sal-flat-module/slice flat-module property num-steps)))
          (with-output-to-file (string-append (car *arguments*) ".eqf")
            (lambda ()
            (sal-to-eqf sliced-module assertion-expr)))
          (print "\ndone."))))))

      
