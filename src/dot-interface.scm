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

(module dot-interface
        (include "utility.sch")
        (import sal-error)
        (export *dotty*
                *sal-dot-tmp-file*
                (dot/show . file))
        )

(define *dotty* "dotty")
(define *sal-dot-tmp-file* "/tmp/sal-tmp-dot.dot")

(define (dot/show . file)
  (let* ((file (optional-arg file *sal-dot-tmp-file*))
         (cmd (string-append *dotty* " \"" file "\"")))
    (unless (= (system cmd) 0)
      (sign-error "Error executing dotty. The command used was: ~a.\nIf this is not the correct command to execute Dotty, it can be changed using the statement: \n  (set! *dotty* \"<path-to-dotty>/dotty\")\nThis statement should be included in your `.salrc' file in your home directory. The temporary file used to store dot files can be set using the statement:\n  (set! *sal-dot-tmp-file* \"temp-file-name\")" cmd))))


