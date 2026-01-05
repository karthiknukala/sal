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

(define *input-files* '())

(front-end/set-categories! '("Help" "Misc"))

(gen-front-end 
 lsal2xml
 "LSAL parser"
 "Usage: lsal2xml [options] [file-names]"
 "Example:
  lsal2xml *.lsal"
(lambda (else)
  (push! else *input-files*))
(begin
  (when (null? *input-files*)
    (lsal2xml/simple-help))
  (for-each (lambda (input-file)
              (let ((output-file (string-append (basename (prefix input-file)) ".xml")))
                (with-output-to-file output-file
                  (lambda ()
                    (print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                    (print "<!DOCTYPE CONTEXT SYSTEM \"sal.dtd\">")
                    (print "<!--  XML version of " input-file " -->")
                    (lsal->xml input-file *sal-env*)))
                (verbose-message 1 "File \"~a\" was generated." output-file)))
            *input-files*)))

(lsal2xml/main)

      

    
