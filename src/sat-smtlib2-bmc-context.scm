;;
;; SAL 3.3, Copyright (C) 2026, SRI International.  All Rights Reserved.
;;
;; SAL is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;

(module sat-smtlib2-bmc-context
        (include "sal.sch")
        (import sat-generic-bmc-context sat-context sat-smtlib2-context sat-bmc-context)
        (export <sat-smtlib2-bmc-context>
                (make-sat-smtlib2-bmc-context flat-module cont-proc)))

(define-class <sat-smtlib2-bmc-context> (<sat-smtlib2-context> <sat-generic-bmc-context>) ())

(define (make-sat-smtlib2-bmc-context flat-module cont-proc)
  (let ((ctx (make-instance <sat-smtlib2-bmc-context>)))
    (sat-generic-bmc-context/init! ctx flat-module cont-proc)))
