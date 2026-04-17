(module sal-read-forms-test
        (include "sal.sch")
        (import utility smtlib2-interface))

(pp (smtlib2/read-forms-from-string "((x 3.0))"))
(newline)
(pp (smtlib2/read-forms-from-string "((b false))"))
(newline)
