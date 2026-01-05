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

(module sal-path-explorer
        (include "sal.sch")
        (import sal-esm-random-simulation sal-module sal-esm
                sal-api sal-esm-options-support sal-esm-engine
                sal-esm-reflexivity sal-esm-options 
                sal-esm-guided-simulation)
        (export (sal-esmc/simulate module))
        )

(define-generic (sal-esmc/simulate module))

(define-method (sal-esmc/simulate (module <primitive>))
  [assert (module) (string? module)]
  (sal-esmc/simulate (sal/module module)))

(define-method (sal-esmc/simulate (module <sal-module>))
  (sal-esmc/simulate (sal->esm module)))

(define-method (sal-esmc/simulate (module <sal-esm-module>))
  (let* ((module (sal-esm/apply-module-transformations module))
         (ctx (esm-options->simulation-context))
         (engine (make-sal-esm-engine module ctx '())))
    (cond
     (*path-explorer-guided?*
      (unless *esm-state-weight-function*
        (sign-error "You must specify a weight function for states."))
      (let ((curr-weight-id (sal-esm/convert-user-function module *esm-state-weight-function* ctx #f))
            (next-weight-id (sal-esm/convert-user-function module *esm-state-weight-function* ctx #t)))
        (sal-esm-engine-scm-context/compile-code! ctx)
        (sal-esm/guided-simulation engine (eval curr-weight-id) (eval next-weight-id))))
     (else
      (sal-esm-engine-scm-context/compile-code! ctx)
      (sal-esm/random-simulation engine)))))

