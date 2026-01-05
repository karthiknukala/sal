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

(module sal-esm-options-support
        (include "sal.sch")
        (import sal2scm-core sal-esm-options
                queue sal-esm-rearrange sal-esm-expand
                sal-promote-inputs sal-esm-engine-scm-context)
        (export (esm-options->verification-context)
                (esm-options->simulation-context)
                (sal-esm/apply-module-transformations module))
        )

(define (esm-options->verification-context)
  (let ((ctx (make-sal-esm-engine-scm-context)))
    (cond
     ((eq? *esmc-traversal-strategy* 'cacheless)
      (set-slot-value! ctx :randomize? #t)
      (set-slot-value! ctx :restricted-randomization? #t))
     ((or (eq? *esmc-traversal-strategy* 'guided-cacheless)
          *esmc-randomize?*)
      (set-slot-value! ctx :randomize? #t)))
    (set-core-options! ctx)
    ctx))

(define (esm-options->simulation-context)
  (let ((ctx (make-sal-esm-engine-scm-context)))
    (cond
     (*path-explorer-guided?*
      (set-slot-value! ctx :randomize? #t))
     (else
      (set-slot-value! ctx :randomize? #t)
      (set-slot-value! ctx :restricted-randomization? #t)))
    (set-core-options! ctx)
    ctx))

(define (set-core-options! ctx)
  (when *esm-dynamic-compilation?*
    (set-slot-value! ctx :compile? #t))
  (when *esm-debug?*
    (set-slot-value! ctx :debug? #t)
    (set-slot-value! ctx :runtime-type-check? #t))
  (when *esm-use-gmp?*
    (set-slot-value! ctx :gmp? #t))
  (when *esm-infinite-loop-detection?*
    (set-slot-value! ctx :loop-detection? #t)))

(define (sal-esm/apply-module-transformations module)
  (let* ((module1 (sal-esm-module/promote-inputs module))
         (module2 (if *esm-expand-multi-commands?*
                    (sal-esm/expand module1)
                    module1))
         (module3 (if *esm-static-scheduler?* 
                    (sal-esm-module/rearrange module2)
                    module2)))
    module3))
