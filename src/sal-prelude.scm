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

(module sal-prelude
        (export *sal-prelude*)
        )

(define *sal-prelude* 
"(context prelude () ()

  { 
   (define-type bool (scalar { false (name-class <sal-false>) }
                             { true (name-class <sal-true>) }
                             ))
   (name-class <sal-bool-type>)
   }

  (define TRUE::bool true)
  (define FALSE::bool false)

  (define-type BOOLEAN bool)

  ;; this is a hack... I don't want to create a separated parametric context to define = and /=
  {
   (define-type any)
   (name-class <sal-any-type>)
   }

  {
   (define =::(-> any any bool))
   (app-class <sal-eq>)
   }
  
  {
   (define /=::(-> any any bool))
   (app-class <sal-diseq>)
   }
  
  (define-type boolean bool)

  {
   (define-inline (and::bool x::bool y::bool)
     (if (= x true) y false))
   (app-class <sal-and>)
   }

  { 
   (define-inline (AND::bool x::bool y::bool) (and x y))
   (app-class <sal-and>)
   }

  {
   (define-inline (or::bool x::bool y::bool)
     (if (= x true) true y))
   (app-class <sal-or>)
   }

  { 
   (define-inline (OR::bool x::bool y::bool) (or x y))
    (app-class <sal-or>)
    }

  {
   (define-inline (not::bool x::bool)
     (if (= x true) false true))
   (app-class <sal-not>)
   }

  {
   (define-inline (NOT::bool x::bool) (not x))
   (app-class <sal-not>)
   }

  {
   (define-inline (implies::bool x::bool y::bool)
     (or (not x) y))
   (app-class <sal-implies>)
   (sal-syntax =>)
   }

  {
   (define-inline (=>::bool x::bool y::bool) (implies x y))
   (app-class <sal-implies>)
   }

  {
   (define-inline (iff::bool x::bool y::bool)
     (= x y))
   (app-class <sal-iff>)
   (sal-syntax <=>)
   }

  {
   (define-inline (<=>::bool x::bool y::bool) (iff x y))
   (app-class <sal-iff>)
   }
  
  {
   (define-inline (xor::bool x::bool y::bool)
     (/= x y))
   (app-class <sal-xor>)
   }

  {
   (define-inline (XOR::bool x::bool y::bool) (xor x y))
   (app-class <sal-xor>)
   }
  
  (define-inline (nor::bool x::bool y::bool)
    (not (or x y)))

  (define-inline (nand::bool x::bool y::bool)
    (not (and x y)))

  (define-inline (xnor::bool x::bool y::bool)
    (not (xor x y)))

  {
   (define-type number)
   (name-class <sal-number-type>)
   }

  {
   (define +::(-> number number number))
   (app-class <sal-add>)
   }

  {
   (define -::(-> number number number))
   (app-class <sal-sub>)
   }
  
  {
   (define *::(-> number number number))
   (app-class <sal-mul>)
   }

  {
   (define /::(-> number number number))
   (app-class <sal-div>)
   }
  
  {
   (define <::(-> number number bool))
   (app-class <sal-lt>)
   }

  {
   (define (<=::bool x::number y::number)
     (or (< x y) (= x y)))
   (app-class <sal-le>)
   }

  {
   (define-inline (>::bool x::number y::number)
     (not (<= x y)))
   (app-class <sal-gt>)
   }
  
  {
   (define-inline (>=::bool x::number y::number)
     (not (< x y)))
   (app-class <sal-ge>)
   }
  
  {
   (define (max::number x::number y::number)
     (if (> x y) x y))
   (app-class <sal-max>)
   }

  {
   (define (min::number x::number y::number)
     (if (< x y) x y))
   (app-class <sal-min>)
   }
  
  {
   (define real-pred?::(-> number bool))
   (app-class <sal-real-pred>)
   }

  {
   (define real_pred?::(-> number bool) real-pred?)
   (app-class <sal-real-pred>)
   }

  {
   (define-type real (subtype real-pred?))
   (name-class <sal-real-type>)
   }

   (define-type REAL real)
     

  {
   (define int-pred?::(-> real bool))
   (app-class <sal-int-pred>)
   }

   { 
    (define int_pred?::(-> real bool) int-pred?)
    (app-class <sal-int-pred>)
     }

  {
   (define-type int (subtype int-pred?))
   (name-class <sal-int-type>)
   }

  (define-type integer int)
  
  (define-type INTEGER int)

  (define-inline (nat-pred?::bool n::int) (>= n 0))

  (define nat_pred?::(-> int bool) nat-pred?)

  {
   (define-type nat (subtype nat-pred?))
   (name-class <sal-nat-type>)
   }

  (define (exp::number x::number y::nat)
    (if (= y 0) 
      1
      (* x (exp x (- y 1)))))

  (define-type natural nat)

  (define-type NATURAL nat)

  (define (nzint-pred?::bool n::int) (/= n 0))
  
  (define-type nzint (subtype nzint-pred?))

  (define-type NZINTEGER nzint)

  (define (nzreal-pred?::bool n::real) (/= n 0))

  (define-type nzreal (subtype nzreal-pred?))

  (define-type NZREAL nzreal)

  (define (nznat-pred?::bool n::nat) (/= n 0))

  (define-type nznat (subtype nznat-pred?))

  {
   (define div::(-> integer integer integer))
   (app-class <sal-idiv>)
   }

  {
   (define-inline (DIV::integer x::integer y::integer) (div x y))
   (app-class <sal-idiv>)
   }
  
  {
   (define mod::(-> integer integer integer))
   (app-class <sal-mod>)
   }

  {
   (define-inline (MOD::integer x::integer y::integer) (mod x y))
   (app-class <sal-mod>)
   }

  (define-type char (subrange 0 255))

  (define-type CHAR char)

  (define-type character char)
  
  (define-type string (tuple nat (array nat char)))

  (define-type STRING string)

  (define-inline (up-to::(-> nat bool) n::nat)
    (lambda (v::nat)
      (<= v n)))

  (define-inline (up_to::(-> nat bool) n::nat)
    (up-to n))

  (define-inline (below::(-> nat bool) n::nat)
    (lambda (v::nat)
      (< v n)))

  (define-inline (above::(-> nat bool) n::nat)
    (lambda (v::nat)
      (> v n)))

  ;; the following definitions are not really correct, but they suffice for our purposes
  ;; --------------------------
  ;; LTL operators
  ;; --------------------------
  {
   (define X::(-> boolean boolean))
   (app-class <sal-ltl-x>)
   }

  {
   (define W::(-> boolean boolean boolean))
   (app-class <sal-ltl-w>)
   }

  {
   (define M::(-> boolean boolean boolean))
   (app-class <sal-ltl-m>)
   }

  {
   (define R::(-> boolean boolean boolean))
   (app-class <sal-ltl-r>)
   }

  {
   (define U::(-> boolean boolean boolean))
   (app-class <sal-ltl-u>)
   }

  {
   (define (G::bool phi::bool)
     (R false phi))
   (app-class <sal-ltl-g>)
   }
  
  {
   (define (F::bool phi::bool)
     (U true phi))
   (app-class <sal-ltl-f>)
   }

  ;; --------------------------
  ;; CTL operators
  ;; --------------------------
  {
   (define EX::(-> boolean boolean))
   (app-class <sal-ctl-ex>)
   }
  
  {
   (define AX::(-> boolean boolean))
   (app-class <sal-ctl-ax>)
   }

  {
   (define EG::(-> boolean boolean))
   (app-class <sal-ctl-eg>)
   }

  {
   (define AG::(-> boolean boolean))
   (app-class <sal-ctl-ag>)
   }

  {
   (define EF::(-> boolean boolean))
   (app-class <sal-ctl-ef>)
   }

  {
   (define AF::(-> boolean boolean))
   (app-class <sal-ctl-af>)
   }

  {
   (define EU::(-> boolean boolean boolean))
   (app-class <sal-ctl-eu>)
   }

  {
   (define AU::(-> boolean boolean boolean))
   (app-class <sal-ctl-au>)
   }

  {
   (define ER::(-> boolean boolean boolean))
   (app-class <sal-ctl-er>)
   }

  {
   (define AR::(-> boolean boolean boolean))
   (app-class <sal-ctl-ar>)
   }
  
  ;; --------------------------
  ;; Low level support for temporal logics
  ;; accepting operators
  ;; --------------------------

  {
   (define accepting::(-> boolean boolean))
   (app-class <sal-accepting>)
   }

  {
   (define weak-accepting::(-> boolean boolean))
   (app-class <sal-weak-accepting>)
   }

  ;; the following definitions are not really correct, but they suffice for our purposes
  ;; --------------------------
  ;; Ringset operators
  ;; --------------------------

  {
   (define rpred::(-> any any))
   (app-class <sal-ring-pre>)
   }

  {
   (define rsucc::(-> any any))
   (app-class <sal-ring-succ>)
   }
  ;; the following definitions are not really correct, but they suffice for our purposes
  ;; --------------------------
  ;; Debugging support
  ;; --------------------------

  {
    (define dbg_print::(-> any any))
    (app-class <sal-debug-print>)
  }

  {
    (define dbg_expr::(-> any any))
    (app-class <sal-debug-expr>)
  }

)
")
