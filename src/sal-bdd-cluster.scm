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

(module sal-bdd-cluster
        (include "sal.sch")
        (include "fast-hash-table.sch")
        (import bdd sal-bdd-context sal-bdd-fsm sal2bdd 
                bdd-util front-end sal-expression sal-pp
                queue)
        (export (sal-bdd-fsm/image fsm state-set)
                (sal-bdd-fsm/image-core fsm lower-bound upper-bound care-set)
                (sal-bdd-fsm/disj-image fsm state-set)
                (sal-bdd-fsm/disj-image-core fsm lower-bound upper-bound care-set)
                (sal-bdd-fsm/pre-condition fsm state-set)
                (sal-bdd-fsm/pre-image-with-choices fsm state-set)
                (sal-bdd-fsm/pre-image-core fsm lower-bound upper-bound care-set)
                (sal-bdd-fsm/pre-image fsm state-set)
                <sal-bdd-cluster>
                <sal-bdd-monolithic-cluster>
                <sal-bdd-disj-cluster>
                <sal-bdd-conj-cluster>
                <sal-bdd-cluster-element>
                (sal-bdd-cluster/set-max-cluster-size! max)
                (sal-bdd-cluster/set-small-cluster-size! size)
                (sal-bdd-cluster/image cluster state-set care-set vars-cube)
                (sal-bdd-cluster/pre-image cluster state-set care-set vars-cube)
                (sal-bdd-cluster/disj-image cluster state-set care-set vars-cube)
                (sal-bdd-cluster/num-clusters cluster)
                (sal-bdd-cluster/size cluster)
                (sal-bdd-cluster/display-info cluster nesting)
                (sal-bdd-cluster/compress cluster)
                (sal-bdd-cluster/push-tiny-bdds cluster)
                (sal-bdd-cluster->bdd cluster)
                (bdd->sal-bdd-monolithic-cluster fsm bdd)
                (bdd-list->sal-bdd-monolithic-cluster-list fsm bdd-list)
                (sal-expr->cluster-core ast env fsm)
                (sal-expr->cluster ast env fsm)
                (cluster-element-list/support manager cluster-element-list)
                (cluster-element/support cluster-elem))
        )

;; using the values of W1, W2, W3, and W4 proposed in Iwls95
(define *iwls95-W1* 6)
(define *iwls95-W2* 1)
(define *iwls95-W3* 1)
(define *iwls95-W4* 2)

(front-end/add-full-option! 
 "BDD Interface" "-W1 <num>" "--W1=<num>"
 "Weight attached with variables getting smoothed.  This weight is used in the clustering heurisitic defined in IWLS95 (default: 6)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (set! *iwls95-W1* arg))))
 
(front-end/add-full-option! 
 "BDD Interface" "-W2 <num>" "--W2=<num>"
 "Weight attached with the support count of a cluster. This weight is used in the clustering heurisitic defined in IWLS95 (default: 1)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (set! *iwls95-W2* arg))))

(front-end/add-full-option! 
 "BDD Interface" "-W3 <num>" "--W3=<num>"
 "Weight attached with variables getting introduced. This weight is used in the clustering heurisitic defined in IWLS95 (default: 1)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (set! *iwls95-W3* arg))))

(front-end/add-full-option! 
 "BDD Interface" "-W4 <num>" "--W4=<num>"
 "Weight attached with the maximum variable position in a cluster. This weight is used in the clustering heurisitic defined in IWLS95 (default: 2)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (set! *iwls95-W4* arg))))

(define *sal-bdd-max-cluster-size* 4096)

(define (sal-bdd-cluster/set-max-cluster-size! max)
  (set! *sal-bdd-max-cluster-size* max))

(define *sal-bdd-small-cluster-size* 512)

(define (sal-bdd-cluster/set-small-cluster-size! size)
  (set! *sal-bdd-small-cluster-size* size))

(define *sal-create-let-bdd-variables?* #f) 

(define (sal-bdd-cluster/set-create-let-bdd-variables?* flag)
  (set! *sal-create-let-bdd-variables?* flag))

(front-end/add-simple-option!
 "BDD Interface" 
 "--max-cluster-size=<num>"
 "Desired maximum cluster size. This threshold is used in the clustering heurisitic defined in IWLS95 (default: 4096)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (sal-bdd-cluster/set-max-cluster-size! arg))))

(front-end/add-simple-option!
 "BDD Interface" 
 "--small-cluster-size=<num>"
 "Define the maximum size of a cluster tagged as `small' (default: 512)."
 (front-end-adapter/nz-nat-arg 
  (lambda (arg) 
    (sal-bdd-cluster/set-small-cluster-size! arg))))

(front-end/add-toggle-option!
 "BDD Interface"
 "let-bdd-vars"
 "creation of let-BDD-variables (default: disabled). A let-BDD-variable is created to minimize the size of the transition relation. Each new let-BDD-variable corresponds to a new cluster. This option is ingored if a monolithic transition relation is required."
 (lambda (flag)
   (set! *sal-create-let-bdd-variables?* flag)))

(define-api (sal-bdd-fsm/pre-condition fsm state-set)
  :doc "Returns a BDD representing the precondition for the set represented by the BDD @code{state-set}. The precondition is defined as @code{(sal-bdd-fsm/complement fsm (sal-bdd-fsm/pre-image fsm (sal-bdd-fsm/complement fsm state-set)))}."
  (sal-bdd-fsm/complement fsm (sal-bdd-fsm/pre-image fsm (sal-bdd-fsm/complement fsm state-set))))

(define-api (sal-bdd-fsm/image fsm state-set)
  :doc "Returns a BDD representing the image of the set represented by the BDD @code{state-set}."
  (sal-bdd-fsm/image-core fsm state-set state-set (bdd/true (sal-bdd-fsm/manager fsm))))

(define-api (sal-bdd-fsm/disj-image fsm state-set)
  :doc "Returns a list of BDDs. The disjunction of these BDDs represent the image of the set represented by the BDD @code{state-set}."
  (sal-bdd-fsm/disj-image-core fsm state-set state-set (bdd/true (sal-bdd-fsm/manager fsm))))

(define (sal-bdd-fsm/image-core fsm lower-bound upper-bound care-set)
  (let* ((forward-cube (sal-bdd-fsm/forward-cube fsm))
         (care-set' (sal-bdd-fsm/map-curr->next fsm care-set))
         (domain-subset (bdd/between lower-bound upper-bound))
         (image' (sal-bdd-cluster/image (slot-value fsm :transition-relation-cluster) 
                                        (sal-bdd-fsm/filter-invalid-state-rep fsm domain-subset)
                                        care-set'
                                        forward-cube)))
    ;; I have to filter invalid state to avoid problems with the IN operator.
    ;; the example in-bug2.lsal contains an example that produces a false counterexample,
    ;; when I do not filter invalid-states...
    (sal-bdd-fsm/filter-invalid-state-rep fsm (sal-bdd-fsm/map-next->curr fsm image'))))
;; I should check if I need to add self loops in the invalid representation states 
;; (e.g. x:[0,2] uses 2 bits but (true, true) is invalid representation).
;; If I need this self loops I should add the following states to the result of image-core and pre-image-core
;; (sal-bdd-fsm/invalid-states fsm domain-subset))))

(define (sal-bdd-fsm/disj-image-core fsm lower-bound upper-bound care-set)
  (let* ((forward-cube (sal-bdd-fsm/forward-cube fsm))
         (care-set' (sal-bdd-fsm/map-curr->next fsm care-set))
         (domain-subset (bdd/between lower-bound upper-bound))
         (image-list' (sal-bdd-cluster/disj-image (slot-value fsm :transition-relation-cluster) 
                                                  (sal-bdd-fsm/filter-invalid-state-rep fsm domain-subset)
                                                  care-set'
                                                  forward-cube)))
    (map (lambda (image-bdd')
           (sal-bdd-fsm/filter-invalid-state-rep fsm (sal-bdd-fsm/map-next->curr fsm image-bdd')))
         image-list')))

(define-api (sal-bdd-fsm/pre-image-with-choices fsm state-set)
  :doc "Returns a BDD representing the pre-image of the set represented by the set @code{state-set}, but the value of choice variables is also included. This function is useful for constructing counterexamples. Choice variables are auxiliary variables created by SAL to store which transition was executed at each step."
  (sal-bdd-fsm/pre-image-core fsm state-set state-set (bdd/true (sal-bdd-fsm/manager fsm))))

(define (sal-bdd-fsm/pre-image-core fsm lower-bound upper-bound care-set)
  (let* ((backward-cube (sal-bdd-fsm/backward-cube fsm))
         (domain-subset (bdd/between lower-bound upper-bound))
         (domain-subset' (sal-bdd-fsm/map-curr->next fsm (sal-bdd-fsm/filter-invalid-state-rep fsm domain-subset)))
         (prev-state-set (sal-bdd-cluster/pre-image (slot-value fsm :transition-relation-cluster) 
                                                    domain-subset'
                                                    care-set
                                                    backward-cube)))
    ;; read the comment in sal-bdd-fsm/image-core
     (sal-bdd-fsm/filter-invalid-state-rep fsm prev-state-set)))

(define-api (sal-bdd-fsm/pre-image fsm state-set)
  :doc "Returns a BDD representing the pre-image of the set represented by the set @code{state-set}."
  (sal-bdd-fsm/remove-choice-vars fsm (sal-bdd-fsm/pre-image-with-choices fsm state-set)))
    
(define-class <sal-bdd-cluster> () (:fsm)
  :doc "Superclass of all BDD cluster objects. @code{:fsm} is the finite state machine which owns this cluster. Clusters are used to save memory in the encoding of the transition relation. They need to be used because it big examples is infeasible to build a monolithic BDD that represents the whole transition relation.")
(define-class <sal-bdd-monolithic-cluster> (<sal-bdd-cluster>) (:bdd)
  :doc "A cluster represented by a single BDD (@code{:bdd}).")
(define-class <sal-bdd-disj-cluster> (<sal-bdd-cluster>) (:cluster-list)
  :doc "Represents the disjunction of the clusters in the list @code{:cluster-list}.")
(define-class <sal-bdd-conj-cluster> (<sal-bdd-cluster>) (:forward-cluster-element-list :backward-cluster-element-list)
  :doc "Represents the conjunction of the clusters elements (see @code{<sal-bdd-cluster-element>}) in the list @code{:forward-cluster-element-list}. The list @code{:backward-cluster-element-list} is a permutation of the list @code{:forward-cluster-element-list}. @code{:forward-cluster-element-list}/@code{:backward-cluster-element-list} is used in the forward/backward computation.")
(define-class <sal-bdd-cluster-element> () (:cluster :e-vars-cube :info)
  :doc "Represents an element in @code{<sal-bdd-conj-cluster>}. @code{:cluster} contains the nested cluster. @code{:e-vars-cube} is the a BDD cube which contains the variables which may be existentially abstracted immediately after this object is processed by the image and/or pre-image procedures.")
(define-class <sal-bdd-let-cluster> (<sal-bdd-cluster>) (:nested-cluster :let-var-cube :num-let-variables)
  :doc "Represents a cluster which uses extra variables to represent the nested cluster.")

(define-inline (sal-bdd-cluster/manager cluster)
  (sal-bdd-context/manager (slot-value cluster :fsm)))

;;
;; Standard Image/PreImage computation functions...
;; 
(define-generic (sal-bdd-cluster/image cluster state-set care-set vars-cube))
(define-generic (sal-bdd-cluster/pre-image cluster state-set care-set vars-cube))

(define (sal-bdd-monolithic-cluster/image-core cluster state-set care-set vars-cube)
  (let* ((relation (slot-value cluster :bdd))
         (care-relation relation)) ;; (bdd/cofactor relation care-set))) <-- this function is broken, it explodes 
    (bdd/and (bdd/and-exists state-set care-relation vars-cube) care-set))) ;; <-- simple use of care-sets
  
(define-method (sal-bdd-cluster/image (cluster <sal-bdd-monolithic-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (sal-bdd-monolithic-cluster/image-core cluster state-set care-set vars-cube))

(define-method (sal-bdd-cluster/pre-image (cluster <sal-bdd-monolithic-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (sal-bdd-monolithic-cluster/image-core cluster state-set care-set vars-cube))

(define (sal-bdd-disj-cluster/image-core cluster state-set care-set proc-child vars-cube)
  (let ((cluster-list (slot-value cluster :cluster-list)))
    (let loop ((cluster-list (cdr cluster-list))
               (result (proc-child (car cluster-list) state-set care-set vars-cube)))
      (if (null? cluster-list)
        result
        (let* ((curr-image (proc-child (car cluster-list) state-set care-set vars-cube))
               (new-result (bdd/or result curr-image)))
          (loop (cdr cluster-list) new-result))))))

(define-method (sal-bdd-cluster/image (cluster <sal-bdd-disj-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (sal-bdd-disj-cluster/image-core cluster state-set care-set sal-bdd-cluster/image vars-cube))

(define-method (sal-bdd-cluster/pre-image (cluster <sal-bdd-disj-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (sal-bdd-disj-cluster/image-core cluster state-set care-set sal-bdd-cluster/pre-image vars-cube))

(define (sal-conj-bdd-cluster/image-core cluster state-set care-set proc-child cluster-elem-list vars-cube)
  (let* ((m (bdd/manager state-set))
         (collect! (make-gc-collector-for m))
         (max-int-size 0))
    (let loop ((curr-product state-set)
               (cluster-elem-list cluster-elem-list))
      (collect!)
      (if (null? cluster-elem-list)
        (let* ((result (bdd/exists curr-product vars-cube))
               (final-size (bdd/size result)))
          (status-message :conj-cluster-image final-size max-int-size)
          (verbose-message 4 "    total size of product: ~a nodes" final-size)
          (verbose-message 4 "    maximum intermediate product: ~a nodes" max-int-size)
          result)
        (let* ((curr-cluster-element (car cluster-elem-list))
               (curr-cluster (slot-value curr-cluster-element :cluster))
               (curr-e-vars-cube (slot-value curr-cluster-element :e-vars-cube))
               (_ [assert (cluster-elem-list curr-e-vars-cube) 
                          (for-all (lambda (cluster-elem)
                                     (bdd/empty-cube? (bdd/cube-intersection curr-e-vars-cube
                                                                             (cluster-element/support cluster-elem))))
                                   (cdr cluster-elem-list))])
               ;; curr-e-vars-cube is the set of variables that can be existencially quantified
               ;; vars-cube is the set of variables that we want to remove
               (new-vars-cube (bdd/cube-intersection curr-e-vars-cube vars-cube))
               (new-product (proc-child curr-cluster curr-product care-set new-vars-cube)))
          (let ((int-size (bdd/size new-product)))
            (verbose-message 4 "    size of intermediate product: ~a nodes" int-size)
            (set! max-int-size (max int-size max-int-size)))
          (loop new-product (cdr cluster-elem-list)))))))
  
(define-method (sal-bdd-cluster/image (cluster <sal-bdd-conj-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (sal-conj-bdd-cluster/image-core cluster state-set care-set sal-bdd-cluster/image 
                                   (slot-value cluster :forward-cluster-element-list)
                                   vars-cube))

(define-method (sal-bdd-cluster/pre-image (cluster <sal-bdd-conj-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (sal-conj-bdd-cluster/image-core cluster state-set care-set sal-bdd-cluster/pre-image 
                                   (slot-value cluster :backward-cluster-element-list)
                                   vars-cube))

(define-method (sal-bdd-cluster/image (cluster <sal-bdd-let-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (bdd/exists (sal-bdd-cluster/image (slot-value cluster :nested-cluster) state-set care-set vars-cube)
              (slot-value cluster :let-var-cube)))
(define-method (sal-bdd-cluster/pre-image (cluster <sal-bdd-let-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (bdd/exists (sal-bdd-cluster/pre-image (slot-value cluster :nested-cluster) state-set care-set vars-cube)
              (slot-value cluster :let-var-cube)))

;; Compute the image of state-set. The result is a list (disjunction) of BDDs.
;; The key idea is that we do not combine the BDDs produced by the components of a disjuctive-cluster.
(define-generic (sal-bdd-cluster/disj-image cluster state-set care-set vars-cube))

(define-method (sal-bdd-cluster/disj-image (cluster <sal-bdd-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (list (sal-bdd-cluster/image cluster state-set care-set vars-cube)))

(define-method (sal-bdd-cluster/disj-image (cluster <sal-bdd-disj-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (map (lambda (nested-cluster)
         (sal-bdd-cluster/image nested-cluster state-set care-set vars-cube)) 
       (slot-value cluster :cluster-list)))

(define-method (sal-bdd-cluster/disj-image (cluster <sal-bdd-let-cluster>) (state-set <bdd>) (care-set <bdd>) (vars-cube <bdd>))
  (map (lambda (bdd)
         (bdd/exists bdd (slot-value cluster :let-var-cube)))
       (sal-bdd-cluster/disj-image (slot-value cluster :nested-cluster) state-set care-set vars-cube)))

(define-generic (sal-bdd-cluster/size cluster)
  :doc "Returns the number of BDD nodes in a BDD cluster object. See @code{<sal-bdd-cluster>}.")
(define-method (sal-bdd-cluster/size (cluster <sal-bdd-monolithic-cluster>))
  (bdd/size (slot-value cluster :bdd)))
(define-method (sal-bdd-cluster/size (cluster <sal-bdd-disj-cluster>))
  (fold-left (lambda (size child)
               (+ size (sal-bdd-cluster/size child)))
             0
             (slot-value cluster :cluster-list)))
(define-method (sal-bdd-cluster/size (cluster <sal-bdd-conj-cluster>))
  (fold-left (lambda (size cluster-elem)
               (+ size (sal-bdd-cluster/size (slot-value cluster-elem :cluster))))
             0
             (slot-value cluster :forward-cluster-element-list)))
(define-method (sal-bdd-cluster/size (cluster <sal-bdd-let-cluster>))
  (sal-bdd-cluster/size (slot-value cluster :nested-cluster)))
  
(define-generic (sal-bdd-cluster/num-clusters cluster)
  :doc "Returns the number of clusters in the BDD cluster @code{cluster}. See @code{<sal-bdd-cluster>}.")
(define-method (sal-bdd-cluster/num-clusters (cluster <sal-bdd-monolithic-cluster>))
  1)
(define-method (sal-bdd-cluster/num-clusters (cluster <sal-bdd-disj-cluster>))
  (fold-left (lambda (num child)
               (+ num (sal-bdd-cluster/num-clusters child)))
             0
             (slot-value cluster :cluster-list)))
(define-method (sal-bdd-cluster/num-clusters (cluster <sal-bdd-conj-cluster>))
  (fold-left (lambda (num cluster-elem)
               (+ num (sal-bdd-cluster/num-clusters (slot-value cluster-elem :cluster))))
             0
             (slot-value cluster :forward-cluster-element-list)))
(define-method (sal-bdd-cluster/num-clusters (cluster <sal-bdd-let-cluster>))
  (+ 1 (sal-bdd-cluster/num-clusters (slot-value cluster :nested-cluster))))

(define-generic (sal-bdd-cluster->bdd cluster)
  :doc "Converts a BDD cluster object (see @code{<sal-bdd-cluster>}) into a single BDD.")
(define-method (sal-bdd-cluster->bdd (cluster <sal-bdd-monolithic-cluster>))
  (slot-value cluster :bdd))
(define-method (sal-bdd-cluster->bdd (cluster <sal-bdd-disj-cluster>))
  (let ((cluster-list (slot-value cluster :cluster-list)))
    (fold-left (lambda (bdd child-cluster)
                 (bdd/or bdd (sal-bdd-cluster->bdd child-cluster)))
               (sal-bdd-cluster->bdd (car cluster-list))
               (cdr cluster-list))))
(define-method (sal-bdd-cluster->bdd (cluster <sal-bdd-conj-cluster>))
  (let ((cluster-elem-list (slot-value cluster :forward-cluster-element-list)))
    (fold-left (lambda (bdd cluster-elem)
                 (bdd/and bdd (sal-bdd-cluster->bdd (slot-value cluster-elem :cluster))))
               (sal-bdd-cluster->bdd (slot-value (car cluster-elem-list) :cluster))
               (cdr cluster-elem-list))))
(define-method (sal-bdd-cluster->bdd (cluster <sal-bdd-let-cluster>))
  (bdd/exists (sal-bdd-cluster->bdd (slot-value cluster :nested-cluster)) (slot-value cluster :auxiliary-vars-cube)))

(define-generic (sal-bdd-cluster/display-info cluster nesting)
  :doc "Display information regarding the BDD cluster object @code{cluster}.")
(define-method (sal-bdd-cluster/display-info (cluster <sal-bdd-monolithic-cluster>) (nesting <primitive>))
  (verbose-message 3 "~amonolithic cluster size: ~a nodes" (make-string nesting) (sal-bdd-cluster/size cluster))
  ;;(bdd/print-debug (bdd/support (slot-value cluster :bdd))))
  )
(define-method (sal-bdd-cluster/display-info (cluster <sal-bdd-disj-cluster>) (nesting <primitive>))
  (verbose-message 3 "~adisjunctive cluster" (make-string nesting))
  (for-each (cut sal-bdd-cluster/display-info <> (+ nesting 2)) (slot-value cluster :cluster-list))
  (verbose-message 3 "~atotal size: ~a nodes" (make-string nesting) (sal-bdd-cluster/size cluster)))
(define-method (sal-bdd-cluster/display-info (cluster <sal-bdd-conj-cluster>) (nesting <primitive>))
    (verbose-message 3 "~aconjunctive cluster" (make-string nesting))
    (for-each (lambda (cluster-elem)
                (sal-bdd-cluster/display-info (slot-value cluster-elem :cluster) (+ nesting 2)))
              (slot-value cluster :forward-cluster-element-list))
    (verbose-message 3 "~atotal size: ~a nodes" (make-string nesting) (sal-bdd-cluster/size cluster)))
(define-method (sal-bdd-cluster/display-info (cluster <sal-bdd-let-cluster>) (nesting <primitive>))
  (verbose-message 3 "~anumber of let (auxiliary) variables: ~a" (make-string nesting) (slot-value cluster :num-let-variables))
  (sal-bdd-cluster/display-info (slot-value cluster :nested-cluster) (+ nesting 2)))

(define-generic (sal-bdd-cluster/compress cluster)
  :doc "Try the number of nested BDD clusters objects in the given cluster object.")
(define-method (sal-bdd-cluster/compress (cluster <sal-bdd-monolithic-cluster>))
  cluster)
(define-method (sal-bdd-cluster/compress (cluster <sal-bdd-conj-cluster>))
  (let* ((cluster-list (map (cut slot-value <> :cluster) (slot-value cluster :forward-cluster-element-list)))
         (new-cluster-list (map sal-bdd-cluster/compress cluster-list)))
    (sal-bdd-cluster-list->sal-bdd-conj-cluster new-cluster-list)))
(define-method (sal-bdd-cluster/compress (cluster <sal-bdd-disj-cluster>))
  (let* ((cluster-list (slot-value cluster :cluster-list))
         (new-cluster-list (map sal-bdd-cluster/compress cluster-list)))
    (sal-bdd-cluster-list->sal-bdd-disj-cluster new-cluster-list)))
(define-method (sal-bdd-cluster/compress (cluster <sal-bdd-let-cluster>))
  (copy-instance cluster :nested-cluster 
                 (sal-bdd-cluster/compress (slot-value cluster :nested-cluster))))

;; The following function performs the conjunction of a cluster with a BDD.
;; This function is also useful to avoid funny cluster structures such as:
;;   (conjunctive-cluster
;;        tiny-monolithic-cluster
;;        (disjuctive-cluster
;;             ...
;;        ))
;; It is usually better to push the tiny cluster inside of the disjuctive cluster
(define (sal-bdd-cluster/push-tiny-bdds cluster)
  (status-message :rearranging-clusters)
  (verbose-message 2 "  rearranging clusters...")
  (let ((m (sal-bdd-cluster/manager cluster)))
    (sal-bdd-cluster/push-tiny-bdds-core cluster (bdd/true m))))

(define-generic (sal-bdd-cluster/push-tiny-bdds-core cluster bdd))
(define-method (sal-bdd-cluster/push-tiny-bdds-core (cluster <sal-bdd-monolithic-cluster>) (bdd <primitive>))
  (let ((new-cluster (bdd->sal-bdd-monolithic-cluster (slot-value cluster :fsm) bdd)))
    (sal-bdd-cluster-list->sal-bdd-conj-cluster (list cluster new-cluster))))
(define-method (sal-bdd-cluster/push-tiny-bdds-core (cluster <sal-bdd-disj-cluster>) (bdd <primitive>))
  (let* ((cluster-list (slot-value cluster :cluster-list))
         (new-cluster-list (map (cut sal-bdd-cluster/push-tiny-bdds-core <> bdd) cluster-list)))
    (sal-bdd-cluster-list->sal-bdd-disj-cluster new-cluster-list)))
(define-method (sal-bdd-cluster/push-tiny-bdds-core (cluster <sal-bdd-conj-cluster>) (bdd <primitive>))
  (let* ((cluster-list (map (cut slot-value <> :cluster) (slot-value cluster :forward-cluster-element-list)))
         (len (length cluster-list))
         (monolithic-cluster-list '())
         (disjuctive-cluster-list '()))
    [assert (len) (>= len 2)]
    [assert (cluster-list cluster) (for-all (lambda (child-cluster)
                                              (not (instance-of? child-cluster <sal-bdd-conj-cluster>)))
                                            cluster-list)]
    (for-each (lambda (child-cluster)
                (cond
                 ((instance-of? child-cluster <sal-bdd-monolithic-cluster>)
                  (push! child-cluster monolithic-cluster-list))
                 (else
                  [assert (child-cluster) (instance-of? child-cluster <sal-bdd-disj-cluster>)]
                  (push! child-cluster disjuctive-cluster-list))))
              cluster-list)
    (cond
     ((= (length monolithic-cluster-list) 1)
      (let ((aux-cluster (sal-bdd-cluster/push-tiny-bdds-core (car monolithic-cluster-list) bdd)))
        (if (and (instance-of? aux-cluster <sal-bdd-monolithic-cluster>)
                 (< (bdd/size (slot-value aux-cluster :bdd)) *sal-bdd-small-cluster-size*))
          (let ((new-disjunctive-cluster-list (map (cut sal-bdd-cluster/push-tiny-bdds-core <> (slot-value aux-cluster :bdd)) disjuctive-cluster-list)))
            (sal-bdd-cluster-list->sal-bdd-conj-cluster new-disjunctive-cluster-list))
          (let* ((true-bdd (bdd/true (sal-bdd-cluster/manager cluster)))
                 ;; look for tiny bdd inside new-disjunctive-cluster-list
                 (new-disjunctive-cluster-list (map (cut sal-bdd-cluster/push-tiny-bdds-core <> true-bdd) disjuctive-cluster-list)))
            (sal-bdd-cluster-list->sal-bdd-conj-cluster (cons aux-cluster new-disjunctive-cluster-list)))))) 
     (else
      (let* ((aux-cluster (bdd->sal-bdd-monolithic-cluster (slot-value cluster :fsm) bdd))
             (true-bdd (bdd/true (sal-bdd-cluster/manager cluster)))
             ;; look for tiny bdd inside cluster-list
             (new-cluster-list (map (cut sal-bdd-cluster/push-tiny-bdds-core <> true-bdd) cluster-list)))
        (sal-bdd-cluster-list->sal-bdd-conj-cluster (cons aux-cluster new-cluster-list))))))) 
(define-method (sal-bdd-cluster/push-tiny-bdds-core (cluster <sal-bdd-let-cluster>) (bdd <primitive>))
  (copy-instance cluster :nested-cluster 
                 (sal-bdd-cluster/push-tiny-bdds-core (slot-value cluster :nested-cluster) bdd)))

(define-generic (sal-bdd-cluster->cluster-list cluster))
(define-method (sal-bdd-cluster->cluster-list (cluster <sal-bdd-monolithic-cluster>))
  (list cluster))
(define-method (sal-bdd-cluster->cluster-list (cluster <sal-bdd-disj-cluster>))
  (slot-value cluster :cluster-list))
(define-method (sal-bdd-cluster->cluster-list (cluster <sal-bdd-conj-cluster>))
  (map (cut slot-value <> :cluster) (slot-value cluster :forward-cluster-element-list)))
(define-method (sal-bdd-cluster->cluster-list (cluster <sal-bdd-let-cluster>))
  (list cluster))

;; there is one <iwls95-cluster-info> instance per cluster
;; the various fields of this structure contain the information for
;; ordering the clusters for image computation purposes.
(define-class <iwls95-cluster-info> () 
  (:supp-Ti         ;; set of support of cluster T_i
   :supp-Q-Ci       ;; {v : v \in S(T_j), j != i, j \in Q}
   :PSPI-Ti         ;; {v : v \in (PS U PI) & v \in S(T_i)}
   :NS-Ti           ;; {v : v \in NS & v \in S(T_i)}
   :v_c             ;; number of variables which can be existentially quantified when T_i is multiplied in a product
   :w_c             ;; number of current and input variables in the support of T_i
   :x_c             ;; number of current and input variables which have not yet been quantified
   :y_c             ;; number of next variables that would be introduced in the product by multiplying T_i
   :z_c             ;; number of next variables not yet introduced in the product
   :m_c             ;; maximum index of a variable to be quantified in S(T_i) (set of support of T_i)
   :M_c))           ;; maximum BDD index of a variable to be quantified out in the remaining clusters

;; compute the benefit function associated to each cluster.
;; the objective function attached with each Ti is
;; Ci =  W1*C1 + W2*C2 - W3*C3 + W4*C4
;; where:
;;  W1 = weight attached with variables getting smoothed
;;  W2 = weight attached with the support count of the Ti
;;  W3 = weight attached with variables getting introduced
;;  W4 = weight attached with the max bdd id of the Ti
;;  C1 = v_c/w_c
;;  C2 = w_c/x_c
;;  C3 = y_c/z_c
;;  C4 = m_c/M_c
(define-macro (ratio slot1 slot2)
  `(if (> (slot-value cluster-info ,slot1) 0)
     (/fl (exact->inexact (slot-value cluster-info ,slot1))
          (exact->inexact (slot-value cluster-info ,slot2)))
     0.0))
(define-macro (term W slot1 slot2)
  `(*fl (exact->inexact ,W)
        (ratio ,slot1 ,slot2)))
(define (cluster-info/benefit cluster-info)
  (with-output-to-trace 'bdd-cluster
                        (trace 'bdd-cluster "computing cluster benefit")
                        (sal/pp cluster-info)
                        (print ""))
  (+fl (-fl (+fl (term *iwls95-W1* :v_c :w_c)
                 (term *iwls95-W2* :w_c :x_c))
            (term *iwls95-W3* :y_c :z_c))
       (term *iwls95-W4* :m_c :M_c)))

;; this is a generic function to compute the set of support of a list of elements.
;; proc-elem-support must return the set of support of an element.
;; the result is a BDD cube.
(define (elem-list/support manager elem-list proc-elem-support)
  (let loop ((elem-list elem-list)
             (result (bdd/true manager)))
    (if (null? elem-list)
      result
      (let* ((curr-elem (car elem-list))
             (curr-support (proc-elem-support curr-elem))
             (new-result (bdd/and result curr-support)))
        (loop (cdr elem-list) new-result)))))

;; return the set of support of an instance of <sal-bdd-cluster>
;; the result is a BDD cube.
(define-generic (sal-bdd-cluster/support cluster))

;; return the set of support of an instance of <sal-bdd-cluster-element>
(define (cluster-element/support cluster-elem)
  (sal-bdd-cluster/support (slot-value cluster-elem :cluster)))

(define-method (sal-bdd-cluster/support (cluster <sal-bdd-monolithic-cluster>))
  (bdd/support (slot-value cluster :bdd)))

(define-method (sal-bdd-cluster/support (cluster <sal-bdd-disj-cluster>))
  (elem-list/support (sal-bdd-cluster/manager cluster)
                     (slot-value cluster :cluster-list)
                     sal-bdd-cluster/support))

(define (cluster-element-list/support manager cluster-element-list)
  (elem-list/support manager
                     cluster-element-list
                     cluster-element/support))

(define-method (sal-bdd-cluster/support (cluster <sal-bdd-conj-cluster>))
  (cluster-element-list/support (sal-bdd-cluster/manager cluster)
                                (slot-value cluster :forward-cluster-element-list)))

(define-method (sal-bdd-cluster/support (cluster <sal-bdd-let-cluster>))
  (bdd/cube-diff (sal-bdd-cluster/support (slot-value cluster :nested-cluster))
                 (slot-value cluster :let-var-cube)))

(define (adjust v)
  (if (< v 0) 0 v))

;; compute the value of x_c, z_c and M_c  
;; m is the bdd manager
;; Q is a list of <sal-bdd-cluster-element>
;; PSPI the cube of present state and primary input variables
;; NS the cube of next state variables
(define (compute-cluster-info-aux m Q PSPI NS)
  (let* ((acc (cluster-element-list/support m Q))
         (acc-PSPI (bdd/cube-intersection acc PSPI))
         (acc-NS (bdd/cube-intersection acc NS)))
    (values (adjust (- (bdd/size acc-PSPI) 1))   ;; x_c -1 because I don't want to count the constant 1 node
            (adjust (- (bdd/size acc-NS) 1))    ;; z_c 
            (bdd/cube-max-pos acc-PSPI)))) ;; M_c

;; computes the set Supp_Q_Ci, that is, the set of support of Q/{Ci}
;;
;; m is the bdd manager
;; Q is a list of <sal-bdd-cluster-element>
;; c-i is a <sal-bdd-cluster-element>
(define (cluster-element-list/supp-Q-Ci m Q c-i)
  (let loop ((Q Q)
             (acc (bdd/true m)))
    (if (null? Q)
      acc
      (let ((c-j (car Q)))
        (if (eq? c-i c-j)
          (loop (cdr Q) acc)
          (let* ((supp (cluster-element/support c-j))
                 (new-acc (bdd/and acc supp)))
            (loop (cdr Q) new-acc)))))))

;; compute <iwls95-cluster-info>
;; m is the bdd manager
;; Q is a list of <sal-bdd-cluster-element> that need to be filled.
;; PS cube of present state variables
;; PI cube of input variables
;; NS cube of next state variables
(define (cluster-element-list/compute-info! m Q PS PI NS)
  (let ((PSPI (bdd/and PS PI)))
    (multiple-value-bind
        (x_c z_c M_c)
        (compute-cluster-info-aux m Q PSPI NS)
      (let loop ((clist Q))
        (unless (null? clist)
          (let* ((curr-cluster-elem (car clist))
                 (curr-cluster (slot-value curr-cluster-elem :cluster))
                 (supp-Ti (sal-bdd-cluster/support curr-cluster))
                 (PSPI-Ti (bdd/cube-intersection supp-Ti PSPI))
                 (NS-Ti (bdd/cube-intersection supp-Ti NS))
                 (supp-Q-Ci (cluster-element-list/supp-Q-Ci m Q curr-cluster-elem))
                 (e-i (bdd/cube-diff PSPI-Ti supp-Q-Ci))
                 (_ [assert (Q e-i) 
                            (bdd/empty-cube? (bdd/cube-intersection 
                                              (cluster-element-list/support m (remq curr-cluster-elem Q))
                                              e-i))])
                 (v_c (adjust (- (bdd/size e-i) 1))) ;; -1 because I don't want to count the constant 1 node
                 (w_c (adjust (- (bdd/size PSPI-Ti) 1)))
                 (y_c (adjust (- (bdd/size NS-Ti) 1)))
                 (m_c (bdd/cube-max-pos PSPI-Ti))
                 (info (make-instance <iwls95-cluster-info>
                                      :supp-Ti supp-Ti
                                      :supp-Q-Ci supp-Q-Ci
                                      :PSPI-Ti PSPI-Ti
                                      :NS-Ti NS-Ti
                                      :v_c v_c
                                      :w_c w_c
                                      :x_c x_c
                                      :y_c y_c
                                      :z_c z_c
                                      :m_c m_c
                                      :M_c M_c)))
            (set-slot-value! curr-cluster-elem :e-vars-cube e-i)
            (set-slot-value! curr-cluster-elem :info info)
            (loop (cdr clist))))))))

;; reorder a list of <sal-bdd-cluster-element> using the heuristic described in IWLS95
(define (cluster-element-list/reorder m unordered PS PI NS)
  (trace 'bdd-cluster "starting cluster reordering...")
  (let loop ((clist unordered)
             (olist '()))
    (trace 'bdd-cluster "cluster reordering iteration...")
    (if (null? clist)
      (let ((ordered (reverse! olist)))
        [assert (unordered ordered) (= (length unordered) (length ordered))]
        (trace 'bdd-cluster "end of cluster reordering")
        ordered)
      (let ((best-benefit -99999.0)
            (best-cluster #unspecified))
        (cluster-element-list/compute-info! m clist PS PI NS)
        [assert (m clist) (for-all (lambda (cluster-elem) (slot-value cluster-elem :info)) clist)]
        (let inner-loop ((clist clist))
          (unless (null? clist)
            (let* ((current (car clist))
                   (info (slot-value current :info))
                   (benefit (cluster-info/benefit info)))
              (when (>fl benefit best-benefit)
                (set! best-benefit benefit)
                (set! best-cluster current)))
            (inner-loop (cdr clist))))
        (set-slot-value! best-cluster :info #unspecified) ;; we don't need the info anymore
        (let ((new-clist (remq best-cluster clist))
              (new-olist (cons best-cluster olist)))
          [assert (m new-clist best-cluster)
                  (bdd/empty-cube? (bdd/cube-intersection (slot-value best-cluster :e-vars-cube) (cluster-element-list/support m new-clist)))]
          (loop new-clist new-olist))))))

;; break a cluster list into a bdd-list and a new cluster-list
;; the <sal-bdd-monolithic-cluster> are mapped to bdds and go to the bdd-list
;; the other clusters go to the new cluster-list
(define (cluster-list->bdd-list-cluster-list cluster-list owner-class)
  (let loop ((cluster-list cluster-list)
             (bdd-list '())
             (new-cluster-list '()))
    (if (null? cluster-list)
      (values bdd-list new-cluster-list)
      (let ((curr (car cluster-list)))
        (cond
         ((instance-of? curr <sal-bdd-monolithic-cluster>)
          (loop (cdr cluster-list) (cons (slot-value curr :bdd) bdd-list) new-cluster-list))
         ((instance-of? curr owner-class)
          (loop (append (sal-bdd-cluster->cluster-list curr) (cdr cluster-list)) bdd-list new-cluster-list))
         (else
          (loop (cdr cluster-list) bdd-list (cons curr new-cluster-list))))))))
      
(define (bdd->sal-bdd-monolithic-cluster fsm bdd)
  (make-instance <sal-bdd-monolithic-cluster>
                 :fsm fsm
                 :bdd bdd))

(define (bdd-list->sal-bdd-monolithic-cluster-list fsm bdd-list)
  (map (cut bdd->sal-bdd-monolithic-cluster fsm <>) bdd-list))

(define (bdd-list->sal-bdd-cluster-element-list fsm bdd-list)
  (map (lambda (bdd)
         (make-instance <sal-bdd-cluster-element>
                        :cluster (bdd->sal-bdd-monolithic-cluster fsm bdd)))
       bdd-list))

(define (cluster-list->cluster-element-list cluster-list)
  (map (lambda (cluster)
         (make-instance <sal-bdd-cluster-element>
                        :cluster cluster))
       cluster-list))

(define (cluster-element-list/copy cluster-elem-list)
  (let ((result-list (make-queue)))
    (for-each (lambda (cluster-elem)
                (queue/insert! result-list (shallow-copy cluster-elem)))
              cluster-elem-list)
    (queue->list result-list)))

(define *sal-bdd-tiny-cluster-size* 32)
                
(define (sal-bdd-cluster-list->sal-bdd-conj-cluster cluster-list)
  [assert (cluster-list) (not (null? cluster-list))]
  (let* ((fsm (slot-value (car cluster-list) :fsm))
         (m (sal-bdd-cluster/manager (car cluster-list)))
         (PS (sal-bdd-fsm/forward-cube fsm))
         (PI (bdd/true m)) ;; it is not really used
         (NS (sal-bdd-fsm/backward-cube fsm)))
    (multiple-value-bind
        (bdd-list cluster-list)
        (cluster-list->bdd-list-cluster-list cluster-list <sal-bdd-conj-cluster>)
      ;; (breakpoint "sal-bdd-cluster-list->sal-bdd-conj-cluster" (bdd-list *sal-bdd-max-cluster-size* fsm cluster-list) (> (bdd/size (car bdd-list)) 300))
      ;; (print "************** CLUSTERING ***************")
      ;;(for-each (lambda (bdd)
      ;;  (bdd/print-debug (bdd/support bdd)))
      ;;  bdd-list)
      ;; (print "*****************************************")

      (let* ((pre-compressed-bdd-list (bdd-and/affinity-cluster bdd-list *sal-bdd-max-cluster-size*)) ;; group bdds
             (compressed-bdd-list (filter (lambda (bdd)
                                            (not (bdd/true? bdd))) ;; remove the true BDDs
                                          pre-compressed-bdd-list)))
        (cond
         ((and (null? compressed-bdd-list)
               (null? cluster-list))
          (bdd->sal-bdd-monolithic-cluster fsm (bdd/true m)))
         ((and (= (length compressed-bdd-list) 1)
               (null? cluster-list))
          (bdd->sal-bdd-monolithic-cluster fsm (car compressed-bdd-list)))
         ((and (= (length compressed-bdd-list) 2)
               (null? cluster-list)
               (or (< (bdd/size (car compressed-bdd-list)) *sal-bdd-tiny-cluster-size*)
                   (< (bdd/size (cadr compressed-bdd-list)) *sal-bdd-tiny-cluster-size*)))
          ;; I don't want clusters composed of big-bdd AND tiny-bdd
          (bdd->sal-bdd-monolithic-cluster fsm (bdd/and (car compressed-bdd-list) (cadr compressed-bdd-list))))
         ((and (null? compressed-bdd-list)
               (= (length cluster-list) 1))
          (car cluster-list))
         (else 
          (let* ((cluster-elem-list (append (bdd-list->sal-bdd-cluster-element-list fsm compressed-bdd-list)
                                            (cluster-list->cluster-element-list cluster-list)))
                 (copy-of-cluster-elem-list (cluster-element-list/copy cluster-elem-list))
                 (forward-cluster-elem-list (cluster-element-list/reorder m cluster-elem-list PS PI NS))
                 (backward-cluster-elem-list (cluster-element-list/reorder m copy-of-cluster-elem-list NS PI PS)))
            (make-instance <sal-bdd-conj-cluster>
                           :fsm fsm
                           :forward-cluster-element-list forward-cluster-elem-list
                           :backward-cluster-element-list backward-cluster-elem-list))))))))

(define (sal-bdd-cluster-list->sal-bdd-disj-cluster cluster-list)
  (let* ((fsm (slot-value (car cluster-list) :fsm))
         (m (sal-bdd-fsm/manager fsm)))
    (multiple-value-bind
        (bdd-list cluster-list)
        (cluster-list->bdd-list-cluster-list cluster-list <sal-bdd-disj-cluster>)
      (let* ((pre-compressed-bdd-list (bdd-or/affinity-cluster bdd-list *sal-bdd-max-cluster-size*)) ;; group bdds
             (compressed-bdd-list (filter (lambda (bdd)
                                            (not (bdd/false? bdd))) ;; remove the true BDDs
                                          pre-compressed-bdd-list)))
        (cond 
         ((and (null? compressed-bdd-list)
               (null? cluster-list))
          (bdd->sal-bdd-monolithic-cluster fsm (bdd/false m)))
         ((and (= (length compressed-bdd-list) 1)
               (null? cluster-list))
          (bdd->sal-bdd-monolithic-cluster fsm (car compressed-bdd-list)))
         ((and (null? compressed-bdd-list)
               (= (length cluster-list) 1))
          (car cluster-list))
         (else
          (let ((result-cluster-list (append (bdd-list->sal-bdd-monolithic-cluster-list fsm compressed-bdd-list)
                                             cluster-list)))
            (make-instance <sal-bdd-disj-cluster>
                           :fsm fsm
                           :cluster-list result-cluster-list))))))))

(define-generic (make-bdd-let-cluster cluster let-var-indices bdd-defs))

(define-method (make-bdd-let-cluster (cluster <sal-bdd-cluster>) (let-var-indices <primitive>) (bdd-defs <primitive>))
  (let* ((fsm (slot-value cluster :fsm))
         (bdd-def-cluster-list (bdd-list->sal-bdd-monolithic-cluster-list fsm bdd-defs))
         (let-cube (indices-list->cube (sal-bdd-fsm/manager fsm) let-var-indices))
         (conj-cluster (sal-bdd-cluster-list->sal-bdd-conj-cluster (cons cluster bdd-def-cluster-list))))
    (make-instance <sal-bdd-let-cluster>
                   :fsm fsm
                   :nested-cluster conj-cluster
                   :let-var-cube let-cube
                   :num-let-variables (length let-var-indices))))

(define-method (make-bdd-let-cluster (cluster <sal-bdd-conj-cluster>) (let-var-indices <primitive>) (bdd-defs <primitive>))
  (let* ((fsm (slot-value cluster :fsm))
         (bdd-def-cluster-list (bdd-list->sal-bdd-monolithic-cluster-list fsm bdd-defs))
         (let-cube (indices-list->cube (sal-bdd-fsm/manager fsm) let-var-indices))
         (cluster-list (sal-bdd-cluster->cluster-list cluster))
         (conj-cluster (sal-bdd-cluster-list->sal-bdd-conj-cluster (append cluster-list bdd-def-cluster-list))))
    (make-instance <sal-bdd-let-cluster>
                   :fsm fsm
                   :nested-cluster conj-cluster
                   :let-var-cube let-cube
                   :num-let-variables (length let-var-indices))))
    
(define (sal-expr->cluster ast env fsm)
  (let* ((let-var-indices (make-queue))
         (bdd-defs (make-queue))
         (manager (sal-bdd-fsm/manager fsm))
         (new-let-var-proc (lambda (big-bdd)
                             (let* ((new-curr-var (bdd/new-var manager))
                                    (new-bdd-def (bdd/iff new-curr-var big-bdd)))
                               (queue/insert! let-var-indices (bdd/var new-curr-var))
                               (queue/insert! bdd-defs new-bdd-def)
                               new-curr-var))))
    (set-slot-value! fsm :new-let-var-proc new-let-var-proc)
    (unwind-protect
     (let ((cluster (sal-expr->cluster-core ast env fsm))
           (let-var-indices (queue->list let-var-indices))
           (bdd-defs (queue->list bdd-defs)))
       (if (null? let-var-indices)
         cluster
         (make-bdd-let-cluster cluster let-var-indices bdd-defs)))
     (set-slot-value! fsm :new-let-var-proc #f))))
                          
(define-generic (sal-expr->cluster-core ast env fsm))

(define-method (sal-expr->cluster-core (ast <sal-ast>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (bdd->sal-bdd-monolithic-cluster fsm (sal-expr->bdd-core ast env fsm)))

(define-method (sal-expr->cluster-core (ast <sal-and>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (let ((cluster-list (map (cut sal-expr->cluster-core <> env fsm) (sal-application/argument-list ast))))
    (sal-bdd-cluster-list->sal-bdd-conj-cluster cluster-list)))
    
(define-method (sal-expr->cluster-core (ast <sal-or>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (let ((cluster-list (map (cut sal-expr->cluster-core <> env fsm) (sal-application/argument-list ast))))
    (sal-bdd-cluster-list->sal-bdd-disj-cluster cluster-list)))

(define-method (sal-expr->cluster-core (ast <sal-let-expr>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (sal-let-expr->bdd-core ast env fsm sal-expr->cluster-core))

(define (make-let-bdd-var-when-big-bdd fsm bdd)
  (if (and *sal-create-let-bdd-variables?* 
           (slot-value fsm :new-let-var-proc)
           (> (bdd/size bdd) *sal-bdd-max-cluster-size*))
    ((slot-value fsm :new-let-var-proc) bdd)
    bdd))

(define-method (sal-expr->bdd-core :around (ast <sal-ast>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (let ((result (call-next-method)))
    (make-let-bdd-var-when-big-bdd fsm result)))

(define (sal-expr-list->bounded-bdd expr-list env fsm bdd-proc bdd-limit-proc)
  (if (and *sal-create-let-bdd-variables?*
           (slot-value fsm :new-let-var-proc))
    (fold-left (lambda (result child-expr)
                 (let* ((bdd (sal-expr->bdd-core child-expr env fsm))
                        (new-result (bdd-limit-proc result bdd *sal-bdd-max-cluster-size*)))
                   (if new-result
                     (make-let-bdd-var-when-big-bdd fsm new-result)
                     (let* ((new-var ((slot-value fsm :new-let-var-proc) result))
                            (new-result (bdd-proc new-var bdd)))
                       (make-let-bdd-var-when-big-bdd fsm new-result)))))
               (sal-expr->bdd-core (car expr-list) env fsm)
               (cdr expr-list))
    (fold-left (lambda (result child-expr)
                 (bdd-proc result (sal-expr->bdd-core child-expr env fsm)))
               (sal-expr->bdd-core (car expr-list) env fsm)
               (cdr expr-list))))

(define-method (sal-expr->bdd-core (ast <sal-and>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (sal-expr-list->bounded-bdd (sal-application/argument-list ast) env fsm bdd/and bdd/and-limit))
    
(define-method (sal-expr->bdd-core (ast <sal-or>) (env <primitive>) (fsm <sal-bdd-fsm>))
  (sal-expr-list->bounded-bdd (sal-application/argument-list ast) env fsm bdd/or bdd/or-limit))
