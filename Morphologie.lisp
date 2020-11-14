;;
;;
;;            Morphologie
;;            by  Jacopo Baboni Schilingi & Frederic Voisin  © IRCAM 1997
;;	          OM version: 1998	
;;
;;

(in-package :om)

(compile&load (om::om-relative-path '("sources") "morphologie"))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *subpackages-morpho* nil)
(setf *subpackages-morpho*
      '(
        
  ;   ("Data" nil nil (data-base  select-data) nil)

        ("Analysis" nil nil (ptrn-recogn
                             ptrn-recogn-ctrl
                             ptrn-reson
                             ptrn-smooth
                             ins-ptrn
                             min-flex-max
                             direct-analysis) nil)
        ("Structure" nil nil (structure-1
                              RMA-1
                              rma-1-scores
                              structure-2) nil)
        ("Reconstitute" nil nil (1-0-1-reconst
                                 reconstitute) nil)
        ("Distance" nil nil (euclidian-d
                             distance
                             ldl-distance
                             multi-distance
                             Prim-tree
                             draw-tree) nil)
        ("Classification" nil nil (s-class
                                   class-1
                                   aver-class
                                   quantize-1
                                   class-center
                                   matrix-center
                                   meta-class1
                                   norm-class
                                   P-class
                                   res-class
                                   E-shannon
                                   e-test) nil)
        
        ("Utilities" nil nil (delta
                                smooth
                                midiseq->alpha
                                rep-by-flag
                                mc->alpha
                                num->alpha
                                str->symb
                                concatstrings
                                l-matrix) nil)
        ))

(om::fill-library *subpackages-morpho*)

(om::set-lib-release 1.1)

(print "
;;            Morphologie
;;            by  Jacopo Baboni Schilingi & Frederic Voisin  © IRCAM 1997
;;	      OM version: 1998	
")


