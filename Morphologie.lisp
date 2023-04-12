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

        ("Analysis" nil nil (morph::ptrn-recogn
                             morph::ptrn-recogn-ctrl
                             morph::ptrn-reson
                             morph::ptrn-smooth
                             morph::ins-ptrn
                             morph::min-flex-max
                             morph::direct-analysis) nil)
        ("Structure" nil nil (morph::structure-1
                              morph::RMA-1
                              morph::rma-1-scores
                              morph::structure-2) nil)
        ("Reconstitute" nil nil (morph::1-0-1-reconst
                                 morph::reconstitute) nil)
        ("Distance" nil nil (morph::euclidian-d
                             morph::distance
                             morph::ldl-distance
                             morph::multi-distance
                             morph::Prim-tree
                             morph::draw-tree) nil)
        ("Classification" nil nil (morph::s-class
                                   morph::class-1
                                   morph::aver-class
                                   morph::quantize-1
                                   morph::class-center
                                   morph::matrix-center
                                   morph::meta-class1
                                   morph::norm-class
                                   morph::P-class
                                   morph::res-class
                                   morph::E-shannon
                                   morph::e-test) nil)
        
        ("Utilities" nil nil (morph::delta
                                morph::smooth
                                morph::midiseq->alpha
                                morph::rep-by-flag
                                morph::mc->alpha
                                morph::num->alpha
                                morph::str->symb
                                morph::concatstrings
                                morph::l-matrix) nil)
        ))

(om::fill-library *subpackages-morpho*)

(om::set-lib-release 1.2)

(print "
;;            Morphologie
;;            by  Jacopo Baboni Schilingi & Frederic Voisin  © IRCAM 1997
;;	      OM version: 1998	
")


