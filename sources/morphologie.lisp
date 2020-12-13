;*************************************************************************************************************
;*                                                                                                           *
;*                   Jacopo Baboni Schilingi & Frederic VOISIN                                               *
;*                                                                                                           *
;*                          IRCAM, Paris, november 1998.                                                     *
;*                                                                                                           *
;* Fonctions d'analyse, reconnaissance de pattern et classification morphologiques des profiles geometriques *
;* Analysis fonctions, pattern recognition and morphological classification of geometric profiles            *
;*                                                                                                           *
;*************************************************************************************************************
 
(defpackage "MORPH"
  (:use "COMMON-LISP" "CL-USER"))

(in-package "MORPH")

;quelques modules -provisoire-

(defun less-deep-mapcar (fun  list? &rest args)
  "Applies <fun> to <list?> <args> if <list?> is a one-level list .
   Mapcars <fun> to <list?> <args> if <list?> is a multi-level list. "
  (cond
    ((null list?) ())
    ((atom (car list?)) (apply fun list? args))
    ((atom (car (car list?))) 
     (cons (apply fun (car list?)  args ) (apply #'less-deep-mapcar fun (cdr list?) args)))
    (t (cons (apply #'less-deep-mapcar fun  (car list?) args)
             (apply #'less-deep-mapcar fun  (cdr list?) args)))))

(defun deep-mapcar (fun fun1 list? &rest args)
  "Mapcars <fun> or applies <fun1> to <list?> <args> whether <list?> is a list or not."
   (cond
    ((null list?) ())
    ((not (consp list?)) (apply fun1 list? args))
    (t (cons (apply #'deep-mapcar fun fun1 (car list?) args)
             (apply #'deep-mapcar fun fun1 (cdr list?) args)))))

(defun the-min (x) (apply 'min x))
(defun the-max (x) (apply 'max x))

(defmethod g-min ((list list))
  (less-deep-mapcar #'the-min (om::list! list)))

(defmethod g-max ((list list))
  (less-deep-mapcar #'the-max (om::list! list)))



(defmethod l-nth ((list list) l-nth)
  (deep-mapcar 'l-nth 'nth l-nth list))

(defmethod posn-match ((list list) l-nth )
  (deep-mapcar 'l-nth 'nth l-nth (om::list! list)))

(defmethod permut-circ ((list list) &optional (nth 1))
  (permut-circn (copy-list list) nth))

(defmethod permut-circn ((list list) &optional (nth 1))
  (when list
    (let ((length (length list)) n-1thcdr)
      (setq nth (mod nth length))
      (if (zerop nth) list
          (prog1
            (cdr (nconc (setq n-1thcdr (nthcdr (1- nth) list)) list))
            (rplacd n-1thcdr ()))))))



;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Ridondanza pattern: ripetizioni;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! primo-passo ((lista list) (n integer))
  
  :initvals '(nil 1) 
  :doc "prende n elementi di una lista"
  :icon 128
  
  (let ((f nil))
    (dotimes (x n)
      (push (nth x lista) f))
    (nreverse f)))

 
;
;
;--------------------------------------
;
;
(om::defmethod! scom ((lista1 list) &optional (n nil))
  
  :initvals '(nil nil) 
  :doc "Scompone la lista1 in funzione delle lunghezze indicate nella n" 
  :icon 128
  
  (let ((ris nil))
    (cond ((equal n 'nil)
           (loop for x from 2 to (floor (/ (length lista1) 2))
                 do
                 (dotimes (y (+ (- (length lista1) x) 1))
                   (push (primo-passo
                          (nthcdr y lista1)
                          x)
                         ris))))
          ((or (integerp n) (and (listp n) (= 1 (length n))))
           (dotimes (y (+ (- (length lista1) n) 1))
             (push (primo-passo
                    (nthcdr y lista1)
                    n)
                   ris)))
          ((and (listp n) (= 2 (length n)))
           (loop for x from (car n) to (cadr n)
                 do
                 (dotimes (y (+ (- (length lista1) x) 1))
                   (push (primo-passo
                          (nthcdr y lista1)
                          x)
                         ris))))
          (t (format t " Invalid optional input") (abort)))
    (reverse ris)))


;
;
;--------------------------------------
;
;
(om::defmethod! pattern-ridond ((lista list) &optional (n nil))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Restituisce tutte le ripetizioni di tutti i sotto-pattern in
             cui può essere scomposta la sequenza in lista."
  
  (let ((ris nil)
        (x (scom lista n))
        y)
    (loop while x
          do (when (find  (setf y (pop x)) x :test 'equal)
               (push y ris))) (reverse ris)
    ))

;
;
;--------------------------------------
;
;
(om::defmethod! ptrn-recogn ((list list))
  :initvals '('(1 2 3 1 2 3 1 2 1 2))
  :icon 128
  :doc "restituisce..."
  
  (let* ((ris nil)
         (ros nil)
         (calcolo (pattern-ridond list))
         (calcoletto (remove-duplicates calcolo :from-end '1 :test 'equalp))
         (calcolaccio (dolist (y calcoletto (nreverse ros))
                        (push (1+ (count y calcolo :test 'equalp)) ros))))
    
    (dotimes (x (length calcoletto) (nreverse ris))
      (push (om::x-append (nth x calcoletto) (list "times" (nth x calcolaccio))) ris))))
;
;
;
;;;;;;;;;;;;;;;;;Ridondanza di rapporti: ripetizioni controllate;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! rispero ((lista list) (n integer)) 
  :initvals '((1 2) 0)
  :icon 128
  :doc "E' come spero solo che divide la 
            lista in base al valore messo in n"
  
  (scom lista n))
;
;
;--------------------------------------
;
;
(om::defmethod! risperiamo ((lista list) (n integer)) 
  
  :initvals '(nil 0)
  :icon 128
  :doc "E' molto simile a speriamo : trova i pattern di n lunghezza
             all'interno della lista"
  
  (let ((ris nil))
    (dolist (x (rispero lista n) (nreverse ris))
      (if (equalp
           (om::included? (list nil)
                          x) nil)
        (push x ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! ptrn-ridond-ctrl-prov ((lista list) (n integer))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Restituisce tutti i sotto-pattern che compaiono almeno
             due volte (ridondanza) e le cui length sono decise da
             noi in N."
  
  (let ((ris nil)
        (x (risperiamo lista n))
        y)
    
    (loop while x do 
      (if (find (setf y (pop x)) x :test 'equal)
        (push y ris))) (nreverse ris)))
;
;
;--------------------------------------
;
;
(om::defmethod! ptrn-recogn-ctrl ((list list) (n integer))
  
  :initvals '('(1 2 3 1 2 3 1 2 1 2) 3)
  :icon 128
  :doc "Restituisce tutti i sotto-pattern che compaiono almeno
             due volte (ridondanza) e le cui length sono decise da
             noi in N ed in più quante volte è ripetuto un pattern"
  
  (let* ((ris nil)
         (ros nil)
         (calcolo (ptrn-ridond-ctrl-prov list n))
         (calcoletto (remove-duplicates calcolo :from-end '1 :test 'equalp))
         (calcolaccio (dolist (y calcoletto (nreverse ros))
                        (push (count y calcolo :test 'equalp) ros))))
    
    (dotimes (x (length calcoletto) (nreverse ris))
      (push (om::x-append (nth x calcoletto) (list "times" (1+ (nth x calcolaccio)) )) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! ptrn-reson ((list list) (windw integer)
                            &optional (which nil)) 
  
  :initvals '('(a b c a b c a b a b a a a a b a b a a a a a b b b b b b a a) 5 '(a b c))
  :icon 128
  :doc "restituisce la lista delle note di una sequenza
             con il loro indice di presenza e con il guppo 
             di appartenenza della lista. Il gruppo è una 
             sotto lista che si stabilisce in windw."
  
  (let* ((which (om::list! which))
         (ris nil)
         (ros nil)
         (rus nil)
         (lungo (floor (/ (length list) windw)))
         (gruppi (om::group-list list (om::create-list lungo windw) 1))
        (resto (- (length list) (length (om::flat gruppi))))
        (quello (om::last-n list resto))
        (calcolaccio (if (> resto 1)
                        (dolist (y (remove-duplicates quello) (nreverse rus))
                          (push (list "last" "group" "-->"
                                      y
                                      (count y quello :test 'equalp)
                                      "length-group" resto)
                                rus))))
        
         
         (calcolo (dotimes (y (length gruppi) 
                               
                               (om::x-append (nreverse ris) calcolaccio)
                              )
                    (dolist (x (remove-duplicates (nth y gruppi)))
                      (push (list "group" y "-->"
                                  x
                                  (count x (nth y gruppi)))                         
                            ris))))
         

         
         (calcolone (dolist (r calcolo (nreverse ros))
                      (when which 
                        (dolist (k which)
                          (if (find k (list (fifth r)) :test 'equalp)
                            (push r ros)))))))

    
    (if which calcolone calcolo)))
;
;
;--------------------------------------
;
;
(om::defmethod! ptrn-smooth ((list list)) 
  
  :initvals '((a b b b c c c d a a b b c c d e d d d b b ))
  :icon 128
  :doc  "It returns the list list without local repetitions.
             For example : list equal to (a a b c a b b c d c c)
             it reurns (a b c a b c d c))"
  
  (let ((L()))
    (loop for x from 0 to (1- (length list)) do
          (when (not (equal (nth (+ x 1) list) (nth x list)))
            (push (nth x list) L)))
    (reverse L)))




;**************** distances suite **************

(om::defmethod! ldl-distance ((l-seq list) (change number) (ins/sup number) (inex number) 
                              (scale string) (result string))
  :initvals '(((a b c) (a b b) (a b c)) 1.0 1.0 0.0 "abs" "short")
  :menuins '( (4 (("relative"  "rel")
                 ("absolute"  "abs")))
             (5 (("short"  "short")
                 ("extended"  "ext")
                 ("save"  "save"))))
  :icon 128
  :doc "Estimates the distances between lists of symbols.
INPUT
l-seq : list of lists of symbols;
change : cost when changing a symbol;
ins/sup : cost when inserting or deleting a symbol;
inex : added cost when the edition is made on a symbol not actual in the other list;
scale : scaling of the distance (ABSOLUTE / RELATIVE), default : ABSOLUTE;
result : output mode - list of list (short), easy-to-read mode (extended) or save to file (save).
OUTPUT
A matrix of distances"
  (when (not (= (length l-seq) (length (remove-duplicates (copy-list l-seq) :test #'equal))))
    (setf l-seq (remove-duplicates l-seq :test #'equal))
    (format t "Warning: removing duplicates in input.~%"))
  (let ((r ()))
    (if (equalp scale "rel")
      (setf scale 1) (setf scale 2))
    (cond ((equalp result "short")
           (setf result 1))
          ((equalp result "ext")
           (setf result 2))
          ((equalp result "save")
           (setf result 2))
          (t (print "Error result menu doesn't exist.")
             (abort)))
    (cond ((= inex 0.0)
           (dotimes (l1 (length l-seq)
                        (cond ((eq result 1) (reverse r))
                              ((eq result 2) (see-ldist (reverse r)))
                              ((eq result 3) (save-ldist (reverse r)))
                              (t r)))
             (dotimes (l2 (length l-seq))
               (cond ((> (- l2 l1) 0)
                      (push (list (nth l1 l-seq) (nth l2 l-seq) (dist-1 (nth l1 l-seq) (nth l2 l-seq)
                                                                        change ins/sup scale))
                            r))))))
          (t
           (dotimes (l1 (length l-seq)
                        (cond ((eq result 1) (reverse r))
                              ((eq result 2) (see-ldist (reverse r)))
                              ((eq result 3) (save-ldist (reverse r)))
                              (t r)))
             (dotimes (l2 (length l-seq))
               (cond ((> (- l2 l1) 0)
                      (push (list (nth l1 l-seq) (nth l2 l-seq) (dist-2 (cdr (nth l1 l-seq)) (cdr (nth l2 l-seq))
                                                                        change ins/sup inex scale))
                            r)))))))))

(defun see-ldist (d-seq?)
"Prints in easy-to-read way the distances from dist-list function"
  (dolist (s d-seq?)
    (format t "~%distance ~S / ~S  --> ~,3F ~%" (car s) (cadr s) (caddr s))))

(defun save-ldist (d-seq?)
"Writes a file with all distances computed by dist-list function"
  (let ((out-file (pathname (om::om-choose-new-file-dialog :prompt "distances file name :" :button-string "save"))))
  (with-open-file (out-stream out-file :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (format t "~&---> Writing distances in : ~S ..." out-file)
    (dolist (s d-seq?)
      (format out-stream "~S~T~S~T~,5F~%" (car s) (cadr s) (caddr s)))
    (format t "~%---> DONE !"))))

;
;******************  contrastive analysis ***************************
;
(defparameter **alpha** '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\°))
(defparameter **num** '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

;++++++++++++++++  CONVERSIONS

(defun ldl-string (seq)
"Converts list or list of lists of integers into one string of characters."
  (if (equal 'nil (member 't (mapcar #'listp seq)))
    (to-alpha seq)
    (let ((r (make-string (+ (* 2 (length (remove-if #'listp seq)))
                             (apply #'+ (mapcar #'length (remove-if #'integerp seq)))
                             (1- (length (remove-if #'integerp seq))))
                          :initial-element #\Space))
          (x 0))
      (dotimes (s (length seq) r)
        (if (integerp (nth s seq))
          (setf (elt r (+ x (* 2 s))) (elt (to-alpha (nth s seq)) 0))
          (dotimes (n (length (nth s seq)) (setf x (+ x (1- (length (nth s seq))))))
            (setf (elt r (+ (* 2 s) n x)) (elt (to-alpha (nth n (nth s seq))) 0))))))))

(defun string-to-list (string)
"Converts a string into a list of characters."
  (let ((lstring ()))
    (dotimes (n (length string) (nreverse lstring))
        (push (elt string n) lstring))))

(defun list-char-score (lcs)
  (setf (car lcs) (list (make-string 1 :initial-element (car lcs)) (cadr lcs))))

(defun posofspaces (string)
  (let ((pos ()))
    (dotimes (n (length string) (nreverse (append (list (length string)) pos)))
      (when (equal (elt string n) #\Space)
        (push n pos)))))

(defun pos-to-lengths (positions)
  (let ((lengths ()))
    (dotimes (n (1- (length positions))
                (append (list (car positions)) (nreverse lengths)))
      (push (- (nth (1+ n) positions) (nth n positions) 1) lengths))))

(defun seqstring-to-liststrings (string)
"Converts a string into a list of strings - words - separated by space character."
  (let ((listofstrings ()) (lengths (pos-to-lengths (posofspaces string)))
        (pos (append (list 0) (mapcar #'(lambda (x) (1+ x)) (butlast (posofspaces string))))))
    (dotimes (l (length lengths) (nreverse listofstrings))
      (let ((str (make-string (nth l lengths) :initial-element #\Space)))
       (dotimes (n (nth l lengths) (push str listofstrings))
         (setf (elt str n) (elt string (+ n (nth l pos)))))))))

(defun readst (st)
  (let ((r ()))
    (dotimes (n (length st) (nreverse r))
      (when (not (eql #\Space (elt st n)))
        (push (read-from-string (string (elt st n))) r)))))

(defun string-to-symbol (string)
"Converts string or list of strings or list of list of strings into list (list of list) of symbols."
  (if (listp string)
    (cond ((equal 'nil (member 'nil (mapcar #'stringp string)))
           (mapcar #'(lambda (st) (read-from-string st :start 0 :end (length st)))
                   string))
          ((equal 'nil (member 'nil (mapcar #'listp string)))
           (let ((r ()))
             (dolist (st string (nreverse r))
               (push (mapcar #'(lambda (s) (read-from-string s :start 0 :end (length s)))
                               st) r)))))
  (when (stringp string)
    (readst string))))

(om::defmethod! str->symb ((strings list))
  :initvals '(nil)
  :icon 128
  :doc "Converts string or list of strings or list of list of strings into list (list of list) of symbols.
!! : please replace double quotes by simple quotes before evalution."
  (mapcar #'string-to-symbol strings))

(om::defmethod! str->symb ((strings string))
  :initvals '(nil)
  :icon 128
  :doc "Converts string or list of strings or list of list of strings into list (list of list) of symbols.
!! : please replace double quotes by simple quotes before evalution."
  (string-to-symbol strings))

(om::defmethod! num->alpha ((list list))
  :initvals '(nil)
  :icon 128
  :doc "converts list of lists and/or integers to symbols"
  (string-to-symbol (seqstring-to-liststrings (ldl-string list))))

(defmethod midicents-to-name ( (x integer) &optional (approx 0) )
  "Converts a midic number to a CMN name approx values are .5
.25 .125 "
  (let* ((approx-cents (funcall #'(lambda ( a b ) (* (round (/
                                                             a b ) ) b ) )
                                x
                                (cond 
                                 ((= approx 0) 100)
                                 ((= approx 1) 50 )
                                 ((= approx 2 ) 25 )
                                 (t 100)
                                 )
                                )
                       )
         (octave-base-note (* (floor approx-cents 1200) 1200 )
                           )
         (note-number (round (/ (- approx-cents
                                   octave-base-note) 25 )) )
         (octave-number (- (floor (round (/ x 100)) 12) 1) ))
    (intern (format nil "~D~D" 
                    (nth note-number '("C" "CN+" "CS--"
                                       "CS-" "CS" "CS+" "CS++" "DN-" 
                                       "D" "DN+" "DS--"
                                       "DS-" "ES" "DS+" "DS++" "EN-"
                                       "E" "EN+" "ES--"
                                       "ES-" 
                                       "F" "FN+" "FS--"
                                       "FS-" "FS" "FS+" "FS++" "GN-"
                                       "G" "GN+" "GS--"
                                       "GS-" "GS" "GS+" "GS++" "AN-"
                                       "A" "AN+" "AS--"
                                       "AS-" "BF" "AS+" "AS++" "BN-"
                                       "B" "BN+" "BS--"
                                       "BS-" ) )
                    octave-number )
            "MORPH")))

(defun mc-to-name (midicents &optional (approx 0))
  "Main fonction."
  (if (listp midicents)
    (let ((r ()))
      (dolist (m midicents (nreverse r))
        (if (listp m)
          (push (concatstrings (mapcar #'(lambda (x) (symbol-name (midicents-to-name x approx))) m)) r)
          (push (symbol-name (midicents-to-name m approx)) r))))
    (symbol-name (midicents-to-name midicents approx))))

(defun mc->alpha1 (midicents approx)
  (mc-to-name midicents approx))

(om::defmethod! mc->alpha ((midicents list) (approx integer)) 
  :initvals '(nil)
  :icon 128
  (mc->alpha midicents approx))

(om::defmethod! concatstrings ((lofstrings list)) 
  :initvals '(nil)
  :icon 128
  :doc  "Concantenates list of strings into one string."
  (let ((concatenated (make-string (apply #'+ (mapcar #'length lofstrings)) :initial-element #\Space))
        (n 0))
    (dolist (k lofstrings concatenated)
      (dotimes (i (length k))
        (setf (elt concatenated n) (elt k i))
        (setf n (1+ n))))))


(defun midiseq->alpha1 (midiseq approx)
"Converts midicents values into symboles."
  (string-to-symbol (mc-to-name midiseq approx)))


(om::defmethod! midiseq->alpha ((midiseq t) (approx integer))
  :doc "Converts midicents values into symboles."
  :initvals '(nil 0)
  :icon 128
  (midiseq->alpha1 midiseq approx))


#|
(defun chordseq->alpha (chordseq &optional (approx 0))
"Converts chordseq OM object into a list of symboles."
(print "a faire"))

 ; (string-to-symbol (mc-to-name (c-get-note-slots:get-note-slots chordseq 'midic) approx)))

|#
;++++++++++++++++ END of CONVERSIONS

(defun seg/contrast (list)
  (let ((c (contraste list))
        (pos ())
        (b1 ())
        (b2 ())
        (res ()))
    (setf pos (cdr c))
    (dolist (l pos)
      (setf b1 nil)
      (setf b1
            (append  (list (car l)) (n-n-1l l)))
      (setf b2 (append b1 (list (- (length list) (car (last l))))))
      (push (om::group-list list b2 1) res))
    (list (car c) (reverse res))))

;touched by aaa rem-dups
(defun segnum1 (seq)
  (let ((seq1 (remove-duplicates  seq :test 'equal))
        (seq2 ())
        (res1 ())
        (res2 ()))
    (dotimes (n (length seq1))
      (push (list (nth n seq1) (+ n 1)) res1))
    (setf res1 (om::flat-once (reverse res1)))
    (dotimes (n (length seq))
      (setf res2 (member (nth n seq) res1 :test 'equal))
      (push (list res2 (second res2)) seq2))
    (om::mat-trans (reverse seq2))))

(defun group (list)
  (let ((seqs (second list))
        (a ())
        (b ())
        (c ())
        (res ())
        )
    (dolist (s seqs)
      (setf c (segnum1 s))
      (setf a (remove-duplicates (om::flat-once (car c))))
      (setf b (cdr c))
      (setf a (om::mat-trans (reverse (om::list-modulo a 2))))
      (push (list a (car b)) res))
    (list (car list) (reverse res))))

(defun form (segs)
  (let ((res ()))
    (setf segs (om::flat-once (cdr segs)))
    (dolist (s segs (reverse res))
      (push (cadr s) res))))

(defun take-date ()
  (multiple-value-bind (s mn h d m y dw ds tz)  (get-decoded-time)
    (cond ((= 0 dw) (setf dw 'monday))
          ((= 1 dw) (setf dw 'tuesday))
          ((= 2 dw) (setf dw 'wednesday))
          ((= 3 dw) (setf dw 'thirday))
          ((= 4 dw) (setf dw 'friday))
          ((= 5 dw) (setf dw 'saturday))
          ((= 6 dw) (setf dw 'sunday)))
    (list dw d m y h mn s ds tz)))

(defun view-str-1 (seq from-struct-1 groups alpha? stream date run-time)
    (format stream "****************************************~%")
    (format stream "~%   Run of Marker Analysis on ~S ~S ~S ~S  at ~S h ~S mn,"
            (nth 0 date) (nth 1 date) (nth 2 date) (nth 3 date) (nth 4 date) (nth 5 date))
    (format stream "~%with the following sequence :~%~%")
    (format stream "~S ~%" seq)
    (dotimes (n (length (car from-struct-1))
                (format stream "~%computation time : ~S seconds~%    End of marker analysis~%" run-time))
      (format stream "~%~S - Structure for critere : ~S~%~%" (+ n 1) (nth n (car from-struct-1)))
      ;(print from-struct-1)
      (format stream "~S " (nth n (cadr from-struct-1)))
      (format stream "~%~%")
      (format stream "    With the following patterns :~%")
      (dotimes (o (length (car (nth n (cadr groups)))))
        (format stream "~S : ~S~%"
                (if (eq alpha? 1)
                  (to-alpha (car (nth o (car (nth n (cadr groups))))))
                  (car (nth o (car (nth n (cadr groups))))))
                (cadr (nth o (car (nth n (cadr groups)))))))))

(om::defmethod! structure-1 ((seq list) &optional
                             (alpha? 1)
                             (lisse? 1)
                             (result 1)) 
  :menuins '( (1 (("alpha"  1)
                 ("num"  0)))
             (2 (("yes"  1)
                 ("no"  0)))
             (3 (("struct"  3)
                 ("short"  0)
                 ("extend"  1)
                 ("save"  2))))
  :initvals '(nil 1 1 1)
  :icon 128
  :doc  "Donne toutes les structures possibles d'une sequence de nombres ou de symboles 
selon une segmentation contrastive.

INPUT
seq : sequence de symboles ou nombres (liste);
alpha? : resultat en mode alphabetique ou numerique (YES NO), optional;
lisse? : suppression des elts concomitents identiques dans seq (YES, NO), optional.
result : short = liste des criteres de segmentation et leur segmentation respective;
         exten = analyse detaillee;
         save  = analyse detaillee ecrite en un fichier texte.

OUTPUT
en mode short, pour le traitement de l'analyse, liste de liste selon le format :

((criteres de segmentation)
(forme selon critere)...)"
  
  (let ((seg ())
        (res ())
        (date (take-date))
        (time-start (get-internal-real-time))
        (run-time 0)
        out-file)
    (when (eq result 2)
      (setf out-file (om::om-choose-new-file-dialog
                      :prompt "Structure-1 Mark Analysis"
                      )))
    (if (eq lisse? 1)
      (setf seg (group (seg/contrast (lisse seq))))
      (setf seg (group (seg/contrast seq))))
    (if (eq alpha? 1)
      (setf res (list (car seg) (to-alpha (form seg))))
      (setf res (list (car seg) (form seg))))
    (setf run-time (float (/ (- (get-internal-real-time) time-start)
                             internal-time-units-per-second)))
    (cond ((eq result 1)
           (view-str-1 seq res seg alpha? 't date run-time))
          ((eq result 2)
           (format t "Writing marker analysis in file : ~S...~%" out-file)
           (with-open-file (out-st out-file                          
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (view-str-1 seq res seg alpha? out-st date run-time))
           ;(set-mac-file-creator out-file 'ttxt)
           (format t "DONE~%"))
          ((eq result 0)
           (format t "~%   Marker analysis (computation time = ~S s.) :~%" run-time)
           (if (eq alpha? 0)
             (list (car seg) (mapcar 'car (cadr seg)) (mapcar 'cadr (cadr seg)))
             (to-a-special (list (car seg) (mapcar 'car (cadr seg)) (last res)))))
          ((eq result 3)
           (car (last res))))))

(defun take-structures (analysis)
  (setf analysis (car (last analysis)))
  (mapcar 'cadr analysis))

(defun take-criteria (analysis)
  (car analysis))

(defun take-patterns (analysis)
  (let ((patts (mapcar 'car (cadr analysis)))
         (patterns ()))
     (dolist (p patts (nreverse patterns))
       (push (mapcar 'cadr p) patterns))))

(defun analyse-contrastive (s lisse?)
  (let (analysis)
    (if (= 0 lisse?)
      (setf analysis (group (seg/contrast s)))
      (setf analysis (group (seg/contrast (lisse s)))))
    (values (take-structures analysis)
            (take-criteria analysis)
            (take-patterns analysis))))

       
(defun convert-to-alpha (list-of-numbers)
  (let ((string (make-string (1- (* 2 (length list-of-numbers)))
                             :initial-element #\ )))
    (dotimes (n (length list-of-numbers) string)
      (setf (char string (* 2 n)) (nth (1- (nth n list-of-numbers)) **alpha**)))))

(defun to-alpha (list-of-nb)
"Converts in alpha integer, list of integers, or list of lists of integers.
1 -> A, 2 -> B etc.
0 (zero) will converted with °."
(cond ((numberp list-of-nb)
       (when (= 0 list-of-nb) (setf list-of-nb 27))
       (make-string 1 :initial-element (nth (1- list-of-nb) **alpha**)))
      (t
  (cond ((equal 'nil (member 'nil (mapcar #'symbolp (om::flat list-of-nb))))
         list-of-nb)
        ((equal 'nil (member 'nil (mapcar 'numberp list-of-nb)))
         (setf list-of-nb (substitute 27 0 list-of-nb))
         (convert-to-alpha list-of-nb))
        ((equal 'nil (member 'nil (mapcar 'listp list-of-nb)))
         (mapcar #'(lambda (x) (substitute 27 0 x)) list-of-nb)
         (mapcar #'convert-to-alpha list-of-nb))
        
        (t
         (format t
                 "~%to-alpha : Error when converting integers to alpha :~%Accepts only, integers or list of int, or list of list of int.~%"))))))

(defun to-alphanum (list-of-nb)
"Converts in alpha integer, list of integers, or list of lists of integers."
;ajouter ici test sur strings
(if (numberp list-of-nb)
  (make-string 1 :initial-element (nth list-of-nb **num**))
  (cond ((eq 'nil (member 'nil (mapcar 'numberp list-of-nb)))
         (convert-to-alphanum list-of-nb))
        ((eq 'nil (member 'nil (mapcar 'listp list-of-nb)))
         (mapcar #'convert-to-alphanum list-of-nb))
        (t
         (format t
                 "~%to-alpha : Error when converting integers to alpha :~%Accepts only, integers or list of int, or list of list of int.~%")))))

(defun convert-to-alphanum (list-of-numbers)
  (let ((string (make-string (1- (* 2 (length list-of-numbers)))
                             :initial-element #\ )))
    (dotimes (n (length list-of-numbers) string)
      (setf (char string (* 2 n)) (nth (nth n list-of-numbers) **num**)))))

(defun to-a-special (list)
  (dotimes (l (length (cadr list)) list)
    (dotimes (n (length (nth l (cadr list))))
      (when (not (numberp (car (nth n (nth l (cadr list))))))
        (format t "~%Error when converting to alpha notation :~%pattern names are already symbols (in place of integers).~%~%")
        (abort))
      (setf (car (nth n (nth l (cadr list))))
            (make-string 1 :initial-element (nth (1- (car (nth n (nth l (cadr list))))) **alpha**))))))

;**************************
(defun take-result-of-ac (seq lisse?)
"Catchs the results of fonction #'analyse-contrastive."
  (multiple-value-bind (structs crits ptrns)  (analyse-contrastive seq lisse?)
    (list structs crits ptrns)))



(defun ac+complete (seq lisse1 prof level lisse2 arbores)
"   Recursive Mark Analysis with short mode results.
seq for sequence of symboles;
lisse1 (0 or 1) for removing or not equal contiguous items on first level on analysis;
prof (integer) is the number of steps of recursivity (forms of upper level);
lisse2 for removing or not equal contiguous items on upper analysis level;
alpha? (0 or 1) for alpha print mode;
out-st is the output stream ('t or file).
!!arg arbores must be an empty list, used for recursive analysis!!"
  (if (= prof 0)
      (values (reverse seq) (reverse arbores))
      (cond ((= level 0)
             (let ((analysis (take-result-of-ac seq lisse1)))
               (push (list (cadr analysis) (caddr analysis)) arbores)
               (ac+complete
                (list (car analysis) seq) lisse1 (1- prof) (1+ level) lisse2 arbores)))
            (t
             (let ((seqr ()) (se '())) 
               (dolist (s (car seq) (push (reverse se) seqr))
                 (let ((analysis (take-result-of-ac s lisse2)))
                   (setf se (append (reverse (car analysis)) se))
                   (push (list (cadr analysis) (caddr analysis)) arbores)))
               (ac+complete
                (append (reverse seqr) seq) lisse1 (1- prof) (1+ level) lisse2 arbores))))))


(defun alp?? (yn x n)
"Converts in alpha mode Up if n>0 and down-case if n=0 strings or num according to yn (1 0) for alpha or num ,
x which can be a integer, or list or list of list, and n which is the analysis level"
(cond ((or (symbolp x) (stringp x))
       x)
      ((listp x)
       (if (equal 'nil
                  (member 'nil (mapcar #'(lambda (z)
                                           (or (symbolp z) (stringp z))) x)))
         x
         (if (= 1 yn)
           (if (= n 0)
             (string-downcase (to-alpha x))
             (to-alpha x))
           (to-alphanum x))))
      (t
       (if (= 1 yn)
         (if (= n 0)
           (string-downcase (to-alpha x))
           (to-alpha x))
         (to-alphanum x)))))

(defun alp?1? (yn x n)
"Converts in alpha mode Up if n>1 and down-case if n=1 strings or num according to yn (1 0) for alpha or num ,
x which can be a integer, or list or list of list, and n which is the analysis level"
(cond ((or (symbolp x) (stringp x))
       x)
      ((listp x)
       (if (equal 'nil
                  (member 'nil (mapcar #'(lambda (z)
                                           (or (symbolp z) (stringp z))) x)))
         x
         (if (= 1 yn)
           (if (= n 1)
             (string-downcase (to-alpha x))
             (to-alpha x))
           (to-alphanum x))))
      (t
       (if (= 1 yn)
         (if (= n 1)
           (string-downcase (to-alpha x))
           (to-alpha x))
         (to-alphanum x)))))

(defun Recursive-Mark-Analyse (seq lisse1 prof lisse2 alpha? out-st)
"   Recursive Mark Analysis with extended results.
seq for sequence of symboles;
lisse1 (0 or 1) for removing or not equal contiguous items on first level on analysis;
prof (integer) is the number of steps of recursivity (forms of upper level);
lisse2 for removing or not equal contiguous items on upper analysis level;
alpha? (0 or 1) for alpha print mode;
out-st is the output stream ('t or file)."

  (multiple-value-bind (st analysis-paths) (ac+complete seq lisse1 prof 0 lisse2 '())
    (let ((list-of-crits (mapcar 'car analysis-paths))
          (list-of-patrns (mapcar 'cadr analysis-paths))
          (structs (cdr st))
          (struct-out (cdr st))
          (crits ())
          (struct-level ())
          (ptrns ())
          (count 0)
          (accum-critere-level 0)
          (c 0) (d 0))
      (loop for n from 0
            until (= 0 (length structs))
            do
            (format out-st "~%~%********** ANALYSIS LEVEL #~S **********~%" (1+ n))
            (setf struct-level (pop structs))
            (setf crits (pop list-of-crits))
            (setf ptrns (pop list-of-patrns))
            (setf accum-critere-level (+ accum-critere-level (length crits)))
            (setf c 0)
            (dotimes (k (length struct-level))
              (setf count (1+ count))
              (cond ((= k 0)
                     (setf d 1)
                     (format out-st "~%~D structure~:P found" (length crits))
                     (if (= n 0)
                       (format out-st ":~%")
                       (format out-st " on structure ~S.~S:~%" n (1+ k))))
                    (t
                     (when (> count accum-critere-level)
                       (setf ptrns (pop list-of-patrns))
                       (setf d (1+ d))
                       (setf crits (pop list-of-crits))
                       (setf accum-critere-level (+ accum-critere-level (length crits)))
                       (setf c 0)
                       (format out-st "~%~%~D structure~:P found" (length crits))
                       (if (= n 0)
                       (format out-st ":~%")
                       (format out-st " on structure ~S.~S :~%" n d)))))
              (format out-st "~%~% mark /~A/      STRUCTURE ~S.~S : ~A~%~D pattern~:P :"
                      (alp?? alpha? (nth c crits) n)
                      (1+ n)
                      (1+ k)
                      (alp?? alpha? (nth k struct-level) n)
                      (length (nth c ptrns)))
              (dotimes (p (length (nth c ptrns)))
                (format out-st "~%  ~A - ~A"
                      (alp?? alpha? (1+ p) n)
                      (alp?1? alpha? (nth p (nth c ptrns)) n)))
              (setf c (1+ c))))
      struct-out)))

(defun print-smoothing (smooth1 smooth2 levels stream)
  (when (and (= smooth1 smooth2) (= 0 smooth1))
      (format stream "with no smoothing.~%"))
  (when (and (and (= smooth1 smooth2) (= 1 smooth1)) (> levels 1))
      (format stream "smoothing on all analysis levels.~%"))
  (when (and (= smooth1 0) (= smooth2 1))
      (format stream "smoothing since analysis level 2 (no smooth on sequence).~%"))
  (when (and (= smooth1 1) (= smooth2 0))
    (if (> levels 1)
      (format stream "smoothing on first analysis level.~%")
      (format stream "with smoothed sequence.~%"))))

(defun ac (seq lisse1 prof level lisse2)
  (cond ((= prof 0)
         (reverse seq))
        (t
         (if (= level 0)
           (ac (list (analyse-contrastive seq lisse1) seq) lisse1 (1- prof) (1+ level) lisse2)
           (let ((seqr ()) (se '()))
             (dolist (s (car seq) (push (reverse se) seqr))
               (setf se (append (reverse (analyse-contrastive s lisse2)) se)))
             (ac (append (reverse seqr) seq) lisse1 (1- prof) (1+ level) lisse2))))))
 
(om::defmethod! RMA-1 ((seq list) (smoo1 integer) (levels integer)
                       &optional
                       (smoo2 0)
                       (alpha? 1)
                       (result 0)) 
  :menuins '( (1 (("yes" 1)
                 ("no"  0)))
             (3 (("no"  0)
                 ("yes"  1)))
             (4 (("alpha" 1)
                 ("num" 0)))
             (5 (("struct"  0)
                 ("short" 1)
                 ("extend" 2)
                 ("save" 3))))
  :initvals '(nil 1 1 0 1 0)
  :icon 128
  :doc "Recursive Mark Analysis. Returns only structures found."
  (when (< levels 1)
    (format t "Recursion error : levels must be >= 1 !~%")
    (abort))
  
  (cond ((= result 0)
         (let ((structures (cdr (ac seq smoo1 levels 0 smoo2))))
           (print-smoothing smoo1 smoo2 levels 't)
           (if (= 1 alpha?)
             (cond ((= 1 levels)
                    (mapcar 'string-downcase (to-alpha (car structures))))
                   ((> levels 1)
                    (setf structures (mapcar 'to-alpha structures))
                    (setf (car structures) (mapcar 'string-downcase (car structures)))
                    structures))
             structures)))
        ((= result 1)
         (print-smoothing smoo1 smoo2 levels 't)
         (multiple-value-bind (sequence analysis) (ac+complete seq smoo1 levels 0 smoo2 '())
           (when (and (= alpha? 1)
                      (not (eq nil (member 0 (car sequence)))))
             (setf (car sequence) (mapcar #'1+ (car sequence))))
           (list (mapcar #'(lambda (s) (alp?? alpha? s levels)) (cdr sequence)) analysis)))
        ((= result 2)
         (let ((time-start (get-internal-real-time)) (date (take-date)) (scores ()))
           (format t "~%~%Run of RECURSIVE MARK ANALYSIS on ~A ~A ~A ~A  at ~S h ~S mn"
                   (nth 0 date) (nth 1 date) (nth 2 date) (nth 3 date) (nth 4 date) (nth 5 date))
           (format t "~%For sequence : ~%~%~A" seq)
           (format t "~%~%~% ~D analysis (recursive) level~:P, " levels)
           (print-smoothing smoo1 smoo2 levels 't)
           (setf scores (rma-1-scores
                         (alpha-struct alpha?
                                       (Recursive-Mark-Analyse seq smoo1 levels smoo2 alpha? t))))
           (format t "~%~%********** SCORES ********** ~%")
           (dotimes (level (length scores))
             (format t "~%Scores at RMA level ~S :~%" (1+ level))
             (if (= 0 level)
               (dotimes (n (length (nth level scores)))
                 (format t "for structure ~S.~S:~%" (1+ level) (1+ n))
                 (dolist (u (nth n (nth level scores)))
                   (format t "~A = ~S~%" (car u) (cadr u))))
               (dolist (u (nth level scores))
                 (format t "~A = ~S~%" (car u) (cadr u)))))
           (format t "~%~%Computation time = ~S seconds." (float
                                                           (/ (- (get-internal-real-time) time-start)
                                                              internal-time-units-per-second))))
         (format t "~%********** End of Recursive Mark Analysis *************~%"))
        ((= result 3)
         (let ((out-file (om::om-choose-new-file-dialog
                          :prompt "Recursive Mark Analysis text file"
                          :button-string "save as"))
               time-start scores
               (date (take-date)))
           (format t "Writing marker analysis in file : ~S...~%" (pathname-name out-file))
           (with-open-file (out-stream out-file                          
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
             (format out-stream "~%~%Run of RECURSIVE MARK ANALYSIS ~A ~A ~A ~A  at ~S h ~S mn"
                     (nth 0 date) (nth 1 date) (nth 2 date) (nth 3 date) (nth 4 date) (nth 5 date))
             (format out-stream "~% on sequence : ~%~%~A" seq)
             (format out-stream "~%~%~% ~D analysis (recursive) level~:P, " levels)
             (print-smoothing smoo1 smoo2 levels out-stream)
             (setf time-start (get-internal-real-time))
             (setf scores (rma-1-scores
                           (alpha-struct alpha?
                                         (Recursive-Mark-Analyse seq smoo1 levels smoo2 alpha? out-stream))))
             (format out-stream "~%~%********** SCORES ********** ~%")
             (dotimes (level (length scores))
               (format out-stream "~%Scores at RMA level ~S :~%" (1+ level))
               (if (= 0 level)
                 (dotimes (n (length (nth level scores)))
                   (format out-stream "for structure ~S.~S:~%" (1+ level) (1+ n))
                   (dolist (u (nth n (nth level scores)))
                     (format out-stream "~A = ~S~%" (car u) (cadr u))))
                 (dolist (u (nth level scores))
                   (format out-stream "~A = ~S~%" (car u) (cadr u)))))
             (format out-stream "~%~%Computation time = ~S seconds." (float
                                                                      (/ (- (get-internal-real-time) time-start)
                                                                         internal-time-units-per-second)))
             (format out-stream "~%********** End of Recursive Mark Analysis *************~%"))
           ;(set-mac-file-creator out-file 'ttxt)
           )
         (format t "DONE~%"))))

(defun alpha-struct (yn structs)
  (loop for n from 0
        until (= n (length structs))
        do
        (setf (nth n structs)
              (mapcar #'(lambda (l) (alp?? yn l n)) (nth n structs))))
  structs)

(defun test-eq-l (list)
  (remove-duplicates list :test 'equalp))

(defun compt-score (pattern list)
  (let ((c 0))
    (loop for n from 0
          until (= 0 (length list))
          do
          (when (equalp pattern (pop list))
            (setf c (1+ c))))
    (list pattern c)))

(defun scores-level-0 (list)
  (cond ((equal 'nil (member 'nil (mapcar #'listp list)))
         (let ((scores ()) (patts (mapcar #'remove-duplicates list)))
           (dotimes (l (length list) (nreverse scores))
             (push (mapcar #'(lambda (x) (compt-score x (nth l list))) (nth l patts)) scores))))
        ((equal 'nil (member 'nil (mapcar #'stringp list)))
         (let ((lstring (mapcar #'string-to-list list)) (patts ()) (scores ()))
           (setf patts (mapcar #'remove-duplicates lstring))
           (setf patts (mapcar #'(lambda (x) (remove #\Space x)) patts))
           (setf lstring (mapcar #'(lambda (x) (remove #\Space x)) lstring))
           (dotimes (l (length lstring)
                       (mapcar #'(lambda (list) (mapcar #'list-char-score list)) (nreverse scores)))
             (push (mapcar #'(lambda (x) (compt-score x (nth l lstring))) (nth l patts)) scores))))
        (t
         (format t "~% Error : scores-level-0 : bad form on structures for level 0.~%")
         (abort))))



(om::defmethod! rma-1-scores ((structures list))
  :initvals '(nil)
  :icon 128
  :doc "Returns the score of each structure, level by level of the rma-1 analysis."
  (let ((types (mapcar 'test-eq-l structures)) (scores ()) (s ()))
    (setf (car types) (mapcar #'remove-duplicates (car types)))
    (if (equal 'nil (member 'nil (mapcar 'listp structures)))
      (dotimes (l (length types) (nreverse scores))
        (setf s '())
        (if (= 0 l)
          (push (scores-level-0 (car structures)) scores)
          (dotimes (p (length (nth l types)) (push (sort s '> :key #'cadr) scores))
            (push (compt-score (nth p (nth l types)) (nth l structures)) s))))
      (format t "~%Error : rma-1-scores : input must be list of lists.~%"))))

;****************

(defun lisse (list)
"supprime les elements contigus identiques"
      (let ((L()))
        (loop for x from 1 to (length list)
               do
               (cond ((not (equal (nth x list) (nth (1- x) list)))
                      (push (nth (1- x) list) L))
                     ((and (= x (length list))
                           (not (equal (nth x list) (nth (1- x) list))))
                      (push (nth x list) L))))
        (reverse L)))

;donne les positions des segments selon critere
(defun con (critere serie)
       (let ((C()))
         (dotimes (q (length serie) (reverse C))
           (if (eq critere (nth q serie))
             (push q C)))))

;idem mais avec liste de criteres et donne (critere . position)
 (defun cont (criterel serie)
       (let ((C()))
         (dolist (c1 criterel (reverse C))
           (dotimes (q (length serie))
             (if (eq c1 (nth q serie))
             (push (cons c1 q) C))))))

;idem mais ldel ((criteres) (positions_du_critern))
(defun contr (criterel serie)
       (let ((C()))
         (dolist (c1 criterel (cons criterel (reverse C)))
           (let ((D()))
             (dotimes (q (length serie) (push (reverse D) C))
               (if (eq c1 (nth q serie))
                 (push q D)))))))

;idem mais defini les criteres
(defun contraste (serie)
       (let ((C()) (crit()))
         (setf crit (remove-duplicates serie))
         (dolist (c1 crit (cons crit (reverse C)))
           (let ((D()))
             (dotimes (q (length serie) (push (reverse D) C))
               (if (eq c1 (nth q serie))
                 (push q D)))))))

;calcul n - (n-1)
(defun n-n-1l (list)
      (let ((L()))
        (dotimes (x (- (length list) 1) (reverse L))
          (push (- (nth (+ x 1) list) (nth x list)) L))))

;calcul de (n+1) - n d'une serie
(defun n+1-n (serie)
    (let ((l()))
      (dotimes (n (- (length serie) 1) (reverse l))
        (push (- (nth (+ n 1) serie) (nth n serie)) l))))


;************************** analyse critere repetition **********************************
(defun cnp (set p)
  (if (= p 1)
    (mapcar 'list set)
    (let ((partial-sol (cnp (rest set) (- p 1))))
      (loop for elt in set
            for partial on partial-sol
            append (loop for elt2 in partial 
                         unless (>= elt (car elt2)) collect (cons elt elt2))))))

#|
(cnp '(0 1 2 3 4 5)  5)
|#

(defun cnp-l (dim max)
  (let ((set
         (let ((r ()))
           (mapcar #'(lambda (x) (- x 1))
                   (dotimes (n dim (reverse r))
                     (push (+ n 1) r))))))
    (loop for n from 1 to max
          append (cnp set n))))

#|
(cnp-l 10 4)
|#

(defun ptrn-mat-bit (ptrn-pos length)
  (let ((m (make-array (list (length ptrn-pos) length))))
    (dotimes (n (length ptrn-pos) m)
      (dotimes (o (length (cadr (nth n ptrn-pos))))
        (dotimes (p (length (car (nth n ptrn-pos))))
          (setf (aref m
                      n (+ (nth o (cadr (nth n ptrn-pos))) p)) 1))))))

#|
(defun ptrn-mat-bit2 (ptrn-pos length enchass)
  (let ((m (make-array (list (length ptrn-pos) length))))
    (dotimes (n (length ptrn-pos) m)
      (dotimes (o (length (cadr (nth n ptrn-pos))))
        (if (eq enchass 0)
          (cond ((> o 1)
                 (when (>= (nth o (cadr (nth n ptrn-pos)))
                           (+ (nth (- o 1) (cadr (nth n ptrn-pos)))
                              (length (car (nth n ptrn-pos)))))
                   (dotimes (p (length (car (nth n ptrn-pos))))
                     (setf (aref m
                                 n (+ (nth o (cadr (nth n ptrn-pos))) p))
                           1))))
                ((and (= o 1)
                      (>= (nth 1 (cadr (nth n ptrn-pos)))
                          (+ (nth 0 (cadr (nth n ptrn-pos)))
                             (length (car (nth n ptrn-pos))))))
                 (dotimes (k 2)
                   (dotimes (p (length (car (nth n ptrn-pos))))
                     (setf (aref m
                                 n (+ (nth k (cadr (nth n ptrn-pos))) p))
                           1)))))
          (dotimes (p (length (car (nth n ptrn-pos))))
            (setf (aref m
                        n (+ (nth o (cadr (nth n ptrn-pos))) p))
                  1)))))))
|#

(defun ptrn-l (l!ptrn)
"Gives only the list of the patterns returned by ptrn-recogn."
  (let ((l-ptrn ()))
    (dolist (l l!ptrn l-ptrn)
      (push (reverse (cdr (reverse l))) l-ptrn))))

(defun pos-ptrn (ptrn seq)
"Gives all positions in seq where starts ptrn.
ptrn : pattern to be found;
seq : list of symbols."
  (let ((pat ())
        (pos-p ()))
    (dotimes (n (+ 1 (- (length seq) (length ptrn))) (reverse pos-p))
      (setf pat ())
      (dotimes (o (length ptrn))
        (setf pat (append pat (list (nth (+ n o) seq)))))
      (when (equalp pat ptrn)
          (push n pos-p)))))

(om::defmethod! pos-ptrn-l ((lptrn list) (seq list)
                            &optional (min 2) (max 12))
  
  :initvals '(nil nil 2 12)
  :icon 128
  :doc "Gives all positions in seq where starts ptrns.
INPUT :
lptrn : list of patterns to be found;
seq : list of symbols where patterns will be found.
min : minimum length of pattern (default = 2);
max : maximum lenght of pattern (default = longer one.
OUTPUT : list of ((ptrn) (positions))"
  (let ((pat ())
        (pos-p ())
        (lpos-p ()))
    (dolist (l lptrn (reverse lpos-p))
      (setf pos-p ())
      (when (and (>= (length l) min) (<= (length l) max))
        (dotimes (n (+ 1 (- (length seq) (length l))) (reverse pos-p))
          (setf pat ())
          (dotimes (o (length l))
            (setf pat (append pat (list (nth (+ n o) seq)))))
          (when (equalp pat l)
            (push n pos-p)))
        (push (list l (reverse pos-p)) lpos-p)))))

(defun ptrn-mat (ptrn-pos length)
  (let ((m (make-array (list (length ptrn-pos) length))))
    (dotimes (n (length ptrn-pos) m)
      (dotimes (o (length (cadr (nth n ptrn-pos))))        
        (setf (aref m
                    n (nth o (cadr (nth n ptrn-pos))))
                (length (car (nth n ptrn-pos))))))))

(defun +row (mat row)
"Returns the sum of elements in row of matrix mat"
(let ((x 0))
(dotimes (i (array-dimension mat 1) x)
  (setf x (+ x (aref mat row i))))))

(defun mat+row (matrix l-rows)
"    Returns the percentage of completion of dim 1 of matrix by the sum of rows in l-rows.
The result is sorted by percentage score (better completions before)."
  (let ((r ())
        (val 0))
    (dolist (row l-rows (sort r #'> :key #'cadr))
      (dolist (v row)
        (setf val (+ val (+row matrix v))))
      (setf val (float (/ (* 100 val) (array-dimension matrix 1))))
      (push (list row val) r)
      (setf val 0))))

(defun mat-ptrn (matrix l-comb)
"    Returns the combinations of rows of the binary matrix specified in l-comb
which sastify a good completion pattern structure.
The result is sorted by percentage score (better completions before)"
  (let ((sum 0)
        (cheksum 0)
        (r ())
        (comb ()))
    (dotimes (n (1- (length l-comb)))
      (setf comb (pop l-comb))
          (loop for q from 0 to (- (array-dimension matrix 1) 1)                
                do                
                (loop for p from 0 to (- (length comb) 1)
                      until (>= cheksum 1)
                      do
                      (setf sum (+ sum (aref matrix (nth p comb) q)))
                      (when (>= sum 2) (setf cheksum 1)))
                (setf sum 0))
          (when (= cheksum 0)
            (push comb r))
          (setf cheksum 0))
    (mat+row matrix r)))

#|
(setf tm  #2A((1 1 0 0 1 1 0 0)
    (0 0 1 0 0 0 1 0)
    (0 0 1 1 0 0 1 1)
    (1 0 0 0 1 0 0 0)
    (0 1 1 1 0 1 1 1)
    (1 0 0 1 1 0 0 1)))
(mat-ptrn tm '((0) (0 2) (0 3) (1 5)))
(mat+row tm '((0) (0 2) (0 3) (1 5)))
|#

(defun min-dom (list)
  (first list))

(defun max-dom (list)
  (second list))

(defun pos-int-dom (list-dom val)
  (flet ((match (x) (and (<= val (max-dom x)) (>= val (min-dom x)))))
    (position-if #'match list-dom)))

(om::defmethod! ins-ptrn ((seq list) (ptrn list) (prof integer)
                          &optional (set nil) (marg 0)) 
  :menuins '((4 (("+-" 3)
                (" + "  1)
                (" - "  2))))
  :initvals '((1 2 3 4 1 2 1 5 1 2 5 3 4) ((1 2 3 4) (1 2)) 1 nil 0)
  :icon 128
  :doc "Finds the pattern(s) ptrn in list seq with or without
up to a number prof inserted items;
Return each pattern and its start positions.
Doesn't permit cross-overing of a pattern on itself.
Optional-1:
            A set (list) of elements which can be inserted in the patterns.
            If set empty, no constraint.
            When seq is a list of values, permits a list of 'domains'
         defined by a list of min and max values.
Optional-2:
            If seq is a list of values, definition of a margin around the values,
         more and less the value specified in set (+-);
         only more the value in set (+);
         or only less the value in set (-)."
  
  (let ((val 0)
        (p 0)
        (pos ())
        (long 0)
        (r ())
        (set2 0))
    (cond ((listp set)
           (format t "~% Set is a list. Using margin option with first value of set.~%")
           (if (not (integerp (car set)))
             (and (format t " First element of set is not a value: ignoring margin~%")
                  (setf marg 0))
             (setf set2 (car set))))
          ((integerp set)
           (setf set2 set))
          (t (format t "~% Margin option can't be applyed (set is not a value): using set without margin.~%")
             (setf marg 0)))
    (when (not (listp (car ptrn)))
      (setf ptrn (list ptrn)))
    (dolist (pattern ptrn (reverse r))
      (setf pos '())
      (setf p 0)
      (setf val 0)
      (setf long 0)
      (loop for n from 0 to (- (length seq) 1)
            do
            (when (> n 0)
              (when (= marg 3)
                (setf set (list (list (- (nth (1- n) seq) set2) (+ (nth (1- n) seq) set2)))))
              (when (= marg 1)
                (setf set (list (list (nth (1- n) seq) (+ (nth (1- n) seq) set2)))))
              (when (= marg 2)
                (setf set (list (list (- (nth (1- n) seq) set2) (nth (1- n) seq))))))
            (cond ((eq (nth n seq) (nth p pattern))
                   (when (<= val prof)
                     (setf p (+ p 1)))
                   (when (= p 1)
                     (setf long 0)))
                  (t (if (listp (car set))
                       (when (and (not (= 0 (length set)))
                                  (equal (integerp (pos-int-dom set (nth n seq))) 'nil))
                         (setf long 0)
                         (setf n (- n (+ p val)))
                         (setf p 0)
                         (setf val 0))
                       (when (and (not (= 0 (length set)))
                                  (equal (member (nth n seq) set) 'nil))
                         (setf long 0)
                         (setf n (- n (+ p val)))
                         (setf p 0)
                         (setf val 0)))
                     (when (and (>= p 1) (< p (length pattern)))                            
                       (setf val (+ val 1)))))
            (cond ((<= val prof)
                   (when (= p (length pattern))
                     (push (- n val (- p 1)) pos)
                     (setf long 0)
                     (setf n (+ 2 (- n (length pattern) val)))
                     (setf val 0)
                     (setf p 0)
                     )))
            (when (or (= p (length pattern))
                      (> (+ long val) (+ (length pattern) prof)))
              (setf p 0)
              (setf long 0)
              (setf n (+ 1 (- n (+ p val))))
              (setf val 0))
            (if (> p 0)
              (setf long (+ long 1))
              (setf long 0)))
      (push (list pattern (reverse pos)) r))))

#|
(ins-ptrn '(1 2 3 4 1 2 1 5 1 2 5 3 4) '((1 2 3 4) (1 2)) 1)
-->(((1 2 3 4) (0 8)) ((1 2) (0 4 8)))
(ins-ptrn '(1 9 2 3 4 1 2 1 5 1 2 2 3 3 4) '((1 2 3 4) (1 2)) 2 '((0 5) (8 10)))
-->(((1 2 3 4) (0 9)) ((1 2) (0 5 7 9)))
(ins-ptrn '(a b c d a e b d e a e b c d) '((a b c d) (a b)) 1 '(v))
-->(((a b c d) (0)) ((a b) (0)))
(ins-ptrn '(a b c d a e b d e a e b c d) '((a b c d) (a b)) 1 '())
-->(((a b c d) (0 9)) ((a b) (0 4 9)))
|#

(om::defmethod! structure-2 ((seq list) (n-max integer) 
                             alpha? result 
                             &optional (length nil) (seuil 10)) 
  :menuins '((2 (("alpha" 1)
                ("num"  0)))
            (3 (("extended" 0)
                ("pos" 1)
                ("mat" 2)
                ("p-score" 3)
                ("save" 4))))
  :initvals '(nil 10 1 0 nil 10)
  :icon 128
  :doc "INPUT
sequence = sequence of nums or symbols;
n-max = maximum number of patterns accepted in structure of seq;
alpha = alpha or num representation of the resulting structures;
result = type of output of analysis
          all -> detailed analysis;
          pos -> returns only the positions of the patterns;
          mat -> return the list of pattern and the associated binary matrix;
          p-score -> returns the score of structure completion for each structure;
          save -> save all analysis into a file.
&OPTIONAL
length = value or list of minimum and maximum values for length of patterns.
         If nil, lengths of patterns are set up to the half-lenght of the sequence;
seuil = minimum completion percentage of the structure taken in account;

OUTPUT
Returns an analysis of seq following the repetition criterium for segmentation.

Note : if out-of memory, try successives computations with a smaller value
of n-max (max number of patterns combined in each structure"
 
    (let ((list-patterns (remove-duplicates (pattern-ridond seq length) :test 'equal))
          (date (take-date))
          (time-start (get-internal-real-time))
          (run-time 0)
          pos-patterns
          mat-bin-patterns
          completion-patterns
          formes
          out-file
          cnp)
    (when (= result 4)
      (setf out-file (om::om-choose-new-file-dialog
                      :prompt "Structure-2 pattern analysis"
                      )))
    (setf pos-patterns (pos-ptrn-l list-patterns seq))
    (cond ((eq result 1)
           pos-patterns)
          (t
           (setf mat-bin-patterns (ptrn-mat-bit pos-patterns (length seq)))
           (cond ((eq result 2)
                  (list list-patterns
                        mat-bin-patterns))
                 (t
                  (setf cnp (cnp-l (array-dimension mat-bin-patterns 0) n-max))
                  (setf completion-patterns (mat-ptrn mat-bin-patterns cnp))
                  (setf cnp nil)
                  (setf mat-bin-patterns nil)                    
                  (setf formes (make-form (forma
                                           (loop for i from 0 to (- (length completion-patterns) 1)
                                                 collect (list
                                                          (loop
                                                            for j
                                                            from 0 to (- (length (car (nth i completion-patterns))) 1)
                                                            collect
                                                            (car (nth (nth j (car (nth i completion-patterns))) pos-patterns)))
                                                          (cadr (nth i completion-patterns))))
                                           seq seuil)
                                          alpha?))
                  (setf run-time (float (/ (- (get-internal-real-time) time-start)
                                           internal-time-units-per-second)))
                  (cond ((eq result 3)
                         formes)
                        ((eq result 0)
                         (to-stream seq list-patterns seuil formes completion-patterns 't date run-time))
                        ((eq result 4)
                         (print (format nil "Writing Structure-2 analysis in file : ~S...~%" out-file))
                         (with-open-file (out-st out-file                          
                                                 :direction :output
                                                 :if-exists :supersede
                                                 :if-does-not-exist :create)
                           (to-stream seq list-patterns seuil formes completion-patterns out-st date run-time))
                        
                         ))))))))

(om::defmethod! forma ((analys list) (seq list) (seuil number))
  
  :initvals '(nil nil 1)
  :icon 128
  (let ((r ()))
    (dolist (l analys (reverse r))
      (when (>= (cadr l) seuil)
        (push (list (pos-ptrn-l (car l) seq) (cadr l)) r)))))

(defun make-form (analysis alpha?)
  (let ((r ())
        (r1 ())
        (r2 ())
        (res ())
        (percent ()))
    (dolist (anal analysis (setf r (reverse r)))
      (setf r2 '())
      (setf percent (append (cdr anal) percent))
      (setf anal (car anal))
      (dolist (l anal)
        (setf r2 (append (cadr l) r2)))
      (setf r2 (sort r2 '<))
      (setf r1 '())
      (dotimes (n (length r2) (push (reverse r1) r))
        (dotimes (o (length anal))
          (when (not (equal (member (nth n r2) (cadr (nth o anal))) 'nil))
            (push (+ o 1) r1)))))
    (cond ((equal alpha? 0)
           (om::mat-trans (list (reverse percent) r)))
          ((= alpha? 1)
           (om::mat-trans (list (reverse percent) (dolist (k r (reverse res))
             (push (to-alpha k) res))))))))

(defun to-stream (seq list-of-pat seuil analysis compl stream date run-time)
  (format stream "~%****************************************~%")
  (format stream "~%   Run of Pattern Analysis - Structure-2 - on ~S ~S ~S ~S  at ~S h ~S mn,"
            (nth 0 date) (nth 1 date) (nth 2 date) (nth 3 date) (nth 4 date) (nth 5 date))
  (format stream "~%with the following sequence :~%~%")
  (format stream "~S~%" seq)
  (cond ((= (length list-of-pat) 0)
         (format stream "~%No pattern found.~%"))
        ((= (length list-of-pat) 1)
         (format stream "~%Pattern is :~%~S~%~%" list-of-pat))
        (t (format stream "~%~S patterns found :~%" (length list-of-pat))
           (dolist (l list-of-pat)
             (format stream "~S~%" l))
           (format stream "~%")))
  (format stream "With completion seuil fixed at ~S % ," seuil)
  (format stream " ~S formes are founded :~%~%" (length analysis))
  (dotimes (a (length analysis))
    (format stream "~S   -> ~,2F % of completion~%" (cdr (nth a analysis)) (car (nth a analysis)))
    (format stream "with the patterns :~%")
    (dolist (k (car (nth a compl)))
      (format stream "~S " (nth k list-of-pat)))
     (format stream "~%~%"))
  (format stream "~%~%")
  (format stream "~%computation time : ~,3F seconds~%~%       End of Pattern Analysis (Structure-2)~%" run-time))

; ********************* Classification automatique ***********************************

(om::defmethod! aver-class ((seq list) (class list))
  :initvals '(nil nil)
  :icon 128
  :doc "Return the average center of classes (one dimension)."
  (let ((r ())
        (rt ())
        (length (remove-duplicates class)))
    (dotimes (n (length length))
      (setf rt '())
      (loop for x from 0 to (- (length seq) 1)
            do
            (when (= (nth x class) n)
              (push (nth x seq) rt)))
      (push (reverse rt) r))
    (setf r (reverse r))  
    (dotimes (l (length r) r)
      (setf (nth l r)
            (float (/ (round
                       (* (/
                           (apply #'+ (nth l r)) (length (nth l r)))
                          10000))
                      10000))))))

(om::defmethod! quantize-1 ((seq list) (class list))
  :initvals '(nil nil)
  :icon 128
  :doc "Returns the quantization of elements in list according to the classification
defined in class (one dimension)"
  (let ((r ())
        (rt ())
        (length (remove-duplicates class)))
    (dotimes (n (length length))
      (setf rt '())
      (loop for x from 0 to (- (length seq) 1)
            do
            (when (= (nth x class) n)
              (push (nth x seq) rt)))
      (push (reverse rt) r))
    (setf r (reverse r))  
    (dotimes (l (length r) r)
      (setf (nth l r)
            (float (/ (round
                       (* (/
                           (apply #'+ (nth l r)) (length (nth l r)))
                          10000))
                      10000))))
    (setf rt ())
    (dolist (n class (reverse rt))
      (push (nth n r) rt))))


(om::defmethod! l-matrix ((list list ))
  :initvals '(nil )
  :icon 128
  :doc  "Makes a matrix from a list of lists."
  (let ((mat (if (not (listp (car list)))
               (make-array (list (length list) 2))
               (make-array (list (length list) (length (car list)))))))
    (if (not (listp (car list)))
      (dotimes (l (length list) mat)
        (setf (aref mat l 0) (nth l list)))
      (dotimes (l (length list) mat)
        (dotimes (m (length (car list)))
          (setf (aref mat l m) (nth m (nth l list))))))))

;;;;************  version matricielle des nuees dynamiques **************

;;en n dimensions
(om::defmethod! class-1 ((matrix t) (n integer) &optional
                         (alpha? 0)
                         (centers nil)
                         (verbose "no"))
  :menuins '((2 (("alpha" 1)
                ("num"  0)))
            (4 (("no" "no")
                ("yes" "yes"))))
  :initvals '(nil 2 0 'nil "no")
  :icon 128
  :doc "Clustering 'mouving-clouds' algorithm. Classify elements in matrix
of d-dimensions into n classes. The nth element in result-list corresponds
to the nth element (line) of matrix. 
The classe number is arbitrary"
  (let ((matrix (if (listp matrix)
                  (l-matrix matrix)
                  matrix))
        *m* *n-cl* *n* centres d-centres minima adresse-minima
        (classes ()) 
        (endtest 1))
    (setf *m* (nth 0 (array-dimensions matrix)))
    (setf *n* (nth 1 (array-dimensions matrix)))
    (setf *n-cl* n)
    ;---------  Initialisation des classes --------------------    
    (cond ((equal 'nil centers)
           ;- On initialise les classes arbitrairement...  
           (when (equalp verbose "yes")
             (format t "~% Random classes initialisation"))
           (setf classes (rand-classes *m* *n-cl*))
           (setf centres (centre-classes matrix classes *m* *n* *n-cl*))
           (setf d-centres (dist-grav2 matrix centres *m* *n* *n-cl*)))
          ;- ou on attribue les classes selon les centres donnes
          (t
           (when (equalp verbose "yes")
             (format t "~% Using initial centers classes :~% ~S~%" centers))
           (setf d-centres (dist-grav2 matrix centers *m* *n* *n-cl*))))
           (dotimes (a *m*)
             (dotimes (b *n-cl*)
               (cond ((eq b 0)
                      (setf minima (aref d-centres a b))
                      (setf adresse-minima 0))
                     (t
                      (when (< (aref d-centres a b) minima)
                        (setf minima (aref d-centres a b))
                        (setf adresse-minima b)))))
             (push adresse-minima classes))
    (setf classes (reverse classes))
    ;--------- On calcule les centres de chaque classe
    ;          puis les distances entre pts et centres ---------
    (do ((cnt 0 (+ cnt 1)))
        ((= endtest 0)
         (when (equalp verbose "yes")
           (format t "~&Minimum Local :"))
         (if (= alpha? 1)
           (return (to-alpha (mapcar #'1+ classes)))
           (return classes)))
      (when (equalp verbose "yes")
        (format t "~&Moving classes iter # ~S..." (1+ cnt))
        (format t "~%Classes :~S~%" classes)
        (format t "~%Centers are :~S~%~%" centres))
        
        (when (> cnt 0)
          (setf centres (centre-classes matrix classes *m* *n* *n-cl*)))
        
        (when (and (= cnt 0) (not (equal centers 'nil)))
          (setf centres centers))
        (setf d-centres (dist-grav2 matrix centres *m* *n* *n-cl*))
        
        ;-------- On teste les minima et reattribue
        ;         les classes au centre le plus proche --------------------------
        (setf endtest 0)
        (let ((new-classes '()))
          (dotimes (a *m*)
            (dotimes (b *n-cl*)
              (cond ((eq b 0)
                     (setf minima (aref d-centres a b))
                     (setf adresse-minima b))
                    (t
                     (cond ((< (aref d-centres a b) minima)
                            (setf minima (aref d-centres a b)
                                  adresse-minima b))
                           ((= (aref d-centres a b) minima)
                            (let ((choix (random 2)))
                              (setf minima (nth choix
                                                (list minima (aref d-centres a b)))
                                    adresse-minima (nth choix
                                                        (list adresse-minima b)))))))))
            (push adresse-minima new-classes))
          (cond ((not (= *n-cl* (length (remove-duplicates new-classes))))
                 (format t "Warning :~%End of algorithm at loop # ~S~%before stabilization for ~S classes.~%" cnt *n-cl*)
                 (setf endtest 0))
                ((not (equal classes (reverse new-classes)))
                 (setf classes (reverse new-classes))
                 (setf endtest 1))
                (t (setf endtest 0)))))))
            
        

#|
(setq mattest #2a((0 0) (1 1) (.5 .5) (5 6) (4 5) (4 7) (1 0)
(3 3) (3 2.5) (3.5 2.5)))
(class-1 mattest 2)

(matrixp mattest)
|#     


(defun centre-gravite (X)
  (let (sum g
            (n (nth 1 (array-dimensions X)))
            (m (nth 0 (array-dimensions X))))
    (setq g (make-array (list 1 n)))
    (dotimes (i n g)
      (setf sum 0)
      (dotimes (j m)
        (setf sum (+ sum (aref X j i))))
      (setf (aref g 0 i) (float (/ sum m))))))

(om::defmethod! matrix-center ((matrix t))
  :initvals '(nil)
  :icon 128
  :doc ""
  (let (sum g
            (n (nth 1 (array-dimensions matrix)))
            (m (nth 0 (array-dimensions matrix))))
    (setq g (make-array (list 1 n)))
    (dotimes (i n g)
      (setf sum 0)
      (dotimes (j m)
        (setf sum (+ sum (aref matrix j i))))
      (setf (aref g 0 i) (float (/ sum m))))))

;;donne la matrice centree de X
(defun x-centree (X)
  (let ((g (centre-gravite X))
         (n (nth 1 (array-dimensions X)))
         (m (nth 0 (array-dimensions X)))
         centree)
    (setq centree (make-array (list m n)))
    (dotimes (i m centree)
      (dotimes (j n)
        (setf (aref centree i j) (aref g 0 j))))
    (setf centree (subtract-two-matrices X centree))))

;;calcul la distance d'un ensemble de point avec son centre de gravite
(defun dist-grav (X)
  (let ((grav (centre-gravite X))
        d-grav
        diff
        (m (nth 0 (array-dimensions X)))
        (n (nth 1 (array-dimensions X))))
    (setf d-grav (make-array (list m 1)))
    (dotimes (i m d-grav)
      (setf diff 0)
      (dotimes (j n)
        (setf diff
              (+ diff (expt
                       (- (aref X i j)
                          (aref grav 0 j))
                       2))))
      (setf (aref d-grav i 0) diff))))


;;;matrice de variance covariance V = tXX avec X centree
(defun matvar (matrix)
  (const*matrix
   (/ 1 (car (array-dimensions matrix)))
   (multiply-two-matrices (transpose matrix) matrix)))

;;donne les distances entre tous les points d'un ensemble
(om::defmethod! dist-euclidienne ((matrix t))
  
  :initvals '(nil)
  :icon 128
  :doc "input = matrix of coordinates of points in a d-space;
   output = upper-matrix of euclidian distances."
  (let (k temp mat-dist
          (m (nth 0 (array-dimensions matrix)))
          (n (nth 1 (array-dimensions matrix))))
    (setf mat-dist (make-array (list (- m 1) m)))
    (dotimes (i m mat-dist) 
      (dotimes (j (- m (+ i 1)))
        (setf k (+ j (+ i 1)))
        (setf temp 0)
        ;somme des differences au carre
        (dotimes (l n)
          (setf temp
                (+ temp (expt
                         (- (aref matrix k l)
                            (aref matrix i l))
                         2))))
        (setf (aref mat-dist i k) (sqrt temp))))))

(om::defmethod! euclidian-d ((matrix t))
  :initvals '(nil)
  :icon 128
  :doc "input = matrix of coordinates of points in a d-space;
   output = upper-matrix of euclidian distances."
  (let (k temp mat-dist
          (m (nth 0 (array-dimensions matrix)))
          (n (nth 1 (array-dimensions matrix))))
    (setf mat-dist (make-array (list (- m 1) m)))
    (dotimes (i m mat-dist) 
      (dotimes (j (- m (+ i 1)))
        (setf k (+ j (+ i 1)))
        (setf temp 0)
        ;somme des differences au carre
        (dotimes (l n)
          (setf temp
                (+ temp (expt
                         (- (aref matrix k l)
                            (aref matrix i l))
                         2))))
        (setf (aref mat-dist i k) (sqrt temp))))))
          
#|          
(setq mattest #2a((0 0 0) (1 1 1) (1 2 1) (2 2 2) (4 1 3)))
(dist-euclidienne mattest)
=> #2a((3 6 12 26) (0 1 3 13) (0 0 2 14) (0 0 0 6))
|#

(defun rand-classes (m n)
  "cree une m-liste aleatoire de n nombres differents"
  (let ((alea ()) (alea-test ()))
    (dotimes (a m alea)
      (push (om::om-random-value n) alea))
    (setf alea-test (remove-duplicates alea))
    (cond ((eq n (length alea-test))
           (setf alea alea))
          (t
           (rand-classes m n)))))

;;calcule la matrice des centres de chaque classe
(defun centre-classes (X classes *m* *n* *n-cl*)
  "input = matrice des points
           liste-vecteur des classes attribuees a chaque point
   output = matrice des centres de chaque classe"
  (let (nuage point c tc centres)
    (setf centres (make-array (list *n-cl* *n*)))
    "-- compte le nbre de points pour chaque classes
     pour construire la matrice de chaque nuage-classe --"
    (dotimes (b *n-cl* centres)
      (setf c 0)
      (dolist (a classes)
        (cond ((eq a b)
               (setf c (+ c 1)))))
      (setf nuage (make-array (list c *n*)))
      ;-- place chaque point de X dans la matrice
      ;   de la classe attribuee --
      (setf point 0)
      (dotimes (a *m*)
        (cond ((eq (nth a classes) b)
               (setf point (+ point 1))
               (dotimes (d *n*)
                 (setf (aref nuage (- point 1) d) (aref X a d)))
               (setf tc (centre-gravite nuage)))))
      (dotimes (d *n*)
        (setf (aref centres b d) (aref tc 0 d))))))

(om::defmethod! class-center ((matrix t) (classes list)) 
  :initvals '(nil nil)
  :icon 128
  :doc "input = matrix of points in d-dimensions
           liste of classes founded for each point (line in matrix);
           Classes must be in numerical representation.
   output = matrix of classes centers."
  (let (nuage point c tc centres
              (*n-cl* (length (remove-duplicates classes)))
              (*m* (nth 0 (array-dimensions matrix)))
              (*n* (nth 1 (array-dimensions matrix))))
    (setf centres (make-array (list *n-cl* *n*)))
    "-- compte le nbre de points pour chaque classes
     pour construire la matrice de chaque nuage-classe --"
    (dotimes (b *n-cl* centres)
      (setf c 0)
      (dolist (a classes)
        (cond ((eq a b)
               (setf c (+ c 1)))))
      (setf nuage (make-array (list c *n*)))
      ;-- place chaque point de matrix dans la matrice
      ;   de la classe attribuee --
      (setf point 0)
      (dotimes (a *m*)
        (cond ((eq (nth a classes) b)
               (setf point (+ point 1))
               (dotimes (d *n*)
                 (setf (aref nuage (- point 1) d) (aref matrix a d)))
               (setf tc (centre-gravite nuage)))))
      (dotimes (d *n*)
        (setf (aref centres b d) (aref tc 0 d))))))


(defun dist-grav2 (X centres *m* *n* *n-cl*)
  "input = matrice des points
           matrice des centres de classes
   output = matrice des distances des points
            avec chaque classes"
  (let (d dist-test)
    (setf dist-test (make-array (list *m* *n-cl*)))
    (dotimes (a *m* dist-test)
      (dotimes (b *n-cl*)
        (setf d 0)
        (dotimes (c *n*)
          (setf d
                (+ d (expt
                         (- (aref X a c)
                            (aref centres b c))
                         2))))
        (setf (aref dist-test a b) (sqrt d))))))



(defun Eshannon (data)
  "Returns the Shannon entropy value of the data classified.
IN
data : list of classes distribution (typically data from class-1;
OUT
Shannon entropie value, 0 <= entropie <= (log n 2).
Cf. J. Wasemberg : L âme de la meduse, idees sur la complexite du monde,
Seuil, Paris, 1997."
  (let ((cl (remove-duplicates data))
        (N (length data))
        (P ()))
    (dolist (ci cl (- 0 (apply #'+ (mapcar #'(lambda (x) (* x (log x 2))) P))))
      (push (/ (length (remove-if-not #'(lambda (x) (equal x ci)) data)) N) P))))

(om::defmethod! E-shannon ((class list) (res string))
  :menuins '((1 (("absolute" "abs")
                ("relative" "rel"))))
  :initvals '(nil "abs")
  :icon 128
  :doc "Returns the Shannon entropy value of the data classified.
data : list of classes distribution (typically data from class-1);
res : absolute or relative entropy;
OUT
Shannon entropie value
0 <= entropy <= (log n 2) if res = absolute;
0 <= entropy <= 1.0 if res = relative.
Cf. J. Wasemberg : L âme de la meduse, idees sur la complexite du monde,
Seuil, Paris, 1997."
  (cond ((not (member 'nil (mapcar #'atom class)))
         (if (equalp res "abs")
           (setf res 1) (setf res 0))
         (cond ((= res 1)
                (Eshannon class))
               ((= res 0)
                (let ((cl (remove-duplicates class)))
                  (if (<= (length cl) 1)
                    (and (format t "Warning:~%1 classe found. No relative value for that case.~%")
                         (abort))
                    (/ (Eshannon class) (log (length cl) 2)))))))
        (t (mapcar #'(lambda (x) (e-shannon x res)) class))))

(om::defmethod! E-shannon ((class string) (res string))
  :menuins '((1 (("absolute" "abs")
                ("relative" "rel"))))
  :initvals '(nil "abs")
  :icon 128
  :doc "Returns the Shannon entropy value of the data classified.
data : list of classes distribution (typically data from class-1;
res : absolute or relative;
OUT
Shannon entropie value
0 <= entropy <= (log n 2) if res = absolute;
0 <= entropy <= 1.0 if res = relative.
Cf. J. Wasemberg : L âme de la meduse, idees sur la complexite du monde,
Seuil, Paris, 1997."
  (setf class (str->symb class))
  (E-shannon class res))

(om::defmethod! meta-class1 ((matrix t) (n integer) (iter integer)
                             &optional
                             (alpha? "num")
                             (centers nil)
                             (verbose "no"))
  :menuins '((3 (("alpha" "alpha")
                ("num" "num")))
            (5 (("no" "no")
                ("yes" "yes"))))
  :initvals '(nil 2 1)
  :icon 128
  :doc "Does n iterations of class-1 algorithm.
The classes designation is normalized."
  (if (equalp "no" verbose)
    (setf verbose 0) (setf verbose 1))
  (let ((r ()))
    (if (equalp "num" alpha?)
      (dotimes (a iter (nreverse r))
        (push (norm-class (class-1 matrix n 0 centers verbose)) r))
      (dotimes (a iter (nreverse r))
        (push (to-alpha (mapcar #'1+ (norm-class (class-1 matrix n 0 centers verbose)))) r)))))

(defun normalize-class (classes)
"reordonne les classes de class-1.
L'ordre de la classe etant le numero de sa premiere occurence dans la liste des classes.
Se connecte typiquement apres class-1 ou meta-class1."
  (let ((set ())
        (r ())
        (marker -1)
        (tempset ())
        n)
    (when (stringp classes)
      (setf classes (str->symb classes)))
    (setf n (length (remove-duplicates classes)))
    (dotimes (o n)
      (push o tempset))
    (setf tempset (reverse tempset))
    (dotimes (c (length classes) (nreverse r))
      (cond ((equal 'nil (member (nth c classes) set))
             (push (nth c classes) set)
             (setf marker (1+ marker))
             (push marker r))
            (t
             (push (- (1- (length set)) (pos (nth c classes) set)) r))))))

(om::defmethod! norm-class ((classes t)) 
  :initvals '(nil)
  :icon 128
  :doc "reordonne les classes de class-1.
L'ordre de la classe etant le numero de sa premiere occurence dans la liste des classes.
Se connecte typiquement apres class-1 ou meta-class1.
IN : string or list (of symbols or lists or strings)."
  (if (or (stringp classes)
          (not (listp (car classes))))
    (normalize-class classes)
    (mapcar #'(lambda (x) (normalize-class x)) classes)))

;(norm-class '("B B B B B B B B B B A A A B A" "B B B B B B B B B B A A A B A"))
;(norm-class '((a a a b a b a c a b) (a a a b a b a c a b d)))
;(norm-class "B B B B B B B B B B A A A B A")

(defun pos (item list)
  (loop for n from 0 to (1- (length list))
        when (equal (nth n list) item)
        do
        (return n)))

;(pos 'c '(a b a d a c a b))


(defun Prob-class (clusters)
"Give the probability for each to be element of class #"
  (let ((probalities (make-array
                      (list (length (remove-duplicates (car clusters)))
                            (length (car clusters))))))
    (dolist (cl clusters
                (const*matrix (float (/ 1 (length clusters))) probalities))
      (dotimes (n (length cl))
        (dotimes (c (length (remove-duplicates (car clusters))))
          (when (= c (nth n cl))
            (setf (aref probalities c n) (1+ (aref probalities c n)))))))))
#|
(prob-class '((0 1 0 0 0 1 1 0 1 0 2 2 2 1 2) (0 1 1 0 1 0 0 1 1 1 2 2 2 0 2) (0 1 2 0 2 0 0 1 1 2 1 2 2 0 1)
 (0 1 0 0 0 1 1 1 1 0 2 2 2 1 2) (0 0 1 0 1 0 0 1 2 1 2 2 2 0 2) (0 1 2 0 2 0 0 2 1 2 1 2 2 0 1)
 (0 1 0 0 0 1 1 0 1 0 2 2 2 1 2) (0 1 0 0 0 0 0 1 1 0 2 2 2 1 1) (0 1 0 0 0 0 0 0 1 0 2 2 2 1 2)
 (0 1 0 0 0 0 0 1 1 0 2 2 2 1 1) (0 1 0 0 0 0 0 1 1 0 2 2 2 0 1) (0 1 0 0 0 1 1 0 1 0 2 2 2 1 2)
 (0 0 1 0 1 0 0 1 2 1 2 2 2 0 2) (0 1 0 0 0 0 0 1 1 0 2 2 2 1 2) (0 1 0 0 0 0 0 1 1 0 2 2 2 0 1)
 (0 1 0 0 0 1 1 0 1 0 2 2 2 1 2) (0 1 2 0 2 0 0 2 1 0 1 2 2 0 1) (0 1 0 0 0 0 0 1 1 0 2 2 2 0 1)
 (0 1 1 0 1 0 0 1 1 0 2 2 2 0 1) (0 1 0 0 0 0 0 1 1 0 2 2 2 0 1)))
|#

(om::defmethod! p-class ((clusters list))
  :initvals '(nil)
  :icon 128
  :doc "Give the probability for each to be element of class #"
  (prob-class clusters))

(defun resume-class (prob val?)
"Affects each point i of the matrix prob to the class (j) with higher probability."
  (let ((nb-class (nth 0 (array-dimensions prob)))
        (n-pts (nth 1 (array-dimensions prob)))
        (r ())
        p
        cl)
    (dotimes (i n-pts (nreverse r))
      (setf p 0)
      (dotimes (j nb-class)
        (when (> (aref prob j i) p)
          (setf p (aref prob j i))
          (setf cl j)))
      (if (= val? 0)
        (push cl r)
        (push (list cl p) r)))))

(om::defmethod! res-class ((proba t) val?)
  
  :menuins '((1 (("classes" 0)
                ("proba" 1))))
  :initvals '(nil 0)
  :icon 128
  :doc "Affects each point i of the matrix prob to the class (j) with higher probability."
  (resume-class proba val?))
               

#|
(resume-class #2A((1.0 0.1 0.68 1.0 0.68 0.62 0.62 0.4 0.0 0.8 0.0 0.0 0.0 0.38 0.0)
    (0.0 0.9 0.24 0.0 0.24 0.38 0.38 0.54 0.92 0.16 0.1 0.02 0.02 0.62 0.26)
    (0.0 0.0 0.08 0.0 0.08 0.0 0.0 0.06 0.08 0.04 0.9 0.98 0.98 0.0 0.74)) )
|#

(defun test-entropie (clusters test out)
  (let ((entropies (mapcar #'(lambda (x) (E-shannon x "abs")) clusters))
        e)
    (when (= 0 test)
      (setf e (car (sort (copy-list entropies) '<))))
    (when (= 1 test)
      (setf e (car (sort (copy-list entropies) '>))))
    (if (= 0 out)
      (pos2 e entropies)
      (mapcar #'(lambda (n) (nth n clusters)) (pos2 e entropies)))))

(om::defmethod! e-test ((clusters list) (test string)
                        &optional (out "clust"))
  :menuins '((1 (("min" "min")
                ("max" "max")))
            (2 (("clust" "clust")
                ("nth" "nth"))))
  :initvals '(nil "min" "clust")
  :icon 128
  :doc "Returns the clusters which have the minimum or maximum entropy."
  (if (equalp "min" test)
    (setf test 0) (setf test 1))
  (if (equalp "clust" out)
    (car (remove-duplicates (test-entropie clusters test 1) :test #'equalp))
    (test-entropie clusters test 0)))


(defun pos2 (item list)
"returns the positions of item in list."
  (let ((r ()))
    (dotimes (n (length list))
      (when (equalp (nth n list) item)
        (push n r)))
    (reverse r)))

;*************** UTILITIES *******************

;;MATRIX

(defun num-rows (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 0))

(defun num-cols (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 1))

(defun make-matrix (rows &optional (cols rows))
  "Create a matrix filled with zeros.  If only one parameter is
  specified the matrix will be square."
  (make-array (list rows cols) :initial-element 0))

(defun copy-matrix (matrix)
  "Return a copy of the matrix."
  (let* ((rows (num-rows matrix))
         (cols (num-cols matrix))
         (copy (make-array (list rows cols))))
    (dotimes (row rows copy)
      (dotimes (col cols)
        (setf (aref copy row col) (aref matrix row col))))))

(defun make-identity-matrix (size)
  "Make an identity matrix of the specified size."
  (let ((matrix (make-array (list size size) :initial-element 0)))
    (dotimes (i size matrix)
      (setf (aref matrix i i) 1))))

;;multiplies two matrices of same or different sizes
(defun multiply-two-matrices
       (a-matrix
        b-matrix
        &key
        (result
         (make-array
          (list (nth 0 (array-dimensions a-matrix))
                (nth 1 (array-dimensions b-matrix))))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> another 2d matrix, with dimensions such that
           the product of a-matrix and b-matrix is defined
   [3] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain product of two matrices
returns
   [1] product of two matrices (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions b-matrix)))
        (common (nth 0 (array-dimensions b-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j) 0.0)
        (dotimes (k common)
          (incf (aref result i j)
                (* (aref a-matrix i k) (aref b-matrix k j))))))))

#|
(multiply-two-matrices #2a((0 0 1) (0 1 0) (1 0 0))
                       #2a((10 9) (8 7) (6 5)))
;==> #2a((6.0 5.0) (8.0 7.0) (10.0 9.0))
|#

;;multiplies two or more square matrices of same size
(defun multiply-matrix (&rest matrices)
  "Multiply matrices"
  (labels ((multiply-two (m1 m2)
             (let* ((rows1 (num-rows m1))
                    (cols1 (num-cols m1))
                    (cols2 (num-cols m2))
                    (result (make-matrix rows1 cols2)))
               (dotimes (row rows1 result)
                 (dotimes (col cols2)
                   (dotimes (i cols1)
                     (setf (aref result row col)
                           (+ (aref result row col)
                              (* (aref m1 row i)
                                 (aref m2 i col))))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'multiply-two matrices))))

(defun transpose
       (a-matrix
        &key
        (result
         (make-array
          (reverse (array-dimensions a-matrix)))))
  "given
   [1] A (required)
       ==> a 2d matrix
   [2] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain transpose of a-matrix
returns
   [1] transpose of a-matrix (placed in result)"
  (let ((list-of-two-integers (array-dimensions a-matrix)))
    (dotimes (i (nth 0 list-of-two-integers) result)
      (dotimes (j (nth 1 list-of-two-integers))
        (setf (aref result j i)
              (aref a-matrix i j))))))

#|
(transpose #2a((1 2) (3 4) (5 6)))
;==> #2a((1 3 5) (2 4 6))
|#


(defun trace-matrix (a-matrix)
  "given a square matrix,
returns its trace"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions a-matrix))))
    (assert (= m n))
    (let ((sum 0.0))
      (dotimes (i m sum)
        (incf sum (aref a-matrix i i))))))

;;inverts square matrix
(defun invert-matrix (matrix &optional (destructive T))
  "Find the inverse of a matrix.  By default this operation is
  destructive.  If you want to preserve the original matrix, call this
  function with an argument of NIL to destructive."
  (let ((result (if destructive matrix (copy-matrix matrix)))
        (size (num-rows matrix))
        (temp 0))
    (dotimes (i size result)
      (setf temp (aref result i i))
      (dotimes (j size)
        (setf (aref result i j)
              (if (= i j)
                  (/ (aref result i j))
                  (/ (aref result i j) temp))))
      (dotimes (j size)
        (unless (= i j)
          (setf temp (aref result j i)
                (aref result j i) 0)
          (dotimes (k size)
            (setf (aref result j k)
                  (- (aref result j k)
                     (* temp (aref result i k))))))))))
;; multiply matrix by constant (integer or float)
(defun const*matrix (const matrix)
  (let ((m (nth 0 (array-dimensions matrix)))
        (n (nth 1 (array-dimensions matrix)))
        result)
    (setq result (make-array (list m n)))
    (dotimes (o m result)
      (dotimes (p n)
        (setf (aref result o p) (* const (aref matrix o p)))))))

(defun subtract-two-matrices
       (a-matrix
        b-matrix
        &key
        (result
         (make-array (array-dimensions a-matrix))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> a 2d matrix, with dimensions the same
           as a-matrix
   [3] result (keyword; new vector of appropriate size)
       <== a matrix to contain result of subtracting
           b-matrix from a-matrix
returns
   [1] a-matrix minus b-matrix (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (nth 1 (array-dimensions a-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
        (setf (aref result i j)
              (- (aref a-matrix i j) (aref b-matrix i j)))))))

(defun multiply-matrix-and-vector
       (a-matrix
        b-vector
        &key
        (result
         (make-array
          (nth 0 (array-dimensions a-matrix)))))
  "given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-vector (required)
       ==> a vector, with dimensions such that
           the product of a-matrix and b-vector is defined
   [3] result (keyword; new vector of appropriate size)
       <== a vector to contain product of a-matrix and b-vector
returns
   [1] product of a-matrix and b-vector (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
        (n (length b-vector)))
    (dotimes (i m result)
      (setf (aref result i) 0.0)
      (dotimes (j n)
        (incf (aref result i)
              (* (aref a-matrix i j) (aref b-vector j)))))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Estremanti;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! just-prim ((ldl list)) 
  
  :initvals '(nil)
  :icon 128
  :doc "restituisce solo i comportamenti : max, flex,min"
  
  (let ((r ()))
    
    (dolist (l ldl (reverse r))
      (push (cadr l) r))))
;
;
;--------------------------------------
;
;
(om::defmethod! prim+prof ((ldl list)) 
  
  :initvals '(nil)
  :icon 128
  :doc"restituisce solo i comportamenti : max, flex,min più la profondita"
  
  (let ((r ()))
    
    (dolist (l ldl (reverse r))
      (push (om::x-append (second l) (fourth l)) r))))
;
;
;--------------------------------------
;
;
(om::defmethod! pos+prim ((ldl list)) 
  
  :initvals '(nil)
  :icon 128
  :doc"restituisce solo i comportamenti : max, flex,min più la profondita"
  
  (let ((r ()))
    
    (dolist (l ldl (reverse r))
      (push (om::x-append (first l) (second l)) r))))
;
;
;--------------------------------------
;
;
(om::defmethod! prim+prof+val ((ldl list))
  
  :initvals '(nil)
  :icon 128
  :doc "restituisce solo i comportamenti :max, flex,min più la profondita
             ed i valori precisi."
  
  (let ((r ()))
    
    (dolist (l ldl (reverse r))
      (push (rest l) r))))
;
;
;--------------------------------------
;
;

(om::defmethod! correx ((seq list))
  
  :initvals '(nil)
  :icon 128
  :doc ""
  
  (let ((lungo (length seq)))
    
    (cond ((and (> (nth (- lungo 2) seq)
                   (nth (- lungo 1) seq))
                (> (nth (- lungo 2) seq)
                   (nth (- lungo 3) seq)))
           (om::x-append (- lungo 2)
                         "max"
                         (nth (- lungo 2) seq)
                         1))
          ((and (< (nth (- lungo 2) seq)
                   (nth (- lungo 1) seq))
                (< (nth (- lungo 2) seq)
                   (nth (- lungo 3) seq)))
           (om::x-append (- lungo 2)
                         "min"
                         (nth (- lungo 2) seq)
                         1))
          ((and (= (nth (- lungo 2) seq)
                   (nth (- lungo 1) seq))
                (not (= (nth (- lungo 2) seq)
                        (nth (- lungo 3) seq))))
           (om::x-append (- lungo 2)
                         "flex"
                         (nth (- lungo 2) seq)
                         2)))))
;
;
;--------------------------------------
;
;
(om::defmethod* primit ((seq list) (profond t)) 
  
  :initvals '(nil nil)
  :icon 128
  :doc "Fa l'analisi di tutti i massimi i minimi e flessi 
             con relativa profondita. Profondita 1 significa che 
             un massimo ha una nota prima e dopo; 2 che ne ha due 
             prima e 2 dopo."
  
  
  (let (prof prim min max pos value 
             (l (length seq))
             (profil ()))
    
    (when (eq profond nil)
      (setf profond (expt 2 33)))
    
    (loop for n from 0 to (- l 1)
          do
          (unless (= n (- l 2))
            (setf prim nil)
            (setf prof 0)
            (cond ((listp profond)
                   (if (eq (length profond) 1)
                     (and (setf min 1)
                          (setf max (car profond)))
                     (and (setf min (car profond))
                          (setf max (cadr profond)))))
                  ((integerp profond)
                   (setf min 1)
                   (setf max profond)))
            (when (< n max)
              (setf max n))
            (when (>= (+ n max) l)
              (setf max (- l n 1)))
            (when (> max 0)
              (loop for o from min to max
                    do
                    
                    ;;;;;;;;;;;;;;;;;;;;;test si primitive maximum
                    (when (and 
                           (nth (+ n 1) seq)
                           (> (nth n seq)
                              (nth (+ n 1) seq))
                           (< (nth (- n 1) seq)
                              (nth n seq)))
                      (setf value (nth n seq))
                      (setf prim "max")
                      (setf pos n)
                      (when (and 
                             (nth (+ n 1) seq)
                             (> (nth (+ n (- o 1)) seq)
                                (nth (+ n o) seq))
                             (> (nth (- n (- o 1)) seq)
                                (nth (- n o) seq)))
                        (setf prof (+ prof 1))))
                    ;;;;;;;;;;;;;;;;;;;;;test si primitive minimum
                    (when (and 
                           (nth (+ n 1) seq)
                           (< (nth n seq)
                              (nth (+ n 1) seq))
                           (> (nth (- n 1) seq)
                              (nth n seq)))
                      (setf value (nth n seq))
                      (setf prim "min")
                      (setf pos n)
                      (when (and 
                             (nth (+ n 1) seq)
                             (< (nth (+ n (- o 1)) seq)
                                (nth (+ n o) seq))
                             (< (nth (- n (- o 1)) seq)
                                (nth (- n o) seq)))
                        (setf prof (+ prof 1))))
                    ;;;;;;;;;;;;;;;;;;;;;test si primitive flex
                    (when (and (= n 1)
                               (eq (nth n seq)
                                   (nth (- n 1) seq)))
                      (setf pos 0)
                      (setf value (nth n seq))
                      (setf prim "flex")
                      (setf prof 2))
                    ;eccezione per l'inizio
                    (when (eq (nth n seq)
                              (nth (+ n 1) seq))
                      (if (and (= n 1) (eq (nth n seq)
                                           (nth (- n 1) seq)))
                        (setf pos 0)
                        (setf pos n))
                      (let ((count (if (= pos  0) 2 1)))
                        (loop while (and (nth (+ n 1) seq)   
                                         (eq (nth n seq)
                                             (nth (+ n 1) seq))) do
                              (incf count)
                              (setf n (+ n 1)))
                        (setf value (nth n seq))
                        (setf prim "flex")
                        (setf prof count)))
                    )
              (when (and (not (equalp prim nil)) (> prof 0))         
                (push (list pos prim value prof) profil)))))
    (reverse profil)))
;
;
;--------------------------------------
;
;    
(om::defmethod* primitives ((seq list) (profond t))
  
  :initvals '(nil nil)
  :icon 128
  :doc  "E' come primit solo che c'e correx alla fine"
  
  (if (equalp (correx seq) nil)
    (primit seq profond)
    (om::x-append (primit seq profond) (list (correx seq)))))
;
;
;--------------------------------------
;
;
(om::defmethod! min-flex-max-2 ((list list) (profond list)
                                (d-flx integer)) 
            
  :initvals '(nil nil 2)
  :icon 128
  :doc  "trastorma i flessi in min o max secondo d-flx."
  
  (let ((ris nil)
        (calcolo (primitives list profond)))
    
    (dotimes (x (length calcolo) (nreverse ris))
      
      
      (cond ((and (and (= x 0)
                       (equalp (second (nth x calcolo)) "flex"))
                  (= (first (nth x calcolo)) 0)
                  (push (nth x calcolo) ris)))
            
            ((and (= (third (nth x calcolo)) (first (last list)))
                  (equalp (second (nth x calcolo)) "flex"))
             (push (nth x calcolo) ris))
            
            ((and (and (equalp (second (nth x calcolo)) "flex")
                       (> (fourth (nth x calcolo)) d-flx))
                  (and (> (nth (first (nth x calcolo)) list)
                          (nth (- (first (nth x calcolo)) 1) list))
                       (> (nth (first (nth x calcolo)) list)
                          (nth (+ (first (nth x calcolo)) (fourth (nth x calcolo))) list))))
             (push (subst "max" "flex" (nth x calcolo) :test 'equalp) ris))
            
            ((and (and (equalp (second (nth x calcolo)) "flex")
                       (> (fourth (nth x calcolo)) d-flx))
                  (and (< (nth (first (nth x calcolo)) list)
                          (nth (- (first (nth x calcolo)) 1) list))
                       (< (nth (first (nth x calcolo)) list)
                          (nth (+ (first (nth x calcolo)) (fourth (nth x calcolo))) list))))
             (push (subst "min" "flex" (nth x calcolo) :test 'equalp) ris))
            
            ((push (nth x calcolo) ris)) 
            ))))
;
;
;--------------------------------------
;
;
(om::defmethod! min-flex-max-1 ((list list) (profond list)
                                &optional (d-flx 2) (chois nil))
  
  :initvals '(nil nil 2 nil)
  :icon 128
  :doc "restituisce l'analisi per min, flex, max ma con
             la possibilita di scegliere non solo la profondita
             da testare ma anche uelle da tenere"
  
  (let ((ris nil)
        (calcolo (min-flex-max-2 list profond d-flx)))
    
    (if (not (equalp chois nil))
      (dotimes (x (length calcolo) (nreverse ris))
        (dolist (y chois)
          (if (equalp (fourth (nth x calcolo)) y)
            (push (nth x calcolo) ris))))
      calcolo)))
;
;
;--------------------------------------
;
;
(om::defmethod! min-flex-max-4 ((list list) (profond list)
                                &optional (chois nil))
  
  :initvals '(nil nil nil)
  :icon 128
  :doc "restituisce l'analisi per min, flex, max ma con
             la possibilita di scegliere non solo la profondita
             da testare ma anche uelle da tenere"
  
  (let ((ris nil)
        (calcolo (primitives list profond)))
    
    (if chois
      (dotimes (x (length calcolo) (nreverse ris))
        (dolist (y chois)
          (if (equalp (fourth (nth x calcolo)) y)
            (push (nth x calcolo) ris))))
      calcolo)))
;
;
;--------------------------------------
;
;
(om::defmethod! min-flex-max-3 ((list list) (profond list)
                                &optional (chois nil))
  
  :initvals '(nil nil nil)
  :icon 128
  :doc "restituisce l'analisi per min, flex, max ma con
             la possibilita di scegliere non solo la profondita
             da testare ma anche uelle da tenere"
  
  (let ((ris nil)
        (calcolo (primitives list profond)))
    
    (if chois
      (dotimes (x (length calcolo) (nreverse ris))
        (dolist (y chois)
          (if (equalp (fourth (nth x calcolo)) y)
            (push (nth x calcolo) ris))))
      calcolo)))
;
;
;--------------------------------------
;
;
(om::defmethod! min-flex-max ((list list) which
                              &optional prof chois d-flx)
  
  :menuins '( (1 (("prim" 1)
                 ("prof" 2)
                 ("vals" 3)
                 ("every" 4))))
  :initvals '((6000 4000 5600 4700 4100 5900 6400 7800 7400 6300 
               5500 5200 4900 6400 6800 8300 8000 6400 6100 6100 6100 
               6100 6100 6100 4600 4700 4500 5300 5400 5600 5600 5600 
               5600 5500 5500 6900 6900 7300 6100 5900) 1 nil nil nil)
  :icon 128
  :doc "restituisce l'analisi per min, flex, max ma con
             la possibilita di scegliere non solo la profondita
             da testare ma anche uelle da tenere. Il tipo di 
             scelta di analisi e stabilito dal menu which."
  
  (let* ((calcolo 
          (cond ((not (equalp d-flx nil))
                 (min-flex-max-1 list (om::list! prof) d-flx (om::list! chois)))
                ((min-flex-max-3 list (om::list! prof) (om::list! chois))))))
    
    
    (cond ((and (not (equalp (flexus list)
                             'no-pure-flex))
                (not (equalp (flexus list)
                             'si-max-min)))
           (flexus list))
          
          ((or (equalp (flexus list)
                       'si-max-min)
               (equalp (flexus list)
                       'no-pure-flex))
           (case which
             (1 (just-prim calcolo))
             (2 (prim+prof calcolo))
             (3 (prim+prof+val calcolo))
             (4 calcolo)
             ))
          )))
;
;
;--------------------------------------
;
;
(om::defmethod! flesso-puro-1 ((list list)) 
  
  :initvals '(nil)
  :icon 128
  :doc "Restituisce nil se c'e o un max o un min
             altrimenti restituisce tutte le note dei flessi"
  
  (let* ((ris nil)
         (analisi (just-prim (primitives list 1)))
         (calcolo (if (and (equalp (count "max" analisi :test 'equalp) 0)
                           (equalp (count "min" analisi :test 'equalp) 0))
                    'no-max-min 'si-max-min)))
    
    (if (equalp calcolo 'no-max-min)
      
      (dotimes (x (- (length list) 1) (nreverse ris))
        (if (= 
             (first (om::x->dx (list (nth x list) (nth (1+ x) list))))
             0)
          (push (nth x list) ris)))
      )))
;
;
;--------------------------------------
;
;
(om::defmethod! flexus ((list list)) 
  
  :initvals '(nil)
  :icon 128
  :doc "Restituisce no-flex se esiste o un max o un min e
             restituisce il flesso con la sua length se esistono
             solo dei flessi, in più mette il segno della curva: fle+
             indica che la curva sale e fle- che la curva scende."
  
  
  (if (not (equalp (flesso-puro-1 list) nil))
    (let* ((ris nil)
           (ros nil)
           (calcolo (flesso-puro-1 list))
           (calcoletto (remove-duplicates calcolo))
           (calcolaccio (dolist (y calcoletto (nreverse ris))
                          (push (count y calcolo :from-end '1) ris)))
           (calcolettino (om::group-list calcolo calcolaccio 1))
           (calcolettone (dolist (y calcolettino (nreverse ros))
                           (push (om::x-append (nth 0 y) (1+ (length y))) ros))))
      
      
      (if (or (< (first list) (first (first calcolettone))
                 (first (last list)))
              
              (> (first list) (first (first calcolettone))
                 (first (last list))))
        
        
        (if (> (nth 1 list) (nth 0 list))
          (om::x-append 'pure-flex+ calcolettone)
          (om::x-append 'pure-flex- calcolettone))
        'no-pure-flex))
    'si-max-min))
;
;
;--------------------------------------
;
;
(om::defmethod! scomp ((list1 list) (list2 list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Restituisce la nostra lista1 di partenza suddivisa nei segmenti
             da noi scelti tramite lista2."
  
  (let (ris)
    (dotimes (x (length list2) (nreverse ris))
      (push (primo-passo
             (nthcdr
              (let (ros)
                (dotimes (y (length ris))
                  (push (nth y list2) ros))
                (apply '+ ros))
              list1)
             (nth x list2))
            ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! find-extr ((list list))
  
  :initvals '(nil)
  :icon 128
  :doc "Trova il massimo assoluto d il minimo assoluto"
  
  (let* ((ris nil)
         (calcolo (min-flex-max list nil))
         (ordina (om::mat-trans calcolo)))
    
    (dotimes (x (length (first ordina)) (nreverse ris))
      (when (eq (nth x (third ordina))
                (g-min (third ordina)))
        (push (om::x-append (nth x (second ordina))
                            'absolute
                            (nth x (third ordina)))
              ris))
      (when (eq (nth x (third ordina))
                (g-max (third ordina)))
        (push (om::x-append (nth x (second ordina))
                            'absolute
                            (nth x (third ordina)))
              ris))
      )))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Ricostituzione;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! 1-0-1-reconst ((list list))
  
  :initvals '(nil)
  :icon 128
  :doc   ""
  
  (om::dx->x 0 list))
;
;
;--------------------------------------
;
;
(om::defmethod! reconst-prim ((list list) (start list)) list
            
            "Ricostruisce la lista usando min, max, flex 
             più eventualmente l'indice di profondita"
  
 (let ((ris start)
        prim
        prof
        last)
    
    (when (stringp (car list))
      (setf prof 1)
      (dolist (l list (reverse ris))
        (cond ((equalp l "min")
               (setf prim '-))
              ((equalp l "max")
               (setf prim '+)))
        (dotimes (n prof)
          (if (equalp l "flex")
            (setf ris (append (list (car ris) (car ris)) ris))
            (and (and (setf last (car ris))
                      (push (apply prim (list last 1)) ris))
                 (dotimes (n prof)
                   (setf last (car ris))
                   (push (apply prim (list last -1)) ris)
                   ))))))))
;
;
;--------------------------------------
;
;
(om::defmethod! reconstitute ((list list) which (start integer))
  :menuins '( (1 (("prim" 1)
                 ("prof" 2)
                 ("vals" 3)
                 ("every" 4))))
  :initvals '(nil 1 0)
  :icon 128
  :doc  ""
  (case which
    (1 (cond ((not (atom (car list)))
              (format t "~%ERROR !! different number of elements in the input list !!")
              (abort))
             ((reconst-prim list (om::list! start)))))
    (2 (cond ((or (atom (car list))
                  (not (equalp (length (car list)) 2)))
              (format t "~%ERROR !! different number of elements in the input list !!")
              (abort))
             ((reconst-prim+prof list))))
    (3 (cond ((or (atom (car list))
                  (not (equalp (length (car list)) 3)))
              (format t "~%ERROR !! different number of elements in the input list !!")
              (abort))
             ((reconst-prim+prof+val list start))))
    (4 (cond ((or (atom (car list))
                  (not (equalp (length (car list)) 4)))
              (format t "~%ERROR !! different number of elements in the input list !!")
              (abort))
             ((pos+prim+prof+val list start))))))
;
;
;--------------------------------------
;
;
#|
(om::defmethod! reconst-prim ((list list) (start list))
  
  :initvals '(nil (0))
  :icon 128
  :doc "Ricostruisce la lista usando min, max, flex 
             più eventualmente l'indice di profondita"
  
  (let ((ris start)
        prim
        last)
    
    (when (symbolp (car list))
      
      (dolist (y list (reverse ris))
        (cond ((equalp y "min")
               (setf prim '-))
              ((equalp y "max")
               (setf prim '+)))
        
        (if (equalp y "flex")
          (setf ris (append (list (car ris) (car ris)) ris))
          (and (and (setf last (car ris))
                    (push (apply prim (list last 1)) ris))
               
               (setf last (car ris))
               (push (apply prim (list last -1)) ris)
               ))))))
|#
;
;
;--------------------------------------
;
;
(om::defmethod! reconst-prim+prof ((list list))
  
  :initvals '(nil)
  :icon 128
  :doc "Ricostruisce la lista usando min, max, flex 
             più eventualmente l'indice di profondita"
  
  (let ((ris nil)
        (start 0))
    
    (dolist (y list (om::flat 
                     (if (equalp "flex" (first (first list)))
                       (nreverse ris)
                       (om::x-append 
                        start (nreverse ris)))))
      
      (push 
       
       (cond ((equalp (first y) "min")
              (rest (om::x-append (om::arithm-ser  start (* -1 (second y)) -1 )
                                  (rest (om::arithm-ser (* -1 (second y)) start 1)))))
             
             ((equalp (first y) "max")
              (rest (om::x-append (om::arithm-ser start (second y) 1)
                                  (rest (om::arithm-ser (second y) start -1)))))
             
             ((equalp (first y) "flex")
              (om::create-list (second y)  start)))
       ris))))
;
;
;--------------------------------------
;
;

;
;
;--------------------------------------
;
;

(om::defmethod! reconst-prim+prof+val ((list list) (start integer))
  
  :initvals '(nil 6000)
  :icon 128
  :doc "Ricostruisce la lista usando min, max, flex
             più eventualmente l'indice di profondita"
  
  (let ((ris nil))
    
    (dotimes (x (length list) (om::flat (nreverse ris)))
      
      (push 
       (cond ((equalp (first (nth x list)) "max")
              (om::x-append (arithm-ser2 start
                                         (* -1 (/ (- start (second (nth x list)))
                                                  (third (nth x list))))
                                         (third (nth x list)))
                            (arithm-ser2 (second (nth x list))
                                         (/ (- start (second (nth x list)))
                                            (third (nth x list)))
                                         (third (nth x list)))))
             
             ((equalp (first (nth x list)) "min")
              (om::x-append (arithm-ser2 start
                                         (* -1 (/ (- start (second (nth x list)))
                                                  (third (nth x list))))
                                         (third (nth x list)))
                            (arithm-ser2 (second (nth x list))
                                         (/ (- start (second (nth x list)))
                                            (third (nth x list)))
                                         (third (nth x list)))))
             
             ((equalp (first (nth x list)) "flex")
              (om::create-list (third (nth x list))  (second (nth x list))))
             
             )
       ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! pos+prim+prof+val ((list list) (start integer))
  
  :initvals '(nil 6000)
  :icon 128
  :doc "Ricostruisce la lista usando min, max, flex
             più eventualmente l'indice di profondita"
  
  (let ((ris nil)
        (valore nil))
    
    (dotimes (x (length list) 
                (om::flat (om::x-append (nreverse ris)
                                        start)))
      
      (cond ((and (and (equalp (second (nth x list)) "min")
                       (= x 0))
                  (< start (third (nth x list))))
             (format t "~%ERROR !! bad starting point because your min is higher than start!!")
             (abort))
            ((and (and (equalp (second (nth x list)) "max")
                       (= x 0))
                  (> start (third (nth x list))))
             (format t "~%ERROR !! bad starting point because your max is lower than start!!")
             (abort))
            
            ((and (equalp (second (nth x list)) "min")
                  (= x 0))
             (setf valore 
                   (om::x-append (arithm-ser2 start
                                              (* -1.0 (abs (om::om-round (/ (- (third (nth 0 list)) start)
                                                                           (fourth (nth 0 list))))))
                                              (fourth (nth 0 list)))
                                 (third (nth x list))))
             (push valore ris))
            
            ((and (equalp (second (nth x list)) "max")
                  (= x 0))
             (setf valore 
                   (om::x-append (arithm-ser2 start
                                              (* 1.0 (abs (om::om-round (/ (- (third (nth 0 list)) start)
                                                                          (fourth (nth 0 list))))))
                                              (fourth (nth 0 list)))
                                 (third (nth x list))))
             (push valore ris))
            
            ((equalp (second (nth x list)) "min")
             (setf valore 
                   (om::x-append (rest (arithm-ser2 (first (last valore))
                                                    (* -1.0 (abs 
                                                             (om::om-round 
                                                              (/ (- (third (nth x list)) 
                                                                    (third (nth (- x 1) list)))
                                                                 (- (first (nth x list))
                                                                    (first (nth (- x 1) list)))
                                                                 ))))
                                                    (if (equalp (second (nth (- x 1) list)) "flex")
                                                      (- (first (nth x list))
                                                         (+ 
                                                          (fourth (nth (- x 1) list)) 
                                                          (first (nth (- x 1) list))))
                                                      
                                                      (- (first (nth x list))
                                                         (first (nth (- x 1) list))))
                                                    ))
                                 (third (nth x list))))
             (push valore ris))
            
            ((equalp (second (nth x list)) "max")
             (setf valore 
                   (om::x-append (rest (arithm-ser2 (first (last valore))
                                                    (* 1.0 (abs 
                                                            (om::om-round 
                                                             (/ (- (third (nth x list)) 
                                                                   (third (nth (- x 1) list)))
                                                                (- (first (nth x list))
                                                                   (first (nth (- x 1) list)))
                                                                ))))
                                                    (if (equalp (second (nth (- x 1) list)) "flex")
                                                      (- (first (nth x list))
                                                         (+ 
                                                          (fourth (nth (- x 1) list)) 
                                                          (first (nth (- x 1) list))))
                                                      
                                                      (- (first (nth x list))
                                                         (first (nth (- x 1) list))))
                                                    ))
                                 (third (nth x list))))
             (push valore ris))
            
            ((equalp (second (nth x list)) "flex")
             (setf valore (om::create-list (fourth (nth x list)) 
                                           (third (nth x list))))
             (push valore ris))
            ))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Distanze;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! controlla-ottave ((list1 list) (list2 list) (modul integer))
  
  :initvals '(nil nil 12)
  :icon 128
  :doc "studia i casi particolari della melodia in questione se questa
             . trasposta piu o meno esattamente"
  
  
  (let* ((ris nil)
         (ros nil)
         (ottave1 (om::om// (om::om// list1 1200) modul))
         (ottave2 (om::om// (om::om// list2 1200) modul))
         (studio (dotimes (x (length ottave1) (nreverse ris))
                   (push (- (nth x ottave1) (nth x ottave2)) ris))))
    
    
    (dotimes (x (- (length studio) 1) (if (not (equalp
                                                (first list1)
                                                (first list2)))
                                        (+ 1 (apply '+ (nreverse ros)))
                                        (apply '+ (nreverse ros))))
      (if (not (equalp (nth x studio)
                       (nth (+ 1 x) studio)))
        (push 1 ros)))))
;
;
;--------------------------------------
;
;
(om::defmethod! controlla-direzioni ((list1 list) (list2 list)) 
  
  
  :initvals '(nil nil)
  :icon 128
  :doc "Studia gli intervalli di due liste e ne fa una analisi"
  
  (let ((ris nil)
        (direzioni1 (direct-analysis list1))
        (direzioni2 (direct-analysis list2)))
    
    (dotimes (x (length direzioni1) (apply '+ (nreverse ris)))
      (when (not (equalp (nth x direzioni1)
                         (nth x direzioni2)))
        (push 1 ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! controlla-intervalli ((list1 list) (list2 list)) 
  
  
  :initvals '(nil nil)
  :icon 128
  :doc "Studia gli intervalli di due liste e ne fa una analisi"
  
  (let ((ris nil)
        (intervalli1 (om::x->dx list1))
        (intervalli2 (om::x->dx list2)))
    
    (dotimes (x (length intervalli1) (apply '+ (nreverse ris)))
      (when (not (equalp (nth x intervalli1)
                         (nth x intervalli2)))
        (push 1 ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! controlla-primitive ((list1 list) (list2 list))
  
  
  :initvals '(nil nil)
  :icon 128
  :doc "Studia gli intervalli di due liste e ne fa una analisi.
             Se c'e lo stesso numero di min flex e max ma cambia
             la pos nella lista allora c'eè una distanza di 1.
             Se c'e la soppressione di un a primitiva allora c'è 
             una differenza di 2.
             Se c'e l'aggiunta di una primitiva allora c'e una 
             differenza di 2.
             Tutti sono addizionati e danno la distanza globale."
  
  (let* ((ris nil)
         (primitive1 (pos+prim (min-flex-max list1 4)))
         (primitive2 (pos+prim (min-flex-max list2 4)))
         (lungo1 (length primitive1))
         (lungo2 (length primitive2))
         (diverse-lunghezze (* 2 (- (g-max (list lungo1 lungo2))
                                    (g-min (list lungo1 lungo2))))))
    
    (dotimes (x lungo1 (apply '+ (nreverse ris)))
      
      (when (not (equalp lungo1 lungo2))
        (push diverse-lunghezze ris))
      
      (when (and (not (equalp (nth 0 (nth x primitive1))
                              (nth 0 (nth x primitive2))))
                 (equalp (nth 1 (nth x primitive1))
                         (nth 1 (nth x primitive2))))
        (push 1 ris))
      
      (when (not (equalp (nth 1 (nth x primitive1))
                         (nth 1 (nth x primitive2))))
        (push 2 ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! melody-case ((list1 list) (list2 list) &optional (modul 12))
  
  :initvals '(nil nil 12)
  :icon 128
  :doc ""
  
  (when (equalp (length list1)
                (length list2))  
    
    (let ((ottave (controlla-ottave list1 list2 modul))
          (direzioni (controlla-direzioni list1 list2))
          (intervalli (controlla-intervalli list1 list2))
          (primitive (controlla-primitive list1 list2)))
      
      
      (if (equalp (om::om// list1 modul)
                  (om::om// list2 modul))
        
        (apply '+ (list ottave direzioni intervalli primitive))))))
;
;
;--------------------------------------
;
;
(om::defmethod! controlla-rapporti ((list1 list) (list2 list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Verifica se le due liste sono identiche nei rapporti
             interni con una approssimazione di due decimali."
  
  (let* ((ris nil)
         (ros nil)
         (rapporto1 (dotimes (x (- (length list1) 1) (nreverse ris))
                      (push (om::om-round (/ (abs (nth (+ 1 x) list1))
                                            (abs (nth x list1))) 2) ris)))
         (rapporto2 (dotimes (k (- (length list2) 1) (nreverse ros))
                      (push (om::om-round (/ (abs (nth (+ 1 k) list2))
                                            (abs (nth k list2))) 2) ros))))
    
    
    (if (equalp rapporto1 rapporto2)
      'OK 'NO)))
;
;
;--------------------------------------
;
;
(om::defmethod! duration-case ((list1 list) (list2 list))
  
  :initvals '(nil nil)
  :icon 128
  :doc ""
  
  (let ((ris nil))
    
    (when (equalp (length list1)
                  (length list2))
      (if (equalp (controlla-rapporti list1 list2) 'OK)
        
        (dotimes (x (length list1) (apply '+ (nreverse ris)))
          (if (not (equalp (signum (nth x list1))
                           (signum (nth x list2))))
            (push 1 ris)))))))
;
;
;--------------------------------------
;
;                   
(om::defmethod! intensity-case ((list1 list) (list2 list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Verifica se le due liste sono identiche nei rapporti
             interni con una approssimazione di due decimali."
  
  (let* ((ris nil)
         (ros nil)
         (rus nil)
         (rapporto1 (dotimes (x (- (length list1) 1) (nreverse ris))
                      (push (om::om-round (/ (abs (nth (+ 1 x) list1))
                                            (abs (nth x list1))) 2) ris)))
         (rapporto2 (dotimes (k (- (length list2) 1) (nreverse ros))
                      (push (om::om-round (/ (abs (nth (+ 1 k) list2))
                                            (abs (nth k list2))) 2) ros))))
    
    (if (equalp rapporto1 rapporto2)
      (push 1 rus))))
;
;
;--------------------------------------
;
;
(defun mini (l)
"Returns the minimum value of a list"
      (car (sort l '<)))

;
;
;--------------------------------------
;
;
(om::defmethod! dist-1-ldl ((seq1 list) (seq2 list) (change t) (ins/sup t)
                            (wgth list))
  
  :initvals '(nil nil 1 1 (1 1 1 1) )
  :icon 128
  :doc ""
  
  (cond ((not (equalp (length (car seq1)) (length (car seq2))))
         (format t "~%ERROR !! different number of parameters in the two lists !!")
         (abort)))
  (let* ((ris 0)
         (matrix1 (om::mat-trans seq1))
         (matrix2 (om::mat-trans seq2))
         (wgth (cond ((not (equalp (length wgth) (length (car seq1))))
                      (format t "~%WARNING : bad definition of wgth; setting all weigths to the first of wgth list...~%Look at the documentation.")
                      (make-list (length (car seq1)) :initial-element (car wgth)))
                     (t
                      wgth))))
    (setf wgth (om::g-scaling/sum wgth 1.0))
    (dotimes (x (length matrix1) (sqrt ris))
      (setf ris
            (+ ris
               ;;;;;revoir la ponderation
               (* (nth x wgth) (expt (dist-1 (nth x matrix1) (nth x matrix2) change ins/sup 1)
                                     2)))))))
;
;
;--------------------------------------
;
;
(om::defmethod! dist-2-ldl ((seq1 list) (seq2 list) (change number) (ins/sup number)
                            (inex number) (wgth list))
  
  :initvals '(nil nil 1 1 0 (1 1 1 1) )
  :icon 128
  :doc ""
  
  (cond ((not (equalp (length (car seq1)) (length (car seq2))))
         (format t "~%ERROR !! different number of parameters in the two lists !!")
         (abort)))
  (let* ((ris 0)
         (matrix1 (om::mat-trans seq1))
         (matrix2 (om::mat-trans seq2))
         (wgth (cond ((not (equalp (length wgth) (length (car seq1))))
                      (format t "~%WARNING : bad definition of wgth; setting all weigths to the first of wgth list...~%Look at the documentation.")
                      (make-list (length (car seq1)) :initial-element (car wgth)))
                     (t
                      wgth))))
    (setf wgth (om::g-scaling/sum wgth 1.0))
    (dotimes (x (length matrix1) (sqrt ris))
      (setf ris
            (+ ris
               ;;;;;revoir la ponderation
               (* (nth x wgth) (expt (dist-2 (nth x matrix1) (nth x matrix2) change ins/sup inex 1)
                                     2)))))))
;
;
;--------------------------------------
;
;
(om::defmethod! dist-1 ((seq1 t) (seq2 t) (change number)
                        (ins/sup number) scale)
  
  :menuins '( (4 (("relative" 1)
                 ("absolute" 2))))
  :initvals '(nil nil 1 1 1)
  :icon 128
  :doc "Returns the smallest distance between two lists of symbols seq1 and seq2
             Args :
            change = cost when changing a symbol in a list without deletion or insertion
            ins/sup = cost for a deletion or insertion
             &optional = scale of the distance, absolute or relative.
             The relative distance is expressed in percent of the longer list"
  
  (let ((matcouts()) d d1 d2 d3 c c1)
    
    (dotimes (j (+ (length seq2) 1))
      (dotimes (i (+ (length seq1) 1))
        (setf d (+ i (* j (+ (length seq1) 1))))
        ;--- SI i et j differents de 0 => cas d'un changement ---
        (cond ((and (> i 0) (> j 0))
               ;;calcul du cout
               (if (eq (nth (- i 1) seq1) (nth (- j 1) seq2))
                 (setf c1 0)
                 (setf c1 change))
               ;;calcul de D(i,j)
               (setf d1 (nth (+ 1 (length seq1)) matcouts))
               (setf d2 (nth (length seq1) matcouts))
               (setf d3 (nth 0 matcouts))
               ;;calcul de cout mini
               (setf c (mini
                        (list
                         (+ c1 d1) (+ ins/sup d2) (+ ins/sup d3)
                         )
                        ))
               (push c matcouts))
              ;--- SI i ou j = 0  => cas d'une suppression ou insertion 
              ;   (= changement par element neutre) ---
              (t
               ;;calcul du cout
               (if (and (eq i 0) (eq j 0))
                 (setf c1 0)
                 (setf c1 ins/sup))
               ;;si i = 0 et j> 0
               (cond ((and (eq i 0) (> j 0))
                      ;calcul de D(i,j)
                      (setf d (nth (length seq1) matcouts))
                      (setf c (+ c1 d))
                      (push c matcouts))
                     ;;si i > 0 et j = 0
                     ((and (> i 0) (eq j 0))
                      ;calcul de D(i,j)
                      (setf d (nth 0 matcouts))
                      (setf c (+ c1 d))
                      (push c matcouts))
                     (t (push '0 matcouts) (setf c '0)))
               )
              )
        ))
    (cond ((or (eq scale 2) (eq scale nil))
           (car matcouts))
          ((eq scale 1)
           (cond ((>= (length seq1) (length seq2))
                  (* 100 (float (/ (car matcouts) (length seq1)))))
                 (t
                  (* 100 (float (/ (car matcouts) (length seq2)))))))
          (t
           (format t "~% ERROR : argument scale: ~S does not exist" scale)
           (format t "~% try arguments 'relative or 'absolute")))))
;
;
;--------------------------------------
;
;
(om::defmethod! dist-2 ((seq1 list) (seq2 list) (change number)
                        (ins/sup number) (inex number) scale)
  
  :menuins '( (5 (("relative" 1)
                 ("absolute" 2))))
  :initvals '(nil nil 1 1 0 1)
  :icon 128
  :doc "Returns the smallest distance between two lists of symbols seq1 and seq2
             Args :
             change = cost when changing a symbol in a list without deletion or insertion
             ins/sup = cost for a deletion or insertion
             inex = added cost if the change is applied on an element which doesn't exist on 
             the other list
             &optional = scale of the distance, absolute or relative.
             The relative distance is expressed in percent of the longer list"
  
  (let ((matcouts()) d d1 d2 d3 c c1 ex ey)
    
    (dotimes (j (+ (length seq2) 1))
      (dotimes (i (+ (length seq1) 1))
        (setf d (+ i (* j (+ (length seq1) 1))))
        ;;;--- SI i et j differents de 0 ---
        (cond ((and (> i 0) (> j 0))
               ;;test si ai et bj elements (respectivement) des chaines n et m
               (if (eq (member (nth (- i 1) seq1) seq2) nil)
                 (setf ex 1) (setf ex 0))
               (if (eq (member (nth (- j 1) seq2) seq1) nil)
                 (setf ey 1) (setf ey 0))
               ;;calcul du cout
               (if (eq (nth (- i 1) seq1) (nth (- j 1) seq2))
                 (setf c1 0)
                 (setf c1 (+ change (* (+ ex ey) inex))))
               ;;calcul de D(i,j)
               (setf d1 (nth (+ 1 (length seq1)) matcouts))
               (setf d2 (nth (length seq1) matcouts))
               (setf d3 (nth 0 matcouts))
               ;;calcul de cout mini
               (setf c (mini
                        (list
                         (+ c1 d1) (+ ins/sup (* ey inex) d2) (+ ins/sup (* ex inex) d3)
                         )))
               (push c matcouts))
              ;;;--- SI i ou j = 0 ---
              (t
               ;;calcul du cout
               (if (and (eq i 0) (eq j 0))
                 (setf c1 0)
                 (setf c1 ins/sup))
               ;;si i = 0 et j> 0
               (cond ((and (eq i 0) (> j 0))
                      ;;test si bj element de seq1
                      (if (eq (member (nth (- j 1) seq2) seq1) nil)
                        (setf ey 1) (setf ey 0))
                      ;calcul de D(i,j)
                      (setf d (nth (length seq1) matcouts))
                      (setf c (+ c1 d (* ey inex)))
                      (push c matcouts))
                     ;;si i > 0 et j = 0
                     ((and (> i 0) (eq j 0))
                      ;;test si ai element de seq2
                      (if (eq (member (nth (- i 1) seq1) seq2) nil)
                        (setf ex 1) (setf ex 0))
                      ;calcul de D(i,j)
                      (setf d (nth 0 matcouts))
                      (setf c (+ c1 d (* ex inex)))
                      (push c matcouts))
                     (t (push '0 matcouts) (setf c '0)))))
        ))
    (cond ((or (eq scale 2) (eq scale nil))
           (car matcouts))
          ((eq scale 1)
           (cond ((>= (length seq1) (length seq2))
                  (* 100 (float (/ (* (car matcouts) (+ ins/sup inex)) (length seq1)))))
                 (t
                  (* 100 (float (/ (* (car matcouts) (+ ins/sup inex)) (length seq2)))))))
          (t
           (format t "~% ERROR : argument dist: ~S does not exist" scale)
           (format t "~% try arguments : 'relative or 'absolute")))))
;
;
;--------------------------------------
;
;
(om::defmethod! distance ((seq1 list) (seq2 list) (change number)
                          (ins/sup number) scale  &optional inex)
  
  :menuins '( (4 (("relative" 1)
                 ("absolute" 2))))
  :initvals '((a b c d e) (a b c d e) 1 1 1 nil)
  :icon 128
  :doc "Returns the smallest distance between two lists of symbols seq1 and seq2
             Args :
             change = cost when changing a symbol in a list without deletion or insertion
             ins/sup = cost for a deletion or insertion
             inex = added cost if the change is applied on an element which doesn't exist on 
             the other list
             &optional = scale of the distance, absolute or relative.
             The relative distance is expressed in percent of the longer list"
  
   
    (if inex 
      (dist-2 seq1 seq2 change ins/sup inex scale) 
      (dist-1 seq1 seq2 change ins/sup scale)))
;
;
;--------------------------------------
;
;
(om::defmethod! multi-distance ((seq1 list) (seq2 list) (change number) (ins/sup number)
                                (wgth list ) &optional inex )
  
  :initvals '(nil nil 1 1 (1 1 1 1) nil)
  :icon 128
  :doc ""    
    (if inex (dist-2-ldl seq1 seq2 change ins/sup inex wgth)
         (dist-1-ldl seq1 seq2 change ins/sup wgth)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Opzioni varie;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Derivation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun med-fix (lista)

            "Restitusce la derivata data dalla media tra una nota e la successiva."

  (let ((ris nil))

    (dotimes (x (- (length lista) 1) (nreverse ris))
      (push (/ (+ (nth x lista) (nth (1+ x) lista)) 
               2) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! mean-derivation ((list list) 
                                 (gr° integer)
                                 &optional (note? nil))
  
  :initvals '(nil 0 nil)
  :icon 128
  :doc ""
  
  (let* ((calcolo (if (= 1 gr°) (med-fix list)
                      (mean-derivation (med-fix list) (- gr° 1)  note?)))
         (con-note (when note?
                     (notes-change calcolo note? 48))))
    
    (if note? con-note calcolo)))
;
;
;--------------------------------------
;
;
(om::defmethod! der ((lista list) (n integer))
  
  :initvals '(nil 0)
  :icon 128
  :doc "Crea la media tra una lista di valori diviso n"
  
  (let ((ris nil)
        ;(n (om::list! n))
        )
    
    (dotimes (x (length (scom lista n)) (nreverse ris))
      (push (remove nil (nth x (scom lista n))) ris))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Media con indice variabile;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! med-var ((lista list) (windw integer))
  
  :initvals '(nil 1)
  :icon 128
  :doc "Restituisce la derivata data dalla media delle note decise in n."
  
  (let ((ris nil)
        (calcolo (der lista windw)))
    
    
    
    (dotimes (x (- (length calcolo) 1) (nreverse ris))
      (push (/ (apply '+ (nth x calcolo))
               (length (nth x calcolo))) ris))))

;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Medie successive variabili;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! variable-derivation ((lista list) 
                                     (windw integer)
                                     (gr° integer))
  
  :initvals '(nil 2 1)
  :icon 128
  :doc "Restituisce le dirivate variabili successive secondo il valore
             scelto in GRADO. N rappresenta il valore con cui effettuare la media."
  
  (if (= 1 gr°) (med-var lista windw)
      (variable-derivation (med-var lista windw) (- gr° 1) windw)))
;
;
;--------------------------------------
;
;changes by carlos
(om::defmethod! notes-change  ((pits integer) (scale integer) 
                               &optional (mod 12))
  
  :initvals '(6000 6000 12)
  :icon 128
  :doc "Cambia un p^rofilo con le note messe in scale."
  
  (let* ((pits (om::list! pits))
         (scale (om::list! scale))
         (modsca (om::om// (om::sort-list (om::remove-duplicates 
                                              (om::om// (om::om/ scale 
                                                                 (/ 100 (/ mod 12))) mod)))))
         (pitmods (om::om// (om::om/ pits (/ 100 (/ mod 12))) mod))
         (octa (octave pits))
         (posdifs (mapcar #'(lambda (p) (position (g-min (om::om-abs (om::om- modsca p)))
                                                  (om::om-abs (om::om- modsca p))))
                          pitmods)))
    (mapcar #'(lambda (index octave) (makenote  index octave mod))
            (om::posn-match modsca posdifs)
            octa)))
;
;
;--------------------------------------
;
;
(om::defmethod! octave ((midic integer))
  
  :initvals '(6000)
  :icon 128
  :doc "retourne l'octave a partir de c3=octave 3"
  
  (let ((midic (om::list! midic)))
    (mapcar #'(lambda (x) (om::om- (om::om// x 1200) 2) ) midic)))
;
;
;--------------------------------------
;
;
(om::defmethod! makenote ((index integer) (octave integer) 
                          &optional (mod 12))
  
  :initvals '(60 3 12)
  :icon 128
  :doc " construction d'une note a partir des donnees 
             de index, octave e modulo du index"
  
  (+ (/ (* index 100 12) mod) (* (+ 2 octave) 1200)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Integrazione;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun lettura-modulare (lista1 lista2)

            "Se la prima lista e piu grande della seconda lista, allora legge 
             modularmente la seconda lista restituendo un length uguale al length
             di lista1."

  (let ((ros nil))
    (dotimes (y (length lista1) (nreverse ros))
      (push 
       (if (< (length lista2) (length lista1))
         (nth 
          (mod y (length lista2))
          lista2)
         (nth y lista2)) ros))))
;
;
;--------------------------------------
;
;
(om::defmethod! inter-profile ((list1 list) (list2 list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Prepara interlock : non mi ricordo cosa fa esattamente."
  
  (let ((ris nil)
        (y (lettura-modulare list1 list2)))
    
    (om::flat 
     (append 
      (dotimes (x (1- (length list1)) (nreverse ris))
        (push
         (om::mat-trans (list (list (nth x list1))
                              (list (trans-approx (list (nth x y))
                                                  (list (nth x list1) 
                                                        (nth (1+ x) list1)))))) ris))
      (last list1)))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;Interposizione di due liste completa;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! prof-inter ((list1 list) (list2 list)  total)
  
  :menuins '( (1 (("ltd" 1) 
                 ("copl" 2))))
  :initvals '(nil nil 1)
  :icon 128
  :doc "Restituisce l'interposizione di list1 con list2. Se list1 e piu piccola
             di list2 allora la funzione crea un'interposizione di n elementi di list2
             dove (= n (- (length list1) 1)). In questo caso si puo' decidere con il 
             menù se avere la prima ricorsione per completare list2."
  
  (case total
    (1 (inter-profile list1 list2))
    (2 (inter-profile (inter-profile list1 list2)
                      (permut-circ list2 (1- (length list1)))))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;Interposizione di due liste ricorsiva;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! interlock ((list1 list) (list2 list) (gr° integer)) 
  
  :initvals '(nil nil 1)
  :icon 128
  :doc "Interpone una lista2 alla lista1 e prende 
             aleatoriamente le note della lista2"
  
  (if (= gr° 1) (inter-profile list1 (om::permut-random list2))
      (interlock (inter-profile list1 (om::permut-random list2))
                 (permut-circ list2 (1- (length list1)))
                 (- gr° 1))))
;
;
;--------------------------------------
;
;
(om::defmethod! new-inter-profile ((list1 list) (list2 list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Prepara interlock : non mi ricordo cosa fa esattamente."
  
  (let ((ris nil)
        (y (lettura-modulare list1 list2)))
    
    (om::flat 
     (append 
      (dotimes (x (1- (length list1)) (nreverse ris))
        (push
         (om::mat-trans (list (list (nth x list1))
                              (list 
                               (om::om+ (om::nth-random (list 1200 0 -1200))
                                       (trans-approx (list (nth x y))
                                                     (list (nth x list1) 
                                                           (nth (1+ x) list1))))
                               ))) ris))
      (last list1)))))
;
;
;--------------------------------------
;
;
(om::defmethod! new-interlock ((list1 list) (list2 list) (gr° integer))
  
  :initvals '(nil nil 1)
  :icon 128
  :doc  "Interpone una lista2 alla lista1 e prende 
             aleatoriamente le note della lista2"
  
  (if (= gr° 1) (new-inter-profile list1 (om::permut-random list2))
      (new-interlock (new-inter-profile list1 (om::permut-random list2))
                     (permut-circ list2 (1- (length list1)))
                     (- gr° 1))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Trasposizione controllata;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun int-com-ottava (lista)
  
  "Restituisce l'intervallo complementare ad ull'intervallo in 'lista'
   ma all'interno di un'ottava."
  
  
  (let ((ris nil))
    
    (om::flat
     (dotimes (x (- (length lista) 1) (nreverse ris))
       (push 
        (om::x->dx 
         (append
          (list (nth x lista))
          (list  
           (- (nth x lista)
              (* (- 12 (mod 
                        (/ (- (first (om::x->dx lista)) 
                              (* 
                               (first 
                                (om::om// (om::x->dx lista) 1200)) 1200)) 100) 12)) 100)))))
        ris)))))
;
;
;--------------------------------------
;
;
(defun mio-transpoct (list range)

            "Restituisce lo stesso risultato di 'transpoct' della libreria Esquisse"

  (let ((ris nil))

    (dolist (y list (nreverse ris))
      (push 
       (cond ((< y (g-min range))
              (+ (g-min range)
                 (+ 1200 (first (int-com-ottava (list (g-min range) y))))))
             ((> y (g-max range))
              (+ 
               (g-max range) 
               (first (int-com-ottava (list (g-max range) y)))))
             (y))
       ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! correttore ((elmt integer) (range list))
  
  :initvals '(1 nil)
  :icon 128
  :doc "Restituisce un elemento se questo compare all'interno del range.
           Se l'elemento è escluso allora lo traspone in modo tale che sia
           il piu vicino possibile o al limite superiore o a quello inferiore.
           Se il limite è DO-SOL allora Mi viene incluso, SI viene trasposto
           sotto il DO e il SOL# viene trasposto sopra il SOL."
  
  (let ((max (g-max range))
        (min (g-min range)))
    (cond ((<= (g-min range) elmt max) 
           elmt)
          ((cond ((< elmt min)
                  (cond ((<= (- min elmt) (- (+ 1200 elmt) max))
                         elmt)
                        ((> (- min elmt) (- (+ 1200 elmt) max))
                         (+ 1200 elmt))))
                 ((> elmt max)
                  (cond ((<= (- elmt max) (- min (- elmt 1200)))
                         elmt)
                        ((> (- elmt max) (- min (- elmt 1200)))
                         (- elmt 1200)))))))))
;
;
;--------------------------------------
;
;
(defun cor-ott-list (elmt range)
        
          "Restituisce un elemento se questo compare all'interno del range.
           Se l'elemento è escluso allora lo traspone in modo tale che sia
           il più vicino possibile o al limite superiore o a quello inferiore.
           Se il limite è DO-SOL allora Mi viene incluso, SI viene trasposto
           sotto il DO e il SOL# viene trasposto sopra il SOL.La differenza
           con 'CORRETTORE' è che questo modulo agisce su una lista intera."
        
        (let ((ris nil))
          (dolist (y elmt)
            (push (correttore y range) ris))
          (nreverse ris)))
;
;
;--------------------------------------
;
;
(om::defmethod! trans-approx ((list list) (range list)) 
  
  :initvals '(nil nil)
  :icon 128
  :doc "E' meglio di transpoct di Esquisse. Infatti attua lo stesso
             procedimento ma traspone una nota non inclusa nel range il più
             vicino o al limite superiore o a quello inferiore."
  
  (cor-ott-list (mio-transpoct list range) range))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Analisi-Profilo melodia (1,0, -1);;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! direct-analysis ((list list))
  
  :initvals '((6000 4000 5600 4700 4100 5900 6400 7800 
               7400 6300 5500 5200 4900 6400 6800 8300 
               8000 6400 6100 6100 6100 6100 6100 6100 
               4600 4700 4500 5300 5400 5600 5600 5600 
               5600 5500 5500 6900 6900 7300 6100 5900))
  :icon 128
  :doc "Analizza il profilo di una linea e restituisce 1 per ogni
             valore crescente e -1 per ogni valore decrescente."
  
  (let ((ris nil))
    
    (dotimes (x (length (om::x->dx list)) (nreverse ris))
      
      (cond ((> (nth x (om::x->dx list)) 0)
             (push '1 ris))
            ((< (nth x (om::x->dx list)) 0)
             (push '-1 ris))
            ((= (nth x (om::x->dx list)) 0)
             (push '0 ris))))))

;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Modulo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun int-com (lista)
  
  "Restituisce l'intervallo complementare di un intervallo dato in funzione della
   prima nota dell'intervallo stesso. Questo significa che se ho SOL3 DO4, la 
   funzione restituisce do4 sol4."
  
  (let ((ris nil))
    
    (om::flat
     (dotimes (x (- (length lista) 1) (nreverse ris))
       (push 
        (om::x->dx (append
                    (list (nth x lista))
                    (list (-  (- (nth x lista)
                                 (*  (- 12 (mod 
                                            (/ (- (first (om::x->dx lista)) 
                                                  (* 
                                                   (first 
                                                    (om::om// (om::x->dx lista) 1200)) 1200)) 100) 12)) 100))
                              (* (first (om::om// (om::x->dx lista) 1200)) 1200)))))
        ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! malt-mod+ ((list list) (limit integer))
  
  :initvals '(nil 6000)
  :icon 128
  :doc  ""
  
  (let ((ris nil)
        (limite (first (om::list! limit))))
    
    (dolist (y list (nreverse ris))
      (push (if (< y limite)
              (- (* 2 limite) y) y) ris))))

;
;
;--------------------------------------
;
;
(defun interno (elmt range)

          "Restituisce l'elemento se è incluso nel 'range' e nil 
           se non e incluso."

  (if (<= (g-min range) elmt (g-max range)) elmt nil))
;
;
;--------------------------------------
;
;
(om::defmethod! malt-mod- ((list list) (limit integer))
  
  :initvals '(nil 6000)
  :icon 128
  :doc   ""
  
  (let ((ris nil)
        (limite (first (om::list! limit))))
    
    (dolist (y list (nreverse ris))
      (push (if (> y limite)
              (- (* 2 limite) y) y) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! reflex-int ((ls list) (value number) up/down menu)
  
  :menuins '( (2 (("up" 1)
                 ("down" 2))))
  :initvals '(nil 0 1)
  :icon 128
  :doc  "Restituisce la rifleesione delle note che sono superiori o inferiori
             al valore indicato con 'value'. Il menù permette di selezionare se si
             vuole una riflessione superiore o inferiore"
  (case up/down
    (1 (malt-mod+ ls value))
    (2 (malt-mod- ls value))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Modulo con altezze fisse;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun mod-fix- (ls asse)
  ""
  (let ((ris nil)
        (asse (om::list! asse)))
    (dotimes (x (length ls) (nreverse ris))
      (push
       (if (<= (nth x ls) (first asse)) (nth x ls)
           (+ (first asse) (first (int-com (list
                                            (first asse)
                                            (nth x ls)))))) 
       ris))))
;
;
;--------------------------------------
;
;
(defun mod-fix+ (ls asse)

  ""
  (let ((ris nil)
        (asse (om::list! asse)))
    (dotimes (x (length ls) (nreverse ris))
      (push
       (if (>= (nth x ls) (first asse)) (nth x ls)
           (+ (first asse) (first (int-com (list
                                            (first asse)
                                            (nth x ls)))))) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! reflex-note ((ls list) (value number) up/down menu)
  
  :menuins '( (2 (("up" 1)
                 ("down" 2))))
  :initvals '(nil 0 1)
  :icon 128
  :doc  "Restituisce per la riflessione superiore con <UP> e quella
             inferiore con <DOWN>."
  
  (case up/down
    (1 (mod-fix+ ls value))
    (2 (mod-fix- ls value))))
;
;
;--------------------------------------
;
;
(om::defmethod! doppio-reflex-note ((list list)
                                    (value list))
  
  :initvals '(nil nil)
  :icon 128
  :doc  "Restituisce due volte <REFLEX-NOTE> la prima volta a <LIST>
             la seconda volta al risultato della prima volta."
  
  (reflex-note (reflex-note list (g-min value) 1) 
               (g-max value) 2))
;
;
;--------------------------------------
;
;
(om::defmethod! doppio-reflex-int ((list list)
                                   (value list))
  
  :initvals '(nil nil)
  :icon 128
  :doc  "Restituisce due volte <REFLEX-INT> la prima volta a <LIST>
             la seconda volta al risultato della prima volta."
  
  
  (reflex-int (reflex-int list (g-min value) 1)
              (g-max value) 2))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Filtra banda;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun int (elt coppia)
  (if (< (first coppia) elt (second coppia)) elt nil))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Filtro passa banda;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! pass-band ((lista list) (alt list))
  
  :initvals '(nil nil)
  :icon 128
  :doc  "Restituisce i valori inclusi in ALT."
  
  (let ((ris nil))
    
    (dolist (x lista (nreverse ris))
      (if (equalp (int x alt) nil) (int x alt) (push x ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! correttore-doppio-reflex-note ((list list)
                                               (value list) inclu? )
  
  :menuins '( (2 (("yes" 1)
                 ("no!" 2))))
  :initvals '(nil nil 1)
  :icon 128
  :doc  "Corregge il risultato di 'DOPPIO-REFLEX-NOTE' in modo che se la 
             riflessione supera i limiti con <YES> abbiamo una trasposizione
             oltre i limiti stessi ma con TRANS-APPROX altrimenti le note 
             che non sono incluse nei limiti vengono escluse dalla funzione
             COMP-OCTAVE."
  
  (let ((risultato (doppio-reflex-note list value )))
    
    (case inclu?
      (1 (trans-approx risultato value))
      (2 (comp-octave risultato value)))))  
;
;
;--------------------------------------
;
;
(om::defmethod! correttore-doppio-reflex-int ((list list)
                                              (value list)) 
  
  :initvals '(nil nil)
  :icon 128
  :doc  "Corregge il risultato di 'DOPPIO-REFLEX-INT' in modo che 
             se il risultato di 'DOPPIO-REFLEX-INT supera i limiti dati
             ripete l'operazione di adattamento fino a che non soddisfa 
             i limiti di esistenza."
  
  (let ((risultato (doppio-reflex-int list value))
        (ris nil))
    
    
    (dolist (y risultato (om::flat (nreverse ris)))
      (push (if (int y value) y
                (correttore-doppio-reflex-int (om::list! (1+ y)) value)) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! reflexion ((list list) 
                           (axis integer)
                           mode? up/down)
  
  :menuins '( (2 (("intrv" 1)
                 ("note" 2)))
             (3 (("up" 1)
                  ("down" 2))))
  :initvals '(nil 6000 1 1)
  :icon 128
  :doc  ""
  
  (funcall (case mode?
             (1 'reflex-int)
             (2 'reflex-note))
           list axis up/down))                     
;
;
;--------------------------------------
;
;
(om::defmethod! double-reflect ((list list) (limits list) mode? inclu? )
  
  :menuins '( (2 (("intrv" 1)
                 ("note" 2)))
             (3 (("up" 1)
                 ("down" 2))))
  :initvals '(nil 6000 1 1)
  :icon 128
  :doc  ""
  
  (case mode?
    (1 (correttore-doppio-reflex-int list limits))
    (2 (correttore-doppio-reflex-note list limits inclu?))))
;
;
;--------------------------------------
;
;
(om::defmethod! comp-octave ((list list) (range list))
  
  :initvals '(nil nil)
  :icon 128
  :doc  "Restituisce una trasposizione della lista mantenendo le altezze
             assolute all'interno del 'range. Se un elemento non è incluso 
             nel 'range', allora viene tolto dal risultato."
  
  (let ((ris nil))
    
    (dolist (y (mio-transpoct list range) (nreverse ris))
      (if (equalp (interno y range) nil) (interno y range) (push y ris)))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Opzioni ritmiche;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! rtm-change ((rhyt list) (modulo list) mode?) 
  
  :menuins '( (2 (("mod" 1)
                 ("ptrn" 2))))
  :initvals '(nil nil 1)
  :icon 128
  :doc "E' la funzione che cambia un ritmo in funzione del menu <mode?>
             Se <mode?> è su mod, questa funzione restituisce i multipli 
             dei valori in <moduli>; se è su ptrn allora retituisce una 
             struttura ritmica che utlilizza solamente i valori in <modulo>"
  
  (let ((modulo (om::list! modulo)))
    
    (case mode?
      (1 (substitute (g-min modulo)
                     0.0 (usa-quel-modulo rhyt modulo)))
      (2 (rtm-change-1 rhyt modulo)))))
;
;
;--------------------------------------
;
;
(om::defmethod! rtm-change-1 ((rhytm list) (vals list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Se in vals c'e un solo valore allora calcola una 
             approssimazione ritmica in modo tale che tutti i valori 
             risultino un multiplo di vals. Se invece in vals c'è una 
             lista di valori allora approssima tutti i valori in rtm
             con i valori di vals."
  
  (let ((vals (om::list! vals)))
    
    
    (vicini-valori vals rhytm)))
;
;
;--------------------------------------
;
;
(om::defmethod! distanza-modulo ((list list) (modulo list)) 
  
  :initvals '(nil nil)
  :icon 128
  :doc "Caclola per ogni elemento della lista list il modulo
             corrispondente per ogni elemento della lista Modulo e
             li raggruppa in sotto-liste."
  
  (let ((ris nil))
    
    (dolist (y list (om::list-explode (nreverse ris) (length list)))
      (dolist (x modulo)
        (push (mod y x) ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! usa-quel-modulo ((list list) (moduli list)) 
  
  :initvals '(nil nil)
  :icon 128
  :doc "Restituisce i valori che sono tutti multupli dei moduli
             messi in Moduli."
  
  (let ((ris nil)
        (calcolo (distanza-modulo list (om::om-abs moduli))))
    
    (dotimes (x (length list) (nreverse ris))
      
      (if (om::included? (list 0) (om::list! (nth x calcolo)))
        (push (nth x list) ris)
        (push (- (nth x list)
                 (g-min (nth x calcolo))) ris)))))
;
;
;--------------------------------------
;
;

(om::defmethod! rtm-change-1val ((rht list) (val integer)) 
  
  :initvals '(nil 1)
  :icon 128
  :doc "Questa funzione prende ogni elemento di rht e restituisce
             lo stesso elemento se il (mod rht val) è uguale a 0
             altrimenti lo approssima al poù vicino"
  
  (let ((ris nil))
    
    (dolist (y rht (nreverse ris))
      (if (= (om::om// y val) 0)
        (push y ris)
        (push (- y (om::om// y val)) ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! tutti-int ((list list) (ref number)) 
  :initvals '(nil 1)
  :icon 128
  :doc "Calcola gli intervalli che ci sono fra una lista di note ed
             un'unica nota di riferimento."
  
  (om::flat
   (let ((ris nil))
     
     (dolist (y list (nreverse ris))
       (push (om::x->dx (list ref y)) ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! segno+picc ((list list))
  :initvals '(nil)
  :icon 128
  :doc "Trasforma tutta la lista in valori tutti positivi e prende il valore
             piu piccolo."
  
  (g-min (mapcar #' (lambda (x) (abs x)) list)))
;
;
;--------------------------------------
;
;
(om::defmethod! nota-vicina ((list list) (ref number)) 
  :initvals '(nil 1)
  :icon 128
  :doc "Prende l'intervallo più piccolo di una lista."
  
  (let* ((intervalli (tutti-int list ref))
         (piccolo (segno+picc intervalli)))
    
    
    (if
      (equalp (abs (first intervalli)) piccolo)
      (first intervalli)
      (nota-vicina (rest list) ref))))
;
;
;--------------------------------------
;
;
(om::defmethod! tieni-nota ((list list) (ref number)) 
  :initvals '(nil 1)
  :icon 128
  :doc "tiene la nota piu vicina."
  
  (om::om+ ref (nota-vicina list ref)))
;
;
;--------------------------------------
;
;
(om::defmethod! vicini-valori ((list1 list) (refs list))
  
  :initvals '(nil nil)
  :icon 128
  :doc "Prende le note piu vicine di list per ogni nota di refs."
  
  (mapcar #' (lambda (x) (tieni-nota list1 x)) refs))
;
;
;--------------------------------------
;
;
(om::defmethod! arithm-ser2 ((begin number) (step number) (xval number))
  :initvals '(0 1 5)
  :icon 128
  :doc "Returns a list of <xval> numbers starting from <begin> with <step>."
  (algeb begin xval step))
;
;
;--------------------------------------
;
;
(defun algeb (init n pas)
  (if (= n 0)()
      (cons init (algeb (+ pas init) (1- n) pas))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;      Interface avec AUDIOSCULPT          ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun markers-AS ()
  (let ((r ()))
    (with-open-file (input-stream (om::om-choose-file-dialog 
                                   :button-string "markers")
                                  :direction :input)
      (push (read input-stream) r))
    (cddar (reverse r))))

(om::defmethod! f0-additive ((step integer) &optional (range '(10 4000)))
  :initvals '(1 (10 4000))
  :icon 128
  :doc "Reads f0 additive analysis.
step = reads each values at step window (default = 1 (all);
range = returns only values (date and frequency)
         if in the range specified by the list (10 4000)."
  (let ((dates ()) (f0 ()) f time
        (file 
         (om::om-choose-file-dialog :button-string "date-f0")))
    (with-open-file (input-stream file :direction :input
                                  :element-type 'character)
      (loop with length = (file-length input-stream)
            for i from 0 to (1- length)
            while (< (file-position input-stream) (1- length))
            do
            (cond ((= 0 (- (/ i step) (round (/ i step))))
                   (setf f (read input-stream))
                   (setf time (read input-stream))
                   (when (not (equal (pos-int-dom (list range) f) 'nil))
                     (push f dates)
                     (push time f0)))
                  (t (dotimes (g 2) (read input-stream)))))                   
      (om::mat-trans (list (reverse dates) (reverse f0))))))

(om::defmethod! pi-dur ((dates list) (pitches list) (min number)  unit) 
  :menuins '( (3 (("sec" 1)
                 (".001" 1000)
                 ("1/100"  10))))
  :initvals '(nil nil 0 1)
  :icon 128
  :doc  "Calculates de durations of the pitches according to a change in pitch"
  (let ((durations ())
        (start-note 0)
        (dur-t 0))
    (loop for n from 1 to (length pitches)
          do
          ;(print (cons dur-t (/ min unit)))
          (cond ((and
                  (or
                   (= n (length pitches))
                   (not (equal (nth n pitches) (nth (1- n) pitches))))
                  (>= dur-t (/ min unit)))
                 (if (= unit 1000)
                   (push
                    (list
                     (nth (1- n) pitches) (round (* unit start-note)) (round (* unit dur-t)))
                    durations)
                   (push
                    (list
                     (nth (1- n) pitches) (* unit start-note) (* unit dur-t))
                    durations))
                 (setf dur-t 0)
                 (setf start-note (nth n dates)))
                (t
                 (setf dur-t (+ dur-t (- (nth n dates) (nth (1- n) dates)))))))
    (reverse durations)))

(defun absol (list)
  (mapcar #'abs list))

(defun moyenne (l)
  (/ (apply #'+ l) (length l)))

(defun aver-window2 (list w)
  (let ((wind ()) (r ()))
    (when (oddp w)
      (format t "ERROR : window size must be even~&")
      (abort))
    (dotimes (i (1- (floor (/ (* 2 (length list)) w))) (nreverse r))
      (setf wind 'nil)
      (dotimes (n w (push (list (/ (* w i) 2) (* 1.0 (moyenne wind))) r))
        (push (nth (+ (/ (* w i) 2) n) list) wind)))))

(defun aver-window (list w)
  (let ((wind ()) (r ()))
    (when (oddp w)
      (format t "ERROR : window size must be even~&")
      (abort))
    (dotimes (i (1- (floor (/ (* 2 (length list)) w))) (nreverse r))
      (setf wind 'nil)
      (dotimes (n w (push (* 1.0 (moyenne wind)) r))
        (push (nth (+ (/ (* w i) 2) n) list) wind)))))

(defun trunc (list start end)
  (when (not (= 0 end))
    (setf list (delete-if #'numberp list :start (- (length list) end))))
  (when (not (= 0 start))
    (setf list (delete-if #'numberp list :start 0 :end (1- start))))
  list)

(defun interpol-lin (wlist w)
"interpolation lineaire entre les points contigus de list par w points"
  (let ((r ()) d)
    (dotimes (n (- (length wlist) 1)  (append (nreverse r) (last wlist)))
      (setf d (- (nth (1+ n) wlist)  (nth n wlist)))
      (dotimes (i (/ w 2))
        (push (* 1.0 (+ (* i (/ d w)) (nth n wlist))) r)))))

(defun no-interpol (wlist w)
  (let ((r ()))
    (dolist (x wlist (nreverse r))
      (dotimes (n (/ w 2))
        (push x r)))))

(om::defmethod! smooth ((list list) (window number)
                        (mode number)
                        &optional (start 0) (end 0))
  :menuins '( (2 (("nth"  1)
                 ("aver0"  1.5)
                 ("aver1"  2)
                 ("interp"  3)
                 ("med0"  4)
                 ("med1"  5)
                 ("med2"  6))))
  :initvals '(nil 2 1 0 0)
  :icon 128
  :doc  "Smooth list.
INPUT:
list : list of values
window : size of the window.
ARGUMENTS:
nth mode: take only a value each window step
aver mode : average values inside window
interpol mode : average and interpole - linear - values in window
med0 mode : median filter
med1 mode : median filter and then averaging
med2 mode : median filter, averaging and linear interpolation.
OPTIONAL:
start : ignore first start values
end : ignore last end values."
  (let ((r ()))
    (when (equal start 'nil) (setf start 0))
    (when (equal end 'nil) (setf end 0))
    (when (or (<= window 0) (not (integerp window)))
      (format t "ERROR : wrong window size, must be integer > 0~&")
      (abort))
    (when (and (oddp window) (and (> mode 1) (< mode 4)))
      (format t "ERROR : window size must be even~&")
      (abort))
    (when (and (evenp window) (> mode 3))
      (format t "! Window size changed to ~S samples for median filter~&" (1+ window))
      )
    (setf list (trunc list start end))
    (cond ((= mode 1)
           (loop for i from 0 to (- (1- (length list)) (+ start end))
                 do
                 (when (integerp (/ i window))
                   (push (nth (+ i start) list) r)))
           (reverse r))
          ((= mode 1.5)
           (setf r (aver-window list window))
           (append (make-list (/ window 2) :initial-element (car r)) r))
          ((= mode 3)
           (setf r (interpol-lin
                    (aver-window list window)
                    window))
           (append (make-list (/ window 2) :initial-element (car r)) r))
          ((= mode 2)
           (setf r (no-interpol
                    (aver-window list window)
                    window))
           (append (make-list (/ window 2) :initial-element (car r)) r))
          ((= mode 4)
           (setf r (filtrage-median list (1+ window)))
           (append (make-list (/ window 2) :initial-element (car r)) r))
          ((= mode 5)
           (setf r (filtrage-median
                    (no-interpol
                     (aver-window list window)
                     window)
                    (1+ window)))
           (append (make-list (/ window 2) :initial-element (car r)) r))
          ((= mode 6)
           (setf r (filtrage-median
                    (interpol-lin
                     (aver-window list window)
                     window)
                    (1+ window)))
           (append (make-list (/ window 2) :initial-element (car r)) r))
          )))

(defun filtrage-median (list  w)
  (let ((r ()) (wind ()))
    (dotimes (i
              (1+ (* (1- (floor (/ (length list) w))) w))
              (nreverse r))
      (setf wind 'nil)
      (dotimes (n
                w
                (push (nth (floor (/ w 2)) (sort wind '<)) r))
        (push (nth (+ i n) list) wind)))))

;************** arbres minimums et substitutions par distances et arbres *************

(defun remove-arete (arete set)
  (remove-if #'(lambda (x) (equalp arete x)) set))

(defun add-sommet (arete set)
  (cond ((equal 'nil (member (car arete) set))
         (push (car arete) set))
        ((equal 'nil (member (cadr arete) set))
         (push (cadr arete) set))))


(defun lookformin-if-set (i dist set)
"Returns a list with i and its nearest point and the distance separating them,
given the semi-matrix of distances dist."
  (let ((d-min (car (sort (mapcar #'caddr dist) '>)))
        arete)
  (loop  for n from 0 to (length dist)
         do
         (when (not (equal 'nil (member i (nth n dist))))
           (when (and (< (caddr (nth n dist)) d-min)
                      (equal 'nil (member (car (remove i (nth n dist))) set)))
             (setf d-min (caddr (nth n dist)))
             (setf arete (nth n dist)))))
  arete))

(om::defmethod! Prim-tree ((dist list)) 
  :initvals '(nil)
  :icon 128
  :doc  "Builds up the shorter tree of the points given in the matrix of distances (list of list),
distances must be expressed as ((xi yi di) etc.).
See: E. Diday & all, 1982 : Elements d'analyse de donnees, Dunod, Paris. pp. 110-111."
  (let ((distances (copy-list dist))
        (Omega '())
        (Te '())
        (Aretes '())
        (aretes-temp '())
        (arete-min '()))
    (setf Omega (remove-duplicates (append (mapcar #'car distances) (mapcar #'cadr distances))))
    (push (car Omega) Te)
    (loop for n from 0 until (= (length Te) (length Omega))
          do
          (setf aretes-temp 'nil)

          (loop for p from 0 to (1- (length Te))
                do
                (push  (lookformin-if-set (nth p Te) distances Te) aretes-temp))
          (setf aretes-temp (remove 'nil aretes-temp))
          (setf arete-min (car (sort aretes-temp '< :key 'caddr)))
          (setf distances (remove-arete arete-min distances))
          (push arete-min Aretes)
          (setf Te (add-sommet arete-min Te)))
    (reverse Aretes)))

(defun remove-ifnot-1 (set)
  (remove-if #'(lambda (s) (not (equal 1 (length s)))) set))

(defun remove-ifonly-1 (set)
  (remove-if #'(lambda (s) (equal 1 (length s))) set))

(defun extremites (points tree)
  (mapcar #'car (remove-ifnot-1 (mapcar #'(lambda (a) (remove 'nil a))
                       (mapcar #'(lambda (point)
                                   (mapcar #'(lambda (x)
                                  (when (not (equalp 'nil (member point x))) x))
                                           tree))
                               points)))))

(defun noeuds (points tree)
  (remove-ifonly-1 (mapcar #'(lambda (a) (remove 'nil a))
                       (mapcar #'(lambda (point)
                                   (mapcar #'(lambda (x)
                                               (when (not (equalp 'nil (member point x))) x))
                                           tree))
                               points))))

(defun tronc (extrem noeuds)
  (dolist (e extrem (remove-duplicates (om::flat-once noeuds) :test 'equalp))
    (dotimes (n (length noeuds))
      (setf (nth n noeuds) (remove-if #'(lambda (x) (equalp e x)) (nth n noeuds))))))

(defun commun (list)
"presuppose qu'il y a au moins un elt commun a toutes les sous-listes de list."
  (car (remove-if #'numberp (intersection (car list) (cadr list)))))

(defun substitute-seuil (seq distances threshold)
  "Substitute each elt of sequence by its nearest if their distance
is equal or lower than threshold and according to the Prim's minimum length tree.
Returns the new sequence with substitution and a list of (by (replaced ....))."
  (let ((sequence (copy-list seq))
        (noeux '())
        (list-of-fathers ())
        (list-of-subst ()))
    (setf noeux (noeuds (remove-duplicates sequence) (prim-tree distances)))
    (dolist (n noeux (values sequence (remove-if #'(lambda (l)
                                                     (or (< (length l) 2)
                                                         (not (listp (cadr l)))))
                                                 list-of-subst)))
      (let ((father (commun n))
            (substituted ()))
        (dolist (p n (push (reverse substituted) list-of-subst))
          (when (and (<= (caddr p) threshold)
                     (eql 'nil (member
                                (car (remove father (list (car p) (cadr p))))
                                list-of-fathers)))
            (nsubstitute father
                         (car (remove father (list (car p) (cadr p))))
                         sequence)
            (push (car (remove father (list (car p) (cadr p)))) substituted)))
        (push father list-of-fathers)
        (when (> (length substituted) 0)
          (push (list father substituted) list-of-subst))))))

;(substitute-seuil '(a b a b c d a b c a d e d f g d e g f a b c) di 0.5)

(om::defmethod! S-Class ((seq list) (dist list) (thresh number))
;;ancienne fonction rbynearest
  :initvals '(nil nil 0)
  :icon 128
  :doc  "Substitute each elt of sequence by its nearest if their distance
is equal or lower than threshold and according to the Prim's minimum length tree.
Returns the new sequence with substitution and a list of (by (replaced ....))."
  (substitute-seuil seq dist thresh))


#|
;*************** elements pour sooth catastrophes, calculer droite de regression ***************************
(defun delta-abs-wind (list w)
  (let ((wind ()) (r ()))
    (when (oddp w)
      (format t "ERROR : window size must be even~&")
      (abort))
    (dotimes (i (1- (floor (/ (* 2 (length list)) w))) (nreverse r))
      (setf wind 'nil)
      (dotimes (n w (push (* 1.0 (apply #'+ (absol (delta wind 1000)))) r))
        (push (nth (+ (/ (* w i) 2) n) list) wind)))))


(defun catas (list w)
  (let ((l1 (sort (delta-abs-wind list w) '<)))
    (nth (find-change (class-1 (l-matrix l1) 2)) l1)))

(defun find-idx (list seuil)
  (loop for i from 0 to (length list)
        until (> (nth i list) seuil)
        finally (return i)))

(defun find-change (list)
  (loop for i from 0 to (- (length list) 2)
        until (not (= (nth i list) (nth (1+ i) list)))
        finally (return i)))
|#

(om::defmethod! delta ((list list) (round integer) )
  :initvals '(nil 1000)
  :icon 128
  :doc  "Round values in list by round factor"
  (let ((l()) (delta ()))
    (dotimes (n (- (length list) 1))
      (push (- (nth (+ n 1) list) (nth n list)) l))
    (dolist (a l delta)
      (push (float (/ (round (* a round)) round)) delta))))

; Comments by Carlos
;(format t "bug dans computation time dans save structure-1 (trop grand !!!!)~%")
;(format t "bug dans rma-1 en mode short, struct non converties en alpha a partir du niveau 2~%")

#|
rma-1 =>

;     recursive-mark-analyse (2 references)
;     alpha-struct (2 references)
;     rma-1-scores (2 references)
;     print-smoothing (4 references)
;     take-date (2 references)
;     alp??
;     ac+complete
;     to-alpha
;     ac

recursive-mark-analyse =>

;     alp?1?
;     alp?? (3 references)
;     ac+complete

ac+complete =>

;     take-result-of-ac (2 references)

take-result-of-ac =>

;     analyse-contrastive

ac =>

;     analyse-contrastive (2 references)

analyse-contrastive =>

;     take-patterns
;     take-criteria
;     take-structures
;     lisse
;     seg/contrast (2 references)
;     group (2 references)
|#

#|
(defun dist-patts (seq)
  (mapcar #'(lambda (x) (ldl-distance x 1 1 0 1 1)) (nth 2 (take-result-of-ac seq 1))))
|#



;#|
;************************ NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ********************
;************************ NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ********************
;************************ NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ********************
;************************ NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ********************
;************************ NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ** NEW ********************

(om::defmethod! draw-tree ((tree list))
:doc "Draw in a new window a graphic representation of the tree.
Tree : tree list from Prim-tree"
:icon 128
  (om::om-make-window 'tree-window :tree tree))

(defclass tree-window (om::om-window) 
  ((tree :initform nil :initarg :tree :accessor tree)) )

(defmethod om::om-draw-contents ((self tree-window))
  (call-next-method)
  (let ((h (om::om-point-h (om::om-view-size self)))
        (v (om::om-point-v (om::om-view-size self))))
    ;(om::om-with-focused-view self
    ;  (om::om-erase-rect-content 0 0 h v))
    ;(om::om-set-font self (om::om-make-font "times" 10))
    (make-graph-tree self (tree self))))

(defmethod om::om-resize-window ((self tree-window) where)
  (call-next-method)
  (om::om-invalidate-view self))

(defun draw-string (x y str)
  (om::om-draw-string x y str))

(defun sommet-p (n tree)
"n is a list from tree, i.e. (a b distance)
and looks if a or b is a peak in tree"
  (let ((sommet? ()) (tree-n (remove n tree :test #'equal)))
    (dotimes (o 2 (nreverse sommet?))
      (push (equal 'nil (member 'nil (mapcar #'(lambda (x)
                                                 (equal 'nil (member (nth o n) x)))
                                             tree-n)))
            sommet?))))
#| 
(setf test-tree '((a c 0.8) (b a 0.5) (b d 2) (d e 0.41) (d f 0.6) (f g 1.4)))
(sommet-p '(a c 0.8) test-tree)
|#

(defvar morph::*x* 0)
(defvar morph::*y* 0)
(defvar morph::*eps* 2)

(defun group1 (a l1)
  (let ((r ())
        )
    (dolist (l l1)
      (when (not (equalp 'nil (member a l :test #'equalp)))
        (push (remove a l :test #'equalp) r)))
    (append (list a) r)))

(defvar *set-tree* '())
(defun elt-tree (tree)
  (cond ((not (= 0 (length tree)))
         (if (listp (car tree))
           (elt-tree (car tree))
           (setf *set-tree* (append (list (car tree)) *set-tree*)))
         (elt-tree (cdr tree)))
        (t *set-tree*)))

(defun elts (tree)
  (setf *set-tree* '())
  (elt-tree tree))

(defun n-it (set)
  (let ((set-i (remove-duplicates set :test #'equalp))
        (r ()))
    (dolist (s set-i r)
      (push (list s (length (remove-if-not #'(lambda (x) (equalp s x)) set))) r))))

;(n-it (remove-if #'numberp (elts t1)))

(defun make-groups (tree)
  (let ((elements (n-it (remove-if #'numberp (elts tree))))
        (r ())
        )
    (dolist (e elements (reverse r))
      (when (> (cadr e) 1)
         (push (group1 (car e) tree) r)))))

(defun calculate-coordinates (tree)
  (let ((new-tree (make-groups tree))
        (coordinates ())
        (temp ())
        current-pos
        (x 0)
        (y 0)
        alpha)
    ;;;calcule les coordonnees des pts definissant le tronc de l'arbre
    (loop until (= 0 (length new-tree))
          do
          (setf temp (pop new-tree))
          (setf current-pos (find-pos (car temp) coordinates))
          (cond ((eq 'nil current-pos)
                 (setf x 0)
                 (setf y 0)
                 (push (list (car temp) x y) coordinates))
                (t
                 (setf x (cadr current-pos))
                 (setf y (caddr current-pos))))

          (dotimes (n (1- (length temp)))
            (setf alpha (* (modulo-alpha n) (/ (* (1+ n) pi) (expt 2 (1+ n)))))
            ;(setf alpha (/ (* (1+ n) pi) (length temp)))
            (when (eq 'nil (find-pos (car (nth (1+ n) temp)) coordinates))
              (push (list (car (nth (1+ n) temp))
                          (+ x (* (cos (* alpha n)) (cadr (nth (1+ n) temp))))
                          (+ y (* (sin (* alpha n)) (cadr (nth (1+ n) temp)))))
                    coordinates)
              )))
    (reverse coordinates)))

(defun modulo-alpha (n)
  (if (= 0 (mod n 2))
    1 -1))

(defun find-pos (x list)
  (loop for n from 0 to (length list)
        when (not (eq 'nil (member x (nth n list) :test #'equalp)))
        return (nth n list)))

(defun take-coord (pt coord)
  (cdar
   (remove 'nil
           (mapcar #'(lambda (x)
                       (when (not (eq 'nil (member pt x :test #'equalp) ))
                         x))
                   coord))))

(defun list->str (list)
  (if (not (member 'nil (mapcar #'listp list)))
    (mapcar #'list->str list)
    (let ((string (make-string (length list))))
      (dotimes (n (length list) string)
        (setf (elt string n) (character (nth n list)))))
        ;(setf (elt string (1+ n)) #\Space)
        ))

(defun test-tree (tree)
  (mapcar #'(lambda (elt-tree)
              (mapcar #'(lambda (x)
                          (if (listp x)
                            (list->str x)
                            x)) elt-tree)) tree))

(defun make-graph-tree (window tree)
  (let ((ntree (test-tree (copy-list tree)))
        (coordinates '())
        scale
        minx maxx
        miny maxy)       
    (setf coordinates (calculate-coordinates (copy-list ntree)))
    (setf minx (apply #'min (mapcar #'cadr coordinates))
          miny (apply #'min (mapcar #'caddr coordinates))
          maxx (apply #'max (mapcar #'cadr coordinates))
          maxy (apply #'max (mapcar #'caddr coordinates)) )
    (setf scale (min (/ 800 (- maxx minx))
                     (/ 600 (- maxy miny))))
    (setf coordinates (mapcar #'(lambda (a)
                                  (list (car a)
                                        (+ 20 (* scale (- (cadr a) minx))) 
                                        (+ 20 (* scale (- (caddr a) miny))))) coordinates))
    ;(om::om-set-view-size window (om::om-make-point (+ (round (apply #'max (mapcar #'cadr coordinates))) 20)
    ;                                            (+ (round (apply #'max (mapcar #'caddr coordinates))) 20)))
    (om::om-with-focused-view window      
      ;;first place points in the window
      (dolist (coord coordinates)
        (draw-string
         (+ *eps* (round (+ morph::*x* (cadr coord))))
         (- (round (+ morph::*y* (caddr coord))) morph::*eps*)
         (if (stringp (car coord))
           (car coord)
           (symbol-name (car coord)))))
      ;;then draw lines
      (dolist (pt ntree)
          (let ((co1 (take-coord (car pt) coordinates))
                (co2 (take-coord (cadr pt) coordinates)))
;;;          (move-to window
;;;                   (round (+ morph::*x* (car co1)))
;;;                   (round (+ morph::*y* (cadr co1))))
;;;          (line-to window
;;;                    (round (+ morph::*x* (car co2)))
;;;                        (round (+ morph::*y* (cadr co2))))
            (om::om-draw-line (round (+ morph::*x* (car co1))) (round (+ morph::*y* (cadr co1)))
                                                (round (+ morph::*x* (car co2))) (round (+ morph::*y* (cadr co2))))
            )))))

(defun etiquet (list)
  (let ((set (remove-duplicates (reverse list) :from-end '0 :test #'equalp)))
    (dotimes (n (length set) (reverse list))
      (setf list (substitute (car (num->alpha (list (1+ n))))
                   (nth n set)
                   list
                   :test #'equalp)))))

(defun rep-by-et (dist list et)
  (dotimes (n (length list) dist)
    (dolist (d dist)
      (nsubstitute (nth n et) (nth n list) d :test #'equalp))))

#|
(defun dist-patts (seq)
  (sort (om::flat-once (mapcar #'(lambda (x) (ldl-distance x 1 1 0 1 1)) (nth 2 (take-result-of-ac seq 1)))) #'< :key #'caddr))
|#


(defun morph::center (x y)
  (setf morph::*x* x)
  (setf morph::*y* y))

;(center 150 100)

#|
(draw-tree test-tree 100)
|#


(defun to-flag1 (list)
  (let ((set (remove-duplicates (reverse list) :from-end '0 :test #'equalp))
        (bag ()))
    (dotimes (n (length set))
      (setf list (substitute (car (num->alpha (list (1+ n))))
                   (nth n set)
                   list
                   :test #'equalp))
      (push (list (car (num->alpha (list (1+ n)))) (nth n set)) bag))
    (dolist (b (reverse bag))
      (format t "~%~S <- ~S" (car b) (cadr b)))
    (values (reverse list) (reverse bag))))
#|
(to-flag '(1 2 g 8 tre (5 8) (5 8) tre g 8 1 2 2))

(om::defmethod! to-flag ((list list))
  :icon 128
  (select-from-list *parameters* (symbol-name database)))
|#


(defun rep-by-flag1 (dist list flags)
  (dotimes (n (length list) dist)
    (dolist (d dist)
      (nsubstitute (nth n flags) (nth n list) d :test #'equalp))))

(om::defmethod! rep-by-flag ((dist list) (list list) (flags list))
  :icon 128
  (rep-by-flag1 dist list flags))


#|
(defun dist-patts (seq)
  (sort (om::flat-once (mapcar #'(lambda (x) (ldl-distance x 1 1 0 1 1)) (nth 2 (take-result-of-ac seq 1)))) #'< :key #'caddr))
|#


#|
(defun select-from-list (list &optional subject)
  (if (not subject)
    (setf subject "select :")
    (setf subject (concatstrings (list "select " subject " :"))))
  (select-item-from-list list
                       :selection-type :disjoint
                       :window-title subject))

(om::defmethod! select-data ((database symbol (:value 'data))) list
            ""
  (select-from-list *parameters* (symbol-name database)))
            

#|
(select-from-list '(1 2 3 4 5 6) "rythms")
|#

(defun enter-string (subject)
  (om::om-get-user-string (concatstrings (list "Enter " subject " :"))
                        :size (om::om-make-point 500 70)
                        :position (om::om-make-point 100 100)))

(defun set-data-base (name &optional length)
  (if (not length)
    (setf (symbol-value name) (make-hash-table))
    (setf (symbol-value name) (make-hash-table :size length)))
  (print name))

(om::defmethod! data-base ((name object (:value 'data))
                       (action menu (:menu-box-list 
                                     (("new" . 1)
                                      ("print" . 2)
                                      ("read" . 0)
                                      ("save" . 3)
                                      ("add" . 4))))
                       &optional (length fix (:value 16))) list
            ""
  (cond ((= 1 action)
         (set-data-base name length)
         (setf *parameters* '()))
        ((= 2 action)
         (format t "~% ~S entries in ~S."
                 (hash-table-count (symbol-value name))
                 name)
         (print (symbol-value name))
         (dolist (p (select-from-list *parameters* (symbol-name name)))
           (format t "~%~S ~S : ~S"
                   name
                   p
                   (gethash p
                            (symbol-value name)))))
        ((= 4 action)
         (add-to-datase name))))

(defvar *parameters* '())

(defun add-to-datase (name)
  (let ((param (read-from-string
                (om::om-get-user-string "Parameter name :"
                        :size (om::om-make-point 250 70)
                        :position (om::om-make-point 100 100)))))
    (setf *parameters* (push param *parameters*))
    (setf (gethash 
           param
           (symbol-value name))
          (append (string-to-symbol (enter-string (symbol-name name))) 
                  (gethash param (symbol-value name))))))

(om::defmethod! add-to-data ((name symbol (:value 'data))) list
                         ""
  (add-to-datase name))


|#