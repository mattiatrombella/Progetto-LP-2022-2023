; 879184 Trombella Mattia
; 885880 Romeo Francesco

; -*- Mode: Lisp -*-
;   jsonparse.lisp
;
;
; This file contains the functions to parse a JSON string
;
; Authors: Trombella Mattia and Francesco Romeo

(defun stringtolist (string)
  (coerce string 'list))

(defun consend (e l)
  (if (null l) (list e)
    (cons (first l) (consend e (rest l)))))

(defun restbutlast (list)
  (rest (butlast list)))

(defun butlast (list)
  (reverse (rest (reverse list))))

(defun endlist (list)
  (first (reverse list)))

(defun whitespace (list instring)
  (if (null list) list
    (let ((current (first list))
          (next (rest list)))
      (if (null instring)
          (cond ((member current *apix*)
                 (append (list current) (whitespace next T)))
                ((member current *spaces*)
                 (whitespace next nil))
                (T (append (list current) (whitespace next nil))))
        (if (member current *apix*)
            (append (list current) (whitespace next nil))
          (append (list current) (whitespace next T)))))))

(defun jsonobject (list)
  (let ((i (first list)) (jsonobj list))
    (cond
     ((and (member i *cpar*) (null (rest jsonobj)))
      (list (list 'json-obj)))
     (T (append
         (list 'json-obj)
         (jsonmembers jsonobj))))))

(defun jsonmembers (list)
  (let ((i (first list)))
    (cond
     ((null list) nil)
     ((member i *apix*)  (jsonpair (rest list) (list i)))
     (T (error "~%OGGETTO ERRATO")))))

(defun jsonpair (list &optional (accum nil))
  (let ((i (first list)))
    (cond ((and (member i *apix*) (eql (cadr list) '#\:))
           (jsonpaircomma (rest (rest list))
                            (consend i accum) nil))
          ((and (member i *apix*) (not (eql (cadr list) '#\:)))
           (error "~%OGGETTO ERRATO"))
          (T (jsonpair (rest list) (consend i accum))))))

(defun jsonpaircomma (list &optional (before nil) (accum nil))
  (let ((i (first list)) (e (rest list)))
    (cond
     ((and (null e) (eql i '#\}))
      (list (append (analyzepayload before)
                    (analyzepayload accum))))
     ((eql i '#\,)
      (append (list (append (analyzepayload before)
                            (analyzepayload accum)))
              (jsonmembers e)))
     ((eql i '#\[)
      (collectarray list nil before))
     ((eql i '#\{)
      (collectobj list nil before))
     ((and (null e) (not (eql i '#\})))
      (error "~%MANCANO PARENTESI"))
     (T (jsonpaircomma (rest list) before (consend i accum))))))

(defun analyzepayload (list)
  (let ((i (first list)))
    (cond ((eql i '#\') (list (jsonstringsingle (rest list))))
          ((eql i '#\") (list (jsonstringdouble (rest list))))
          ((or (member i *digits*) (member i *symbols*))
           (list (jsonnumbers list)))
          ((or (eql i '#\[) (eql i '#\{))
           (list (jsonparser list))))))

(defun collectarray (list &optional (accum nil) (before nil))
  (let ((i (first list)))
    (cond ((eql i '#\])
           (jsonpaircomma (rest list) before (consend i accum)))
          (T (collectarray (rest list) (consend i accum) before)))))

(defun collectobj (list &optional (accum nil) (before nil))
  (let ((i (first list)))
    (cond ((eql i '#\})
           (jsonpaircomma (rest list) before (consend i accum)))
          (T (collectobj (rest list) (consend i accum) before)))))

(defun jsonnumbers (list &optional (accum nil))
  (let ((i (first list)))
    (cond
     ((eql i '#\.) (jsonnumbersfloat list accum))
     ((null list) (parse-integer (coerce accum 'string)))
     ((member i *spaces*) (error "~%VALORE NON VALIDO"))
     (T (jsonnumbers (rest list) (consend i accum))))))

(defun jsonnumbersfloat (list &optional (accum nil))
  (parse-float (coerce (append accum list) 'string)))

(defun jsonstringsingle (list &optional accum)
  (let ((i (first list)))
    (cond
     ((eql i '#\')
      (coerce accum 'string))
     ((eql i '#\")
      (error  "~% APICI CONFUSI"))
     (T (jsonstringsingle (rest list)
                     (consend i accum))))))

(defun jsonstringdouble (list &optional accum)
  (let ((i (first list)))
    (cond
     ((eql i '#\")
      (coerce accum 'string))
     ((eql i '#\')
      (error  "~% ERRORE APICI"))
     (T (jsonstringdouble (rest list)
                     (consend i accum))))))

(defun jsonarray (list)
  (let ((i (first list)) (jsonobj list))
    (cond
     ((and (member i *apar*) (null (rest jsonobj)))
      (list '(jsonarray)))
     (T (append '(jsonarray) (jsonelements jsonobj))))))

(defun jsonelements (list &optional (accum nil))
  (let ((i (first list)) (e (rest list)))
    (cond
     ((null e)
      (analyzepayload accum))
     ((or (eql i '#\{) (eql i '#\[))
      (jsonelementsnested list))
     ((eql i '#\,)
      (append (analyzepayload accum)
              (jsonelements e)))
     (T (jsonelements e (consend i accum))))))

(defun jsonelementsnested (list &optional accum)
  (let ((i (first list)) (e (rest list)))
    (cond
     ((or (eql i '#\}) (eql i '#\]))
      (append (list (jsonparser (consend i accum)))
              (jsonelements (rest(rest list)))))
     (T (jsonelementsnested e (consend i accum))))))

(defun jsonparser (list)
  (cond ((eql (first list) '#\{)
         (jsonobject (rest list)))
        ((eql (first list) '#\[)
         (jsonarray (rest list)))
        (T (error  "~%OGGETTO JSON ERRATO"))))

(defun jsonparse (json)
  (let ((list (stringtolist json)))
    (jsonparser (whitespace list nil))))

(defun jsonread (filename)
  (jsonparse (readfile filename)))

(defun readfile (filename)
  (if (null filename)
      (error "Il filename NIL")
    (with-open-file
        (in filename
            :if-does-not-exist :error)
      (let ((json (make-string (file-length in))))
        (read-sequence json in)
        json))))

(defun jsondump (list filename)
  (cond ((null filename) (error "Il filename e' NIL"))
        (T (with-open-file
               (stream filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
             (format stream (listtostring list))))))

(defun listtostring (list)
  (cond
   ((eql (first list) 'json-obj)
    (concatenate 'string "{" (dumpobj(rest list)) "}"))
   ((eql (first list) 'jsonarray)
    (concatenate 'string "[" (dumparray(rest list)) "]"))
   (T (error "Malformed object"))))

(defun dumpobj (list)
  (let ((i (first list)) (r (rest list)))
    (cond
     ((null list) "")
     ((concatenate 'string (dumppair i)(dumpobj r))))))

(defun dumparray(list)
  (let ((i (first list)) (r (rest list)))
    (cond
     ((null list) "")
     ((null (rest list)) (dumpvalue i))
     (T (concatenate 'string (dumpvalue i) ","
                     (dumparray r))))))

(defun dumppair(list)
  (concatenate 'string "\"" (car list) "\"" ":"
               (dumpvalue(cadr list))))

(defun dumpvalue (list)
  (cond
   ((numberp list) (write-to-string list))
   ((stringp list) (concatenate 'string "\"" list "\""))
   (T (listtostring list))))

(defun jsonaccess (list &rest fields)
  (jsonfields list fields))

(defun jsonfields (lista val)
  (cond ((null val) lista)
	((stringp (first val))
         (jsonfields (searchkey (first val) lista) (rest val)))
	((and (numberp (first val)) (< (first val) 0))
         (error "IMPOSSIBILE USARE VALORI NEGATIVI"))
	((numberp (first val))
	 (jsonfields
          (searchindex lista (first val)) (rest val)))))

(defun searchkey (key list)
  (let ((l (car list)))
    (cond ((null list) (error "~%CHIAVE NON TROVATA"))
          ((equal 'json-obj (first list))
           (searchkey key (rest list)))
          ((equal key (first l))
           (first (rest l)))
          (T (searchkey key (cdr list))))))

(defun searchindex (list index)
  (cond
   ((eql 'jsonarray (first list))
    (searchindex (rest list) index))
   ((null list) (error "IMPOSSIBILE TROVARE INDICE"))
   ((eq index 0) (car list))
   (T (searchindex (cdr list) (- index 1)))))

(defvar *cpar* '(#\}))
(defvar *apar* '(#\]))
(defvar *spaces* '(#\Space #\Newline #\Tab))
(defvar *apix* '(#\" #\'))
(defvar *digits* '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
(defvar *symbols* '(#\. #\+ #\-))
