(defun double-header (graf startl endl startv endv) ; l - buffer (traversed) v - visited
  (cond ((null startl) '())
        ((null endl) '())
        ((ispitaj startl endl) (vrati-put (startv (ispitaj startl endl) endv)))
        (:else (let* ((new-startv (append startv (list (car startl))))
                     (new-endv (append endv (list (car endl))))
                     (new-startc (add-child graf (car startl) (append (cdr startl) new-startv))) ; c - children
                     (new-endc (add-child graf (car endl) (append (cdr endl) new-endv)))
                     (new-startl (append (cdr startl) new-startc))
                     (new-endl (append (cdr endl) new-endc)))
                 (double-header graf new-startl new-endl new-startv new-endv)))))



(defun add-child (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) (car cvor)) (novi-cvorovi (cadar graf) cvorovi (car cvor))) ; (car cvor) je zbog assoc (A '()), (B A), (C A) itd.
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi predak)
  (cond ((null potomci) '())
        ((clan (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi predak))
        (:else (cons (list (car potomci) predak) (novi-cvorovi (cdr potomci) cvorovi predak)))))


(defun ispitaj (startl endl)
  (cond ((null startl) '())
        ((atom startl) (if (member startl endl) startl '() ))
        (t(append (ispitaj (car startl) endl) (ispitaj (cdr startl) endl)))))

(defun vrati-put (startv el endv)
  (trazi-rod el graf3))

(defun trazi-rod (el graf)
  (cond ((null graf) '())
        ((member el (cadar graf)) (cons (caar graf) (trazi-rod (caar graf) graf3)))
        (t(trazi-rod el (cdr graf)))))