(defun nadji-put (graf l cvorovi)
  (cond ((null l) (napravi-stablo graf cvorovi))
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (nadji-put graf l1 cvorovi1)))))

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) (car cvor)) (novi-cvorovi (cadar graf) cvorovi (car cvor))) ; (car cvor) je zbog assoc (A '()), (B A), (C A) itd.
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi predak)
  (cond ((null potomci) '())
        ((clan (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi predak))
        (:else (cons (list (car potomci) predak) (novi-cvorovi (cdr potomci) cvorovi predak)))))

(defun clan (cvor lista)
  (cond ((null lista) '())
        ((equal cvor (caar lista)) t)
        (:else (clan cvor (cdr lista)))))

(defun napravi-stablo (graf cvorovi)
  (cond ((null cvorovi) graf)
        (:else (let* ((graf1 (izbaci graf (car cvorovi))))
                 (napravi-stablo graf1 (cdr cvorovi))))))

(defun izbaci (graf cvor)
  (cond ((null graf) '())
        ((equal (caar graf) (cadr cvor)) (cons (car graf) (izbaci (cdr graf) cvor)))
        (:else (cons (list (caar graf) (izbaci-potomke (cadar graf) (car cvor))) (izbaci (cdr graf) cvor)))))

(defun izbaci-potomke (lista cvor)
  (cond ((null lista) '())
        ((equal (car lista) cvor) (cdr lista))
        (:else (cons (car lista) (izbaci-potomke (cdr lista) cvor)))))