(defun nadji-put (graf l cvorovi)
  (cond ((null l) (mapcar (lambda (x) (nadji-cvor x graf)) cvorovi))
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1))
                 (graf1 (izbaci graf (car l) (append (cdr l) cvorovi1))))
            (nadji-put graf1 l1 cvorovi1)))))

(defun nadji-cvor (cvor graf)
  (cond ((null graf) '())
        ((equal cvor (caar graf)) (car graf))
        (t(nadji-cvor cvor (cdr graf)))))

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor) (novi-cvorovi (cadar graf) cvorovi cvor))
        (t(dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi cvor)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi))
        (t(cons (car potomci) (novi-cvorovi (cdr potomci) cvorovi)))))

(defun clan (cvor lista)
  (cond ((null lista) t)
        ((equal cvor (caar lista)) '())
        (:else (clan cvor (cdr lista)))))

(defun izbaci (graf cvor cvorovi)
  (cond ((null graf) '())
        ((member (caar graf) cvorovi) (cons (car graf) (izbaci (cdr graf) cvor cvorovi)))
        (t(cons (list (caar graf) (remove cvor (cadar graf))) (izbaci (cdr graf) cvor cvorovi)))))
        