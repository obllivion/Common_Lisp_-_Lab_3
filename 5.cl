(defun brgrafa (graf)
  (cond ((null graf) 0)
        (t(let* ((cvorovi1 (disjoint graf (car graf) '()))
                 (graf1 (razlika graf cvorovi1)))
            (1+ (brgrafa graf1))))))

(defun disjoint (graf lista cvorovi)
  (cond ((null lista) cvorovi)
        (t(let* ((cvorovi1 (cons (car lista) cvorovi))
                 (potomci1 (dodaj-potomke graf (car lista) cvorovi1))
                 (lista1 (append (cdr lista) potomci1)))
            (disjoint graf lista1 cvorovi1)))))

(defun dodaj-potomke (graf lista cvorovi)
  (cond ((null graf) '())
        ((equal lista (caar graf)) (novi-cvorovi (cadar graf) cvorovi))
        (t(dodaj-potomke (cdr graf) lista cvorovi))))

(defun novi-cvorovi (pot cvorovi)
  (cond ((null pot) '())
        ((member (car pot) cvorovi) (novi-cvorovi (cdr pot) cvorovi))
        (t(cons (car pot) (novi-cvorovi (cdr pot) cvorovi)))))

(defun razlika (graf cvorovi)
  (cond ((null graf) '())
        ((member (caar graf) cvorovi) (razlika (cdr graf) cvorovi))
        (t(cons (car graf) (razlika (cdr graf) cvorovi)))))
        