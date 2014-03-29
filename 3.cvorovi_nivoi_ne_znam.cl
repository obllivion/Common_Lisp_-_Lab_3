(defun formiraj_assoc (graf l cvorovi)
  (cond ((null l) (mapcar (lambda (x) (napravi x (length graf))) (cdr cvorovi)))
        (t(let* ((cvorovi1 (append cvorovi (list (car l))))
                 (potomci1 (dodaj-potomke graf (car l) (append (cdr l) cvorovi1)))
                 (l1 (append (cdr l) potomci1)))
            (formiraj_assoc graf l1 cvorovi1)))))

(defun napravi (cvor duzina)
  (list (car cvor) (- duzina (cadr cvor))))

(defun dodaj-potomke (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) (car cvor)) (novi-cvorovi (cadar graf) cvorovi (1+ (cadr cvor))))
        (:else (dodaj-potomke (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi lvl)
  (cond ((null potomci) '())
        ((ispitaj (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi lvl))
        (:else (cons (list (car potomci) lvl) (novi-cvorovi (cdr potomci) cvorovi lvl)))))

(defun ispitaj (cvor obradjeni)
  (cond ((null obradjeni) '())
        ((equal cvor (caar obradjeni)) t)
        (:else (ispitaj cvor (cdr obradjeni)))))
        

