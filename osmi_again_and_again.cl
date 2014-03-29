(defun double-header (graf startl endl startv endv) ; l - buffer (traversed) v - visited
  (cond ((null startl) '())
        ((null endl) '())
        ((equal (car startl) (car endl)) (list (car startl)))
        (:else (let* ((new-startv (append startv (list (car startl))))
                     (new-endv (append endv (list (car endl))))
                     (new-startc (add-child graf (car startl) (append (cdr startl) new-startv))) ; c - children
                     (new-endc (add-child graf (car endl) (append (cdr endl) new-endv)))
                     (new-startl (append (cdr startl) new-startc))
                     (new-endl (append (cdr endl) new-endc))
                     (putanja (append (double-header graf new-startl endl new-startv endv) (double-header graf startl new-endl startv new-endv))))
                   (cond ((null putanja) '())
                         ((member (car putanja) new-startv) (if (member (last putanja) new-endv)
                                                                (cons (car startl) (append putanja (list (car endl))))
                                                              (cons (car startl) putanja)))
                         (:else (if (member (last putanja) new-endv)
                                    (append putanja (list (car endl)))
                                  putanja)))))))


(defun add-child (graf cvor cvorovi)
  (cond ((null graf) '())
        ((equal (caar graf) cvor) (novi-cvorovi (cadar graf) cvorovi))
        (:else (add-child (cdr graf) cvor cvorovi))))

(defun novi-cvorovi (potomci cvorovi)
  (cond ((null potomci) '())
        ((member (car potomci) cvorovi) (novi-cvorovi (cdr potomci) cvorovi))
        (:else (cons (car potomci) (novi-cvorovi (cdr potomci) cvorovi)))))