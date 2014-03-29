(defun search-tree(graph traversed visited)
  (cond
   ((null traversed) NIL)
   (:else (let*
              ((new-visited (append visited (list (car traversed))))
               (children (add-children graph (car traversed)
                                       (append (cdr traversed) new-visited))))
            (cond
             ((null children) (list (car traversed)))
             (:else 
              (cons (car traversed)
                    (mapcar (lambda(x) (search-tree graph (list x) new-visited)) children)))
             )
            )
          )
   )
  )

;;; Selects the node to pick returned children from
(defun add-children(graph node visited)
  (cond
   ((null graph) NIL)
   ((equal (caar graph) node) (new-nodes (cadar graph) visited))
   (:else (add-children (cdr graph) node visited))
   )
  )

;;; Returns new, unvisited nodes from the children of a node
(defun new-nodes(children visited)
  (cond
   ((null children) NIL)
   ((member (car children) visited) (new-nodes (cdr children) visited))
   (:else (cons (car children) (new-nodes (cdr children) visited)))
   )
  )