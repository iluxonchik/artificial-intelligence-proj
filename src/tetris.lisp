;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

(defun try-tab(row col) (cond 
                         ( (zerop row) '()) 
                         (t (setf aux '()) (dotimes (n col)
                                             (setf aux (append aux (list 0) )))
                            (cons aux (try-tab (1- row) col)))))

