;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

(defun cria-tabuleiro(&optional row &optional col) 
  (if (null row) (setf row 18)) 
  (if (null col) (setf col 19)) 
  (make-array (cons row (list col))))

