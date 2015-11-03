;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

;;; Acao [2.2.1]
(defun cria-accao(leftmost-col piece)
    (cons leftmost-col piece))

(defun accao-coluna(action) 
    (car action))

(defun accao-peca(action) 
    (cdr action))


;;; Tabuleiro [2.1.2]
(defun cria-tabuleiro (&optional (row 18) (col 19))
    (make-array (list row col)))

