;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

(defconstant piece-i 'i)
(defconstant piece-j 'j)
(defconstant piece-l 'l)
(defconstant piece-o 'o)
(defconstant piece-s 's)
(defconstant piece-z 'z)
(defconstant piece-t 't)

;;; Acao [2.2.1]
(defun cria-accao (leftmost-col piece)
    (cons leftmost-col piece))

(defun accao-coluna (action) 
    (car action))

(defun accao-peca (action) 
    (cdr action))


;;; Tabuleiro [2.1.2]
(defun cria-tabuleiro (&optional (row 18) (col 19))
    (make-array (list row col)))


;;; Estado [2.1.3]
(defstruct estado
    pontos
    pecas-por-colocar   ; list ordered by exit order  | legal values: i,j,l,o,s,z,t
    pecas-por-colocadas ; list ordered by most recent | legal values: i,j,l,o,s,z,t
    Tabuleiro)

(defun copia-estado (state)
    (copy-structure state))

(defun estados-iguais-p (state1 state2)
    (equalp state1 state2))

(defun estado-final-p (state)
    (or (tabuleiro-topo-preenchido-p (estado-Tabuleiro state)) (null (estado-pecas-por-colocar state))))


;;; Problema [2.1.4]
(defstruct problema
    estado-inicial)

;;; Abstact operations on "problema"
(defgeneric solucao (solution))
(defgeneric accoes (state))
(defgeneric result (state action))
(defgeneric custo-caminho (state))

;;; Method definitions for "problema"
(defmethod solucao (solution) t)

(defmethod accoes (state) 
    (let* (
        (actions (list)) ; stores the resulting list of actions
        
        (add-piece-i-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-i0)))
            (setf actions (append actions (generate-piece-actions peca-i1)))))

        (add-piece-l-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-l0)))
            (setf actions (append actions (generate-piece-actions peca-l1)))
            (setf actions (append actions (generate-piece-actions peca-l2)))
            (setf actions (append actions (generate-piece-actions peca-l3)))))

        (add-piece-j-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-j0)))
            (setf actions (append actions (generate-piece-actions peca-j1)))
            (setf actions (append actions (generate-piece-actions peca-j2)))
            (setf actions (append actions (generate-piece-actions peca-j3)))))

        (add-piece-o-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-o0)))))

        (add-piece-s-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-s0)))
            (setf actions (append actions (generate-piece-actions peca-s1)))))

        (add-piece-z-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-z0)))
            (setf actions (append actions (generate-piece-actions peca-z1)))))

        (add-piece-t-actions #'(lambda () 
            (setf actions (append actions (generate-piece-actions peca-t0)))
            (setf actions (append actions (generate-piece-actions peca-t1)))
            (setf actions (append actions (generate-piece-actions peca-t2)))
            (setf actions (append actions (generate-piece-actions peca-t3))))))
        
        ;; Go through the list of "pieces to be placed" and append the
        ;; available actions for each one of them
        (dolist (piece (estado-pecas-por-colocar state)) 
            (cond

                ((equalp piece piece-i) (funcall add-piece-i-actions))
                ((equalp piece piece-j) (funcall add-piece-j-actions))
                ((equalp piece piece-l) (funcall add-piece-l-actions))
                ((equalp piece piece-o) (funcall add-piece-o-actions))
                ((equalp piece piece-s) (funcall add-piece-s-actions))
                ((equalp piece piece-z) (funcall add-piece-z-actions))
                ((equalp piece piece-t) (funcall add-piece-t-actions))))
            actions))

(defmethod resultado (state action) t)
(defmethod custo-caminho (state) t)


(defun generate-piece-actions (piece)
    "Given a piece configuration generates a list of valid actions
     for that piece"
    (let (   ; num columns the piece spans
             (piece-columns (nth 1 (array-dimensions piece)))
             (table-columns 10)
             (actions (list))
         )
         (dotimes (column (+ 1 (- table-columns piece-columns)))
             (setf actions (append actions (list(cria-accao column piece)))))
          actions))