;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

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
(defgeneric solucao (problem solution))
(defgeneric accoes (problem state))
(defgeneric result (problem state action))
(defgeneric custo-caminho (problem state))

;;; Method definitions for "problema"
(defmethod solucao ((problem problema) solution) t)
(defmethod accoes ((problem problema) state) t)
(defmethod result ((problem problema) state action) t)
(defmethod custo-caminho ((problem problema) state) t)