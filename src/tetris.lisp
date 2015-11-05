;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

;;; Acao [2.2.1]
(defun cria-accao (leftmost-col piece)
    (cons leftmost-col piece))

(defun copia-tabuleiro (tab)
    (copy-array tab))

(defun accao-coluna (action) 
    (car action))

(defun accao-peca (action) 
    (cdr action))


;;; Tabuleiro [2.1.2]
(defun cria-tabuleiro (&optional (row 18) (col 10))
    (make-array (list row col)))

(defun tabuleiro-preenchido-p(tab rowNum colNum) 
	(if (null (aref tab rowNum colNum)) nil T))
	
(defun tabuleiro-altura-coluna (tab colN &optional (rowN 0)) 
	(cond ((or (equalp rowN (first (array-dimensions tab))) (not (tabuleiro-preenchido-p tab rowN colN)) ) 0)
	(t (1+ (tabuleiro-altura-coluna tab colN (1+ rowN))))))
	
(defun tabuleiro-linha-completa-p (tab rowN &optional (colN 0)) 
	(cond ((and (equlp colN (nth 1 (array-dimensions tab))) (tabuleiro-preenchido-p rowN colN) ) T) ; colN == 10?
	((not (tabuleiro-preenchido-p tab rowN colN)) nil)
	(t (and T (tabuleiro-linha-completa-p tab rowN (1+ colN))))))

(defun tabuleiro-preenche!(tab rowN colN) 
	(if 
	(and (< rowN (first (array-dimensions tab)))
	(< colN (nth 1 (array-dimensions tab))))
	(setf (aref tab rowN colN) T)))

(defun tabuleiro-topo-preenchido-p(tab &optional (colN 0)) 
	(cond
	((equalp colN (nth 1 (array-dimensions tab))) T)
	((not (tabuleiro-preenchido-p tab (1- (first (array-dimensions tab))) colN)) nil)
	(t (and T (tabuleiro-topo-preenchido-p tab (1+ colN))))))
	
(defun tabuleiro-remove-linha!(tab rowN)
    (let ((col-size (tabuleiro-col-size tab))
          (row-size (tabuleiro-row-size tab))
          (upper-rowN (+ rowN 1))
         )
        (cond
            ((= upper-rowN col-size)
            ; put an empty line in the top row
                (dotimes (i col-size) (setf (aref tab rowN i) nil) )
            )
            ((< upper-rowN col-size)
                ; move the row rowN+1 to rowN
                (dotimes (i col-size) (setf (aref tab rowN i) (aref tab upper-rowN i) ) )
                (tabuleiro-remove-linha! tab (+ rowN 1))
            )
            (t nil)
        )
    )
)

(defun tabuleiro-col-size(tab)
    (nth 1 (array-dimensions tab))
)

(defun tabuleiro-row-size(tab)
    (nth 0 (array-dimensions tab))
)

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


;;; Utils
(defun copy-array (arr)
    "Given an array, returns a copy of it"
    (let ((dims (array-dimensions arr)))
        (adjust-array
            (make-array dims :displaced-to arr) dims)))
