;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

;;; Uncomment Line 1 AND comment line 2 (below) when submitting to Mooshak
;;; Uncomment Line 2 AND comment line 1 (below) when using locally
;;;(load "utils.fas")           ; line 1
(load "../libs/utils.lisp")     ; line 2

;;;pieces
(defconstant piece-i 'i)
(defconstant piece-j 'j)
(defconstant piece-l 'l)
(defconstant piece-o 'o)
(defconstant piece-s 's)
(defconstant piece-z 'z)
(defconstant piece-t 't)

;;;piece values
(defconstant piece-i-value 800)
(defconstant piece-j-value 500)
(defconstant piece-l-value 500)
(defconstant piece-o-value 300)
(defconstant piece-s-value 300)
(defconstant piece-z-value 300)
(defconstant piece-t-value 300)


;;; Acao [2.2.1]

(defun cria-accao (leftmost-col piece)
    (cons leftmost-col piece))

(defun accao-coluna (action) 
    (car action))

(defun accao-peca (action) 
    (cdr action))


;;; Tabuleiro [2.1.2]

(defun cria-tabuleiro (&optional (row 18) (col 10))
    (make-array (list row col)))

(defun copia-tabuleiro (tab)
    (copy-array tab))

(defun tabuleiro-preenchido-p(tab rowNum colNum) 
	(if (null (aref tab rowNum colNum)) nil T))
	
(defun tabuleiro-altura-coluna(tab colN)  
	(if (< colN (nth 1 (array-dimensions tab )))
		(let ((X (- (first (array-dimensions tab)) 1)))
			(block loopBlock 
				(loop while (>= X 0) do
					(cond   
						((tabuleiro-preenchido-p tab x colN)
							(return-from loopBlock (+ x 1))
						)                                  
						((= x 0) 
							(return-from loopBlock x)
						)
						(t 
							(setf x (1- x))
						)
					)
				)
			)
		)
	)
)


	
(defun tabuleiro-linha-completa-p(tab rowN) 
	(if (and (< rowN (first (array-dimensions tab))) (>= rowN 0))
		(let ((x (- (nth 1 (array-dimensions tab)) 1)))
			(block loopBlock
				(loop while (>= x 0) do
				(cond 
						((tabuleiro-preenchido-p tab rowN x)
						(return-from loopBlock T)
                				)
						(
						(and (not (tabuleiro-preenchido-p tab rowN x)) (= x 0)) (return-from loopBlock nil)
                    				)
                 	  		 	(t 
						(setf x (1- x))
                	    			)
					)
				)
			)
		)
	)
)	
	

(defun tabuleiro-preenche!(tab rowN colN) 
	(if 
	(and (and (< rowN (first (array-dimensions tab)))
	(< colN (nth 1 (array-dimensions tab))))
	(and (>= rowN 0) (>= colN 0))
	)
	(setf (aref tab rowN colN) T)))

(defun tabuleiro-topo-preenchido-p(tab) 
	(if (tabuleiro-linha-completa-p tab (- (first (array-dimensions tab)) 1))
		T 
		nil
	)
)
	
(defun tabuleiro-remove-linha!(tab rowN)
    (let ((num-of-cols (tabuleiro-num-of-cols tab))
          (num-of-rows (tabuleiro-num-of-rows tab))
          (upper-rowN (+ rowN 1))
         )
        (cond
            ((= upper-rowN num-of-rows)
                ; put an empty line in the top row
                (dotimes (i num-of-cols) (setf (aref tab rowN i) nil) )
            )
            ((< upper-rowN num-of-rows)
                ; move the row rowN+1 to rowN
                (dotimes (i num-of-cols) (setf (aref tab rowN i) (aref tab upper-rowN i) ) )
                (tabuleiro-remove-linha! tab upper-rowN)
            )
            (t nil)
        )
    )
)

(defun tabuleiro-num-of-cols(tab)
    (nth 1 (array-dimensions tab))
)

(defun tabuleiro-num-of-rows(tab)
    (nth 0 (array-dimensions tab))
)

(defun tabuleiros-iguais-p(tab1 tab2)
    (equalp tab1 tab2)
)

(defun tabuleiro->array(tab)
    (copy-array tab)
)

(defun array->tabuleiro(tab)
    (copy-array tab)
)


;;; Estado [2.1.3]

(defstruct estado
    pontos
    pecas-por-colocar   ; list ordered by exit order  | legal values: i,j,l,o,s,z,t
    pecas-colocadas     ; list ordered by most recent | legal values: i,j,l,o,s,z,t
    Tabuleiro)

(defun copia-estado (state)
    (make-estado :pontos (estado-pontos state) :pecas-por-colocar (copy-list (estado-pecas-por-colocar state)) :pecas-colocadas (copy-list (estado-pecas-colocadas state)) :Tabuleiro (copia-tabuleiro (estado-Tabuleiro state)))
)

(defun estados-iguais-p (state1 state2)
    (equalp state1 state2))

(defun estado-final-p (state)
    (or (tabuleiro-topo-preenchido-p (estado-Tabuleiro state)) (null (estado-pecas-por-colocar state))))


;;; Problema [2.1.4]

(defstruct problema
    estado-inicial
    solucao
    accoes
    resultado
    custo-caminho)

;;; Funcoes Do Problema de Procura [2.2.1]

(defun accoes (state) 
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
                ((equalp piece piece-t) (funcall add-piece-t-actions))
                (t nil)))
            actions))

(defun solucao (state)
    (and (null(estado-pecas-por-colocar state)) 
            (not (tabuleiro-topo-preenchido-p (estado-Tabuleiro state)))))

(defun qualidade(state)
	(setf (estado-pontos  state)(* (estado-pontos state) (- 1)))
)


(defun custo-oportunidade(state)
	(- (calculate-points (estado-pecas-colocadas state)) (estado-pontos state))
)

;;; TODO:
; (defun resultado (state action) t)

;;; Utils
(defun copy-array (arr)
    "Given an array, returns a copy of it"
    (let ((dims (array-dimensions arr)))
        (adjust-array
            (make-array dims :displaced-to arr) dims)))

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

(defun getPoints(piece)
    (cond
        ((eq piece piece-i) piece-i-value)
        ((eq piece piece-j) piece-j-value)
        ((eq piece piece-l) piece-l-value)
        ((eq piece piece-o) piece-o-value)
        ((eq piece piece-s) piece-s-value)
        ((eq piece piece-z) piece-z-value)
        ((eq piece piece-t) piece-t-value)
    (t 0)))

(defun calculate-points(l1)
    (cond
        ((not(null (first l1)))
            (+ (getPoints (first l1)) (calculate-points (rest l1))))
        (t 0)))
