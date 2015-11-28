;;;; Grupo 49: Illya Gerasymchuk (78134), Nuno Silva (78454), Jorge Heleno (79042) ;;;;
;;;; Tetris source file

;;; Pieces
(defconstant piece-i 'i)
(defconstant piece-j 'j)
(defconstant piece-l 'l)
(defconstant piece-o 'o)
(defconstant piece-s 's)
(defconstant piece-z 'z)
(defconstant piece-t 't)

;;; Piece values
(defconstant piece-i-value 800)
(defconstant piece-j-value 500)
(defconstant piece-l-value 500)
(defconstant piece-o-value 300)
(defconstant piece-s-value 300)
(defconstant piece-z-value 300)
(defconstant piece-t-value 300)

;;; Num-removed-lines-to-score hash table
(defparameter *score* (make-hash-table))
(setf (gethash 0 *score*) 0)
(setf (gethash 1 *score*) 100)
(setf (gethash 2 *score*) 300)
(setf (gethash 3 *score*) 500)
(setf (gethash 4 *score*) 800)


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
    (if (< colN (nth 1 (array-dimensions tab)))
        (let ((X (- (first (array-dimensions tab)) 1)))
            (loop while (>= X 0) do
                (cond
                    ((tabuleiro-preenchido-p tab x colN) (return-from tabuleiro-altura-coluna (+ x 1)))
                    ((= x 0) (return-from tabuleiro-altura-coluna x))
                    (t (setf x (1- x))))))))



(defun tabuleiro-linha-completa-p(tab rowN)
        (let ((x (- (nth 1 (array-dimensions tab)) 1))
            (line-has-nil T))
            (loop for i from 0 to x do
                (cond
                    ((not (tabuleiro-preenchido-p tab rowN i)) (setf line-has-nil nil))
                    (t t)))
            line-has-nil))


(defun tabuleiro-preenche!(tab rowN colN)
    (if (and (and (< rowN (first (array-dimensions tab)))
    (< colN (nth 1 (array-dimensions tab))))
    (and (>= rowN 0) (>= colN 0)))
    (setf (aref tab rowN colN) T)))

(defun tabuleiro-topo-preenchido-p(tab)
    (tabuleiro-ha-elementos-na-linha tab 17))


(defun tabuleiro-ha-elementos-na-linha(tab rowN)
    (if (and (< rowN (first (array-dimensions tab))) (>= rowN 0))
        (let ((x (- (nth 1 (array-dimensions tab)) 1)))
            (block loopBlock
                (loop while (>= x 0) do
                (cond
                    ((tabuleiro-preenchido-p tab rowN x)
                    (return-from loopBlock T))
                    ((and (not (tabuleiro-preenchido-p tab rowN x)) (= x 0)) (return-from loopBlock nil))
                    (t(setf x (1- x)))))))))

(defun tabuleiro-remove-linha!(tab rowN)
    (let ((num-of-cols (tabuleiro-num-of-cols tab))
          (num-of-rows (tabuleiro-num-of-rows tab))
          (upper-rowN (+ rowN 1))
         )
        (cond
            ((= upper-rowN num-of-rows)
                ; put an empty line in the top row
                (dotimes (i num-of-cols) (setf (aref tab rowN i) nil)))
            ((< upper-rowN num-of-rows)
                ; move the row rowN+1 to rowN
                (dotimes (i num-of-cols) (setf (aref tab rowN i) (aref tab upper-rowN i) ) )
                (tabuleiro-remove-linha! tab upper-rowN))
            (t nil))))

(defun tabuleiro-num-of-cols(tab)
    (nth 1 (array-dimensions tab)))

(defun tabuleiro-num-of-rows(tab)
    (nth 0 (array-dimensions tab)))

(defun tabuleiros-iguais-p(tab1 tab2)
    (equalp tab1 tab2))

(defun tabuleiro->array(tab)
    (copy-array tab))

(defun array->tabuleiro(tab)
    (copy-array tab))


;;; Estado [2.1.3]

(defstruct estado
    pontos
    pecas-por-colocar   ; list ordered by exit order  | legal values: i,j,l,o,s,z,t
    pecas-colocadas     ; list ordered by most recent | legal values: i,j,l,o,s,z,t
    Tabuleiro)

(defun copia-estado (state)
    (make-estado :pontos (estado-pontos state) :pecas-por-colocar (copy-list (estado-pecas-por-colocar state)) :pecas-colocadas
                         (copy-list (estado-pecas-colocadas state)) :Tabuleiro (copia-tabuleiro (estado-Tabuleiro state))))

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
    (setf (estado-pontos  state)(* (estado-pontos state) (- 1))))


(defun custo-oportunidade(state)
    (- (calculate-points (estado-pecas-colocadas state)) (estado-pontos state)))


(defun empty-lines-above-column (piece col max-line)
    (let ((counter 0))
        (loop for i from 0 to max-line do
            (cond
                ((equalp nil (aref piece i col)) (setf counter (incf counter)))
                (T (return-from empty-lines-above-column counter))
            )
        )
        counter
))


(defun resultado (state action)
    (let* (
        (state-copy (copia-estado state))
        (column (accao-coluna action))
        (real-piece (first (estado-pecas-por-colocar state-copy)))
        (piece (accao-peca action))
        (tab (estado-Tabuleiro state-copy))
        (tab-arr (tabuleiro->array tab))
        ; TODO: run through the piece and determine the highest column
        (column-height) 
        (piece-lines (1- (nth 0 (array-dimensions piece))))
        (piece-columns (1- (nth 1 (array-dimensions piece))))
        (tab-num-of-lines (tabuleiro-num-of-rows tab))
        (tab-num-of-cols (tabuleiro-num-of-cols tab)))

        ;; Decide the column-height to use
        (let ((line-val (list)) (max-line-val-index 0) (max-val 0))

            (loop for i from 0 to piece-columns do
                (cond
                    ;; If entry is nil, set that line-val position to be column height - number of empty spaces above
                    ((equalp nil (aref piece 0 i))
                        (setf line-val (append line-val (list (- (tabuleiro-altura-coluna tab (+ i column)) (empty-lines-above-column piece i piece-lines))))))
                    (T (setf line-val (append line-val  (list (tabuleiro-altura-coluna tab (+ i column))))))))

            ;; Find index with maximum value in the list. This + piece's leftmost column will be the column-height
            (loop for i from 0 to (1- (list-length line-val)) do
                (cond
                    ((> (nth i line-val) max-val) (setf max-val (nth i line-val)) (setf max-line-val-index i))
                    (T t)))
        (setf column-height max-val))

        ;; TODO: put this in a separate helper function
        ;; Place piece on the tab
        (loop for i from 0 to piece-lines do
            (let* (
                (tab-line-index (+ column-height i)))
                (if (>= tab-line-index tab-num-of-lines) (loop-finish))
                (loop for j from 0 to piece-columns do
                    (let* (
                        (tab-col-index (+ column j)))
                        (if (>= tab-col-index tab-num-of-cols) (loop-finish))
                        ;; Only place the piece part if the position is free (to avoid overriden pieces)
                        (if (equalp (aref tab-arr tab-line-index tab-col-index) nil) 
                            (setf (aref tab-arr tab-line-index tab-col-index) (aref piece i j)))))))
                        

        ;; Update tab
        (setf tab (array->tabuleiro tab-arr))

        ;; remove piece from pecas-por-colocar
        (setf (estado-pecas-por-colocar state-copy)
            (remove real-piece (estado-pecas-por-colocar state-copy) :test #'equal :count 1))

        ;; add piece to pecas-colocadas
        (setf (estado-pecas-colocadas state-copy)
            (append (list real-piece) (estado-pecas-colocadas state-copy)))

        ;; Top of tab is filled, return the resulting state
        (if (tabuleiro-topo-preenchido-p tab)
            (progn
                (setf (estado-Tabuleiro state-copy) tab)
                (return-from resultado state-copy)))

        ;; Remove the necessary lines
        (let((num-removed-lines 0))
            (loop for i from 0 to piece-lines do
                (if (tabuleiro-linha-completa-p tab i)
                    (progn
                        (incf num-removed-lines)
                        (tabuleiro-remove-linha! tab i)
                        (decf i)
                        )))
            ;; Update the score
            (setf (estado-pontos state-copy)
                (+ (estado-pontos state-copy) (gethash num-removed-lines *score*))))

            ;; Update the state
            (setf (estado-Tabuleiro state-copy) tab)

        state-copy ; return the updated state
))

;;; Procuras [2.2.2]

;; procura-A*: problema x heuristica -> lista acoes
;; Funcao que recebe um problema e uma fx h, e faz A*.
;; Devolve uma lista com as acoes que levam a' solucao encontrada.
;;  heuristica: estado -> inteiro
(defun procura-A* (problem heuristic)
    (let* (
            (curr-state (problema-estado-inicial problem))
            (open (list curr-state)) ; open list initially contains the start state
            (closed (make-hash-table :test #'equalp)) ; T if state has benn closed
            (parent-state (make-hash-table :test #' equalp)) ; parent of a state
            (parent-action (make-hash-table :test #'equalp)) ; parent action of a state
            (f (make-hash-table :test #'equalp)) ; f function of a state
            (g (make-hash-table :test #'equalp)) ; g function of a state
            (h (make-hash-table :test #'equalp)) ; h function of a state
            (state-actions nil)
            (child-state nil)
            ; this does not work :(   \/
            ;(solucao (problema-solucao problem)) ; funcao solucao do problema
            ;(accoes (problema-accoes problem))   ; funcao accoes  do problema
            ;(resultado (problema-resultado problem))  ; funcao resultado do problema
            ;(custo (problema-custo-caminho problem))  ; funcao custo-caminho do problema
            (temp-g 0)
            (temp-h 0)
        )
        (loop ; while solution not found
            (if (null open) (return nil)) ; no solution found

            ; sort list of open states according to their f value
            (stable-sort open #'(lambda (x y)
                ( < (gethash x f) (gethash y f)) ) ; sort ascending
            )

            (setf curr-state (first open)) ; the first state from queue
            (setf open (rest open)) ; remove element from queue
            (if (funcall (problema-solucao problem) curr-state) (return nil)) ; TODO: create list of actions

            (if (not (gethash curr-state closed)) ; if not already visited
                (progn
                    (setf (gethash curr-state closed) t) ; mark state as visited

                    (setf state-actions (funcall (problema-accoes problem) curr-state)) ; gen possible actions
                    ; foreach action a in state-actions
                    (dolist (a state-actions)
                        (setf child-state (funcall (problema-resultado problem) curr-state a)) ; apply action to state
                        (setf open (append (list child-state) open)) ; append to beggining of list.
                        ; /\ Como usamos stable sort, se dois estados tiverem
                        ;    f igual vai ser selecionado o ultimo a entrar, ou seja,
                        ;    o que esta' mais perto do inicio da lista.

                        (setf (gethash child-state parent-state) curr-state) ; register parent state
                        (setf (gethash child-state parent-action) a) ; register parent action

                        ; calculate g, h, and f
                        (setf temp-g (funcall (problema-custo-caminho problem) child-state)) ; calculate g
                        (setf (gethash child-state g) temp-g) ; store g

                        (setf temp-h (funcall heuristic child-state)) ; calculate h
                        (setf (gethash child-state h) temp-h) ; store h

                        ; store and calculate f
                        (setf (gethash child-state f) (+ temp-g temp-h) )
                    )
                )
            )

        )
    )
)


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


;;; Uncomment Line 1 AND comment line 2 (below) when submitting to Mooshak
;;; Uncomment Line 2 AND comment line 1 (below) when using locally
;;;(load "utils.fas")           ; line 1
(load "../libs/utils.lisp")     ; line 2
