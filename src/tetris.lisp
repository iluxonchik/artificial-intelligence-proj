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

;;; piece to piece-conf hash table (populated at the end of the file)
(defparameter *piece-confs* (make-hash-table))

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
    (nth 1 (array-dimensions (tabuleiro->array tab))))

(defun tabuleiro-num-of-rows(tab)
    (nth 0 (array-dimensions (tabuleiro->array tab))))

(defun tabuleiros-iguais-p(tab1 tab2)
    (equalp tab1 tab2))

(defun tabuleiro->array(tab)
    (copy-array tab))

(defun array->tabuleiro(tab)
    (copy-array tab))


;;; Estado [2.1.3]

(defstruct estado
    (pontos 0)
    pecas-por-colocar   ; list ordered by exit order  | legal values: i,j,l,o,s,z,t
    pecas-colocadas     ; list ordered by most recent | legal values: i,j,l,o,s,z,t
    (Tabuleiro (cria-tabuleiro)))

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
    (let ((actions (list))) ; stores the resulting list of actions

        ;; return empty list if this is a terminal state
        (if (estado-final-p state) (return-from accoes nil))

        ;; create actions for the next piece
        (let ((piece (first (estado-pecas-por-colocar state))))
            (dolist (p (gethash piece *piece-confs*))
                (setf actions (append actions (generate-piece-actions p)))))
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
                (T (return-from empty-lines-above-column counter))))
        counter))


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
        ;; NOTE: will override other pieces in case of conflict
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
                        (decf i))))
            ;; Update the score
            (setf (estado-pontos state-copy)
                (+ (estado-pontos state-copy) (gethash num-removed-lines *score*))))

            ;; Update the state
            (setf (estado-Tabuleiro state-copy) tab)

        state-copy)) ; return the updated state


;;; [2.2.2] Procuras

;; TODO: actions-to-state should be a lambda inside procura-pp
(defun actions-to-state (state parent-state parent-action)
    "Given a state, a parent-state hashmap and a parent-action hashmap returns a list of actions to state"
    (let ((action-list nil)
           (temp-action nil)
           (temp-state state))

    (setf temp-action (gethash temp-state parent-action))
    (if (equalp temp-action nil) (return-from actions-to-state nil)) ; passed state is initial state

    (loop

        (if (equalp temp-action nil) (return-from actions-to-state action-list)) ; reached intial state
        (setf action-list (append (list temp-action) action-list)) ; add action to actions list

        ;; Update temp-state and temp-action values
        (setf temp-state (gethash temp-state parent-state))
        (setf temp-action (gethash temp-state parent-action)))))

(defun procura-pp (problem)
    (let* ((curr-state (problema-estado-inicial problem))
           (open (list curr-state)) ; open list, intially contains the start state
           (closed (make-hash-table :test #'equalp)) ; T if state has been closed
           (parent-state (make-hash-table :test #'equalp)) ; parent of a state
           (parent-action (make-hash-table :test #'equalp)) ; parent action of a state
           (state-actions nil)
           (child-state nil))

           (loop
                (if (null open) (return nil))  ; no solution found, return empty list

                (setf curr-state (first open)) ; get first state from open states list
                (setf open (rest open))        ; remove element from list
                (if (funcall (problema-solucao problem) curr-state) (return (actions-to-state curr-state parent-state parent-action))) ; end state reached, return solution
                ;; TODO: move into a separate function ?
                (cond
                    ((not (gethash curr-state closed))  ; if current state hasn't been visited before
                        (setf (gethash curr-state closed) t) ; mark current state as visited

                        (setf state-actions (funcall (problema-accoes problem) curr-state))
                        ;; foreach(action a in actions)
                        (dolist (a state-actions)
                            (setf child-state (funcall (problema-resultado problem) curr-state a)) ; apply action to state
                            (setf open (append (list child-state) open)) ; LIFO
                            (setf (gethash child-state parent-state) curr-state) ; register parent state of new state
                            (setf (gethash child-state parent-action) a))) ; register parent action of new state

                    (t t)))))

(defun procura-best(board pieces)
    (return-from procura-best (procura-pp (make-problema :estado-inicial (make-estado :pontos 0 :pecas-por-colocar pieces :pecas-colocadas '() :Tabuleiro board )
                                        :solucao #'solucao :accoes #'accoes
                                        :resultado #'resultado :custo-caminho #'compute-score))))


;;; Heuristic Functions

(defun aggregateHeight(board)
    (let ((height 0) (maxCol 10) (i 0))
        (loop while (< i maxCol) do
            (setf height (+ height (tabuleiro-altura-coluna board i)))
            (setf i (1+ i)))
        (return-from aggregateHeight height)))

(defun completeLines(board)
    (let ((numCompLines 0)
          (maxLine 18)
          (i 0))
        (loop while (< i maxLine) do
            (cond
                ((tabuleiro-linha-completa-p board i) (setf numCompLines (1+ numCompLines) ) (setf i (1+ i)))
                (t (setf i (1+ i)))))
        (return-from completeLines numCompLines)))

(defun numHolesCol(board col)
    (let ((i 0) (holecount 0)(height (tabuleiro-altura-coluna board col)))
            (loop while(< i height) do
                (cond

                  ((not (tabuleiro-preenchido-p board i col)) (setf holecount (1+ holecount)) (setf i (1+ i)))
                  (t (setf i (1+ i)))))
            (return-from numHolesCol holecount)))

(defun numHoles(board)
    (let ((i 0) (holeCount 0) (maxCol 10))
        (loop while(< i maxCol) do
            (setf holeCount (+ holeCount (numHolesCol board i)))
            (setf i (1+ i)))
        (return-from numHoles holeCount)))


(defun bumpiness(board)
    (let ((i 1) (bump 0) (maxCol 10))
      (loop while(< i  maxCol) do
        (setf bump (+ bump (abs (- (tabuleiro-altura-coluna board (1- i)) (tabuleiro-altura-coluna board  i) ))))
        (setf i (+ 1 i)))
      (return-from bumpiness bump)))

(defun compute-score(state)
    (return-from compute-score (+ (* 1 (aggregateHeight (estado-Tabuleiro state))) (* 1 (completeLines (estado-Tabuleiro state)))
    (* 1 (numHoles (estado-Tabuleiro state))) (* 1 (bumpiness (estado-Tabuleiro state)))

    ))
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


;;; TODO: this doesn't really belong here
;;; Possible piece configurations (for *piece-confs* hash)
(defconstant piece-i-confs (list peca-i0 peca-i1))
(defconstant piece-l-confs (list peca-l0 peca-l1 peca-l2 peca-l3))
(defconstant piece-j-confs (list peca-j0 peca-j1 peca-j2 peca-j3))
(defconstant piece-o-confs (list peca-o0))
(defconstant piece-s-confs (list peca-s0 peca-s1))
(defconstant piece-z-confs (list peca-z0 peca-z1))
(defconstant piece-t-confs (list peca-t0 peca-t1 peca-t2 peca-t3))

;;; piece to piece-conf hash table (for accoes)
(setf (gethash piece-i *piece-confs*) piece-i-confs)
(setf (gethash piece-l *piece-confs*) piece-l-confs)
(setf (gethash piece-j *piece-confs*) piece-j-confs)
(setf (gethash piece-o *piece-confs*) piece-o-confs)
(setf (gethash piece-s *piece-confs*) piece-s-confs)
(setf (gethash piece-z *piece-confs*) piece-z-confs)
(setf (gethash piece-t *piece-confs*) piece-t-confs)
