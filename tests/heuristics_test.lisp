(load "../src/tetris.lisp")

(defparameter *test-num* 1
	"Counts the number of test being executed")

(defparameter *passed-tests* NIL 				; list, in case passed and failed test nums are needed
	"Stores the numbers of passed tests")

(defparameter *failed-tests* nil
	"Stores the numbers of failed tests")		; list, in case passed and failed test nums are needed

;;; Runs a single test
;;; Parameters:
;;;		heuristic-fun - heuristic function to test
;;;		board - game board
;;;		expected-val - expected result of applying the heuristic function to the board
;;;		test - test function to use [OPTIONAL, defaults to #'equalp]
(defun run-test (heuristic-fun board expected-val &optional (test #'equalp))
	(let ((result nil))
		(setf result (funcall heuristic-fun board))
		(cond 
			((funcall test result expected-val)				; test passed
				(format T "Test ~d ... OK~%" *test-num*)
				(setf *passed-tests* (append *passed-tests* (list *test-num*))))
			(t 
				(format T "Test ~d ... FAILED | " *test-num*)	; test failed
				(format T "Expected: ~d; Obtained: ~d~%" expected-val result)
				(setf *failed-tests* (append *failed-tests* (list *test-num*)))))

		(setf *test-num* (1+ *test-num*)))) 				; update counter for next test

;;; Runs the tests and outputs the results
(defun run-tests ()
	(run-test #'aggregateHeight (create-board-1) 48)
	(run-test #'completeLines (create-board-1) 2)
	(run-test #'numHoles (create-board-1) 2)
	(run-test #'bumpiness (create-board-1) 6)
	
	(format T "~%")
	(format T "~d PASSED; ~d FAILED~%" (length *passed-tests*) (length *failed-tests*)))


;;; Functions for creating various boards

;;; NOTE: due to lack of time, all of them were done in a fast-hacky way, just to get 
;;; the job done (I mean, this whole file is), but hey, it's better to have this than
;;; no tests at all. Tests are good.

;;;Expected:
;;;    Aggregate Height: 48
;;;    Complete Lines: 2
;;;    Holes: 2
;;;    Bumpiness: 6
(defun create-board-1 ()
	#2A((T T T T T T T T T T)
    (T T T NIL T T T T T T)
    (T T T T T T T T T T)
    (NIL T T NIL T T T T T T)
    (NIL T T T T T T NIL NIL T)
	(NIL NIL NIL NIL T T NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))
