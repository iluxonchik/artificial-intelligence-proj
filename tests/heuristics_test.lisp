(defparameter *test-num* 1
	"Counts the number of test being executed")

(defparameter *passed-tests* nil
	"Stores the numbers of passed tests")

(defparameter *failed-tests* nil
	"Stores the numbers of failed tests")

(defun run-test (heuristic-fun board expected-val &optional (test #'equalp))
	(let ((result nil))
		(setf result (funcall heuristic-fun board))
		(cond 
			((funcall test result expected-val)				; test passed
				(format T "Test ~d ... OK" *test-num*)
				(append *passed-tests* (list *test-num*)))
			(t 
				(format T "Test ~d ... FAILED" *test-num*)	; test failed
				(append *failed-tests* (list *test-num*))))

		(setf *test-num* (1+ *test-num*)))) 				; update counter for next test