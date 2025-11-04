(defun add-prev-fn (&key (transform #'identity)) 
  (let ((prev nil))
    (lambda (current)
      (let ((result (cons (funcall transform current) 
                          (if prev (funcall transform prev)))))
        (setf prev current) 
        result))))
		
(defun check-add-prev-fn (name input expected &key (transform #'identity))
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (mapcar (add-prev-fn :transform transform) input) expected)
          name))


(defun test-add-prev-fn ()
  (check-add-prev-fn "test 1" '(1 2 3) '((1 . NIL) (2 . 1) (3 . 2)))
  (check-add-prev-fn "test 2" '(42) '((42 . NIL)))
  (check-add-prev-fn "test 3" '() '())
  (check-add-prev-fn "test 4" '(1 2 3) '((2 . NIL) (3 . 2) (4 . 3)) :transform #'1+)
  (check-add-prev-fn "test 5" '(1 2 3) '((2 . NIL) (4 . 2) (6 . 4)) :transform #'(lambda (x) (* 2 x))))
