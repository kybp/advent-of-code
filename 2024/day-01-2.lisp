(defun membership-set (list)
  (loop with seen = (make-hash-table)
        for element in list do
          (setf (gethash element seen) t)
        finally (return seen)))

(defun seenp (item membership-set)
  (gethash item membership-set))

(defun similarity (list1 list2)
  (loop with list1-items = (membership-set list1)
        for element in list2
        if (seenp element list1-items)
          sum element))

(defun read-lists (filespec)
  (with-open-file (input filespec)
    (loop for x = (read input nil)
          for y = (read input nil)
          while (and x y)
          collect x into list1
          collect y into list2
          finally (return (list list1 list2)))))

(format t "~&~a~%" (apply #'similarity (read-lists "2024/input/day-01.txt")))
