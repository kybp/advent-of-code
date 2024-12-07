(defun list-difference (list1 list2)
  (loop for x in (sort list1 #'<)
        for y in (sort list2 #'<)
        summing (abs (- x y))))

(defun read-lists (filespec)
  (with-open-file (input filespec)
    (loop for x = (read input nil)
          for y = (read input nil)
          while (and x y)
          collect x into list1
          collect y into list2
          finally (return (list list1 list2)))))

(print (apply #'list-difference (read-lists "input.txt")))
