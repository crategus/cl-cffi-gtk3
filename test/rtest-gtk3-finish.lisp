(in-package :gtk-test)

(in-suite gtk-test)

(test gtk-test-finished
  (cond (*first-run-testsuite*
         (setf *first-run-testsuite* nil)
         (format t "~%First run of the gtk-test suite finished.~%"))
        (t
         (format t "~%Second or more run of the gtk-test suite finished.~%"))))

;;; 2025-4-26
