(in-package :gtk-test)

(def-suite gtk-version :in gtk-suite)
(in-suite gtk-version)

;;; --- Functions --------------------------------------------------------------

;;;     gtk_get_major_version
;;;     gtk_get_minor_version
;;;     gtk_get_micro_version

(test gtk-major-version
  (is (=  3 (gtk:major-version)))
  (is (= 24 (gtk:minor-version)))
  (is (= 38 (gtk:micro-version))))

;;;     gtk_get_binary_age
;;;     gtk_get_interface_age

;;;     gtk_check_version

(test gtk-check-version
  (is-false (gtk:check-version 3 24 37))
  (is (string= "GTK+ version too new (major mismatch)"
               (gtk:check-version 2 0 0))))

;;; --- 2023-11-4 --------------------------------------------------------------
