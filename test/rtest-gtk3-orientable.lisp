(in-package :gtk-test)

(def-suite gtk-orientable :in gtk-suite)
(in-suite gtk-orientable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkOrientable

(test orientable-interface
  ;; Type check
  (is (g:type-is-interface "GtkOrientable"))
  ;; Check the registered name
  (is (eq 'gtk:orientable
          (glib:symbol-for-gtype "GtkOrientable")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkOrientable")
          (g:gtype (cffi:foreign-funcall "gtk_orientable_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '("orientation")
             (list-interface-properties "GtkOrientable")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkOrientable" GTK-ORIENTABLE
                    (:EXPORT T :TYPE-INITIALIZER "gtk_orientable_get_type")
                    (ORIENTATION GTK-ORIENTABLE-ORIENTATION "orientation"
                     "GtkOrientation" T T))
             (gobject:get-g-type-definition "GtkOrientable"))))

;;; --- Properties -------------------------------------------------------------

;;;     orientation

(test orientable-properties
  (let ((box (gtk:box-new :horizontal)))
    (is (eq :horizontal (gtk:orientable-orientation box)))
    (is (eq :vertical (setf (gtk:orientable-orientation box) :vertical)))
    (is (eq :vertical (gtk:orientable-orientation box)))))

;;; --- 2023-5-29 --------------------------------------------------------------
