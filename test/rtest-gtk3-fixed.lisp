(in-package :gtk-test)

(def-suite gtk-fixed :in gtk-suite)
(in-suite gtk-fixed)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFixed

(test gtk-fixed-class
  ;; Type check
  (is (g:type-is-object "GtkFixed"))
  ;; Check the registered name
  (is (eq 'gtk:fixed
          (glib:symbol-for-gtype "GtkFixed")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFixed")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkFixed")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFixed")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkFixed")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkFixed")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFixed")))
  ;; Check the child properties
  (is (equal '("x" "y")
             (list-child-properties "GtkFixed")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFixed")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFixed" GTK-FIXED
                       (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_fixed_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFixed"))))

;;; --- Child Properties -------------------------------------------------------

;;;     x
;;;     y

(test gtk-fixed-child-properties
  (let ((fixed (make-instance 'gtk:fixed))
        (button (gtk:button-new)))
    (is-false (gtk:container-add fixed button))
    (is (= 0 (gtk:fixed-child-x fixed button)))
    (is (= 0 (gtk:fixed-child-y fixed button)))
    (is (= 10 (setf (gtk:fixed-child-x fixed button) 10)))
    (is (= 20 (setf (gtk:fixed-child-y fixed button) 20)))
    (is (= 10 (gtk:fixed-child-x fixed button)))
    (is (= 20 (gtk:fixed-child-y fixed button)))))

(test gtk-fixed-child-x-property
  (is (equal '(PROGN
                (DEFUN FIXED-CHILD-X (GTK:CONTAINER GTK::CHILD)
                  (GTK:CONTAINER-CHILD-PROPERTY GTK:CONTAINER
                                                GTK::CHILD
                                                "x" "gint"))
                (DEFUN (SETF FIXED-CHILD-X) (GTK::VALUE GTK:CONTAINER GTK::CHILD)
                  (SETF (GTK:CONTAINER-CHILD-PROPERTY GTK:CONTAINER
                                                      GTK::CHILD
                                                      "x" "gint")
                        GTK::VALUE)
                  GTK::VALUE)
                (EXPORT 'FIXED-CHILD-X))
             (macroexpand '(gtk::define-child-property fixed-child-x
                                                       "x" "gint" t t t)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_fixed_new

(test gtk-fixed-new
  (is (typep (gtk:fixed-new) 'gtk:fixed)))

;;;     gtk_fixed_put
;;;     gtk_fixed_move

(test gtk-fixed-put/move
  (let ((fixed (gtk:fixed-new))
        (button (gtk:button-new)))
    (gtk:fixed-put fixed button 10 20)
    (is (= 10 (gtk:fixed-child-x fixed button)))
    (is (= 20 (gtk:fixed-child-y fixed button)))
    (gtk:fixed-move fixed button 15 25)
    (is (= 15 (gtk:fixed-child-x fixed button)))
    (is (= 25 (gtk:fixed-child-y fixed button)))))

;;; 2024-1-2
