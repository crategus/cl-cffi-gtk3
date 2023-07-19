(in-package :gtk-test)

(def-suite gtk-spinner :in gtk-suite)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test spinner-class
  ;; Type check
  (is (g:type-is-object "GtkSpinner"))
  ;; Check the registered name
  (is (eq 'gtk:spinner
          (glib:symbol-for-gtype "GtkSpinner")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSpinner")
          (g:gtype (cffi:foreign-funcall "gtk_spinner_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkSpinner")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSpinner")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkSpinner")))
  ;; Check the class properties
  (is (equal '("active")
             (list-properties "GtkSpinner")))
  ;; Check the style properties.
  (is (equal '()
             (list-style-properties "GtkSpinner")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkSpinner")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkSpinner" GTK-SPINNER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_spinner_get_type")
                       ((ACTIVE GTK-SPINNER-ACTIVE "active" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test spinner-properties
  (let ((spinner (make-instance 'gtk:spinner)))
    (is-false (gtk:spinner-active spinner))
    (is-true (setf (gtk:spinner-active spinner) t))
    (is-true (gtk:spinner-active spinner))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spinner_new

(test spinner-new
  (is (eq 'gtk:spinner (type-of (gtk:spinner-new)))))

;;;     gtk_spinner_start
;;;     gtk_spinner_end

(test spinner-start
  (let ((spinner (gtk:spinner-new)))
    (is-false (gtk:spinner-active spinner))
    (is-false (gtk:spinner-start spinner))
    (is-true (gtk:spinner-active spinner))
    (is-false (gtk:spinner-stop spinner))
    (is-false (gtk:spinner-active spinner))))

;;; --- 2023-5-29 --------------------------------------------------------------
