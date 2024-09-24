(in-package :gtk-test)

(def-suite gtk-spinner :in gtk-suite)
(in-suite gtk-spinner)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSpinner

(test gtk-spinner-class
  ;; Check type
  (is (g:type-is-object "GtkSpinner"))
  ;; Check registered name
  (is (eq 'gtk:spinner
          (glib:symbol-for-gtype "GtkSpinner")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSpinner")
          (g:gtype (cffi:foreign-funcall "gtk_spinner_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkSpinner")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSpinner")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkSpinner")))
  ;; Check class properties
  (is (equal '("active")
             (glib-test:list-properties "GtkSpinner")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkSpinner")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSpinner")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSpinner" GTK:SPINNER
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_spinner_get_type")
                       ((ACTIVE SPINNER-ACTIVE "active" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkSpinner"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-spinner-properties
  (let ((spinner (make-instance 'gtk:spinner)))
    (is-false (gtk:spinner-active spinner))
    (is-true (setf (gtk:spinner-active spinner) t))
    (is-true (gtk:spinner-active spinner))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_spinner_new

(test gtk-spinner-new
  (is (eq 'gtk:spinner (type-of (gtk:spinner-new)))))

;;;     gtk_spinner_start
;;;     gtk_spinner_end

(test gtk-spinner-start
  (let ((spinner (gtk:spinner-new)))
    (is-false (gtk:spinner-active spinner))
    (is-false (gtk:spinner-start spinner))
    (is-true (gtk:spinner-active spinner))
    (is-false (gtk:spinner-stop spinner))
    (is-false (gtk:spinner-active spinner))))

;;; 2024-9-22
