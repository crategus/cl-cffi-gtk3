(in-package :gtk-test)

(def-suite gtk-accel-label :in gtk-suite)
(in-suite gtk-accel-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccelLabel

(test gtk-accel-label-class
  ;; Check type
  (is (g:type-is-object "GtkAccelLabel"))
  ;; Check registered name
  (is (eq 'gtk:accel-label
          (glib:symbol-for-gtype "GtkAccelLabel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccelLabel")
          (g:gtype (cffi:foreign-funcall "gtk_accel_label_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLabel") (g:type-parent "GtkAccelLabel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAccelLabel")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkAccelLabel")))
  ;; Check class properties
  (is (equal '("accel-closure" "accel-widget")
             (glib-test:list-properties "GtkAccelLabel")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkAccelLabel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAccelLabel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAccelLabel" GTK:ACCEL-LABEL
                       (:SUPERCLASS GTK:LABEL
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_accel_label_get_type")
                       ((ACCEL-CLOSURE ACCEL-LABEL-ACCEL-CLOSURE
                         "accel-closure" "GClosure" T T)
                        (ACCEL-WIDGET ACCEL-LABEL-ACCEL-WIDGET
                         "accel-widget" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkAccelLabel"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-accel-label-properties
  (let ((accel-label (make-instance 'gtk:accel-label :label "text")))
    ;; TODO: GClosure is in C implemented as a boxed type, but not in Lisp
    ;; therefore we get an error with the accessor
;    (is-false (gtk:accel-label-accel-closure accel-label))
    (is-false (gtk:accel-label-accel-widget accel-label))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accel_label_new

(test gtk-accel-label-new
  (is (eq 'gtk:accel-label (gtk:accel-label-new "text"))))

;;;     gtk_accel_label_get_accel_width

(test gtk-accel-label-new
  (let ((accel-label (gtk:accel-label-new "text")))
    (is (= 0 (gtk:accel-label-accel-width accel-label)))))

;;;     gtk_accel_label_set_accel
;;;     gtk_accel_label_get_accel

(test gtk-accel-label-accel
  (let ((accel-label (gtk:accel-label-new "text")))
    (is-false (gtk:accel-label-set-accel accel-label
                                         (gdk:keyval-from-name "p")
                                         :control-mask))
    (multiple-value-bind (key mods)
        (gtk:accel-label-get-accel accel-label)
      (is (= 112 key))
      (is (equal '(:control-mask) mods)))))

;;;     gtk_accel_label_refetch

;;; 2024-9-22
