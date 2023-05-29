(in-package :gtk-test)

(def-suite gtk-accel-label :in gtk-suite)
(in-suite gtk-accel-label)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccelLabel

(test accel-label-class
  ;; Type check
  (is (g:type-is-object "GtkAccelLabel"))
  ;; Check the registered name
  (is (eq 'gtk:accel-label
          (glib:symbol-for-gtype "GtkAccelLabel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccelLabel")
          (g:gtype (cffi:foreign-funcall "gtk_accel_label_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkLabel") (g:type-parent "GtkAccelLabel")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAccelLabel")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkAccelLabel")))
  ;; Check the class properties
  (is (equal '("accel-closure" "accel-widget")
             (list-properties "GtkAccelLabel")))
  ;; Check the style properties.
  (is (equal '()
             (list-style-properties "GtkAccelLabel")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAccelLabel")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAccelLabel" GTK-ACCEL-LABEL
                       (:SUPERCLASS GTK-LABEL :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_accel_label_get_type")
                       ((ACCEL-CLOSURE GTK-ACCEL-LABEL-ACCEL-CLOSURE
                         "accel-closure" "GClosure" T T)
                        (ACCEL-WIDGET GTK-ACCEL-LABEL-ACCEL-WIDGET
                         "accel-widget" "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkAccelLabel"))))

;;; --- Properties -------------------------------------------------------------

(test accel-label-properties
  (let ((accel-label (make-instance 'gtk:accel-label :label "text")))
    ;; TODO: GClosure is in C implemented as a boxed type, but not in Lisp
    ;; therefore we get an error with the accessor
;    (is-false (gtk:accel-label-accel-closure accel-label))
    (is-false (gtk:accel-label-accel-widget accel-label))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accel_label_new

(test accel-label-new
  (is (eq 'gtk:accel-label (gtk:accel-label-new "text"))))

;;;     gtk_accel_label_get_accel_width

(test accel-label-new
  (let ((accel-label (gtk:accel-label-new "text")))
    (is (= 0 (gtk:accel-label-accel-width accel-label)))))

;;;     gtk_accel_label_set_accel
;;;     gtk_accel_label_get_accel

(test accel-label-accel
  (let ((accel-label (gtk:accel-label-new "text")))
    (is-false (gtk:accel-label-set-accel accel-label
                                         (gdk:keyval-from-name "p")
                                         :control-mask))
    (multiple-value-bind (key mods)
        (gtk:accel-label-get-accel accel-label)
      (is (= 112 key))
      (is (equal '(:control-mask) mods)))))

;;;     gtk_accel_label_refetch

;;; --- 2023-5-29 --------------------------------------------------------------
