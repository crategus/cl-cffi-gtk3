(in-package :gtk-test)

(def-suite gtk-misc :in gtk-suite)
(in-suite gtk-misc)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMisc

(test misc-class
  ;; Type check
  (is (g:type-is-object "GtkMisc"))
  ;; Check the registered name
  (is (eq 'gtk:misc
          (glib:symbol-for-gtype "GtkMisc")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMisc")
          (g:gtype (cffi:foreign-funcall "gtk_misc_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkMisc")))
  ;; Check the children
  (is (equal '("GtkArrow" "GtkImage" "GtkLabel")
             (list-children "GtkMisc")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkMisc")))
  ;; Check the class properties
  (is (equal '("xalign" "xpad" "yalign" "ypad")
             (list-properties "GtkMisc")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkMisc")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkMisc")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMisc" GTK-MISC
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_misc_get_type")
                       ((XALIGN GTK-MISC-XALIGN "xalign" "gfloat" T T)
                        (XPAD GTK-MISC-XPAD "xpad" "gint" T T)
                        (YALIGN GTK-MISC-YALIGN "yalign" "gfloat" T T)
                        (YPAD GTK-MISC-YPAD "ypad" "gint" T T)))
             (gobject:get-g-type-definition "GtkMisc"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_misc_set_alignment
;;;     gtk_misc_get_alignment

(test misc-alignment
  (let ((label (gtk:label-new "label")))
    (is (equal '(0.5 0.5) (multiple-value-list (gtk:misc-get-alignment label))))
    (gtk:misc-set-alignment label 0.1 0.2)
    (is (equal '(0.1 0.2) (multiple-value-list (gtk:misc-get-alignment label))))))

;;;     gtk_misc_set_padding
;;;     gtk_misc_get_padding

(test misc-padding
  (let ((label (gtk:label-new "label")))
    (is (equal '(0 0) (multiple-value-list (gtk:misc-get-padding label))))
    (gtk:misc-set-padding label 3 6)
    (is (equal '(3 6) (multiple-value-list (gtk:misc-get-padding label))))))

;;; --- 2023-5-29 --------------------------------------------------------------
