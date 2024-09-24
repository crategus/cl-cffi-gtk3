(in-package :gtk-test)

(def-suite gtk-misc :in gtk-suite)
(in-suite gtk-misc)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMisc

(test gtk-misc-class
  ;; Check type
  (is (g:type-is-object "GtkMisc"))
  ;; Check registered name
  (is (eq 'gtk:misc
          (glib:symbol-for-gtype "GtkMisc")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMisc")
          (g:gtype (cffi:foreign-funcall "gtk_misc_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkMisc")))
  ;; Check children
  (is (equal '("GtkArrow" "GtkImage" "GtkLabel")
             (glib-test:list-children "GtkMisc")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkMisc")))
  ;; Check class properties
  (is (equal '("xalign" "xpad" "yalign" "ypad")
             (glib-test:list-properties "GtkMisc")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkMisc")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMisc")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMisc" GTK:MISC
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_misc_get_type")
                       ((XALIGN MISC-XALIGN "xalign" "gfloat" T T)
                        (XPAD MISC-XPAD "xpad" "gint" T T)
                        (YALIGN MISC-YALIGN "yalign" "gfloat" T T)
                        (YPAD MISC-YPAD "ypad" "gint" T T)))
             (gobject:get-gtype-definition "GtkMisc"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_misc_set_alignment
;;;     gtk_misc_get_alignment

(test gtk-misc-alignment
  (let ((label (gtk:label-new "label")))
    (is (equal '(0.5 0.5) (multiple-value-list (gtk:misc-get-alignment label))))
    (gtk:misc-set-alignment label 0.1 0.2)
    (is (equal '(0.1 0.2) (multiple-value-list (gtk:misc-get-alignment label))))))

;;;     gtk_misc_set_padding
;;;     gtk_misc_get_padding

(test gtk-misc-padding
  (let ((label (gtk:label-new "label")))
    (is (equal '(0 0) (multiple-value-list (gtk:misc-get-padding label))))
    (gtk:misc-set-padding label 3 6)
    (is (equal '(3 6) (multiple-value-list (gtk:misc-get-padding label))))))

;;; 2024-9-21
