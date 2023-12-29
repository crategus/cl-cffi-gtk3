(in-package :gtk-test)

(def-suite gtk-alignment :in gtk-suite)
(in-suite gtk-alignment)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAlignment

(test gtk-alignment-class
  ;; Type check
  (is (g:type-is-object "GtkAlignment"))
  ;; Check the registered name
  (is (eq 'gtk:alignment
          (glib:symbol-for-gtype "GtkAlignment")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAlignment")
          (g:gtype (cffi:foreign-funcall "gtk_alignment_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkAlignment")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAlignment")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkAlignment")))
  ;; Check the class properties
  (is (equal '("bottom-padding" "left-padding" "right-padding" "top-padding"
               "xalign" "xscale" "yalign" "yscale")
             (list-properties "GtkAlignment")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkAlignment")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkAlignment")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAlignment")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkAlignment")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAlignment" GTK-ALIGNMENT
                               (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_alignment_get_type")
                               ((BOTTOM-PADDING GTK-ALIGNMENT-BOTTOM-PADDING
                                 "bottom-padding" "guint" T T)
                                (LEFT-PADDING GTK-ALIGNMENT-LEFT-PADDING
                                 "left-padding" "guint" T T)
                                (RIGHT-PADDING GTK-ALIGNMENT-RIGHT-PADDING
                                 "right-padding" "guint" T T)
                                (TOP-PADDING GTK-ALIGNMENT-TOP-PADDING
                                 "top-padding" "guint" T T)
                                (XALIGN GTK-ALIGNMENT-XALIGN "xalign" "gfloat"
                                 T T)
                                (XSCALE GTK-ALIGNMENT-XSCALE "xscale" "gfloat"
                                 T T)
                                (YALIGN GTK-ALIGNMENT-YALIGN "yalign" "gfloat"
                                 T T)
                                (YSCALE GTK-ALIGNMENT-YSCALE "yscale" "gfloat"
                                 T T)))
             (gobject:get-g-type-definition "GtkAlignment"))))

;;; --- Properties -------------------------------------------------------------

;;;     bottom-padding
;;;     left-padding
;;;     right-padding
;;;     top-padding
;;;     xalign
;;;     xscale
;;;     yalign
;;;     yscale

(test gtk-alignment-properties
  (let ((alignment (make-instance 'gtk:alignment)))
    (is (= 0 (gtk:alignment-bottom-padding alignment)))
    (is (= 0 (gtk:alignment-left-padding alignment)))
    (is (= 0 (gtk:alignment-right-padding alignment)))
    (is (= 0 (gtk:alignment-top-padding alignment)))
    (is (= 0.5 (gtk:alignment-xalign alignment)))
    (is (= 1.0 (gtk:alignment-xscale alignment)))
    (is (= 0.5 (gtk:alignment-yalign alignment)))
    (is (= 1.0 (gtk:alignment-yscale alignment)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_alignment_new

(test gtk-alignment-new
  (is (typep (gtk:alignment-new 0.5 0.0 1.0 1.0) 'gtk:alignment))
  (is (typep (gtk:alignment-new 1/2 3/4 1 1.0d0) 'gtk:alignment)))

;;;     gtk_alignment_set

(test gtk-alignment-set
  (let ((alignment (make-instance 'gtk:alignment)))
    (is-false (gtk:alignment-set alignment 0.5 0.0 1.0 1.0))
    (is-false (gtk:alignment-set alignment 1/2 3/4 1 1.0d0))))

;;;     gtk_alignment_get_padding
;;;     gtk_alignment_set_padding

(test gtk-alignment-get/set-padding
  (let ((alignment (make-instance 'gtk:alignment)))
    (is-false (gtk:alignment-set-padding alignment 10 20 30 40))
    (is (equal '(10 20 30 40)
               (multiple-value-list (gtk:alignment-get-padding alignment))))))

;;; 2023-12-28
