(in-package :gtk-test)

(def-suite gtk-alignment :in gtk-suite)
(in-suite gtk-alignment)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAlignment

(test gtk-alignment-class
  ;; Check type
  (is (g:type-is-object "GtkAlignment"))
  ;; Check registered name
  (is (eq 'gtk:alignment
          (glib:symbol-for-gtype "GtkAlignment")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAlignment")
          (g:gtype (cffi:foreign-funcall "gtk_alignment_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkAlignment")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAlignment")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkAlignment")))
  ;; Check class properties
  (is (equal '("bottom-padding" "left-padding" "right-padding" "top-padding"
               "xalign" "xscale" "yalign" "yscale")
             (glib-test:list-properties "GtkAlignment")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkAlignment")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkAlignment")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAlignment")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkAlignment")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAlignment" GTK:ALIGNMENT
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_alignment_get_type")
                       ((BOTTOM-PADDING ALIGNMENT-BOTTOM-PADDING
                         "bottom-padding" "guint" T T)
                        (LEFT-PADDING ALIGNMENT-LEFT-PADDING
                         "left-padding" "guint" T T)
                        (RIGHT-PADDING ALIGNMENT-RIGHT-PADDING
                         "right-padding" "guint" T T)
                        (TOP-PADDING ALIGNMENT-TOP-PADDING
                         "top-padding" "guint" T T)
                        (XALIGN ALIGNMENT-XALIGN "xalign" "gfloat" T T)
                        (XSCALE ALIGNMENT-XSCALE "xscale" "gfloat" T T)
                        (YALIGN ALIGNMENT-YALIGN "yalign" "gfloat" T T)
                        (YSCALE ALIGNMENT-YSCALE "yscale" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkAlignment"))))

;;; --- Properties -------------------------------------------------------------

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

;;; 2024-9-23
