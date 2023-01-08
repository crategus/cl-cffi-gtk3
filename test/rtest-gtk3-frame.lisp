(in-package :gtk-test)

(def-suite gtk-frame :in gtk-suite)
(in-suite gtk-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFrame

(test frame-class
  ;; Type check
  (is (g:type-is-object "GtkFrame"))
  ;; Check the registered name
  (is (eq 'gtk:frame
          (gobject:symbol-for-gtype "GtkFrame")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFrame")
          (g:gtype (cffi:foreign-funcall "gtk_frame_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkFrame")))
  ;; Check the children
  (is (equal '("GtkAspectFrame")
             (list-children "GtkFrame")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkFrame")))
  ;; Check the class properties
  (is (equal '("label" "label-widget" "label-xalign" "label-yalign"
               "shadow-type")
             (list-properties "GtkFrame")))
  ;; Get the names of the style properties.
  (is (equal '()
             (list-style-properties "GtkFrame")))
  ;; Get the names of the child properties
  (is (equal '()
             (list-child-properties "GtkFrame")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFrame")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFrame" GTK-FRAME
                       (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_frame_get_type")
                       ((LABEL GTK-FRAME-LABEL "label" "gchararray" T T)
                        (LABEL-WIDGET GTK-FRAME-LABEL-WIDGET "label-widget"
                         "GtkWidget" T T)
                        (LABEL-XALIGN GTK-FRAME-LABEL-XALIGN "label-xalign"
                         "gfloat" T T)
                        (LABEL-YALIGN GTK-FRAME-LABEL-YALIGN "label-yalign"
                         "gfloat" T T)
                        (SHADOW-TYPE GTK-FRAME-SHADOW-TYPE "shadow-type"
                         "GtkShadowType" T T)))
             (gobject:get-g-type-definition "GtkFrame"))))

(test frame-properties
  (let ((widget (make-instance 'gtk:frame)))
    (is-false (gtk:frame-label widget))
    (is-false (gtk:frame-label-widget widget))
    (is (= 0.0 (gtk:frame-label-xalign widget)))
    (is (= 0.5 (gtk:frame-label-yalign widget)))
    (is (eq :etched-in (gtk:frame-shadow-type widget)))))

(test frame-style-properties
  (let ((widget (make-instance 'gtk:frame)))
    (is (= 0.04 (gtk:widget-style-property widget "cursor-aspect-ratio")))
    (is-false (gtk:widget-style-property widget "cursor-color"))
    (is (equal "" (gtk:widget-style-property widget "focus-line-pattern")))
    (is (= 1 (gtk:widget-style-property widget "focus-line-width")))
    (is-true (integerp (gtk:widget-style-property widget "focus-padding")))
    (is-true (gtk:widget-style-property widget "interior-focus"))
    #-windows
    (is-false (gtk:widget-style-property widget "link-color"))
    (is (= 16 (gtk:widget-style-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk:widget-style-property widget "scroll-arrow-vlength")))
    (is-false (gtk:widget-style-property widget "secondary-cursor-color"))
    (is-true (integerp (gtk:widget-style-property widget "separator-height")))
    (is-true (integerp (gtk:widget-style-property widget "separator-width")))
    (is (= 24 (gtk:widget-style-property widget "text-handle-height")))
    (is (= 20 (gtk:widget-style-property widget "text-handle-width")))
    #-windows
    (is-false (gtk:widget-style-property widget "visited-link-color"))
    (is-false (gtk:widget-style-property widget "wide-separators"))
    (is-false (gtk:widget-style-property widget "window-dragging"))))

;;;     gtk_frame_new
;;;     gtk_frame_set_label
;;;     gtk_frame_set_label_widget
;;;     gtk_frame_set_label_align
;;;     gtk_frame_set_shadow_type
;;;     gtk_frame_get_label
;;;     gtk_frame_get_label_align
;;;     gtk_frame_get_label_widget
;;;     gtk_frame_get_shadow_type

;;; 2022-12-26
