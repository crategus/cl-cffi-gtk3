(in-package :gtk-test)

(def-suite gtk-frame :in gtk-suite)
(in-suite gtk-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFrame

(test gtk-frame-class
  ;; Check type
  (is (g:type-is-object "GtkFrame"))
  ;; Check registered name
  (is (eq 'gtk:frame
          (glib:symbol-for-gtype "GtkFrame")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFrame")
          (g:gtype (cffi:foreign-funcall "gtk_frame_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkFrame")))
  ;; Check children
  (is (equal '("GtkAspectFrame")
             (glib-test:list-children "GtkFrame")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkFrame")))
  ;; Check class properties
  (is (equal '("label" "label-widget" "label-xalign" "label-yalign"
               "shadow-type")
             (glib-test:list-properties "GtkFrame")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFrame")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFrame")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFrame")))
  ;; CSS information
  (is (string= "frame"
               (gtk:widget-class-css-name "GtkFrame")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFrame" GTK:FRAME
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_frame_get_type")
                       ((LABEL FRAME-LABEL "label" "gchararray" T T)
                        (LABEL-WIDGET FRAME-LABEL-WIDGET
                         "label-widget" "GtkWidget" T T)
                        (LABEL-XALIGN FRAME-LABEL-XALIGN
                         "label-xalign" "gfloat" T T)
                        (LABEL-YALIGN FRAME-LABEL-YALIGN
                         "label-yalign" "gfloat" T T)
                        (SHADOW-TYPE FRAME-SHADOW-TYPE
                         "shadow-type" "GtkShadowType" T T)))
             (gobject:get-gtype-definition "GtkFrame"))))

(test gtk-frame-properties.1
  (let ((widget (make-instance 'gtk:frame)))
    (is-false (gtk:frame-label widget))
    (is-false (gtk:frame-label-widget widget))
    (is (= 0.0 (gtk:frame-label-xalign widget)))
    (is (= 0.5 (gtk:frame-label-yalign widget)))
    (is (eq :etched-in (gtk:frame-shadow-type widget)))))

(test gtk-frame-properties.2
  (let ((frame (gtk:frame-new "label")))
    (is (string= "label" (gtk:frame-label frame)))
    (is (typep (gtk:frame-label-widget frame) 'gtk:label))
    (is (string= "label" (gtk:label-label (gtk:frame-label-widget frame))))))

(test gtk-frame-style-properties
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

(test gtk-frame-new
  (is (typep (gtk:frame-new) 'gtk:frame))
  (is (typep (gtk:frame-new nil) 'gtk:frame))
  (is (typep (gtk:frame-new "label") 'gtk:frame)))

;;;     gtk_frame_set_label_align
;;;     gtk_frame_get_label_align

(test gtk-frame-label-align
  (let ((frame (gtk:frame-new "label")))
    (is (equal '(0.0 0.5)
               (multiple-value-list (gtk:frame-label-align frame))))
    (is (equal '(1 1/2)
               (multiple-value-list
                   (setf (gtk:frame-label-align frame) (list 1 1/2)))))
    (is (equal '(1.0 0.5)
               (multiple-value-list (gtk:frame-label-align frame))))))

;;; 2024-9-21
