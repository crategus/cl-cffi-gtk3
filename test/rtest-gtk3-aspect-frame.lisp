(in-package :gtk-test)

(def-suite gtk-aspect-frame :in gtk-suite)
(in-suite gtk-aspect-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAspectFrame

(test gtk-aspect-frame-class
  ;; Type check
  (is (g:type-is-object "GtkAspectFrame"))
  ;; Check the registered name
  (is (eq 'gtk:aspect-frame
          (glib:symbol-for-gtype "GtkAspectFrame")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAspectFrame")
          (g:gtype (cffi:foreign-funcall "gtk_aspect_frame_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkFrame")
          (g:type-parent "GtkAspectFrame")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAspectFrame")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkAspectFrame")))
  ;; Check the class properties
  (is (equal '("obey-child" "ratio" "xalign" "yalign")
             (list-properties "GtkAspectFrame")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkAspectFrame")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkAspectFrame")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAspectFrame")))
  ;; CSS information
  (is (string= "frame"
               (gtk:widget-class-css-name "GtkAspectFrame")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAspectFrame" GTK-ASPECT-FRAME
                               (:SUPERCLASS GTK-FRAME :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_aspect_frame_get_type")
                               ((OBEY-CHILD GTK-ASPECT-FRAME-OBEY-CHILD
                                 "obey-child" "gboolean" T T)
                                (RATIO GTK-ASPECT-FRAME-RATIO "ratio" "gfloat"
                                 T T)
                                (XALIGN GTK-ASPECT-FRAME-XALIGN "xalign"
                                 "gfloat" T T)
                                (YALIGN GTK-ASPECT-FRAME-YALIGN "yalign"
                                 "gfloat" T T)))
             (gobject:get-g-type-definition "GtkAspectFrame"))))

;;; --- Properties -------------------------------------------------------------

;;;     obey-child
;;;     ratio
;;;     xalign
;;;     yalign

(test gtk-aspect-frame-properties
  (let ((frame (make-instance 'gtk:aspect-frame)))
    (is-true (gtk:aspect-frame-obey-child frame))
    (is (= 1.0 (gtk:aspect-frame-ratio frame)))
    (is (= 0.5 (gtk:aspect-frame-xalign frame)))
    (is (= 0.5 (gtk:aspect-frame-yalign frame)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_aspect_frame_new

(test gtk-aspect-frame-new
  (is (typep (gtk:aspect-frame-new "label" 1.0 1.0 0.5 nil) 'gtk:aspect-frame)))

;;;     gtk_aspect_frame_set

(test gtk-aspect-frame-set
  (let ((frame (make-instance 'gtk:aspect-frame)))
    (is-false (gtk:aspect-frame-set frame 1.0 1.0 0.5 nil))
    (is-false (gtk:aspect-frame-obey-child frame))
    (is (= 0.5 (gtk:aspect-frame-ratio frame)))
    (is (= 1.0 (gtk:aspect-frame-xalign frame)))
    (is (= 1.0 (gtk:aspect-frame-yalign frame)))))

;;; 2023-12-30
