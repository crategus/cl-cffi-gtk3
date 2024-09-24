(in-package :gtk-test)

(def-suite gtk-aspect-frame :in gtk-suite)
(in-suite gtk-aspect-frame)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAspectFrame

(test gtk-aspect-frame-class
  ;; Check type
  (is (g:type-is-object "GtkAspectFrame"))
  ;; Check registered name
  (is (eq 'gtk:aspect-frame
          (glib:symbol-for-gtype "GtkAspectFrame")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAspectFrame")
          (g:gtype (cffi:foreign-funcall "gtk_aspect_frame_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkFrame")
          (g:type-parent "GtkAspectFrame")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAspectFrame")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkAspectFrame")))
  ;; Check class properties
  (is (equal '("obey-child" "ratio" "xalign" "yalign")
             (glib-test:list-properties "GtkAspectFrame")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkAspectFrame")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkAspectFrame")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAspectFrame")))
  ;; CSS information
  (is (string= "frame"
               (gtk:widget-class-css-name "GtkAspectFrame")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAspectFrame" GTK:ASPECT-FRAME
                       (:SUPERCLASS GTK:FRAME
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_aspect_frame_get_type")
                       ((OBEY-CHILD ASPECT-FRAME-OBEY-CHILD
                         "obey-child" "gboolean" T T)
                        (RATIO ASPECT-FRAME-RATIO "ratio" "gfloat" T T)
                        (XALIGN ASPECT-FRAME-XALIGN "xalign" "gfloat" T T)
                        (YALIGN ASPECT-FRAME-YALIGN "yalign" "gfloat" T T)))
             (gobject:get-gtype-definition "GtkAspectFrame"))))

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

;;; 2024-9-21
