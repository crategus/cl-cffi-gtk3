(in-package :gtk-test)

(def-suite gdk-cairo :in gdk-suite)
(in-suite gdk-cairo)

;;; --- Functions --------------------------------------------------------------

;;;     gdk_window_create_similar_surface

#-windows
(test gdk-window-create-similar-surface
  (glib-test:with-check-memory (window :strong 1)
    (is (typep (setf window
                     (make-instance 'gtk:window
                                    :type :toplevel
                                    :default-width 200
                                    :default-height 100)) 'gtk:window))
    (is-false (gtk:widget-realize window))
    (let* ((win (gtk:widget-window window))
           (width (gdk:window-width win))
           (height (gdk:window-height win))
           (surface nil))

      (is (typep win 'gdk:window))
      (is (or (= 200 width) (= 252  width)))
      (is (or (= 100 height) (= 189 height)))
      (is (cffi:pointerp
              (setf surface
                    (gdk:window-create-similar-surface win :color width height))))
      (is (eq :success (cairo:surface-status surface)))
      (is (or (eq :XLIB (cairo:surface-type surface))
              (eq :image (cairo:surface-type surface))))
      (is-false (cairo:surface-destroy surface))
      (is-false (gtk:widget-destroy window)))))

;;;     gdk_window_create_similar_image_surface

(test gdk-window-create-similar-image-surface
  (glib-test:with-check-memory (window :strong 1)
    (is (typep (setf window
                     (make-instance 'gtk:window
                                    :type :toplevel
                                    :default-width 200
                                    :default-height 100)) 'gtk:window))
    (is-false (gtk:widget-realize window))
    (let* ((win (gtk:widget-window window))
           (scale (gdk:window-scale-factor win))
           (width (* scale (gdk:window-width win)))
           (height (* scale (gdk:window-height win)))
           (surface nil))

      (is (typep win 'gdk:window))
      (is (or (= 200 width) (= 252  width)))
      (is (or (= 100 height) (= 189 height)))
      (is (cffi:pointerp
              (setf surface
                    (gdk:window-create-similar-image-surface win
                                                             :rgb24
                                                             width height
                                                             scale))))
      (is (eq :image (cairo:surface-type surface)))
      (is-false (cairo:surface-destroy surface))
      (is-false (gtk:widget-destroy window)))))

;;;     gdk_cairo_create

(test gdk-cairo-create
  (glib-test:with-check-memory (window :strong 1)
    (is (typep (setf window
                     (make-instance 'gtk:window
                                    :type :toplevel
                                    :default-width 200
                                    :default-height 100)) 'gtk:window))
    (is-false (gtk:widget-realize window))
    (let* ((win (gtk:widget-window window))
           (context nil))
      (is (typep win 'gdk:window))
      (is (cffi:pointerp (setf context (gdk:cairo-create win))))
      (is (eq :success (cairo:status context)))
      (is (cffi:pointerp (cairo:target context)))
      (is-false (cairo:destroy context))
      (is-false (gtk:widget-destroy window)))))

;;;     gdk_cairo_get_clip_rectangle

#-windows
(test gdk-cairo-clip-rectangle
  (glib-test:with-check-memory (window :strong 1)
    (is (typep (setf window
                     (make-instance 'gtk:window
                                    :type :toplevel
                                    :default-width 200
                                    :default-height 100)) 'gtk:window))
    (is-false (gtk:widget-realize window))
    (let ((context (gdk:cairo-create (gtk:widget-window window)))
          (rectangle nil))
      (is (eq :success (cairo:status context)))
      (is-false (cairo:reset-clip context))
      (is (typep (setf rectangle
                       (gdk:cairo-clip-rectangle context)) 'gdk:rectangle))
      (is (=   0 (gdk:rectangle-x rectangle)))
      (is (=   0 (gdk:rectangle-y rectangle)))
      (is (or (= 200 (gdk:rectangle-width rectangle))
              (= 252 (gdk:rectangle-width rectangle))))
      (is (or (= 100 (gdk:rectangle-height rectangle))
              (= 189 (gdk:rectangle-height rectangle))))
      ;; Remove references
      (is-false (cairo:destroy context))
      (is-false (gtk:widget-destroy window)))))

;;;     gdk_cairo_get_drawing_context

(test gdk-cairo-drawing-context
  (glib-test:with-check-memory (window :strong 1)
    (is (typep (setf window
                     (make-instance 'gtk:window
                                    :type :toplevel
                                    :default-width 200
                                    :default-height 100)) 'gtk:window))
    (is-false (gtk:widget-realize window))
    (let* ((win (gtk:widget-window window))
           (region (cairo:region-create))
           (context nil) (cr nil))
      (is (typep (setf context
                       (gdk:window-begin-draw-frame win region))
                 'gdk:drawing-context))
      (is (cffi:pointerp (setf cr (gdk:drawing-context-cairo-context context))))
      (is (typep (gdk:cairo-drawing-context cr) 'gdk:drawing-context))
      (is-false (gdk:window-end-draw-frame win context))
      (is-false (cairo:region-destroy region))
      (is-false (gtk:widget-destroy window)))))

;;;     gdk_cairo_set_source_color
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf
;;;     gdk_cairo_set_source_window

;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;     gdk_cairo_surface_create_from_pixbuf
;;;     gdk_cairo_draw_from_gl

;;; 2025-06-19
