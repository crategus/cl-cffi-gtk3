(in-package :gtk-test)

(def-suite gdk-cairo :in gdk-suite)
(in-suite gdk-cairo)

;;; --- Types and Values -------------------------------------------------------

;;;     CairoSurface

(test gdk-cairo-surface-boxed
  ;; Check type
  (is (g:type-is-boxed "CairoSurface"))
;  TODO: Check this: We have no type initializer!?
;  (is (eq (g:gtype "CairoSurface")
;          (g:gtype (cffi:foreign-funcall "cairo_surface_get_type" :size))))
  (cairo:with-image-surface (surface :rgb24 100 150)
    (let ((cairosurface nil))
      ;; CairoSurface cannot be created from the Lisp side
      (signals (error) (make-instance 'gdk:cairo-surface))
      ;; Create CairoContext from foreign pointer surface
      (is (typep (setf cairosurface
                       (cffi:convert-from-foreign surface
                                                  '(g:boxed gdk:cairo-surface)))
                 'gdk:cairo-surface))
      (is (cffi:pointer-eq surface
                           (glib::boxed-opaque-pointer cairosurface))))))

;;;     CairoContext

(test gdk-cairo-context-boxed
  ;; Check type
  (is (g:type-is-boxed "CairoContext"))
;  TODO: Check this: We have no type initializer!?
;  (is (eq (g:gtype "CairoContext")
;          (g:gtype (cffi:foreign-funcall "cairo_context_get_type" :size))))
  (cairo:with-context-for-image-surface (context :rgb24 100 150)
    (let ((cairocontext nil))
      ;; CairoContext cannot be created from the Lisp side
      (signals (error) (make-instance 'gdk:cairo-context))
      ;; Create CairoContext from foreign pointer context
      (is (typep (setf cairocontext
                       (cffi:convert-from-foreign context
                                                  '(g:boxed gdk:cairo-context)))
                 'gdk:cairo-context))
      (is (cffi:pointer-eq context
                           (glib::boxed-opaque-pointer cairocontext))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_window_create_similar_surface

#-windows
(test gdk-window-create-similar-surface
  (let ((window (make-instance 'gtk:window :type :toplevel
                                           :default-width 200
                                           :default-height 100)))

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

      (is (or (eq :XLIB (cairo:surface-type surface))
              (eq :image (cairo:surface-type surface))))
      (is-false (cairo:surface-destroy surface)))))

;;;     gdk_window_create_similar_image_surface

(test gdk-window-create-similar-image-surface
  (let ((window (make-instance 'gtk:window :type :toplevel
                                           :default-width 200
                                           :default-height 100)))

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
      (is-false (cairo:surface-destroy surface)))))

;;;     gdk_cairo_create

(test gdk-cairo-create
  (let ((window (make-instance 'gtk:window :type :toplevel
                                           :default-width 200
                                           :default-height 100)))

    (is-false (gtk:widget-realize window))

    (let* ((win (gtk:widget-window window))
           (context nil))

      (is (typep win 'gdk:window))
      (is (cffi:pointerp (setf context (gdk:cairo-create win))))

      (is (cffi:pointerp context))
      (is (cffi:pointerp (cairo:target context)))
      (is-false (cairo:destroy context)))))

;;;     gdk_cairo_get_clip_rectangle

#-windows
(test gdk-cairo-clip-rectangle
  (let ((window (make-instance 'gtk:window :type :toplevel
                                           :default-width 200
                                           :default-height 100)))

    (is-false (gtk:widget-realize window))

    (let* ((win (gtk:widget-window window))
           (context (gdk:cairo-create win))
           (rectangle nil))

      (is (typep win 'gdk:window))
      (is (cffi:pointerp context))
      (is-false (cairo:reset-clip context))
      (is (typep (setf rectangle
                       (gdk:cairo-clip-rectangle context)) 'gdk:rectangle))
      (is (=   0 (gdk:rectangle-x rectangle)))
      (is (=   0 (gdk:rectangle-y rectangle)))
      (is (or (= 200 (gdk:rectangle-width rectangle))
              (= 252 (gdk:rectangle-width rectangle))))
      (is (or (= 100 (gdk:rectangle-height rectangle))
              (= 189 (gdk:rectangle-height rectangle))))

      (is-false (cairo:destroy context)))))

;;;     gdk_cairo_get_drawing_context

(test gdk-cairo-drawing-context
  (let ((window (make-instance 'gtk:window :type :toplevel
                                           :default-width 200
                                           :default-height 100)))

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

      (is-false (cairo:region-destroy region)))))

;;;     gdk_cairo_set_source_color
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf
;;;     gdk_cairo_set_source_window

;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;     gdk_cairo_surface_create_from_pixbuf
;;;     gdk_cairo_draw_from_gl

;;; --- 2023-7-19 --------------------------------------------------------------
