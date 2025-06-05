;;;; Example Drawing area
;;;;
;;;; 2024-1-5

(in-package :gtk3-example)

(defun example-drawing-area (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :application application
                                 :title "Drawing Area"
                                 :default-width 400
                                 :default-height 300))
          ;; Create the drawing area
          (area (make-instance 'gtk:drawing-area)))
      ;; Signal handler for the drawing area
      (g:signal-connect area "draw"
          (lambda (widget cr)
            (let* ((width (gtk:widget-allocated-width widget))
                   (height (gtk:widget-allocated-height widget))
                   (context (gtk:widget-style-context widget))
                   (color (gtk:style-context-color context :focused)))
                ;; Set the color from the style context of the widget
                (gdk:cairo-set-source-rgba cr color)
                ;; Draw and fill a circle on the drawing area
                (cairo:arc cr
                           (/ width 2.0)
                           (/ height 2.0)
                           (- (/ (min width height) 2.0) 12)
                           0.0
                           (* 2.0 pi))
                (cairo:fill cr))))
      ;; Signal handler for the window to handle the signal "destroy"
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Show the window
      (gtk:container-add window area)
      (gtk:widget-show-all window))))
