;;;; Drawing an animation with Pango and Cairo
;;;;
;;;; This example shows an animated circle with text. The purpose of this
;;;; example is to keep track of the memory usage of the draw function. When
;;;; the *check-memory-usage* variable is set to T a full garbage collection is
;;;; performed after each single call to the draw function.
;;;;
;;;; 2025-06-01

(in-package :gtk3-example)

(defvar *check-memory-usage* nil)

(defun example-pango-drawing-animation (&optional application)
  (gtk:within-main-loop
    (let ((area (make-instance 'gtk:drawing-area))
          (window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title
                                 "Drawing Animation using Pango with Cairo"
                                 :application application
                                 :default-width 400
                                 :default-height 400))
          (circle 100)
          (words 12)
          (font "Sans Bold 14")
          (timeout 0)
          (offset 0))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Remove timer
                          (unless (= 0 timeout)
                            (g:source-remove timeout)
                            (setf timeout 0))
                          (gtk:leave-gtk-main)))
      ;; Connect to "draw" signal to handle the backing surface
      (g:signal-connect area "draw"
         (lambda (widget cr)
           (let* ((cr (glib:pointer cr))
                  (width (gtk:widget-allocated-width widget))
                  (height (gtk:widget-allocated-height widget))
                  (radius (- (/ (min width height) 2) 20)))
             ;; Set up a transformation matrix so that the user space
             ;; coordinates for where we are drawing are [-RADIUS, RADIUS],
             ;; [-RADIUS, RADIUS]. We first center, then change the scale.
             (cairo:translate cr
                              (+ radius (/ (- width (* 2 radius)) 2))
                              (+ radius (/ (- height (* 2 radius)) 2)))
             (cairo:scale cr (/ radius circle) (/ radius circle))
           ;; Clear surface
           (cairo:set-source-rgb cr 1 1 1)
           (cairo:paint cr)
           ;; Create a PangoLayout, set the font and text
           (let* ((context (gtk:widget-pango-context widget))
                  (layout (pango:layout-new context))
                  (desc (pango:font-description-from-string font)))
             (setf (pango:layout-text layout) "Text")
             (setf (pango:layout-font-description layout) desc)
             ;; Draw the layout WORDS times in a circle
             (do* ((i 0 (+ i 1))
                   (angle (- offset) (- (/ (* 360 i) words) offset))
                   ;; Gradient from red to blue
                   (red (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)
                        (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)))
                  ((>= i words))
               (cairo:save cr)
               (cairo:set-source-rgb cr red 0 (- 1 red))
               (cairo:rotate cr (/ (* angle pi) 180))
               ;; Inform Pango to re-layout the text with the new
               ;; transformation matrix
               (pango:cairo-update-layout cr layout)
               (multiple-value-bind (width height)
                   (pango:layout-size layout)
                 (declare (ignore height))
                 (cairo:move-to cr (- (/ width 2 pango:+scale+))
                                   (- circle)))
               (pango:cairo-show-layout cr layout)
               (cairo:restore cr)))
            (cairo:destroy cr)
           (when *check-memory-usage*
             (tg:gc :full t))
           t)))
      ;; Create timer for animation
      (setf timeout
            (g:timeout-add 300
                           (lambda ()
                             ;; Increase OFFSET for the drawing
                             (setf offset (mod (+ 5 offset) 360))
                             (gtk:widget-queue-draw area)
                             g:+source-continue+)))
      ;; Add area to window and show it
      (gtk:container-add window area)
      (gtk:widget-show-all window))))
