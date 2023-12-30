;;;; Example Simple Grid
;;;;
;;;; 2023-12-30

(in-package :gtk3-example)

(defun example-grid-simple (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title "Simple Grid"
                                 :application application
                                 :border-width 12
                                 :default-width 320))
          (grid (make-instance 'gtk:grid
                               :column-homogeneous t
                               :column-spacing 6
                               :row-homogeneous t
                               :row-spacing 6))
          (button1 (make-instance 'gtk:button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk:button
                                  :label "Button 2"))
          (button3 (make-instance 'gtk:button
                                  :label "Button 3")))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (gtk:grid-attach grid button1 0 0 1 1)
      (gtk:grid-attach grid button2 1 0 1 1)
      (gtk:grid-attach grid button3 0 1 2 1)
      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
