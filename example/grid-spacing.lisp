;;;; Example Grid Spacing
;;;;
;;;; 2023-12-29

(in-package :gtk3-example)

(defun example-grid-spacing (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title "Grid Spacing"
                                 :application application
                                 :border-width 12
                                 :default-width 320))
          (grid (make-instance 'gtk:grid
                               :column-homogeneous t
                               :column-spacing 6
                               :row-homogeneous t
                               :row-spacing 6))
          (button1 (make-instance 'gtk:toggle-button
                                  :label "More Row Spacing"))
          (button2 (make-instance 'gtk:toggle-button
                                  :label "More Col Spacing"))
          (button3 (make-instance 'gtk:button
                                  :label "Button 3")))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (g:signal-connect button1 "toggled"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (setf (gtk:grid-row-spacing grid) 24
                     (gtk:button-label widget) "Less Row Spacing")
               (setf (gtk:grid-row-spacing grid) 6
                     (gtk:button-label widget) "More Row Spacing"))))
      (g:signal-connect button2 "toggled"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (setf (gtk:grid-column-spacing grid) 24
                     (gtk:button-label widget) "Less Col Spacing")
               (setf (gtk:grid-column-spacing grid) 6
                     (gtk:button-label widget) "More Col Spacing"))))
      (gtk:grid-attach grid button1 0 0 1 1)
      (gtk:grid-attach grid button2 1 0 1 1)
      (gtk:grid-attach grid button3 0 1 2 1)
      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
