;;;; Example Switch
;;;;
;;;; 2024-1-3

(in-package :gtk3-example)

(defun example-switch (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title "Switch"
                                 :application application
                                 :default-width 250
                                 :border-width 24))
          (switch (make-instance 'gtk:switch
                                 :active t))
          (label (make-instance 'gtk:label
                                :xalign 0.0
                                :label "Switch is On"))
          (grid (make-instance 'gtk:grid
                               :orientation :horizontal
                               :halign :center
                               :valign :center
                               :column-spacing 24)))

      (g:signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (gtk:leave-gtk-main)))

      (g:signal-connect switch "notify::active"
         (lambda (widget param)
           (declare (ignore param))
           (if (gtk:switch-active widget)
               (setf (gtk:label-label label) "Switch is On")
               (setf (gtk:label-label label) "Switch is Off"))))

      (gtk:container-add grid switch)
      (gtk:container-add grid label)
      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
