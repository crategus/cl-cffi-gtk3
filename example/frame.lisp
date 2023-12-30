;;;; Example Frame
;;;;
;;;; 2023-12-30

(in-package :gtk3-example)

(defun example-frame (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :application application
                                 :title "Frame Widget"
                                 :default-width 250
                                 :default-height 200
                                 :border-width 12))
          (frame (make-instance 'gtk:frame
                                :label "Frame Label"
                                :label-xalign 1.0
                                :label-yalign 0.5
                                :shadow-type :etched-in)))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (gtk:container-add window frame)
      (gtk:widget-show-all window))))
