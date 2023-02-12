;;;; Color Chooser Widget - 2023-2-12

(in-package :gtk3-example)

(defun example-color-chooser-widget (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Example Color Chooser Widget"
                                 :application application
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 400))
          (color-chooser (make-instance 'gtk:color-chooser-widget)))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g:signal-connect color-chooser "color-activated"
            (lambda (chooser color)
              (declare (ignore chooser))
              (format t "Selected color is ~a~%" (gdk:rgba-to-string color))))
        (gtk:container-add window color-chooser)
        (gtk:widget-show-all window))))
