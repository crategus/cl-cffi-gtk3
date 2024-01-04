;;;; Color Chooser Widget
;;;;
;;;; 2024-1-4

(in-package :gtk3-example)

(defun example-color-chooser-widget (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Color Chooser Widget"
                                 :application application
                                 :type :toplevel
                                 :border-width 12
                                 :default-width 400))
          (color-chooser (make-instance 'gtk:color-chooser-widget)))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
        (g:signal-connect color-chooser "color-activated"
            (lambda (chooser color)
              (declare (ignore chooser))
              (format t "Selected color is ~a~%" (gdk:rgba-to-string color))))
        (gtk:container-add window color-chooser)
        (gtk:widget-show-all window))))
