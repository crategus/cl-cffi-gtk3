;;;; Example Color Button
;;;;
;;;; The example shows a color button. The button is initialized with the color
;;;; "Blue". The handler for the "color-set" signal prints the selected color
;;;; on the console.
;;;;
;;;; 2024-1-4

(in-package :gtk3-example)

(defun example-color-button (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Color Button"
                                 :type :toplevel
                                 :application application
                                 :border-width 12
                                 :default-width 250
                                 :default-height 200))
          (button (make-instance 'gtk:color-button
                                 :rgba (gdk:rgba-parse "Blue"))))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (g:signal-connect button "color-set"
         (lambda (widget)
           (let ((rgba (gtk:color-chooser-rgba widget)))
             (format t "Selected color is ~A~%" (gdk:rgba-to-string rgba)))))
      (gtk:container-add window button)
      (gtk:widget-show-all window))))
