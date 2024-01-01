;;;; Scrolled Window
;;;;
;;;; 2024-1-1

(in-package :gtk3-example)

(defun example-scrolled-window (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Scrolled Window"
                                 :type :toplevel
                                 :application application
                                 :width-request 350
                                 :height-request 300))
          (scrolled (make-instance 'gtk:scrolled-window
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always))
          (image (gtk:image-new-from-file (sys-path "resource/ducky.png"))))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Pack and show the widgets
      (gtk:container-add scrolled image)
      (gtk:container-add window scrolled)
      (gtk:widget-show-all window))))
