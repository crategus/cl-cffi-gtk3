;;;; Example Scale Button
;;;;
;;;; 2024-1-3

(in-package :gtk3-example)

(defun example-scale-button (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Scale Button"
                                 :type :toplevel
                                 :application application
                                 :border-width 12
                                 :width-request 360
                                 :height-request 240))
          (button (make-instance 'gtk:scale-button
                                 :margin-left 60
                                 :size 6
                                 :value 9.0
                                 :icons
                                 '("face-crying"     ; lowest value
                                   "face-smile-big"  ; highest value
                                   "face-sad"        ; other value between
                                   "face-worried"
                                   "face-uncertain"
                                   "face-plain"
                                   "face-smile")
                                 :adjustment
                                 (make-instance 'gtk:adjustment
                                                :lower 0.0
                                                :upper 10.0
                                                :step-increment 1.0
                                                :page-increment 2.0))))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Pack and show the widgets
      (gtk:container-add window button)
      (gtk:widget-show-all window))))
