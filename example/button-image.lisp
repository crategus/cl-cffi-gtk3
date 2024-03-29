;;;; Example Simple Button
;;;;
;;;; 2023-12-30

(in-package :gtk3-example)

(defun image-label-box (filename text)
  (let ((box (make-instance 'gtk:box
                            :orientation :horizontal
                            :border-width 3))
        (label (make-instance 'gtk:label
                              :hexpand nil
                              :margin-left 6
                              :label text))
        (image (gtk:image-new-from-file filename)))
    (gtk:box-pack-start box image :expand nil :fill nil :padding 3)
    (gtk:box-pack-start box label :expand nil :fill nil :padding 3)
    box))

(defun example-button-image (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Button with Image"
                                 :application application
                                 :type :toplevel
                                 :default-width 240
                                 :default-height 120))
          (button (make-instance 'gtk:button
                                 :halign :center
                                 :valign :center))
          (box (image-label-box (sys-path "resource/save.png") "Save to File")))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (gtk:container-add button box)
      (gtk:container-add window button)
      (gtk:widget-show-all window))))
