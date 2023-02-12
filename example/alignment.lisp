;;;; Example Alignment - 2023-2-12

(in-package :gtk3-example)

(defun example-alignment (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Example Alignment"
                                 :type :toplevel
                                 :application application
                                 :border-width 12
                                 :width-request 300
                                 :height-request 300))
          (grid (make-instance 'gtk:grid
                                :column-spacing 12
                                :column-homogeneous t
                                :row-spacing 12
                                :row-homogeneous t)))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((frame (make-instance 'gtk:frame
                                  :label "xalign: 0, yalign: 0"))
            (button (make-instance 'gtk:button
                                   :label "Button"))
            (alignment (make-instance 'gtk:alignment
                                      :xalign 0.00
                                      :yalign 0.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk:alignment-set-padding alignment 6 6 6 6)
        (gtk:container-add alignment button)
        (gtk:container-add frame alignment)
        (gtk:grid-attach grid frame 0 1 1 1))
      (let ((frame (make-instance 'gtk:frame
                                  :label "xalign: 0, yalign: 1"))
            (button (make-instance 'gtk:button
                                   :label "Button"))
            (alignment (make-instance 'gtk:alignment
                                      :xalign 0.00
                                      :yalign 1.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk:alignment-set-padding alignment 6 6 6 6)
        (gtk:container-add alignment button)
        (gtk:container-add frame alignment)
        (gtk:grid-attach grid frame 1 1 1 1))
      (let ((frame (make-instance 'gtk:frame
                                  :label "xalign: 1, yalign: 0"))
            (button (make-instance 'gtk:button
                                   :label "Button"))
            (alignment (make-instance 'gtk:alignment
                                      :xalign 1.00
                                      :yalign 0.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk:alignment-set-padding alignment 6 6 6 6)
        (gtk:container-add alignment button)
        (gtk:container-add frame alignment)
        (gtk:grid-attach grid frame 0 2 1 1))
      (let ((frame (make-instance 'gtk:frame
                                  :label "xalign: 1, yalign: 1"))
            (button (make-instance 'gtk:button
                                   :label "Button"))
            (alignment (make-instance 'gtk:alignment
                                      :xalign 1.00
                                      :yalign 1.00
                                      :xscale 0.50
                                      :yscale 0.25)))
        (gtk:alignment-set-padding alignment 6 6 6 6)
        (gtk:container-add alignment button)
        (gtk:container-add frame alignment)
        (gtk:grid-attach grid frame 1 2 1 1))
      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
