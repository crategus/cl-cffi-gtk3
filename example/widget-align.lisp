;;;; Example Align Widget
;;;;
;;;; Similiar to example-alignment, but using the child properties "margin",
;;;; "valign", and "halign" of the button widget.  In distinction to
;;;; example-alignment the scaling of the button is not implemented.
;;;;
;;;; 2024-1-5

(in-package :gtk3-example)

(defun example-widget-align (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Align Widget"
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
                          (gtk:leave-gtk-main)))
      (let ((frame (make-instance 'gtk:frame
                                  :label "halign: start, valign: start"))
            (button (make-instance 'gtk:button
                                   :label "Button"
                                   :margin 6
                                   :halign :start
                                   :valign :start)))
        (gtk:container-add frame button)
        (gtk:grid-attach grid frame 0 1 1 1))
      (let ((frame (make-instance 'gtk:frame
                                  :label "halign: start, valign: end"))
            (button (make-instance 'gtk:button
                                   :label "Button"
                                   :margin 6
                                   :halign :start
                                   :valign :end)))
        (gtk:container-add frame button)
        (gtk:grid-attach grid frame 1 1 1 1))
      (let ((frame (make-instance 'gtk:frame
                                  :label "halign: end, valign: start"))
            (button (make-instance 'gtk:button
                                   :label "Button"
                                   :margin 6
                                   :halign :end
                                   :valign :start)))
        (gtk:container-add frame button)
        (gtk:grid-attach grid frame 0 2 1 1))
      (let ((frame (make-instance 'gtk:frame
                                  :label "halign: end, valign: end"))
            (button (make-instance 'gtk:button
                                   :label "Button"
                                   :margin 6
                                   :halign :end
                                   :valign :end)))
        (gtk:container-add frame button)
        (gtk:grid-attach grid frame 1 2 1 1))
      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
