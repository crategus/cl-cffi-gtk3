;;;; Example Table packing with more spacing
;;;;
;;;; 2023-12-29

(in-package :gtk3-example)

(defun example-table-packing-2 (&optional application)
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title "Table Packing"
                                 :type :toplevel
                                 :application application
                                 :border-width 12
                                 :default-width 300))
          (table (make-instance 'gtk:table
                                :n-columns 2
                                :n-rows 2
                                :homogeneous t))
          (button1 (make-instance 'gtk:toggle-button
                                  :label "More Row Spacing"))
          (button2 (make-instance 'gtk:toggle-button
                                  :label "More Col Spacing"))
          (quit (make-instance 'gtk:button
                               :label "Quit")))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      (g:signal-connect button1 "toggled"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (setf (gtk:table-row-spacing table) 12
                     (gtk:button-label widget) "Less Row Spacing")
               (setf (gtk:table-row-spacing table) 0
                     (gtk:button-label widget) "More Row Spacing"))))
      (g:signal-connect button2 "toggled"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (setf (gtk:table-column-spacing table) 12
                     (gtk:button-label widget) "Less Col Spacing")
               (setf (gtk:table-column-spacing table) 0
                     (gtk:button-label widget) "More Col Spacing"))))
      (g:signal-connect quit "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:widget-destroy window)))
      (gtk:table-attach table button1 0 1 0 1)
      (gtk:table-attach table button2 1 2 0 1)
      (gtk:table-attach table quit    0 2 1 2)
      (gtk:container-add window table)
      (gtk:widget-show-all window))))
