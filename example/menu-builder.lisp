(in-package :gtk3-example)

(defun example-menu-builder ()
  (gtk:within-main-loop
      (setf (gtk:settings-gtk-shell-shows-app-menu (gtk:settings-default))
            nil)
      (setf (gtk:settings-gtk-shell-shows-menubar (gtk:settings-default))
            nil)
    (let ((builder (make-instance 'gtk:builder)))

      (gtk:builder-add-from-file builder (sys-path "resource/menu-builder.ui"))
      (let ((window (gtk:builder-object builder "window")))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
      (gtk:widget-show-all window)))))

