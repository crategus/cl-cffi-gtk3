;;;; Example Stack
;;;;
;;;; GtkStack is a container that shows a single child at a time, with
;;;; nice transitions when the visible child changes.
;;;; GtkStackSwitcher adds buttons to control which child is visible.
;;;;
;;;; 2024-1-3

(in-package :gtk3-example)

(defun example-stack (&optional application)
  (gtk:within-main-loop
    (let* ((builder (gtk:builder-new-from-file (sys-path "resource/stack.ui")))
           (window (gtk:builder-object builder "window1")))
      (setf (gtk:window-application window) application)
      (g:signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (gtk:leave-gtk-main)))
      (gtk:widget-show-all window))))
