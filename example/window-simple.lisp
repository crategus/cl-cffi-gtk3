;;;; Example Simple Window - 2023-4-5
;;;;
;;;; This example shows a very simple window. The program creates a 200 x 200
;;;; pixel window. In this case the window has the default title "sbcl". The
;;;; window can be sized and moved.

(in-package :gtk3-example)

(defun example-window-simple ()
  (gtk:within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk:window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Show the window.
      (gtk:widget-show-all window))))
