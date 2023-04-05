;;;; Example Getting Started - 2023-4-5

(in-package :gtk3-example)

(defun example-getting-started ()
  (gtk:within-main-loop
    (let (;; Create a toplevel window with a title and a default width.
          (window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title "Getting started"
                                 :default-width 250)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Show the window.
      (gtk:widget-show-all window))))
