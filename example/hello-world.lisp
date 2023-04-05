;;;; Example Hello World - 2023-4-5

(in-package :gtk3-example)

(defun example-hello-world ()
  (gtk:within-main-loop
    (let (;; Create a toplevel window, set a border width.
          (window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title "Hello World"
                                 :default-width 250
                                 :border-width 12))
          ;; Create a button with a label.
          (button (make-instance 'gtk:button
                                 :label "Hello World")))
      ;; Signal handler for the button to handle the signal "clicked".
      (g:signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Hello world.~%")
                          (gtk:widget-destroy window)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Signal handler for the window to handle the signal "delete-event".
      (g:signal-connect window "delete-event"
                        (lambda (widget event)
                          (declare (ignore widget event))
                          (format t "Delete Event Occured.~%")
                          gdk:+gdk-event-stop+))
      ;; Put the button into the window.
      (gtk:container-add window button)
      ;; Show the window and the button.
      (gtk:widget-show-all window))))
