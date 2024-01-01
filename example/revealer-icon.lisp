;;;; Example Revealer Icon
;;;;
;;;; GtkRevealer is a container that animates showing and hiding
;;;; of its sole child with nice transitions.
;;;;
;;;; 2024-1-1

(in-package :gtk3-example)

(defun example-revealer-icon (&optional application)
  (gtk:within-main-loop
    (let* ((count 0)
           (timeout 0)
           (builder (gtk:builder-new-from-file
                        (sys-path "resource/revealer-icon.ui")))
           (window (gtk:builder-object builder "window")))
      (g:signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (unless (= timeout 0)
                                   (g:source-remove timeout)
                                   (setf timeout 0))
                                 (gtk:leave-gtk-main)))
      (setf (gtk:window-application window) application)
      (setf timeout
            (g:timeout-add 690
                (lambda ()
                  (let* ((name (format nil "revealer~d" count))
                         (revealer (gtk:builder-object builder name)))
                    (setf (gtk:revealer-reveal-child revealer) t)
                    (g:signal-connect revealer "notify::child-revealed"
                        (lambda (widget pspec)
                          (declare (ignore pspec))
                          (when (gtk:widget-mapped widget)
                            (setf (gtk:revealer-reveal-child widget)
                                  (not (gtk:revealer-child-revealed widget))))))
                    (setf count (+ count 1))
                    (if (>= count 9)
                        (progn
                          (setf timeout 0)
                          nil)
                        t)))))
      (gtk:widget-show-all window))))
