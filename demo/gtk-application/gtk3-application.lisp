;;; gtk3-application.lisp

(defpackage :gtk3-application
  (:use :common-lisp)
  (:export #:application-cmdline
           #:application-inhibit
           #:application-menu
           #:application-notification
           #:application-properties
           #:application-simple
           #:bloatpad
           #:sunny
))

(in-package :gtk3-application)

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk3-application)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))

(defun load-file-into-buffer (buffer filename)
  (with-open-file (stream filename)
    (clear-buffer buffer)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (gtk:text-buffer-insert buffer line)
      (gtk:text-buffer-insert buffer (format nil "~%")))))

;;; 2024-1-6
