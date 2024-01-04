;;;; Example File Chooser Dialog
;;;;
;;;; 2024-1-4

(in-package :gtk3-example)

(defun create-file-chooser-dialog (&optional parent)
  (let ((dialog (gtk:file-chooser-dialog-new "File Chooser Dialog"
                                             parent
                                             :open
                                             "gtk-open" :accept
                                             "gtk-cancel" :cancel)))
    (when (eq :accept (gtk:dialog-run dialog))
      (format t "Save to file ~A~%"
                (gtk:file-chooser-filename dialog)))
    (gtk:widget-destroy dialog)))
