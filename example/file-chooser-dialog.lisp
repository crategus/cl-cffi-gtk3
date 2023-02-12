;;;; Example File Chooser Dialog - 2023-2-12

(in-package :gtk3-example)

(defun create-file-chooser-dialog (&optional parent)
  (let ((dialog (gtk:file-chooser-dialog-new "Example File Chooser Dialog"
                                             parent
                                             :open
                                             "gtk-open" :accept
                                             "gtk-cancel" :cancel)))
    (when (eq :accept (gtk:dialog-run dialog))
      (format t "Save to file ~A~%"
                (gtk:file-chooser-filename dialog)))
    (gtk:widget-destroy dialog)))
