;;;; Example File Chooser Dialog - 2022-12-20

(in-package :gtk3-example)

(defun create-file-chooser-dialog ()
  (let ((dialog (gtk:file-chooser-dialog-new "Example File Chooser Dialog"
                                             nil
                                             :open
                                             "gtk-open" :accept
                                             "gtk-cancel" :cancel)))
    (when (eq :accept (gtk:dialog-run dialog))
      (format t "Save to file ~A~%"
                (gtk:file-chooser-filename dialog)))
    (gtk:widget-destroy dialog)))
