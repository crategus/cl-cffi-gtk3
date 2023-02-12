;;;; Example File Chooser Preview - 2023-2-12

(in-package :gtk3-example)

(defun create-file-chooser-preview (&optional parent)
  (let ((response nil)
        (preview-width 256)
        (preview-height 256)
        (chooser (gtk:file-chooser-dialog-new "Example File Chooser Preview"
                                              parent
                                              :open
                                              "gtk-open" :accept
                                              "gtk-cancel" :cancel))
        (preview (make-instance 'gtk:image
                                :margin 24)))
    ;; Handler for the signal "upadate-preview"
    (g:signal-connect chooser "update-preview"
        (lambda (chooser)
          (let* ((filename (gtk:file-chooser-preview-filename chooser))
                 (pixbuf (when filename
                           (gdk:pixbuf-new-from-file-at-size filename
                                                             preview-width
                                                             preview-height))))
            (if pixbuf
                (progn
                  (gtk:image-set-from-pixbuf preview pixbuf)
                  (setf (gtk:file-chooser-preview-widget-active chooser) t))
                (setf (gtk:file-chooser-preview-widget-active chooser) nil)))))
    ;; Set the preview widget
    (setf (gtk:file-chooser-preview-widget chooser) preview)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk:dialog-run chooser)))
      (format t "Save to file ~A~%"
                (gtk:file-chooser-filename chooser)))
    (gtk:widget-destroy chooser)
    response))
