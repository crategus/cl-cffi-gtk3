;;;; Example Icon View Content Type - 2023-2-12

;; TODO: On Windows we do not get any icons. Can this be improved?

(in-package :gtk3-example)

(let ((col-icon 0) (col-icon-name 1) (col-mime-type 2) (col-desc 3))

  (declare (ignore col-mime-type))

  (defun create-and-fill-model-icon-view ()
    (let ((data (g:content-types-registered))
          (model (gtk:list-store-new "GdkPixbuf"
                                     "gchararray" "gchararray" "gchararray"))
          (icon-theme (gtk:icon-theme-default)))
      (dolist (mime-type data)
        (let* ((description (g:content-type-description mime-type))
               (name (g:content-type-generic-icon-name mime-type))
               (icon-name (if name name ""))
               (icon (gtk:icon-theme-load-icon icon-theme
                                               icon-name
                                               24
                                               0)))
          (gtk:list-store-set model (gtk:list-store-append model)
                                    icon
                                    icon-name
                                    mime-type
                                    description)))
      model))

  (defun example-icon-view (&optional application)
    (within-main-loop
      (let ((window (make-instance 'gtk:window
                                   :title "Example Icon View"
                                   :type :toplevel
                                   :application application
                                   :default-width 500
                                   :default-height 350))
            (scrolled (make-instance 'gtk:scrolled-window))
            (view (make-instance 'gtk:icon-view
                                 :model (create-and-fill-model-icon-view)
                                 :pixbuf-column col-icon
                                 :text-column col-icon-name
                                 :tooltip-column col-desc)))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (gtk:container-add scrolled view)
        (gtk:container-add window scrolled)
        (gtk:widget-show-all window)))))
