;;;; Example Tree View Content Type
;;;;
;;;; 2024-1-4

(in-package :gtk3-example)

(let ((colicon 0) (coliconname 1) (colmimetype 2) (coldesc 3))

  (defun create-and-fill-model-content-type ()
    (let ((data (g:content-types-registered))
          (model (gtk:list-store-new "GdkPixbuf"
                                     "gchararray" "gchararray" "gchararray"))
          (icon-theme (gtk:icon-theme-default)))
      (dolist (mime-type data)
        (let* ((description (g:content-type-description mime-type))
               (name (g:content-type-generic-icon-name mime-type))
               ;; gtk:icon-theme-load-icon does not accept nil for the icon-name
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

  (defun create-view-and-model-content-type ()
    (let* ((model (create-and-fill-model-content-type))
           (view (gtk:tree-view-new-with-model model)))
     ;; First column displays icon and icon-name
     (let ((column (gtk:tree-view-column-new))
           (renderer (gtk:cell-renderer-pixbuf-new)))
       (setf (gtk:tree-view-column-title column) "Icon")
       (gtk:tree-view-column-pack-start column renderer :expand nil)
       (gtk:tree-view-column-add-attribute column
                                           renderer
                                           "pixbuf"
                                           colicon)
       (setf renderer (gtk:cell-renderer-text-new))
       (gtk:tree-view-column-pack-start column renderer :expand nil)
       (gtk:tree-view-column-add-attribute column
                                           renderer
                                           "text"
                                           coliconname)
        (gtk:tree-view-append-column view column))
      ;; Second column for the MIME Type
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "MIME Type"
                                                               renderer
                                                               "text"
                                                                colmimetype)))
        (gtk:tree-view-append-column view column))
      ;; Third column for the description
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "Descripton"
                                                               renderer
                                                               "text"
                                                               coldesc)))
        (gtk:tree-view-append-column view column))
      view))

  (defun example-tree-view-content-type (&optional application)
    (gtk:within-main-loop
      (let ((window (make-instance 'gtk:window
                                   :title "Tree View Content Type"
                                   :type :toplevel
                                   :application application
                                   :default-width 550
                                   :default-height 350))
            (scrolled (make-instance 'gtk:scrolled-window))
            (view (create-view-and-model-content-type)))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
        ;; Set the tree view reorderable
        (setf (gtk:tree-view-reorderable view) t)
        ;; Pack and show the widgets
        (gtk:container-add scrolled view)
        (gtk:container-add window scrolled)
        (gtk:widget-show-all window)))))
