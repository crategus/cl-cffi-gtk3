;;;; Example Tree View Simple
;;;;
;;;; 2024-3-14

(in-package :gtk3-example)

(let ((coltitle 0) (colauthor 1) (colyear 2))

  (defun create-and-fill-model-simple ()
    (let ((model (gtk:tree-store-new "gchararray" "gchararray" "guint")))
      ;; First Book
      (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
        ;; Set the toplevel row
        (gtk:tree-store-set model
                            iter
                            "The Art of Computer Programming"
                            "Donald E. Knuth"
                            2011)
        ;; Append and set child rows
        (gtk:tree-store-set model
                            (gtk:tree-store-append model iter) ; Child iterator
                            "Volume 1: Fundamental Algorithms"
                            ""
                            1997)
        (gtk:tree-store-set model
                            (gtk:tree-store-append model iter) ; Child iterator
                            "Volume 2: Seminumerical Algorithms"
                            ""
                            1998)
        (gtk:tree-store-set model
                            (gtk:tree-store-append model iter) ; Child iterator
                            "Volume 3: Sorting and Searching"
                            ""
                            1998))
      ;; Second Book
      (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
        (gtk:tree-store-set model
                            iter
                            "Let Over Lambda"
                            "Doug Hoyte"
                            2008))
      ;; Third Book
      (let ((iter (gtk:tree-store-append model nil))) ; Toplevel iterator
        (gtk:tree-store-set model
                            iter
                            "On Lisp"
                            "Paul Graham"
                            1993))
      model))

  (defun create-view-and-model-simple ()
    (let* ((model (create-and-fill-model-simple))
           (view (gtk:tree-view-new-with-model model)))
      ;; Create renderer for Title column
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "Title"
                                                               renderer
                                                               "text"
                                                                coltitle)))
        (gtk:tree-view-append-column view column))
      ;; Create renderer for Author column
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "Author"
                                                               renderer
                                                               "text"
                                                               colauthor)))
        (gtk:tree-view-append-column view column))
      ;; Create renderer for Year column
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "Year"
                                                               renderer
                                                               "text"
                                                               colyear)))
        (gtk:tree-view-append-column view column))
      view))

  (defun example-tree-view-simple (&optional application)
    (gtk:within-main-loop
      (let* ((window (make-instance 'gtk:window
                                    :title "Simple Tree View"
                                    :type :toplevel
                                    :application application
                                    :default-width 350
                                    :default-height 200))
             (view (create-view-and-model-simple))
             (vbox (make-instance 'gtk:box
                                  :orientation :vertical
                                  :homogeneous nil
                                  :spacing 3))
             (statusbar (make-instance 'gtk:statusbar))
             (id (gtk:statusbar-context-id statusbar "Selection")))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
        ;; Setup the selection handler
        (let ((selection (gtk:tree-view-selection view)))
          (setf (gtk:tree-selection-mode selection) :single)
          (g:signal-connect selection "changed"
              (lambda (object)
                (let* ((view (gtk:tree-selection-tree-view object))
                       (model (gtk:tree-view-model view))
                       (iter (gtk:tree-selection-selected object))
                       (title (gtk:tree-model-value model iter coltitle)))
                  (gtk:statusbar-pop statusbar id)
                  (gtk:statusbar-push statusbar
                                      id
                                      (format nil "Selection : ~a" title))))))
        ;; Pack and show the widgets
        (gtk:box-pack-start vbox view)
        (gtk:box-pack-start vbox statusbar :expand nil)
        (gtk:container-add window vbox)
        (gtk:widget-show-all window)))))
