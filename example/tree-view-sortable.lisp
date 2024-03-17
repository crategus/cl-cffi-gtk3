;;;; Example Tree View Sorting
;;;;
;;;; 2024-3-15

(in-package :gtk3-example)

(let ((colfirstname 0) (collastname 1) (colyearborn 2))

  (defun age-cell-data-sortable (column renderer model iter)
    (declare (ignore column))
    (let ((year (sixth (multiple-value-list (get-decoded-time))))
          (text nil)
          (value (gtk:tree-model-value model iter colyearborn)))
      (if (and (< value year) (> value 0))
          (progn
            (setf text (format nil "~a  years old" (- year value)))
            (setf (gtk:cell-renderer-text-foreground-set renderer) nil))
          (progn
            (setf text "age unknown")
            (setf (gtk:cell-renderer-text-foreground renderer) "Red")))
      (setf (gtk:cell-renderer-text-text renderer) text)))

  (defun create-and-fill-model-sortable ()
    (let ((model (gtk:list-store-new "gchararray" "gchararray" "guint")))
      ;; Append a row and fill in some data
      (gtk:list-store-set model (gtk:list-store-append model)
                                "Hans" "Müller" 1961)
      ;; Append another row and fill in some data
      (gtk:list-store-set model (gtk:list-store-append model)
                                "Barbara" "Schmidt" 1998)
      ;; Append a third row
      (gtk:list-store-set model (gtk:list-store-append model)
                                "Peter" "Schneider" 1982)
      ;; Append a third row
      (gtk:list-store-set model (gtk:list-store-append model)
                                "Ursula" "Fischer" 2009)
      ;; Append a third row
      (gtk:list-store-set model (gtk:list-store-append model)
                                "Wolfgang" "Weber" 2002)
      (gtk:tree-sortable-set-sort-func model colyearborn
              (lambda (sortable iter1 iter2)
                (- (gtk:tree-model-value sortable iter2 colyearborn)
                   (gtk:tree-model-value sortable iter1 colyearborn))))
      (setf (gtk:tree-sortable-sort-column-id model :descending) colyearborn)
      model))

  (defun create-view-and-model-sortable ()
    (let* ((model (create-and-fill-model-sortable))
           (view (gtk:tree-view-new-with-model model)))
      ;; Variant 1: Insert column with attributes
      (let* ((renderer (gtk:cell-renderer-text-new)))
        (gtk:tree-view-insert-column-with-attributes view
                                                     -1
                                                     "First Name"
                                                     renderer
                                                     "text"
                                                     colfirstname))
      ;; Variant 2: Create column and append it
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "Last Name"
                                                               renderer
                                                               "text"
                                                                collastname)))
        ;; Set weight property to bold
        (setf (gtk:cell-renderer-text-weight renderer) 700)
        (setf (gtk:cell-renderer-text-weight-set renderer) t)
        (gtk:tree-view-append-column view column))
      ;; Variant 3: Insert column with data function
      (let* ((renderer (gtk:cell-renderer-text-new))
             column)
        (gtk:tree-view-insert-column-with-data-func view
                                                    -1
                                                    "Age"
                                                    renderer
                                                    #'age-cell-data-sortable)
        ;; Get the last column and configure it
        (setf column (gtk:tree-view-column view colyearborn))
        (setf (gtk:tree-view-column-sort-column-id column) colyearborn)
        (setf (gtk:tree-view-column-sort-indicator column) t))
      (setf (gtk:tree-selection-mode (gtk:tree-view-selection view)) :none)
      view))

  (defun example-tree-view-sortable (&optional application)
    (gtk:within-main-loop
      (let* ((window (make-instance 'gtk:window
                                    :title "Example Tree View Sortable"
                                    :application application
                                    :type :toplevel
                                    :default-width 350
                                    :default-height 200))
             (toolbar (make-instance 'gtk:toolbar))
             (addbutton (make-instance 'gtk:tool-button
                                       :label "Add year"))
             (subbutton (make-instance 'gtk:tool-button
                                       :label "Sub year"))
             (vbox (make-instance 'gtk:box :orientation :vertical))
             (view (create-view-and-model-sortable))
             (statusbar (make-instance 'gtk:statusbar))
             (id (gtk:statusbar-context-id statusbar "Sortable")))
        (g:signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (gtk:leave-gtk-main)))
        ;; Print the name when double click a row
        (g:signal-connect view "row-activated"
            (lambda (view path column)
              (declare (ignore column))
              (let* ((model (gtk:tree-view-model view))
                     ;; Lookup iter from path
                     (iter (gtk:tree-model-iter model path)))
                (when iter
                  (let ((value (gtk:tree-model-value model iter collastname)))
                    (gtk:statusbar-pop statusbar id)
                    (gtk:statusbar-push statusbar id
                                        (format nil "Double click on ~a"
                                                    value)))))))
        ;; Add one year to the column year born
        (g:signal-connect addbutton "clicked"
            (lambda (button)
              (declare (ignore button))
              (do* ((model (gtk:tree-view-model view))
                    (iter (gtk:tree-model-iter-first model)
                          (gtk:tree-model-iter-next model iter)))
                   ((null iter))
                   (let ((value (gtk:tree-model-value model iter colyearborn)))
                     (gtk:list-store-set-value model iter
                                               colyearborn
                                               (1+ value))))))
        ;; Sub one year from the column year born
        (g:signal-connect subbutton "clicked"
            (lambda (button)
              (declare (ignore button))
              (do* ((model (gtk:tree-view-model view))
                    (iter (gtk:tree-model-iter-first model)
                          (gtk:tree-model-iter-next model iter)))
                   ((null iter))
                   (let ((value (gtk:tree-model-value model iter colyearborn)))
                     (gtk:list-store-set-value model iter
                                               colyearborn
                                               (1- value))))))
        ;; Pack and show the widgets
        (gtk:toolbar-insert toolbar addbutton)
        (gtk:toolbar-insert toolbar subbutton)
        (gtk:box-pack-start vbox toolbar :expand nil)
        (gtk:box-pack-start vbox view)
        (gtk:box-pack-start vbox statusbar :expand nil)
        (gtk:container-add window vbox)
        (gtk:widget-show-all window)))))
