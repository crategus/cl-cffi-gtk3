;;;; Tree Store
;;;;
;;;; This code was taken from anranyicheng (https://github.com/anranyicheng),
;;;; who created the example to test the memory management for the used objects
;;;; and widgets. The code populates a GtkTreeStore object with several thousand
;;;; columns and displays their values in a GtkTreeView widget.
;;;;
;;;; 2025-06-09

(in-package :gtk3-example)

(defun example-tree-store ()
  (gtk:within-main-loop
    (let* ((path (glib-sys:sys-path "resource/tree-store.ui" "gtk3-example"))
           (builder (gtk:builder-new-from-file path))
           (window (gtk:builder-object builder "window"))
           (model (gtk:builder-object builder "model"))
           (view (gtk:builder-object builder "view"))
           (value-label (gtk:builder-object builder "value-label"))
           (reset-treestore (gtk:builder-object builder "reset-treestore"))
           ;; Tree View Colums from UI definition
           (t-v-col0 (gtk:builder-object builder "t-v-col0"))
           (t-v-col1 (gtk:builder-object builder "t-v-col1"))
           (t-v-col2 (gtk:builder-object builder "t-v-col2"))
           (t-v-col3 (gtk:builder-object builder "t-v-col3"))
           (t-v-col4 (gtk:builder-object builder "t-v-col4"))
           (t-v-col5 (gtk:builder-object builder "t-v-col5"))
           (t-v-col6 (gtk:builder-object builder "t-v-col6"))
           (t-v-col7 (gtk:builder-object builder "t-v-col7"))
           (t-v-col8 (gtk:builder-object builder "t-v-col8"))
           (t-v-col9 (gtk:builder-object builder "t-v-col9"))
           (t-v-col10 (gtk:builder-object builder "t-v-col10"))
           (t-v-col11 (gtk:builder-object builder "t-v-col11"))
           (t-v-col12 (gtk:builder-object builder "t-v-col12"))
           (t-v-col13 (gtk:builder-object builder "t-v-col13"))
           (t-v-col14 (gtk:builder-object builder "t-v-col14"))
           (t-v-col15 (gtk:builder-object builder "t-v-col15"))
           (t-v-col16 (gtk:builder-object builder "t-v-col16"))
           (t-v-col17 (gtk:builder-object builder "t-v-col17"))
           (t-v-col18 (gtk:builder-object builder "t-v-col18"))
           ;; Create GTK:CELL-RENDERER-TEXT objects
           (render0 (gtk:cell-renderer-text-new))
           (render1 (gtk:cell-renderer-text-new))
           (render2 (gtk:cell-renderer-text-new))
           (render3 (gtk:cell-renderer-text-new))
           (render4 (gtk:cell-renderer-text-new))
           (render5 (gtk:cell-renderer-text-new))
           (render6 (gtk:cell-renderer-text-new))
           (render7 (gtk:cell-renderer-text-new))
           (render8 (gtk:cell-renderer-text-new))
           (render9 (gtk:cell-renderer-text-new))
           (render10 (gtk:cell-renderer-text-new))
           (render11 (gtk:cell-renderer-text-new))
           (render12 (gtk:cell-renderer-text-new))
           (render13 (gtk:cell-renderer-text-new))
           (render14 (gtk:cell-renderer-text-new))
           (render15 (gtk:cell-renderer-text-new))
           (render16 (gtk:cell-renderer-text-new))
           (render17 (gtk:cell-renderer-text-new))
           (render18 (gtk:cell-renderer-text-new))
           ;; Intial values
           (col-0 0)
           (col-1 1)
           (col-2 2)
           (col-3 3)
           (col-4 4)
           (col-5 5)
           (col-6 6)
           (col-7 7)
           (col-8 8)
           (col-9 9)
           (col-10 10)
           (col-11 11)
           (col-12 12)
           (col-13 13)
           (col-14 14)
           (col-15 15)
           (col-16 16)
           (col-17 17)
           (col-18 18))
      ;; Setup for columns
      (dolist (lst (list (list t-v-col0 render0 col-0)
                         (list t-v-col1 render1 col-1)
                         (list t-v-col2 render2 col-2)
                         (list t-v-col3 render3 col-3)
                         (list t-v-col4 render4 col-4)
                         (list t-v-col5 render5 col-5)
                         (list t-v-col6 render6 col-6)
                         (list t-v-col7 render7 col-7)
                         (list t-v-col8 render8 col-8)
                         (list t-v-col9 render9 col-9)
                         (list t-v-col10 render10 col-10)
                         (list t-v-col11 render11 col-11)
                         (list t-v-col12 render12 col-12)
                         (list t-v-col13 render13 col-13)
                         (list t-v-col14 render14 col-14)
                         (list t-v-col15 render15 col-15)
                         (list t-v-col16 render16 col-16)
                         (list t-v-col17 render17 col-17)
                         (list t-v-col18 render18 col-18)))
        (setf (gtk:cell-renderer-xalign (elt lst 1)) 0.5)
        (gtk:tree-view-column-pack-start (elt lst 0) (elt lst 1) :expand t)
        (setf (gtk:cell-renderer-text-background (elt lst 1)) "#000000")
        (setf (gtk:cell-renderer-text-foreground (elt lst 1)) "#FFFFFF")
        (gtk:tree-view-column-add-attribute (elt lst 0)
                                            (elt lst 1)
                                            "text"
                                            (elt lst 2)))
      ;; Autosize columns
      (gtk:tree-view-columns-autosize view)
      ;; Set sort function for column 1
      (gtk:tree-sortable-set-sort-func model col-1
          (lambda (sortable iter1 iter2)
            (- (read-from-string (gtk:tree-model-value sortable iter1 col-1))
               (read-from-string (gtk:tree-model-value sortable iter2 col-1)))))
      (setf (gtk:tree-sortable-sort-column-id model :ascending) col-1)
      ;; Set sort function for column 0
      (gtk:tree-sortable-set-sort-func model col-0
          (lambda (sortable iter1 iter2)
            (- (gtk:tree-model-value sortable iter1 col-0)
               (gtk:tree-model-value sortable iter2 col-0))))
      ;; Current sort column is column 0
      (setf (gtk:tree-sortable-sort-column-id model :ascending) col-0)
      ;; Selection mode is single
      (setf (gtk:tree-selection-mode (gtk:tree-view-selection view)) :single)
      ;; Fill tree store with values
      (loop for i from 1 upto 6000 do
            (let* ((iter (gtk:tree-store-append model nil)))
              (gtk:tree-store-set model iter i
                                  (princ-to-string (+ i 1))
                                  (princ-to-string (+ i 2))
                                  (princ-to-string (+ i 3))
                                  (princ-to-string (+ i 4))
                                  (princ-to-string (+ i 5))
                                  (princ-to-string (+ i 6))
                                  (princ-to-string (+ i 7))
                                  (princ-to-string (+ i 8))
                                  (princ-to-string (+ i 9))
                                  (princ-to-string (+ i 10))
                                  (princ-to-string (+ i 11))
                                  (princ-to-string (+ i 12))
                                  (princ-to-string (+ i 13))
                                  (princ-to-string (+ i 14))
                                  (princ-to-string (+ i 15))
                                  (princ-to-string (+ i 16))
                                  (princ-to-string (+ i 17))
                                  (princ-to-string (+ i 18)))))
      ;; Destroy window
      (gobject:signal-connect window "destroy"
          (lambda (widget)
            (gtk:widget-destroy widget)
            (gtk:leave-gtk-main)))
      ;; On activation set VALUE-LABEL with values from COL-0 and COL-1
      (g:signal-connect view "row-activated"
          (lambda (view path column)
            (declare (ignore column))
            (let* ((model (gtk:tree-view-model view))
                   (iter (gtk:tree-model-iter model path))
                   (no (gtk:tree-model-value model iter col-0))
                   (value (gtk:tree-model-value model iter col-1)))
              (setf (gtk:label-text value-label)
                    (concatenate 'string (princ-to-string no) "--->" value)))))
      ;; Refill tree store
      (g:signal-connect reset-treestore "clicked"
          (lambda (widget)
            (declare (ignore widget))
            (gtk:tree-store-clear model)
            (loop for i from 10000 upto 16000 do
                  (let* ((iter (gtk:tree-store-append model nil)))
                    (gtk:tree-store-set model iter i
                                        (princ-to-string (+ i 1))
                                        (princ-to-string (+ i 2))
                                        (princ-to-string (+ i 3))
                                        (princ-to-string (+ i 4))
                                        (princ-to-string (+ i 5))
                                        (princ-to-string (+ i 6))
                                        (princ-to-string (+ i 7))
                                        (princ-to-string (+ i 8))
                                        (princ-to-string (+ i 9))
                                        (princ-to-string (+ i 10))
                                        (princ-to-string (+ i 11))
                                        (princ-to-string (+ i 12))
                                        (princ-to-string (+ i 13))
                                        (princ-to-string (+ i 14))
                                        (princ-to-string (+ i 15))
                                        (princ-to-string (+ i 16))
                                        (princ-to-string (+ i 17))
                                        (princ-to-string (+ i 18)))))))
      (gtk:widget-show-all window))))
