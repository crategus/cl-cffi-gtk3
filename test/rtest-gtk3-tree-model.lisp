(in-package :gtk-test)

(def-suite gtk-tree-model :in gtk-suite)
(in-suite gtk-tree-model)

(defun create-and-fill-list-store ()
  (let ((listdata '("Name1" "Name2" "Name3" "Name4" "Name5"))
        ;; Create a new list store with three columns
        (liststore (make-instance 'gtk:list-store
                                  :column-types
                                  '("gint" "gchararray" "gboolean"))))
    ;; Fill in some data
    (loop for data in listdata
          for i from 0 do
          ;; Add a new row to the model
          (gtk:list-store-set liststore
                              (gtk:list-store-append liststore)
                              i
                              data
                              nil))
    ;; Modify a particular row
    (let ((path (gtk:tree-path-new-from-string "2")))
      (gtk:list-store-set-value liststore
                                (gtk:tree-model-iter liststore path)
                                2
                                t))
    ;; Return the new list store
    liststore))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeIter

(test gtk-tree-iter-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkTreeIter"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeIter")
          (g:gtype (cffi:foreign-funcall "gtk_tree_iter_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:tree-iter
          (glib:symbol-for-gtype "GtkTreeIter"))))

;;;     gtk_tree_iter_copy

(test gtk-tree-iter-copy
  (let ((iter (make-instance 'gtk:tree-iter)))
    (is (typep (gtk:tree-iter-copy iter) 'gtk:tree-iter))))

;;; ----------------------------------------------------------------------------

;;;     GtkTreePath

(test gtk-tree-path-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkTreePath"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreePath")
          (g:gtype (cffi:foreign-funcall "gtk_tree_path_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:tree-path
          (glib:symbol-for-gtype "GtkTreePath"))))

;;;     gtk_tree_path_new

(test gtk-tree-path-new
  (is (typep (gtk:tree-path-new) 'gtk:tree-path)))

;;;     gtk_tree_path_new_first

(test gtk-tree-path-new-first
  (is (typep (gtk:tree-path-new-first) 'gtk:tree-path))
  (is (string= "0" (gtk:tree-path-to-string (gtk:tree-path-new-first)))))

;;;     gtk_tree_path_new_from_string

(test gtk-tree-path-new-from-string
  (is (typep (gtk:tree-path-new-from-string "10:4:0") 'gtk:tree-path)))

;;;     gtk_tree_path_new_from_indices

(test gtk-tree-path-new-from-indices
  (is (typep (gtk:tree-path-new-from-indices 10 4 0) 'gtk:tree-path)))

;;;     gtk_tree_path_copy

(test gtk-tree-path-copy
  (let ((path (gtk:tree-path-new-from-string "10:4:0")))
    (is (string= "10:4:0"
                 (gtk:tree-path-to-string (gtk:tree-path-copy path))))))

;;;     gtk_tree_path_to_string

(test gtk-tree-path-to-string
  (let ((path1 (gtk:tree-path-new-from-string "10:4:0"))
        (path2 (gtk:tree-path-new-from-indices 10 4 0)))
    (is (string= "10:4:0" (gtk:tree-path-to-string path1)))
    (is (string= "10:4:0" (gtk:tree-path-to-string path2)))))

;;;     gtk_tree_path_append_index

(test gtk-tree-path-append-index
  (let ((path (gtk:tree-path-new-from-string "10")))
    (is (typep (gtk:tree-path-append-index path 4) 'gtk:tree-path))
    (is (typep (setf path (gtk:tree-path-append-index path 4)) 'gtk:tree-path))
    (is (string= "10:4" (gtk:tree-path-to-string path)))
    (is (typep (gtk:tree-path-append-index path 3) 'gtk:tree-path))
    (is (typep (setf path (gtk:tree-path-append-index path 3)) 'gtk:tree-path))
    (is (string= "10:4:3" (gtk:tree-path-to-string path)))))

;;;     gtk_tree_path_prepend_index

(test gtk-tree-path-prepend-index
  (let ((path (gtk:tree-path-new-from-string "10")))
    (is (typep (gtk:tree-path-prepend-index path 4) 'gtk:tree-path))
    (is (typep (setf path (gtk:tree-path-prepend-index path 4)) 'gtk:tree-path))
    (is (string= "4:10" (gtk:tree-path-to-string path)))
    (is (typep (gtk:tree-path-prepend-index path 3) 'gtk:tree-path))
    (is (typep (setf path (gtk:tree-path-prepend-index path 3)) 'gtk:tree-path))
    (is (string= "3:4:10" (gtk:tree-path-to-string path)))))

;;;     gtk_tree_path_get_depth

(test gtk-tree-path-depth
  (let ((path (gtk:tree-path-new-from-string "10:4:0")))
    (is (= 3 (gtk:tree-path-depth path)))))

;;;     gtk_tree_path_get_indices

(test gtk-tree-path-indices
  (let ((path (gtk:tree-path-new-from-string "10:4:0")))
    (is (equal '(10 4 0) (gtk:tree-path-indices path)))))

;;;     gtk_tree_path_compare

(test gtk-tree-path-compare
  (let ((path1 (gtk:tree-path-new-from-string "10:4:0"))
       (path2 (gtk:tree-path-new-from-string "10:4:1")))
  (is (=  0 (gtk:tree-path-compare path1 path1)))
  (is (=  0 (gtk:tree-path-compare path2 path2)))
  (is (= -1 (gtk:tree-path-compare path1 path2)))
  (is (=  1 (gtk:tree-path-compare path2 path1)))))

;;;     gtk_tree_path_next

(test gtk-tree-path-next
  (let ((path (gtk:tree-path-new-from-string "10:4:0")))
    (is (string= "10:4:1" (gtk:tree-path-to-string (gtk:tree-path-next path))))
    (is (string= "10:4:1" (gtk:tree-path-to-string path)))
    (is (string= "10:4:2"
                 (gtk:tree-path-to-string (setf path
                                                (gtk:tree-path-next path)))))
    (is (string= "10:4:3"
                 (gtk:tree-path-to-string (gtk:tree-path-next path))))))

;;;     gtk_tree_path_prev

(test gtk-tree-path-prev
  (let ((path (gtk:tree-path-new-from-string "10:4:2")))
    (is (string= "10:4:1" (gtk:tree-path-to-string (gtk:tree-path-prev path))))
    (is (string= "10:4:1" (gtk:tree-path-to-string path)))
    (is (string= "10:4:0" (gtk:tree-path-to-string (gtk:tree-path-prev path))))
    (is-false (gtk:tree-path-prev path))))

;;;     gtk_tree_path_up

(test gtk-tree-path-up
  (let ((path (gtk:tree-path-new-from-string "10:4:2")))
    (is (string= "10:4" (gtk:tree-path-to-string (gtk:tree-path-up path))))
    (is (string= "10:4" (gtk:tree-path-to-string path)))
    (is (string= "10"   (gtk:tree-path-to-string (gtk:tree-path-up path))))
    (is-false (gtk:tree-path-to-string (gtk:tree-path-up path)))))

;;;     gtk_tree_path_down

(test gtk-tree-path-down
  (let ((path (gtk:tree-path-new-from-string "10:4:2")))
    (is (string= "10:4:2:0"
                 (gtk:tree-path-to-string (gtk:tree-path-down path))))
    (is (string= "10:4:2:0:0"
                 (gtk:tree-path-to-string (gtk:tree-path-down path))))))

;;;     gtk_tree_path_is_ancestor

(test gtk-tree-path-is-ancestor
  (let ((path1 (gtk:tree-path-new-from-string "10:4:3"))
        (path2 (gtk:tree-path-new-from-string "10:4:3:2")))
    (is-true (gtk:tree-path-is-ancestor path1 path2))
    (is-false (gtk:tree-path-is-ancestor path2 path1))))

;;;     gtk_tree_path_is_descendant

(test gtk-tree-path-is-descendant
  (let ((path1 (gtk:tree-path-new-from-string "10:4:3"))
        (path2 (gtk:tree-path-new-from-string "10:4:3:2")))
    (is-false (gtk:tree-path-is-descendant path1 path2))
    (is-true (gtk:tree-path-is-descendant path2 path1))))

;;;     GtkTreeRowReference

(test gtk-tree-row-reference-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkTreeRowReference"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeRowReference")
          (g:gtype (cffi:foreign-funcall "gtk_tree_row_reference_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:tree-row-reference
          (glib:symbol-for-gtype "GtkTreeRowReference"))))

;;;     gtk_tree_row_reference_new
;;;     gtk_tree_row_reference_get_model
;;;     gtk_tree_row_reference_get_path
;;;     gtk_tree_row_reference_valid

(test gtk-tree-row-reference-new
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((model 3) :strong 1)
      (let ((path (gtk:tree-path-new-from-string "2"))
            (row nil))
        (setf model (create-and-fill-list-store))
        (is (= 1 (g:object-ref-count model)))
        (is (typep (setf row (gtk:tree-row-reference-new model path))
                   'gtk:tree-row-reference))
        (is (gtk:tree-row-reference-valid row))
        (is (typep (gtk:tree-row-reference-model row) 'gtk:tree-model))
        (is (typep (gtk:tree-row-reference-path row) 'gtk:tree-path))))))

;;;     gtk_tree_row_reference_new_proxy
;;;     gtk_tree_row_reference_free

;;;     gtk_tree_row_reference_copy

(test gtk-tree-row-reference-copy
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((model 5) :strong 1)
      (let ((path (gtk:tree-path-new-from-string "2"))
            (row1 nil) (row2 nil))
        (setf model (create-and-fill-list-store))
        (is (= 1 (g:object-ref-count model)))
        (is (typep (setf row1 (gtk:tree-row-reference-new model path))
                   'gtk:tree-row-reference))
        (is (typep (setf row2 (gtk:tree-row-reference-copy row1))
                   'gtk:tree-row-reference))
        (is (not (cffi:pointer-eq (glib:pointer row1)
                                  (glib:pointer row2))))
        (is (cffi:pointer-eq
                (glib:pointer (gtk:tree-row-reference-model row1))
                (glib:pointer (gtk:tree-row-reference-model row2))))))))

;;;     gtk_tree_row_reference_inserted
;;;     gtk_tree_row_reference_deleted
;;;     gtk_tree_row_reference_reordered

;;;     GtkTreeModelIface
;;;     GtkTreeModelFlags

;;;     GtkTreeModel

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeModelForeachFunc

;;;     gtk_tree_model_get_flags

(test gtk-tree-model-flags
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:list-store))
    (is (equal '(:ITERS-PERSIST :LIST-ONLY) (gtk:tree-model-flags model)))))

;;;     gtk_tree_model_get_n_columns

(test gtk-tree-model-n-columns
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:list-store
                               :column-types
                               '("gint" "gchararray" "gboolean")))
    (is (= 3 (gtk:tree-model-n-columns model)))))

;;;     gtk_tree_model_get_column_type

(test gtk-tree-model-column-type
  (glib-test:with-check-memory (model)
    (setf model (make-instance 'gtk:list-store
                               :column-types
                               '("gint" "gchararray" "gboolean")))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type model 0))))
    (is (string= "gchararray" (g:type-name (gtk:tree-model-column-type model 1))))
    (is (string= "gboolean" (g:type-name (gtk:tree-model-column-type model 2))))))

;;;     gtk_tree_model_get_iter

(test gtk-tree-model-iter
  (glib-test:with-check-memory (model)
    (let ((path (gtk:tree-path-new-from-string "2")))
      (setf model (create-and-fill-list-store))
      (is (eq 'gtk:tree-iter (type-of (gtk:tree-model-iter model path)))))))

;;;     gtk_tree_model_get_iter_from_string

(test gtk-tree-model-iter-from-string
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-list-store))
    (is (eq 'gtk:tree-iter (type-of (gtk:tree-model-iter-from-string model "2"))))))

;;;     gtk_tree_model_get_iter_first

(test gtk-tree-model-iter-first
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-list-store))
    (is (eq 'gtk:tree-iter (type-of (gtk:tree-model-iter-first model))))))

;;;     gtk_tree_model_get_path

;(test tree-model-path
;  (let* ((model (create-and-fill-list-store))
;         (iter (gtk:tree-model-iter-from-string model "2")))
;    (is (string= "2"
;                 (gtk:tree-path-to-string (gtk:tree-model-path model iter))))
;))

;;;     gtk_tree_model_get_value
;;;     gtk_tree_model_iter_next
;;;     gtk_tree_model_iter_previous
;;;     gtk_tree_model_iter_children
;;;     gtk_tree_model_iter_has_child
;;;     gtk_tree_model_iter_n_children
;;;     gtk_tree_model_iter_nth_child
;;;     gtk_tree_model_iter_parent
;;;     gtk_tree_model_get_string_from_iter
;;;     gtk_tree_model_ref_node
;;;     gtk_tree_model_unref_node
;;;     gtk_tree_model_get
;;;     gtk_tree_model_get_valist
;;;     gtk_tree_model_foreach
;;;     gtk_tree_model_row_changed
;;;     gtk_tree_model_row_inserted
;;;     gtk_tree_model_row_has_child_toggled
;;;     gtk_tree_model_row_deleted
;;;     gtk_tree_model_rows_reordered
;;;     gtk_tree_model_rows_reordered_with_length

;;; 2025-1-6
