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

(defun create-and-fill-tree-store ()
  (let* ((model (gtk:tree-store-new "gchararray" "gchararray"))
         (parent (gtk:tree-store-append model nil))
         (parent1 nil)
         (child nil)
         (path (gtk:tree-model-path model parent)))
    (gtk:tree-store-set model
                        parent
                        (gtk:tree-path-to-string path)
                        "Songs")
    (setf child (gtk:tree-store-append model parent))
    (setf path (gtk:tree-model-path model child))
    (gtk:tree-store-set model
                        child
                        (gtk:tree-path-to-string path)
                        "MP3s")
    (setf child (gtk:tree-store-append model parent))
    (setf path (gtk:tree-model-path model child))
    (gtk:tree-store-set model
                        child
                        (gtk:tree-path-to-string path)
                        "Oggs")
    (setf parent (gtk:tree-store-append model nil))
    (setf path (gtk:tree-model-path model parent))
    (gtk:tree-store-set model
                        parent
                        (gtk:tree-path-to-string path)
                        "Videos")
    (setf parent1 (gtk:tree-store-append model parent))
    (setf path (gtk:tree-model-path model parent1))
    (gtk:tree-store-set model
                        parent1
                        (gtk:tree-path-to-string path)
                        "Clips")
    (setf child (gtk:tree-store-append model parent1))
    (setf path (gtk:tree-model-path model child))
    (gtk:tree-store-set model
                        child
                        (gtk:tree-path-to-string path)
                        "Funny Clips")
    (setf child (gtk:tree-store-append model parent1))
    (setf path (gtk:tree-model-path model child))
    (gtk:tree-store-set model
                        child
                        (gtk:tree-path-to-string path)
                        "Movie Trailers")
    (setf child (gtk:tree-store-append model parent))
    (setf path (gtk:tree-model-path model child))
    (gtk:tree-store-set model
                        child
                        (gtk:tree-path-to-string path)
                        "Movies")
    model))

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

;;;     gtk_tree_row_reference_free                         not needed

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

;;;     gtk_tree_row_reference_new_proxy                    not implemented
;;;     gtk_tree_row_reference_inserted                     not implemented
;;;     gtk_tree_row_reference_deleted                      not implemented
;;;     gtk_tree_row_reference_reordered                    not implemented

;;; ----------------------------------------------------------------------------

;;;     GtkTreeModelFlags

(test gtk-tree-model-flags
  ;; Check type
  (is (g:type-is-flags "GtkTreeModelFlags"))
  ;; Check registered name
  (is (eq 'gtk:tree-model-flags
          (glib:symbol-for-gtype "GtkTreeModelFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeModelFlags")
          (g:gtype (cffi:foreign-funcall "gtk_tree_model_flags_get_type" :size))))
  ;; Check names
  (is (equal '()
             (glib-test:list-flags-item-names "GtkTreeModelFlags")))
  ;; Check values
  (is (equal '()
             (glib-test:list-flags-item-values "GtkTreeModelFlags")))
  ;; Check nick names
  (is (equal '()
             (glib-test:list-flags-item-nicks "GtkTreeModelFlags")))
  ;; Check flags definition
  (is (equal '()
             (gobject:get-gtype-definition "GtkTreeModelFlags"))))

;;;     GtkTreeModel

(test gtk-tree-model-interface
  ;; Check type
  (is (g:type-is-interface "GtkTreeModel"))
  ;; Check registered name
  (is (eq 'gtk:tree-model
          (glib:symbol-for-gtype "GtkTreeModel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeModel")
          (g:gtype (cffi:foreign-funcall "gtk_tree_model_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkTreeModel")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkTreeModel")))
  ;; Check signals
  (is (equal '("row-changed" "row-deleted" "row-has-child-toggled"
               "row-inserted" "rows-reordered")
             (glib-test:list-signals "GtkTreeModel")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkTreeModel" GTK:TREE-MODEL
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_tree_model_get_type"))
             (gobject:get-gtype-definition "GtkTreeModel"))))

;;; --- Signals ----------------------------------------------------------------

;;;     row-changed

(test gtk-tree-model-row-changed-signal
  (let* ((name "row-changed")
         (gtype (g:gtype "GtkTreeModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkTreePath" "GtkTreeIter")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     row-deleted

(test gtk-tree-model-row-deleted-signal
  (let* ((name "row-deleted")
         (gtype (g:gtype "GtkTreeModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkTreePath")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     row-has-child-toggled

(test gtk-tree-model-row-has-child-toggled-signal
  (let* ((name "row-has-child-toggled")
         (gtype (g:gtype "GtkTreeModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkTreePath" "GtkTreeIter")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     row-inserted

(test gtk-tree-model-row-inserted-signal
  (let* ((name "row-inserted")
         (gtype (g:gtype "GtkTreeModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkTreePath" "GtkTreeIter")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     rows-reordered

(test gtk-tree-model-rows-reordered-signal
  (let* ((name "rows-reordered")
         (gtype (g:gtype "GtkTreeModel"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkTreePath" "GtkTreeIter" "gpointer")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

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

(test gtk-tree-model-path
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-list-store))
    (let ((iter (gtk:tree-model-iter-from-string model "2")))
      (is (string= "2"
                   (gtk:tree-path-to-string (gtk:tree-model-path model iter)))))))

;;;     gtk_tree_model_get_value
;;;     gtk_tree_model_get

(test gtk-tree-model-value
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-list-store))
    (let ((iter (gtk:tree-model-iter-from-string model "2")))
      (is (= 2 (gtk:tree-model-value model iter 0)))
      (is (string= "Name3" (gtk:tree-model-value model iter 1)))
      (is-true (gtk:tree-model-value model iter 2))

      (is (equal '(2) (gtk:tree-model-get model iter 0)))
      (is (equal '("Name3") (gtk:tree-model-get model iter 1)))
      (is (equal '(T) (gtk:tree-model-get model iter 2)))
      (is (equal '(2 "Name3" T) (gtk:tree-model-get model iter 0 1 2))))))

;;;     gtk_tree_model_get_valist                           not implemented

;;;     gtk_tree_model_iter_next
;;;     gtk_tree_model_iter_previous

(test gtk-tree-model-iter-next/previous
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-list-store))
    (let* ((iter (gtk:tree-model-iter-from-string model "2"))
           (next (gtk:tree-model-iter-next model iter))
           (prev (gtk:tree-model-iter-previous model iter)))
      ;; Check iterator
      (is (= 2 (gtk:tree-model-value model iter 0)))
      (is (string= "Name3" (gtk:tree-model-value model iter 1)))
      (is-true (gtk:tree-model-value model iter 2))
      ;; Check next iterator
      (is (= 3 (gtk:tree-model-value model next 0)))
      (is (string= "Name4" (gtk:tree-model-value model next 1)))
      (is-false (gtk:tree-model-value model next 2))
      ;; Check previous iterator
      (is (= 1 (gtk:tree-model-value model prev 0)))
      (is (string= "Name2" (gtk:tree-model-value model prev 1)))
      (is-false (gtk:tree-model-value model prev 2)))))

;;;     gtk_tree_model_iter_children
;;;     gtk_tree_model_iter_has_child
;;;     gtk_tree_model_iter_n_children
;;;     gtk_tree_model_iter_nth_child
;;;     gtk_tree_model_iter_parent
;;;     gtk_tree_model_get_string_from_iter

(test gtk-tree-model-iter-children
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-tree-store))
    (let* ((parent (gtk:tree-model-iter-first model))
           (child (gtk:tree-model-iter-children model parent)))
      (is-true (gtk:tree-model-iter-has-child model parent))
      (is-false (gtk:tree-model-iter-has-child model child))
      (is (= 2 (gtk:tree-model-iter-n-children model parent)))
      (is (= 0 (gtk:tree-model-iter-n-children model child)))
      (is (string= "0" (gtk:tree-model-string-from-iter model parent)))
      (is (string= "0:0" (gtk:tree-model-string-from-iter model child)))
      (is (string= "0" (gtk:tree-model-value model parent 0)))
      (is (string= "Songs" (gtk:tree-model-value model parent 1)))
      (is (string= "0:0" (gtk:tree-model-value model child 0)))
      (is (string= "MP3s" (gtk:tree-model-value model child 1)))
      (let ((iter (gtk:tree-model-iter-parent model child)))
        (is (string= "0" (gtk:tree-model-value model iter 0)))
        (is (string= "Songs" (gtk:tree-model-value model iter 1))))
      (let ((iter (gtk:tree-model-iter-nth-child model parent 0)))
        (is (string= "0:0" (gtk:tree-model-value model iter 0)))
        (is (string= "MP3s" (gtk:tree-model-value model iter 1))))
      (let ((iter (gtk:tree-model-iter-nth-child model parent 1)))
        (is (string= "0:1" (gtk:tree-model-value model iter 0)))
        (is (string= "Oggs" (gtk:tree-model-value model iter 1)))))))

;;;     gtk_tree_model_ref_node
;;;     gtk_tree_model_unref_node

(test gtk-tree-model-ref/unref-node
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-tree-store))
    (let ((node (gtk:tree-model-iter-first model)))
      (is-false (gtk:tree-model-ref-node model node))
      (is-false (gtk:tree-model-unref-node model node)))))

;;;     gtk_tree_model_foreach

(test gtk-tree-model-foreach
  (glib-test:with-check-memory (model)
    (setf model (create-and-fill-tree-store))
    (let (result)
      (gtk:tree-model-foreach model
          (lambda (model path iter)
            (declare (ignore path))
            (push (gtk:tree-model-get model iter 0) result)
            nil))
      (is (equal '(("0") ("0:0") ("0:1") ("1") ("1:0") ("1:0:0") ("1:0:1") ("1:1"))
                 (reverse result))))))

;;;     gtk_tree_model_row_changed
;;;     gtk_tree_model_row_inserted
;;;     gtk_tree_model_row_has_child_toggled
;;;     gtk_tree_model_row_deleted
;;;     gtk_tree_model_rows_reordered
;;;     gtk_tree_model_rows_reordered_with_length

;;; 2026-05-21
