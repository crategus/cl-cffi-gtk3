(in-package :gtk-test)

(def-suite gtk-tree-sortable :in gtk-suite)
(in-suite gtk-tree-sortable)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
;;;     GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID

(test gtk-default/unsorted-sort-column-id
  (is (= -1 gtk:+default-sort-column-id+))
  (is (= -2 gtk:+unsorted-sort-column-id+)))

;;;     GtkTreeSortable

(test gtk-tree-sortable-interface
  ;; Check type
  (is (g:type-is-interface "GtkTreeSortable"))
  ;; Check registered name
  (is (eq 'gtk:tree-sortable
          (glib:symbol-for-gtype "GtkTreeSortable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeSortable")
          (g:gtype (cffi:foreign-funcall "gtk_tree_sortable_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkTreeModel" "GObject")
             (glib-test:list-interface-prerequisites "GtkTreeSortable")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkTreeSortable")))
  ;; Check signals
  (is (equal '("sort-column-changed")
             (glib-test:list-signals "GtkTreeSortable")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkTreeSortable" GTK:TREE-SORTABLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_tree_sortable_get_type"))
             (gobject:get-gtype-definition "GtkTreeSortable"))))

;;; --- Signals ----------------------------------------------------------------

;;;     sort-column-changed

(test gtk-tree-sortable-sort-column-changed-signal
  (let* ((name "sort-column-changed")
         (gtype (g:gtype "GtkTreeSortable"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeIterCompareFunc

;;;     gtk_tree_sortable_sort_column_changed

(test gtk-tree-sortable-sort-column-changed
  (glib-test:with-check-memory (sortable)
    (setf sortable (create-and-fill-list-store))
    (let (msg handler)
      (setf handler (g:signal-connect sortable "sort-column-changed"
                            (lambda (sortable1)
                              (is (eq sortable sortable1))
                              (setf msg "sort-column-changed")
                              t)))
      ;; Emit signal
      (gtk:tree-sortable-sort-column-changed sortable)
      (is (string= "sort-column-changed" msg))
      (is-false (g:signal-handler-disconnect sortable handler)))))

;;;     gtk_tree_sortable_get_sort_column_id
;;;     gtk_tree_sortable_set_sort_column_id

(test gtk-tree-sortable-sort-column-id
  (glib-test:with-check-memory (sortable)
    (setf sortable (create-and-fill-list-store))
    (setf (gtk:tree-sortable-sort-column-id sortable) 1)
    (is (equal '(1 :ascending)
               (multiple-value-list (gtk:tree-sortable-sort-column-id sortable))))
    (setf (gtk:tree-sortable-sort-column-id sortable :descending) 1)
    (is (equal '(1 :descending)
               (multiple-value-list (gtk:tree-sortable-sort-column-id sortable))))))

;;;     gtk_tree_sortable_set_sort_func

(test gtk-tree-sortable-set-sort-func
  (glib-test:with-check-memory (sortable)
    (setf sortable (create-and-fill-list-store))

    (is-false (gtk:tree-sortable-set-sort-func sortable 1
                      (lambda (model iter1 iter2)
                        (let ((column (gtk:tree-sortable-sort-column-id model)))
                          (format t " ~a : ~a and ~a~%"
                                    column
                                    (gtk:tree-model-value model iter1 column)
                                    (gtk:tree-model-value model iter2 column))
                          1))))))

;;;     gtk_tree_sortable_set_default_sort_func
;;;     gtk_tree_sortable_has_default_sort_func

(test gtk-tree-sortable-default-sort-func
  (glib-test:with-check-memory (sortable)
    (setf sortable (create-and-fill-list-store))

    (is-false (gtk:tree-sortable-has-default-sort-func sortable))

    (is-false (gtk:tree-sortable-set-default-sort-func sortable
                      (lambda (model iter1 iter2)
                        (let ((column (gtk:tree-sortable-sort-column-id model)))
                          (format t " ~a : ~a and ~a~%"
                                    column
                                    (gtk:tree-model-value model iter1 column)
                                    (gtk:tree-model-value model iter2 column))
                          1))))

    (is-true (gtk:tree-sortable-has-default-sort-func sortable))))

;;; 2026-05-21
