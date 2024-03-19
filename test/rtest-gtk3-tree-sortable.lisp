(in-package :gtk-test)

(def-suite gtk-tree-sortable :in gtk-suite)
(in-suite gtk-tree-sortable)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSortable
;;;     GtkTreeSortableIface

;;;     GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID
;;;     GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID

(test gtk-default/unsorted-sort-column-id
  (is (= -1 gtk:+default-sort-column-id+))
  (is (= -2 gtk:+unsorted-sort-column-id+)))

;;; --- Signals ----------------------------------------------------------------

;;;     sort-column-changed

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeIterCompareFunc

;;;     gtk_tree_sortable_sort_column_changed
;;;     gtk_tree_sortable_get_sort_column_id
;;;     gtk_tree_sortable_set_sort_column_id
;;;     gtk_tree_sortable_set_sort_func
;;;     gtk_tree_sortable_set_default_sort_func
;;;     gtk_tree_sortable_has_default_sort_func

;;; 2024-3-15
