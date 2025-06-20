(in-package :gtk-test)

(def-suite gtk-tree-view-column :in gtk-suite)
(in-suite gtk-tree-view-column)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeViewColumnSizing

(test gtk-tree-view-column-sizing
  ;; Check type
  (is (g:type-is-enum "GtkTreeViewColumnSizing"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeViewColumnSizing")
          (g:gtype (cffi:foreign-funcall "gtk_tree_view_column_sizing_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:tree-view-column-sizing
          (glib:symbol-for-gtype "GtkTreeViewColumnSizing")))
  ;; Check names
  (is (equal '("GTK_TREE_VIEW_COLUMN_GROW_ONLY" "GTK_TREE_VIEW_COLUMN_AUTOSIZE"
               "GTK_TREE_VIEW_COLUMN_FIXED")
             (glib-test:list-enum-item-names "GtkTreeViewColumnSizing")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkTreeViewColumnSizing")))
  ;; Check nick names
  (is (equal '("grow-only" "autosize" "fixed")
             (glib-test:list-enum-item-nicks "GtkTreeViewColumnSizing")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkTreeViewColumnSizing"
                                    GTK:TREE-VIEW-COLUMN-SIZING
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_tree_view_column_sizing_get_type")
                                    (:GROW-ONLY 0)
                                    (:AUTOSIZE 1)
                                    (:FIXED 2))
             (gobject:get-gtype-definition "GtkTreeViewColumnSizing"))))

;;;     GtkTreeViewColumn

(test gtk-tree-view-column-class
  ;; Check type
  (is (g:type-is-object "GtkTreeViewColumn"))
  ;; Check registered name
  (is (eq 'gtk:tree-view-column
          (glib:symbol-for-gtype "GtkTreeViewColumn")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeViewColumn")
          (g:gtype (cffi:foreign-funcall "gtk_tree_view_column_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GInitiallyUnowned")
          (g:type-parent "GtkTreeViewColumn")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeViewColumn")))
  ;; Check interfaces
  (is (equal '("GtkCellLayout" "GtkBuildable")
             (glib-test:list-interfaces "GtkTreeViewColumn")))
  ;; Check class properties
  (is (equal '("alignment" "cell-area" "clickable" "expand" "fixed-width"
               "max-width" "min-width" "reorderable" "resizable" "sizing"
               "sort-column-id" "sort-indicator" "sort-order" "spacing" "title"
               "visible" "widget" "width" "x-offset")
             (glib-test:list-properties "GtkTreeViewColumn")))
  ;; Check signals
  (is (equal '("clicked")
             (glib-test:list-signals "GtkTreeViewColumn")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeViewColumn" GTK:TREE-VIEW-COLUMN
                      (:SUPERCLASS G:INITIALLY-UNOWNED
                       :EXPORT T
                       :INTERFACES ("GtkBuildable" "GtkCellLayout")
                       :TYPE-INITIALIZER "gtk_tree_view_column_get_type")
                      ((ALIGNMENT TREE-VIEW-COLUMN-ALIGNMENT
                        "alignment" "gfloat" T T)
                       (CELL-AREA TREE-VIEW-COLUMN-CELL-AREA
                        "cell-area" "GtkCellArea" T NIL)
                       (CLICKABLE TREE-VIEW-COLUMN-CLICKABLE
                        "clickable" "gboolean" T T)
                       (EXPAND TREE-VIEW-COLUMN-EXPAND "expand" "gboolean" T T)
                       (FIXED-WIDTH TREE-VIEW-COLUMN-FIXED-WIDTH
                        "fixed-width" "gint" T T)
                       (MAX-WIDTH TREE-VIEW-COLUMN-MAX-WIDTH
                        "max-width" "gint" T T)
                       (MIN-WIDTH TREE-VIEW-COLUMN-MIN-WIDTH
                        "min-width" "gint" T T)
                       (REORDERABLE TREE-VIEW-COLUMN-REORDERABLE
                        "reorderable" "gboolean" T T)
                       (RESIZABLE TREE-VIEW-COLUMN-RESIZABLE
                        "resizable" "gboolean" T T)
                       (SIZING TREE-VIEW-COLUMN-SIZING
                        "sizing" "GtkTreeViewColumnSizing" T T)
                       (SORT-COLUMN-ID TREE-VIEW-COLUMN-SORT-COLUMN-ID
                        "sort-column-id" "gint" T T)
                       (SORT-INDICATOR TREE-VIEW-COLUMN-SORT-INDICATOR
                        "sort-indicator" "gboolean" T T)
                       (SORT-ORDER TREE-VIEW-COLUMN-SORT-ORDER
                        "sort-order" "GtkSortType" T T)
                       (SPACING TREE-VIEW-COLUMN-SPACING "spacing" "gint" T T)
                       (TITLE TREE-VIEW-COLUMN-TITLE "title" "gchararray" T T)
                       (VISIBLE TREE-VIEW-COLUMN-VISIBLE
                        "visible" "gboolean" T T)
                       (WIDGET TREE-VIEW-COLUMN-WIDGET "widget" "GtkWidget" T T)
                       (WIDTH TREE-VIEW-COLUMN-WIDTH "width" "gint" T NIL)
                       (X-OFFSET TREE-VIEW-COLUMN-X-OFFSET
                        "x-offset" "gint" T NIL)))
             (gobject:get-gtype-definition "GtkTreeViewColumn"))))

;;; --- Signals ----------------------------------------------------------------

;;;     clicked

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-view-column-properties
  (glib-test:with-check-memory (column :strong 1)
    (is (typep (setf column
                     (make-instance 'gtk:tree-view-column))
               'gtk:tree-view-column))
    (is (= 0.0 (gtk:tree-view-column-alignment column)))
    ;; Creates a strong reference for GtkCellAreaBox object
    (is (typep (gtk:tree-view-column-cell-area column) 'gtk:cell-area))
    (is-false (gtk:tree-view-column-clickable column))
    (is-false (gtk:tree-view-column-expand column))
    (is (= -1 (gtk:tree-view-column-fixed-width column)))
    (is (= -1 (gtk:tree-view-column-max-width column)))
    (is (= -1 (gtk:tree-view-column-min-width column)))
    (is-false (gtk:tree-view-column-reorderable column))
    (is-false (gtk:tree-view-column-resizable column))
    (is (eq :grow-only (gtk:tree-view-column-sizing column)))
    (is (= -1 (gtk:tree-view-column-sort-column-id column)))
    (is-false (gtk:tree-view-column-sort-indicator column))
    (is (eq :ascending (gtk:tree-view-column-sort-order column)))
    (is (= 0 (gtk:tree-view-column-spacing column)))
    (is (string= "" (gtk:tree-view-column-title column)))
    (is-true (gtk:tree-view-column-visible column))
    (is-false (gtk:tree-view-column-widget column))
    (is (= 0 (gtk:tree-view-column-width column)))
    (is (= 0 (gtk:tree-view-column-x-offset column)))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeCellDataFunc

;;;     gtk_tree_view_column_new

(test gtk-tree-view-column-new
  (glib-test:with-check-memory (column)
    (is (typep (setf column
                     (gtk:tree-view-column-new)) 'gtk:tree-view-column))))

;;;     gtk_tree_view_column_new_with_area

(test gtk-tree-view-column-new-with-area
  (glib-test:with-check-memory ((area 3) column :strong 1)
    (is (typep (setf area (gtk:cell-area-box-new)) 'gtk:cell-area-box))
    (is (typep (setf column
                     (gtk:tree-view-column-new-with-area area))
               'gtk:tree-view-column))))

;;;     gtk_tree_view_column_new_with_attributes

(test gtk-tree-view-column-new-with-attributes
  (glib-test:with-check-memory (renderer column)
    (is (typep (setf renderer
                     (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
    (is (typep (setf column
                     (gtk:tree-view-column-new-with-attributes "Example"
                                                               renderer
                                                               "text" 0
                                                               "foreground" 1))
               'gtk:tree-view-column))
    ;; Remove references
    (is-false (gtk:tree-view-column-clear column))))

;;;     gtk_tree_view_column_pack_start
;;;     gtk_tree_view_column_pack_end
;;;     gtk_tree_view_column_clear

(test gtk-tree-view-column-pack-start/end
  (glib-test:with-check-memory (column renderer1 renderer2)
    (is (typep (setf column (gtk:tree-view-column-new)) 'gtk:tree-view-column))
    (is (typep (setf renderer1
                     (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
    (is (typep (setf renderer2
                     (gtk:cell-renderer-text-new)) 'gtk:cell-renderer-text))
    (is-false (gtk:tree-view-column-pack-start column renderer1))
    (is-false (gtk:tree-view-column-pack-end column renderer2))
    ;; Remove references
    (is-false (gtk:tree-view-column-clear column))))

;;;     gtk_tree_view_column_add_attribute
;;;     gtk_tree_view_column_set_attributes
;;;     gtk_tree_view_column_set_cell_data_func
;;;     gtk_tree_view_column_clear_attributes
;;;     gtk_tree_view_column_clicked
;;;     gtk_tree_view_column_get_button
;;;     gtk_tree_view_column_cell_set_cell_data
;;;     gtk_tree_view_column_cell_get_size
;;;     gtk_tree_view_column_cell_get_position
;;;     gtk_tree_view_column_cell_is_visible
;;;     gtk_tree_view_column_focus_cell
;;;     gtk_tree_view_column_queue_resize
;;;     gtk_tree_view_column_get_tree_view

;;; 2025-06-10
