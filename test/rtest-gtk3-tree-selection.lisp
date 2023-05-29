(in-package :gtk-test)

(def-suite gtk-tree-selection :in gtk-suite)
(in-suite gtk-tree-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSelection

(test tree-selection-class
  ;; Type check
  (is (g:type-is-object "GtkTreeSelection"))
  ;; Check the registered name
  (is (eq 'gtk:tree-selection
          (glib:symbol-for-gtype "GtkTreeSelection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTreeSelection")
          (g:gtype (cffi:foreign-funcall "gtk_tree_selection_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkTreeSelection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTreeSelection")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkTreeSelection")))
  ;; Check the class properties
  (is (equal '("mode")
             (list-properties "GtkTreeSelection")))
  ;; Check the signals
  (is (equal '("changed")
             (list-signals "GtkTreeSelection")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTreeSelection" GTK-TREE-SELECTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_tree_selection_get_type")
                       ((MODE GTK-TREE-SELECTION-MODE "mode" "GtkSelectionMode"
                         T T)))
             (gobject:get-g-type-definition "GtkTreeSelection"))))

;;; --- Properties -------------------------------------------------------------

(test tree-selection-properties
  (let* ((tree-view (make-instance 'gtk:tree-view))
         (selection (gtk:tree-view-selection tree-view)))
    (is (eq 'gtk:tree-selection (type-of selection)))
    (is (eq :single (gtk:tree-selection-mode selection)))
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
    (is (eq :multiple (gtk:tree-selection-mode selection)))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeSelectionFunc
;;;     GtkTreeSelectionForeachFunc

;;;     gtk_tree_selection_set_select_function
;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data

;;;     gtk_tree_selection_get_tree_view

(test tree-selection-tree-view
  (let* ((tree-view (make-instance 'gtk:tree-view))
         (selection (gtk:tree-view-selection tree-view)))
    (is (typep selection 'gtk:tree-selection))
    (is (typep (gtk:tree-selection-tree-view selection) 'gtk:tree-view))))

;;;     gtk_tree_selection_get_selected

;; TODO: Implement more examples for this function.

(test tree-selection-selected
  (let* ((tree-view (make-instance 'gtk:tree-view))
         (selection (gtk:tree-view-selection tree-view)))
    (is-false (gtk:tree-selection-selected selection))
))

;;;     gtk_tree_selection_selected_foreach
;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows
;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected
;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected
;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all
;;;     gtk_tree_selection_select_range
;;;     gtk_tree_selection_unselect_range

;;; --- 2023-5-29 --------------------------------------------------------------
