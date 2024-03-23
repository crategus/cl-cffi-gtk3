(in-package :gtk-test)

(def-suite gtk-tree-selection :in gtk-suite)
(in-suite gtk-tree-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSelection

(test gtk-tree-selection-class
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
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTreeSelection"
                                             GTK-TREE-SELECTION
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_tree_selection_get_type")
                       ((MODE GTK-TREE-SELECTION-MODE "mode" "GtkSelectionMode"
                         T T)))
             (gobject:get-g-type-definition "GtkTreeSelection"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-selection-properties
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

(test gtk-tree-selection-set-select-function
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view))
         msg)
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
    (is-false (gtk:tree-selection-set-select-function selection
                  (lambda (selection model path selected)
                    (declare (ignore selection model selected))
                    (push (gtk:tree-path-to-string path) msg))))
    (is-false (gtk:tree-selection-select-all selection))
    (is (equal '("0" "1" "2") (sort msg #'string<)))))

;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data

;;;     gtk_tree_selection_get_tree_view

(test gtk-tree-selection-tree-view
  (let* ((tree-view (make-instance 'gtk:tree-view))
         (selection (gtk:tree-view-selection tree-view)))
    (is (typep selection 'gtk:tree-selection))
    (is (typep (gtk:tree-selection-tree-view selection) 'gtk:tree-view))))

;;;     gtk_tree_selection_get_selected

;; TODO: Improve this test

(test gtk-tree-selection-selected
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view))
         (path (gtk:tree-path-new-from-string "1"))
         iter)

    (is-false (gtk:tree-selection-select-path selection path))
    (is (typep (setf iter
                     (gtk:tree-selection-selected selection)) 'gtk:tree-iter))))

;;;     gtk_tree_selection_selected_foreach

;; Not expanded tree view
(test gtk-tree-selection-selected-foreach.1
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view))
         msg)
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
    (is-false (gtk:tree-selection-select-all selection))
    (is-false (gtk:tree-selection-selected-foreach selection
                  (lambda (model path iter)
                    (declare (ignore model iter))
                    (push (gtk:tree-path-to-string path) msg))))
    (is (equal '("2" "1" "0") msg))))

;; Expanded tree view
(test gtk-tree-selection-selected-foreach.2
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view))
         msg)
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
    (is-false (gtk:tree-view-expand-all view))
    (is-false (gtk:tree-selection-select-all selection))
    (is-false (gtk:tree-selection-selected-foreach selection
                  (lambda (model path iter)
                    (declare (ignore model iter))
                    (push (gtk:tree-path-to-string path) msg))))
    (is (equal '("2" "1" "0:2" "0:1" "0:0" "0") msg))))

;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows

;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected

(test gtk-tree-selection-select/unselect-path
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view))
         (path (gtk:tree-path-new-from-string "1")))
    (is-false (gtk:tree-selection-select-path selection path))
    (is-true (gtk:tree-selection-path-is-selected selection path))
    (is-false (gtk:tree-selection-unselect-path selection path))
    (is-false (gtk:tree-selection-path-is-selected selection path))))

;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected

;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all

;; Not expanded tree view
(test gtk-tree-selection-select-all.1
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view)))
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
    (is (= 0 (gtk:tree-selection-count-selected-rows selection)))
    (is-false (gtk:tree-selection-select-all selection))
    (is (= 3 (gtk:tree-selection-count-selected-rows selection)))
    (is (equal '("0" "1" "2")
               (mapcar #'gtk:tree-path-to-string
                       (gtk:tree-selection-selected-rows selection))))
    (is-false (gtk:tree-selection-unselect-all selection))
    (is (= 0 (gtk:tree-selection-count-selected-rows selection)))
    (is (equal '()
               (mapcar #'gtk:tree-path-to-string
                       (gtk:tree-selection-selected-rows selection))))))

;; Expanded tree view
(test gtk-tree-selection-select-all.2
  (let* ((view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
         (selection (gtk:tree-view-selection view)))
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
    (is (= 0 (gtk:tree-selection-count-selected-rows selection)))
    (is-false (gtk:tree-view-expand-all view))
    (is-false (gtk:tree-selection-select-all selection))
    (is (= 6 (gtk:tree-selection-count-selected-rows selection)))
    (is (equal '("0" "0:0" "0:1" "0:2" "1" "2")
               (mapcar #'gtk:tree-path-to-string
                       (gtk:tree-selection-selected-rows selection))))
    (is-false (gtk:tree-selection-unselect-all selection))
    (is (= 0 (gtk:tree-selection-count-selected-rows selection)))
    (is (equal '()
               (mapcar #'gtk:tree-path-to-string
                       (gtk:tree-selection-selected-rows selection))))))

;;;     gtk_tree_selection_select_range
;;;     gtk_tree_selection_unselect_range

;;; 2024-3-19
