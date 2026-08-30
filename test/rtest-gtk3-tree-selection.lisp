(in-package :gtk-test)

(def-suite gtk-tree-selection :in gtk-suite)
(in-suite gtk-tree-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTreeSelection

(test gtk-tree-selection-class
  ;; Check type
  (is (g:type-is-object "GtkTreeSelection"))
  ;; Check registered name
  (is (eq 'gtk:tree-selection
          (glib:symbol-for-gtype "GtkTreeSelection")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTreeSelection")
          (g:gtype (cffi:foreign-funcall "gtk_tree_selection_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkTreeSelection")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTreeSelection")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTreeSelection")))
  ;; Check class properties
  (is (equal '("mode")
             (glib-test:list-properties "GtkTreeSelection")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkTreeSelection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTreeSelection" GTK:TREE-SELECTION
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_tree_selection_get_type")
                      ((MODE TREE-SELECTION-MODE
                        "mode" "GtkSelectionMode" T T)))
             (gobject:get-gtype-definition "GtkTreeSelection"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-tree-selection-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GtkTreeSelection"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tree-selection-properties
  (glib-test:with-check-memory (view (selection 2) :strong 1)
    (is (typep (setf view (make-instance 'gtk:tree-view)) 'gtk:tree-view))
    (is (typep (setf selection (gtk:tree-view-selection view)) 'gtk:tree-selection))
    ;; Property MODE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkTreeSelection" "mode")))
    (is (eq :single (gtk:tree-selection-mode selection)))
    (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkTreeSelectionFunc
;;;     GtkTreeSelectionForeachFunc

;;;     gtk_tree_selection_set_select_function

(test gtk-tree-selection-set-select-function
  (glib-test:with-check-memory (view (selection 2) :strong 2)
    (let (msg)
      (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
      (setf selection (gtk:tree-view-selection view))
      (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
      (is-false (gtk:tree-selection-set-select-function selection
                    (lambda (selection model path selected)
                      (declare (ignore selection model selected))
                      (push (gtk:tree-path-to-string path) msg))))
      (is-false (gtk:tree-selection-select-all selection))
      (is (equal '("0" "1" "2") (sort msg #'string<))))))

;;;     gtk_tree_selection_get_select_function
;;;     gtk_tree_selection_get_user_data

;;;     gtk_tree_selection_get_tree_view

(test gtk-tree-selection-tree-view
  (glib-test:with-check-memory (view (selection 2) :strong 1)
    (setf view (make-instance 'gtk:tree-view))
    (setf selection (gtk:tree-view-selection view))
    (is (typep (gtk:tree-selection-tree-view selection) 'gtk:tree-view))))

;;;     gtk_tree_selection_get_selected

;; TODO: Improve this test

(test gtk-tree-selection-selected
  (glib-test:with-check-memory ((view 2) (selection 2) :strong 3)
    (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
    (is (= 1 (g:object-ref-count view)))
    (setf selection (gtk:tree-view-selection view))
    (is (= 1 (g:object-ref-count view)))
    (let ((path (gtk:tree-path-new-from-string "1")))
      (is-false (gtk:tree-selection-select-path selection path))
      ;; FIXME: At this point we have got a second reference. Why?
      (is (= 2 (g:object-ref-count view)))
      (is (typep (gtk:tree-selection-selected selection) 'gtk:tree-iter)))))

;;;     gtk_tree_selection_selected_foreach

;; Not expanded tree view
(test gtk-tree-selection-selected-foreach.1
  (glib-test:with-check-memory (view (selection 2) :strong 3)
    (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
    (setf selection (gtk:tree-view-selection view))
    (let (msg)
      (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
      (is-false (gtk:tree-selection-select-all selection))
      (is-false (gtk:tree-selection-selected-foreach selection
                    (lambda (model path iter)
                      (declare (ignore model iter))
                      (push (gtk:tree-path-to-string path) msg))))
      (is (equal '("2" "1" "0") msg)))))

;; Expanded tree view
(test gtk-tree-selection-selected-foreach.2
  (glib-test:with-check-memory (view (selection 2) :strong 3)
    (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
    (setf selection (gtk:tree-view-selection view))
    (let (msg)
      (is (eq :multiple (setf (gtk:tree-selection-mode selection) :multiple)))
      (is-false (gtk:tree-view-expand-all view))
      (is-false (gtk:tree-selection-select-all selection))
      (is-false (gtk:tree-selection-selected-foreach selection
                    (lambda (model path iter)
                      (declare (ignore model iter))
                      (push (gtk:tree-path-to-string path) msg))))
      (is (equal '("2" "1" "0:2" "0:1" "0:0" "0") msg)))))

;;;     gtk_tree_selection_get_selected_rows
;;;     gtk_tree_selection_count_selected_rows

;;;     gtk_tree_selection_select_path
;;;     gtk_tree_selection_unselect_path
;;;     gtk_tree_selection_path_is_selected

(test gtk-tree-selection-select/unselect-path
  (glib-test:with-check-memory (view (selection 2) :strong 3)
    (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
    (setf selection (gtk:tree-view-selection view))
    (let ((path (gtk:tree-path-new-from-string "1")))
      (is-false (gtk:tree-selection-select-path selection path))
      (is-true (gtk:tree-selection-path-is-selected selection path))
      (is-false (gtk:tree-selection-unselect-path selection path))
      (is-false (gtk:tree-selection-path-is-selected selection path)))))

;;;     gtk_tree_selection_select_iter
;;;     gtk_tree_selection_unselect_iter
;;;     gtk_tree_selection_iter_is_selected

;;;     gtk_tree_selection_select_all
;;;     gtk_tree_selection_unselect_all

;; Not expanded tree view
(test gtk-tree-selection-select-all.1
  (glib-test:with-check-memory (view (selection 2) :strong 3)
    (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
    (setf selection (gtk:tree-view-selection view))
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
  (glib-test:with-check-memory (view (selection 2) :strong 3)
    (setf view (gtk:tree-view-new-with-model (create-and-fill-model-simple)))
    (setf selection (gtk:tree-view-selection view))
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

;;; 2026-05-21
