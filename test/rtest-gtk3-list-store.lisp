(in-package :gtk-test)

(def-suite gtk-list-store :in gtk-suite)
(in-suite gtk-list-store)

;;; --- Types and Values -------------------------------------------------------

;;;   GtkListStore

(test gtk-list-store-class
  ;; Check type
  (is (g:type-is-object "GtkListStore"))
  ;; Check registered name
  (is (eq 'gtk:list-store
          (glib:symbol-for-gtype "GtkListStore")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListStore")
          (g:gtype (cffi:foreign-funcall "gtk_list_store_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkListStore")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListStore")))
  ;; Check interfaces
  (is (equal '("GtkTreeModel" "GtkTreeDragSource" "GtkTreeDragDest"
               "GtkTreeSortable" "GtkBuildable")
             (glib-test:list-interfaces "GtkListStore")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkListStore")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkListStore")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListStore" GTK:LIST-STORE
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES
                        ("GtkBuildable" "GtkTreeDragDest" "GtkTreeDragSource"
                         "GtkTreeModel" "GtkTreeSortable")
                        :TYPE-INITIALIZER "gtk_list_store_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkListStore"))))

;;; --- Functions --------------------------------------------------------------

;;;   gtk_list_store_new

;; FIXME: Check this again. IS GString allowed for gchararray on Linux?

#-windows
(test gtk-list-store-new.1
  (let ((store (make-instance 'gtk:list-store
                              :column-types '("gint" "GString" "GdkPixbuf"))))
    (is (eq 'gtk:list-store (type-of store)))
    (is (= 3 (gtk:tree-model-n-columns store)))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type store 0))))
    (is (string= "GString" (g:type-name (gtk:tree-model-column-type store 1))))
    (is (string= "GdkPixbuf"
                 (g:type-name (gtk:tree-model-column-type store 2))))))

#+windows
(test gtk-list-store-new.1
  (let ((store (make-instance 'gtk:list-store
                              :column-types '("gint" "gchararray" "GdkPixbuf"))))
    (is (eq 'gtk:list-store (type-of store)))
    (is (= 3 (gtk:tree-model-n-columns store)))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type store 0))))
    (is (string= "gchararray" (g:type-name (gtk:tree-model-column-type store 1))))
    (is (string= "GdkPixbuf"
                 (g:type-name (gtk:tree-model-column-type store 2))))))

#-windows
(test gtk-list-store-new.2
  (let ((store (gtk:list-store-new "gint" "GString" "GdkPixbuf")))
    (is (eq 'gtk:list-store (type-of store)))
    (is (= 3 (gtk:tree-model-n-columns store)))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type store 0))))
    (is (string= "GString" (g:type-name (gtk:tree-model-column-type store 1))))
    (is (string= "GdkPixbuf"
                 (g:type-name (gtk:tree-model-column-type store 2))))))

#+windows
(test gtk-list-store-new.2
  (let ((store (gtk:list-store-new "gint" "gchararray" "GdkPixbuf")))
    (is (eq 'gtk:list-store (type-of store)))
    (is (= 3 (gtk:tree-model-n-columns store)))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type store 0))))
    (is (string= "gchararray" (g:type-name (gtk:tree-model-column-type store 1))))
    (is (string= "GdkPixbuf"
                 (g:type-name (gtk:tree-model-column-type store 2))))))

;;;   gtk_list_store_newv                                  Not implemented

;;;   gtk_list_store_set_column_types

;; FIXME: Check this again. IS GString allowed for gchararray on Linux?

#-windows
(test gtk-list-store-set-column-types
  (let ((store (make-instance 'gtk:list-store)))
    (is-false (gtk:list-store-set-column-types store
                                               "gint" "GString" "GdkPixbuf"))
    (is (= 3 (gtk:tree-model-n-columns store)))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type store 0))))
    (is (string= "GString" (g:type-name (gtk:tree-model-column-type store 1))))
    (is (string= "GdkPixbuf"
                 (g:type-name (gtk:tree-model-column-type store 2))))))

#+windows
(test gtk-list-store-set-column-types
  (let ((store (make-instance 'gtk:list-store)))
    (is-false (gtk:list-store-set-column-types store
                                               "gint" "gchararray" "GdkPixbuf"))
    (is (= 3 (gtk:tree-model-n-columns store)))
    (is (string= "gint" (g:type-name (gtk:tree-model-column-type store 0))))
    (is (string= "gchararray" (g:type-name (gtk:tree-model-column-type store 1))))
    (is (string= "GdkPixbuf"
                 (g:type-name (gtk:tree-model-column-type store 2))))))

;;;   gtk_list_store_set

(test gtk-list-store-set
  (let* ((store (gtk:list-store-new "gint" "gchararray"))
         (iter (gtk:list-store-set store (gtk:list-store-append store)
                                         99 "string")))
    (is (= 99 (gtk:tree-model-value store iter 0)))
    (is (equal "string" (gtk:tree-model-value store iter 1)))
    (is (eq 'gtk:tree-iter (type-of (gtk:list-store-set store iter 199))))
    (is (= 199 (gtk:tree-model-value store iter 0)))
    (is (equal "string" (gtk:tree-model-value store iter 1)))))

;;;   gtk_list_store_set_valist                            Not implemented

;;;   gtk_list_store_set_value

(test gtk-list-store-set-value
  (let* ((store (gtk:list-store-new "gint" "gchararray"))
         (iter (gtk:list-store-append store)))
    (is-false (gtk:list-store-set-value store iter 0 99))
    (is (= 99 (gtk:tree-model-value store iter 0)))
    (is-false (gtk:list-store-set-value store iter 1 "string"))
    (is (equal "string" (gtk:tree-model-value store iter 1)))))

;;;   gtk_list_store_set_valuesv                         Not exported

;;;   gtk_list_store_remove

(test gtk-list-store-remove
  (let* ((store (gtk:list-store-new "gint" "gchararray"))
         (iter (gtk:list-store-append store)))
    (is-true iter)
    (setf iter (gtk:list-store-remove store iter))
    (is-false iter)))

;;;   gtk_list_store_insert



;;;     gtk_list_store_insert_before
;;;     gtk_list_store_insert_after
;;;     gtk_list_store_insert_with_values
;;;     gtk_list_store_insert_with_valuesv
;;;     gtk_list_store_prepend
;;;     gtk_list_store_append
;;;     gtk_list_store_clear
;;;     gtk_list_store_iter_is_valid
;;;     gtk_list_store_reorder
;;;     gtk_list_store_swap


;;;     gtk_list_store_move_before

(test gtk-list-store-move-after
  (let* ((store (gtk:list-store-new "gint" "gchararray"))
         (iter (gtk:list-store-append store)))
    (setf iter (gtk:list-store-append store))
    (gtk:list-store-move-before store iter nil)))

;;;     gtk_list_store_move_after

(test gtk-list-store-move-after
  (let* ((store (gtk:list-store-new "gint" "gchararray"))
         (iter (gtk:list-store-append store)))
    (setf iter (gtk:list-store-prepend store))
    (gtk:list-store-move-after store iter nil)))

;;; 2024-9-22
