(in-package :gtk-test)

(def-suite gtk-cell-layout :in gtk-suite)
(in-suite gtk-cell-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellLayout

(test gtk-cell-layout-interface
  ;; Type check
  (is-true (g:type-is-interface "GtkCellLayout"))
  ;; Check the registered name
  (is (eq 'gtk:cell-layout
          (glib:symbol-for-gtype "GtkCellLayout")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkCellLayout")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkCellLayout"
                                  GTK-CELL-LAYOUT
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_cell_layout_get_type"))
             (gobject:get-g-type-definition "GtkCellLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_layout_pack_start

(test gtk-cell-layout-pack-start
  (let ((layout (make-instance 'gtk:cell-view))
        (cell1 (make-instance 'gtk:cell-renderer-text))
        (cell2 (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell1))
    (is (equal cell1 (first (gtk:cell-layout-cells layout))))
    (is-false (gtk:cell-layout-pack-start layout cell2))
    (is (equal cell1 (first (gtk:cell-layout-cells layout))))
    (is (equal cell2 (second (gtk:cell-layout-cells layout))))))

;;;     gtk_cell_layout_pack_end

(test gtk-cell-layout-pack-end
  (let ((layout (make-instance 'gtk:cell-view))
        (cell1 (make-instance 'gtk:cell-renderer-text))
        (cell2 (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-end layout cell1))
    (is (equal cell1 (first (gtk:cell-layout-cells layout))))
    (is-false (gtk:cell-layout-pack-end layout cell2))
    (is (equal cell1 (first (gtk:cell-layout-cells layout))))
    (is (equal cell2 (second (gtk:cell-layout-cells layout))))))

;;;     gtk_cell_layout_get_area

(test gtk-cell-layout-area
  (let ((layout (make-instance 'gtk:cell-view))
        (cell (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell))
    (is (typep (gtk:cell-layout-area layout) 'gtk:cell-area-box))))

;;;     gtk_cell_layout_get_cells

(test gtk-cell-layout-cells
  (let ((layout (make-instance 'gtk:cell-view))
        (cell1 (make-instance 'gtk:cell-renderer-text))
        (cell2 (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell1))
    (is-false (gtk:cell-layout-pack-start layout cell2))
    (is-true (listp (gtk:cell-layout-cells layout)))
    (is (= 2 (length (gtk:cell-layout-cells layout))))
    (is (every (lambda (x) (typep x 'gtk:cell-renderer-text))
               (gtk:cell-layout-cells layout)))))

;;;     gtk_cell_layout_reorder

(test gtk-cell-layout-reorder
  (let ((layout (make-instance 'gtk:cell-view))
        (cell1 (make-instance 'gtk:cell-renderer-text))
        (cell2 (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell1))
    (is-false (gtk:cell-layout-pack-start layout cell2))
    (is (eq cell1 (first (gtk:cell-layout-cells layout))))
    (is (eq cell2 (second (gtk:cell-layout-cells layout))))
    (is-false (gtk:cell-layout-reorder layout cell1 1))
    (is (eq cell2 (first (gtk:cell-layout-cells layout))))
    (is (eq cell1 (second (gtk:cell-layout-cells layout))))))

;;;     gtk_cell_layout_clear

(test gtk-cell-layout-clear
  (let ((layout (make-instance 'gtk:cell-view))
        (cell1 (make-instance 'gtk:cell-renderer-text))
        (cell2 (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell1))
    (is-false (gtk:cell-layout-pack-start layout cell2))
    (is-false (gtk:cell-layout-clear layout))
    (is (equal '() (gtk:cell-layout-cells layout)))))

;;;     gtk_cell_layout_add_attribute

(test gtk-cell-layout-add-attribute
  (let ((layout (make-instance 'gtk:cell-view))
        (cell1 (make-instance 'gtk:cell-renderer-text))
        (cell2 (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell1))
    (is-false (gtk:cell-layout-pack-start layout cell2))
    (is-false (gtk:cell-layout-add-attribute layout cell1 "text" 0))
    (is-false (gtk:cell-layout-add-attribute layout cell1 "font" 1))
    (is-false (gtk:cell-layout-add-attribute layout cell2 "size" 2))))

;;;     gtk_cell_layout_set_attributes

(test gtk-cell-layout-set-attributes
  (let ((layout (make-instance 'gtk:cell-view))
        (cell (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell))
    (is-false (gtk:cell-layout-set-attributes layout
                                              cell
                                              "text" 0
                                              "font" 1
                                              "size" 2))))

;;;     GtkCellLayoutDataFunc
;;;     gtk_cell_layout_set_cell_data_func

;; TODO: Finish this example

(test gtk-cell-layout-set-cell-data-func.1
  (let ((layout (make-instance 'gtk:cell-view))
        (cell (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-set-cell-data-func layout cell
                  (lambda (layout cell model iter)
                    (declare (ignore layout cell model iter)))))
    (is-false (gtk:cell-layout-pack-start layout cell))
    (is-false (gtk:cell-layout-add-attribute layout cell "text" 0))))

(test gtk-cell-layout-set-cell-data-func.2
  (let ((layout (make-instance 'gtk:cell-view))
        (cell (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-set-cell-data-func layout cell nil))
    (is-false (gtk:cell-layout-pack-start layout cell))
    (is-false (gtk:cell-layout-add-attribute layout cell "text" 0))))

;;;     gtk_cell_layout_clear_attributes

(test gtk-cell-layout-clear-attributes
  (let ((layout (make-instance 'gtk:cell-view))
        (cell (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-layout-pack-start layout cell))
    (is-false (gtk:cell-layout-set-attributes layout
                                              cell
                                              "text" 0
                                              "font" 1
                                              "size" 2))
    (is-false (gtk:cell-layout-clear-attributes layout cell))))

;;; 2024-3-24
