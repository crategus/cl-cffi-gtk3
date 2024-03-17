(in-package :gtk-test)

(def-suite gtk-cell-renderer-combo :in gtk-suite)
(in-suite gtk-cell-renderer-combo)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererCombo

(test gtk-cell-renderer-combo-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererCombo"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-combo
          (glib:symbol-for-gtype "GtkCellRendererCombo")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererCombo")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_combo_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCellRendererCombo")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererCombo")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererCombo")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkCellRendererCombo")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellRendererCombo")))
  ;; Check the class definition
  (is (equal '()
             (gobject:get-g-type-definition "GtkCellRendererCombo"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-combo
  (let ((renderer (make-instance 'gtk:cell-renderer-combo)))
    (is-true (gtk:cell-renderer-combo-has-entry renderer))
    (is-false (gtk:cell-renderer-combo-model renderer))
    (is (= -1 (gtk:cell-renderer-combo-text-column renderer)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_combo_new

(test gtk_cell-renderer-combo-new
  (is (typep (gtk:cell-renderer-combo-new) 'gtk:cell-renderer-combo)))

;;; 2024-3-17
