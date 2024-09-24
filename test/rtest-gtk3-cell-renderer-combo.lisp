(in-package :gtk-test)

(def-suite gtk-cell-renderer-combo :in gtk-suite)
(in-suite gtk-cell-renderer-combo)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererCombo

(test gtk-cell-renderer-combo-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererCombo"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-combo
          (glib:symbol-for-gtype "GtkCellRendererCombo")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererCombo")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_combo_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkCellRendererCombo")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererCombo")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererCombo")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkCellRendererCombo")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellRendererCombo")))
  ;; Check class definition
  ;; FIXME: We have no class definition!?
  (is (equal '()
             (gobject:get-gtype-definition "GtkCellRendererCombo"))))

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

;;; 2024-9-23
