(in-package :gtk-test)

(def-suite gtk-cell-renderer-toggle :in gtk-suite)
(in-suite gtk-cell-renderer-toggle)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererToggle

(test gtk-cell-renderer-toggle-class
  ;; Check type
  (is (g:type-is-object "GtkCellRendererToggle"))
  ;; Check registered name
  (is (eq 'gtk:cell-renderer-toggle
          (glib:symbol-for-gtype "GtkCellRendererToggle")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellRendererToggle")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_toggle_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererToggle")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellRendererToggle")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCellRendererToggle")))
  ;; Check class properties
  (is (equal '("activatable" "active" "inconsistent" "indicator-size" "radio")
             (glib-test:list-properties "GtkCellRendererToggle")))
  ;; Check signals
  (is (equal '("toggled")
             (glib-test:list-signals "GtkCellRendererToggle")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellRendererToggle"
                                       GTK:CELL-RENDERER-TOGGLE
                       (:SUPERCLASS GTK:CELL-RENDERER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_cell_renderer_toggle_get_type")
                       ((ACTIVATABLE CELL-RENDERER-TOGGLE-ACTIVATABLE
                         "activatable" "gboolean" T T)
                        (ACTIVE CELL-RENDERER-TOGGLE-ACTIVE
                         "active" "gboolean" T T)
                        (INCONSISTENT CELL-RENDERER-TOGGLE-INCONSISTENT
                         "inconsistent" "gboolean" T T)
                        (INDICATOR-SIZE CELL-RENDERER-TOGGLE-INDICATOR-SIZE
                         "indicator-size" "gint" T T)
                        (RADIO CELL-RENDERER-TOGGLE-RADIO
                         "radio" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkCellRendererToggle"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-toggle-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-toggle)))
    (is-true (gtk:cell-renderer-toggle-activatable renderer))
    (is-false (gtk:cell-renderer-toggle-active renderer))
    (is-false (gtk:cell-renderer-toggle-inconsistent renderer))
    (is (= 0 (gtk:cell-renderer-toggle-indicator-size renderer)))
    (is-false (gtk:cell-renderer-toggle-radio renderer))))

;;; --- Signals ----------------------------------------------------------------

;;;;     toggled

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_toggle_new

(test gtk-cell-renderer-toggle-new
  (is (typep (gtk:cell-renderer-toggle-new) 'gtk:cell-renderer-toggle)))

;;; 2024-9-22
