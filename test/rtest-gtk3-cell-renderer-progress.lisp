(in-package :gtk-test)

(def-suite gtk-cell-renderer-progress :in gtk-suite)
(in-suite gtk-cell-renderer-progress)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererProgress

(test cell-renderer-progress-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererProgress"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-progress
          (glib:symbol-for-gtype "GtkCellRendererProgress")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererProgress")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_progress_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererProgress")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererProgress")))
  ;; Check the interfaces
  (is (equal '("GtkOrientable")
             (list-interfaces "GtkCellRendererProgress")))
  ;; Check the class properties
  (is (equal '("inverted" "orientation" "pulse" "text" "text-xalign"
               "text-yalign" "value")
             (list-properties "GtkCellRendererProgress")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellRendererProgress")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkCellRendererProgress"
                                     GTK-CELL-RENDERER-PROGRESS
                       (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T :INTERFACES
                        ("GtkOrientable") :TYPE-INITIALIZER
                        "gtk_cell_renderer_progress_get_type")
                       ((INVERTED GTK-CELL-RENDERER-PROGRESS-INVERTED
                         "inverted" "gboolean" T T)
                        (PULSE GTK-CELL-RENDERER-PROGRESS-PULSE "pulse" "gint"
                         T T)
                        (TEXT GTK-CELL-RENDERER-PROGRESS-TEXT "text"
                         "gchararray" T T)
                        (TEXT-XALIGN GTK-CELL-RENDERER-PROGRESS-TEXT-XALIGN
                         "text-xalign" "gfloat" T T)
                        (TEXT-YALIGN GTK-CELL-RENDERER-PROGRESS-TEXT-YALIGN
                         "text-yalign" "gfloat" T T)
                        (VALUE GTK-CELL-RENDERER-PROGRESS-VALUE "value" "gint"
                         T T)))
             (gobject:get-g-type-definition "GtkCellRendererProgress"))))

;;; --- Properties -------------------------------------------------------------

;;;     inverted
;;;     pulse
;;;     text
;;;     text-xalign
;;;     text-yalign
;;;     value

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_progress_new

;;; --- 2023-5-29 --------------------------------------------------------------
