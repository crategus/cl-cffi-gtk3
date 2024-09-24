(in-package :gtk-test)

(def-suite gtk-cell-view :in gtk-suite)
(in-suite gtk-cell-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellView

(test gtk-cell-view-class
  ;; Check type
  (is (g:type-is-object "GtkCellView"))
  ;; Check registered name
  (is (eq 'gtk:cell-view
          (glib:symbol-for-gtype "GtkCellView")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCellView")
          (g:gtype (cffi:foreign-funcall "gtk_cell_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkCellView")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCellView")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkCellView")))
  ;; Check class properties
  (is (equal '("background" "background-gdk" "background-rgba" "background-set"
               "cell-area" "cell-area-context" "draw-sensitive" "fit-model"
               "model" "orientation")
             (glib-test:list-properties "GtkCellView")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkCellView")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCellView")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCellView" GTK:CELL-VIEW
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_cell_view_get_type")
                       ((BACKGROUND CELL-VIEW-BACKGROUND
                         "background" "gchararray" NIL T)
                        (BACKGROUND-GDK CELL-VIEW-BACKGROUND-GDK
                         "background-gdk" "GdkColor" T T)
                        (BACKGROUND-RGBA CELL-VIEW-BACKGROUND-RGBA
                         "background-rgba" "GdkRGBA" T T)
                        (BACKGROUND-SET CELL-VIEW-BACKGROUND-SET
                         "background-set" "gboolean" T T)
                        (CELL-AREA CELL-VIEW-CELL-AREA
                         "cell-area" "GtkCellArea" T NIL)
                        (CELL-AREA-CONTEXT CELL-VIEW-CELL-AREA-CONTEXT
                         "cell-area-context" "GtkCellAreaContext" T NIL)
                        (DRAW-SENSITIVE CELL-VIEW-DRAW-SENSITIVE
                         "draw-sensitive" "gboolean" T T)
                        (FIT-MODEL CELL-VIEW-FIT-MODEL
                         "fit-model" "gboolean" T T)
                        (MODEL CELL-VIEW-MODEL "model" "GtkTreeModel" T T)))
             (gobject:get-gtype-definition "GtkCellView"))))

;;; --- Properties -------------------------------------------------------------

;;;              gchar*   background           Write
;;;           GdkColor*   background-gdk       Read / Write
;;;            GdkRGBA*   background-rgba      Read / Write
;;;           gboolean    background-set       Read / Write
;;;        GtkCellArea*   cell-area            Read / Write / Construct Only
;;; GtkCellAreaContext*   cell-area-context    Read / Write / Construct Only
;;;           gboolean    draw-sensitive       Read / Write
;;;           gboolean    fit-model            Read / Write
;;;       GtkTreeModel*   model                Read / Write

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_view_new
;;;     gtk_cell_view_new_with_context
;;;     gtk_cell_view_new_with_text
;;;     gtk_cell_view_new_with_markup
;;;     gtk_cell_view_new_with_pixbuf
;;;     gtk_cell_view_set_model                            Accessor
;;;     gtk_cell_view_get_model                            Accessor
;;;     gtk_cell_view_set_displayed_row
;;;     gtk_cell_view_get_displayed_row
;;;     gtk_cell_view_get_size_of_row
;;;     gtk_cell_view_set_background_color                 deprecated
;;;     gtk_cell_view_set_background_rgba                  Accessor
;;;     gtk_cell_view_set_draw_sensitive                   Accessor
;;;     gtk_cell_view_get_draw_sensitive                   Accessor
;;;     gtk_cell_view_set_fit_model                        Accessor
;;;     gtk_cell_view_get_fit_model                        Accessor

;;; 2024-9-22
