(in-package :gtk-test)

(def-suite gtk-cell-renderer-pixbuf :in gtk-suite)
(in-suite gtk-cell-renderer-pixbuf)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererPixbuf

(test gtk-cell-renderer-pixbuf-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererPixbuf"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-pixbuf
          (glib:symbol-for-gtype "GtkCellRendererPixbuf")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererPixbuf")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_pixbuf_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererPixbuf")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCellRendererPixbuf")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererPixbuf")))
  ;; Check the class properties
  (is (equal '("follow-state" "gicon" "icon-name" "pixbuf"
               "pixbuf-expander-closed" "pixbuf-expander-open" "stock-detail"
               "stock-id" "stock-size" "surface")
             (list-properties "GtkCellRendererPixbuf")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkCellRendererPixbuf")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererPixbuf"
                                             GTK-CELL-RENDERER-PIXBUF
                               (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_pixbuf_get_type")
                               ((FOLLOW-STATE
                                 GTK-CELL-RENDERER-PIXBUF-FOLLOW-STATE
                                 "follow-state" "gboolean" T T)
                                (GICON GTK-CELL-RENDERER-PIXBUF-GICON "gicon"
                                 "GIcon" T T)
                                (ICON-NAME GTK-CELL-RENDERER-PIXBUF-ICON-NAME
                                 "icon-name" "gchararray" T T)
                                (PIXBUF GTK-CELL-RENDERER-PIXBUF-PIXBUF
                                 "pixbuf" "GdkPixbuf" T T)
                                (PIXBUF-EXPANDER-CLOSED
                                 GTK-CELL-RENDERER-PIXBUF-PIXBUF-EXPANDER-CLOSED
                                 "pixbuf-expander-closed" "GdkPixbuf" T T)
                                (PIXBUF-EXPANDER-OPEN
                                 GTK-CELL-RENDERER-PIXBUF-PIXBUF-EXPANDER-OPEN
                                 "pixbuf-expander-open" "GdkPixbuf" T T)
                                (STOCK-DETAIL
                                 GTK-CELL-RENDERER-PIXBUF-STOCK-DETAIL
                                 "stock-detail" "gchararray" T T)
                                (STOCK-ID GTK-CELL-RENDERER-PIXBUF-STOCK-ID
                                 "stock-id" "gchararray" T T)
                                (STOCK-SIZE GTK-CELL-RENDERER-PIXBUF-STOCK-SIZE
                                 "stock-size" "guint" T T)
                                (SURFACE GTK-CELL-RENDERER-PIXBUF-SURFACE
                                 "surface" "CairoSurface" T T)))
             (gobject:get-g-type-definition "GtkCellRendererPixbuf"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-pixbuf-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-pixbuf)))
    (is-true (gtk:cell-renderer-pixbuf-follow-state renderer))
    (is-false (gtk:cell-renderer-pixbuf-gicon renderer))
    (is-false (gtk:cell-renderer-pixbuf-icon-name renderer))
    (is-false (gtk:cell-renderer-pixbuf-pixbuf renderer))
    (is-false (gtk:cell-renderer-pixbuf-pixbuf-expander-closed renderer))
    (is-false (gtk:cell-renderer-pixbuf-pixbuf-expander-open renderer))
    (is-false (gtk:cell-renderer-pixbuf-stock-detail renderer))
    (is-false (gtk:cell-renderer-pixbuf-stock-id renderer))
    (is (= 1 (gtk:cell-renderer-pixbuf-stock-size renderer)))
    (is-false (gtk:cell-renderer-pixbuf-surface renderer))))

;;; ---  Functions--------------------------------------------------------------

;;;     gtk_cell_renderer_pixbuf_new

(test gtk-cell-renderer-pixbuf-new
  (is (typep (gtk:cell-renderer-pixbuf-new) 'gtk:cell-renderer-pixbuf)))

;;; 2024-3-17
