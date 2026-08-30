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

(test gtk-cell-view-properties
  (glib-test:with-check-memory (view :strong 2)
    (is (typep (setf view (gtk:cell-view-new)) 'gtk:cell-view))
    ;; Property background is not readable
    (signals (error) (gtk:cell-view-background view))
    ;; Set a background color
    (is (string= "red" (setf (gtk:cell-view-background view) "red")))
    (is (gdk:color-equal (gdk:color-new :red 65535 :green 0 :blue 0)
                         (gtk:cell-view-background-gdk view)))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1.0 :green 0 :blue 0 :alpha 1.0)
                        (gtk:cell-view-background-rgba view)))
    (is-true (gtk:cell-view-background-set  view))
    (is (typep (gtk:cell-view-cell-area view) 'gtk:cell-area-box))
    (is (typep (gtk:cell-view-cell-area-context view) 'gtk:cell-area-context))
    (is-false (gtk:cell-view-draw-sensitive view))
    (is-false (gtk:cell-view-fit-model view))
    (is-false (gtk:cell-view-model view))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_view_new

(test gtk-cell-view-new
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:cell-view-new)) 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_context

(test gtk-cell-vew-new-with-context.1
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:cell-view-new-with-context nil nil)) 'gtk:cell-view))))

;; TODO: Why 3 references for AREA

(test gtk-cell-view-new-with-context.2
  (glib-test:with-check-memory (view (area 3) :strong 1)
    (is (typep (setf area (make-instance 'gtk:cell-area-box)) 'gtk:cell-area-box))
    (is (typep (setf view (gtk:cell-view-new-with-context area nil)) 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_text

(test gtk-cell-view-new-with-text
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:cell-view-new-with-text "text")) 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_markup

(test gtk-cell-view-new-with-markup
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:cell-view-new-with-markup "<b>text</b>")) 'gtk:cell-view))))

;;;     gtk_cell_view_new_with_pixbuf

(test gtk-cell-view-new-with-pixbuf
  (glib-test:with-check-memory (view (pixbuf 2) :strong 1)
    (is (typep (setf pixbuf
                     (gdk:pixbuf-new-from-file
                         (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
               'gdk:pixbuf))
    (is (typep (setf view (gtk:cell-view-new-with-pixbuf pixbuf)) 'gtk:cell-view))))

;;;     gtk_cell_view_set_displayed_row
;;;     gtk_cell_view_get_displayed_row
;;;     gtk_cell_view_get_size_of_row

;;;     gtk_cell_view_set_background_color                 deprecated

;;; 2026-07-08
