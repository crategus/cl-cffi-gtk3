(in-package :gtk-test)

(def-suite gtk-drawing-area :in gtk-suite)
(in-suite gtk-drawing-area)

;;; --- Types ------------------------------------------------------------------

;;;     GtkDrawingArea

(test gtk-drawing-area-class
  ;; Check type
  (is (g:type-is-object "GtkDrawingArea"))
  ;; Check registered name
  (is (eq 'gtk:drawing-area
          (glib:symbol-for-gtype "GtkDrawingArea")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDrawingArea")
          (g:gtype (cffi:foreign-funcall "gtk_drawing_area_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkDrawingArea")))
  ;; Check children
  (is (equal '("GtkColorPlane")
             (glib-test:list-children "GtkDrawingArea")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkDrawingArea")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkDrawingArea")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkDrawingArea")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkDrawingArea")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkDrawingArea")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDrawingArea" GTK:DRAWING-AREA
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_drawing_area_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkDrawingArea"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drawing_area_new

(test gtk-drawing-area-new
  (glib-test:with-check-memory (area)
    (is (typep (setf area (gtk:drawing-area-new)) 'gtk:drawing-area))))

;;; 2025-06-01
