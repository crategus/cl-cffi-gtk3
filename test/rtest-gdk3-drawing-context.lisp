(in-package :gtk-test)

(def-suite gdk-drawing-context :in gdk-suite)
(in-suite gdk-drawing-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDrawingContext

(test gdk-drawing-context-class
  ;; Check type
  (is (g:type-is-object "GdkDrawingContext"))
  ;; Check registered name
  (is (eq 'gdk:drawing-context
          (glib:symbol-for-gtype "GdkDrawingContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDrawingContext")
          (g:gtype (cffi:foreign-funcall "gdk_drawing_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDrawingContext")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDrawingContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDrawingContext")))
  ;; Check class properties
  (is (equal '("clip" "window")
             (glib-test:list-properties "GdkDrawingContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkDrawingContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDrawingContext" GDK:DRAWING-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_drawing_context_get_type")
                       ((CLIP DRAWING-CONTEXT-CLIP "clip" "CairoRegion" T NIL)
                        (WINDOW DRAWING-CONTEXT-WINDOW
                         "window" "GdkWindow" T NIL)))
             (gobject:get-gtype-definition "GdkDrawingContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     clip
;;;     window

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drawing_context_get_window                     Accessor
;;;     gdk_drawing_context_get_clip                       Accessor
;;;     gdk_drawing_context_get_cairo_context
;;;     gdk_drawing_context_is_valid

;;; 2024-9-22
