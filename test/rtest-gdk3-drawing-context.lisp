(in-package :gtk-test)

(def-suite gdk-drawing-context :in gdk-suite)
(in-suite gdk-drawing-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDrawingContext

(test drawing-context-class
  ;; Type check
  (is (g:type-is-object "GdkDrawingContext"))
  ;; Check the registered name
  (is (eq 'gdk:drawing-context
          (glib:symbol-for-gtype "GdkDrawingContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDrawingContext")
          (g:gtype (cffi:foreign-funcall "gdk_drawing_context_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDrawingContext")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkDrawingContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDrawingContext")))
  ;; Check the class properties
  (is (equal '("clip" "window")
             (list-properties "GdkDrawingContext")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkDrawingContext")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDrawingContext" GDK-DRAWING-CONTEXT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_drawing_context_get_type")
                       ((CLIP GDK-DRAWING-CONTEXT-CLIP "clip" "CairoRegion" T
                         NIL)
                        (WINDOW GDK-DRAWING-CONTEXT-WINDOW "window" "GdkWindow"
                         T NIL)))
             (gobject:get-g-type-definition "GdkDrawingContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     clip
;;;     window

;;; --- Functions --------------------------------------------------------------

;;;     gdk_drawing_context_get_window                     Accessor
;;;     gdk_drawing_context_get_clip                       Accessor
;;;     gdk_drawing_context_get_cairo_context
;;;     gdk_drawing_context_is_valid

;;; --- 2023-5-29 --------------------------------------------------------------
