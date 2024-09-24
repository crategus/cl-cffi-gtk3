(in-package :gtk-test)

(def-suite gdk-gl-context :in gdk-suite)
(in-suite gdk-gl-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkGLError

;;;     GdkGLContext

(test gdk-gl-context-class
  ;; Check type
  (is (g:type-is-object "GdkGLContext"))
  ;; Check registered name
  (is (eq 'gdk:gl-context
          (glib:symbol-for-gtype "GdkGLContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkGLContext")
          (g:gtype (cffi:foreign-funcall "gdk_gl_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkGLContext")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkGLContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkGLContext")))
  ;; Check class properties
  (is (equal '("display" "shared-context" "window")
             (glib-test:list-properties "GdkGLContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkGLContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkGLContext" GDK:GL-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL)
                       ((DISPLAY GL-CONTEXT-DISPLAY
                         "display" "GdkDisplay" T NIL)
                        (SHARED-CONTEXT GL-CONTEXT-SHARED-CONTEXT
                         "shared-context" "GdkGLContext" T NIL)
                        (WINDOW GL-CONTEXT-WINDOW "window" "GdkWindow" T NIL)))
             (gobject:get-gtype-definition "GdkGLContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     display
;;;     shared-context
;;;     window

;;; --- Functions --------------------------------------------------------------

;;;     gdk_gl_context_get_version
;;;     gdk_gl_context_set_required_version
;;;     gdk_gl_context_get_required_version
;;;     gdk_gl_context_set_debug_enabled
;;;     gdk_gl_context_get_debug_enabled
;;;     gdk_gl_context_set_forward_compatible
;;;     gdk_gl_context_get_forward_compatible
;;;     gdk_gl_context_set_use_es ()
;;;     gdk_gl_context_get_use_es ()
;;;     gdk_gl_context_is_legacy ()
;;;     gdk_gl_context_realize
;;;     gdk_gl_context_make_current
;;;     gdk_gl_context_get_current
;;;     gdk_gl_context_clear_current

;;; 2024-9-21
