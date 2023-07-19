(in-package :gtk-test)

(def-suite gdk-gl-context :in gdk-suite)
(in-suite gdk-gl-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkGLError

;;;     GdkGLContext

(test gl-context-class
  ;; Type check
  (is (g:type-is-object "GdkGLContext"))
  ;; Check the registered name
  (is (eq 'gdk:gl-context
          (glib:symbol-for-gtype "GdkGLContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkGLContext")
          (g:gtype (cffi:foreign-funcall "gdk_gl_context_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkGLContext")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkGLContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkGLContext")))
  ;; Check the class properties
  (is (equal '("display" "shared-context" "window")
             (list-properties "GdkGLContext")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkGLContext")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkGLContext" GDK-G-L-CONTEXT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL)
                       ((DISPLAY GDK-G-L-CONTEXT-DISPLAY "display" "GdkDisplay"
                         T NIL)
                        (SHARED-CONTEXT GDK-G-L-CONTEXT-SHARED-CONTEXT
                         "shared-context" "GdkGLContext" T NIL)
                        (WINDOW GDK-G-L-CONTEXT-WINDOW "window" "GdkWindow" T
                         NIL)))
             (gobject:get-g-type-definition "GdkGLContext"))))

;;; --- Properties -------------------------------------------------------------

;;;     display
;;;     shared-context
;;;     window

;;; --- Functions --------------------------------------------------------------

;;;     gdk_gl_context_get_display ()                      Accessor
;;;     gdk_gl_context_get_window ()                       Accessor
;;;     gdk_gl_context_get_shared_context ()               Accessor
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

;;; --- 2023-5-29 --------------------------------------------------------------
