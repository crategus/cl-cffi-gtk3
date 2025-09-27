;;; ----------------------------------------------------------------------------
;;; gdk3.gl-context.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2016 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GdkGLContext
;;;
;;;     OpenGL context
;;;
;;; Types and Values
;;;
;;;     GdkGLContext
;;;     GdkGLError
;;;
;;; Accessors
;;;
;;;     gdk_gl_context_get_display
;;;     gdk_gl_context_get_window
;;;     gdk_gl_context_get_shared_context
;;;
;;; Functions
;;;
;;;     gdk_gl_context_get_version
;;;     gdk_gl_context_set_required_version
;;;     gdk_gl_context_get_required_version
;;;     gdk_gl_context_set_debug_enabled
;;;     gdk_gl_context_get_debug_enabled
;;;     gdk_gl_context_set_forward_compatible
;;;     gdk_gl_context_get_forward_compatible
;;;     gdk_gl_context_set_use_es
;;;     gdk_gl_context_get_use_es
;;;     gdk_gl_context_is_legacy
;;;     gdk_gl_context_realize
;;;     gdk_gl_context_make_current
;;;     gdk_gl_context_get_current
;;;     gdk_gl_context_clear_current
;;;
;;; Properties
;;;
;;;     display
;;;     shared-context
;;;     window
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;      ╰── GdkGLContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkGLError
;;;
;;; Error enumeration for GdkGLContext.
;;;
;;; Members
;;;
;;; GDK_GL_ERROR_NOT_AVAILABLE
;;;     OpenGL support is not available
;;;
;;; GDK_GL_ERROR_UNSUPPORTED_FORMAT
;;;     The requested visual format is not supported
;;;
;;; GDK_GL_ERROR_UNSUPPORTED_PROFILE
;;;     The requested profile is not supported
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkGLContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkGLContext" gl-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_gl_context_get_type")
  ((display
    gl-context-display
    "display" "GdkDisplay" t t)
   (shared-context
    gl-context-shared-context
    "shared-context" "GdkGLContext" t t)
   (window
    gl-context-window
    "window" "GdkWindow" t t)))

#+liber-documentation
(setf (documentation 'gl-context 'type)
 "@version{2024-06-26}
  @begin{short}
    The @class{gdk:gl-context} object is representing the platform-specific
    OpenGL drawing context.
  @end{short}
  The @class{gdk:gl-context} object is created for a @class{gdk:window} object
  using the @fun{gdk:window-create-gl-context} function, and the context will
  match the @class{gdk:visual} object of the window.

  The @class{gdk:gl-context} object is not tied to any particular normal
  framebuffer. For instance, it cannot draw to the @class{gdk:window} back
  buffer. The GDK repaint system is in full control of the painting to that.
  Instead, you can create render buffers or textures and use the
  @fun{gdk:cairo-draw-from-gl} function in the draw function of your widget to
  draw them. Then GDK will handle the integration of your rendering with that
  of other widgets.

  Support for the @class{gdk:gl-context} object is platform-specific, context
  creation can fail, returning a @code{NULL} context.

  The @class{gdk:gl-context} object has to be made \"current\" in order to start
  using it, otherwise any OpenGL call will be ignored.

  @subheading{Creating a new OpenGL context}
  In order to create a new @class{gdk:gl-context} instance you need a
  @class{gdk:window} object, which you typically get during the realize call
  of a widget.

  The @class{gdk:gl-context} object is not realized until either the
  @fun{gdk:gl-context-make-current} function, or until it is realized using the
  @fun{gdk:gl-context-realize} function. It is possible to specify details of
  the GL context like the OpenGL version to be used, or whether the GL context
  should have extra state validation enabled after calling the
  @fun{gdk:window-create-gl-context} function by calling the
  @fun{gdk:gl-context-realize} function. If the realization fails you have the
  option to change the settings of the @class{gdk:gl-context} object and try
  again.

  @subheading{Using a OpenGl context}
  You will need to make the @class{gdk:gl-context} object the current context
  before issuing OpenGL calls. The system sends OpenGL commands to whichever
  context is current. It is possible to have multiple contexts, so you always
  need to ensure that the one which you want to draw with is the current one
  before issuing commands:
    @begin{pre}
gdk_gl_context_make_current (context);
    @end{pre}
    You can now perform your drawing using OpenGL commands.

    You can check which @class{gdk:gl-context} object is the current one by
    using the @fun{gdk:gl-context-current} function. You can also unset any
    @class{gdk:gl-context} object that is currently set by calling the
    @fun{gdk:gl-context-clear-current} function.
  @see-slot{gdk:gl-context-display}
  @see-slot{gdk:gl-context-shared-context}
  @see-slot{gdk:gl-context-window}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:gl-context-display -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'gl-context) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct) @br{}
  The display used to create the OpenGL drawing context.")

#+liber-documentation
(setf (liber:alias-for-function 'gl-context-display)
      "Accessor"
      (documentation 'gl-context-display 'function)
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-display object) => display}
  @argument[object]{a @class{gdk:gl-context} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:gl-context]{display} slot of the
    @class{gdk:gl-context} class.
  @end{short}
  The @fun{gdk:gl-context-display} function retrieves the display the OpenGL
  drawing context is created for.
  @see-class{gdk:gl-context}
  @see-class{gdk:display}")

;;; --- gdk:gl-context-shared-context ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shared-context" 'gl-context) t)
 "The @code{shared-context} property of type @class{gdk:gl-context}
  (Read / Write / Construct) @br{}
  The OpenGL drawing context that this context is sharing data with, or
  @code{nil}.")

#+liber-documentation
(setf (liber:alias-for-function 'gl-context-shared-context)
      "Accessor"
      (documentation 'gl-context-shared-context 'function)
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-shared-context object) => shared-context}
  @argument[object]{a @class{gdk:gl-context} object}
  @argument[shared-context]{a @class{gdk:gl-context} object}
  @begin{short}
    Accessor of the @slot[gdk:gl-context]{shared-context} slot of the
    @class{gdk:gl-context} class.
  @end{short}
  The @fun{gdk:gl-context-shared-context} function retrieves the OpenGL drawing
  context that this context share data with.
  @see-class{gdk:gl-context}")

;;; --- gdk:gl-context-window --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "window" 'gl-context) t)
 "The @code{window} property of type @class{gdk:window}
  (Read / Write / Construct) @br{}
  The window the OpenGL drawing context is bound to.")

#+liber-documentation
(setf (liber:alias-for-function 'gl-context-window)
      "Accessor"
      (documentation 'gl-context-window 'function)
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-window object) => window}
  @argument[object]{a @class{gdk:gl-context} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Accessor of the @slot[gdk:gl-context]{window} slot of the
    @class{gdk:gl-context} class.
  @end{short}
  The @fun{gdk:gl-context-window} function retrieves the window used by the
  OpenGL drawing context.
  @see-class{gdk:gl-context}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_get_version" %gl-context-version) :void
  (context (g:object gl-context))
  (major (:pointer :int))
  (minor (:pointer :int)))

(defun gl-context-version (context)
 #+liber-documentation
 "@version{#2024-06-26}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{return}
    @arg{major} - an integer with the major version @br{}
    @arg{minor} - an integer with the minor version
  @end{return}
  @begin{short}
    Retrieves the OpenGL version of the GL context.
  @end{short}
  The GL context must be realized prior to calling this function.
  @see-class{gdk:gl-context}"
  (cffi:with-foreign-objects ((major :int) (minor :int))
    (%gl-context-version context major minor)
    (values (cffi:mem-ref major :int)
            (cffi:mem-ref minor :int))))

(export 'gl-context-version)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_required_version
;;; gdk_gl_context_get_required_version
;;; ----------------------------------------------------------------------------

(defun (setf gl-context-required-version) (value context)
  (destructuring-bind (major minor) value
    (cffi:foreign-funcall "gdk_gl_context_set_required_version"
                          (g:object gl-context) context
                          :int major
                          :int minor
                          :void)
    (values major minor)))

(cffi:defcfun ("gdk_gl_context_get_required_version"
               %gl-context-required-version) :void
  (context (g:object gl-context))
  (major (:pointer :int))
  (minor (:pointer :int)))

(defun gl-context-required-version (context)
 #+liber-documentation
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-required-version object) => major, minor}
  @syntax{(setf gdk:gl-context-required-version object) (list major minor))}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{return}
    @arg{major} - an integer with the major version to request @br{}
    @arg{minor} - an integer with the minor version to request
  @end{return}
  @begin{short}
    The @fun{gdk:gl-context-required-version} function retrieves the major and
    minor version of OpenGL to request.
  @end{short}
  The @setf{gdk:gl-context-required-version} function sets the major and minor
  version to request. Setting @arg{major} and @arg{minor} to zero will use the
  default values.

  The @class{gdk:gl-context} object must not be realized or made current prior
  to calling this function.
  @see-class{gdk:gl-context}"
  (cffi:with-foreign-objects ((major :int) (minor :int))
    (%gl-context-version context major minor)
    (values (cffi:mem-ref major :int)
            (cffi:mem-ref minor :int))))

(export 'gl-context-required-version)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_debug_enabled
;;; gdk_gl_context_get_debug_enabled
;;; ----------------------------------------------------------------------------

(defun (setf gl-context-debug-enabled) (enabled context)
  (cffi:foreign-funcall "gdk_gl_context_set_debug_enabled"
                        (g:object gl-context) context
                        :boolean enabled
                        :void)
  enabled)

(cffi:defcfun ("gdk_gl_context_get_debug_enabled"
               gl-context-debug-enabled) :boolean
 #+liber-documentation
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-debug-enabled object) => enabled}
  @syntax{(setf gdk:gl-context-debug-enabled object) enabled)}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[enabled]{a boolean whether debugging is enabled}
  @begin{short}
    The @fun{gdk:gl-context-debug-enabled} function retrieves whether debugging
    is enabled.
  @end{short}
  The @setf{gdk:gl-context-debug-enabled} function sets whether the GL context
  should perform extra validations and run time checking. This is useful during
  development, but has additional overhead.

  The @class{gdk:gl-context} object must not be realized or made current prior
  to calling this function.
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-debug-enabled)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_forward_compatible
;;; gdk_gl_context_get_forward_compatible
;;; ----------------------------------------------------------------------------

(defun (setf gl-context-forward-compatible) (setting context)
  (cffi:foreign-funcall "gdk_gl_context_set_forward_compatible"
                        (g:object gl-context) context
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("gdk_gl_context_get_forward_compatible"
               gl-context-forward-compatible) :boolean
 #+liber-documentation
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-forward-compatible object) => setting}
  @syntax{(setf gdk:gl-context-forward-compatible object) setting)}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[setting]{a boolean whether @arg{context} is forward compatible}
  @begin{short}
    The @fun{gdk:gl-context-forward-compatible} function returns whether the
    GL context should be forward compatible.
  @end{short}
  The @setf{gdk:gl-context-forward-compatible} function sets whether the GL
  context should be forward compatible.

  Forward compatible GL contexts must not support OpenGL functionality that has
  been marked as deprecated in the requested version. Non-forward compatible
  GL contexts, on the other hand, must support both deprecated and non
  deprecated functionality.

  The @class{gdk:gl-context} object must not be realized or made current prior
  to calling this function.
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-forward-compatible)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_set_use_es
;;; gdk_gl_context_get_use_es
;;; ----------------------------------------------------------------------------

;; TODO: The return value is of type boolean, but an integer when setting.

(defun (setf gl-context-use-es) (value context)
  (cffi:foreign-funcall "gdk_gl_context_set_uses-es"
                        (g:object gl-context) context
                        :int value)
  value)

(cffi:defcfun ("gdk_gl_context_get_use_es" gl-context-use-es) :boolean
 #+liber-documentation
 "@version{#2024-06-26}
  @syntax{(gdk:gl-context-uses-es object) => setting}
  @syntax{(setf gdk:gl-context-use-es object) setting)}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[setting]{an integer whether the context uses OpenGL instead of
    OpenGL, or -1 to allow auto-detection}
  @begin{short}
    The @fun{gdk:gl-context-use-es} function checks whether the context is
    using an OpenGL or OpenGL ES profile.
  @end{short}
  The @fun{gdk:gl-context-use-es} function requests that GDK create an OpenGL
  ES context instead of an OpenGL one, if the platform and windowing system
  allows it. The context must not have been realized.

  By default, GDK will attempt to automatically detect whether the underlying
  GL implementation is OpenGL or OpenGL ES once the context is realized.

  You should check the return value of the @fun{gdk:gl-context-use-es} function
  after calling the @fun{gdk:gl-context-realize} function to decide whether to
  use the OpenGL or OpenGL ES API, extensions, or shaders.
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-realize}"
  (context (g:object gl-context)))

(export 'gl-context-use-es)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_is_legacy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_is_legacy" gl-context-is-legacy) :boolean
 #+liber-documentation
 "@version{#2024-06-26}
  @argument[context]{a @class{gdk:gl-context} object}
  @return{@em{True} if the GL context is in legacy mode.}
  @begin{short}
    Whether the GL context is in legacy mode or not.
  @end{short}
  The @class{gdk:gl-context} object must be realized before calling this
  function.

  When realizing a GL context, GDK will try to use the OpenGL 3.2 core profile.
  This profile removes all the OpenGL API that was deprecated prior to the 3.2
  version of the specification. If the realization is successful, this function
  will return @em{false}.

  If the underlying OpenGL implementation does not support core profiles, GDK
  will fall back to a pre-3.2 compatibility profile, and this function will
  return @em{true}.

  You can use the value returned by this function to decide which kind of
  OpenGL API to use, or whether to do extension discovery, or what kind of
  shader programs to load.
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-is-legacy)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_realize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_realize" %gl-context-realize) :boolean
  (context (g:object gl-context))
  (err :pointer))

(defun gl-context-realize (context)
 #+liber-documentation
 "@version{#2024-11-20}
  @argument[context]{a @class{gdk:gl-context} object}
  @return{@em{True} if the GL context is realized.}
  @begin{short}
    Realizes the given GL context.
  @end{short}
  It is safe to call this function on a realized GL context.
  @see-class{gdk:gl-context}"
  (glib:with-error (err)
    (%gl-context-realize context err)))

(export 'gl-context-realize)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_make_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_make_current" gl-context-make-current) :void
 #+liber-documentation
 "@version{#2024-06-26}
  @argument[context]{a @class{gdk:gl-context} object}
  @begin{short}
    Makes the GL context the current one.
  @end{short}
  @see-class{gdk:gl-context}"
  (context (g:object gl-context)))

(export 'gl-context-make-current)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_get_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_get_current" gl-context-current)
    (g:object gl-context)
 #+liber-documentation
 "@version{#2024-06-26}
  @return{The current @class{gdk:gl-context} object, or @code{nil}.}
  @begin{short}
    Retrieves the current GL context.
  @end{short}
  @see-class{gdk:gl-context}")

(export 'gl-context-current)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_context_clear_current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_context_clear_current" gl-context-clear-current) :void
 #+liber-documentation
 "@version{#2024-06-26}
  @begin{short}
    Clears the current GL context.
  @end{short}
  Any OpenGL call after this function returns will be ignored until the
  @fun{gdk:gl-context-make-current} function is called.
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-make-current}")

(export 'gl-context-clear-current)

;;; --- End of file gdk3.gl-context.lisp ---------------------------------------
