;;; ----------------------------------------------------------------------------
;;; gdk3.drawing-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GdkDrawingContext
;;;
;;;     Drawing context for GDK windows
;;;
;;; Types and Values
;;;
;;;     GdkDrawingContext
;;;
;;; Functions
;;;
;;;     gdk_drawing_context_get_window                     Accessor
;;;     gdk_drawing_context_get_clip                       Accessor
;;;     gdk_drawing_context_get_cairo_context
;;;     gdk_drawing_context_is_valid
;;;
;;; Properties
;;;
;;;     clip
;;;     window
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrawingContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; struct GdkDrawingContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkDrawingContext" drawing-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_drawing_context_get_type")
  ((clip
    drawing-context-clip
    "clip" "CairoRegion" t t)
   (window
    drawing-context-window
    "window" "GdkWindow" t t)))

#+liber-documentation
(setf (documentation 'drawing-context 'type)
 "@version{#2023-3-10}
  @begin{short}
    The @class{gdk:drawing-context} object is an object that represents the
    current drawing state of a @class{gdk:window} object.
  @end{short}
  It is possible to use a @class{gdk:drawing-context} object to draw on a
  @class{gdk:window} object via rendering API like Cairo or OpenGL.

  A @class{gdk:drawing-context} object can only be created by calling
  the @fun{gdk:window-begin-draw-frame} function and will be valid until a call
  to the @fun{gdk:window-end-draw-frame} function.

  The @class{gdk:drawing-context} class is available since GDK 3.22.
  @see-class{gdk:window}
  @see-function{gdk:window-begin-draw-frame}
  @see-function{gdk:window-end-draw-frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- drawing-context-clip ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "clip" 'drawing-context) t)
 "The @code{clip} property of type @symbol{cairo:region-t}
  (Read / Write / Construct Only) @br{}
  The clip region applied to the drawing context. Since 3.22")

#+liber-documentation
(setf (liber:alias-for-function 'drawing-context-clip)
      "Accessor"
      (documentation 'drawing-context-clip 'function)
 "@version{#2023-3-10}
  @syntax[]{(gdk:drawing-context-clip object) => region}
  @argument[object]{a @class{gdk:drawing-context} object}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @begin{short}
    Accessor of the @slot[gdk:drawing-context]{clip} slot of the
    @class{gdk:drawing-context} class.
  @end{short}
  The @fun{gdk:drawing-context-clip} function retrieves a copy of the clip
  region used when creating the context.

  Since 3.22
  @see-class{gdk:drawing-context}
  @see-symbol{cairo:region-t}")

;;; --- drawing-context-window -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "window" 'drawing-context) t)
 "The @code{window} property of type @class{gdk:window}
  (Read / Write / Construct Only) @br{}
  The window that created the drawing context. Since 3.22")

#+liber-documentation
(setf (liber:alias-for-function 'drawing-context-window)
      "Accessor"
      (documentation 'drawing-context-window 'function)
 "@version{#2023-3-10}
  @syntax[]{(gdk:drawing-context-window object) => window}
  @argument[object]{a @class{gdk:drawing-context} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Accessor of the @slot[gdk:drawing-context]{window} slot of the
    @class{gdk:drawing-context} class.
  @end{short}
  The @fun{gdk:drawing-context-window} function retrieves the window that
  created the drawing context.

  Since 3.22
  @see-class{gdk:drawing-context}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; gdk_drawing_context_get_cairo_context () -> drawing-context-cairo-context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drawing_context_get_cairo_context" drawing-context-cairo-context)
    (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{2023-3-10}
  @argument[object]{a @class{gdk:drawing-context} object}
  @begin{return}
    A @symbol{cairo:context-t} Cairo context to be used to draw the contents of
    the @class{gdk:window} object.
  @end{return}
  @begin{short}
    Retrieves a Cairo context to be used to draw on the window that created the
    drawing context.
  @end{short}
  The returned context is guaranteed to be valid as long as the
  @class{gdk:drawing-context} object is valid, that is between a call to
  the @fun{gdk:window-begin-draw-frame} and @fun{gdk:window-end-draw-frame}
  functions.

  Since 3.22
  @see-class{gdk:drawing-context}
  @see-class{gdk:window}
  @see-symbol{cairo:context-t}
  @see-function{gdk:window-begin-draw-frame}
  @see-function{gdk:window-end-draw-frame}"
  (context (g:object drawing-context)))

(export 'drawing-context-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gdk_drawing_context_is_valid ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drawing_context_is_valid" drawing-context-is-valid) :boolean
 #+liber-documentation
 "@version{#2023-3-10}
  @argument[context]{a @class{gdk:drawing-context} object}
  @return{@em{True} if the drawing context is valid.}
  @begin{short}
    Checks whether the given drawing context is valid.
  @end{short}

  Since 3.22
  @see-class{gdk:drawing-context}"
  (context (g:object drawing-context)))

(export 'drawing-context-is-valid)

;;; --- End of file gdk3.drawing-context.lisp ----------------------------------
