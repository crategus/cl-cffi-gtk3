;;; ----------------------------------------------------------------------------
;;; gdk3.cairo.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; Cairo Interaction
;;;
;;;     Functions to support using Cairo
;;;
;;; Types and Values
;;;
;;;     CairoContext
;;;     CairoSurface
;;;
;;; Functions
;;;
;;;     gdk_window_create_similar_surface
;;;     gdk_window_create_similar_image_surface
;;;     gdk_cairo_create
;;;     gdk_cairo_get_clip_rectangle
;;;     gdk_cairo_get_drawing_context
;;;     gdk_cairo_set_source_color
;;;     gdk_cairo_set_source_rgba
;;;     gdk_cairo_set_source_pixbuf
;;;     gdk_cairo_set_source_window
;;;     gdk_cairo_rectangle
;;;     gdk_cairo_region
;;;     gdk_cairo_region_create_from_surface
;;;     gdk_cairo_surface_create_from_pixbuf
;;;     gdk_cairo_draw_from_gl
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------

;;; CairoSurface represents a cairo:surface-t, but we need a boxed type in GTK

(define-g-boxed-opaque cairo-surface "CairoSurface"
  :alloc (error "CairoSurface cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'cairo-surface)
      "GBoxed"
      (documentation 'cairo-surface 'type)
 "@version{2023-2-3}
  @begin{short}
    The @sym{gdk:cairo-surface} structure represents a Cairo surface in GTK.
  @end{short}
  See the documentation of the @symbol{cairo:surface-t} structure for more
  information.
  @begin{pre}
(define-g-boxed-opaque cairo-surface \"CairoSurface\"
  :alloc (error \"CairoSurface cannot be created from the Lisp side.\"))
  @end{pre}
  @see-symbol{cairo:surface-t}")

(export 'cairo-surface)

;;; CairoContext represents a cairo:context-t, but we need a boxed type in GTK

(define-g-boxed-opaque cairo-context "CairoContext"
  :alloc (error "CairoContext cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'cairo-context)
      "GBoxed"
      (documentation 'cairo-context 'type)
 "@version{2023-2-3}
  @begin{short}
    The @sym{gdk:cairo-context} structure represents a Cairo context in GTK.
  @end{short}
  See the documentation of the @symbol{cairo:context-t} structure for more
  information.
  @begin{pre}
(define-g-boxed-opaque cairo-context \"CairoContext\"
  :alloc (error \"CairoContext cannot be created from the Lisp side.\"))
  @end{pre}
  @see-symbol{cairo:context-t}")

(export 'cairo-context)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_similar_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_create_similar_surface" window-create-similar-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{2023-2-3}
  @argument[window]{a @class{gdk:window} object to make the new surface similar
    to}
  @argument[content]{a value of the @symbol{cairo:content-t} enumeration for
    the content of the new surface}
  @argument[width]{an integer with the width of the new surface}
  @argument[height]{an integer with the height of the new surface}
  @begin{return}
    A newly allocated @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return
    a \"nil\" surface if the surface is in an error state.
  @end{return}
  @begin{short}
    Creates a new surface that is as compatible as possible with the given
    @arg{window}.
  @end{short}
  For example the new surface will have the same fallback resolution and font
  options as @arg{window}. Generally, the new surface will also use the same
  backend as @arg{window}, unless that is not possible for some reason. The
  type of the returned surface may be examined with the
  @fun{cairo:surface-type} function.

  Initially the surface contents are all 0, transparent if contents have
  transparency, black otherwise.
  @see-class{gdk:window}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-type}"
  (window (g:object window))
  (content cairo:content-t)
  (width :int)
  (height :int))

(export 'window-create-similar-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_similar_image_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_create_similar_image_surface"
           window-create-similar-image-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{2023-2-3}
  @argument[window]{a @class{gdk:window} object to make the new surface similar
    to}
  @argument[format]{a value of the @symbol{cairo:format-t} enumeration for the
    format of the new surface}
  @argument[width]{an integer with the width of the new surface}
  @argument[height]{an integer with the height of the new surface}
  @argument[scale]{an integer with the scale of the new surface, or 0 to use
    same as @arg{window}}
  @begin{return}
    A newly allocated @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return a
    \"nil\" surface if the surface is in an error state or any other error
    occurs.
  @end{return}
  @begin{short}
    Create a new image surface that is efficient to draw on the given
    @arg{window}.
  @end{short}
  Initially the surface contents are all 0, transparent if contents have
  transparency, black otherwise.

  The width and height of the new surface are not affected by the scaling
  factor of the window, or by the scale argument. They are the size of the
  surface in device pixels. If you wish to create an image surface capable of
  holding the contents of the window you can use:
  @begin{pre}
(let* ((win (gtk:widget-window window))
       (scale (gdk:window-scale-factor win))
       (width (* scale (gdk:window-width win)))
       (height (* scale (gdk:window-height win)))
       (surface nil))
  ;; Create the image surface
  (setf surface
        (gdk:window-create-similar-image-surface win
                                                 :rgb24
                                                 width height
                                                 scale))
  ...
  (cairo:surface-destroy surface))
  @end{pre}
  Note that unlike the @fun{cairo:surface-create-similar-image} function, the
  new device scale of the surface is set to @arg{scale}, or to the scale factor
  of @arg{window} if the @arg{scale} argument is 0.
  @see-class{gdk:window}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:format-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-create-similar-image}"
  (window (g:object window))
  (format cairo:format-t)
  (width :int)
  (height :int)
  (scale :int))

(export 'window-create-similar-image-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_create" cairo-create) (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{2023-2-3}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    A newly created @symbol{cairo:context-t} context. Free with the
    @fun{cairo-destroy} function when you are done drawing.
  @end{return}
  @begin{short}
    Creates a Cairo context for drawing to @arg{window}.
  @end{short}
  Note that calling the @fun{cairo:reset-clip} function on the resulting
  @symbol{cairo:context-t} context will produce undefined results, so avoid it
  at all costs.

  Typically, this function is used to draw on a @class{gdk:window} object out
  of the paint cycle of the toolkit. This should be avoided, as it breaks
  various assumptions and optimizations.

  If you are drawing on a native @class{gdk:window} object in response to a
  @code{:expose} event you should use the @fun{gdk:window-begin-draw-frame} and
  @fun{gdk:drawing-context-cairo-context} functions instead. GTK will
  automatically do this for you when drawing a widget.
  @begin[Warning]{dictionary}
    The @sym{gdk:cairo-create} function has been deprecated since version 3.22
    and should not be used in newly written code. Use the
    @fun{gdk:window-begin-draw-frame} and
    @fun{gdk:drawing-context-cairo-context} functions instead.
  @end{dictionary}
  @see-class{gdk:window}
  @see-symbol{cairo:context-t}
  @see-function{cairo:destroy}
  @see-function{cairo:reset-clip}
  @see-function{gdk:window-begin-draw-frame}
  @see-function{gdk:drawing-context-cairo-context}"
  (window (g:object window)))

(export 'cairo-create)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_get_clip_rectangle () -> cairo-clip-rectangle
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_get_clip_rectangle" %cairo-clip-rectangle) :boolean
  (cr (:pointer (:struct cairo:context-t)))
  (rectangle (g:boxed rectangle)))

(defun cairo-clip-rectangle (cr)
 #+liber-documentation
 "@version{2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @begin{return}
    A @class{gdk:rectangle} instance with the clip or @em{false} if all of
    @arg{cr} is clipped and all drawing can be skipped.
  @end{return}
  @begin{short}
    This is a convenience function around the @fun{cairo:clip-extents} function.
  @end{short}
  It rounds the clip extents to integer coordinates and returns a boolean
  indicating if a clip area exists.
  @see-symbol{cairo:context-t}
  @see-class{gdk:rectangle}
  @see-function{cairo:clip-extents}"
  (let ((rect (rectangle-new)))
    (when (%cairo-clip-rectangle cr rect)
      rect)))

(export 'cairo-clip-rectangle)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_get_drawing_context ()
;;; ----------------------------------------------------------------------------

#+gtk-3-22
(defcfun ("gdk_cairo_get_drawing_context" cairo-drawing-context)
    (g:object drawing-context)
 #+liber-documentation
 "@version{2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @return{A @class{gdk:drawing-context} object, if any is set.}
  @begin{short}
    Retrieves the drawing context that created the Cairo context @arg{cr}.
  @end{short}

  Since 3.22
  @see-symbol{cairo:context-t}
  @see-class{gdk:drawing-context}"
  (cr (:pointer (:struct cairo:context-t))))

#+gtk-3-22
(export 'cairo-drawing-context)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_color ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_color" cairo-set-source-color) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[color]{a @class{gdk:color} color}
  @begin{short}
    Sets the specified color as the source color of the Cairo context.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk:cairo-set-source-color} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @fun{gdk:cairo-set-source-rgba} function instead.
  @end{dictionary}
  @see-symbol{cairo:context-t}
  @see-class{gdk:color}
  @see-function{gdk:cairo-set-source-rgba}"
  (cr (:pointer (:struct cairo:context-t)))
  (color (g:boxed color)))

(export 'cairo-set-source-color)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_rgba" cairo-set-source-rgba) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[rgba]{a @class{gdk:rgba} color}
  @begin{short}
    Sets the specified @arg{rgba} color as the source color of the Cairo
    context.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-class{gdk:rgba}"
  (cr (:pointer (:struct cairo:context-t)))
  (rgba (g:boxed rgba)))

(export 'cairo-set-source-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_pixbuf" %cairo-set-source-pixbuf) :void
  (cr (:pointer (:struct cairo:context-t)))
  (pixbuf (g:object gdk-pixbuf:pixbuf))
  (x :double)
  (y :double))

(defun cairo-set-source-pixbuf (cr pixbuf x y)
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[pixbuf]{a @class{gdk:pixbuf} object}
  @argument[x]{a number coerced to a double float x coordinate of location to
    place upper left corner of @arg{pixbuf}}
  @argument[y]{a number coerced to a double float y coordinate of location to
    place upper left corner of @arg{pixbuf}}
  @begin{short}
    Sets the given @arg{pixbuf} as the source pattern for @arg{cr}.
  @end{short}
  The pattern has a @code{:none} extend mode of the @symbol{cairo:extend-t}
  enumeration and is aligned so that the origin of the pixbuf is (@arg{x},
  @arg{y}).
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:extend-t}
  @see-class{gdk:pixbuf}"
  (%cairo-set-source-pixbuf cr
                            pixbuf
                            (coerce x 'double-float)
                            (coerce y 'double-float)))

(export 'cairo-set-source-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_set_source_window ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_set_source_window" %cairo-set-source-window) :void
  (cr (:pointer (:struct cairo:context-t)))
  (window (g:object window))
  (x :double)
  (y :double))

(defun cairo-set-source-window (cr window x y)
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[window]{a @class{gdk:window} object}
  @argument[x]{a number coerced to a double float x coordinate of location to
    place upper left corner of @arg{window}}
  @argument[y]{a number coerced to a double float y coordinate of location to
    place upper left corner of @arg{window}}
  @begin{short}
    Sets the given @arg{window} as the source pattern for @arg{cr}.
  @end{short}
  The pattern has a @code{:none} extend mode of the @symbol{cairo:extend-t}
  enumeration and is aligned so that the origin of @arg{window} is (@arg{x},
  @arg{y}). The window contains all its subwindows when rendering.

  Note that the contents of @arg{window} are undefined outside of the visible
  part of the window, so use this function with care.
  @see-class{gdk:window}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:extend-t}"
  (%cairo-set-source-window cr
                            window
                            (coerce x 'double-float)
                            (coerce y 'double-float)))

(export 'cairo-set-source-window)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_rectangle ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_rectangle" cairo-rectangle) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[rectangle]{a @class{gdk:rectangle} instance}
  @begin{short}
    Adds the given @arg{rectangle} to the current path of @arg{cr}.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-class{gdk:rectangle}"
  (cr (:pointer (:struct cairo:context-t)))
  (rectangle (g:boxed rectangle)))

(export 'cairo-rectangle)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_region" cairo-region) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @begin{short}
    Adds the given @arg{region} to the current path of @arg{cr}.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:region-t}"
  (cr (:pointer (:struct cairo:context-t)))
  (region (:pointer (:struct cairo:region-t))))

(export 'cairo-region)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_region_create_from_surface ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_region_create_from_surface"
           cairo-region-create-from-surface)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[surface]{a @symbol{cairo:surface-t} instance}
  @begin{return}
    A @symbol{cairo:region-t} instance, must be freed with the
    @fun{cairo:region-destroy} function.
  @end{return}
  @begin{short}
    Creates region that describes covers the area where the given surface is
    more than 50% opaque.
  @end{short}
  This function takes into account device offsets that might be set with
  the @fun{cairo:surface-set-device-offset} function.
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}
  @see-function{cairo:surface-set-device-offset}"
  (surface (:pointer (:struct cairo:surface-t))))

(export 'cairo-region-create-from-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_surface_create_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_surface_create_from_pixbuf"
           cairo-surface-create-from-pixbuf)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[pixbuf]{a @class{gdk:pixbuf} object}
  @argument[scale]{an integer with the scale of the new surface, or 0 to use
    same as @arg{window}}
  @argument[window]{a @class{gdk:window} object this will be drawn to, or
    @code{nil}}
  @begin{return}
    A new @symbol{cairo:surface-t} instance, must be freed with the
    @fun{cairo:surface-destroy} function.
  @end{return}
  @begin{short}
    Creates an image surface with the same contents as the pixbuf.
  @end{short}
  @see-class{gdk:pixbuf}
  @see-class{gdk:window}
  @see-symbol{cairo:surface-t}
  @see-function{cairo:surface-destroy}"
  (pixbuf (g:object gdk-pixbuf:pixbuf))
  (scale :int)
  (window (g:object window)))

(export 'cairo-surface-create-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_cairo_draw_from_gl ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cairo_draw_from_gl" cairo-draw-from-gl) :void
 #+liber-documentation
 "@version{#2023-2-3}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[window]{a @class{gdk:window} object we are rendering for,
    not necessarily into}
  @argument[source]{an integer with the GL ID of the source buffer}
  @argument[type]{an integer with the type of the source}
  @argument[scale]{an integer with the scale factor that the source buffer is
    allocated for}
  @argument[x]{an integer with the source x position in source to start copying
    from in GL coordinates}
  @argument[y]{an integer with the source y position in source to start copying
    from in GL coordinates}
  @argument[width]{an integer with the width of the region to draw}
  @argument[height]{an integer with the height of the region to draw}
  @begin{short}
    This is the main way to draw GL content in GTK.
  @end{short}
  It takes a render buffer ID (@arg{type} == @code{GL_RENDERBUFFER}) or a
  texture ID (@arg{type} == @code{GL_TEXTURE}) and draws it onto @arg{cr} with
  an @code{OVER} operation, respecting the current clip. The top left corner of
  the rectangle specified by @arg{x}, @arg{y}, @arg{width} and @arg{height}
  will be drawn at the current (0,0) position of the Cairo context.

  This will work for all Cairo contexts, as long as @arg{window} is realized,
  but the fallback implementation that reads back the pixels from the buffer
  may be used in the general case. In the case of direct drawing to a window
  with no special effects applied to @arg{cr} it will however use a more
  efficient approach.

  For @code{GL_RENDERBUFFER} the code will always fall back to software for
  buffers with alpha components, so make sure you use @code{GL_TEXTURE} if
  using alpha.

  Calling this may change the current GL context.
  @see-symbol{cairo:context-t}
  @see-class{gdk:window}"
  (cr (:pointer (:struct cairo:context-t)))
  (window (g:object window))
  (source :int)
  (type :int)
  (scale :int)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'cairo-draw-from-gl)

;;; --- End of file gdk3.cairo.lisp --------------------------------------------
