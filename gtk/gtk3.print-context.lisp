;;; ----------------------------------------------------------------------------
;;; gtk3.print-context.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkPrintContext
;;;
;;;     Encapsulates context for drawing pages
;;;
;;; Types and Values
;;;
;;;     GtkPrintContext
;;;
;;; Functions
;;;
;;;     gtk_print_context_get_cairo_context
;;;     gtk_print_context_set_cairo_context
;;;     gtk_print_context_get_page_setup
;;;     gtk_print_context_get_width
;;;     gtk_print_context_get_height
;;;     gtk_print_context_get_dpi_x
;;;     gtk_print_context_get_dpi_y
;;;     gtk_print_context_get_pango_fontmap
;;;     gtk_print_context_create_pango_context
;;;     gtk_print_context_create_pango_layout
;;;     gtk_print_context_get_hard_margins
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPrintContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPrintContext" print-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'print-context 'type)
 "@version{2023-2-11}
  @begin{short}
    The @class{gtk:print-context} object encapsulates context information that
    is required when drawing pages for printing, such as the Cairo context and
    important parameters like page size and resolution.
  @end{short}
  It also lets you easily create a @class{pango:layout} object and
  @class{pango:context} objects that match the font metrics of the Cairo
  surface.

  The @class{gtk:print-context} object gets passed to the
  @code{\"begin-print\"}, @code{\"end-print\"}, @code{\"request-page-setup\"}
  and @code{\"draw-page\"} signals on the print operation.
  @begin[Examples]{dictionary}
    Using the @class{gtk:print-context} object in a \"draw-page\" callback.
    @begin{pre}
(defun draw-page (operation context page-nr)
  (declare (ignore operation page-nr))
  (let ((cr (gtk:print-context-get-cairo-context context))
        (layout (gtk:print-context-create-pango-layout context)))

    ;; Draw a red rectangle, as wide as the paper (inside the margins)
    (cairo-set-source-rgb cr 1.0 0 0)
    (cairo-rectangle cr 0 0 (gtk:print-context-width context) 50)
    (cairo-fill cr)

    ;; Draw some lines
    (cairo-move-to cr 20 10)
    (cairo-line-to cr 40 20)
    (cairo-arc cr 60 60 20 0 3.14)
    (cairo-line-to cr 80 20)

    (cairo-set-source-rgb cr 0 0 0)
    (cairo-set-line-width cr 5)
    (cairo-set-line-cap cr :round)
    (cairo-set-line-join cr :round)

    (cairo-stroke cr)

    ;; Draw some text
    (setf (pango:layout-text layout) \"Hello World! Printing is easy\")
    (setf (pango:layout-font-description layout)
          (pango:font-description-from-string \"sans 28\"))
    (cairo-move-to cr 30 20)
    (pango:cairo-layout-path cr layout)

    ;; Font Outline
    (cairo-set-source-rgb cr 0.93 1.0 0.47)
    (cairo-set-line-width cr 0.5)
    (cairo-stroke-preserve cr)

    ;; Font Fill
    (cairo-set-source-rgb cr 0 0.0 1.0)
    (cairo-fill cr)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-operation}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_cairo_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_cairo_context"
               print-context-cairo-context) (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The @sym{cairo:context-t} Cairo context for @arg{context}.}
  @begin{short}
    Obtains the Cairo context that is associated with the print text.
  @end{short}
  @see-class{gtk:print-context}
  @see-symbol{cairo:context-t}
  @see-function{gtk:print-context-set-cairo-context}"
  (context (g:object print-context)))

(export 'print-context-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_set_cairo_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_set_cairo_context"
               %print-context-set-cairo-context) :void
  (context (g:object print-context))
  (cr (:pointer (:struct cairo:context-t)))
  (xdpi :double)
  (ydpi :double))

(defun print-context-set-cairo-context (context cr xdpi ydpi)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[context]{a @class{gtk:print-context} object}
  @argument[cr]{a @sym{cairo:contex-t} Cairo context}
  @argument[xdpi]{a number coerced to a double float for the horizontal
    resolution to use with @arg{cr}}
  @argument[ydpi]{a number coerced to a double float for the vertical
    resolution to use with @arg{cr}}
  @begin{short}
    Sets a new Cairo context on a print context.
  @end{short}
  This function is intended to be used when implementing an internal print
  preview, it is not needed for printing, since GTK itself creates a suitable
  Cairo context in that case.
  @see-class{gtk:print-context}
  @see-symbol{cairo:context-t}
  @see-function{gtk:print-context-cairo-context}"
  (%print-context-set-cairo-context context
                                    cr
                                    (coerce xdpi 'double-float)
                                    (coerce ydpi 'double-float)))

(export 'print-context-set-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_page_setup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_page_setup" print-context-page-setup)
    (g:object page-setup)
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The @class{gtk:page-setup} object of the print context.}
  @begin{short}
    Obtains the page setup that determines the page dimensions of the print
    context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{gtk:page-setup}"
  (context (g:object print-context)))

(export 'print-context-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_width" print-context-width) :double
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the width of @arg{context}.}
  @begin{short}
    Obtains the width of the print context, in pixels.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-height}"
  (context (g:object print-context)))

(export 'print-context-width)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_height" print-context-height) :double
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the height of @arg{context}.}
  @begin{short}
    Obtains the height of the print context, in pixels.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-width}"
  (context (g:object print-context)))

(export 'print-context-height)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_dpi_x" print-context-dpi-x) :double
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the horizontal resolution of @arg{context}.}
  @begin{short}
    Obtains the horizontal resolution of the print context, in dots per inch.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-dpi-y}"
  (context (g:object print-context)))

(export 'print-context-dpi-x)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_dpi_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_dpi_y" print-context-dpi-y) :double
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The double float for the vertical resolution of @arg{context}.}
  @begin{short}
    Obtains the vertical resolution of the print context, in dots per inch.
  @end{short}
  @see-class{gtk:print-context}
  @see-function{gtk:print-context-dpi-x}"
  (context (g:object print-context)))

(export 'print-context-dpi-y)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_pango_fontmap
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_pango_fontmap" print-context-pango-fontmap)
    (g:object pango:font-map)
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The @class{pango:font-map} object of @arg{context}.}
  @begin{short}
    Returns a font map that is suitable for use with the
    @class{gtk:print-context} object.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{pango:font-map}"
  (context (g:object print-context)))

(export 'print-context-pango-fontmap)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_create_pango_context"
               print-context-create-pango-context) (g:object pango:context)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The new @class{pango:context} object for @arg{context}.}
  @begin{short}
    Creates a new Pango context that can be used with the print context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{pango:context}"
  (context (g:object print-context)))

(export 'print-context-create-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_create_pango_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_create_pango_layout"
               print-context-create-pango-layout) (g:object pango:layout)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[context]{a @class{gtk:print-context} object}
  @return{The new @class{pango:layout} object for @arg{context}.}
  @begin{short}
    Creates a new Pango layout that is suitable for use with the print context.
  @end{short}
  @see-class{gtk:print-context}
  @see-class{pango:layout}"
  (context (g:object print-context)))

(export 'print-context-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_context_get_hard_margins
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_context_get_hard_margins" %print-context-hard-margins)
    :boolean
  (context (g:object print-context))
  (top (:pointer :int))
  (bottom (:pointer :int))
  (left (:pointer :int))
  (right (:pointer :int)))

(defun print-context-hard-margins (context)
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[context]{a @class{gtk:print-context} object}
  @begin{return}
    @arg{top} -- an integer with the top hardware printer margin @br{}
    @arg{bottom} -- an integer with the bottom hardware printer margin @br{}
    @arg{left} -- an integer with the left hardware printer margin @br{}
    @arg{right} -- an integer with the right hardware printer margin
  @end{return}
  @begin{short}
    Obtains the hardware printer margins of the print context, in units.
  @end{short}
  @see-class{gtk:print-context}"
  (cffi:with-foreign-objects ((top :int) (bottom :int) (left :int) (right :int))
    (%print-context-hard-margins context top bottom left right)
    (values (cffi:mem-ref top :int)
            (cffi:mem-ref bottom :int)
            (cffi:mem-ref left :int)
            (cffi:mem-ref right :int))))

(export 'print-context-hard-margins)

;;; --- End of file gtk3.print-context.lisp ------------------------------------
