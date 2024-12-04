;;; ----------------------------------------------------------------------------
;;; gtk3.color-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GtkColorChooser
;;;
;;;     Interface implemented by widgets for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooser
;;;
;;; Accessors
;;;
;;;     gtk_color_chooser_get_rgba
;;;     gtk_color_chooser_set_rgba
;;;     gtk_color_chooser_get_use_alpha
;;;     gtk_color_chooser_set_use_alpha
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_add_palette
;;;
;;; Properties
;;;
;;;     rgba
;;;     use-alpha
;;;
;;; Signals
;;;
;;;     color-activated
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkColorChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorChooser
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkColorChooser" color-chooser
  (:export t
   :type-initializer "gtk_color_chooser_get_type")
  ((rgba
    color-chooser-rgba
    "rgba" "GdkRGBA" t t)
   (use-alpha
    color-chooser-use-alpha
    "use-alpha" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'color-chooser)
      "Interface"
      (documentation 'color-chooser 'type)
 "@version{2023-6-12}
  @begin{short}
    The @class{gtk:color-chooser} interface is an interface that is implemented
    by widgets for choosing colors.
  @end{short}
  Depending on the situation, colors may be allowed to have alpha
  (translucency). The main widgets that implement this interface are the
  @class{gtk:color-button}, @class{gtk:color-chooser-widget}, and
  @class{gtk:color-chooser-dialog} widgets.
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-activated\" signal}
      @begin{pre}
lambda (chooser color)    :run-first
      @end{pre}
      Emitted when a color is activated from the color chooser. This usually
      happens when the user clicks a color swatch, or a color is selected and
      the user presses one of the @kbd{Space}, @kbd{Shift+Space}, @kbd{Return}
      or @kbd{Enter} keys.
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:color-chooser} widget which received
          the signal.}
        @entry[color]{The @struct{gdk:rgba} color.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:color-chooser-rgba}
  @see-slot{gtk:color-chooser-use-alpha}
  @see-struct{gdk:rgba}
  @see-class{gtk:color-button}
  @see-class{gtk:color-chooser-dialog}
  @see-class{gtk:color-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:color-chooser-rgba -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'color-chooser) t)
 "The @code{rgba} property of type @struct{gdk:rgba} (Read / Write) @br{}
  The @code{rgba} property contains the currently selected color, as a
  RGBA color. The property can be set to change the current selection
  programmatically.")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-rgba)
      "Accessor"
      (documentation 'color-chooser-rgba 'function)
 "@version{2023-6-12}
  @syntax{(gtk:color-chooser-rgba object) => color}
  @syntax{(setf (gtk:color-chooser-rgba object) color)}
  @argument[object]{a @class{gtk:color-chooser} widget}
  @argument[color]{a @struct{gdk:rgba} color}
  @begin{short}
    Accessor of the @slot[gtk:color-chooser]{rgba} slot of the
    @class{gtk:color-chooser} class.
  @end{short}
  The @fun{gtk:color-chooser-rgba} function gets the currently selected color.
  The @setf{gtk:color-chooser-rgba} function sets the color.

  The @code{rgba} property contains the currently selected color, as a
  @struct{gdk:rgba} color. The property can be set to change the current
  selection programmatically.
  @see-class{gtk:color-chooser}
  @see-struct{gdk:rgba}")

;;; --- gtk:color-chooser-use-alpha --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-alpha" 'color-chooser) t)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  When @code{use-alpha} is @em{true}, colors may have alpha (translucency)
  information. When it is @em{false}, the RGBA color obtained via the
  @code{rgba} property will be forced to have the alpha value 1.0.
  Implementations are expected to show alpha by rendering the color over a
  non-uniform background (like a checkerboard pattern). @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-use-alpha)
      "Accessor"
      (documentation 'color-chooser-use-alpha 'function)
 "@version{2023-6-12}
  @syntax{(gtk:color-chooser-use-alpha object) => use-alpha}
  @syntax{(setf (gtk:color-chooser-use-alpha object) use-alpha)}
  @argument[object]{a @class{gtk:color-chooser} widget}
  @argument[use-alpha]{@em{true} if the color chooser should use alpha channel,
    @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk:color-chooser]{use-alpha} slot of the
    @class{gtk:color-chooser} class.
  @end{short}
  The @fun{gtk:color-chooser-use-alpha} function returns whether the color
  chooser shows the alpha channel. The @setf{gtk:color-chooser-use-alpha}
  function sets whether or not the color chooser should use the alpha channel.
  @see-class{gtk:color-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_add_palette ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_color_chooser_add_palette" %color-chooser-add-palette) :void
  (chooser (g:object color-chooser))
  (orientation orientation)
  (colors-per-line :int)
  (n-colors :int)
  (colors :pointer))

(defun color-chooser-add-palette (chooser orientation colors-per-line colors)
 #+liber-documentation
 "@version{2023-6-12}
  @argument[chooser]{a @class{gtk:color-chooser} widget}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @argument[colors-per-line]{an integer with the number of colors to show in
    each row/column}
  @argument[colors]{a list with the @struct{gdk:rgba} colors of the palette,
    or @code{nil}}
  @begin{short}
    Adds a palette to the color chooser.
  @end{short}
  If the orientation is @code{:horizontal}, the colors are grouped in rows,
  with @arg{colors-per-line} colors in each row. If the orientation is
  @code{:vertical}, the colors are grouped in columns instead.

  The default color palette of the color chooser widget has 27 colors,
  organized in columns of 3 colors. The default gray palette has 9 grays in a
  single row. The layout of the color chooser widget works best when the
  palettes have 9 - 10 columns.

  Calling this function for the first time has the side effect of removing the
  default color and gray palettes from the color chooser. If @arg{colors} is
  @code{nil}, removes all previously added palettes.
  @see-struct{gdk:rgba}
  @see-class{gtk:color-chooser}
  @see-symbol{gtk:orientation}"
  (if colors
      (glib:with-gboxed-array (n-colors colors-ptr gdk:rgba colors)
        (%color-chooser-add-palette chooser
                                    orientation
                                    colors-per-line
                                    n-colors
                                    colors-ptr))
      (%color-chooser-add-palette chooser
                                  orientation
                                  colors-per-line
                                  0
                                  (cffi:null-pointer))))

(export 'color-chooser-add-palette)

;;; --- End of file gtk3.color-chooser.lisp ------------------------------------
