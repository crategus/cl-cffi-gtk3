;;; ----------------------------------------------------------------------------
;;; gdk3.color.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
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
;;; Colors
;;;
;;;     Manipulation of colors
;;;
;;; Types and Values
;;;
;;;     GdkColor
;;;
;;; Functons
;;;
;;;     gdk_color_copy
;;;     gdk_color_free                                      not needed
;;;     gdk_color_parse
;;;     gdk_color_equal
;;;     gdk_color_hash
;;;     gdk_color_to_string
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkColor
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct color "GdkColor"
  (:export t
   :type-initializer "gdk_color_get_type")
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'color)
      "GBoxed"
      (documentation 'color 'type)
 "@version{2025-07-03}
  @begin{declaration}
(define-gboxed-cstruct color \"GdkColor\"
  (:export t
   :type-initializer \"gdk_color_get_type\")
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[pixel]{For allocated colors, the pixel value used to draw this
        color on the screen. Not used anymore.}
      @entry[red]{The red component of the color. This is a value between
        0 and 65535, with 65535 indicating full intensity.}
      @entry[green]{The green component of the color. This is a value between
        0 and 65535, with 65535 indicating full intensity.}
      @entry[blue]{The blue component of the color. This is a value between
        0 and 65535, with 65535 indicating full intensity.}
  @end{simple-table}
  @end{values}
  @begin{short}
    The @class{gdk:color} structure is used to describe a color, similar to the
    XColor structure used in the X11 drawing API.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gdk:color} structure has been deprecated since version 3.14 and
    should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-constructor{gdk:color-new}
  @see-constructor{gdk:color-copy}
  @see-slot{gdk:color-pixel}
  @see-slot{gdk:color-red}
  @see-slot{gdk:color-green}
  @see-slot{gdk:color-blue}
  @see-class{gdk:rgba}")

;;; ----------------------------------------------------------------------------
;;; Accessors of the GdkColor structure
;;; ----------------------------------------------------------------------------

;;; --- gdk:color-pixel --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-pixel)
      "Accessor"
      (documentation 'color-pixel 'function)
 "@version{2025-1-15}
  @syntax{(gdk:color-pixel instance) => pixel}
  @syntax{(setf (gdk:color-pixel instance) pixel)}
  @begin{short}
    Accessor of the @code{pixel} slot of the @class{gdk:color} structure.
  @end{short}
  For allocated colors, the pixel value used to draw this color on the screen.
  Not used anymore.
  @begin[Warning]{dictionary}
    The @fun{gdk:color-pixel} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}")

;;; --- gdk:color-red ----------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-red)
      "Accessor"
      (documentation 'color-red 'function)
 "@version{2025-1-15}
  @syntax{(gdk:color-red instance) => red}
  @syntax{(setf (gdk:color-red instance) red)}
  @begin{short}
    Accessor of the @code{red} slot of the @class{gdk:color} structure.
  @end{short}
  The red component of the color. This is a value between 0 and 65535, with
  65535 indicating full intensity.
  @begin[Warning]{dictionary}
    The @fun{gdk:color-red} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}
  @see-function{gdk:color-green}
  @see-function{gdk:color-blue}")

;;; --- gdk:color-green --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-green)
      "Accessor"
      (documentation 'color-green 'function)
 "@version{2025-1-15}
  @syntax{(gdk:color-green instance) => green}
  @syntax{(setf (gdk:color-green instance) green)}
  @begin{short}
    Accessor of the @code{green} slot of the @class{gdk:color} structure.
  @end{short}
  The green component of the color. This is a value between 0 and 65535, with
  65535 indicating full intensity.
  @begin[Warning]{dictionary}
    The @fun{gdk:color-green} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}
  @see-function{gdk:color-red}
  @see-function{gdk:color-blue}")

;;; --- gdk:color-blue ---------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-blue)
      "Accessor"
      (documentation 'color-blue 'function)
 "@version{2025-1-15}
  @syntax{(gdk:color-blue instance) => blue}
  @syntax{(setf (gdk:color-blue instance) blue)}
  @begin{short}
    Accessor of the @code{blue} slot of the @class{gdk:color} structure.
  @end{short}
  The blue component of the color. This is a value between 0 and 65535, with
  65535 indicating full intensity.
  @begin[Warning]{dictionary}
    The @fun{gdk:color-blue} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}
  @see-function{gdk:color-red}
  @see-function{gdk:color-green}")

;;; ----------------------------------------------------------------------------
;;; gdk:color-new
;;; ----------------------------------------------------------------------------

(declaim (inline color-new))

(defun color-new (&key (pixel 0) (red 0) (green 0) (blue 0))
 #+liber-documentation
 "@version{2025-1-15}
  @argument[pixel]{an unsigned integer for the pixel value used to draw this
    color on the screen, not used anymore}
  @argument[red]{an unsigned integer for the red component of the color, this
    is a value between 0 and 65535, with 65535 indicating full intensity}
  @argument[green]{an unsigned integer for the green component of the color}
  @argument[blue]{an unsigned integer for the blue component of the color}
  @return{The newly created @class{gdk:color} instance.}
  @begin{short}
    Creates a new @class{gdk:color} instance.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:color-new} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}"
  (make-color :pixel pixel :red red :green green :blue blue))

(export 'color-new)

;;; ----------------------------------------------------------------------------
;;; gdk_color_copy
;;; ----------------------------------------------------------------------------

(declaim (inline color-copy))

(defun color-copy (color)
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color]{a @class{gdk:color} color}
  @return{The newly created @class{gdk:color} instance with the copy of
    @arg{color}.}
  @begin{short}
    Makes a copy of a color.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:color-copy} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}"
  (copy-color color))

(export 'color-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_color_free
;;;
;;; Frees a color structure created with gdk_color_copy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_parse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_parse" %color-parse) :boolean
  (spec :string)
  (color (g:boxed color :return)))

(defun color-parse (spec)
 #+liber-documentation
 "@version{2025-1-15}
  @argument[spec]{a string specifying the color}
  @return{The @class{gdk:color} instance or @code{nil} if the parsing did not
    succeed.}
  @begin{short}
    Parses a textual specification of a color and fill in the red, green, and
    blue fields of a @class{gdk:color} color.
  @end{short}
  The string can either one of a large set of standard names taken from the
  X11 @file{rgb.txt} file, or it can be a hex value in the form @code{#rgb},
  @code{#rrggbb}, @code{#rrrgggbbb} or @code{#rrrrggggbbbb} where @code{r},
  @code{g} and @code{b} are hex digits of the red, green, and blue components
  of the color, respectively. White in the four forms is @code{#fff},
  @code{#ffffff}, @code{#fffffffff} and @code{#ffffffffffff}.
  @begin[Warning]{dictionary}
    The @fun{gdk:color-parse} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}
  @see-function{gdk:color-to-string}"
  (let ((color (color-new)))
    (when (%color-parse spec color)
      color)))

(export 'color-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_color_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_equal" color-equal) :boolean
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color1]{a @class{gdk:color} instance}
  @argument[color2]{another @class{gdk:color} instance}
  @return{@em{True} if the two colors compare equal.}
  @begin{short}
    Compares two colors.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:color-equal} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}"
  (color1 (g:boxed color))
  (color2 (g:boxed color)))

(export 'color-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_color_hash
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_hash" color-hash) :uint
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color]{a @class{gdk:color} instance}
  @return{The unsigned integer with the hash value applied to @arg{color}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    @class{gdk:color} colors.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:color-hash} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}"
  (color (g:boxed color)))

(export 'color-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_color_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_to_string" color-to-string)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color]{a @class{gdk:color} instance}
  @return{The text string representing @arg{color}.}
  @begin{short}
    Returns a textual specification of @arg{color} in the hexadecimal form
    @code{#rrrrggggbbbb}, where @code{r}, @code{g} and @code{b} are hex digits
    representing the red, green and blue components respectively.
  @end{short}
  The returned string can be parsed by the @fun{gdk:color-parse} function.
  @begin[Warning]{dictionary}
    The @fun{gdk:color-to-string} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the @class{gdk:rgba}
    structure.
  @end{dictionary}
  @see-class{gdk:color}
  @see-class{gdk:rgba}
  @see-function{gdk:color-parse}"
  (color (g:boxed color)))

(export 'color-to-string)

;;; --- End of file gdk3.color.lisp --------------------------------------------
