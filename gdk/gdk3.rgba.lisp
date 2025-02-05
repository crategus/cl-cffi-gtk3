;;; ----------------------------------------------------------------------------
;;; gdk3.rgba.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; RGBA Colors
;;;
;;;     RGBA colors
;;;
;;; Types and Values
;;;
;;;     GdkRGBA
;;;
;;; Functions
;;;
;;;     gdk_rgba_copy
;;;     gdk_rgba_free
;;;     gdk_rgba_parse
;;;     gdk_rgba_equal
;;;     gdk_rgba_hash
;;;     gdk_rgba_to_string
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRGBA
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct rgba "GdkRGBA"
  (:export t
   :type-initializer "gdk_rgba_get_type")
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))

#+liber-documentation
(setf (liber:alias-for-class 'rgba)
      "GBoxed"
      (documentation 'rgba 'type)
 "@version{2025-1-15}
  @begin{declaration}
(define-gboxed-cstruct gdk:rgba \"GdkRGBA\"
  (:export t
   :type-initializer \"gdk_rgba_get_type\")
  (red :double :initform 0.0d0)
  (green :double :initform 0.0d0)
  (blue :double :initform 0.0d0)
  (alpha :double :initform 0.0d0))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[red]{The intensity of the red channel from 0.0 to 1.0 inclusive.}
      @entry[green]{The intensity of the green channel from 0.0 to 1.0
        inclusive.}
      @entry[blue]{The intensity of the blue channel from 0.0 to 1.0 inclusive.}
      @entry[alpha]{The opacity of the color from 0.0 for completely translucent
        to 1.0 for opaque.}
    @end{table}
  @end{values}
  @begin{short}
    The @class{gdk:rgba} structure is used to represent a (possibly translucent)
    color, in a way that is compatible with Cairo's notion of color.
  @end{short}
  @see-constructor{gdk:rgba-new}
  @see-constructor{gdk:rgba-copy}
  @see-slot{gdk:rgba-red}
  @see-slot{gdk:rgba-green}
  @see-slot{gdk:rgba-blue}
  @see-slot{gdk:rgba-alpha}")

;;; ----------------------------------------------------------------------------
;;; Accessors
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rgba-red)
      "Accessor"
      (documentation 'rgba-red 'function)
 "@version{2025-1-25}
  @syntax{(gdk:rgba-red instance) => red}
  @syntax{(setf (gdk:rgba-red instance) red)}
  @argument[instance]{a @struct{gdk:rgba} instance}
  @argument[red]{a number coerced to a double float for the intensity of
    the red channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{red} slot of the @struct{gdk:rgba} structure.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-green}
  @see-function{gdk:rgba-blue}
  @see-function{gdk:rgba-alpha}")

#+liber-documentation
(setf (liber:alias-for-function 'rgba-green)
      "Accessor"
      (documentation 'rgba-green 'function)
 "@version{2025-1-25}
  @syntax{(gdk:rgba-green instance) => green}
  @syntax{(setf (gdk:rgba-green instance) green)}
  @argument[instance]{a @struct{gdk:rgba} instance}
  @argument[green]{a number coerced to a double float for the intensity
    of the green channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{green} slot of the @struct{gdk:rgba} structure.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-red}
  @see-function{gdk:rgba-blue}
  @see-function{gdk:rgba-alpha}")

#+liber-documentation
(setf (liber:alias-for-function 'rgba-blue)
      "Accessor"
      (documentation 'rgba-blue 'function)
 "@version{2025-1-25}
  @syntax{(gdk:rgba-blue instance) => blue}
  @syntax{(setf (gdk:rgba-blue instance) blue)}
  @argument[instance]{a @struct{gdk:rgba} color}
  @argument[blue]{a number coerced to a double float for the intensity
    of the blue channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{blue} slot of the @struct{gdk:rgba} structure.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-red}
  @see-function{gdk:rgba-green}
  @see-function{gdk:rgba-alpha}")

#+liber-documentation
(setf (liber:alias-for-function 'rgba-alpha)
      "Accessor"
      (documentation 'rgba-alpha 'function)
 "@version{2025-1-25}
  @syntax{(gdk:rgba-alpha instance) => alpha}
  @syntax{(setf (gdk:rgba-alpha instance) alpha)}
  @argument[instance]{a @struct{gdk:rgba} instance}
  @argument[alpha]{a number coerced to a double float for the opacity of
    the color from 0.0 for completely translucent to 1.0 for opaque}
  @begin{short}
    Accessor of the @code{alpha} slot of the @struct{gdk:rgba} structure.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-red}
  @see-function{gdk:rgba-green}
  @see-function{gdk:rgba-blue}")

;;; ----------------------------------------------------------------------------
;;; rgba-new
;;; ----------------------------------------------------------------------------

(defun rgba-new (&key (red 0.0d0) (green 0.0d0) (blue 0.0d0) (alpha 0.0d0))
 "@version{2025-1-25}
  @argument[red]{a number for the intensity of the red channel from 0.0 to 1.0
    inclusive}
  @argument[green]{a number for the intensity of the green channel from 0.0 to
    1.0 inclusive}
  @argument[blue]{a number for the intensity of the blue channel from 0.0 to
    1.0 inclusive}
  @argument[alpha]{a number for the opacity of the color from 0.0 for
    completely translucent to 1.0 for opaque}
  @return{The newly created @class{gdk:rgba} instance.}
  @begin{short}
    Creates a @struct{gdk:rgba} instance.
  @end{short}
  @begin[Note]{dictionary}
    All numbers are coerced to a double float before being passed to the
    foreign C function.
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-copy}"
  (make-rgba :red (coerce red 'double-float)
             :green (coerce green 'double-float)
             :blue (coerce blue 'double-float)
             :alpha (coerce alpha 'double-float)))

(export 'rgba-new)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_copy
;;; ----------------------------------------------------------------------------

(defun rgba-copy (rgba)
 #+liber-documentation
 "@version{2025-1-15}
  @argument[rgba]{a @struct{gdk:rgba} instance}
  @return{The newly allocated @struct{gdk:rgba} instance with the same contents
    as @arg{rgba}.}
  @short{Makes a copy of a RGBA color.}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-new}"
  (copy-rgba rgba))

(export 'rgba-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_free                                           not needed
;;;
;;; Frees a GdkRGBA struct created with gdk_rgba_copy()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_parse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rgba_parse" %rgba-parse) :boolean
  (rgba (g:boxed rgba :return))
  (str :string))

(defun rgba-parse (str)
 #+liber-documentation
 "@version{2025-1-15}
  @argument[str]{a string specifying the color}
  @return{The @struct{gdk:rgba} instance with the filled in values.}
  @begin{short}
    Parses a textual representation of a color, and returns a RGBA instance
    filling in the @code{red}, @code{green}, @code{blue} and @code{alpha}
    fields.
  @end{short}
  The string can be either one of:
  @begin{itemize}
    @item{A standard name taken from the X11 @code{rgb.txt} file.}
    @item{A hex value in the form @code{rgb}, @code{rrggbb}, @code{rrrgggbbb}
      or @code{rrrrggggbbbb}.}
    @item{A RGB color in the form @code{rgb(r,g,b)}. In this case the color
      will have full opacity.}
    @item{A RGBA color in the form @code{rgba(r,g,b,a)}.}
  @end{itemize}
  Where @code{r}, @code{g}, @code{b} and @code{a} are respectively the red,
  green, blue and alpha color values. In the last two cases, @code{r}, @code{g}
  and @code{b} are either integers in the range 0 to 255 or precentage values
  in the range 0% to 100%, and @code{a} is a floating point value in the range
  0.0 to 1.0.
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:rgba-parse \"LightGreen\")
=> #S(GDK-RGBA
      :RED 0.5647058823529412d0
      :GREEN 0.9333333333333333d0
      :BLUE 0.5647058823529412d0
      :ALPHA 1.0d0)
(gdk-rgba-parse \"#90ee90\")
=> #S(GDK-RGBA
      :RED 0.5647058823529412d0
      :GREEN 0.9333333333333333d0
      :BLUE 0.5647058823529412d0
      :ALPHA 1.0d0)
    @end{pre}
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-to-string}"
  (let ((rgba (make-rgba)))
    (when (%rgba-parse rgba str)
      rgba)))

(export 'rgba-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rgba_equal" rgba-equal) :boolean
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color1]{a @struct{gdk:rgba} instance}
  @argument[color2]{another @struct{gdk:rgba} instance}
  @return{@em{True} if the two colors compare equal.}
  @short{Compares two RGBA colors.}
  @see-struct{gdk:rgba}"
  (color1 (g:boxed rgba))
  (color2 (g:boxed rgba)))

(export 'rgba-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_hash
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rgba_hash" rgba-hash) :uint
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color]{a @struct{gdk:rgba} instance}
  @return{The unsigned integer with the hash value for @arg{color}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    RGBA colors.
  @end{short}
  @see-struct{gdk:rgba}"
  (color (g:boxed rgba)))

(export 'rgba-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rgba_to_string" rgba-to-string) :string
 #+liber-documentation
 "@version{2025-1-15}
  @argument[color]{a @struct{gdk:rgba} instance}
  @return{The string with the textual specification of @arg{color}.}
  @begin{short}
    Returns a textual specification of @arg{color} in the form @code{rgb(r,g,b)}
    or @code{rgba(r,g,b,a)}, where @code{r}, @code{g}, @code{b} and @code{a}
    represent the red, green, blue and alpha values respectively.
  @end{short}
  @code{r}, @code{g}, and @code{b} are represented as integers in the range 0
  to 255, and @code{a} is represented as a floating point value in the range
  0.0 to 1.0.

  These string forms are supported by the CSS3 colors module, and can be parsed
  by the @fun{gdk:rgba-parse} function.

  Note that this string representation may loose some precision, since @code{r},
  @code{g} and @code{b} are represented as 8-bit integers. If this is a concern,
  you should use a different representation.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:rgba-to-string (gdk:rgba-new :red 1.0))
=> \"rgba(255,0,0,0)\"
(gdk:rgba-parse *)
=> #S(GDK-RGBA :RED 1.0d0 :GREEN 0.0d0 :BLUE 0.0d0 :ALPHA 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-parse}"
  (color (g:boxed rgba)))

(export 'rgba-to-string)

;;; --- End of file gdk3.rgba.lisp ---------------------------------------------
