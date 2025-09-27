;;; ----------------------------------------------------------------------------
;;; gdk3.rectangle.lisp
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
;;; Points and Rectangles
;;;
;;;     Simple graphical data types
;;;
;;; Types and Values
;;;
;;;     GdkRectangle
;;;
;;; Functions
;;;
;;;     gdk_rectangle_intersect
;;;     gdk_rectangle_union
;;;     gdk_rectangle_equal
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRectangle
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct rectangle "GdkRectangle"
  (:export t
   :type-initializer "gdk_rectangle_get_type")
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'rectangle)
      "GBoxed"
      (documentation 'rectangle 'type)
 "@version{2025-08-31}
  @begin{declaration}
(define-gboxed-cstruct rectangle \"GdkRectangle\"
  (:export t
   :type-initializer \"gdk_rectangle_get_type\")
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))
  @end{declaration}
  @begin{values}
    @begin{simple-table}
      @entry[x]{The integer for the x coordinate of the top left corner.}
      @entry[y]{The integer for the y coordinate of the top left corner.}
      @entry[width]{The integer for the width of the rectangle.}
      @entry[height]{The integer for the height of the rectangle.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @class{gdk:rectangle} structure is holding the position and size of a
    rectangle.
  @end{short}

  The intersection of two rectangles can be computed with the
  @fun{gdk:rectangle-intersect} function. To find the union of two rectangles
  use the @fun{gdk:rectangle-union} function.

  The @sym{cairo:region-t} structure is usually used for managing clipping
  of graphical operations.
  @see-constructor{gdk:rectangle-new}
  @see-constructor{gdk:rectangle-copy}
  @see-slot{gdk:rectangle-x}
  @see-slot{gdk:rectangle-y}
  @see-slot{gdk:rectangle-width}
  @see-slot{gdk:rectangle-height}
  @see-symbol{cairo:region-t}")

;;; --- gdk:rectangle-x --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-x)
      "Accessor"
      (documentation 'rectangle-x 'function)
 "@version{2025-08-31}
  @syntax{(gdk:rectangle-x instance) => x}
  @syntax{(setf (gdk:rectangle-x instance) x)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[x]{an integer for the x coordinate of the rectangle}
  @begin{short}
    The accessor for the @code{x} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-y --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-y)
      "Accessor"
      (documentation 'rectangle-y 'function)
 "@version{2025-08-31}
  @syntax{(gdk:rectangle-y instance) => y}
  @syntax{(setf (gdk:rectangle-y instance) y)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[y]{an integer for the y coordinate of the rectangle}
  @begin{short}
    The accessor for the @code{y} slot of the @class{gdk:rectangle} structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-width ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-width)
      "Accessor"
      (documentation 'rectangle-width 'function)
 "@version{2025-08-31}
  @syntax{(gdk:rectangle-width instance) => width}
  @syntax{(setf (gdk:rectangle-width instance) width)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[width]{an integer for the width of the rectangle}
  @begin{short}
    The accessor for the @code{width} slot of the @class{gdk:rectangle}
    structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-height ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-height)
      "Accessor"
      (documentation 'rectangle-height 'function)
 "@version{2025-08-31}
  @syntax{(gdk:rectangle-height instance) => height}
  @syntax{(setf (gdk:rectangle-height instance) height)}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @argument[height]{an integer for the height of the rectangle}
  @begin{short}
    The accessor for the @code{height} slot of the @class{gdk:rectangle}
    structure.
  @end{short}
  @see-class{gdk:rectangle}")

;;; --- gdk:rectangle-new ------------------------------------------------------

(declaim (inline rectangle-new))

(defun rectangle-new (&key (x 0) (y 0) (width 0) (height 0))
 #+liber-documentation
 "@version{2024-01-01}
  @argument[x]{an integer with the value for the @code{x} slot}
  @argument[y]{an integer with the value for the @code{y} slot}
  @argument[width]{an integer with the value for the @code{width} slot}
  @argument[height]{an integer with the value for the @code{height} slot}
  @return{The newly created @class{gdk:rectangle} instance.}
  @begin{short}
    Returns a @class{gdk:rectangle} instance with the initial values given to
    @arg{x}, @arg{y}, @arg{width}, and @arg{height}.
  @end{short}
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-copy}"
  (make-rectangle :x x :y y :width width :height height))

(export 'rectangle-new)

;;; --- gdk:rectangle-copy -----------------------------------------------------

(declaim (inline rectangle-copy))

(defun rectangle-copy (instance)
 #+liber-documentation
 "@version{2024-01-01}
  @argument[instance]{a @class{gdk:rectangle} instance}
  @return{The @class{gdk:rectangle} instance.}
  @short{Copy constructor of a @class{gdk:rectangle} structure.}
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-new}"
  (copy-rectangle instance))

(export 'rectangle-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_intersect ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_intersect" %rectangle-intersect) :boolean
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle))
  (dest  (g:boxed rectangle)))

(defun rectangle-intersect (rect1 rect2)
 #+liber-documentation
 "@version{2024-01-01}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @return{The @class{gdk:rectangle} instance with the intersection of
    @arg{rect1} and @arg{rect2}, or @code{nil}.}
  @begin{short}
    Calculates the intersection of two rectangles.
  @end{short}
  If the rectangles do not intersect @code{nil} is returned.
  @see-class{gdk:rectangle}
  @see-function{gdk:rectangle-union}"
  (let ((dest (make-rectangle)))
    (when (%rectangle-intersect rect1 rect2 dest)
      dest)))

(export 'rectangle-intersect)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_union ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_union" %rectangle-union) :void
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle))
  (dest  (g:boxed rectangle)))

(defun rectangle-union (rect1 rect2)
 #+liber-documentation
 "@version{2024-01-01}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @return{The @class{gdk:rectangle} instance with the union of @arg{rect1} and
    @arg{rect2}.}
  @begin{short}
    Calculates the union of two rectangles.
  @end{short}
  The union of rectangles @arg{rect1} and @arg{rect2} is the smallest rectangle
  which includes both rectangles within it.
  @see-class{gdk:rectangle}
  @see-function{gtk-rectangle-intersect}"
  (let ((dest (make-rectangle)))
    (%rectangle-union rect1 rect2 dest)
    dest))

(export 'rectangle-union)

;;; ----------------------------------------------------------------------------
;;; gdk_rectangle_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_rectangle_equal" rectangle-equal) :boolean
 #+liber-documentation
 "@version{2024-01-01}
  @argument[rect1]{a @class{gdk:rectangle} instance}
  @argument[rect2]{a @class{gdk:rectangle} instance}
  @return{@em{True} if the rectangles are equal.}
  @short{Checks if the two given rectangles are equal.}
  @see-class{gdk:rectangle}"
  (rect1 (g:boxed rectangle))
  (rect2 (g:boxed rectangle)))

(export 'rectangle-equal)

;;; --- End of file gdk3.rectangle.lisp ----------------------------------------
