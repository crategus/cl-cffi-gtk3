;;; ----------------------------------------------------------------------------
;;; gtk3.orientable.lisp
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
;;; GtkOrientable
;;;
;;;     An interface for flippable widgets
;;;
;;; Types and Values
;;;
;;;     GtkOrientable
;;;
;;; Functions
;;;
;;;     gtk_orientable_get_orientation                     Accessor
;;;     gtk_orientable_set_orientation                     Accessor
;;;
;;; Properties
;;;
;;;     orientation
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOrientable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkOrientable" orientable
  (:export t
   :type-initializer "gtk_orientable_get_type")
  ((orientation
    orientable-orientation
    "orientation" "GtkOrientation" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'orientable)
      "Interface"
      (documentation 'orientable 'type)
 "@version{2025-07-11}
  @begin{short}
    An interface for flippable widgets.
  @end{short}
  The @class{gtk:orientable} interface is implemented by all widgets that can
  be oriented horizontally or vertically. Historically, such widgets have been
  realized as subclasses of a common base class, for example, GtkBox, GtkHBox,
  GtkVBox. The @class{gtk:orientable} interface is more flexible in that it
  allows the orientation to be changed at runtime, allowing the widgets to
  \"flip\".
  @see-slot{gtk:orientable-orientation}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:orientable-orientation ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'orientable) t)
 "The @code{orientation} property of type @sym{gtk:orientation} (Read / Write)
  @br{}
  The orientation of the orientable widget. @br{}
  Default value: @val[gtk:orientation]{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'orientable-orientation)
      "Accessor"
      (documentation 'orientable-orientation 'function)
 "@version{2025-07-11}
  @syntax{(gtk:orientable-orientation object) => orientation}
  @syntax{(setf (gtk:orientable-orientation object) orientation)}
  @argument[object]{a @class{gtk:orientable} widget}
  @argument[orientation]{a value of the @sym{gtk:orientation} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:orientable]{orientation} slot of the
    @class{gtk:orientable} interface.
  @end{short}
  The @fun{gtk:orientable-orientation} function returns the orientation of the
  orientable widget. The @setf{gtk:orientable-orientation} function sets the
  orientation.
  @see-class{gtk:orientable}
  @see-symbol{gtk:orientation}")

;;; --- End of file gtk3.orientable.lisp ---------------------------------------
