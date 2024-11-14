;;; ----------------------------------------------------------------------------
;;; gtk3.separator.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkSeparator
;;;
;;;     A separator widget
;;;
;;; Types and Values
;;;
;;;     GtkSeparator
;;;
;;; Functions
;;;
;;;     gtk_separator_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSeparator
;;;                 ├── GtkHSeparator
;;;                 ╰── GtkVSeparator
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSeparator implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparator
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSeparator" separator
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_separator_get_type")
  nil)

#+liber-documentation
(setf (documentation 'separator 'type)
 "@version{2023-12-30}
  @begin{short}
    The @class{gtk:separator} widget is a horizontal or vertical separator
    widget, depending on the value of the @slot[gtk:orientable]{orientation}
    property of the @class{gtk:orientable} interface, used to group the widgets
    within a window.
  @end{short}
  It displays a line with a shadow to make it appear sunken into the
  interface.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:separator} implementation has a single CSS node with name
    @code{separator}. The node gets one of the @code{.horizontal} or
    @code{.vertical} style classes.
  @end{dictionary}
  @see-constructor{gtk:separator-new}
  @see-class{gtk:orientable}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline separator-new))

(defun separator-new (orientation)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[orientation]{a @symbol{gtk:orientation} value with orientation of
    of the separator}
  @return{The new @class{gtk:separator} widget.}
  @begin{short}
    Creates a new separator widget with the given @arg{orientation}.
  @end{short}
  See also the @class{gtk:orientable} interface.
  @see-class{gtk:separator}
  @see-class{gtk:orientable}
  @see-symbol{gtk:orientation}"
  (make-instance 'separator
                 :orientation orientation))

(export 'separator-new)

;;; --- End of file gtk3.separator.lisp ----------------------------------------
