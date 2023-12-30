;;; ----------------------------------------------------------------------------
;;; gtk3.bin.lisp
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
;;; GtkBin
;;;
;;;     A container with just one child
;;;
;;; Types and Values
;;;
;;;     GtkBin
;;;
;;; Functions
;;;
;;;     gtk_bin_get_child
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ├── GtkWindow
;;;                     ├── GtkActionBar
;;;                     ├── GtkAlignment
;;;                     ├── GtkComboBox
;;;                     ├── GtkFrame
;;;                     ├── GtkButton
;;;                     ├── GtkMenuItem
;;;                     ├── GtkEventBox
;;;                     ├── GtkExpander
;;;                     ├── GtkFlowBoxChild
;;;                     ├── GtkHandleBox
;;;                     ├── GtkListBoxRow
;;;                     ├── GtkToolItem
;;;                     ├── GtkOverlay
;;;                     ├── GtkScrolledWindow
;;;                     ├── GtkPopover
;;;                     ├── GtkRevealer
;;;                     ├── GtkSearchBar
;;;                     ├── GtkStackSidebar
;;;                     ╰── GtkViewport
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBin implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkBin
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkBin" bin
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_bin_get_type")
  nil)

#+liber-documentation
(setf (documentation 'bin 'type)
 "@version{2023-12-30}
  @begin{short}
    The @class{gtk:bin} widget is a container with just one child.
  @end{short}
  It is not very useful itself, but it is useful for deriving subclasses, since
  it provides common code needed for handling a single child widget.

  Many GTK widgets are subclasses of the @class{gtk:bin} class, including the
  @class{gtk:window}, @class{gtk:button}, @class{gtk:frame}, or
  @class{gtk:scrolled-window} classes.
  @see-class{gtk:window}
  @see-class{gtk:button}
  @see-class{gtk:frame}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_bin_get_child () -> bin-child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bin_get_child" bin-child) (g:object widget)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[bin]{a @class{gtk:bin} widget}
  @return{The @class{gtk:widget} child widget of @arg{bin}.}
  @begin{short}
    Gets the child widget of the bin widget, or @code{nil} if the @arg{bin}
    argument contains no child widget.
  @end{short}
  @see-class{gtk:bin}
  @see-class{gtk:widget}"
  (bin (g:object bin)))

(export 'bin-child)

;;; --- End of file gtk3.bin.lisp ----------------------------------------------
