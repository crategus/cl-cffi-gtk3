;;; ----------------------------------------------------------------------------
;;; gtk3.separator-menu-item.lisp
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
;;; GtkSeparatorMenuItem
;;;
;;;     A separator used in menus
;;;
;;; Types and Values
;;;
;;;     GtkSeparatorMenuItem
;;;
;;; Functions
;;;
;;;     gtk_separator_menu_item_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkSeparatorMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSeparatorMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSeparatorMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSeparatorMenuItem" separator-menu-item
  (:superclass menu-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_separator_menu_item_get_type")
  nil)

#+liber-documentation
(setf (documentation 'separator-menu-item 'type)
 "@version{#2023-2-27}
  @begin{short}
    The @sym{gtk:separator-menu-item} widget is a separator used to group items
    within a menu.
  @end{short}
  It displays a horizontal line with a shadow to make it appear sunken into the
  interface.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:separator-menu-item} implementation has a single CSS node with
    name @code{separator}.
  @end{dictionary}
  @see-constructor{gtk:separator-menu-item-new}
  @see-class{gtk:menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline separator-menu-item-new))

(defun separator-menu-item-new ()
 #+liber-documentation
 "@version{#2023-2-27}
  @return{A new @class{gtk:separator-menu-item} widget.}
  @begin{short}
    Creates a new separator menu item.
  @end{short}
  @see-class{gtk:separator-menu-item}"
  (make-instance 'separator-menu-item))

(export 'separator-menu-item-new)

;;; --- End of file gtk3.separator-menu-item.lisp ------------------------------
