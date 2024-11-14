;;; ----------------------------------------------------------------------------
;;; gtk3.tearoff-menu-item.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkTearoffMenuItem
;;;
;;;     A menu item used to tear off and reattach its menu
;;;
;;; Types and Values
;;;
;;;     GtkTearoffMenuItem
;;;
;;; Functions
;;;
;;;     gtk_tearoff_menu_item_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkTearoffMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTearoffMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTearoffMenuItem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTearoffMenuItem" tearoff-menu-item
  (:superclass menu-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_tearoff_menu_item_get_type")
  nil)

#+liber-documentation
(setf (documentation 'tearoff-menu-item 'type)
 "@version{#2023-3-28}
  @begin{short}
    The @class{gtk:tearoff-menu-item} widget is a special @class{gtk:menu-item}
    widget which is used to tear off and reattach its menu.
  @end{short}

  When its menu is shown normally, the tearoff menu is drawn as a dotted line
  indicating that the menu can be torn off. Activating it causes its menu to be
  torn off and displayed in its own window as a tearoff menu.

  When its menu is shown as a tearoff menu, the tearoff menu is drawn as a
  dotted line which has a left pointing arrow graphic indicating that the
  tearoff menu can be reattached. Activating it will erase the tearoff menu
  window.
  @begin[Warning]{dictionary}
    The @class{gtk:tearoff-menu-item} widget is deprecated since GTK 3.4 and
    should not be used in newly written code. Menus are not meant to be torn
    around.
  @end{dictionary}
  @see-class{gtk:menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tearoff_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline tearoff-menu-item-new))

(defun tearoff-menu-item-new ()
 #+liber-documentation
 "@version{#2023-3-28}
  @return{A new @class{gtk:tearoff-menu-item} widget.}
  @short{Creates a new tearoff menu.}
  @begin[Warning]{dictionary}
    The @fun{gtk:tearoff-menu-item-new} function has been deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:tearoff-menu-item}"
  (make-instance 'tearoff-menu-item))

(export 'tearoff-menu-item-new)

;;; --- End of file gtk3.tearoff-menu-item.lisp --------------------------------
