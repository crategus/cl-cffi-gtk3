;;; ----------------------------------------------------------------------------
;;; gtk3.recent-chooser-menu.lisp
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
;;; GtkRecentChooserMenu
;;;
;;;     Displays recently used files in a menu
;;;
;;; Types and Values
;;;
;;;     GtkRecentChooserMenu
;;;
;;; Functions
;;;
;;;     gtk_recent_chooser_menu_new
;;;     gtk_recent_chooser_menu_new_for_manager
;;;     gtk_recent_chooser_menu_get_show_numbers
;;;     gtk_recent_chooser_menu_set_show_numbers
;;;
;;; Properties
;;;
;;;     show-numbers
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkMenuShell
;;;                     ╰── GtkMenu
;;;                         ╰── GtkRecentChooserMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentChooserMenu implements AtkImplementorIface, GtkBuildable,
;;;     GtkRecentChooser and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserMenu
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkRecentChooserMenu" recent-chooser-menu
  (:superclass menu
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkRecentChooser"
                "GtkActivatable")
   :type-initializer "gtk_recent_chooser_menu_get_type")
  ((show-numbers
    recent-chooser-menu-show-numbers
    "show-numbers" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'recent-chooser-menu 'type)
 "@version{#2023-3-24}
  @begin{short}
    The @class{gtk:recent-chooser-menu} widget is a widget suitable for
    displaying recently used files inside a menu.
  @end{short}
  It can be used to set a sub-menu of a @class{gtk:menu-item} widget using the
  @fun{gtk:menu-item-submenu} function, or as the menu of a
  @class{gtk:menu-tool-button} widget.

  Note that the @class{gtk:recent-chooser-menu} widget does not have any methods
  of its own. Instead, you should use the functions that work on a
  @class{gtk:recent-chooser} widget.

  Note also that the @class{gtk:recent-chooser-menu} widget does not support
  multiple filters, as it has no way to let the user choose between them as the
  @class{gtk:recent-chooser-widget} and @class{gtk:recent-chooser-dialog}
  widgets do. Thus using the @fun{gtk:recent-chooser-add-filter} function on a
  @class{gtk:recent-chooser-menu} widget will yield the same effects as using
  the @fun{gtk:recent-chooser-filter} function, replacing any currently set
  filter with the supplied filter. The @fun{gtk:recent-chooser-remove-filter}
  function will remove any currently set @class{gtk:recent-filter} object and
  will unset the current filter. The @fun{gtk:recent-chooser-list-filters}
  function will return a list containing a single @class{gtk:recent-filter}
  object.
  @see-constructor{gtk:recent-chooser-menu-new}
  @see-constructor{gtk:recent-chooser-menu-for-manager}
  @see-class{gtk:recent-chooser}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:recent-chooser-menu-show-numbers -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-numbers"
                                               'recent-chooser-menu) t)
 "The @code{show-numbers} property of type @code{:boolean} (Read / Write) @br{}
  Whether the first ten items in the menu should be prepended by a number
  acting as a unique mnemonic. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-chooser-menu-show-numbers)
      "Accessor"
      (documentation 'recent-chooser-menu-show-numbers 'function)
 "@version{#2023-3-24}
  @syntax{(gtk:recent-chooser-menu-show-numbers object) => show-numbers}
  @syntax{(setf (gtk:recent-chooser-menu-show-numbers object) show-numbers)}
  @argument[obect]{a @class{gtk:recent-chooser-menu} widget}
  @argument[show-numbers]{a boolean whether to show numbers}
  @begin{short}
    Accessor of the @slot[gtk:recent-chooser-menu-show-numbers]{show-numbers}
    slot of the @class{gtk:recent-chooser-menu} class.
  @end{short}
  The @fun{gtk:recent-chooser-menu-show-numbers} function returns whether a
  number should be added to the items of menu. The
  @setf{gtk:recent-chooser-menu-show-numbers} function sets whether a number
  should be added to the items of menu.

  The numbers are shown to provide a unique character for a mnemonic to be used
  inside ten menu label of the item. Only the first the items get a number to
  avoid clashes.
  @see-class{gtk:recent-chooser-menu}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline recent-chooser-menu-new))

(defun recent-chooser-menu-new ()
 #+liber-documentation
 "@version{#2023-3-24}
  @return{A new @class{gtk:recent-chooser-menu} widget.}
  @begin{short}
    Creates a new @class{gtk:recent-chooser-menu} widget.
  @end{short}
  This kind of widget shows the list of recently used resources as a menu,
  each item as a menu item. Each item inside the menu might have an icon,
  representing its MIME type, and a number, for mnemonic access.

  This widget implements the @class{gtk:recent-chooser} interface.

  This widget creates its own @class{gtk:recent-manager} object. See the
  @fun{gtk:recent-chooser-menu-new-for-manager} function to know how to create
  a @class{gtk:recent-chooser-menu} widget bound to another
  @class{gtk:recent-manager} object.
  @see-class{gtk:recent-chooser-menu}
  @see-class{gtk:recent-chooser}
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-chooser-menu-new-for-manager}"
  (make-instance 'recent-chooser-menu))

(export 'recent-chooser-menu-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_menu_new_for_manager ()
;;; ----------------------------------------------------------------------------

(declaim (inline recent-chooser-menu-new-for-manager))

(defun recent-chooser-menu-new-for-manager (manager)
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @return{A new @class{gtk:recent-chooser-menu} widget, bound to @arg{manager}.}
  @begin{short}
    Creates a new @class{gtk:recent-chooser-menu} widget using @arg{manager} as
    the underlying recently used resources manager.
  @end{short}

  This is useful if you have implemented your own recent manager, or if you
  have a customized instance of a @class{gtk:recent-manager} object or if you
  wish to share a common @class{gtk:recent-manager} object among multiple
  @class{gtk:recent-chooser} widgets.
  @see-class{gtk:recent-chooser-menu}
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-chooser-menu-new}"
  (make-instance 'recent-chooser-menu
                 :recent-manager manager))

(export 'recent-chooser-menu-new-for-manager)

;;; --- End of file gtk3.recent-chooser-menu.lisp ------------------------------
