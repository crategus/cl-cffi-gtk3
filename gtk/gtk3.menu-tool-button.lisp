;;; ----------------------------------------------------------------------------
;;; gtk3.menu-tool-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkMenuToolButton
;;;
;;;     A GtkToolItem containing a button with an additional dropdown menu
;;;
;;; Types and Values
;;;
;;;     GtkMenuToolButton
;;;
;;; Functions
;;;
;;;     gtk_menu_tool_button_new
;;;     gtk_menu_tool_button_new_from_stock
;;;     gtk_menu_tool_button_set_menu                      Accessor
;;;     gtk_menu_tool_button_get_menu                      Accessor
;;;     gtk_menu_tool_button_set_arrow_tooltip_text
;;;     gtk_menu_tool_button_set_arrow_tooltip_markup
;;;
;;; Properties
;;;
;;;     menu
;;;
;;; Signals
;;;
;;;     show-menu
;;;
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ╰── GtkToolButton
;;;                             ╰── GtkMenuToolButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuToolButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuToolButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuToolButton" menu-tool-button
  (:superclass tool-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable"
                "GtkActionable")
   :type-initializer "gtk_menu_tool_button_get_type")
  ((menu
    menu-tool-button-menu
    "menu" "GtkMenu" t t)))

#+liber-documentation
(setf (documentation 'menu-tool-button 'type)
 "@version{#2023-2-27}
  @begin{short}
    A @sym{gtk:menu-tool-button} widget is a @class{gtk:tool-item} that
    contains a button and a small additional button with an arrow.
  @end{short}
  When clicked, the arrow button pops up a dropdown menu.

  Use the @fun{gtk:menu-tool-button-new} function to create a new
  @sym{gtk:menu-tool-button} widget. Use the
  @fun{gtk:menu-tool-button-new-from-stock} function to create a new
  @sym{gtk:menu-tool-button} widget containing a stock item.
  @begin[GtkMenuToolButton as GtkBuildable]{dictionary}
    The @sym{gtk:menu-tool-button} implementation of the @class{gtk:buildable}
    interface supports adding a menu by specifying \"menu\" as the \"type\"
    attribute of a @code{<child>} element.

    @b{Example:} A UI definition fragment with menus
    @begin{pre}
<object class=\"GtkMenuToolButton\">
  <child type=\"menu\">
    <object class=\"GtkMenu\"/>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"show-menu\" signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      The signal is emitted before the menu is shown. It can be used to populate
      the menu on demand, using the @fun{gtk:menu-tool-button-menu} function.
      Note that even if you populate the menu dynamically in this way, you must
      set an empty menu on the @sym{gtk:menu-tool-button} widget beforehand,
      since the arrow is made insensitive if the menu is not set.
      @begin[code]{table}
        @entry[button]{The @sym{gtk:menu-tool-button} widget on which the
          signal is emitted.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:menu-tool-button-new}
  @see-constructor{gtk:menu-tool-button-new-from-stock}
  @see-slot{gtk:menu-tool-button-menu}
  @see-class{gtk:tool-item}
  @see-class{gtk:tool-button}
  @see-class{gtk:toolbar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu" 'menu-tool-button) t)
 "The @code{menu} property of type @class{gtk:menu-tool-button} (Read / Write)
  @br{}
  The dropdown menu.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-tool-button-menu)
      "Accessor"
      (documentation 'menu-tool-button-menu 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:menu-tool-button-menu object) => menu}
  @syntax[]{(setf (gtk:menu-tool-button-menu object) menu)}
  @argument[object]{a @class{gtk:menu-tool-button} widget}
  @argument[menu]{a @class{gtk:menu} associated with @arg{button}}
  @begin{short}
    Accessor of the @slot[gtk:menu-tool-button]{menu} slot of the
    @class{gtk:menu-tool-button} class.
  @end{short}
  The @sym{gtk:menu-tool-button-menu} function gets the menu associated with
  @arg{button}. The @sym{(setf gtk:menu-tool-button-menu)} function sets the
  menu that is popped up when the user clicks on the arrow. If @arg{menu} is
  @code{nil}, the arrow button becomes insensitive.
  @see-class{gtk:menu-tool-button}
  @see-class{gtk:menu}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_new ()
;;; ----------------------------------------------------------------------------

(defun menu-tool-button-new (icon-widget label)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[icon-widget]{a @class{gtk:widget} object that will be used as icon
    widget, or @code{nil}}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @return{The new @class{gtk:menu-tool-button} widget.}
  @begin{short}
    Creates a new menu tool button using @arg{icon-widget} as icon and
    @arg{label} as label.
  @end{short}
  @see-class{gtk:menu-tool-button}
  @see-function{gtk:menu-tool-button-new-from-stock}"
  (let ((button (make-instance 'menu-tool-button)))
    (when icon-widget
      (setf (tool-button-icon-widget button) icon-widget))
    (when label
      (setf (tool-button-label button) label))
    button))

(export 'menu-tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline menu-tool-button-new-from-stock))

(defun menu-tool-button-new-from-stock (stock-id)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[stock-id]{a string with the name of a stock item}
  @return{The new @class{gtk:menu-tool-button} widget.}
  @begin{short}
    Creates a new menu tool button.
  @end{short}
  The new menu tool button will contain an icon and label from the stock item
  indicated by @arg{stock-id}.
  @begin[Warning]{dictionary}
    The @sym{gtk:menu-tool-button-new-from-stock} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:menu-tool-button-new} function instead.
  @end{dictionary}
  @see-class{gtk:menu-tool-button}
  @see-function{gtk:menu-tool-button-new}"
  (make-instance 'menu-tool-button
                 :stock-id stock-id))

(export 'menu-tool-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_set_arrow_tooltip_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_tool_button_set_arrow_tooltip_text"
           menu-tool-button-set-arrow-tooltip-text) :void
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[button]{a @class{gtk:menu-tool-button} widget}
  @argument[text]{a string with the text to be used as tooltip text for
    @arg{button}'s arrow button}
  @begin{short}
    Sets the tooltip text to be used as tooltip for the arrow button which pops
    up the menu.
  @end{short}
  See the @fun{gtk:tool-item-set-tooltip-text} function for setting a tooltip
  on the whole menu tool button.
  @see-class{gtk:menu-tool-button}
  @see-function{gtk:tool-item-set-tooltip-text}"
  (button (g:object menu-tool-button))
  (text :string))

(export 'menu-tool-button-set-arrow-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_tool_button_set_arrow_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_tool_button_set_arrow_tooltip_markup"
           menu-tool-button-set-arrow-tooltip-markup) :void
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[button]{a @class{gtk:menu-tool-button} widget}
  @argument[markup]{a string with the markup text to be used as tooltip text
    for @arg{button}'s arrow button}
  @begin{short}
    Sets the tooltip markup text to be used as tooltip for the arrow button
    which pops up the menu.
  @end{short}
  See the @fun{gtk:tool-item-set-tooltip-text} function for setting a tooltip
  on the whole menu tool button.
  @see-class{gtk:menu-tool-button}
  @see-function{gtk:tool-item-set-tooltip-text}"
  (button (g:object menu-tool-button))
  (markup :string))

(export 'menu-tool-button-set-arrow-tooltip-markup)

;;; --- End of file gtk3.menu-tool-button.lisp ---------------------------------
