;;; ----------------------------------------------------------------------------
;;; gtk3.radio-tool-button.lisp
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
;;; GtkRadioToolButton
;;;
;;;     A toolbar item that contains a radio button
;;;
;;; Types and Values
;;;
;;;     GtkRadioToolButton
;;;
;;; Functions
;;;
;;;     gtk_radio_tool_button_new
;;;     gtk_radio_tool_button_new_from_stock
;;;     gtk_radio_tool_button_new_from_widget
;;;     gtk_radio_tool_button_new_with_stock_from_widget
;;;     gtk_radio_tool_button_get_group
;;;     gtk_radio_tool_button_set_group
;;;
;;; Properties
;;;
;;;     group
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
;;;                             ╰── GtkToggleToolButton
;;;                                 ╰── GtkRadioToolButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRadioToolButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioToolButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkRadioToolButton" radio-tool-button
  (:superclass toggle-tool-button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable"
                 "GtkActionable")
    :type-initializer "gtk_radio_tool_button_get_type")
  ((group
    radio-tool-button-group
    "group" "GtkRadioToolButton" nil t)))

#+liber-documentation
(setf (documentation 'radio-tool-button 'type)
 "@version{2024-1-2}
  @begin{short}
    The @class{gtk:radio-tool-button} widget is a @class{gtk:tool-item} that
    contains a radio button, that is, a button that is part of a group of toggle
    buttons where only one button can be active at a time.
  @end{short}

  Use the @fun{gtk:radio-tool-button-new} function to create a new radio tool
  button. Use the @fun{gtk:radio-tool-button-new-from-widget} function to create
  a new radio tool button that is part of the same group as an existing radio
  tool button.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:radio-tool-button} implementation has a single CSS node with
    name @code{toolbutton}.
  @end{dictionary}
  @see-constructor{gtk:radio-tool-button-new}
  @see-constructor{gtk:radio-tool-button-new-from-stock}
  @see-constructor{gtk:radio-tool-button-new-from-widget}
  @see-constructor{gtk:radio-tool-button-new-with-stock-from-widget}
  @see-slot{gtk:radio-tool-button-group}
  @see-class{gtk:tool-item}
  @see-class{gtk:tool-button}
  @see-class{gtk:toggle-tool-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group" 'radio-tool-button) t)
 "The @code{group} property of type @code{gtk:radio-tool-button} (Write) @br{}
  Sets a new group for a radio tool button.")

#+liber-documentation
(setf (liber:alias-for-function 'radio-tool-button-group)
      "Accessor"
      (documentation 'radio-tool-button-group 'function)
 "@version{#2023-3-22}
  @begin{short}
    Accessor of the @slot[gtk:radio-tool-button]{group} slot of the
    @class{gtk:radio-tool-button} class.
  @end{short}
  Sets a new group for a radio tool button.
  @see-class{gtk:radio-tool-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_tool_button_new" radio-tool-button-new)
    (g:object tool-item)
 #+liber-documentation
 "@version{#2023-3-22}
  @argument[group]{an existing @class{gtk:radio-tool-button} group, or
    @code{nil} if you are creating a new group}
  @return{The new @class{gtk:radio-tool-button}.}
  @begin{short}
    Creates a new radio tool button, adding it to @arg{group}.
  @end{short}
  @see-class{gtk:radio-tool-button}"
  (group (g:slist-t (g:object radio-button))))

(export 'radio-tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_tool_button_new_from_stock"
           radio-tool-button-new-from-stock) (g:object tool-item)
 #+liber-documentation
 "@version{#2023-3-22}
  @argument[group]{an existing @class{gtk:radio-tool-button} group, or
    @code{nil} if you are creating a new group}
  @argument[stock]{a string with the name of a stock item}
  @return{The new @class{gtk:radio-tool-button} widget.}
  @begin{short}
    Creates a new radio tool button, adding it to @arg{group}.
  @end{short}
  The new radio tool button will contain an icon and label from the stock item
  indicated by @arg{stock}.
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-tool-button-new-from-stock} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:radio-tool-button-new} function instead.
  @end{dictionary}
  @see-class{gtk:radio-tool-button}
  @see-function{gtk:radio-tool-button-new}"
  (group (g:slist-t (g:object radio-button)))
  (stock :string))

(export 'radio-tool-button-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_from_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_tool_button_new_from_widget"
               radio-tool-button-new-from-widget) (g:object tool-item)
 #+liber-documentation
 "@version{2024-1-2}
  @argument[group]{an existing @class{gtk:radio-tool-button} group, or
    @code{nil}}
  @return{The new @class{gtk:radio-tool-button} widget.}
  @begin{short}
    Creates a new radio tool button adding it to the same group as @arg{group}.
  @end{short}
  @see-class{gtk:radio-tool-button}"
  (group (g:object radio-tool-button)))

(export 'radio-tool-button-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_new_with_stock_from_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_tool_button_new_with_stock_from_widget"
               radio-tool-button-new-with-stock-from-widget)
    (g:object tool-item)
 #+liber-documentation
 "@version{#2023-3-22}
  @argument[group]{an existing @class{gtk:radio-tool-button} widget}
  @argument[stock]{a string with the name of a stock item}
  @return{The new @class{gtk:radio-tool-button} widget.}
  @begin{short}
    Creates a new radio tool button adding it to the same group as @arg{group}.
  @end{short}
  The new radio tool button will contain an icon and label from the stock item
  indicated by @arg{stock}.
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-tool-button-new-with-stock-from-widget} function has
    been deprecated since version 3.10 and should not be used in newly written
    code. Use the @fun{gtk:radio-tool-button-new-from-widget} function instead.
  @end{dictionary}
  @see-class{gtk:radio-tool-button}
  @see-function{gtk:radio-tool-button-new-from-widget}"
  (group (g:object radio-tool-button))
  (stock :string))

(export 'radio-tool-button-new-with-stock-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_get_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_tool_button_get_group" radio-tool-button-get-group)
    (g:slist-t (g:object radio-tool-button) :free-from-foreign nil)
 #+liber-documentation
 "@version{#2023-3-22}
  @argument[button]{a @class{gtk:radio-tool-button} widget}
  @return{The group @arg{button} belongs to.}
  @short{Returns the radio button group @arg{button} belongs to.}
  @see-class{gtk:radio-tool-button}"
  (button (g:object radio-tool-button)))

(export 'radio-tool-button-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_tool_button_set_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_tool_button_set_group" radio-tool-button-set-group)
    :void
 #+liber-documentation
 "@version{#2023-3-22}
  @argument[button]{a @class{gtk:radio-tool-button} widget}
  @argument[group]{an existing @class{gtk:radio-tool-button} group}
  @begin{short}
    Adds @arg{button} to @arg{group}, removing it from the group it belonged to
    before.
  @end{short}
  @see-class{gtk:radio-tool-button}"
  (button (g:object radio-tool-button))
  (group (g:slist-t (g:object radio-tool-button))))

(export 'radio-tool-button-set-group)

;;; --- End of file gtk3.radio-tool-button.lisp --------------------------------
