;;; ----------------------------------------------------------------------------
;;; gtk3.tool-button.lisp
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
;;; GtkToolButton
;;;
;;;     A GtkToolItem subclass that displays buttons
;;;
;;; Types and Values
;;;
;;;     GtkToolButton
;;;
;;; Functions
;;;
;;;     gtk_tool_button_new
;;;     gtk_tool_button_new_from_stock
;;;     gtk_tool_button_set_label                          Accessor
;;;     gtk_tool_button_get_label                          Accessor
;;;     gtk_tool_button_set_use_underline                  Accessor
;;;     gtk_tool_button_get_use_underline                  Accessor
;;;     gtk_tool_button_set_stock_id                       Accessor
;;;     gtk_tool_button_get_stock_id                       Accessor
;;;     gtk_tool_button_set_icon_name                      Accessor
;;;     gtk_tool_button_get_icon_name                      Accessor
;;;     gtk_tool_button_set_icon_widget                    Accessor
;;;     gtk_tool_button_get_icon_widget                    Accessor
;;;     gtk_tool_button_set_label_widget                   Accessor
;;;     gtk_tool_button_get_label_widget                   Accessor
;;;
;;; Properties
;;;
;;;     icon-name
;;;     icon-widget
;;;     label
;;;     label-widget
;;;     stock-id
;;;     use-underline
;;;
;;; Style Properties
;;;
;;;     icon-spacing
;;;
;;; Signals
;;;
;;;     clicked
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
;;;                             ├── GtkMenuToolButton
;;;                             ╰── GtkToggleToolButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolButton implements AtkImplementorIface, GtkBuildable,
;;;      GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToolButton" tool-button
  (:superclass tool-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable")
    :type-initializer "gtk_tool_button_get_type")
  ((icon-name
    tool-button-icon-name
    "icon-name" "gchararray" t t)
   (icon-widget
    tool-button-icon-widget
    "icon-widget" "GtkWidget" t t)
   (label
    tool-button-label
    "label" "gchararray" t t)
   (label-widget
    tool-button-label-widget
    "label-widget" "GtkWidget" t t)
   (stock-id
    tool-button-stock-id
    "stock-id" "gchararray" t t)
   (use-underline
    tool-button-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'tool-button 'type)
 "@version{#2023-2-27}
  @begin{short}
    The @sym{gtk:tool-button} widgets are @class{gtk:tool-item} widgets
    containing buttons.
  @end{short}
  Use the @fun{gtk:tool-button-new} function to create a new
  @sym{gtk:tool-button} widget.

  The label of a @sym{gtk:tool-button} widget is determined by the
  @code{label-widget}, @code{label}, @code{icon-name}, and @code{stock-id}
  properties. If the @code{label-widget} property is non-@code{nil}, then that
  widget is used as the label. Otherwise, if the @code{label} property is
  non-@code{nil}, that string is used as the label. Otherwise, if the
  @code{stock-id} property is non-@code{nil}, the label is determined by the
  stock item. Otherwise, the button does not have a label.

  The icon of a @sym{gtk:tool-button} widget is determined by the
  @code{icon-widget}, @code{icon-name}, and @code{stock-id} properties. If the
  @code{icon-widget} property is non-@code{nil}, then that widget is used as the
  icon. Otherwise, if the @code{icon-name} or @code{stock-id} properties are
  non-@code{nil}, the icons are determined by the icon name or the stock item.
  Otherwise, the button does not have a icon.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:tool-button} implementation has a single CSS node with name
    @code{toolbutton}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[icon-spacing]{entry}
        The @code{icon-spacing} style property of type @code{:int}
        (Read / Write) @br{}
        Spacing in pixels between the icon and label. @br{}
        Allowed values: >= 0 @br{}
        Default value: 3
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"clicked\" signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      The signal is emitted when the tool button is clicked with the mouse or
      activated with the keyboard.
      @begin[code]{table}
        @entry[button]{The @sym{gtk:tool-button} widget that emitted the
          signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:menu-tool-button-new}
  @see-constructor{gtk:menu-tool-button-new-from-stock}
  @see-slot{gtk:tool-button-icon-name}
  @see-slot{gtk:tool-button-icon-widget}
  @see-slot{gtk:tool-button-label}
  @see-slot{gtk:tool-button-label-widget}
  @see-slot{gtk:tool-button-stock-id}
  @see-slot{gtk:tool-button-use-underline}
  @see-class{gtk:tool-item}
  @see-class{gtk:toolbar}
  @see-class{gtk:menu-tool-button}
  @see-class{gtk:toggle-tool-button}
  @see-class{gtk:radio-tool-button}
  @see-class{gtk:separator-tool-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tool-button-icon-name --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'tool-button) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the themed icon displayed on the item. This property only has an
  effect if not overridden by the @code{label}, @code{icon-widget} or
  @code{stock-id} properties. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-icon-name)
      "Accessor"
      (documentation 'tool-button-icon-name 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-button-icon-name object) => icon-name}
  @syntax[]{(setf (gtk:tool-button-icon-name object) icon-name)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[icon-name]{a string with the name of the themed icon}
  @begin{short}
    Accessor of the @slot[gtk:tool-button]{icon-name} slot of the
    @class{gtk:tool-button} class.
  @end{short}
  The @sym{gtk:tool-button-icon-name} function returns the name of the themed
  icon for the tool button. The @sym{(setf gtk:tool-button-icon-name)} function
  sets the icon for the tool button from a named themed icon.

  See the docs for the @class{gtk:icon-theme} object for more details. The
  @slot[gtk:tool-button]{icon-name} property only has an effect if not
  overridden by non-@code{nil} @slot[gtk:tool-button]{label},
  @slot[gtk:tool-button]{icon-widget} and @slot[gtk:tool-button]{stock-id}
  properties.
  @see-class{gtk:tool-button}
  @see-class{gtk:icon-theme}
  @see-function{gtk:tool-button-label}
  @see-function{gtk:tool-button-icon-widget}
  @see-function{gtk:tool-button-stock-id}")

;;; --- tool-button-icon-widget ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-widget" 'tool-button) t)
 "The @code{icon-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  Icon widget to display in the item.")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-icon-widget)
      "Accessor"
      (documentation 'tool-button-icon-widget 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-button-icon-widget object) => icon-widget}
  @syntax[]{(setf (gtk:tool-button-icon-widget object) icon-widget)}
  @argument[button]{a @class{gtk:tool-button} widget}
  @argument[icon-widget]{a @class{gtk:widget} object used as icon, or
    @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tool-button]{icon-widget} slot of the
    @class{gtk:tool-button} class.
  @end{short}
  The @sym{gtk:tool-button-icon-widget} function returns the widget used as
  icon widget on the tool button. The @sym{(setf gtk:tool-button-icon-widget)}
  function sets icon as the widget used as icon on the tool button.

  If the @arg{icon-widget} argument is @code{nil} the icon is determined by the
  @slot[gtk:tool-button]{stock-id} property. If the
  @slot[gtk:tool-button]{stock-id} property is also @code{nil}, the tool button
  will not have an icon.
  @see-class{gtk:tool-button}
  @see-class{gtk:widget}
  @see-function{gtk:tool-button-stock-id}")

;;; --- tool-button-label ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'tool-button) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  Text to show in the tool item. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-label)
      "Accessor"
      (documentation 'tool-button-label 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-button-label object) => label}
  @syntax[]{(setf (gtk:tool-button-label object) label)}
  @argument[button]{a @class{gtk:tool-button} widget}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tool-button]{label} slot of the
    @class{gtk:tool-button} class.
  @end{short}
  The @sym{gtk:tool-button-label} function returns the label used by the tool
  button, or @code{nil} if the tool button does not have a label or uses a label
  from a stock item. The @sym{(setf gtk:tool-button-label)} function sets the
  label.

  The @slot[gtk:tool-button]{label} property only has an effect if not
  overridden by a non-@code{nil} @slot[gtk:tool-button]{label-widget} property.
  If both the @slot[gtk:tool-button]{label-widget} and
  @slot[gtk:tool-button]{label} properties are @code{nil}, the label is
  determined by the @slot[gtk:tool-button]{stock-id} property. If the
  @slot[gtk:tool-button]{stock-id} property is also @code{nil}, the tool
  button will not have a label.
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-label-widget}
  @see-function{gtk:tool-button-stock-id}")

;;; --- tool-button-label-widget -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget" 'tool-button) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  Widget to use as the item label.")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-label-widget)
      "Accessor"
      (documentation 'tool-button-label-widget 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-button-label-widget object) => label-widget}
  @syntax[]{(setf (gtk:tool-button-label-widget object) label-widget)}
  @argument[button]{a @class{gtk:tool-button} widget}
  @argument[label-widget]{a @class{gtk:widget} object used as label, or
    @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tool-button]{label-widget} slot of the
    @class{gtk:tool-button} class.
  @end{short}
  The @sym{gtk:tool-button-label-widget} function returns the widget used as
  label on the tool button. The @sym{(setf gtk:tool-button-label-widget)}
  function sets @arg{label-widget} as the widget that will be used as the label
  for the tool button.

  If the @arg{label-widget} argument is @code{nil} the
  @slot[gtk:tool-button]{label} property is used as label. If
  @slot[gtk:tool-button]{label} is also @code{nil}, the label in the stock item
  determined by the @slot[gtk:tool-button]{stock-id} property is used as label.
  If @slot[gtk:tool-button]{stock-id} is also @code{nil}, @arg{button} does not
  have a label.
  @see-class{gtk:tool-button}
  @see-class{gtk:widget}
  @see-function{gtk:tool-button-label}
  @see-function{gtk:tool-button-stock-id}")

;;; --- tool-button-stock-id ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock-id" 'tool-button) t)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock icon displayed on the item. @br{}
  @em{Warning:} The @code{stock-id} property has been deprecated since version
  3.10 and should not be used in newly written code. Use the @code{icon-name}
  property instead. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-stock-id)
      "Accessor"
      (documentation 'tool-button-stock-id 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-button-stock-id object) => stock-id}
  @syntax[]{(setf (gtk:tool-button-stock-id object) stock-id)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[stock-id]{a string with the name of a stock item, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:tool-button]{stock-id} slot of the
    @class{gtk:tool-button} class.
  @end{short}
  The @sym{gtk:tool-button-stock-id} function returns the name of the stock
  item. The @sym{(setf gtk:tool-button-stock-id)} function sets the name.

  The @slot[gtk:tool-button]{stock-id} property only has an effect if not
  overridden by non-@code{nil} @slot[gtk:tool-button]{label},
  @slot[gtk:tool-button]{icon-name}, and @slot[gtk:tool-button]{icon-widget}
  properties.
  @begin[Warning]{dictionary}
    The @sym{gtk:tool-button stock-id} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:tool-button-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-label}
  @see-function{gtk:tool-button-icon-widget}
  @see-function{gtk:tool-button-icon-name}")

;;; --- tool-button-use-underline ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'tool-button) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the label property indicates that the next character
  should be used for the mnemonic accelerator key in the overflow menu. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-use-underline)
      "Accessor"
      (documentation 'tool-button-use-underline 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-button-use-underline object) => use-underline}
  @syntax[]{(setf (gtk:tool-button-use-underline) use-underline)}
  @argument[button]{a @class{gtk:tool-button} widget}
  @argument[use-underline]{a boolean whether the button label has the form
    \"_Open\"}
  @begin{short}
    Accessor of the @slot[gtk:tool-button]{use-underline} slot of the
    @class{gtk:tool-button} class.
  @end{short}
  The @sym{gtk:tool-button-use-underline} function returns whether underscores
  in the @slot[gtk:tool-button]{label} property are used as mnemonics on menu
  items on the overflow menu.

  If set, an underline in the @slot[gtk:tool-button]{label} property indicates
  that the next character should be used for the mnemonic accelerator key in
  the overflow menu.

  For example, if the label property is \"_Open\" and @arg{use-underline} is
  @em{true}, the label on the tool button will be \"Open\" and the item on the
  overflow menu will have an underlined 'O'.

  Labels shown on tool buttons never have mnemonics on them. This property
  only affects the menu item on the overflow menu.
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_new ()
;;; ----------------------------------------------------------------------------

(defun tool-button-new (icon label)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[icon]{a @class{gtk:widget} object that will be used as the button
    contents, or @code{nil}}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @return{A new @class{gtk:tool-button} widget.}
  @begin{short}
    Creates a new tool button using @arg{icon} as contents and @arg{label} as
    label.
  @end{short}
  @see-class{gtk:tool-button}"
  (let ((button (make-instance 'tool-button)))
    (when icon
      (setf (tool-button-icon-widget button) icon))
    (when label
      (setf (tool-button-label button) label))
    button))

(export 'tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(defun tool-button-new-from-stock (stock)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[stock]{a string with the name of the stock item}
  @return{A new @class{gtk:tool-button} widget.}
  @begin{short}
    Creates a new tool button containing the image and text from a stock item.
  @end{short}
  It is an error if the @arg{stock} argument is not a name of a stock item.
  @begin[Warning]{dictionary}
    The @sym{gtk:tool-button-new-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:tool-button-new} function together with the
    @fun{gtk:image-new-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-new}
  @see-function{gtk:image-new-from-icon-name}"
  (make-instance 'tool-button
                 :stock-id stock))

(export 'tool-button-new-from-stock)

;;; --- End of file gtk3.tool-button.lisp --------------------------------------
