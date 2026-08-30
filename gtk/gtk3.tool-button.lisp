;;; ----------------------------------------------------------------------------
;;; gtk3.tool-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GtkToolButton
;;;
;;;     A GtkToolItem subclass that displays buttons
;;;
;;; Types and Values
;;;
;;;     GtkToolButton
;;;
;;; Accessors
;;;
;;;     gtk_tool_button_set_icon_name
;;;     gtk_tool_button_get_icon_name
;;;     gtk_tool_button_set_icon_widget
;;;     gtk_tool_button_get_icon_widget
;;;     gtk_tool_button_set_label
;;;     gtk_tool_button_get_label
;;;     gtk_tool_button_set_label_widget
;;;     gtk_tool_button_get_label_widget
;;;     gtk_tool_button_set_stock_id
;;;     gtk_tool_button_get_stock_id
;;;     gtk_tool_button_set_use_underline
;;;     gtk_tool_button_get_use_underline
;;;
;;; Functions
;;;
;;;     gtk_tool_button_new
;;;     gtk_tool_button_new_from_stock
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
;;; GtkToolButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkToolButton" tool-button
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
 "@version{2026-05-29}
  @begin{short}
    The @class{gtk:tool-button} widgets are @class{gtk:tool-item} widgets
    containing buttons.
  @end{short}
  Use the @fun{gtk:tool-button-new} function to create a new
  @class{gtk:tool-button} widget.

  The label of a @class{gtk:tool-button} widget is determined by the
  @slot[gtk:tool-button]{label-widget}, @slot[gtk:tool-button]{label},
  @slot[gtk:tool-button]{icon-name}, and @slot[gtk:tool-button]{stock-id}
  properties. If the @slot[gtk:tool-button]{label-widget} property is
  non-@code{nil}, then that widget is used as the label. Otherwise, if the
  @slot[gtk:tool-button]{label} property is non-@code{nil}, that string is used
  as the label. Otherwise, if the @slot[gtk:tool-button]{stock-id} property is
  non-@code{nil}, the label is determined by the stock item. Otherwise, the
  button does not have a label.

  The icon of a @class{gtk:tool-button} widget is determined by the
  @slot[gtk:tool-button]{icon-widget}, @slot[gtk:tool-button]{icon-name}, and
  @slot[gtk:tool-button]{stock-id} properties. If the
  @slot[gtk:tool-button]{icon-widget} property is non-@code{nil}, then that
  widget is used as the icon. Otherwise, if the
  @slot[gtk:tool-button]{icon-name} or @slot[gtk:tool-button]{stock-id}
  properties are non-@code{nil}, the icons are determined by the icon name or
  the stock item. Otherwise, the button does not have a icon.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:tool-button} implementation has a single CSS node with name
    @code{toolbutton}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[tool-button:icon-spacing]{property}
      The @code{icon-spacing} style property of type @code{:int} (Read / Write)
      @br{}
      The sSpacing in pixels between the icon and label. @br{}
      Allowed values: >= 0 @br{}
      Default value: 3
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[tool-button::clicked]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:tool-button} widget that emitted the
          signal.}
      @end{simple-table}
      The signal is emitted when the tool button is clicked with the mouse or
      activated with the keyboard.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:menu-tool-button-new}
  @see-constructor{gtk:menu-tool-button-new-from-stock}
  @see-slot{gtk:tool-button-icon-name}
  @see-slot{gtk:tool-button-icon-widget}
  @see-slot{gtk:tool-button-label}
  @see-slot{gtk:tool-button-label-widget}
  @see-slot{gtk:tool-button-stock-id}
  @see-slot{gtk:tool-button-use-underline}
  @see-class{gtk:tool-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:tool-button-icon-name ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'tool-button) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the themed icon displayed on the item. This property only has an
  effect if not overridden by the @slot[gtk:tool-button]{label},
  @slot[gtk:tool-button]{icon-widget} or @slot[gtk:tool-button]{stock-id}
  properties. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-icon-name)
      "Accessor"
      (documentation 'tool-button-icon-name 'function)
 "@version{2026-05-31}
  @syntax{(gtk:tool-button-icon-name object) => name}
  @syntax{(setf (gtk:tool-button-icon-name object) name)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[name]{a string for the name of the themed icon}
  @begin{short}
    The accessor for the @slot[gtk:tool-button]{icon-name} slot gets or sets
    the name of the themed icon for the tool button.
  @end{short}
  See the docs for the @class{gtk:icon-theme} implementation for more details.
  The @slot[gtk:tool-button]{icon-name} property only has an effect if not
  overridden by non-@code{nil} @slot[gtk:tool-button]{label},
  @slot[gtk:tool-button]{icon-widget} and @slot[gtk:tool-button]{stock-id}
  properties.
  @see-class{gtk:tool-button}
  @see-class{gtk:icon-theme}
  @see-function{gtk:tool-button-label}
  @see-function{gtk:tool-button-icon-widget}
  @see-function{gtk:tool-button-stock-id}")

;;; --- gtk:tool-button-icon-widget --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-widget" 'tool-button) t)
 "The @code{icon-widget} property of type @class{gtk:widget} (Read / Write)@br{}
  The icon to display in the item.")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-icon-widget)
      "Accessor"
      (documentation 'tool-button-icon-widget 'function)
 "@version{2026-05-31}
  @syntax{(gtk:tool-button-icon-widget object) => icon}
  @syntax{(setf (gtk:tool-button-icon-widget object) icon)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[icon]{a @class{gtk:widget} object used as icon, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:tool-button]{icon-widget} slot gets or sets
    the widget used as icon on the tool button.
  @end{short}
  If the @arg{icon} argument is @code{nil} the icon is determined by the
  @slot[gtk:tool-button]{stock-id} property. If the
  @slot[gtk:tool-button]{stock-id} property is also @code{nil}, the tool button
  will not have an icon.
  @see-class{gtk:tool-button}
  @see-class{gtk:widget}
  @see-function{gtk:tool-button-stock-id}")

;;; --- gtk:tool-button-label --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'tool-button) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text to show in the tool item. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-label)
      "Accessor"
      (documentation 'tool-button-label 'function)
 "@version{2026-05-31}
  @syntax{(gtk:tool-button-label object) => label}
  @syntax{(setf (gtk:tool-button-label object) label)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:tool-button]{label} slot gets or sets the
    label used by the tool button.
  @end{short}
  If @code{nil} the tool button does not have a label or uses a label from a
  stock item.

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

;;; --- gtk:tool-button-label-widget -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget" 'tool-button) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The widget to use as the item label.")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-label-widget)
      "Accessor"
      (documentation 'tool-button-label-widget 'function)
 "@version{2026-05-31}
  @syntax{(gtk:tool-button-label-widget object) => widget}
  @syntax{(setf (gtk:tool-button-label-widget object) widget)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[widget]{a @class{gtk:widget} object used as label, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:tool-button]{label-widget} slot gets or sets
    the widget used as label on the tool button.
  @end{short}
  If the @arg{widget} argument is @code{nil} the @slot[gtk:tool-button]{label}
  property is used as label. If @slot[gtk:tool-button]{label} is also
  @code{nil}, the label in the stock item determined by the
  @slot[gtk:tool-button]{stock-id} property is used as label. If
  @slot[gtk:tool-button]{stock-id} is also @code{nil}, @arg{button} does not
  have a label.
  @see-class{gtk:tool-button}
  @see-class{gtk:widget}
  @see-function{gtk:tool-button-label}
  @see-function{gtk:tool-button-stock-id}")

;;; --- gtk:tool-button-stock-id -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock-id" 'tool-button) t)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock icon displayed on the item. @br{}
  @em{Warning:} This property has been deprecated since version 3.10 and should
  not be used in newly written code. Use the @code{icon-name} property instead.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-button-stock-id)
      "Accessor"
      (documentation 'tool-button-stock-id 'function)
 "@version{2026-05-31}
  @syntax{(gtk:tool-button-stock-id object) => stock-id}
  @syntax{(setf (gtk:tool-button-stock-id object) stock-id)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[stock-id]{a string for the name of a stock item, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gtk:tool-button]{stock-id} slot gets or sets the
    name of the stock item.
  @end{short}
  The @slot[gtk:tool-button]{stock-id} property only has an effect if not
  overridden by non-@code{nil} @slot[gtk:tool-button]{label},
  @slot[gtk:tool-button]{icon-name}, and @slot[gtk:tool-button]{icon-widget}
  properties.
  @begin[Warning]{dictionary}
    This function has been deprecated since version 3.10 and should not be used
    in newly written code. Use the @fun{gtk:tool-button-icon-name} function
    instead.
  @end{dictionary}
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-label}
  @see-function{gtk:tool-button-icon-widget}
  @see-function{gtk:tool-button-icon-name}")

;;; --- gtk:tool-button-use-underline ------------------------------------------

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
 "@version{2026-06-05-31}
  @syntax{(gtk:tool-button-use-underline object) => setting}
  @syntax{(setf (gtk:tool-button-use-underline) setting)}
  @argument[object]{a @class{gtk:tool-button} widget}
  @argument[setting]{a boolean whether the button label has the form \"_Open\"}
  @begin{short}
    The accessor for the @slot[gtk:tool-button]{use-underline} slot gets or sets
    whether underscores in the @slot[gtk:tool-button]{label} property are used
    as mnemonics on menu items on the overflow menu.
  @end{short}
  If set, an underline in the @slot[gtk:tool-button]{label} property indicates
  that the next character should be used for the mnemonic accelerator key in
  the overflow menu.

  Labels shown on tool buttons never have mnemonics on them. This property
  only affects the menu item on the overflow menu.
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_button_new
;;; ----------------------------------------------------------------------------

(defun tool-button-new (&optional icon label)
 #+liber-documentation
 "@version{2026-05-29}
  @argument[icon]{a @class{gtk:widget} object that will be used as the button
    contents, or @code{nil}}
  @argument[label]{a string that will be used as label, or @code{nil}}
  @return{The new @class{gtk:tool-button} widget.}
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
;;; gtk_tool_button_new_from_stock
;;; ----------------------------------------------------------------------------

(defun tool-button-new-from-stock (stock-id)
 #+liber-documentation
 "@version{2026-05-29}
  @argument[stock-id]{a string for the name of the stock item}
  @return{The new @class{gtk:tool-button} widget.}
  @begin{short}
    Creates a new tool button containing the image and text from a stock item.
  @end{short}
  It is an error if the @arg{stock-id} argument is not a name of a stock item.
  @begin[Warning]{dictionary}
    This function has been deprecated since version 3.10 and should not be used
    in newly written code. Use the @fun{gtk:tool-button-new} function together
    with the @fun{gtk:image-new-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:tool-button}
  @see-function{gtk:tool-button-new}
  @see-function{gtk:image-new-from-icon-name}"
  (make-instance 'tool-button
                 :stock-id stock-id))

(export 'tool-button-new-from-stock)

;;; --- End of file gtk3.tool-button.lisp --------------------------------------
