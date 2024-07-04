;;; ----------------------------------------------------------------------------
;;; gtk3.model-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkModelButton
;;;
;;;     A button that uses a GAction as model
;;;
;;; Types and Values
;;;
;;;     GtkModelButton
;;;     GtkButtonRole
;;;
;;; Functions
;;;
;;;     gtk_model_button_new ()
;;;
;;; Properties
;;;
;;;     active
;;;     centered
;;;     icon
;;;     iconic
;;;     inverted
;;;     menu-name
;;;     role
;;;     text
;;;     use-markup
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkModelButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkModelButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkButtonRole
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkButtonRole" button-role
  (:export t
   :type-initializer "gtk_button_role_get_type")
  :normal
  :check
  :radio)

#+liber-documentation
(setf (liber:alias-for-symbol 'button-role)
      "GEnum"
      (liber:symbol-documentation 'button-role)
 "@version{#2024-3-22}
  @begin{declaration}
(gobject:define-g-enum \"GtkButtonRole\" button-role
  (:export t
   :type-initializer \"gtk_button_role_get_type\")
  :normal
  :check
  :radio)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:normal]{A plain button.}
      @entry[:check]{A check button.}
      @entry[:radio]{A radio button.}
    @end{table}
  @end{values}
  @begin{short}
    The role specifies the desired appearance of a @class{gtk:model-button}
    widget.
  @end{short}
  @see-class{gtk:model-button}")

;;; ----------------------------------------------------------------------------
;;; struct GtkModelButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkModelButton" model-button
  (:superclass button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_model_button_get_type")
  ((active
    model-button-active
    "active" "gboolean" t t)
   (centered
    model-button-centered
    "centered" "gboolean" t t)
   (icon
    model-button-icon
    "icon" "GIcon" t t)
   (iconic
    model-button-iconic
    "iconic" "gboolean" t t)
   (inverted
    model-button-inverted
    "inverted" "gboolean" t t)
   (menu-name
    model-button-menu-name
    "menu-name" "gchararray" t t)
   (role
    model-button-role
    "role" "GtkButtonRole" t t)
   (text
    model-button-text
    "text" "gchararray" t t)
   (use-markup
    model-button-use-markup
    "use-markup" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'model-button 'type)
 "@version{#2023-3-21}
  @begin{short}
    The @class{gtk:model-button} class is a button class that can use a
    @class{g:action} object as its model.
  @end{short}
  In contrast to the @class{gtk:toggle-button} or @class{gtk:radio-button}
  classes, which can also be backed by a @class{g:action} object via the
  @code{action-name} property, the @class{gtk:model-button} widget will adapt
  its appearance according to the kind of action it is backed by, and appear
  either as a plain, check or radio button.

  Model buttons are used with popovers from a menu model with the
  @fun{gtk:popover-new-from-model} function. They can also be used manually in
  a @class{gtk:popover-menu} widget.

  When the action is specified via the @slot[gtk:actionable]{action-name} and
  @slot[gtk:actionable]{action-target} properties, the role of the button, i.e.
  whether it is a plain, check or radio button, is determined by the type of the
  action and does not have to be explicitly specified with the @code{role}
  property.

  The content of the button is specified by the @code{text} and @code{icon}
  properties.

  The appearance of model buttons can be influenced with the @code{centered}
  and @code{iconic} properties.

  Model buttons have built-in support for submenus in the
  @class{gtk:popover-menu} widget. To make a @class{gtk:model-button} widget
  that opens a submenu when activated, set the @code{menu-name} property. To
  make a button that goes back to the parent menu, you should set the
  @code{inverted} property to place the submenu indicator at the opposite side.
  @begin{examples}
    @begin{pre}
<object class=\"GtkPopoverMenu\">
  <child>
    <object class=\"GtkBox\">
      <property name=\"visible\">True</property>
      <property name=\"margin\">10</property>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">view.cut</property>
          <property name=\"text\" translatable=\"yes\">Cut</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">view.copy</property>
          <property name=\"text\" translatable=\"yes\">Copy</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">view.paste</property>
          <property name=\"text\" translatable=\"yes\">Paste</property>
        </object>
      </child>
    </object>
  </child>
</object>
    @end{pre}
  @end{examples}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
modelbutton
├── <child>
╰── check

modelbutton
├── <child>
╰── radio

modelbutton
├── <child>
╰── arrow
    @end{pre}
    The @class{gtk:model-button} implementation has a main CSS node with name
    @code{modelbutton}, and a subnode, which will have the name @code{check},
    @code{radio} or @code{arrow}, depending on the role of the button and
    whether it has a menu name set.

    The subnode is positioned before or after the content nodes and gets the
    @code{.left} or @code{.right} style class, depending on where it is located.
    @begin{pre}
button.model
├── <child>
╰── check
    @end{pre}
    Iconic model buttons, see the @code{iconic} property, change the name of
    their main node to button and add a @code{.model} style class to it. The
    indicator subnode is invisible in this case.
  @end{dictionary}
  @see-constructor{gtk:model-button-new}
  @see-slot{gtk:model-button-active}
  @see-slot{gtk:model-button-centered}
  @see-slot{gtk:model-button-icon}
  @see-slot{gtk:model-button-iconic}
  @see-slot{gtk:model-button-inverted}
  @see-slot{gtk:model-button-menu-name}
  @see-slot{gtk:model-button-role}
  @see-slot{gtk:model-button-text}
  @see-slot{gtk:model-button-use-markup}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:model-button-active ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'model-button) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  The state of the button. This is reflecting the state of the associated
  @class{g:action}. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-active)
      "Accessor"
      (documentation 'model-button-active 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-active object) => active}
  @syntax{(setf (gtk:model-button-active object) active)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[active]{a boolean with the state of the button}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{active} slot of the
    @class{gtk:model-button} class.
  @end{short}
  The state of the button. This is reflecting the state of the associated
  @class{g:action}.
  @see-class{gtk:model-button}
  @see-class{g:action}")

;;; --- gtk:model-button-centered ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "centered" 'model-button) t)
 "The @code{centered} property of type @code{:boolean} (Read / Write) @br{}
  Whether to render the button contents centered instead of left-aligned. This
  property should be set for title-like items. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-centered)
      "Accessor"
      (documentation 'model-button-centered 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-centered object) => centered}
  @syntax{(setf (gtk:model-button-centered object) centered)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[active]{a boolean whether to render the button contents centered
    instead of left-aligned}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{centered} slot of the
    @class{gtk:model-button} class.
  @end{short}
  Whether to render the button contents centered instead of left-aligned. This
  property should be set for title-like items.
  @see-class{gtk:model-button}")

;;; --- gtk:model-button-icon --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon" 'model-button) t)
 "The @code{icon} property of type @class{g:icon} (Read / Write) @br{}
  An icon that will be used if iconic appearance for the button is desired.")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-icon)
      "Accessor"
      (documentation 'model-button-icon 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-icon object) => icon}
  @syntax{(setf (gtk:model-button-icon object) icon)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{icon} slot of the
    @class{gtk:model-button} class.
  @end{short}
  An icon that will be used if iconic appearance for the button is desired.
  @see-class{gtk:model-button}
  @see-class{g:icon}
  @see-function{gtk:model-button-iconic}")

;;; --- gtk:model-button-iconic ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "iconic" 'model-button) t)
 "The @code{iconic} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set, the button will show an icon if one is set. If no
  icon is set, the text will be used. This is typically used for horizontal
  sections of linked buttons. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-iconic)
      "Accessor"
      (documentation 'model-button-iconic 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-iconic object) => iconic}
  @syntax{(setf (gtk:model-button-iconic object) iconic)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[iconic]{a boolean whether the button will show an icon}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{iconic} slot of the
    @class{gtk:model-button} class.
  @end{short}
  If this property is set, the button will show an icon if one is set. If no
  icon is set, the text will be used. This is typically used for horizontal
  sections of linked buttons.
  @see-class{gtk:model-button}
  @see-function{gtk:model-button-icon}")

;;; --- gtk:model-button-inverted ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inverted" 'model-button) t)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Whether to show the submenu indicator at the opposite side than normal. This
  property should be set for model buttons that 'go back' to a parent menu.@br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-inverted)
      "Accessor"
      (documentation 'model-button-inverted 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-inverted object) => inverted}
  @syntax{(setf (gtk:model-button-inverted object) inverted)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[inverted]{a boolean whether to show the submenu indicator at the
    opposite side than normal}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{inverted} slot of the
    @class{gtk:model-button} class.
  @end{short}
  Whether to show the submenu indicator at the opposite side than normal. This
  property should be set for model buttons that 'go back' to a parent menu.
  @see-class{gtk:model-button}")

;;; --- gtk:model-button-menu-name ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-name" 'model-button) t)
 "The @code{menu-name} property of type @code{:string} (Read / Write) @br{}
  The name of a submenu to open when the button is activated. If this is set,
  the button should not have an action associated with it. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-menu-name)
      "Accessor"
      (documentation 'model-button-menu-name 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-menu-name object) => name}
  @syntax{(setf (gtk:model-button-menu-name object) name)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[name]{a string with the name of a submenu}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{menu-name} slot of the
    @class{gtk:model-button} class.
  @end{short}
  The name of a submenu to open when the button is activated. If this is set,
  the button should not have an action associated with it.
  @see-class{gtk:model-button}")

;;; --- gtk:model-button-role --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "role" 'model-button) t)
 "The @code{role} property of type @symbol{gtk:button-role} (Read / Write) @br{}
  Specifies whether the button is a plain, check or radio button. When the
  @slot[gtk:actionable]{action-name} property is set, the role will be
  determined from the action and does not have to be set explicitly. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-role)
      "Accessor"
      (documentation 'model-button-role 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-role object) => role}
  @syntax{(setf (gtk:model-button-role object) role)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[role]{a value of the @symbol{gtk:button-role} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{role} slot of the
    @class{gtk:model-button} class.
  @end{short}
  Specifies whether the button is a plain, check or radio button. When the
  @slot[gtk:actionable]{action-name} property is set, the role will be
  determined from the action and does not have to be set explicitly.
  @see-class{gtk:model-button}
  @see-symbol{gtk:button-role}")

;;; --- gtk:model-button-text --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'model-button) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The label for the button. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-text)
      "Accessor"
      (documentation 'model-button-text 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-text object) => text}
  @syntax{(setf (gtk:model-button-text object) text)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[text]{a string with the label for the button}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{text} slot of the
    @class{gtk:model-button} class.
  @end{short}
  The label for the button.
  @see-class{gtk:model-button}")

;;; --- gtk:model-button-use-markup --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup" 'model-button) t)
 "The @code{use-markjup} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, XML tags in the text of the button are interpreted to format the
  enclosed spans of text. If @em{false}, the text will be displayed verbatim.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'model-button-use-markup)
      "Accessor"
      (documentation 'model-button-use-markup 'function)
 "@version{#2023-3-21}
  @syntax{(gtk:model-button-use-markup object) => use-markup}
  @syntax{(setf (gtk:model-button-use-markup object) use-markup)}
  @argument[object]{a @class{gtk:model-button} widget}
  @argument[use-markup]{a boolean whether to use Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:model-button]{use-markup} slot of the
    @class{gtk:model-button} class.
  @end{short}
  If @em{true}, XML tags in the text of the button are interpreted to format the
  enclosed spans of text. If @em{false}, the text will be displayed verbatim.
  @see-class{gtk:model-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_model_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline model-button-new))

(defun model-button-new ()
 #+liber-documentation
 "@version{#2023-3-21}
  @return{A new @class{gtk:model-button} widget.}
  @short{Creates a new model button widget.}
  @see-class{gtk:model-button}"
  (make-instance 'model-button))

(export 'model-button-new)

;;; --- End of file gtk3.model-button.lisp -------------------------------------
