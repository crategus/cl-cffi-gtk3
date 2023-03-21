;;; ----------------------------------------------------------------------------
;;; gtk3.menu-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkMenuButton
;;;
;;;     A widget that shows a popup when clicked on.
;;;
;;; Types and Values
;;;
;;;     GtkArrowType
;;;     GtkMenuButton
;;;
;;; Functions
;;;
;;;     gtk_menu_button_new
;;;     gtk_menu_button_set_popup                          Accessor
;;;     gtk_menu_button_get_popup                          Accessor
;;;     gtk_menu_button_set_popover                        Accessor
;;;     gtk_menu_button_get_popover                        Accessor
;;;     gtk_menu_button_set_menu_model                     Accessor
;;;     gtk_menu_button_get_menu_model                     Accessor
;;;     gtk_menu_button_set_use_popover                    Accessor
;;;     gtk_menu_button_get_use_popover                    Accessor
;;;     gtk_menu_button_set_direction                      Accessor
;;;     gtk_menu_button_get_direction                      Accessor
;;;     gtk_menu_button_set_align_widget                   Accessor
;;;     gtk_menu_button_get_align_widget                   Accessor
;;;
;;; Properties
;;;
;;;     align-widget
;;;     direction
;;;     menu-model
;;;     popover
;;;     popup
;;;     use-popover
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkToggleButton
;;;                             ╰── GtkMenuButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkArrowType
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkArrowType" arrow-type
  (:export t
   :type-initializer "gtk_arrow_type_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'arrow-type)
      "GEnum"
      (liber:symbol-documentation 'arrow-type)
 "@version{#2023-3-21}
  @begin{short}
    Used to indicate the direction in which an arrow should point in a
    @class{gtk:menu-button} widget.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkArrowType\" arrow-type
  (:export t
   :type-initializer \"gtk_arrow_type_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:none 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{Represents an upward pointing arrow.}
    @entry[:down]{Represents a downward pointing arrow.}
    @entry[:left]{Represents a left pointing arrow.}
    @entry[:right]{Represents a right pointing arrow.}
    @entry[:none]{No arrow.}
  @end{table}
  @see-class{gtk:menu-button}")

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuButton" menu-button
  (:superclass toggle-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_menu_button_get_type")
  ((align-widget
    menu-button-align-widget
    "align-widget" "GtkContainer" t t)
   (direction
    menu-button-direction
    "direction" "GtkArrowType" t t)
   (menu-model
    menu-button-menu-model
    "menu-model" "GMenuModel" t t)
   (popover
    menu-button-popover
    "popover" "GtkPopover" t t)
   (popup
    menu-button-popup
    "popup" "GtkMenu" t t)
   (use-popover
    menu-button-use-popover
    "use-popover" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'menu-button 'type)
 "@version{#2023-3-21}
  @begin{short}
    The @sym{gtk:menu-button} widget is used to display a popup when clicked on.
  @end{short}
  This popup can be provided either as a @class{gtk:menu} widget, a
  @class{gtk:popover} widget or an abstract @class{g:menu-model} object.

  @image[menu-button]{Figure: GtkMenuButton}

  The @sym{gtk:menu-button} widget can hold any valid child widget. That is, it
  can hold almost any other standard @class{gtk:widget} object. The most
  commonly used child is the @class{gtk:image} widget. If no widget is
  explicitely added to the @sym{gtk:menu-button} widget, a @class{gtk:image}
  widget is automatically created, using an arrow image oriented according to
  \"direction\" or the generic \"open-menu-symbolic\" icon if the direction is
  not set.

  The positioning of the popup is determined by the @code{direction} property
  of the menu button.

  For menus, the @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign}
  properties of the menu are also taken into account. For example, when the
  direction is @code{:down} and the horizontal alignment is @code{:start}, the
  menu will be positioned below the button, with the starting edge, depending
  on the text direction, of the menu aligned with the starting edge of the
  button. If there is not enough space below the button, the menu is popped up
  above the button instead. If the alignment would move part of the menu
  offscreen, it is \"pushed in\".
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:menu-button} implementation has a single CSS node with name
    @code{button}. To differentiate it from a plain @class{gtk:button} widget,
    it gets the @code{.popup} style class.
  @end{dictionary}
  @see-constructor{gtk:menu-button-new}
  @see-slot{gtk:menu-button-align-widget}
  @see-slot{gtk:menu-button-direction}
  @see-slot{gtk:menu-button-menu-model}
  @see-slot{gtk:menu-button-popover}
  @see-slot{gtk:menu-button-popup}
  @see-slot{gtk:menu-button-use-popover}
  @see-class{gtk:menu}
  @see-class{gtk:image}
  @see-class{gtk:popover}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; Accessor and Property Details
;;; ----------------------------------------------------------------------------

;;; --- menu-button-align-widget -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "align-widget" 'menu-button) t)
 "The @code{align-widget} property of type @class{gtk:container} (Read / Write)
  @br{}
  The widget to use to align the menu with.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-align-widget)
      "Accessor"
      (documentation 'menu-button-align-widget 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-button-align-widget object) => widget}
  @syntax[]{(setf (gtk:menu-button-align-widget object) widget)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{align-widget} slot of the
    @class{gtk:menu-button} class.
  @end{short}
  The @sym{gtk:menu-button-align-widget} function returns the parent widget to
  use to line up with menu or @code{nil}. The
  @sym{(setf gtk:menu-button-align-widget)} function sets the widget to use to
  line the menu with when popped up. Note that the @arg{widget} argument must
  contain the menu button itself.

  Setting it to @code{nil} means that the menu will be aligned with the button
  itself.

  Note that this property is only used with menus currently, and not for
  popovers.
  @see-class{gtk:menu-button}
  @see-class{gtk:widget}")

;;; --- menu-button-direction --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "direction" 'menu-button) t)
 "The @code{direction} property of type @symbol{gtk:arrow-type} (Read / Write)
  @br{}
  The arrow type representing the direction in which the menu or popover will
  be popped out. @br{}
  Default value: @code{:down}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-direction)
      "Accessor"
      (documentation 'menu-button-direction 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-button-direction object) => direction}
  @syntax[]{(setf (gtk:menu-button-direction object) direction)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[direction]{a value of the @symbol{gtk:arrow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{direction} slot of the
    @class{gtk:menu-button} class.
  @end{short}
  The @sym{gtk:menu-button-align-widget} function returns the direction the
  popup will be pointing at when popped up. The
  @sym{(setf gtk:menu-button-align-widget)} function sets the direction in
  which the popup will be popped up, as well as changing the direction of the
  arrow. The child will not be changed to an arrow if it was customized.

  If the popup does not fit in the available space in the given direction, GTK
  will its best to keep it inside the screen and fully visible.

  If you pass the @code{:none} value for a direction, the popup will behave as
  if you passed the @code{:down} value, although you will not see any arrows.
  @see-class{gtk:menu-button}
  @see-symbol{gtk:arrow-type}")

;;; --- menu-button-menu-model -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-model" 'menu-button) t)
 "The @code{menu-model} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model from which the popup will be created. Depending on the
  @code{use-popover} property, that may be a menu or a popover. See the
  @fun{gtk:menu-button-menu-model} function for the interaction with the
  @code{popup} property.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-menu-model)
      "Accessor"
      (documentation 'menu-button-menu-model 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-button-menu-model object) => model}
  @syntax[]{(setf (gtk:menu-button-menu-model object) model)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[model]{a @class{g:menu-model} object, or @code{nil} to unset and
    disable the button}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{menu-model} slot of the
    @class{gtk:menu-button} class.
  @end{short}
  The @sym{gtk:menu-button-menu-model} function returns the menu model used to
  generate the popup. The @sym{(setf gtk:menu-button-menu-model)} function sets
  the menu model from which the popup will be constructed, or @code{nil} to
  dissociate any existing menu model and disable the button.

  Depending on the value of @slot[gtk:menu-button]{use-popover} property,
  either a @class{gtk:menu} widget will be created with the
  @fun{gtk:menu-new-from-model} function, or a @class{gtk:popover} widget with
  the @fun{gtk:popover-new-from-model} function. In either case, actions will
  be connected as documented for these functions.

  If the @slot[gtk:menu-button]{popup} or @slot[gtk:menu-button]{popover}
  properties are already set, those widgets are dissociated from the menu
  button, and those properties are set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-class{g:menu-model}
  @see-class{gtk:menu}
  @see-class{gtk:popover}
  @see-function{gtk:menu-new-from-model}
  @see-function{gtk:popover-new-from-model}
  @see-function{gtk:menu-button-use-popover}
  @see-function{gtk:menu-button-popup}
  @see-function{gtk:menu-button-popover}")

;;; --- menu-button-popover ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popover" 'menu-button) t)
 "The @code{popover} property of type @class{gtk:popover} (Read / Write) @br{}
  The popover that will be popped up when the button is clicked.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-popover)
      "Accessor"
      (documentation 'menu-button-popover 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-button-popover object) => popover}
  @syntax[]{(setf (gtk:menu-button-popover object) popover)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[popover]{a @class{gtk:popover} widget, or @code{nil} to unset and
    disable the button}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{popover} slot of the
    @class{gtk:menu-button} class.
  @end{short}
  The @sym{gtk:menu-button-align-widget} function returns the popover that pops
  out of the button. If the button is not using a popover, this function returns
  @code{nil}. The @sym{(setf gtk:menu-button-align-widget)} function sets the
  popover that will be popped up when the menu button is clicked, or @code{nil}
  to dissociate any existing popover and disable the button.

  If the @slot[gtk:menu-button]{menu-model} or @slot[gtk:menu-button]{popup}
  properties are set, those objects are dissociated from the menu button, and
  those properties are set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-menu-model}
  @see-function{gtk:menu-button-popup}")

;;; --- menu-button-popup ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup" 'menu-button) t)
 "The @code{popup} property of type @class{gtk:menu} (Read / Write) @br{}
  The menu that will be popped up when the button is clicked.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-popup)
      "Accessor"
      (documentation 'menu-button-popup 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-button-popup object) => popup}
  @syntax[]{(setf (gtk:menu-button-popup object) popup)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[menu]{a @class{gtk:menu} widget, or @code{nil} to unset and disable
  the button}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{popup} slot of the
    @class{gtk:menu-button} class.
  @end{short}
  The @sym{gtk:menu-button-popup} function returns the menu that pops out of the
  button. If the button does not use a menu, this function returns @code{nil}.
  The @sym{(setf gtk:menu-button-popup)} function sets the menu that will be
  popped up when the menu button is clicked, or @code{nil} to dissociate any
  existing menu and disable the button.

  If the @slot[gtk:menu-button]{menu-model} or @slot[gtk:menu-button]{popover}
  are set, those objects are dissociated from the menu button, and those
  properties are set to @code{nil}.
  @see-class{gtk:menu-button}
  @see-class{gtk:menu}
  @see-function{gtk:menu-button-menu-model}
  @see-function{gtk:menu-button-popover}")

;;; --- menu-button-use-popover ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-popover" 'menu-button) t)
 "The @code{use-popover} property of type @code{:boolean} (Read / Write) @br{}
  Whether to construct a @class{gtk:popover} widget from the menu model, or a
  @class{gtk:menu} widget. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-button-use-popover)
      "Accessor"
      (documentation 'menu-button-use-popover 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-button-use-popover object) => use-popover}
  @syntax[]{(setf (gtk:menu-button-use-popover object) use-popover)}
  @argument[object]{a @class{gtk:menu-button} widget}
  @argument[use-popover]{@em{true} to construct a popover from the menu model}
  @begin{short}
    Accessor of the @slot[gtk:menu-button]{use-popover} slot of the
    @class{gtk:menu-button} class.
  @end{short}
  The @sym{gtk:menu-button-use-popover} function returns whether a popover or a
  menu will be constructed from the menu model. The
  @sym{(setf gtk:menu-button-use-popover)} function sets whether to construct a
  popover instead of a menu when the @fun{gtk:menu-button-menu-model} function
  is called. Note that this property is only consulted when a new menu model is
  set.
  @see-class{gtk:menu-button}
  @see-function{gtk:menu-button-menu-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline menu-button-new))

(defun menu-button-new ()
 #+liber-documentation
 "@version{#2023-3-21}
  @return{The new @class{gtk:menu-button} widget.}
  @begin{short}
    Creates a new menu button with downwards pointing arrow as the only child.
  @end{short}
  You can replace the child widget with another widget should you wish to.
  @see-class{gtk:menu-button}"
  (make-instance 'menu-button))

(export 'menu-button-new)

;;; --- End of file gtk3.menu-button.lisp --------------------------------------
