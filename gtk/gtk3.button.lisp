;;; ----------------------------------------------------------------------------
;;; gtk3.button.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkButton
;;;
;;;     A widget that emits a signal when clicked on.
;;;
;;; Types and Values
;;;
;;;     GtkButton
;;;
;;; Accessors
;;;
;;;     gtk_button_get_always_show_image
;;;     gtk_button_set_always_show_image
;;;     gtk_button_get_focus_on_click
;;;     gtk_button_set_focus_on_click
;;;     gtk_button_get_image
;;;     gtk_button_set_image
;;;     gtk_button_get_image_position
;;;     gtk_button_set_image_position
;;;     gtk_button_get_label
;;;     gtk_button_set_label
;;;     gtk_button_get_relief
;;;     gtk_button_set_relief
;;;     gtk_button_get_use_stock
;;;     gtk_button_set_use_stock
;;;     gtk_button_get_use_underline
;;;     gtk_button_set_use_underline
;;;
;;; Functions
;;;
;;;     gtk_button_new
;;;     gtk_button_new_with_label
;;;     gtk_button_new_with_mnemonic
;;;     gtk_button_new_from_icon_name ()
;;;     gtk_button_new_from_stock                           deprecated
;;;     gtk_button_pressed                                  deprecated
;;;     gtk_button_released                                 deprecated
;;;     gtk_button_clicked
;;;     gtk_button_enter                                    deprecated
;;;     gtk_button_leave                                    deprecated
;;;     gtk_button_set_alignment                            deprecated
;;;     gtk_button_get_alignment                            deprecated
;;;     gtk_button_get_event_window
;;;
;;; Properties
;;;
;;;     always-show-image
;;;     focus-on-click
;;;     image
;;;     image-position
;;;     label
;;;     relief
;;;     use-stock
;;;     use-underline
;;;     xalign
;;;     yalign
;;;
;;; Style Properties
;;;
;;;     child-displacement-x
;;;     child-displacement-y
;;;     default-border
;;;     default-outside-border
;;;     displace-focus
;;;     image-spacing
;;;     inner-border
;;;
;;; Signals
;;;
;;;     activate
;;;     clicked
;;;     enter
;;;     leave
;;;     pressed
;;;     released
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ├── GtkToggleButton
;;;                         ├── GtkColorButton
;;;                         ├── GtkFontButton
;;;                         ├── GtkLinkButton
;;;                         ├── GtkLockButton
;;;                         ├── GtkModelButton
;;;                         ╰── GtkScaleButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkButton implements AtkImplementorIface, GtkBuildable, GtkActionable
;;;     and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkButton" button
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_button_get_type")
  ((always-show-image
    button-always-show-image
    "always-show-image" "gboolean" t t)
   (focus-on-click
    button-focus-on-click
    "focus-on-click" "gboolean" t t)
   (image
    button-image
    "image" "GtkWidget" t t)
   (image-position
    button-image-position
    "image-position" "GtkPositionType" t t)
   (label
    button-label
    "label" "gchararray" t t)
   (relief
    button-relief
    "relief" "GtkReliefStyle" t t)
   (use-stock
    button-use-stock
    "use-stock" "gboolean" t t)
   (use-underline
    button-use-underline
    "use-underline" "gboolean" t t)
   (xalign
    button-xalign
    "xalign" "gfloat" t t)
   (yalign
    button-yalign
    "yalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'button 'type)
 "@version{2025-07-14}
  @begin{short}
    The @class{gtk:button} widget emits a signal when clicked on.
  @end{short}

  @image[button]{Figure: GtkButton}

  The @class{gtk:button} widget is generally used to trigger a callback function
  that is called when the button is pressed. The various signals and how to use
  them are outlined below.

  The @class{gtk:button} widget can hold any valid child widget. That is, it can
  hold almost any other standard @class{gtk:widget} object. The most commonly
  used child is the @class{gtk:label} widget.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:button} implementation has a single CSS node with name
    @code{button}. The node will get the @code{.image-button} or
    @code{.text-button} style classes, if the content is just an image or label,
    respectively. It may also receive the @code{.flat} style class.

    Other style classes that are commonly used with the @class{gtk:button}
    widget include @code{.suggested-action} and @code{.destructive-action}. In
    special cases, buttons can be made round by adding the @code{.circular}
    style class.

    Button-like widgets like @class{gtk:toggle-button}, @class{gtk:menu-button},
    @class{gtk:volume-button}, @class{gtk:lock-button},
    @class{gtk:color-button}, @class{gtk:font-button} or
    @class{gtk:file-chooser-button} use style classes such as @code{.toggle},
    @code{.popup}, @code{.scale}, @code{.lock}, @code{.color}, @code{.font},
    @code{.file} to differentiate themselves from a plain @class{gtk:button}
    widget.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[button:child-displacement-x]{property}
      The @code{child-displacement-x} style property of type @code{:int}
      (Read) @br{}
      How far in the x direction to move the child when the button is
      depressed. @br{}
      @em{Warning:} The @code{child-displacement-x} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. Use CSS margins and padding instead. The value of this style
      property is ignored. @br{}
      Default value: 0
    @end{property}
    @begin[button:child-displacement-y]{property}
      The @code{child-displacement-y} style property of type @code{:int}
      (Read) @br{}
      How far in the y direction to move the child when the button is
      depressed. @br{}
      @em{Warning:} The @code{child-displacement-x} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. Use CSS margins and padding instead. The value of this style
      property is ignored. @br{}
      Default value: 0
    @end{property}
    @begin[button:default-border]{property}
      The @code{default-border} style property of type @class{gtk:border}
      (Read) @br{}
      Defines the extra space to add around a button that can become the
      default widget of its window. For more information about default
      widgets, see the @fun{gtk:widget-grab-default} function. @br{}
      @em{Warning:} The @code{default-border} style property has been
      deprecated since version 3.14 and should not be used in newly written
      code. Use CSS margins and padding instead. The value of this style
      property is ignored.
    @end{property}
    @begin[button:default-outside-border]{property}
      The @code{default-outside-border} style property of type
      @class{gtk:border} (Read) @br{}
      Defines the extra outside space to add around a button that can become
      the default widget of its window. Extra outside space is always drawn
      outside the button border. For more information about default widgets,
      see the @fun{gtk:widget-grab-default} function. @br{}
      @em{Warning:} The @code{default-outside-border} style property has been
      deprecated since version 3.14 and should not be used in newly written
      code. Use CSS margins and padding instead. The value of this style
      property is ignored.
    @end{property}
    @begin[button:displace-focus]{property}
      The @code{displace-focus} style property of type @code{:boolean}
      (Read) @br{}
      Whether the @prop[gtk:button]{child-displacement-x} or
      @prop[gtk:button]{child-displacement-y} properties should also affect the
      focus rectangle. @br{}
      @em{Warning:} The @code{displace-focus} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. Use CSS margins and padding instead. The value of this style
      property is ignored. @br{}
      Default value: @em{false}
    @end{property}
    @begin[button:image-spacing]{property}
      The @code{image-spacing} style property of type @code{:int} (Read) @br{}
      The spacing in pixels between the image and label. @br{}
      Allowed values: >= 0 @br{}
      Default value: 2
    @end{property}
    @begin[button:inner-border]{property}
      The @code{inner-border} style property of type @class{gtk:border}
      (Read) @br{}
      Sets the border between the button edges and child. @br{}
      @em{Warning:} The @code{inner-border} style property has been
      deprecated since version 3.4 and should not be used in newly written
      code. Use the standard border and padding CSS properties. The value
      of this style property is ignored. @br{}
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[button::activate]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      The signal on the @class{gtk:button} widget is an action signal and
      emitting it causes the button to animate press then release. Applications
      should never connect to this signal, but use the @sig[gtk:button]{clicked}
      signal.
    @end{signal}
    @begin[button::clicked]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      Emitted when the button has been activated (pressed and released).
    @end{signal}
    @begin[button::enter]{signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      Emitted when the pointer enters the button. @br{}
      @em{Warning:} The @sig[gtk:button]{enter} signal has been deprecated
      since version 2.8 and should not be used in newly written code. Use the
      @sig[gtk:widget]{enter-notify-event} signal of the @class{gtk:widget}
      class.
    @end{signal}
    @begin[button::leave]{signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      Emitted when the pointer leaves the button. @br{}
      @em{Warning:} The @sig[gtk:button]{leave} signal has been deprecated
      since version 2.8 and should not be used in newly written code. Use the
      @sig[gtk:widget]{leave-notify-event} signal of the @class{gtk:widget}
      class.
    @end{signal}
    @begin[button::pressed]{signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      Emitted when the button is pressed. @br{}
      @em{Warning:} The @sig[gtk:button]{pressed} signal has been deprecated
      since version 2.8 and should not be used in newly written code. Use the
      @sig[gtk:widget]{button-press-event} signal of the @class{gtk:widget}
      class.
    @end{signal}
    @begin[button::released]{signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:button} widget that received the signal.}
      @end{simple-table}
      Emitted when the button is released. @br{}
      @em{Warning:} The @sig[gtk:button]{released} signal has been deprecated
      since version 2.8 and should not be used in newly written code. Use the
      @sig[gtk:widget]{button-release-event} signal of the @class{gtk:widget}
      class.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:button-new}
  @see-constructor{gtk:button-new-with-label}
  @see-constructor{gtk:button-new-with-mnemonic}
  @see-constructor{gtk:button-new-from-icon-name}
  @see-slot{gtk:button-always-show-image}
  @see-slot{gtk:button-focus-on-click}
  @see-slot{gtk:button-image}
  @see-slot{gtk:button-image-position}
  @see-slot{gtk:button-label}
  @see-slot{gtk:button-relief}
  @see-slot{gtk:button-use-stock}
  @see-slot{gtk:button-use-underline}
  @see-slot{gtk:button-xalign}
  @see-slot{gtk:button-yalign}
  @see-class{gtk:widget}
  @see-class{gtk:label}
  @see-class{gtk:border}
  @see-symbol{gtk:position-type}
  @see-symbol{gtk:relief-style}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:button-always-show-image -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "always-show-image" 'button) t)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the button will ignore the
  @slot[gtk:settings]{gtk-button-images} setting and always show the image, if
  available. Use this property if the button would be useless or hard to use
  without the image. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'button-always-show-image)
      "Accessor"
      (documentation 'button-always-show-image 'function)
 "@version{2023-12-30}
  @syntax{(gtk:button-always-show-image object) => setting}
  @syntax{(setf (gtk:button-always-show-image object) setting)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[setting]{@em{true} if the button should always show the image}
  @begin{short}
    Accessor of the @slot[gtk:button]{always-show-image} slot of the
    @class{gtk:button} class.
  @end{short}
  The @fun{gtk:button-always-show-image} function returns whether the button
  will ignore the @slot[gtk:settings]{gtk-button-images} setting and always
  show the image, if available. The @setf{gtk:button-always-show-image} function
  sets the property.

  Use this property if the button would be useless or hard to use without
  the image.
  @see-class{gtk:button}
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-button-images}")

;;; --- gtk:button-focus-on-click ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-on-click" 'button) t)
 "The @code{focus-on-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the button grabs focus when it is clicked with the mouse. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'button-focus-on-click)
      "Accessor"
      (documentation 'button-focus-on-click 'function)
 "@version{2023-12-30}
  @syntax{(gtk:button-focus-on-click object) => setting}
  @syntax{(setf (gtk:button-focus-on-click object) setting)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[setting]{a boolean whether the button grabs focus when clicked with
    the mouse}
  @begin{short}
    Accessor of the @slot[gtk:button]{focus-on-click} slot of the
    @class{gtk:button} class.
  @end{short}
  The @fun{gtk:button-focus-on-click} function returns whether the button grabs
  focus when it is clicked with the mouse. The
  @setf{gtk:button-focus-on-click} function sets whether the button will grab
  focus.

  Making mouse clicks not grab focus is useful in places like toolbars where you
  do not want the keyboard focus removed from the main area of the application.
  @begin[Warning]{dictionary}
    The @fun{gtk:button-focus-on-click} function has been deprecated since
    version 3.20 and should not be used in newly written code. Use the
    @fun{gtk:widget-focus-on-click} function instead.
  @end{dictionary}
  @see-class{gtk:button}
  @see-function{gtk:widget-focus-on-click}")

;;; --- gtk:button-image -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "image" 'button) t)
 "The @code{image} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget to appear next to the button text.")

#+liber-documentation
(setf (liber:alias-for-function 'button-image)
      "Accessor"
      (documentation 'button-image 'function)
 "@version{2023-12-30}
  @syntax{(gtk:button-image object) => image}
  @syntax{(setf (gtk:button-image object) image)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[image]{a @class{gtk:widget} to set as the image for the button}
  @begin{short}
    Accessor of the @slot[gtk:button]{image} slot of the @class{gtk:button}
    class.
  @end{short}
  The @fun{gtk:button-image} function gets the widget that is currently set as
  the image of the button. This may have been explicitly set by the
  @setf{gtk:button-image} function or constructed by the
  @fun{gtk:button-new-from-icon-name} function.

  Note that it depends on the @slot[gtk:button]{always-show-image} property
  whether the image will allways be displayed or not. You do not have to call
  the @fun{gtk:widget-show} function on the image yourself.
  @see-class{gtk:button}
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:button-new-from-icon-name}
  @see-function{gtk:button-always-show-image}")

;;; --- gtk:button-image-position ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "image-position" 'button) t)
 "The @code{image-position} property of type @sym{gtk:position-type}
  (Read / Write) @br{}
  The position of the image relative to the text inside the button. @br{}
  Default value: @val[gtk:position-type]{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'button-image-position)
      "Accessor"
      (documentation 'button-image-position 'function)
 "@version{2025-06-28}
  @syntax{(gtk:button-image-position object) => position}
  @syntax{(setf (gtk:button-image-position object) position)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[position]{a value of the @sym{gtk:position-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:button]{image-position} slot of the
    @class{gtk:button} class.
  @end{short}
  The @fun{gtk:button-image-position} function gets the position of the image
  relative to the text inside the button. The @setf{gtk:button-image-position}
  function sets the position of the image.
  @see-class{gtk:button}
  @see-symbol{gtk:position-type}")

;;; --- gtk:button-label -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'button) t)
 "The @code{label} property of type @code{:string} (Read / Write / Construct)
  @br{}
  The text of the label inside the button, if the button contains a label. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'button-label)
      "Accessor"
      (documentation 'button-label 'function)
 "@version{2025-06-18}
  @syntax{(gtk:button-label object) => label}
  @syntax{(setf (gtk:button-label object) label)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[label]{a string for the text of the label}
  @begin{short}
    Accessor of the @slot[gtk:button]{label} slot of the @class{gtk:button}
    class.
  @end{short}
  The @fun{gtk:button-label} function fetches the text from the label of the
  button. The @setf{gtk:button-label} function sets the text.

  If the label text has not been set the return value will be @code{nil}. This
  will be the case if you create an empty button with the @fun{gtk:button-new}
  function to use as a container.
  @see-class{gtk:button}
  @see-function{gtk:button-new}")

;;; --- gtk:button-relief ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "relief" 'button) t)
 "The @code{relief} property of type @sym{gtk:relief-style} (Read / Write) @br{}
  The border relief style. @br{}
  Default value: @val[gtk:relief-style]{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'button-relief)
      "Accessor"
      (documentation 'button-relief 'function)
 "@version{2025-06-28}
  @syntax{(gtk:button-relief object) => style}
  @syntax{(setf (gtk:button-relief object) style)}
  @argument[object]{a @class{gtk:button} widget you want to set relief
    styles of}
  @argument[style]{a @sym{gtk:relief-style} value}
  @begin{short}
    Accessor of the @slot[gtk:button]{relief} slot of the @class{gtk:button}
    class.
  @end{short}
  The @fun{gtk:button-relief} function returns the current relief style of the
  button. The @setf{gtk:button-relief} function sets the relief style of the
  edges of the given button.

  Three styles exist, the @val[gtk:relief-style]{:normal},
  @val[gtk:relief-style]{:half}, @val[gtk:relief-style]{:none} style. The
  default style is the @val[gtk:relief-style]{:normal} style.
  @see-class{gtk:button}
  @see-symbol{gtk:relief-style}")

;;; --- gtk:button-use-stock ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-stock" 'button) t)
 "The @code{use-stock} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, the label is used to pick a stock item instead of being
  displayed. @br{}
  @em{Warning:} The @code{use-stock} property has been deprecated since
  version 3.10 and should not be used in newly written code. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'button-use-stock)
      "Accessor"
      (documentation 'button-use-stock 'function)
 "@version{2023-12-30}
  @syntax{(gtk:button-use-stock object) => setting}
  @syntax{(setf (gtk:button-use-stock object) setting)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[setting]{@em{true} if the button should use a stock item}
  @begin{short}
    Accessor of the @slot[gtk:button]{use-stock} slot of the @class{gtk:button}
    class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:button-use-stock} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:button}")

;;; --- gtk:button-use-underline -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'button) t)
 "The @code{use-underline} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'button-use-underline)
      "Accessor"
      (documentation 'button-use-underline 'function)
 "@version{2023-12-30}
  @syntax{(gtk:button-use-underline object) => setting}
  @syntax{(setf (gtk:button-use-underline object) setting)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:button]{use-underline} slot of the
    @class{gtk:button} class.
  @end{short}
  The @fun{gtk:button-use-underline} function returns whether an embedded
  underline in the button label indicates a mnemonic. If @em{true}, an
  underline in the text of the button label indicates the next character should
  be used for the mnemonic accelerator key.
  @see-class{gtk:button}")

;;; --- gtk:button-xalign ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'button) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  If the child of the button is a @class{gtk:misc} or @class{gtk:alignment}
  widget, this property can be used to control its horizontal alignment. The
  value 0.0 is left aligned, 1.0 is right aligned. @br{}
  @em{Warning:} The @code{xalign} property has been deprecated since version
  3.14 and should not be used in newly written code. Access the child widget
  directly if you need to control its alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'button-xalign)
      "Accessor"
      (documentation 'button-xalign 'function)
 "@version{2025-06-18}
  @syntax{(gtk:button-xalign object) => xalign}
  @syntax{(setf (gtk:button-xalign object) xalign)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[xalign]{a number coerced to a single float for the horizontal
    alignment}
  @begin{short}
    Accessor of the @slot[gtk:button]{xalign} slot of the @class{gtk:button}
    class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:button-xalign} function has been deprecated since version 3.14
    and should not be used in newly written code. Access the child widget
    directly if you need to control its alignment.
  @end{dictionary}
  @see-class{gtk:button}")

;;; --- gtk:button-yalign ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'button) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  If the child of the button is a @class{gtk:misc} or @class{gtk:alignment}
  widget, this property can be used to control its vertical alignment. The
  value 0.0 is top aligned, 1.0 is bottom aligned. @br{}
  @em{Warning:} The @code{yalign} property has been deprecated since version
  3.14 and should not be used in newly written code. Access the child widget
  directly if you need to control its alignment. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'button-yalign)
      "Accessor"
      (documentation 'button-yalign 'function)
 "@version{2025-06-18}
  @syntax{(gtk:button-yalign object) => yalign}
  @syntax{(setf (gtk:button-yalign object) yalign)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[xalign]{a number coerced to a single float for the vertical
    alignment}
  @begin{short}
    Accessor of the @slot[gtk:button]{yalign} slot of the @class{gtk:button}
    class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:button-yalign} function has been deprecated since version 3.14
    and should not be used in newly written code. Access the child widget
    directly if you need to control its alignment.
  @end{dictionary}
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; gtk_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline button-new))

(defun button-new ()
 #+liber-documentation
 "@version{2023-12-30}
  @return{The newly created @class{gtk:button} widget.}
  @begin{short}
    Creates a new button.
  @end{short}
  To add a child widget to the button, use the @fun{gtk:container-add} function.
  @see-class{gtk:button}
  @see-function{gtk:button-new-with-label}
  @see-function{gtk:button-new-with-mnemonic}
  @see-function{gtk:button-new-from-icon-name}
  @see-function{gtk:container-add}"
  (make-instance 'button))

(export 'button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_label
;;; ----------------------------------------------------------------------------

(declaim (inline button-new-with-label))

(defun button-new-with-label (label)
 #+liber-documentation
 "@version{2025-06-18}
  @argument[label]{a string for the text you want the @class{gtk:label} child
    widget to hold}
  @return{The newly created @class{gtk:button} widget.}
  @begin{short}
    Creates a button with a label containing the given text in @arg{label}.
  @end{short}
  @see-class{gtk:button}
  @see-class{gtk:label}
  @see-function{gtk:button-new}
  @see-function{gtk:button-new-with-mnemonic}
  @see-function{gtk:button-new-from-icon-name}"
  (make-instance 'button
                 :label label))

(export 'button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(declaim (inline button-new-with-mnemonic))

(defun button-new-with-mnemonic (label)
 #+liber-documentation
 "@version{2025-06-18}
  @argument[label]{a string for the text of the button, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:button} widget.}
  @begin{short}
    Creates a new button widget containing a label with a mnemonic.
  @end{short}
  If characters in @arg{label} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use
  '__' (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. Pressing @kbd{Alt} and that key activates the
  button.
  @see-class{gtk:button}
  @see-function{gtk:button-new}
  @see-function{gtk:button-new-with-label}
  @see-function{gtk:button-new-from-icon-name}"
  (make-instance 'button
                 :label label
                 :use-underline t))

(export 'button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_button_new_from_icon_name" button-new-from-icon-name)
    (g:object widget)
 #+liber-documentation
 "@version{2025-06-28}
  @argument[name]{a string for the icon name}
  @argument[size]{a @sym{gtk:icon-size} value}
  @return{The new @class{gtk:button} widget displaying the themed icon.}
  @begin{short}
    Creates a new button containing an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.

  This function is a convenience wrapper around the @fun{gtk:button-new} and
  @fun{gtk:button-image} functions.
  @see-class{gtk:button}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:button-new}
  @see-function{gtk:button-image}"
  (name :string)
  (size icon-size))

(export 'button-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_stock                               not exported
;;; ----------------------------------------------------------------------------

(defun button-new-from-stock (stock)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[stock]{a string for the name of the stock item}
  @return{The new @class{gtk:button} widget.}
  @begin{short}
    Creates a new button widget containing the image and text from a stock item.
  @end{short}
  If @arg{stock} is unknown, then it will be treated as a mnemonic label
  as for the @fun{gtk:button-new-with-mnemonic} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:button-new-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:button-new-with-label} function instead.
  @end{dictionary}
  @see-class{gtk:button}
  @see-function{gtk:button-new}
  @see-function{gtk:button-new-with-label}
  @see-function{gtk:button-new-with-mnemonic}"
  (make-instance 'button
                 :label stock
                 :use-underline t
                 :use-stock t))

;;; ----------------------------------------------------------------------------
;;; gtk_button_pressed                                      deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_released                                     deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_clicked
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_button_clicked" button-clicked) :void
 #+liber-documentation
 "@version{2025-06-28}
  @argument[button]{a @class{gtk:button} widget you want to send the signal to}
  @begin{short}
    Emits a @sig[gtk:button]{clicked} signal to the given button.
  @end{short}
  @see-class{gtk:button}"
  (button (g:object button)))

(export 'button-clicked)

;;; ----------------------------------------------------------------------------
;;; gtk_button_enter                                        deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_leave                                        deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_alignment
;;; gtk_button_set_alignment                                deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_button_get_event_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_button_get_event_window" button-event-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[button]{a @class{gtk:button} widget}
  @return{The @class{gdk:window} event window of the button.}
  @begin{short}
    Returns the GDK event window of the button if it is realized, @code{nil}
    otherwise.
  @end{short}
  This function should be rarely needed.
  @see-class{gtk:button}
  @see-class{gdk:window}"
  (button (g:object button)))

(export 'button-event-window)

;;; --- End of file gtk3.button.lisp -------------------------------------------
