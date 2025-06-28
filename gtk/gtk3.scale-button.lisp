;;; ----------------------------------------------------------------------------
;;; gtk3.scale-button.lisp
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
;;; GtkScaleButton
;;;
;;;     A button which pops up a scale
;;;
;;; Types and Values
;;;
;;;     GtkScaleButton
;;;
;;; Functions
;;;
;;;     gtk_scale_button_new
;;;     gtk_scale_button_set_adjustment                     Accessor
;;;     gtk_scale_button_set_icons                          Accessor
;;;     gtk_scale_button_set_value                          Accessor
;;;     gtk_scale_button_get_adjustment                     Accessor
;;;     gtk_scale_button_get_value                          Accessor
;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button
;;;
;;; Properties
;;;
;;;     adjustment
;;;     icons
;;;     size
;;;     value
;;;
;;; Signals
;;;
;;;     popdown
;;;     popup
;;;     value-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkScaleButton
;;;                             ╰── GtkVolumeButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScaleButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable, GtkActivatable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScaleButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkScaleButton" scale-button
  (:superclass button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable"
                "GtkOrientable")
   :type-initializer "gtk_scale_button_get_type")
  ((adjustment
    scale-button-adjustment
    "adjustment" "GtkAdjustment" t t)
   (icons
    scale-button-icons
    "icons" "GStrv" t t)
   (size
    scale-button-size
    "size" "GtkIconSize" t t)
   (value
    scale-button-value
    "value" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'scale-button 'type)
 "@version{#2025-06-28}
  @begin{short}
    The @class{gtk:scale-button} widget provides a button which pops up a scale
    widget.
  @end{short}
  This kind of widget is commonly used for volume controls in multimedia
  applications, and GTK provides a @class{gtk:volume-button} subclass that is
  tailored for this use case.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:scale-button} implementation has a single CSS node with name
    @code{button}. To differentiate it from a plain @class{gtk:button} widget,
    it gets the @code{.scale} style class. The popup widget that contains the
    scale has a @code{.scale-popup} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[scal-button::popdown]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:scale-button} widget which received the
          signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to popdown the scale
      widget. The default binding for this signal is the @kbd{Escape} key.
    @end{signal}
    @begin[scale-button::popup]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:scale-button} widget which received the
          signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to popup the scale
      widget. The default bindings for this signal are the @kbd{Space},
      @kbd{Enter} and @kbd{Return} keys.
    @end{signal}
    @begin[scale-button::value-changed]{signal}
      @begin{pre}
lambda (button value)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:scale-button} widget which received the
          signal.}
        @entry[value]{The double float with the new value.}
      @end{simple-table}
      The signal is emitted when the value field has changed.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:scale-button-new}
  @see-slot{gtk:scale-button-adjustment}
  @see-slot{gtk:scale-button-icons}
  @see-slot{gtk:scale-button-size}
  @see-slot{gtk:scale-button-value}
  @see-class{gtk:volume-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:scale-button-adjustment --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment" 'scale-button) t)
 "The @code{adjustment} property of type @class{gtk:adjustment} (Read / Write)
  @br{}
  The adjustment that contains the current value of the scale button.")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-adjustment)
      "Accessor"
      (documentation 'scale-button-adjustment 'function)
 "@version{#2023-03-24}
  @syntax{(gtk:scale-button-adjustment object object) => adjustment}
  @syntax{(setf (gtk:scale-button-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:scale-button]{adjustment} slot of the
    @class{gtk:scale-button} class.
  @end{short}
  The @fun{gtk:scale-button-adjustment} function gets the adjustment associated
  with the scale button. The @setf{gtk:scale-button-adjustment} function sets
  the adjustment.
  @see-class{gtk:scale-button}
  @see-class{gtk:adjustment}")

;;; --- gtk:scale-button-icons -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icons" 'scale-button) t)
 "The @code{icons} property of type @type{g:strv-t} (Read / Write) @br{}
  The names of the icons to be used by the scale button. The first item in
  the list will be used in the button when the current value is the lowest
  value, the second item for the highest value. All the subsequent icons will
  be used for all the other values, spread evenly over the range of values.
  If there is only one icon name in the icons list, it will be used for all
  the values. If only two icon names are in the icons array, the first one
  will be used for the bottom 50% of the scale, and the second one for the
  top 50%. It is recommended to use at least 3 icons so that the scale button
  reflects the current value of the scale better for the users.")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-icons)
      "Accessor"
      (documentation 'scale-button-icons 'function)
 "@version{#2025-06-18}
  @syntax{(gtk:scale-button-icons object object) => icons}
  @syntax{(setf (gtk:scale-button-icons object) icons)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[icons]{a list of strings for the icon names}
  @begin{short}
    Accessor of the @slot[gtk:scale-button]{icons} slot of the
    @class{gtk:scale-button} class.
  @end{short}
  The @fun{gtk:scale-button-icons} function gets the icons to be used by the
  scale button. The @setf{gtk:scale-button-icons} function sets the icons.

  The names of the icons to be used by the scale button. The first item in
  the list will be used in the button when the current value is the lowest
  value, the second item for the highest value. All the subsequent icons will
  be used for all the other values, spread evenly over the range of values.
  If there is only one icon name in the icons list, it will be used for all
  the values. If only two icon names are in the icons array, the first one
  will be used for the bottom 50% of the scale, and the second one for the
  top 50%. It is recommended to use at least 3 icons so that the scale button
  reflects the current value of the scale better for the users.
  @see-class{gtk:scale-button}")

;;; --- gtk:scale-button-size --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size" 'scale-button) t)
 "The @code{size} property of type @sym{gtk:icon-size} (Read / Write) @br{}
  The icon size. @br{}
  Default value: @val[gtk:icon-size]{:small-toolbar}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-size)
      "Accessor"
      (documentation 'scale-button-size 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:scale-button-size object object) => size}
  @syntax{(setf (gtk:scale-button-size object) size)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[size]{a value of the @sym{gtk:icon-size} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:scale-button]{size} slot of the
    @class{gtk:scale-button} class.
  @end{short}
  The icon size.
  @see-class{gtk:scale-button}
  @see-symbol{gtk:icon-size}")

;;; --- gtk:scale-button-value -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value" 'scale-button) t)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  The value of the scale. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-value)
      "Accessor"
      (documentation 'scale-button-value 'function)
 "@version{#2025-06-18}
  @syntax{(gtk:scale-button-value object) => value}
  @syntax{(setf (gtk:scale-button-value object) value)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[value]{a number coerced to a double float for the value of the
    scale button}
  @begin{short}
    Accessor of the @slot[gtk:scale-button]{value} slot of the
    @class{gtk:scale-button} class.
  @end{short}
  The @fun{gtk:scale-button-value} function gets the current value of the scale
  button. The @setf{gtk:scale-button-value} function sets the current value.

  If the value is outside the minimum or maximum range values, it will be
  clamped to fit inside them. The scale button emits the
  @sig[gtk:scale-button]{value-changed} signal if the value changes.
  @see-class{gtk:scale-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline scale-button-new))

(defun scale-button-new (size min max step icons)
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[size]{a value of the @sym{gtk:icon-size} enumeration}
  @argument[min]{a number coerced to a double float for the minimum value of
    the scale}
  @argument[max]{a number coerced to a double float for the maximum value of
    the scale}
  @argument[step]{a number coerced to a double float for the stepping of the
    value when a scroll-wheel event, or up/down arrow event occurs}
  @argument[icons]{a list of strings for the icon names, or @code{nil} if you
    want to set the list later with the @fun{gtk:scale-button-icons} function}
  @return{The new @class{gtk:scale-button} widget.}
  @begin{short}
    Creates a scale button, with a range between @arg{min} and @arg{max}, with
    a stepping of @arg{step}.
  @end{short}
  @see-class{gtk:scale-button}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:scale-button-icons}"
  (let ((button (make-instance 'scale-button
                               :size size
                               :adjustment
                               (make-instance 'adjustment
                                              :value min
                                              :lower min
                                              :upper max
                                              :step-increment step
                                              :page-increment (* 10 step)
                                              :page-size 0))))
    (when icons
      (setf (scale-button-icons button) icons))
    button))

(export 'scale-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_popup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_button_get_popup" scale-button-popup)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-18}
  @argument[button]{a @class{gtk:scale-button} widget}
  @return{The @class{gtk:widget} widget with the popup of the scale button.}
  @short{Retrieves the popup of the scale button.}
  @see-class{gtk:scale-button}
  @see-class{gtk:widget}"
  (button (g:object scale-button)))

(export 'scale-button-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_plus_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_button_get_plus_button" scale-button-plus-button)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-18}
  @argument[button]{a @class{gtk:scale-button} widget}
  @begin{return}
    The @class{gtk:widget} widget with the plus button of the scale button.
  @end{return}
  @short{Retrieves the plus button of the scale button.}
  @see-class{gtk:scale-button}
  @see-function{gtk:sacle-button-minus-button}"
  (button (g:object scale-button)))

(export 'scale-button-plus-button)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_minus_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_button_get_minus_button" scale-button-minus-button)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-18}
  @argument[button]{a @class{gtk:scale-button} widget}
  @begin{return}
    The @class{gtk:widget} widget with the minus button of the scale button.
  @end{return}
  @short{Retrieves the minus button of the scale button.}
  @see-class{gtk:scale-button}
  @see-function{gtk:scale-button-plus-button}"
  (button (g:object scale-button)))

(export 'scale-button-minus-button)

;;; --- End of file gtk3.scale-button.lisp -------------------------------------
