;;; ----------------------------------------------------------------------------
;;; gtk.color-button.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK+ library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; GtkColorButton
;;;
;;;     A button to launch a color selection dialog
;;;
;;; Types and Values
;;;
;;;     GtkColorButton
;;;
;;; Functions
;;;
;;;     gtk_color_button_new
;;;     gtk_color_button_new_with_color
;;;     gtk_color_button_new_with_rgba
;;;     gtk_color_button_set_color                         Accessor
;;;     gtk_color_button_get_color                         Accessor
;;;     gtk_color_button_set_alpha                         Accessor
;;;     gtk_color_button_get_alpha                         Accessor
;;;     gtk_color_button_set_rgba                          Accessor
;;;     gtk_color_button_get_rgba                          Accessor
;;;     gtk_color_button_set_use_alpha                     Accessor
;;;     gtk_color_button_get_use_alpha                     Accessor
;;;     gtk_color_button_set_title                         Accessor
;;;     gtk_color_button_get_title                         Accessor
;;;
;;; Properties
;;;
;;;     alpha
;;;     color
;;;     rgba
;;;     show-editor
;;;     title
;;;     use-alpha
;;;
;;; Signals
;;;
;;;     color-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkColorButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable, GtkActivatable and GtkColorChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorButton" color-button
  (:superclass button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable"
                "GtkColorChooser")
   :type-initializer "gtk_color_button_get_type")
  ((alpha
    color-button-alpha
    "alpha" "guint" t t)
   (color
    color-button-color
    "color" "GdkColor" t t)
   (rgba
    color-button-rgba
    "rgba" "GdkRGBA" t t)
   #+gtk-3-20
   (show-editor
    color-button-show-editor
    "show-editor" "gboolean" t t)
   (title
    color-button-title
    "title" "gchararray" t t)
   (use-alpha
    color-button-use-alpha
    "use-alpha" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'color-button 'type)
 "@version{#2021-1-24}
  @begin{short}
    The @sym{gtk:color-button} widget is a button which displays the currently
    selected color and allows to open a color selection dialog to change the
    color.
  @end{short}
  It is a suitable widget for selecting a color in a preference dialog.

  @image[color-button]{}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:color-button} implementation has a single CSS node with name
    @arg{button}. To differentiate it from a plain @class{gtk:button} widget,
    it gets the @code{.color} style class.
  @end{dictionary}
  @begin[Example]{dictionary}
    The example shows a color button. The button is initialized with the color
    \"Blue\". The handler for the \"color-set\" signal prints the selected
    color on the console.
    @begin{pre}
(defun example-color-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :title \"Example Color Button\"
                                 :border-width 12
                                 :default-width 250
                                 :default-height 200))
          (button (make-instance 'gtk:color-button
                                 :rgba (gdk:rgba-parse \"Blue\"))))
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g:signal-connect button \"color-set\"
         (lambda (widget)
           (let ((rgba (gtk:color-chooser-rgba widget)))
             (format t \"Selected color is ~A~%\" (gdk:rgba-to-string rgba)))))
      (gtk:container-add window button)
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-set\" signal}
      @begin{pre}
lambda (button)    : Run First
      @end{pre}
      The \"color-set\" signal is emitted when the user selects a color. When
      handling this signal, use the function @fun{gtk:color-chooser-rgba} to
      find out which color was just selected. Note that this signal is only
      emitted when the user changes the color. If you need to react to
      programmatic color changes as well, use the \"notify::color\" signal.
      @begin[code]{table}
        @entry[button]{The @sym{gtk:color-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:color-button-alpha}
  @see-slot{gtk:color-button-color}
  @see-slot{gtk:color-button-rgba}
  @see-slot{gtk:color-button-show-editor}
  @see-slot{gtk:color-button-title}
  @see-slot{gtk:color-button-use-alpha}
  @see-class{gtk:color-chooser}
  @see-class{gtk:color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- color-button-alpha -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "alpha" 'color-button) t)
 "The @code{alpha} property of tpye @code{:uint} (Read / Write) @br{}
  The selected opacity value, 0 fully transparent, 65535 fully opaque. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 65535")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-alpha)
      "Accessor"
      (documentation 'color-button-alpha 'function)
 "@version{#2020-5-23}
  @syntax[]{(gtk:color-button-alpha object) => alpha)}
  @syntax[]{(setf (gtk:color-button-alpha object) alpha)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{alpha} slot of the
    @class{gtk:color-button} class.
  @end{short}

  The slot access function @sym{gtk:color-button-alpha} returns the current
  alpha value. The slot access function @sym{(setf gtk:color-button-alpha)}
  sets the current opacity to be @arg{alpha}.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-button-alpha} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-chooser-rgba} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-function{gtk:color-chooser-rgba}")

;;; --- color-button-color -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "color" 'color-button) t)
 "The @code{color} property of type @struct{gdk:color} (Read / Write) @br{}
  The selected color. @br{}
  @em{Warning:} The @code{color} property has been deprecated since version 3.4
  and should not be used in newly written code. Use the @code{rgba}
  property instead.")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-color)
      "Accessor"
      (documentation 'color-button-color 'function)
 "@version{#2021-1-23}
  @syntax[]{(gtk:color-button-color object) => color)}
  @syntax[]{(setf (gtk:color-button-color object) color)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[color]{a @struct{gdk:color} to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{color} slot of the
    @class{gtk:color-button} class.
  @end{short}

  The slot access function @sym{gtk:color-button-color} gets the current color
  in the color button. The slot access function
  @sym{(setf gtk:color-button-color)} sets the current color to be @arg{color}.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-button-color} function is deprecated and should not be
    used in newly written code. Use the @fun{gtk:color-chooser-rgba} function
    instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-struct{gdk:color}
  @see-function{gtk:color-chooser-rgba}")

;;; --- color-button-rgba --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'color-button) t)
 "The @code{rgba} property of type @struct{gdk:rgba} (Read / Write) @br{}
  The selected RGBA color.")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-rgba)
      "Accessor"
      (documentation 'color-button-rgba 'function)
 "@version{#2021-1-23}
  @syntax[]{(gtk:color-button-rgba object) => rgba)}
  @syntax[]{(setf (gtk:color-button-rgba object) rgba)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[rgba]{a @struct{gdk:rgba} color to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{rgba} slot of the
    @class{gtk:color-button} class.
  @end{short}

  The slot access function @sym{gtk:color-button-rgba} gets the current color
  in the color button. The slot access function @sym{(setf gtk:color-button)}
  sets the current color to be @arg{rgba}.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-button-rgba} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-chooser-rgba} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-struct{gdk:rgba}
  @see-function{gtk:color-chooser-rgba}")

;;; --- color-button-show-editor -----------------------------------------------

#+(and gtk-3-20 liber-documentation)
(setf (documentation (liber:slot-documentation "show-editor" 'color-button) t)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  Set this property to @em{true} to skip the palette in the dialog and go
  directly to the color editor. This property should be used in cases where the
  palette in the editor would be redundant, such as when the color button is
  already part of a palette. Since 3.20 @br{}
  Default value: @em{false}")

#+(and gtk-3-20 liber-documentation)
(setf (liber:alias-for-function 'color-button-show-editor)
      "Accessor"
      (documentation 'color-button-show-editor 'function)
 "@version{#2023-1-22}
  @syntax[]{(gtk:color-button-show-editor object) => show-editor)}
  @syntax[]{(setf (gtk:color-button-show-editor object) show-editor)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[show-editor]{a boolean whether to skip the palette in the dialog}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{show-editor} slot of the
    @class{gtk:color-button} class.
  @end{short}
  Set this property to @em{true} to skip the palette in the dialog and go
  directly to the color editor. This property should be used in cases where the
  palette in the editor would be redundant, such as when the color button is
  already part of a palette.

  Since 3.20
  @see-class{gtk:color-button}")

;;; --- color-button-title -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'color-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the color selection dialog. @br{}
  Default value: \"Pick a Color\"")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-title)
      "Accessor"
      (documentation 'color-button-title 'function)
 "@version{#2021-1-23}
  @syntax[]{(gtk:color-button-title object) => title)}
  @syntax[]{(setf (gtk:color-button-title object) title)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[title]{a string containing the window title}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{title} slot of the
    @class{gtk:color-button} class.
  @end{short}

  The slot access function @sym{gtk:color-button-title} gets the title of the
  color selection dialog. The slot access function
  @sym{(setf gtk:color-button-title)} sets the title.
  @see-class{gtk:color-button}")

;;; color-button-use-alpha -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-alpha"
                                               'color-button) t)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the color swatch on the button is
  rendered against a checkerboard background to show its opacity and the
  opacity slider is displayed in the color selection dialog. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-use-alpha)
      "Accessor"
      (documentation 'color-button-use-alpha 'function)
 "@version{#2020-5-23}
  @syntax[]{(gtk:color-button-use-alpha object) => use-alpha)}
  @syntax[]{(setf (gtk:color-button-use-alpha object) use-alpha)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[use-alpha]{@em{true} if the color button should use alpha channel,
    @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{use-alpha} slot of the
    @class{gtk:color-button} class.
  @end{short}

  Sets whether or not the color button should use the alpha channel.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-button-use-alpha} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-chooser-use-alpha} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-function{gtk:color-chooser-use-alpha}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new))

(defun color-button-new ()
 #+liber-documentation
 "@version{#2021-1-23}
  @return{A new @class{gtk:color-button} widget.}
  @short{Creates a new color button.}

  This returns a widget in the form of a small button containing a swatch
  representing the current selected color. When the button is clicked, a
  color-selection dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk:color-button}
  @see-function{gtk:color-button-new-with-rgba}"
  (make-instance 'color-button))

(export 'color-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_color ()
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new-with-color))

(defun color-button-new-with-color (color)
 #+liber-documentation
 "@version{#2021-1-23}
  @argument[color]{a @struct{gdk:color} to set the current color with}
  @return{A new @class{gtk:color-button} widget.}
  @short{Creates a new color button.}
  @begin[Warning]{dictionary}
    The @sym{gtk:color-button-new-with-color} function has been deprecated
    since version 3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-button-new-with-rgba} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-struct{gdk:color}
  @see-function{gtk:color-button-new-with-rgba}"
  (make-instance 'color-button
                 :color color))

(export 'color-button-new-with-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new-with-rgba))

(defun color-button-new-with-rgba (rgba)
 #+liber-documentation
 "@version{#2021-1-23}
  @argument[rgba]{a @struct{gdk:rgba} color to set the current color with}
  @return{A new @class{gtk:color-button} widget.}
  @short{Creates a new color button with the given RGBA color.}
  @see-class{gtk:color-button}

  This returns a widget in the form of a small button containing a swatch
  representing the current selected color. When the button is clicked, a
  color-selection dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk:color-button}
  @see-struct{gdk:rgba}
  @see-function{gtk:color-button-new}"
  (make-instance 'color-button
                 :rgba rgba))

(export 'color-button-new-with-rgba)

;;; --- End of file gtk3.color-button.lisp -------------------------------------
