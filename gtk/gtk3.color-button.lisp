;;; ----------------------------------------------------------------------------
;;; gtk3.color-button.lisp
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
;;; GtkColorButton
;;;
;;;     A button to launch a color selection dialog
;;;
;;; Types and Values
;;;
;;;     GtkColorButton
;;;
;;; Accessors
;;;
;;;     gtk_color_button_set_color
;;;     gtk_color_button_get_color
;;;     gtk_color_button_set_alpha
;;;     gtk_color_button_get_alpha
;;;     gtk_color_button_set_rgba
;;;     gtk_color_button_get_rgba
;;;     gtk_color_button_set_use_alpha
;;;     gtk_color_button_get_use_alpha
;;;     gtk_color_button_set_title
;;;     gtk_color_button_get_title
;;;
;;; Functions
;;;
;;;     gtk_color_button_new
;;;     gtk_color_button_new_with_color
;;;     gtk_color_button_new_with_rgba
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
;;; GtkColorButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColorButton" color-button
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
 "@version{2025-07-14}
  @begin{short}
    The @class{gtk:color-button} widget is a button which displays the currently
    selected color and allows to open a color selection dialog to change the
    color.
  @end{short}
  It is a suitable widget for selecting a color in a preference dialog.

  @image[color-button]{Figure: GtkColorButton}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:color-button} implementation has a single CSS node with name
    @arg{button}. To differentiate it from a plain @class{gtk:button} widget,
    it gets the @code{.color} style class.
  @end{dictionary}
  @begin[Examples]{dictionary}
    The example shows a color button. The button is initialized with the color
    \"Blue\". The handler for the @sig[gtk:color-button]{color-set} signal
    prints the selected color on the console.
    @begin{pre}
(defun example-color-button ()
  (gtk:within-main-loop
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
                          (gtk:leave-gtk-main)))
      (g:signal-connect button \"color-set\"
         (lambda (widget)
           (let ((rgba (gtk:color-chooser-rgba widget)))
             (format t \"Selected color is ~A~%\" (gdk:rgba-to-string rgba)))))
      (gtk:container-add window button)
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[color-button::color-set]{signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:color-button} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted when the user selects a color. When handling this
      signal, use the @fun{gtk:color-chooser-rgba} function to find out which
      color was just selected. Note that this signal is only emitted when the
      user changes the color. If you need to react to programmatic color
      changes as well, use the @sig[g:object]{notify::color} signal.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:color-button-new}
  @see-constructor{gtk:color-button-new-with-color}
  @see-constructor{gtk:color-button-new-with-rgba}
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

;;; --- gtk:color-button-alpha -------------------------------------------------

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
 "@version{2023-06-14}
  @syntax{(gtk:color-button-alpha object) => alpha)}
  @syntax{(setf (gtk:color-button-alpha object) alpha)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{alpha} slot of the
    @class{gtk:color-button} class.
  @end{short}
  The @fun{gtk:color-button-alpha} function returns the current alpha value.
  The @setf{gtk:color-button-alpha} function sets the current opacity to be
  @arg{alpha}.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-button-alpha} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-chooser-rgba} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-function{gtk:color-chooser-rgba}")

;;; --- gtk:color-button-color -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "color" 'color-button) t)
 "The @code{color} property of type @struct{gdk:color} (Read / Write) @br{}
  The selected color. @br{}
  @em{Warning:} The @code{color} property has been deprecated since version 3.4
  and should not be used in newly written code. Use the @code{rgba} property
  instead.")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-color)
      "Accessor"
      (documentation 'color-button-color 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-button-color object) => color)}
  @syntax{(setf (gtk:color-button-color object) color)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[color]{a @struct{gdk:color} color to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{color} slot of the
    @class{gtk:color-button} class.
  @end{short}
  The @fun{gtk:color-button-color} function gets the current color in the color
  button. The @setf{gtk:color-button-color} function sets the current color to
  be @arg{color}.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-button-color} function is deprecated and should not be
    used in newly written code. Use the @fun{gtk:color-chooser-rgba} function
    instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-struct{gdk:color}
  @see-function{gtk:color-chooser-rgba}")

;;; --- gtk:color-button-rgba --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'color-button) t)
 "The @code{rgba} property of type @struct{gdk:rgba} (Read / Write) @br{}
  The selected RGBA color.")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-rgba)
      "Accessor"
      (documentation 'color-button-rgba 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-button-rgba object) => rgba)}
  @syntax{(setf (gtk:color-button-rgba object) rgba)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[rgba]{a @struct{gdk:rgba} color to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{rgba} slot of the
    @class{gtk:color-button} class.
  @end{short}
  The @fun{gtk:color-button-rgba} function gets the current color in the color
  button. The @setf{gtk:color-button} function sets the current color to be
  @arg{rgba}.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-button-rgba} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-chooser-rgba} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-struct{gdk:rgba}
  @see-function{gtk:color-chooser-rgba}")

;;; --- gtk:color-button-show-editor -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-editor" 'color-button) t)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  Set this property to @em{true} to skip the palette in the dialog and go
  directly to the color editor. This property should be used in cases where the
  palette in the editor would be redundant, such as when the color button is
  already part of a palette. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-show-editor)
      "Accessor"
      (documentation 'color-button-show-editor 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-button-show-editor object) => show-editor)}
  @syntax{(setf (gtk:color-button-show-editor object) show-editor)}
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
  @see-class{gtk:color-button}")

;;; --- gtk:color-button-title -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'color-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the color selection dialog. @br{}
  Default value: \"Pick a Color\"")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-title)
      "Accessor"
      (documentation 'color-button-title 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-button-title object) => title)}
  @syntax{(setf (gtk:color-button-title object) title)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[title]{a string containing the window title}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{title} slot of the
    @class{gtk:color-button} class.
  @end{short}
  The @fun{gtk:color-button-title} function gets the title of the color
  selection dialog. The @setf{gtk:color-button-title} function sets the title.
  @see-class{gtk:color-button}")

;;; gtk:color-button-use-alpha -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-alpha" 'color-button) t)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the color swatch on the button is
  rendered against a checkerboard background to show its opacity and the
  opacity slider is displayed in the color selection dialog. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-use-alpha)
      "Accessor"
      (documentation 'color-button-use-alpha 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-button-use-alpha object) => use-alpha)}
  @syntax{(setf (gtk:color-button-use-alpha object) use-alpha)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[use-alpha]{@em{true} if the color button should use alpha channel,
    @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{use-alpha} slot of the
    @class{gtk:color-button} class.
  @end{short}
  Sets whether or not the color button should use the alpha channel.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-button-use-alpha} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @fun{gtk:color-chooser-use-alpha} function instead.
  @end{dictionary}
  @see-class{gtk:color-button}
  @see-function{gtk:color-chooser-use-alpha}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new))

(defun color-button-new ()
 #+liber-documentation
 "@version{2023-06-14}
  @return{The new @class{gtk:color-button} widget.}
  @short{Creates a new color button.}
  This returns a widget in the form of a small button containing a swatch
  representing the current selected color. When the button is clicked, a
  color selection dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk:color-button}
  @see-function{gtk:color-button-new-with-rgba}"
  (make-instance 'color-button))

(export 'color-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_color
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new-with-color))

(defun color-button-new-with-color (color)
 #+liber-documentation
 "@version{2023-06-14}
  @argument[color]{a @struct{gdk:color} to set the current color with}
  @return{The new @class{gtk:color-button} widget.}
  @short{Creates a new color button.}
  @begin[Warning]{dictionary}
    The @fun{gtk:color-button-new-with-color} function has been deprecated
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
;;; gtk_color_button_new_with_rgba
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new-with-rgba))

(defun color-button-new-with-rgba (rgba)
 #+liber-documentation
 "@version{2023-06-14}
  @argument[rgba]{a @struct{gdk:rgba} color to set the current color with}
  @return{The new @class{gtk:color-button} widget.}
  @short{Creates a new color button with the given RGBA color.}
  This returns a widget in the form of a small button containing a swatch
  representing the current selected color. When the button is clicked, a
  color selection dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk:color-button}
  @see-struct{gdk:rgba}
  @see-function{gtk:color-button-new}"
  (make-instance 'color-button
                 :rgba rgba))

(export 'color-button-new-with-rgba)

;;; --- End of file gtk3.color-button.lisp -------------------------------------
