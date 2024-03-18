;;; ----------------------------------------------------------------------------
;;; gtk3.font-button.lisp
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
;;; GtkFontButton
;;;
;;;     A button to launch a font chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkFontButton
;;;
;;; Accessors
;;;
;;;     gtk_font_button_set_font_name                      Accessor
;;;     gtk_font_button_get_font_name                      Accessor
;;;     gtk_font_button_set_show_style                     Accessor
;;;     gtk_font_button_get_show_style                     Accessor
;;;     gtk_font_button_set_show_size                      Accessor
;;;     gtk_font_button_get_show_size                      Accessor
;;;     gtk_font_button_set_use_font                       Accessor
;;;     gtk_font_button_get_use_font                       Accessor
;;;     gtk_font_button_set_use_size                       Accessor
;;;     gtk_font_button_get_use_size                       Accessor
;;;     gtk_font_button_set_title                          Accessor
;;;     gtk_font_button_get_title                          Accessor
;;;
;;; Functions
;;;
;;;     gtk_font_button_new
;;;     gtk_font_button_new_with_font
;;;
;;; Properties
;;;
;;;     font-name
;;;     show-size
;;;     show-style
;;;     title
;;;     use-font
;;;     use-size
;;;
;;; Signals
;;;
;;;     font-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkFontButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFontButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable, GtkActivatable and GtkFontChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFontButton" font-button
  (:superclass button
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActionable"
                 "GtkActivatable"
                 "GtkFontChooser")
    :type-initializer "gtk_font_button_get_type")
  ((font-name
    font-button-font-name
    "font-name" "gchararray" t t)
   (show-size
    font-button-show-size
    "show-size" "gboolean" t t)
   (show-style
    font-button-show-style
    "show-style" "gboolean" t t)
   (title
    font-button-title
    "title" "gchararray" t t)
   (use-font
    font-button-use-font
    "use-font" "gboolean" t t)
   (use-size
    font-button-use-size
    "use-size" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'font-button 'type)
 "@version{2023-6-16}
  @begin{short}
    The @class{gtk:font-button} widget is a button which displays the currently
    selected font and allows to open a font chooser dialog to change the font.
  @end{short}
  It is a suitable widget for selecting a font in a preference dialog.

  @image[font-button]{GtkFontButton}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:font-button} implementation has a single CSS node with name
    @code{button} and @code{.font} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"font-set\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      The signal is emitted when the user selects a font. When handling this
      signal, use the @fun{gtk:font-chooser-font} function to find out which
      font was just selected. Note that this signal is only emitted when the
      user changes the font. If you need to react to programmatic font changes
      as well, use the \"notify::font-name\" signal.
      @begin[code]{table}
        @entry[widget]{The @class{gtk:font-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:font-button-new}
  @see-constructor{gtk:font-button-new-with-font}
  @see-slot{gtk:font-button-font-name}
  @see-slot{gtk:font-button-show-size}
  @see-slot{gtk:font-button-show-style}
  @see-slot{gtk:font-button-title}
  @see-slot{gtk:font-button-use-font}
  @see-slot{gtk:font-button-use-size}
  @see-class{gtk:font-chooser}
  @see-class{gtk:font-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-button-font-name ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-name" 'font-button) t)
 "The @code{font-name} property of type @code{:string} (Read / Write) @br{}
  The name of the currently selected font. @br{}
  @em{Warning:} The @code{font-name} property has been deprecated since version
  3.22 and should not be used in newly written code. Use the
  @slot[gtk:font-chooser]{font} property instead. @br{}
  Default value: \"Sans 12\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-font-name)
      "Accessor"
      (documentation 'font-button-font-name 'function)
 "@version{2023-6-16}
  @syntax{(gtk:font-button-font-name object) => fontname}
  @syntax{(setf (gtk:font-button-font-name object) fontname)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[fontname]{a string with the name of the font to display in the font
    chooser dialog}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{font-name} slot of the
    @class{gtk:font-button} class.
  @end{short}
  The @fun{gtk:font-name-button-font-name} function retrieves the name of the
  currently selected font. The @setf{gtk:font-button-font-name} function sets
  or updates the currently displayed font in the font picker dialog.


  This name includes style and size information as well. If you want to render
  something with the font, use this string with the
  @fun{pango:font-description-from-string} function. If you are interested in
  peeking certain values, family name, style, size, weight, just query these
  properties from the @class{pango:font-description} instance.
  @begin[Warning]{dictionary}
    The @fun{gtk:font-button-font-name} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gtk:font-chooser-font} function instead.
  @end{dictionary}
  @see-class{gtk:font-button}
  @see-class{gtk:font-chooser}
  @see-class{pango:font-description}
  @see-function{pango:font-description-from-string}
  @see-function{gtk:font-chooser-font}")

;;; --- gtk:font-button-show-size ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-size" 'font-button) t)
 "The @code{show-size} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the selected font size will be shown in
  the label. For a more WYSIWYG way to show the selected size, see the
  @code{use-size} property. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-show-size)
      "Accessor"
      (documentation 'font-button-show-size 'function)
 "@version{2023-6-16}
  @syntax{(gtk:font-button-show-size object) => show-size}
  @syntax{(setf (gtk:font-button-show-size object) show-size)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[show-size]{@em{true} if font size should be displayed in dialog}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{show-size} slot of the
    @class{gtk:font-button} class.
  @end{short}
  If @arg{show-size} is @em{true}, the font size will be displayed along with
  the name of the selected font.
  @see-class{gtk:font-button}")

;;; --- gtk:font-button-show-style ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-style" 'font-button) t)
 "The @code{show-style} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the name of the selected font style will
  be shown in the label. For a more WYSIWYG way to show the selected style, see
  the @code{use-font} property. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-show-style)
      "Accessor"
      (documentation 'font-button-show-style 'function)
 "@version{2023-6-16}
  @syntax{(gtk:font-button-show-style object) => show-style}
  @syntax{(setf (gtk:font-button-show-style object) show-style)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[show-style]{@em{true} if font style should be displayed in label}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{show-style} slot of the
    @class{gtk:font-button} class.
  @end{short}
  If @arg{show-style} is @em{true}, the font style will be displayed along with
  name of the selected font.
  @see-class{gtk:font-button}")

;;; --- gtk:font-button-title --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'font-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the font chooser dialog. @br{}
  Default value: \"Pick a Font\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-title)
      "Accessor"
      (documentation 'font-button-title 'function)
 "@version{2023-6-16}
  @syntax{(gtk:font-button-title object) => title}
  @syntax{(setf (gtk:font-button-title object) title)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[title]{a string containing the font chooser dialog title}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{title} slot of the
    @class{gtk:font-button} class.
  @end{short}
  The @fun{gtk:font-button-title} function retrieves the title of the font
  chooser dialog. The @setf{gtk:font-button-title} function sets the title for
  the font chooser dialog.
  @see-class{gtk:font-button}
  @see-class{gtk:font-chooser-dialog}")

;;; --- gtk:font-button-use-font -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-font" 'font-button) t)
 "The @code{use-font} property of type  @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn in the selected
  font. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-use-font)
      "Accessor"
      (documentation 'font-button-use-font 'function)
 "@version{2023-6-16}
  @syntax{(gtk:font-button-title object) => use-font}
  @syntax{(setf (gtk:font-button-title object) use-font)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[use-font]{if @em{true}, font name will be written using font chosen}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{use-font} slot of the
    @class{gtk:font-button} class.
  @end{short}
  If @arg{use-font} is @em{true}, the font name will be written using the
  selected font.
  @see-class{gtk:font-button}")

;;; --- gtk:font-button-use-size -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-size" 'font-button) t)
 "The @code{use-size} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn with the
  selected font size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-use-size)
      "Accessor"
      (documentation 'font-button-use-size 'function)
 "@version{2023-6-16}
  @syntax{(gtk:font-button-use-size object) => use-size}
  @syntax{(setf (gtk:font-button-use-size object) use-size)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[use-size]{if @em{true}, the font name will be written using the
    selected size}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{use-size} slot of the
    @class{gtk:font-button} class.
  @end{short}
  If @arg{use-size} is @em{true}, the font name will be written using the
  selected size.
  @see-class{gtk:font-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-button-new))

(defun font-button-new ()
 #+liber-documentation
 "@version{2023-6-16}
  @return{A new @class{gtk:font-button} widget.}
  @short{Creates a new font picker widget.}
  @see-class{gtk:font-button}"
  (make-instance 'font-button))

(export 'font-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new_with_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-button-new-with-font))

(defun font-button-new-with-font (fontname)
 #+liber-documentation
 "@version{2023-6-16}
  @argument[fontname]{a string with the name of the font to display in the font
    chooser dialog}
  @return{A new @class{gtk:font-button} widget.}
  @short{Creates a new font picker widget.}
  @see-class{gtk:font-button}"
  (make-instance 'font-button
                 :font-name fontname))

(export 'font-button-new-with-font)

;;; --- End of file gtk3.font-button.lisp --------------------------------------
