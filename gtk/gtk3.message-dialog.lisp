;;; ----------------------------------------------------------------------------
;;; gtk3.message-dialog.lisp
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
;;; GtkMessageDialog
;;;
;;;     A convenient message window
;;;
;;; Types and Values
;;;
;;;     GtkMessageDialog
;;;     GtkMessageType
;;;     GtkButtonsType
;;;
;;; Accessors
;;;
;;;     gtk_message_dialog_set_image
;;;     gtk_message_dialog_get_image
;;;     gtk_message_dialog_get_message_area
;;;
;;; Functions
;;;
;;;     gtk_message_dialog_new
;;;     gtk_message_dialog_new_with_markup
;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup
;;;
;;; Properties
;;;
;;;     buttons
;;;     image
;;;     message-area
;;;     message-type
;;;     secondary-text
;;;     secondary-use-markup
;;;     text
;;;     use-markup
;;;
;;; Style Properties
;;;
;;;     message-border
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ╰── GtkMessageDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMessageDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMessageType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkMessageType" message-type
  (:export t
   :type-initializer "gtk_message_type_get_type")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'message-type)
      "GEnum"
      (liber:symbol-documentation 'message-type)
 "@version{2024-3-16}
  @begin{declaration}
(gobject:define-g-enum \"GtkMessageType\" message-type
  (:export t
   :type-initializer \"gtk_message_type_get_type\")
  (:info 0)
  (:warning 1)
  (:question 2)
  (:error 3)
  (:other 4))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:info]{Informational message.}
      @entry[:warning]{Nonfatal warning message.}
      @entry[:question]{Question requiring a choice.}
      @entry[:error]{Fatal error message.}
      @entry[:other]{None of the above, does not get an icon.}
    @end{table}
  @end{values}
  @begin{short}
    The type of message being displayed in the message dialog.
  @end{short}
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; GtkButtonsType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkButtonsType" buttons-type
  (:export t
   :type-initializer "gtk_buttons_type_get_type")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))

#+liber-documentation
(setf (liber:alias-for-symbol 'buttons-type)
      "GEnum"
      (liber:symbol-documentation 'buttons-type)
 "@version{2024-3-16}
  @begin{declaration}
(gobject:define-g-enum \"GtkButtonsType\" buttons-type
  (:export t
   :type-initializer \"gtk_buttons_type_get_type\")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No buttons at all.}
      @entry[:ok]{An OK button.}
      @entry[:close]{A Close button.}
      @entry[:cancel]{A Cancel button.}
      @entry[:yes-no]{Yes and No buttons.}
      @entry[:ok-cancel]{OK and Cancel buttons.}
    @end{table}
  @end{values}
  @begin{short}
    Prebuilt sets of buttons for the dialog.
  @end{short}
  If none of these choices are appropriate, simply use the @code{:none} value
  and call the @fun{gtk:dialog-add-buttons} function to add your own buttons.

  Please note that the @code{:ok}, @code{:yes-no} and @code{:ok-cancel} values
  are discouraged by the @url[https://developer.gnome.org/hig/]{GNOME Human
  Interface Guidelines}.
  @see-class{gtk:message-dialog}
  @see-function{gtk:dialog-add-buttons}")

;;; ----------------------------------------------------------------------------
;;; struct GtkMessageDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMessageDialog" message-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_message_dialog_get_type")
  ((buttons
    message-dialog-buttons
    "buttons" "GtkButtonsType" nil nil)
   (image
    message-dialog-image
    "image" "GtkWidget" t t)
   (message-area
    message-dialog-message-area
    "message-area" "GtkWidget" t nil)
   (message-type
    message-dialog-message-type
    "message-type" "GtkMessageType" t t)
   (secondary-text
    message-dialog-secondary-text
    "secondary-text" "gchararray" t t)
   (secondary-use-markup
    message-dialog-secondary-use-markup
    "secondary-use-markup" "gboolean" t t)
   (text
    message-dialog-text
    "text" "gchararray" t t)
   (use-markup
    message-dialog-use-markup
    "use-markup" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'message-dialog 'type)
 "@version{2024-3-16}
  @begin{short}
    The @class{gtk:message-dialog} widget presents a dialog with some message
    text.
  @end{short}
  It is simply a convenience widget. You could construct the equivalent of a
  message dialog from a @class{gtk:dialog} widget without too much effort, but
  the @class{gtk:message-dialog} widget saves typing.

  @image[messagedialog]{Figure: GtkMessageDialog}

  One difference from the @class{gtk:dialog} widget is that the message dialog
  sets the @slot[gtk:window]{skip-taskbar-hint} property to @em{true}, so that
  the message dialog is hidden from the taskbar by default.

  The easiest way to do a modal message dialog is to use the
  @fun{gtk:dialog-run} function, though you can also pass in the @code{:modal}
  flag of type @symbol{gtk:dialog-flags}, the @fun{gtk:dialog-run} function
  automatically makes the message dialog modal and waits for the user to respond
  to it. The @fun{gtk:dialog-run} function returns when any message dialog
  button is clicked.
  @begin{examples}
    A modal message dialog.
    @begin{pre}
(let ((dialog (gtk:message-dialog-new main-window
                                      '(:destroy-with-parent)
                                      :error
                                      :close
                                      \"Error loading file ~s\"
                                      filename)))
  (gtk:dialog-run dialog)
  (gtk:widget-destroy dialog))
    @end{pre}
    You might do a non-modal message dialog as follows.
    @begin{pre}
(let ((dialog (gtk:message-dialog-new main-window
                                      '(:destroy-with-parent)
                                      :error
                                      :close
                                      \"Error loading file ~s\"
                                      filename)))
  ;; Destroy the dialog when the user responds to it
  (g:signal-connect dialog \"response\"
                    (lambda (dialog response-id)
                      (declare (ignore response-id))
                      (gtk:widget-destroy dialog)))
  ... )
    @end{pre}
  @end{examples}
  @begin[GtkMessageDialog as GtkBuildable]{dictionary}
    The @class{gtk:message-dialog} implementation of the @class{gtk:buildable}
    interface exposes the message area as an internal child with the name
    @code{message_area}.
  @end{dictionary}
  @begin[Style Properties]{dictionary}
    @begin[code]{table}
      @begin[message-border]{entry}
        The @code{message-border} style property of type @code{:int} (Read)@br{}
        Width of border around the label and image in the message dialog. @br{}
        Allowed values: >= 0 @br{}
        Default value: 12
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:message-dialog-new}
  @see-constructor{gtk:message-dialog-new-with-markup}
  @see-slot{gtk:message-dialog-buttons}
  @see-slot{gtk:message-dialog-image}
  @see-slot{gtk:message-dialog-message-area}
  @see-slot{gtk:message-dialog-message-type}
  @see-slot{gtk:message-dialog-secondary-text}
  @see-slot{gtk:message-dialog-secondary-use-markup}
  @see-slot{gtk:message-dialog-text}
  @see-slot{gtk:message-dialog-use-markup}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:message-dialog-buttons ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buttons" 'message-dialog) t)
 "The @code{buttons} property of type @symbol{gtk:buttons-type}
  (Write / Construct Only) @br{}
  The buttons shown in the message dialog. @br{}
  @em{Note:} This property is not accessible from the Lisp binding. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-buttons)
      "Accessor"
      (documentation 'message-dialog-buttons 'function)
 "@version{2024-3-16}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{buttons} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  @begin{notes}
    This property is not accessible from the Lisp binding.
  @end{notes}
  @see-class{gtk:message-dialog}")

;;; --- gtk:message-dialog-image -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "image" 'message-dialog) t)
 "The @code{image} property of type @class{gtk:widget} (Read / Write) @br{}
  The image for the message dialog. @br{}
  @em{Warning:} The @code{image} property has been deprecated since version
  3.12 and should not be used in newly written code. Use the @class{gtk:dialog}
  widget to create dialogs with images.")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-image)
      "Accessor"
      (documentation 'message-dialog-image 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-image object) => image}
  @syntax{(setf (gtk:message-dialog-image object) image)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[image]{a @class{gtk:image} widget}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{image} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:message-dialog-image} function has been deprecated since
    version 3.12 and should not be used in newly written code. Use the
    @class{gtk:dialog} widget to create dialogs with images.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:dialog}
  @see-class{gtk:image}")

;;; --- gtk:message-dialog-message-area ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-area"
                                               'message-dialog) t)
 "The @code{message-area} property of type @class{gtk:widget} (Read) @br{}
  The @class{gtk:box} widget of @code{:vertical} orientation that corresponds
  to the message area of the message dialog. See the
  @fun{gtk:message-dialog-message-area} function for a detailed description of
  the message area.")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-message-area)
      "Accessor"
      (documentation 'message-dialog-message-area 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-message-area object) => area}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[area]{a @class{gtk:box} widget of @code{:vertical} orientation}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{message-area} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The @fun{gtk:message-dialog-message-area} function returns the @class{gtk:box}
  widget with @code{:vertical} orientation corresponding to the \"message area\"
  in the message dialog. This is the box where the primary and secondary labels
  of the message dialog are packed.

  You can add your own extra content to that box and it will appear below those
  labels. See the @fun{gtk:dialog-content-area} function for the corresponding
  function in the parent @class{gtk:dialog} class.
  @see-class{gtk:message-dialog}
  @see-class{gtk:box}
  @see-class{gtk:dialog}
  @see-function{gtk:dialog-content-area}")

;;; --- gtk:message-dialog-message-type ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-type"
                                               'message-dialog) t)
 "The @code{message-type} property of type @symbol{gtk:message-type}
  (Read / Write / Construct) @br{}
  The type of the message. The type is used to determine the image that is
  shown in the message dialog, unless the image is explicitly set by the
  @code{image} property. @br{}
  Default value: @code{:info}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-message-type)
      "Accessor"
      (documentation 'message-dialog-message-type 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-message-type object) => type}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[type]{a value of the @symbol{gtk:message-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{message-type} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The type of the message. The type is used to determine the image that is
  shown in the message dialog, unless the image is explicitly set by the
  @code{image} property.
  @see-class{gtk:message-dialog}
  @see-symbol{gtk:message-type}")

;;; --- gtk:message-dialog-secondary-text --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-text"
                                               'message-dialog) t)
 "The @code{secondary-text} property of type @code{:string} (Read / Write) @br{}
  The secondary text of the message dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-secondary-text)
      "Accessor"
      (documentation 'message-dialog-secondary-text 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-secondary-text object) => text}
  @syntax{(setf (gtk:message-dialog-secondary-text object) text)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[text]{a string with the secondary text of the message dialog}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{secondary-text} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The secondary text of the message dialog.
  @see-class{gtk:message-dialog}
  @see-function{gtk:message-dialog-format-secondary-text}")

;;; --- gtk:message-dialog-secondary-use-markup --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-use-markup"
                                               'message-dialog) t)
 "The @code{secondary-use-markup} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the secondary text of the message dialog includes Pango markup.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-secondary-use-markup)
      "Accessor"
      (documentation 'message-dialog-secondary-use-markup 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-secondary-use-markup object) => setting}
  @syntax{(setf (gtk:message-dialog-secondary-use-markup object) setting)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[setting]{a boolean whether to use Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{secondary-use-markup} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  @em{True} if the secondary text of the message dialog includes Pango markup.
  @see-class{gtk:message-dialog}
  @see-function{gtk:message-dialog-format-secondary-markup}")

;;; --- gtk:message-dialog-text ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'message-dialog) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The primary text of the message dialog. If the dialog has a secondary text,
  this will appear as the title. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-text)
      "Accessor"
      (documentation 'message-dialog-text 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-text object) => text}
  @syntax{(setf (gtk:message-dialog-text object) text)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[text]{a string with the primary text of the message dialog}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{text} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The primary text of the message dialog. If the dialog has a secondary text,
  this will appear as the title.
  @see-class{gtk:message-dialog}")

;;; --- gtk:message-dialog-use-markup ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup" 'message-dialog) t)
 "The @code{use-markup} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the primary text of the message dialog includes Pango markup.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-use-markup)
      "Accessor"
      (documentation 'message-dialog-use-markup 'function)
 "@version{2024-3-16}
  @syntax{(gtk:message-dialog-use-markup object) => setting}
  @syntax{(setf (gtk:message-dialog-use-markup object) setting)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[setting]{a boolean whether to use Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{use-markup} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  @em{True} if the primary text of the message dialog includes Pango markup.
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun message-dialog-new (parent flags type buttons message &rest args)
 #+liber-documentation
 "@version{2024-4-8}
  @argument[parent]{a @class{gtk:window} transient parent, or @code{nil} for
    none}
  @argument[flags]{a value of the @symbol{gtk:dialog-flags} flags}
  @argument[type]{a value of the @symbol{gtk:message-type} enumeration for the
    type of the message}
  @argument[buttons]{set of values of the @symbol{gtk:buttons-type} enumeration
    for the buttons to use}
  @argument[message]{a format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @return{The new @class{gtk:message-dialog} widget.}
  @begin{short}
    Creates a new message dialog, which is a simple dialog with some text the
    user may want to see.
  @end{short}
  When the user clicks a button a @code{\"response\"} signal is emitted with
  response IDs from the @symbol{gtk:response-type} enumeration. See the
  @class{gtk:dialog} widget for more details.
  @see-class{gtk:message-dialog}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:dialog-flags}
  @see-symbol{gtk:message-type}
  @see-symbol{gtk:buttons-type}
  @see-symbol{gtk:response-type}"
  (let ((dialog (make-instance 'message-dialog
                               :message-type type
                               :buttons buttons)))
    (if message
        (setf (message-dialog-text dialog)
        (apply #'format nil message args)))
    (if parent
        (setf (window-transient-for dialog) parent))
    (if (member :modal flags)
        (setf (window-modal dialog) t))
    (if (member :destroy-with-parent flags)
        (setf (window-destroy-with-parent dialog) t))
    dialog))

(export 'message-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new_with_markup ()
;;; ----------------------------------------------------------------------------

(defun message-dialog-new-with-markup (parent
                                       flags type buttons message &rest args)
 #+liber-documentation
 "@version{2024-4-8}
  @argument[parent]{a @class{gtk:window} transient parent, or @code{nil} for
    none}
  @argument[flags]{a value of the @symbol{gtk:dialog-flags} flags}
  @argument[type]{a value of the @symbol{gtk:message-type} enumeration}
  @argument[buttons]{set of values of the @symbol{gtk:buttons-type} enumeration
    for the buttons to use}
  @argument[message]{a format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @return{The new @class{gtk:message-dialog} widget.}
  @begin{short}
    Creates a new message dialog, which is a simple dialog with some text which
    is marked up with the Pango text markup language.
  @end{short}
  When the user clicks a button a @code{\"response\"} signal is emitted with
  response IDs from the @symbol{gtk:response-type} enumeration. See the
  @class{gtk:dialog} class for more details.

  Special XML characters in the message arguments passed to this function will
  automatically be escaped as necessary. Usually this is what you want, but if
  you have an existing Pango markup string that you want to use literally as the
  label, then you need to use the @fun{gtk:message-dialog-set-markup} function
  instead, since you cannot pass the markup string either as the format, it
  might contain '%' characters, or as a string argument.
  @begin{examples}
    @begin{pre}
(let ((dialog (gtk:message-dialog-new main-window
                                      '(:destroy-with-parent)
                                      :error
                                      close
                                      nil)))
  (gtk:message-dialog-set-markup dialog markup)
  ... )
    @end{pre}
  @end{examples}
  @see-class{gtk:message-dialog}
  @see-class{gtk:window}
  @see-class{gtk:dialog}
  @see-symbol{gtk:dialog-flags}
  @see-symbol{gtk:message-type}
  @see-symbol{gtk:buttons-type}
  @see-symbol{gtk:response-type}
  @see-function{gtk:message-dialog-set-markup}"
  (let ((dialog (make-instance 'message-dialog
                               :use-markup t
                               :message-type type
                               :buttons buttons)))
    (if message
        (setf (message-dialog-text dialog)
        (apply #'format nil message args)))
    (if parent
        (setf (window-transient-for dialog) parent))
    (if (member :modal flags)
        (setf (window-modal dialog) t))
    (if (member :destroy-with-parent flags)
        (setf (window-destroy-with-parent dialog) t))
    dialog))

(export 'message-dialog-new-with-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_markup ()
;;; ----------------------------------------------------------------------------

(defun message-dialog-set-markup (dialog text)
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[text]{a markup string, see Pango markup format}
  @begin{short}
    Sets the text of the message dialog to be @arg{text}, which is marked
    up with the Pango text markup language.
  @end{short}
  @see-class{gtk:message-dialog}"
  (setf (message-dialog-use-markup dialog) t
        (message-dialog-text dialog) text))

(export 'message-dialog-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline message-dialog-format-secondary-text))

(defun message-dialog-format-secondary-text (dialog message &rest args)
 #+liber-documentation
 "@version{#2024-3-16}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[message]{a format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @begin{short}
    Sets the secondary text of the message dialog to be @arg{message} with
    the arguments in @arg{args}.
  @end{short}
  Note that setting a secondary text makes the primary text become bold, unless
  you have provided explicit markup.
  @see-class{gtk:message-dialog}
  @see-function{gtk:message-dialog-format-secondary-markup}"
  (setf (message-dialog-secondary-text dialog)
        (apply #'format nil message args)))

(export 'message-dialog-format-secondary-text)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_markup ()
;;; ----------------------------------------------------------------------------

(declaim (inline message-dialog-format-secondary-markup))

(defun message-dialog-format-secondary-markup (dialog message &rest args)
 #+liber-documentation
 "@version{#2024-3-16}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[message]{a markup string, see Pango markup format, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @begin{short}
    Sets the secondary text of the message dialog to be @arg{message} with
    the arguments in @arg{args}, which is marked up with the Pango text markup
    language.
  @end{short}
  Note that setting a secondary text makes the primary text become bold,
  unless you have provided explicit markup. Due to an oversight in the C
  implementation, this function does not escape special XML characters like the
  @fun{gtk:message-dialog-new-with-markup} function does.
  @see-class{gtk:message-dialog}
  @see-function{gtk:message-dialog-new-with-markup}
  @see-function{gtk:message-dialog-format-secondary-text}"
  (setf (message-dialog-secondary-use-markup dialog) t
        (message-dialog-secondary-text dialog)
        (apply #'format nil message args)))

(export 'message-dialog-format-secondary-markup)

;;; --- End of file gtk3.message-dialog.lisp -----------------------------------
