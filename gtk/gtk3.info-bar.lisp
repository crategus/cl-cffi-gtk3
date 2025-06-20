;;; ----------------------------------------------------------------------------
;;; gtk3.info-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkInfoBar
;;;
;;;     Report important messages to the user
;;;
;;; Types and Values
;;;
;;;     GtkInfoBar
;;;
;;; Functions
;;;
;;;     gtk_info_bar_new
;;;     gtk_info_bar_new_with_buttons
;;;     gtk_info_bar_add_action_widget
;;;     gtk_info_bar_add_button
;;;     gtk_info_bar_add_buttons
;;;     gtk_info_bar_set_response_sensitive
;;;     gtk_info_bar_set_default_response
;;;     gtk_info_bar_response
;;;     gtk_info_bar_set_message_type                       Accessor
;;;     gtk_info_bar_get_message_type                       Accessor
;;;     gtk_info_bar_get_action_area
;;;     gtk_info_bar_get_content_area
;;;     gtk_info_bar_get_show_close_button                  Accessor
;;;     gtk_info_bar_set_show_close_button                  Accessor
;;;     gtk_info_bar_get_revealed                           Accessor
;;;     gtk_info_bar_set_revealed                           Accessor
;;;
;;; Properties
;;;
;;;     message-type
;;;     revealed
;;;     show-close-button
;;;
;;; Style Properties
;;;
;;;     action-area-border
;;;     button-spacing
;;;     content-area-border
;;;     content-area-spacing
;;;
;;; Signals
;;;
;;;     close
;;;     response
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkInfoBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkInfoBar implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkInfoBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkInfoBar" info-bar
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_info_bar_get_type")
  ((message-type
    info-bar-message-type
    "message-type" "GtkMessageType" t t)
   (revealed
    info-bar-revealed
    "revealed" "gboolean" t t)
   (show-close-button
    info-bar-show-close-button
    "show-close-button" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'info-bar 'type)
 "@version{#2025-06-17}
  @begin{short}
    The @class{gtk:info-bar} widget can be used to show messages to the user
    without showing a dialog.
  @end{short}
  It is often temporarily shown at the top or bottom of a document. In contrast
  to the @class{gtk:dialog} widget, which has a horizontal action area at the
  bottom, the info bar has a vertical action area at the side.

  @image[info-bar]{Figure: GtkInfoBar}

  The API of the @class{gtk:info-bar} widget is very similar to the
  @class{gtk:dialog} widget, allowing you to add buttons to the action area
  with the @fun{gtk:info-bar-add-button} or @fun{gtk:info-bar-new-with-buttons}
  functions. The sensitivity of action widgets can be controlled with the
  @fun{gtk:info-bar-set-response-sensitive} function. To add widgets to the main
  content area of an info bar, use the @fun{gtk:info-bar-content-area} function
  and add your widgets to the container.

  Similar to @class{gtk:message-dialog} widget, the contents of an info bar can
  by classified as error message, warning, informational message, etc, by using
  the @fun{gtk:info-bar-message-type} function. GTK uses the message type to
  determine the background color of the message area.
  @begin[Examples]{dictionary}
    Simple info bar usage.
    @begin{pre}
(defun create-info-bar ()
  (let* ((info-bar (make-instance 'gtk:info-bar))
         (message (make-instance 'gtk:label :label \"Some text\"))
         (content (gtk:info-bar-content-area info-bar)))
    ;; Hide the info by default
    (setf (gtk:widget-no-show-all info-bar) t)
    ;; Add a label for the message to the content of the info bar
    (gtk:container-add content message)
    (gtk:widget-show message)
    ;; Add buttons to the info bar
    (gtk:info-bar-add-buttons info-bar \"gtk-ok\" 1 \"gtk-cancel\" 2)
    ;; Connect a signal handler to the info bar
    (g:signal-connect info-bar \"response\"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:widget-hide widget)))
    info-bar))

(defun show-error-message (info-bar message type)
  (let* ((content (gtk:info-bar-content-area info-bar))
         (label (first (gtk:container-children content))))
    (setf (gtk:label-label label) message)
    (setf (gtk:info-bar-message-type info-bar) type)
    (gtk:widget-show info-bar)))
    @end{pre}
  @end{dictionary}
  @begin[GtkInfoBar as GtkBuildable]{dictionary}
    The @class{gtk:info-bar} implementation of the @class{gtk:buildable}
    interface exposes the content area and action area as internal children
    with the names @code{content_area} and @code{action_area}.

    The @class{gtk:info-bar} implementation supports a custom
    @code{<action-widgets>} element, which can contain multiple
    @code{<action-widget>} elements. The @code{response} attribute specifies a
    numeric response, and the content of the element is the ID of widget, which
    should be a child of the dialogs action area.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:info-bar} implementation has a single CSS node with name
    @code{infobar}. The node may get one of the @code{.info}, @code{.warning},
    @code{.error} or @code{.question} style classes, depending on the message
    type.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[action-area-border]{entry}
        The @code{action-area-border} style property of type @code{:int} (Read)
        @br{}
        The width of the border around the action area of the info bar. @br{}
        @em{Warning:} The @code{action-area-border} style property has been
        deprecated since version 3.6 and should not be used in newly written
        code. Use the @fun{gtk:container-border-width} function. @br{}
        Allowed values: >= 0 @br{}
        Default value: 5
      @end{entry}
      @begin[button-spacing]{entry}
        The @code{button-spacing} style property of type @code{:int} (Read)@br{}
        The spacing between buttons in the action area of the info bar. @br{}
        @em{Warning:} The @code{button-spacing} style property has been
        deprecated since version 3.6 and should not be used in newly written
        code. Use the @fun{gtk:box-spacing} function. @br{}
        Allowed values: >= 0 @br{}
        Default value: 6
      @end{entry}
      @begin[content-area-border]{entry}
        The @code{content-area-border} style property of type @code{:int} (Read)
        @br{}
        The width of the border around the content content area of the info bar.
        @br{}
        @em{Warning:} The @code{content-area-border} style property has been
        deprecated since version 3.6 and should not be used in newly written
        code. Use the @fun{gtk:container-border-width} function. @br{}
        Allowed values: >= 0 @br{}
        Default value: 8
      @end{entry}
      @begin[content-area-spacing]{entry}
        The @code{content-area-spacing} style property of type @code{:int}
        (Read) @br{}
        The default spacing used between elements of the content area of the
        info bar. @br{}
        @em{Warning:} The @code{content-area-spacing} style property has been
        deprecated since version 3.6 and should not be used in newly written
        code. Use the @fun{gtk:box-spacing} function. @br{}
        Allowed values: >= 0 @br{}
        Default value: 16
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"close\" signal}
      @begin{pre}
lambda (infobar)    :action
      @end{pre}
      @begin[code]{table}
        @entry[infobar]{The @class{gtk:info-bar} widget on which the signal is
          emitted.}
      @end{table}
      The signal is a keybinding signal which gets emitted when the user uses a
      keybinding to dismiss the info bar. The default binding for this signal
      is the @kbd{Escape} key.
    @subheading{The \"response\" signal}
      @begin{pre}
lambda (infobar response)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[infobar]{The @class{gtk:info-bar} widget on which the signal is
          emitted.}
        @entry[response]{The integer with the response ID.}
      @end{table}
      Emitted when an action widget is clicked or the application programmer
      calls the @fun{gtk:dialog-response} function. The @arg{response} argument
      depends on which action widget was clicked.
  @end{dictionary}
  @see-constructor{gtk:info-bar-new}
  @see-constructor{gtk:info-bar-new-with-buttons}
  @see-slot{gtk:info-bar-message-type}
  @see-slot{gtk:info-bar-revealed}
  @see-slot{gtk:info-bar-show-close-button}
  @see-class{gtk:statusbar}
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:info-bar-message-type ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-type" 'info-bar) t)
 "The @code{message-type} property of type @symbol{gtk:message-type}
  (Read / Write / Construct) @br{}
  The type of the message. The type may be used to determine the appearance of
  the info bar. @br{}
  Default value: @code{:info}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-message-type)
      "Accessor"
      (documentation 'info-bar-message-type 'function)
 "@version{#2023-03-20}
  @syntax{(gtk:info-bar-message-type object) => message-type}
  @syntax{(setf (gtk:info-bar-message-type object) message-type)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[message-type]{a value of the @symbol{gtk:message-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:info-bar]{message-type} slot of the
    @class{gtk:info-bar} class.
  @end{short}
  The @fun{gtk:info-bar-message-type} function returns the message type of the
  message area. The @setf{gtk:info-bar-message-type} function sets the message
  type.

  GTK uses this type to determine what color to use when drawing the message
  area.
  @see-class{gtk:info-bar}")

;;; --- gtk:info-bar-revealed --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "revealed" 'info-bar) t)
 "The @code{revealed} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether the action bar shows its contents or not. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-revealed)
      "Accessor"
      (documentation 'info-bar-revealed 'function)
 "@version{#2023-03-20}
  @syntax{(gtk:info-bar-revealed object) => revealed}
  @syntax{(setf (gtk:info-bar-revealed object) revealed)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[revealed]{a boolean whether the action bar shows its contents}
  @begin{short}
    Accessor of the @slot[gtk:info-bar]{revealed} slot of the
    @class{gtk:info-bar} class.
  @end{short}
  The @fun{gtk:info-bar-revealed} function returns the current value of the
  @slot[gtk:info-bar]{revealed} property. The @setf{gtk:info-bar-revealed}
  function sets the property.

  This will cause the info bar to show up with a slide-in transition. Note that
  this property does not automatically show the info bar and thus will not have
  any effect if it is invisible.
  @see-class{gtk:info-bar}")

;;; --- gtk:info-bar-show-close-button -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-close-button"
                                               'info-bar) t)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether to include a standard Close button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-show-close-button)
      "Accessor"
      (documentation 'info-bar-show-close-button 'function)
 "@version{#2023-03-20}
  @syntax{(gtk:info-bar-show-close-button object) => setting}
  @syntax{(setf (gtk:info-bar-show-close-button object) setting)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[setting]{@em{true} to include a Close button}
  @begin{short}
    Accessor of the @slot[gtk:info-bar]{show-close-button} slot of the
    @class{gtk:info-bar} class.
  @end{short}
  The @fun{gtk:info-bar-show-close-button} function returns whether the widget
  will display a standard Close button. If @em{true}, a standard Close button
  is shown. When clicked it emits the @code{:close} response.
  @see-class{gtk:info-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline info-bar-new))

(defun info-bar-new ()
 #+liber-documentation
 "@version{#2025-06-17}
  @return{The new @class{gtk:info-bar} widget.}
  @short{Creates a new info bar.}
  @see-class{gtk:info-bar}"
  (make-instance 'info-bar))

(export 'info-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_new_with_buttons
;;; ----------------------------------------------------------------------------

(defun info-bar-new-with-buttons (&rest args)
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[args]{first a string for the text and second an integer for the
    response ID for each button, then more pairs for each button}
  @return{The new @class{gtk:info-bar} widget.}
  @short{Creates a new info bar with buttons.}
  Button text/response ID pairs should be listed. Button text can be some
  arbitrary text. A response ID can be any positive number, or one of the values
  in the @symbol{gtk:response-type} enumeration. If the user clicks one of these
  dialog buttons, the info bar will emit the @code{\"response\"} signal with the
  corresponding response ID.
  @see-class{gtk:info-bar}
  @see-symbol{gtk:response-type}"
  (let ((infobar (make-instance 'info-bar)))
     (apply #'info-bar-add-buttons infobar args)
     infobar))

(export 'info-bar-new-with-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_add_action_widget" info-bar-add-action-widget)
    :void
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[child]{an activatable @class{gtk:widget} widget}
  @argument[response]{an integer for the response ID for @arg{child}}
  @begin{short}
    Add an activatable widget to the action area of an info bar, connecting a
    signal handler that will emit the @code{\"response\"} signal on the message
    area when the widget is activated.
  @end{short}
  The widget is appended to the end of the message areas action area.
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (child (g:object widget))
  (response :int))

(export 'info-bar-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_add_button" info-bar-add-button) (g:object widget)
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[text]{a string for the text of the button}
  @argument[response]{an integer for the response ID for the button}
  @return{The @class{gtk:button} widget that was added.}
  @begin{short}
    Adds a button with the given text, and sets things up so that clicking the
    button will emit the @code{\"response\"} signal with the given response ID.
  @end{short}
  The button is appended to the end of the action area of the info bar. The
  button widget is returned, but usually you do not need it.
  @see-class{gtk:info-bar}
  @see-class{gtk:button}
  @see-function{gtk:info-bar-add-buttons}"
  (infobar (g:object info-bar))
  (text :string)
  (response :int))

(export 'info-bar-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_buttons
;;; ----------------------------------------------------------------------------

(defun info-bar-add-buttons (infobar &rest args)
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[args]{first a string for a button text and second an integer for
    a response ID, then more pairs for each button}
  @begin{short}
    Adds more buttons, same as calling the @fun{gtk:info-bar-add-button}
    function repeatedly.
  @end{short}
  Each button must have both text and a response ID.
  @see-class{gtk:info-bar}
  @see-function{gtk:info-bar-add-button}"
  (loop for (text response) on args by #'cddr
        do (info-bar-add-button infobar text response)))

(export 'info-bar-add-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_set_response_sensitive
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_set_response_sensitive"
               info-bar-set-response-sensitive) :void
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer for a response ID}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls the @fun{gtk:widget-sensitive} function for each widget in the action
    area of the info bar with the given response ID.
  @end{short}
  A convenient way to sensitize/desensitize dialog buttons.
  @see-class{gtk:info-bar}
  @see-function{gtk:widget-sensitive}"
  (infobar (g:object info-bar))
  (response :int)
  (setting :boolean))

(export 'info-bar-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_set_default_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_set_default_response"
               info-bar-set-default-response) :void
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer for a response ID}
  @begin{short}
    Sets the last widget in the action area of the info bar with the given
    response ID as the default widget for the info bar.
  @end{short}
  Pressing the @kbd{Enter} key usually activates the default widget.

  Note that this function currently requires the info bar to be added to a
  widget hierarchy.
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (response :int))

(export 'info-bar-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_response" info-bar-response) :void
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer for a response ID}
  @short{Emits the @code{\"response\"} signal with the given response ID.}
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (response :int))

(export 'info-bar-response)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_get_action_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_get_action_area" info-bar-action-area)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-17}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @return{The @class{gtk:widget} object with the action area.}
  @short{Returns the action area of the info bar.}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}"
  (infobar (g:object info-bar)))

(export 'info-bar-action-area)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_get_content_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_get_content_area" info-bar-content-area)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-03-20}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @return{The @class{gtk:box} content area.}
  @short{Returns the content area of the info bar.}
  @see-class{gtk:info-bar}
  @see-class{gtk:box}"
  (infobar (g:object info-bar)))

(export 'info-bar-content-area)

;;; --- End of file gtk3.info-bar.lisp -----------------------------------------
