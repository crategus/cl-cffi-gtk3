;;; ----------------------------------------------------------------------------
;;; gtk3.dialog.lisp
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
;;; GtkDialog
;;;
;;;     Create popup windows
;;;
;;; Types and Values
;;;
;;;     GtkDialog
;;;     GtkDialogFlags
;;;     GtkResponseType
;;;
;;; Functions
;;;
;;;     gtk_dialog_new
;;;     gtk_dialog_new_with_buttons
;;;     gtk_dialog_run
;;;     gtk_dialog_response
;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_add_action_widget
;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_get_action_area
;;;     gtk_dialog_get_content_area
;;;     gtk_dialog_get_header_bar
;;;     gtk_alternative_dialog_button_order                 not exported
;;;     gtk_dialog_set_alternative_button_order             not exported
;;;     gtk_dialog_set_alternative_button_order_from_array  not exported
;;;
;;; Properties
;;;
;;;     use-header-bar
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
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkDialog
;;;                             ├── GtkAboutDialog
;;;                             ├── GtkAppChooserDialog
;;;                             ├── GtkColorChooserDialog
;;;                             ├── GtkColorSelectionDialog
;;;                             ├── GtkFileChooserDialog
;;;                             ├── GtkFontChooserDialog
;;;                             ├── GtkFontSelectionDialog
;;;                             ├── GtkMessageDialog
;;;                             ├── GtkPageSetupUnixDialog
;;;                             ├── GtkPrintUnixDialog
;;;                             ╰── GtkRecentChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDialogFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkDialogFlags" dialog-flags
  (:export t
   :type-initializer "gtk_dialog_flags_get_type")
  (:modal               #.(ash 1 0))
  (:destroy-with-parent #.(ash 1 1))
  (:use-header-bar      #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'dialog-flags)
      "GFlags"
      (liber:symbol-documentation 'dialog-flags)
 "@version{2024-03-16}
  @begin{declaration}
(gobject:define-gflags \"GtkDialogFlags\" dialog-flags
  (:export t
   :type-initializer \"gtk_dialog_flags_get_type\")
  (:modal               #.(ash 1 0))
  (:destroy-with-parent #.(ash 1 1))
  (:use-header-bar      #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:modal]{Make the constructed dialog modal, see the
        @fun{gtk:window-modal} function.}
      @entry[:destroy-with-parent]{Destroy the dialog when its parent is
        destroyed, see the @fun{gtk:window-destroy-with-parent} function.}
      @entry[:use-header-bar]{Create the dialog with actions in the header bar
        instead of an action area.}
    @end{table}
  @end{values}
  @begin{short}
    Flags used to influence the @class{gtk:dialog} widget construction.
  @end{short}
  @see-class{gtk:dialog}
  @see-function{gtk:window-modal}
  @see-function{gtk:window-destroy-with-parent}")

;;; ----------------------------------------------------------------------------
;;; GtkResponseType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkResponseType" response-type
  (:export t
   :type-initializer "gtk_response_type_get_type")
  (:none -1)
  (:reject -2)
  (:accept -3)
  (:delete-event -4)
  (:ok -5)
  (:cancel -6)
  (:close -7)
  (:yes -8)
  (:no -9)
  (:apply -10)
  (:help -11))

#+liber-documentation
(setf (liber:alias-for-symbol 'response-type)
      "GEnum"
      (liber:symbol-documentation 'response-type)
 "@version{2024-03-16}
  @begin{declaration}
(gobject:define-genum \"GtkResponseType\" response-type
  (:export t
   :type-initializer \"gtk_response_type_get_type\")
  (:none -1)
  (:reject -2)
  (:accept -3)
  (:delete-event -4)
  (:ok -5)
  (:cancel -6)
  (:close -7)
  (:yes -8)
  (:no -9)
  (:apply -10)
  (:help -11))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{Returned if an action widget has no response ID, or if the
        dialog gets programmatically hidden or destroyed.}
      @entry[:reject]{Generic response ID, not used by GTK dialog.}
      @entry[:accept]{Generic response ID, not used by GTK dialog.}
      @entry[:delete-event]{Returned if the dialog is deleted.}
      @entry[:ok]{Returned by OK buttons in GTK dialog.}
      @entry[:cancel]{Returned by Cancel buttons in GTK dialog.}
      @entry[:close]{Returned by Close buttons in GTK dialog.}
      @entry[:yes]{Returned by Yes buttons in GTK dialog.}
      @entry[:no]{Returned by No buttons in GTK dialog.}
      @entry[:apply]{Returned by Apply buttons in GTK dialog.}
      @entry[:help]{Returned by Help buttons in GTK dialog.}
    @end{table}
  @end{values}
  @begin{short}
    Predefined values for use as response IDs in the @fun{gtk:dialog-add-button}
    function.
  @end{short}
  All predefined values are negative, GTK leaves positive values for application
  defined response IDs.
  @see-class{gtk:dialog}
  @see-function{gtk:dialog-add-button}")

;;; ----------------------------------------------------------------------------
;;; GtkDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkDialog" dialog
  (:superclass window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_dialog_get_type")
  ((use-header-bar
    dialog-use-header-bar
    "use-header-bar" "gint" t t)))

#+liber-documentation
(setf (documentation 'dialog 'type)
 "@version{2025-06-05}
  @begin{short}
    Dialogs are a convenient way to prompt the user for a small amount of input,
    for example, to display a message, ask a question, or anything else that
    does not require extensive effort on the part of the user.
  @end{short}

  GTK treats a dialog as a window split vertically. The top section is known as
  the \"content area\" and is a @class{gtk:box} widget with a @code{:vertical}
  orientation. This is where widgets such as a @class{gtk:label} widget or a
  @class{gtk:entry} widget should be packed. The bottom area is known as the
  \"action area\". This is generally used for packing buttons into the dialog
  which may perform functions such as Cancel, OK, or Apply.

  The @class{gtk:dialog} widget is created with a call to to the
  @fun{gtk:dialog-new} or @fun{gtk:dialog-new-with-buttons} functions. The
  @fun{gtk:dialog-new-with-buttons} function is recommended. It allows you to
  set the dialog title, some convenient flags, and add simple buttons.

  If the dialog is a newly created dialog, the two primary areas of the dialog
  can be accessed through the @fun{gtk:dialog-content-area} and
  @fun{gtk:dialog-action-area} functions.

  A modal dialog, that is, one which freezes the rest of the application from
  user input, can be created by calling the @fun{gtk:window-modal} function on
  the dialog. When using the @fun{gtk:dialog-new-with-buttons} function you can
  also pass the @code{:modal} flag of the @symbol{gtk:dialog-flags} flags to
  make a dialog modal.

  If you add buttons to a dialog using the @fun{gtk:dialog-new-with-buttons},
  @fun{gtk:dialog-add-button}, @fun{gtk:dialog-add-buttons}, or
  @fun{gtk:dialog-add-action-widget} functions, clicking the button will emit
  a @code{\"response\"} signal with a response ID that you specified. GTK will
  never assign a meaning to positive response IDs. These are entirely
  user-defined. But for convenience, you can use the response IDs in the
  @symbol{gtk:response-type} enumeration. These all have values less than zero.
  If a dialog receives a delete event, the @code{\"response\"} signal will be
  emitted with a @code{:delete-event} response ID.

  If you want to block waiting for a dialog to return before returning control
  flow to your code, you can call the @fun{gtk:dialog-run} function. This
  function enters a recursive main loop and waits for the user to respond to
  the dialog, returning the response ID corresponding to the button the user
  clicked.

  For the simple dialog in the following example, in reality you would probably
  use a @class{gtk:message-dialog} widget to save yourself some effort. But you
  would need to create the dialog contents manually if you had more than a
  simple message in the dialog.
  @begin[Examples]{dictionary}
    Simple @class{gtk:dialog} widget usage:
    @begin{pre}
;; Function to open a dialog displaying the message provided.
(defun quick-message (window message)
  (let (;; Create the widgets
        (dialog (gtk:dialog-new-with-buttons \"Message\"
                                             window
                                             '(:destroy-with-parent)
                                             \"_OK\"
                                             :none))
        (label (gtk:label-new message)))
    ;; Ensure that the dialog is destroyed when the user responds.
    (g:signal-connect dialog \"response\"
                      (lambda (dialog id)
                        (declare (ignore id))
                        (gtk:widget-destroy dialog)))
    ;; Add the label, and show everything we have added to the dialog.
    (gtk:container-add (gtk:dialog-content-area dialog) label)
    (gtk:widget-show-all dialog)))
    @end{pre}
    You can use a dialog as a toplevel window from Lisp code. The
    following code shows a complete example of a function which displays a
    message in a dialog. In this case you have to connect to the
    @code{\"response\"} signal. It is not possible to use the
    @fun{gtk:dialog-run} and @fun{gtk:dialog-response} functions for this
    toplevel dialog. In the Lisp binding your program will hang, when using
    this functions to run the dialog and to get the response.

    A toplevel dialog which can be called from any Lisp code:
    @begin{pre}
(defun example-dialog-toplevel (message)
 (let ((response nil))
   (gtk:within-main-loop
    (let (;; Create the widgets
          (dialog (gtk:dialog-new-with-buttons \"Demo Toplevel Dialog\"
                                               nil ; No Parent window
                                               '(:modal)
                                               \"_OK\"
                                               :none
                                               \"_Cancel\"
                                               :cancel))
          (label (gtk:label-new message)))
      ;; Signal handler for the dialog to handle the \"destroy\" signal.
      (g:signal-connect dialog \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Quit the main loop and destroy the thread.
                          (gtk:leave-gtk-main)))
      ;; Get the response and destroy the dialog.
      (g:signal-connect dialog \"response\"
                        (lambda (dialog id)
                          (setf response id)
                          (gtk:widget-destroy dialog)))
      ;; Add the label, and show everything we have added to the dialog.
      (gtk:container-add (gtk:dialog-content-area dialog) label)
      (gtk:widget-show-all dialog)))
    ;; Wait until the dialog is destroyed.
    (gtk:join-gtk-main)
    (when response
      (format t \"The response ID is ~A\" response))))
    @end{pre}
  @end{dictionary}
  @begin[GtkDialog as GtkBuildable]{dictionary}
    The @class{gtk:dialog} implementation of the @class{gtk:buildable} interface
    exposes the content area and action area as internal children with the names
    @code{vbox} and @code{action_area}.

    The @class{gtk:dialog} implementation supports a custom
    @code{<action-widgets>} element, which can contain multiple
    @code{<action-widget>} elements. The @code{\"response\"} attribute specifies
    a numeric response, and the content of the element is the ID of the widget,
    which should be a child of the action area of the dialog. To mark a
    response as default, set the @code{\"default\"} attribute of the
    @code{<action-widget>} element to true.

    The @class{gtk:dialog} implementation supports adding action widgets by
    specifying @code{\"action\"} as the @code{\"type\"} attribute of a
    @code{<child>} element. The widget will be added either to the action area
    or the headerbar of the dialog, depending on the @code{use-header-bar}
    property. The response ID has to be associated with the action widget using
    the @code{<action-widgets>} element.

    @b{Example:} A @class{gtk:dialog} UI definition fragment.
    @begin{pre}
<object class=\"GtkDialog\" id=\"dialog1\">
  <child type=\"action\">
    <object class=\"GtkButton\" id=\"button_cancel\"/>
  </child>
  <child type=\"action\">
    <object class=\"GtkButton\" id=\"button_ok\">
      <property name=\"can-default\">True</property>
    </object>
  </child>
  <action-widgets>
    <action-widget response=\"cancel\">button_cancel</action-widget>
    <action-widget response=\"ok\" default=\"true\">button_ok</action-widget>
  </action-widgets>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[action-area-border]{entry}
        The @code{action-area-border} style property of type @code{:int} (Read)
        @br{}
        The width of border around the button area at the bottom of the dialog.
        @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
      @begin[button-spacing]{entry}
        The @code{button-spacing} style property of type @code{:int} (Read)
        @br{}
        The spacing between buttons. @br{}
        Allowed values: >= 0 @br{}
        Default value: 4
      @end{entry}
      @begin[content-area-border]{entry}
        The @code{content-area-border} style property of type @code{:int} (Read)
        @br{}
        The width of border around the main dialog area. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
      @begin[content-area-spacing]{entry}
        The @code{content-area-spacing} style property of type @code{:int}
        (Read) @br{}
        The default spacing used between elements of the content area of the
        dialog, as returned by the @fun{gtk:dialog-content-area} function,
        unless the @fun{gtk:box-spacing} function was called on that widget
        directly. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"close\" signal}
      @begin{pre}
lambda (dialog)    :action
      @end{pre}
      @begin[code]{table}
        @entry[dialog]{The @class{gtk:dialog} widget on which the signal is
          emitted.}
      @end{table}
      A keybinding signal which gets emitted when the user uses a keybinding to
      close the dialog. The default binding for this signal is the @kbd{Escape}
      key.
    @subheading{The \"response\" signal}
      @begin{pre}
lambda (dialog response)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[dialog]{The @class{gtk:dialog} widget on which the signal is
          emitted.}
        @entry[response]{The integer with the response ID.}
      @end{table}
      Emitted when an action widget is clicked, the dialog receives a delete
      event, or the application programmer calls the @fun{gtk:dialog-response}
      function. On a delete event, the response ID is the @code{:delete-event}
      value of the @symbol{gtk:response-type} enumeration. Otherwise, it
      depends on which action widget was clicked.
  @end{dictionary}
  @see-constructor{gtk:dialog-new}
  @see-constructor{gtk:dialog-new-with-buttons}
  @see-slot{gtk:dialog-use-header-bar}
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:dialog-use-header-bar ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-header-bar" 'dialog) t)
 "The @code{use-header-bar} property of type @code{:int}
  (Read / Write / Construct) @br{}
  @em{True} if the dialog uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}. @br{}
  Allowed values: [-1, 1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'dialog-use-header-bar)
      "Accessor"
      (documentation 'dialog-use-header-bar 'function)
 "@version{2024-03-16}
  @syntax{(gtk:dialog-use-header-bar object) => setting}
  @syntax{(setf (gtk:dialog-use-header-bar object) setting)}
  @argument[object]{a @class{gtk:dialog} widget}
  @argument[setting]{@em{true} if the dialog uses a header bar}
  @begin{short}
    Accessor of the @slot[gtk:dialog]{use-header-bar} slot of the
    @class{gtk:dialog} class.
  @end{short}
  @em{True} if the dialog uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}.
  @see-class{gtk:dialog}
  @see-class{gtk:header-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline dialog-new))

(defun dialog-new ()
 #+liber-documentation
 "@version{2024-03-16}
  @return{The new @class{gtk:dialog} widget.}
  @short{Creates a new dialog.}
  Widgets should not be packed into this dialog directly, but into the content
  area and action area, which can be accessed with the
  @fun{gtk:dialog-content-area} and @fun{gtk:dialog-action-area} functions.
  @see-class{gtk:dialog}
  @see-function{gtk:dialog-action-area}
  @see-function{gtk:dialog-content-area}"
  (make-instance 'dialog))

(export 'dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new_with_buttons
;;; ----------------------------------------------------------------------------

(defun dialog-new-with-buttons (title parent flags &rest buttons)
 #+liber-documentation
 "@version{2025-06-05}
  @argument[title]{a string for the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog,
    or @code{nil}}
  @argument[flags]{a list of flags of type @symbol{gtk:dialog-flags}}
  @argument[buttons]{pairs for a button text and the response ID for the
    button, which is a positive integer or a value of the
    @symbol{gtk:response-type} enumeration}
  @return{The new @class{gtk:dialog} widget.}
  @begin{short}
    Creates a new dialog with title @arg{title}, or @code{nil} for the default
    title, see the @fun{gtk:window-title} function, and transient parent
    @arg{parent}, or @code{nil} for none, see the @fun{gtk:window-transient-for}
    function.
  @end{short}
  The @arg{flags} argument can be used to make the dialog modal with the
  @code{:modal} flag of the @symbol{gtk:dialog-flags} flags and/or to have it
  destroyed along with its transient parent with the @code{:destroy-with-parent}
  flag.

  After the @arg{flags} argument, button text/response ID pairs should be
  listed. Button text can be arbitrary text. A response ID can be any positive
  number, or one of the values in the @symbol{gtk:response-type} enumeration.
  If the user clicks one of these dialog buttons, the @class{gtk:dialog} widget
  will emit the @code{\"response\"} signal with the corresponding response ID.
  If a @class{gtk:dialog} widget receives the @code{\"delete-event\"} signal, it
  will emit the @code{\"response\"} signal with a @code{:delete-event} response
  ID. However, destroying a dialog does not emit the @code{\"response\"} signal.
  So be careful relying on the @code{\"response\"} signal when using the
  @code{:destroy-with-parent} flag. Buttons are from left to right, so the
  first button in the list will be the leftmost button in the dialog.
  @begin[Examples]{dictionary}
    @begin{pre}
(let ((dialog (gtk:dialog-new-with-buttons \"My dialog\"
                                           main-app-window
                                           '(:modal :destroy-with-parent)
                                           \"_OK\"
                                           :accept
                                           \"_Cancel\"
                                           :reject)))
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:dialog-flags}
  @see-symbol{gtk:response-type}
  @see-function{gtk:window-title}
  @see-function{gtk:window-transient-for}"
  (let ((dialog (make-instance 'dialog))
        (flags (if (listp flags) flags (list flags))))
    (when title
      (setf (window-title dialog) title))
    (when parent
      (setf (window-transient-for dialog) parent))
    (when (member :modal flags)
      (setf (window-modal dialog) t))
    (when (member :destroy-with-parent flags)
      (setf (window-destroy-with-parent dialog) t))
    (when buttons
     (apply #'dialog-add-buttons dialog buttons))
    dialog))

(export 'dialog-new-with-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_run
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_run" dialog-run) response-type
 #+liber-documentation
 "@version{2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @begin{return}
    The response ID, which is a positive integer or a value of the
    @symbol{gtk:response-type} enumeration.
  @end{return}
  @begin{short}
    Blocks in a recursive main loop until the dialog either emits the
    @code{\"response\"} signal, or is destroyed.
  @end{short}
  If the dialog is destroyed during the call to the @fun{gtk:dialog-run}
  function, it returns the @code{:none} response ID. Otherwise, it returns the
  response ID from the @code{\"response\"} signal emission.

  Before entering the recursive main loop, the @fun{gtk:dialog-run} function
  calls the @fun{gtk:widget-show} function on the dialog for you. Note that you
  still need to show any children of the dialog yourself.

  During the execution of the @fun{gtk:dialog-run} function, the default
  behavior of the @code{\"delete-event\"} signal is disabled. If the dialog
  receives the @code{\"delete-event\"} signal, it will not be destroyed as
  windows usually are, and the @fun{gtk:dialog-run} function will return the
  @code{:delete-event} response ID. Also, during the execution of the
  @fun{gtk:dialog-run} function the dialog will be modal. You can force the
  @fun{gtk:dialog-run} function to return at any time by calling the
  @fun{gtk:dialog-response} function to emit the @code{\"response\"} signal.
  Destroying the dialog during the execution of the @fun{gtk:dialog-run}
  function is a very bad idea, because your post-run code will not know whether
  the dialog was destroyed or not.

  After the @fun{gtk:dialog-run} function returns, you are responsible for
  hiding or destroying the dialog if you wish to do so.
  @begin[Examples]{dictionary}
    Typical usage of this function might be:
    @begin{pre}
(let ((response (gtk:dialog-run dialog)))
  (cond ((eq response :ok)
         (do-application-specific-something))
        (t
         (do-nothing-since-dialog-was-cancelled)))
  (gtk:widget-destroy dialog))
    @end{pre}
    Note that even though the recursive main loop gives the effect of a modal
    dialog, because it prevents the user from interacting with other windows in
    the same window group while the dialog is run, callbacks such as timeouts,
    IO channel watches, DND drops, etc, will be triggered during a
    @fun{gtk:dialog-run} function call.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-response}
  @see-function{gtk:widget-show}"
  (dialog (g:object dialog)))

(export 'dialog-run)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_response" dialog-response) :void
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value of
    the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Emits the @code{\"response\"} signal with the given response ID.
  @end{short}
  Used to indicate that the user has responded to the dialog in some way.
  Typically either you or the @fun{gtk:dialog-run} function will be monitoring
  the @code{\"response\"} signal and take appropriate action.
  @see-class{gtk:dialog}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-run}"
  (dialog (g:object dialog))
  (response response-type))

(export 'dialog-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_add_button" dialog-add-button) (g:object widget)
 #+liber-documentation
 "@version{2025-06-05}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[text]{a string for the text of the button}
  @argument[response]{a response ID for the button, which is a positive integer
    or a value of the @symbol{gtk:response-type} enumeration}
  @return{The @class{gtk:button} widget that was added.}
  @begin{short}
    Adds a button with the given text and sets things up so that clicking the
    button will emit the @code{\"response\"} signal with the given
    @arg{response} value.
  @end{short}
  The button is appended to the end of the action area of the dialog.
  @see-class{gtk:dialog}
  @see-class{gtk:button}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-add-buttons}
  @see-function{gtk:dialog-add-action-widget}"
  (dialog (g:object dialog))
  (text :string)
  (response response-type))

(export 'dialog-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_buttons
;;; ----------------------------------------------------------------------------

(defun dialog-add-buttons (dialog &rest buttons)
 #+liber-documentation
 "@version{2025-06-05}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[buttons]{pairs for a button text and the response ID, which is a
    positive integer or a value of the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Adds more buttons, same as calling the @fun{gtk:dialog-add-button} function
    repeatedly.
  @end{short}
  Each button must have both text and response ID.
  @begin[Notes]{dictionary}
    The Lisp implementation does not call the C function, but the
    @fun{gtk:dialog-add-button} function is called in a loop to add the buttons.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-add-button}
  @see-function{gtk:dialog-add-action-widget}"
  (iter (for (text id) on buttons by #'cddr)
        (dialog-add-button dialog text id)))

(export 'dialog-add-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_add_action_widget" dialog-add-action-widget) :void
 #+liber-documentation
 "@version{2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[child]{an activatable @class{gtk:widget} widget}
  @argument[response]{a response ID for @arg{child}, which is a positive
    integer or a value of the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Adds an activatable child widget to the action area of the dialog,
    connecting a signal handler that will emit the @code{\"response\"} signal
    on the dialog when the child widget is activated.
  @end{short}
  The child widget is appended to the end of the action area of the dialog. If
  you want to add a non-activatable widget, simply pack it into the action area
  of the dialog.
  @see-class{gtk:dialog}
  @see-class{gtk:widget}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-add-button}
  @see-function{gtk:dialog-add-buttons}"
  (dialog (g:object dialog))
  (child (g:object widget))
  (response response-type))

(export 'dialog-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_default_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_set_default_response" dialog-set-default-response)
    :void
 #+liber-documentation
 "@version{2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value
    of the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Sets the last widget in the action area of the dialog with the given
    @arg{response} value as the default widget for the dialog.
  @end{short}
  Pressing the @kbd{Enter} key normally activates the default widget.
  @see-class{gtk:dialog}
  @see-symbol{gtk:response-type}"
  (dialog (g:object dialog))
  (response response-type))

(export 'dialog-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_response_sensitive
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_set_response_sensitive"
               dialog-set-response-sensitive) :void
 #+liber-documentation
 "@version{2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value
    of the @symbol{gtk:response-type} enumeration}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls the @fun{gtk:widget-sensitive} function for each widget in the
    action area of the dialog with the given @arg{response} value.
  @end{short}
  A convenient way to sensitize/desensitize dialog buttons.
  @see-class{gtk:dialog}
  @see-symbol{gtk:response-type}
  @see-function{gtk:widget-sensitive}"
  (dialog (g:object dialog))
  (response response-type)
  (setting :boolean))

(export 'dialog-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_response_for_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_response_for_widget" dialog-response-for-widget)
    :int
 #+liber-documentation
 "@version{#2023-03-17}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[widget]{a @class{gtk:widget} widget in the action area of
    @arg{dialog}}
  @begin{return}
    The response ID of @arg{widget}, which is a positive integer or a value of
    the @symbol{gtk:response-type} enumeration, the value is @code{:none} if
    @arg{widget} does not have a response ID set.
  @end{return}
  @begin{short}
    Gets the response ID of the widget in the action area of the dialog.
  @end{short}
  @see-class{gtk:dialog}
  @see-class{gtk:widget}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-widget-for-response}"
  (dialog (g:object dialog))
  (widget (g:object widget)))

(export 'dialog-response-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_widget_for_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_widget_for_response" dialog-widget-for-response)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value
    of the @symbol{gtk:response-type} enumeration}
  @begin{return}
    The @class{gtk:widget} button that uses the given @arg{response} value,
    or @code{nil}.
  @end{return}
  @begin{short}
    Gets the button that uses the given response ID in the action area of the
    dialog.
  @end{short}
  @see-class{gtk:dialog}
  @see-class{gtk:widget}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-response-for-widget}"
  (dialog (g:object dialog))
  (response response-type))

(export 'dialog-widget-for-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_action_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_action_area" dialog-action-area)
    (g:object widget)
 #+liber-documentation
 "@version{2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @return{The @class{gtk:widget} action area of the dialog.}
  @short{Returns the action area of the dialog.}
  @begin[Warning]{dictionary}
    The @fun{gtk:dialog-action-area} function has been deprecated since version
    3.12 and should not be used in newly written code. Direct access to the
    action area is discouraged. Use the @fun{gtk:dialog-add-button} function,
    etc.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:widget}
  @see-function{gtk:dialog-add-button}
  @see-function{gtk:dialog-content-area}"
  (dialog (g:object dialog)))

(export 'dialog-action-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_content_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_content_area" dialog-content-area)
    (g:object widget)
 #+liber-documentation
 "@version{2024-03-16}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @begin{return}
    The @class{gtk:box} content area with a @code{:vertical} orientation.
  @end{return}
  @short{Returns the content area of the dialog.}
  @see-class{gtk:dialog}
  @see-class{gtk:box}
  @see-function{gtk:dialog-action-area}"
  (dialog (g:object dialog)))

(export 'dialog-content-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_header_bar
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_header_bar" dialog-header-bar) (g:object widget)
 #+liber-documentation
 "@version{#2023-03-17}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @return{The @class{gtk:header-bar} widget.}
  @begin{short}
    Returns the header bar of the dialog.
  @end{short}
  Note that the header bar is only used by the dialog if the
  @slot[gtk:dialog]{use-header-bar} property is @em{true}.
  @see-class{gtk:dialog}
  @see-class{gtk:header-bar}
  @see-function{gtk:dialog-use-header-bar}"
  (dialog (g:object dialog)))

(export 'dialog-header-bar)

;;; ----------------------------------------------------------------------------
;;; gtk_alternative_dialog_button_order                     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_alternative_dialog_button_order"
               alternative-dialog-button-order) :boolean
 #+liber-documentation
 "@version{#2023-03-17}
  @argument[screen]{a @class{gdk:screen} object, or @code{nil} to use the
    default screen}
  @return{The boolean whether the alternative button order should be used.}
  @begin{short}
    Returns @em{true} if dialogs are expected to use an alternative button
    order on the screen.
  @end{short}
  See the @fun{gtk:dialog-set-alternative-button-order} function for more
  details about alternative button order.

  If you need to use this function, you should probably connect to the
  @code{\"notify:gtk-alternative-button-order\"} signal on the
  @class{gtk:settings} object associated to the screen, in order to be notified
  if the button order setting changes.
  @begin[Warning]{dictionary}
    The @fun{gtk:alternative-dialog-button-order} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gdk:screen}
  @see-function{gtk:dialog-set-alternative-button-order}
  @see-function{gtk:settings-gtk-alternative-button-order}"
  (screen (g:object gdk:screen)))

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order                 not exported
;;; ----------------------------------------------------------------------------

(defun dialog-set-alternative-button-order (dialog response)
 #+liber-documentation
 "@version{#2023-03-17}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a list of response IDs, which are positive integer or
    values of the  @symbol{gtk:response-type} enumeration}
  @begin{short}
    Sets an alternative button order.
  @end{short}
  If the @slot[gtk:settings]{gtk-alternative-button-order} setting is set to
  @em{true}, the dialog buttons are reordered according to the order of the
  response IDs passed to this function.

  By default, GTK dialogs use the button order advocated by the GNOME Human
  Interface Guidelines with the affirmative button at the far right, and the
  cancel button left of it. But the built-in GTK dialogs and message dialogs
  do provide an alternative button order, which is more suitable on some
  platforms, for example Windows.
  @begin[Examples]{dictionary}
    Use this function after adding all the buttons to your dialog, as the
    following example shows:
    @begin{pre}
(let (;; Create a dialog with three buttons
      (dialog (gtk:dialog-new-with-buttons \"Demo Dialog\"
                                           nil ; No Parent window
                                           '(:modal)
                                           \"gtk-cancel\"
                                           :cancel
                                           \"gtk-ok\"
                                           :ok
                                           \"gtk-apply\"
                                           :apply)))
  ;; Set the default button.
  (gtk:widget-grab-default (gtk:dialog-widget-for-response dialog :ok))
  ;; Allow alternative button order for the default screen.
  (setf (gtk:settings-gtk-alternative-button-order
            (gtk:settings-default))
        t)
  ;; Set the alternative button order.
  (gtk:dialog-set-alternative-button-order dialog '(:ok :cancel :apply))
  ...)
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:dialog-set-alternative-button-order} function has been
    deprecated since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:message-dialog}
  @see-symbol{gtk:response-type}
  @see-function{gtk:settings-gtk-alternative-button-order}"
  (cffi:with-foreign-object (new-order 'response-type (length response))
    (loop for i from 0
          for id in response
          do (setf (cffi:mem-aref new-order 'response-type i) id))
    (%dialog-set-alternative-button-order-from-array dialog
                                                     (length response)
                                                     new-order))
  response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_alternative_button_order_from_array      not exported
;;; ----------------------------------------------------------------------------

;; This function is for internal use and not exported.
;; The function is called from gtk:dialog-set-alternative-button-order.

(cffi:defcfun ("gtk_dialog_set_alternative_button_order_from_array"
               %dialog-set-alternative-button-order-from-array) :void
 #+liber-documentation
 "@version{#2023-03-17}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[n-params]{the number of response IDs in @arg{new-order}}
  @argument[new-order]{an array of response IDs of dialog's buttons}
  @short{Sets an alternative button order.}
  If the @code{gtk:alternative-button-order} setting is set to @em{true},
  the dialog buttons are reordered according to the order of the response IDs
  in @arg{new-order}.

  See the @fun{gtk:dialog-set-alternative-button-order} function for more
  information.

  This function is for use by language bindings.
  @begin[Warning]{dictionary}
    The @fun{gtk:dialog-set-alternative-button-order} function has been
    deprecated since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:dialog}"
  (dialog (g:object dialog))
  (n-params :int)
  (new-order (:pointer response-type)))

;;; --- End of file gtk3.dialog.lisp -------------------------------------------
