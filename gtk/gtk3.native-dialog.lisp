;;; ----------------------------------------------------------------------------
;;; gtk3.native-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2020 - 2023 Dieter Kaiser
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
;;; GtkNativeDialog
;;;
;;;     Integrate with native dialogs
;;;
;;; Types and Values
;;;
;;;     GtkNativeDialog
;;;
;;; Functions
;;;
;;;     gtk_native_dialog_show
;;;     gtk_native_dialog_hide
;;;     gtk_native_dialog_destroy
;;;     gtk_native_dialog_get_visible
;;;     gtk_native_dialog_set_modal
;;;     gtk_native_dialog_get_modal
;;;     gtk_native_dialog_set_title
;;;     gtk_native_dialog_get_title
;;;     gtk_native_dialog_set_transient_for
;;;     gtk_native_dialog_get_transient_for
;;;     gtk_native_dialog_run
;;;
;;; Properties
;;;
;;;     modal
;;;     title
;;;     transient-for
;;;     visible
;;;
;;; Signals
;;;
;;;     response
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNativeDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkNativeDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkNativeDialog" native-dialog
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_native_dialog_get_type")
  ((modal
    native-dialog-modal
    "modal" "gboolean" t t)
   (title
    native-dialog-title
    "title" "gchararray" t t)
   (transient-for
    native-dialog-transient-for
    "transient-for" "GtkWindow" t t)
   (visible
    native-dialog-visible
    "visible" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'native-dialog 'type)
 "@version{#2023-6-10}
  @begin{short}
    Native dialogs are platform dialogs that do not use @class{gtk:dialog} or
    @class{gtk:window} widgets.
  @end{short}
  They are used in order to integrate better with a platform, by looking the
  same as other native applications and supporting platform specific features.

  The @class{gtk:dialog} functions cannot be used on such objects, but we need
  a similar API in order to drive them. The @sym{gtk:native-dialog} object is
  an API that allows you to do this. It allows you to set various common
  properties on the dialog, as well as show and hide it and get a \"response\"
  signal when the user finished with the dialog.

  There is also a @fun{gtk:native-dialog-run} helper function that makes it
  easy to run any native dialog in a modal way with a recursive main loop,
  similar to the @fun{gtk:dialog-run} function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"response\" signal}
      @begin{pre}
lambda (dialog id)    :run-last
      @end{pre}
      Emitted when the user responds to the dialog window. When this is called
      the dialog window has been hidden. If you call the
      @fun{gtk:native-dialog-hide} function before the user responds to the
      dialog window this signal will not be emitted.
      @begin[code]{table}
        @entry[dialog]{The @sym{gtk:native-dialog} widget which received the
          signal.}
        @entry[id]{An integer with the response ID.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:native-dialog-modal}
  @see-slot{gtk:native-dialog-title}
  @see-slot{gtk:native-dialog-transient-for}
  @see-slot{gtk:native-dialog-visible}
  @see-class{gtk:dialog}
  @see-class{gtk:file-chooser-native}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- native-dialog-modal ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'native-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the dialog window should be modal with respect to its transient
  parent.")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-modal)
      "Accessor"
      (documentation 'native-dialog-modal 'function)
 "@version{#2023-6-10}
  @syntax{(gtk:native-dialog-modal object) => modal}
  @syntax{(setf (gtk:native-dialog-modal object) modal)}
  @argument[object]{a @class{gtk:native-dialog} widget}
  @argument[modal]{a boolean whether the dialog window should be modal with
    respect to the transient parent}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{modal} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  The @sym{gtk:native-dialog-modal} function returns whether the dialog window
  is modal. The @sym{(setf gtk:native-dialog-modal)} function sets a dialog
  window modal or non-modal. Modal dialog windows prevent interaction with
  other windows in the same application. To keep modal dialogs on top of main
  application windows, use the @fun{gtk:native-dialog-transient-for} functiom
  to make the dialog window transient for the parent. Most window managers will
  then disallow lowering the dialog window below the parent.
  @see-class{gtk:native-dialog}
  @see-function{gtk:native-dialog-transient-for}")

;;; --- native-dialog-title ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'native-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the dialog window.")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-title)
      "Accessor"
      (documentation 'native-dialog-title 'function)
 "@version{#2023-6-10}
  @syntax{(gtk:native-dialog-title object) => title}
  @syntax{(setf (gtk:native-dialog-title object) title)}
  @argument[object]{a @class{gtk:native-dialog} widget}
  @argument[title]{a string with the title of the dialog window, or @code{nil}
    if none has been set}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{title} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  The @sym{gtk:native-dialog-title} function gets the title of the dialog
  window. The @sym{(setf gtk:native-dialog-title)} function sets the title.
  @see-class{gtk:native-dialog}")

;;; --- native-dialog-transient-for --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transient-for"
                                               'native-dialog) t)
 "The @code{transient-for} property of type @class{gtk:window}
  (Read / Write / Construct) @br{}
  The transient parent of the dialog window, or @code{nil} for none.")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-transient-for)
      "Accessor"
      (documentation 'native-dialog-transient-for 'function)
 "@version{#2023-6-10}
  @syntax{(gtk:native-dialog-transient-for object) => parent}
  @syntax{(setf (gtk:native-dialog-transient-for object) parent)}
  @argument[object]{a @class{gtk:native-dialog} widget}
  @argument[parent]{a @class{gtk:window} transient parent for the dialog window}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{transient-for} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  The @sym{gtk:native-dialog-transient-for} function fetches the transient
  parent for the dialog window. The @sym{(setf gtk:native-dialog-transient-for)}
  function sets the transient parent.

  Dialogs should be set transient for the main application window they were
  spawned from. This allows window managers to e.g. keep the dialog window on
  top of the main window, or center the dialog window over the main window.

  Passing @code{nil} for parent unsets the current transient window.
  @see-class{gtk:native-dialog}
  @see-class{gtk:window}")

;;; --- native-dialog-visible --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'native-dialog) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the dialog window is currenlty visible.")

#+liber-documentation
(setf (liber:alias-for-function 'native-dialog-visible)
      "Accessor"
      (documentation 'native-dialog-visible 'function)
 "@version{#2023-6-10}
  @syntax{(gtk:native-dialog-visible object) => visible}
  @syntax{(setf (gtk:native-dialog-visible object) visible)}
  @argument[object]{a @class{gtk:native-dialog} widget}
  @argument[visible]{a boolean whether the dialog window is visible}
  @begin{short}
    Accessor of the @slot[gtk:native-dialog]{visible} slot of the
    @class{gtk:native-dialog} class.
  @end{short}
  Determines whether the dialog window is visible.
  @see-class{gtk:native-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_show ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_dialog_show" native-dialog-show) :void
 #+liber-documentation
 "@version{#2023-6-10}
  @argument[dialog]{a @class{gtk:native-dialog} widget}
  @begin{short}
    Shows the dialog window on the display, allowing the user to interact with
    it.
  @end{short}
  When the user accepts the state of the dialog window the dialog window will
  be automatically hidden and the \"response\" signal will be emitted.

  Multiple calls while the dialog window is visible will be ignored.
  @see-class{gtk:native-dialog}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-show)

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_hide ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_dialog_hide" native-dialog-hide) :void
 #+liber-documentation
 "@version{#2023-6-10}
  @argument[dialog]{a @class{gtk:native-dialog} widget}
  @begin{short}
    Hides the dialog window if it is visible, aborting any interaction.
  @end{short}
  Once this is called the \"response\" signal will not be emitted until after
  the next call to the @fun{gtk:native-dialog-show} function.

  If the dialog window is not visible this does nothing.
  @see-class{gtk:native-dialog}
  @see-function{gtk:native-dialog-show}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-hide)

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_dialog_destroy" native-dialog-destroy) :void
 #+liber-documentation
 "@version{#2023-6-10}
  @argument[dialog]{a @class{gtk:native-dialog} widget}
  @begin{short}
    Destroys a dialog.
  @end{short}
  When a dialog window is destroyed, it will break any references it holds to
  other objects. If it is visible it will be hidden and any underlying window
  system resources will be destroyed.

  Note that this does not release any reference to the object, as opposed to
  destroying a @class{gtk:Window} widget because there is no reference from the
  windowing system to the @class{gtk:native-dialog} widget.
  @see-class{gtk:native-dialog}
  @see-class{gtk:window}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-destroy)

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_run ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_native_dialog_run" native-dialog-run) :int
 #+liber-documentation
 "@version{#2023-6-10}
  @argument[dialog]{a @class{gtk:native-dialog} widget}
  @return{an integer with the response ID}
  @begin{short}
    Blocks in a recursive main loop until self emits the \"response\" signal.
  @end{short}
  It then returns the response ID from the \"response\" signal emission.

  Before entering the recursive main loop, the @fun{gtk:native-dialog-run}
  function calls the @fun{gtk:native-dialog-show} function on the dialog for
  you.

  After the @fun{gtk:native-dialog-run} function returns, then dialog window
  will be hidden.
  @begin[Example]{dictionary}
    Typical usage of this function might be:
    @begin{pre}
gint result = gtk_native_dialog_run (GTK_NATIVE_DIALOG (dialog));
switch (result)
  {
    case GTK_RESPONSE_ACCEPT:
       do_application_specific_something ();
       break;
    default:
       do_nothing_since_dialog_was_cancelled ();
       break;
  @}
g_object_unref (dialog);
    @end{pre}
  @end{dictionary}

  Note that even though the recursive main loop gives the effect of a modal
  dialog window, it prevents the user from interacting with other windows in
  the same window group while the dialog is run, callbacks such as timeouts, IO
  channel watches, DND drops, etc, will be triggered during a
  @fun{gtk:native-dialog-run} function call.
  @see-class{gtk:native-dialog}
  @see-function{gtk:native-dialog-run}
  @see-function{gtk:native-dialog-show}"
  (dialog (g:object native-dialog)))

(export 'native-dialog-run)

;;; --- End of file gtk3.native-dialog.lisp ------------------------------------
