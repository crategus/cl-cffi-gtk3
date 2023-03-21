;;; ----------------------------------------------------------------------------
;;; gtk3.native-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK+ 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;;
;;; Description
;;;
;;; Native dialogs are platform dialogs that don't use GtkDialog or GtkWindow.
;;; They are used in order to integrate better with a platform, by looking the
;;; same as other native applications and supporting platform specific features.
;;;
;;; The GtkDialog functions cannot be used on such objects, but we need a
;;; similar API in order to drive them. The GtkNativeDialog object is an API
;;; that allows you to do this. It allows you to set various common properties
;;; on the dialog, as well as show and hide it and get a "response" signal when
;;; the user finished with the dialog.
;;;
;;; There is also a gtk_native_dialog_run() helper that makes it easy to run
;;; any native dialog in a modal way with a recursive main loop, similar to
;;; gtk_dialog_run().
;;;
;;; See Also
;;;     GtkFileChooserNative, GtkDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkNativeDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkNativeDialog" native-dialog
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

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_show ()
;;;
;;; void
;;; gtk_native_dialog_show (GtkNativeDialog *self);
;;;
;;; Shows the dialog on the display, allowing the user to interact with it.
;;; When the user accepts the state of the dialog the dialog will be
;;; automatically hidden and the "response" signal will be emitted.
;;;
;;; Multiple calls while the dialog is visible will be ignored.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_hide ()
;;;
;;; void
;;; gtk_native_dialog_hide (GtkNativeDialog *self);
;;;
;;; Hides the dialog if it is visilbe, aborting any interaction. Once this is
;;; called the "response" signal will not be emitted until after the next call
;;; to gtk_native_dialog_show().
;;;
;;; If the dialog is not visible this does nothing.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_destroy ()
;;;
;;; void
;;; gtk_native_dialog_destroy (GtkNativeDialog *self);
;;;
;;; Destroys a dialog.
;;;
;;; When a dialog is destroyed, it will break any references it holds to other
;;; objects. If it is visible it will be hidden and any underlying window
;;; system resources will be destroyed.
;;;
;;; Note that this does not release any reference to the object (as opposed to
;;; destroying a GtkWindow) because there is no reference from the windowing
;;; system to the GtkNativeDialog.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_visible ()
;;;
;;; gboolean
;;; gtk_native_dialog_get_visible (GtkNativeDialog *self);
;;;
;;; Determines whether the dialog is visible.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     TRUE if the dialog is visible
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_set_modal ()
;;;
;;; void
;;; gtk_native_dialog_set_modal (GtkNativeDialog *self,
;;;                              gboolean modal);
;;;
;;; Sets a dialog modal or non-modal. Modal dialogs prevent interaction with
;;; other windows in the same application. To keep modal dialogs on top of main
;;; application windows, use gtk_native_dialog_set_transient_for() to make the
;;; dialog transient for the parent; most window managers will then disallow
;;; lowering the dialog below the parent.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; modal :
;;;     whether the window is modal
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_modal ()
;;;
;;; gboolean
;;; gtk_native_dialog_get_modal (GtkNativeDialog *self);
;;;
;;; Returns whether the dialog is modal. See gtk_native_dialog_set_modal().
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     TRUE if the dialog is set to be modal
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_set_title ()
;;;
;;; void
;;; gtk_native_dialog_set_title (GtkNativeDialog *self,
;;;                              const char *title);
;;;
;;; Sets the title of the GtkNativeDialog.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; title :
;;;     title of the dialog
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_title ()
;;;
;;; const char *
;;; gtk_native_dialog_get_title (GtkNativeDialog *self);
;;;
;;; Gets the title of the GtkNativeDialog.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     the title of the dialog, or NULL if none has been set explicitly. The
;;;     returned string is owned by the widget and must not be modified or
;;;     freed.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_set_transient_for ()
;;;
;;; void
;;; gtk_native_dialog_set_transient_for (GtkNativeDialog *self,
;;;                                      GtkWindow *parent);
;;;
;;; Dialogs should be set transient for the main application window they
;;; were spawned from. This allows window managers to e.g. keep the dialog on
;;; top of the main window, or center the dialog over the main window.
;;;
;;; Passing NULL for parent unsets the current transient window.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; parent :
;;;     parent window, or NULL.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_get_transient_for ()
;;;
;;; GtkWindow *
;;; gtk_native_dialog_get_transient_for (GtkNativeDialog *self);
;;;
;;; Fetches the transient parent for this window.
;;; See gtk_native_dialog_set_transient_for().
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     the transient parent for this window, or NULL if no transient parent
;;;     has been set.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_native_dialog_run ()
;;;
;;; gint
;;; gtk_native_dialog_run (GtkNativeDialog *self);
;;;
;;; Blocks in a recursive main loop until self emits the "response" signal. It
;;; then returns the response ID from the ::response signal emission.
;;;
;;; Before entering the recursive main loop, gtk_native_dialog_run() calls
;;; gtk_native_dialog_show() on the dialog for you.
;;;
;;; After gtk_native_dialog_run() returns, then dialog will be hidden.
;;;
;;; Typical usage of this function might be:
;;;
;;; gint result = gtk_native_dialog_run (GTK_NATIVE_DIALOG (dialog));
;;; switch (result)
;;;   {
;;;     case GTK_RESPONSE_ACCEPT:
;;;        do_application_specific_something ();
;;;        break;
;;;     default:
;;;        do_nothing_since_dialog_was_cancelled ();
;;;        break;
;;;   }
;;; g_object_unref (dialog);
;;;
;;; Note that even though the recursive main loop gives the effect of a modal
;;; dialog (it prevents the user from interacting with other windows in the
;;; same window group while the dialog is run), callbacks such as timeouts, IO
;;; channel watches, DND drops, etc, will be triggered during a
;;; gtk_nautilus_dialog_run() call.
;;;
;;; self :
;;;     a GtkNativeDialog
;;;
;;; Returns :
;;;     response ID
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.native-dialog.lisp ------------------------------------
