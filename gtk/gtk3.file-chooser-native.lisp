;;; ----------------------------------------------------------------------------
;;; gtk3.file-chooser-native.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkFileChooserNative
;;;
;;;     A native file chooser dialog, suitable for "File/Open" or "File/Save"
;;;     commands
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_native_new
;;;     gtk_file_chooser_native_get_accept_label
;;;     gtk_file_chooser_native_set_accept_label
;;;     gtk_file_chooser_native_get_cancel_label
;;;     gtk_file_chooser_native_set_cancel_label
;;; ----------------------------------------------------------------------------
;;;
;;; Description
;;;
;;; GtkFileChooserNative is an abstraction of a dialog box suitable for use with
;;; "File/Open" or "File/Save as" commands. By default, this just uses a
;;; GtkFileChooserDialog to implement the actual dialog. However, on certain
;;; platforms, such as Windows and macOS, the native platform file chooser is
;;; used instead. When the application is running in a sandboxed environment
;;; without direct filesystem access (such as Flatpak), GtkFileChooserNative may
;;; call the proper APIs (portals) to let the user choose a file and make it
;;; available to the application.
;;;
;;; While the API of GtkFileChooserNative closely mirrors GtkFileChooserDialog,
;;; the main difference is that there is no access to any GtkWindow or GtkWidget
;;; for the dialog. This is required, as there may not be one in the case of a
;;; platform native dialog. Showing, hiding and running the dialog is handled by
;;; the GtkNativeDialog functions.
;;;
;;; Typical usage
;;;
;;; In the simplest of cases, you can the following code to use
;;; GtkFileChooserDialog to select a file for opening:
;;;
;;; GtkFileChooserNative *native;
;;; GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
;;; gint res;
;;;
;;; native = gtk_file_chooser_native_new ("Open File",
;;;                                       parent_window,
;;;                                       action,
;;;                                       "_Open",
;;;                                       "_Cancel");
;;;
;;; res = gtk_native_dialog_run (GTK_NATIVE_DIALOG (native));
;;; if (res == GTK_RESPONSE_ACCEPT)
;;;   {
;;;     char *filename;
;;;     GtkFileChooser *chooser = GTK_FILE_CHOOSER (native);
;;;     filename = gtk_file_chooser_get_filename (chooser);
;;;     open_file (filename);
;;;     g_free (filename);
;;;   }
;;;
;;; g_object_unref (native);
;;;
;;; To use a dialog for saving, you can use this:
;;;
;;; GtkFileChooserNative *native;
;;; GtkFileChooser *chooser;
;;; GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_SAVE;
;;; gint res;
;;;
;;; native = gtk_file_chooser_native_new ("Save File",
;;;                                       parent_window,
;;;                                       action,
;;;                                       "_Save",
;;;                                       "_Cancel");
;;; chooser = GTK_FILE_CHOOSER (native);
;;;
;;; gtk_file_chooser_set_do_overwrite_confirmation (chooser, TRUE);
;;;
;;; if (user_edited_a_new_document)
;;;   gtk_file_chooser_set_current_name (chooser,
;;;                                      _("Untitled document"));
;;; else
;;;   gtk_file_chooser_set_filename (chooser,
;;;                                  existing_filename);
;;;
;;; res = gtk_native_dialog_run (GTK_NATIVE_DIALOG (native));
;;; if (res == GTK_RESPONSE_ACCEPT)
;;;   {
;;;     char *filename;
;;;
;;;     filename = gtk_file_chooser_get_filename (chooser);
;;;     save_to_file (filename);
;;;     g_free (filename);
;;;   }
;;;
;;; g_object_unref (native);
;;;
;;; For more information on how to best set up a file dialog, see
;;; GtkFileChooserDialog.
;;;
;;; Response Codes
;;;
;;; GtkFileChooserNative inherits from GtkNativeDialog, which means it will
;;; return GTK_RESPONSE_ACCEPT if the user accepted, and GTK_RESPONSE_CANCEL if
;;; he pressed cancel. It can also return GTK_RESPONSE_DELETE_EVENT if the
;;; window was unexpectedly closed.
;;;
;;; Differences from GtkFileChooserDialog
;;;
;;; There are a few things in the GtkFileChooser API that are not possible to
;;; use with GtkFileChooserNative, as such use would prohibit the use of a
;;; native dialog.
;;;
;;; There is no support for the signals that are emitted when the user navigates
;;; in the dialog, including:
;;;
;;; "current-folder-changed"
;;; "selection-changed"
;;; "file-activated"
;;; "confirm-overwrite"
;;;
;;; You can also not use the methods that directly control user navigation:
;;;
;;; gtk_file_chooser_unselect_filename()
;;; gtk_file_chooser_select_all()
;;; gtk_file_chooser_unselect_all()
;;;
;;; If you need any of the above you will have to use GtkFileChooserDialog
;;; directly.
;;;
;;; No operations that change the the dialog work while the dialog is visible.
;;; Set all the properties that are required before showing the dialog.
;;;
;;; Win32 details
;;;
;;; On windows the IFileDialog implementation (added in Windows Vista) is used.
;;; It supports many of the features that GtkFileChooserDialog does, but there
;;; are some things it does not handle:
;;;
;;; Extra widgets added with gtk_file_chooser_set_extra_widget().
;;; Use of custom previews by connecting to "update-preview".
;;; Any GtkFileFilter added using a mimetype or custom filter.
;;;
;;; If any of these features are used the regular GtkFileChooserDialog will be
;;; used in place of the native one.
;;;
;;; Portal details
;;;
;;; When the org.freedesktop.portal.FileChooser portal is available on the
;;; session bus, it is used to bring up an out-of-process file chooser.
;;; Depending on the kind of session the application is running in, this may or
;;; may not be a GTK+ file chooser. In this situation, the following things are
;;; not supported and will be silently ignored:
;;;
;;; Extra widgets added with gtk_file_chooser_set_extra_widget().
;;; Use of custom previews by connecting to "update-preview".
;;; Any GtkFileFilter added with a custom filter.
;;;
;;; macOS details
;;;
;;; * On macOS the NSSavePanel and NSOpenPanel classes are used to provide
;;;   native file chooser dialogs. Some features provided by
;;;   GtkFileChooserDialog are not supported:
;;;
;;; * Extra widgets added with gtk_file_chooser_set_extra_widget(), unless the
;;;   widget is an instance of GtkLabel, in which case the label text will be
;;;   used to set the NSSavePanel message instance property.
;;;
;;; * Use of custom previews by connecting to "update-preview".
;;;
;;; * Any GtkFileFilter added with a custom filter.
;;;
;;; * Shortcut folders.
;;;
;;; See Also
;;;
;;;     GtkFileChooser, GtkNativeDialog, GtkFileChooserDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_new ()
;;;
;;; GtkFileChooserNative *
;;; gtk_file_chooser_native_new (const gchar *title,
;;;                              GtkWindow *parent,
;;;                              GtkFileChooserAction action,
;;;                              const gchar *accept_label,
;;;                              const gchar *cancel_label);
;;;
;;; Creates a new GtkFileChooserNative.
;;;
;;; title :
;;;     Title of the native, or NULL.
;;;
;;; parent :
;;;     Transient parent of the native, or NULL.
;;;
;;; action :
;;;     Open or save mode for the dialog
;;;
;;; accept_label :
;;;     text to go in the accept button, or NULL for the default.
;;;
;;; cancel_label :
;;;     text to go in the cancel button, or NULL for the default.
;;;
;;; Returns :
;;;     a new GtkFileChooserNative
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_get_accept_label ()
;;;
;;; const char *
;;; gtk_file_chooser_native_get_accept_label
;;;                                (GtkFileChooserNative *self);
;;;
;;; Retrieves the custom label text for the accept button.
;;;
;;; self :
;;;     a GtFileChooserNative
;;;
;;; Returns :
;;;     The custom label, or NULL for the default. This string is owned by GTK+
;;;     and should not be modified or freed.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_set_accept_label ()
;;;
;;; void
;;; gtk_file_chooser_native_set_accept_label
;;;                                (GtkFileChooserNative *self,
;;;                                 const char *accept_label);
;;;
;;; Sets the custom label text for the accept button.
;;;
;;; If characters in label are preceded by an underscore, they are underlined.
;;; If you need a literal underscore character in a label, use "__"
;;; (two underscores). The first underlined character represents a keyboard
;;; accelerator called a mnemonic. Pressing Alt and that key activates the
;;; button.
;;;
;;; self :
;;;     a GtFileChooserNative
;;;
;;; accept_label :
;;;     custom label or NULL for the default.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_get_cancel_label ()
;;;
;;; const char *
;;; gtk_file_chooser_native_get_cancel_label (GtkFileChooserNative *self);
;;;
;;; Retrieves the custom label text for the cancel button.
;;;
;;; self :
;;;     a GtFileChooserNative
;;;
;;; Returns :
;;;     The custom label, or NULL for the default. This string is owned by GTK+
;;;     and should not be modified or freed.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_set_cancel_label ()
;;;
;;; void
;;; gtk_file_chooser_native_set_cancel_label
;;;                                (GtkFileChooserNative *self,
;;;                                 const char *cancel_label);
;;;
;;; Sets the custom label text for the cancel button.
;;;
;;; If characters in label are preceded by an underscore, they are underlined.
;;; If you need a literal underscore character in a label, use "__"
;;; (two underscores). The first underlined character represents a keyboard
;;; accelerator called a mnemonic. Pressing Alt and that key activates the
;;; button.
;;;
;;; self :
;;;     a GtFileChooserNative
;;;
;;; cancel_label :
;;;     custom label or NULL for the default.
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.file-chooser-native.lisp ------------------------------
