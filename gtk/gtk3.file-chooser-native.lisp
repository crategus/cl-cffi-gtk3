;;; ----------------------------------------------------------------------------
;;; gtk3.file-chooser-native.lisp
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
;;; GtkFileChooserNative
;;;
;;;     A native file chooser dialog, suitable for "File/Open" or "File/Save"
;;;     commands
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserNative
;;;
;;; Accessors
;;;
;;;     gtk_file_chooser_native_get_accept_label
;;;     gtk_file_chooser_native_set_accept_label
;;;     gtk_file_chooser_native_get_cancel_label
;;;     gtk_file_chooser_native_set_cancel_label
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_native_new
;;;
;;; Properties
;;;
;;;     accept-label
;;;     cancel-label
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNativeDialog
;;;         ╰── GtkFileChooserNative
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileChooserNative implements GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserNative
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFileChooserNative" file-chooser-native
  (:superclass native-dialog
   :export t
   :interfaces ("GtkFileChooser")
   :type-initializer "gtk_file_chooser_native_get_type")
  ((accept-label
    file-chooser-native-accept-label
    "accept-label" "gchararray" t t)
   (cancel-label
    file-chooser-native-cancel-label
    "cancel-label" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'file-chooser-native 'type)
 "@version{#2023-6-12}
  @begin{short}
    The @class{gtk:file-chooser-native} widget is an abstraction of a dialog box
    suitable for use with \"File/Open\" or \"File/Save as\" commands.
  @end{short}
  By default, this just uses a @class{gtk:file-chooser-dialog} widget to
  implement the actual dialog. However, on certain platforms, such as Windows
  and macOS, the native platform file chooser is used instead. When the
  application is running in a sandboxed environment without direct filesystem
  access (such as Flatpak), the @class{gtk:file-chooser-native} widget may call
  the proper APIs (portals) to let the user choose a file and make it available
  to the application.

  While the API of @class{gtk:file-chooser-native} widget closely mirrors
  the @class{gtk:file-chooser-dialog} widget, the main difference is that there
  is no access to any @class{gtk:window} or @class{gtk:widget} widget for the
  dialog. This is required, as there may not be one in the case of a platform
  native dialog. Showing, hiding and running the dialog is handled by the
  @class{gtk:native-dialog} functions.

  @subheading{Typical usage}
  In the simplest of cases, you can the following code to use the
  @class{gtk:file-chooser-dialog} widget to select a file for opening:
  @begin{pre}
GtkFileChooserNative *native;
GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
gint res;

native = gtk_file_chooser_native_new (\"Open File\",
                                      parent_window,
                                      action,
                                      \"_Open\",
                                      \"_Cancel\");

res = gtk_native_dialog_run (GTK_NATIVE_DIALOG (native));
if (res == GTK_RESPONSE_ACCEPT)
  {
    char *filename;
    GtkFileChooser *chooser = GTK_FILE_CHOOSER (native);
    filename = gtk_file_chooser_get_filename (chooser);
    open_file (filename);
    g_free (filename);
  @}

g_object_unref (native);
  @end{pre}
  To use a dialog for saving, you can use this:
  @begin{pre}
GtkFileChooserNative *native;
GtkFileChooser *chooser;
GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_SAVE;
gint res;

native = gtk_file_chooser_native_new (\"Save File\",
                                      parent_window,
                                      action,
                                      \"_Save\",
                                      \"_Cancel\");
chooser = GTK_FILE_CHOOSER (native);

gtk_file_chooser_set_do_overwrite_confirmation (chooser, TRUE);

if (user_edited_a_new_document)
  gtk_file_chooser_set_current_name (chooser,
                                     _(\"Untitled document\"));
else
  gtk_file_chooser_set_filename (chooser,
                                 existing_filename);

res = gtk_native_dialog_run (GTK_NATIVE_DIALOG (native));
if (res == GTK_RESPONSE_ACCEPT)
  {
    char *filename;

    filename = gtk_file_chooser_get_filename (chooser);
    save_to_file (filename);
    g_free (filename);
  @}

g_object_unref (native);
  @end{pre}
  For more information on how to best set up a file dialog, see the
  @class{gtk:file-chooser-dialog} documentation.

  @subheading{Response Codes}
  The @class{gtk:file-chooser-native} widget inherits from the
  @class{gtk:native-dialog} class, which means it will return @code{:accept}
  if the user accepted, and @code{:cancel} if he pressed Cancel. It can also
  return @code{:delete-event} if the window was unexpectedly closed.

  @subheading{Differences from GtkFileChooserDialog}
  There are a few things in the @class{gtk:file-chooser} API that are not
  possible to use with the @class{gtk:file-chooser-native} widget, as such use
  would prohibit the use of a native dialog.

  There is no support for the signals that are emitted when the user navigates
  in the dialog, including:
  @begin{itemize}
    @item{\"current-folder-changed\"}
    @item{\"selection-changed\"}
    @item{\"file-activated\"}
    @item{\"confirm-overwrite\"}
  @end{itemize}
  You can also not use the methods that directly control user navigation:
  @begin{itemize}
    @item{@fun{gtk:file-chooser-unselect-filename}}
    @item{@fun{gtk:file-chooser-select-all}}
    @item{@fun{gtk:file-chooser-unselect-all}}
  @end{itemize}
  If you need any of the above you will have to use the
  @class{gtk:file-chooser-dialog} widget directly.

  No operations that change the dialog work while the dialog is visible. Set
  all the properties that are required before showing the dialog.

  @subheading{Win32 details}
  On windows the @code{IFileDialog} implementation (added in Windows Vista) is
  used. It supports many of the features that the
  @class{gtk:file-chooser-dialog} widget does, but there are some things it
  does not handle:
  @begin{itemize}
    @item{Extra widgets added with the @fun{gtk:file-chooser-extra-widget}
      function.}
    @item{Use of custom previews by connecting to the @code{\"update-preview\"}
      signal.}
    @item{Any @class{gtk:file-filter} object added using a mimetype or custom
      filter.}
  @end{itemize}
  If any of these features are used the regular @class{gtk:file-chooser-dialog}
  widget will be used in place of the native one.

  @subheading{Portal details}
  When the @code{org.freedesktop.portal.FileChooser} portal is available on the
  session bus, it is used to bring up an out-of-process file chooser. Depending
  on the kind of session the application is running in, this may or may not be
  a GTK file chooser. In this situation, the following things are not supported
  and will be silently ignored:
  @begin{itemize}
    @item{Extra widgets added with the @fun{gtk:file-chooser-extra-widget}
      function.}
    @item{Use of custom previews by connecting to the @code{\"update-preview\"}
      signal.}
    @item{Any @class{gtk:file-filter} object added with a custom filter.}
  @end{itemize}
  @subheading{macOS details}
  On macOS the @code{NSSavePanel} and @code{NSOpenPanel} classes are used to
  provide native file chooser dialogs. Some features provided by
  the @class{gtk:file-chooser-dialog} widget are not supported:
  @begin{itemize}
    @item{Extra widgets added with the @fun{gtk:file-chooser-extra-widget}
      function, unless the widget is an instance of the @class{gtk:label}
      widget, in which case the label text will be used to set the
      @code{NSSavePanel} message instance property.}
    @item{Use of custom previews by connecting to the @code{\"update-preview\"}
      signal.}
    @item{Any @class{gtk:file-filter} object added with a custom filter.}
    @item{Shortcut folders.}
  @end{itemize}
  @see-constructor{gtk:file-chooser-native-new}
  @see-slot{gtk:file-chooser-native-accept-label}
  @see-slot{gtk:file-chooser-native-cancel-label}
  @see-class{gtk:file-chooser}
  @see-class{gtk:native-dialog}
  @see-class{gtk:file-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:file-chooser-native-accept-label -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accept-label"
                                               'file-chooser-native) t)
 "The @code{accept-label} property of type @code{:string} (Read / Write) @br{}
  The text used for the label on the accept button in the dialog, or @code{nil}
  to use the default text.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-native-accept-label)
      "Accessor"
      (documentation 'file-chooser-native-accept-label 'function)
 "@version{#2023-6-12}
  @syntax{(gtk:file-chooser-native-accept-label object) => label}
  @syntax{(setf (gtk:file-chooser-native-accept-label object) label)}
  @argument[object]{a @class{gtk:file-chooser-native} widget}
  @argument[label]{a string with the custom label or @code{nil} for the default}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-native]{accept-label} slot of the
    @class{gtk:file-chooser-native} class.
  @end{short}
  The @fun{gtk:file-chooser-native-accept-label} function retrieves the custom
  label text for the accept button. The
  @setf{gtk:file-chooser-native-accept-label} function sets the custom label
  text.

  If characters in label are preceded by an underscore, they are underlined. If
  you need a literal underscore character in a label, use \"__\" (two
  underscores). The first underlined character represents a keyboard accelerator
  called a mnemonic. Pressing @kbd{Alt} and that key activates the button.
  @see-class{gtk:file-chooser-native}")

;;; --- gtk:file-chooser-native-cancel-label -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cancel-label"
                                               'file-chooser-native) t)
 "The @code{cancel-label} property of type @code{:string} (Read / Write) @br{}
  The text used for the label on the cancel button in the dialog, or @code{nil}
  to use the default text.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-native-cancel-label)
      "Accessor"
      (documentation 'file-chooser-native-cancel-label 'function)
 "@version{#2023-6-12}
  @syntax{(gtk:file-chooser-native-cancel-label object) => label}
  @syntax{(setf (gtk:file-chooser-native-cancel-label object) label)}
  @argument[object]{a @class{gtk:file-chooser-native} widget}
  @argument[label]{a string with the custom label or @code{nil} for the default}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-native]{cancel-label} slot of the
    @class{gtk:file-chooser-native} class.
  @end{short}
  The @fun{gtk:file-chooser-native-cancel-label} function retrieves the custom
  label text for the cancel button. The
  @setf{gtk:file-chooser-native-cancel-label} function sets the custom label
  text.

  If characters in label are preceded by an underscore, they are underlined. If
  you need a literal underscore character in a label, use \"__\" (two
  underscores). The first underlined character represents a keyboard accelerator
  called a mnemonic. Pressing @kbd{Alt} and that key activates the button.
  @see-class{gtk:file-chooser-native}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_native_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_native_new" file-chooser-native-new)
    (g:object file-chooser-native)
 "@version{#2023-6-12}
  @argument[title]{a string with the title of the native, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the native, or
    @code{nil}}
  @argument[action]{a @symbol{gtk:file-chooser-action} value with the open or
    save mode for the dialog}
  @argument[accept-label]{a string with the text to go in the accept button, or
    @code{nil} for the default}
  @argument[cancel-label]{a string with the text to go in the cancel button, or
    @code{nil} for the default}
  @return{A new @class{gtk:file-chooser-native} widget.}
  @begin{short}
    Creates a new @class{gtk:file-chooser-native} widget.
  @end{short}
  @see-class{gtk:file-chooser-native}
  @see-class{gtk:window}
  @see-symbol{gtk:file-chooser-action}"
  (title :string)
  (parent (g:object window))
  (action file-chooser-action)
  (accept-label :string)
  (canel-label :string))

(export 'file-chooser-native-new)

;;; --- End of file gtk3.file-chooser-native.lisp ------------------------------
