;;; ----------------------------------------------------------------------------
;;; gtk3.app-chooser-dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GtkAppChooserDialog
;;;
;;;     An application chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget
;;;     gtk_app_chooser_dialog_set_heading                 Accessor
;;;     gtk_app_chooser_dialog_get_heading                 Accessor
;;;
;;; Properties
;;;
;;;     gfile
;;;     heading
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
;;;                             ╰── GtkAppChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAppChooserDialog implements AtkImplementorIface, GtkBuildable and
;;;     GtkAppChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAppChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAppChooserDialog" app-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_dialog_get_type")
  ((gfile
    app-chooser-dialog-gfile
    "gfile" "GFile" t t)
   (heading
    app-chooser-dialog-heading
    "heading" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'app-chooser-dialog 'type)
 "@version{2023-2-18}
  @begin{short}
    The @class{gtk:app-chooser-dialog} widget shows a
    @class{gtk:app-chooser-widget} widget inside a @class{gtk:dialog} widget.
  @end{short}
  Note that the @class{gtk:app-chooser-dialog} widget does not have any
  interesting methods of its own. Instead, you should get the embedded
  @class{gtk:app-chooser-widget} widget using the
  @fun{gtk:app-chooser-dialog-widget} function and call its methods if the
  @class{gtk:app-chooser} interface is not sufficient for your needs.

  To set the heading that is shown above the @class{gtk:app-chooser-widget}
  widget, use the @fun{gtk:app-chooser-dialog-heading} function.
  @see-constructor{gtk:app-chooser-dialog-new}
  @see-constructor{gtk:app-chooser-dialog-new-for-content-type}
  @see-slot{gtk:app-chooser-dialog-gfile}
  @see-slot{gtk:app-chooser-dialog-heading}
  @see-class{gtk:dialog}
  @see-class{gtk:app-chooser}
  @see-function{gtk:app-chooser-dialog-widget}
  @see-function{gtk:app-chooser-dialog-heading}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:app-chooser-dialog-gfile -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gfile" 'app-chooser-dialog) t)
 "The @code{gfile} property of type @class{g:file}
  (Read / Write / Construct Only) @br{}
  The @class{g:file} object used by the @class{gtk:app-chooser-dialog} widget.
  The dialog's content type will be guessed from the file, if present.")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-dialog-gfile)
      "Accessor"
      (documentation 'app-chooser-dialog-gfile 'function)
 "@version{2023-2-18}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-dialog]{gfile} slot of the
    @class{gtk:app-chooser-dialog} class.
  @end{short}
  @see-class{gtk:app-chooser-dialog}")

;;; --- gtk:app-chooser-dialog-heading -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "heading" 'app-chooser-dialog) t)
 "The @code{heading} property of type @code{:string} (Read / Write) @br{}
  The text to show at the top of the dialog. The string may contain Pango
  markup. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-dialog-heading)
      "Accessor"
      (documentation 'app-chooser-dialog-heading 'function)
 "@version{2023-2-18}
  @syntax{gtk:app-chooser-dialog-heading object) => heading}
  @syntax{(setf (gtk:app-chooser-dialog-heading object) heading)}
  @argument[object]{a @class{gtk:app-chooser-dialog} widget}
  @argument[heading]{a string containing Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-dialog]{heading} slot of the
    @class{gtk:app-chooser-dialog} class.
  @end{short}
  The @fun{gtk:app-chooser-dialog-heading} function returns the text to display
  at the top of the dialog. The @setf{gtk:app-chooser-dialog-heading} function
  sets the text to display at the top of the dialog. If the heading is not set,
  the dialog displays a default text.
  @see-class{gtk:app-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_dialog_new" app-chooser-dialog-new)
    (g:object widget)
 #+liber-documentation
 "@version{2025-07-07}
  @argument[parent]{a @class{gtk:window} widget, or @code{nil}}
  @argument[flags]{a @sym{gtk:dialog-flags} value for this dialog}
  @argument[file]{a @class{g:file} object}
  @return{The newly created @class{gtk:app-chooser-dialog} widget.}
  @begin{short}
    Creates a new application chooser dialog for the provided @class{g:file}
    object, to allow the user to select an application for it.
  @end{short}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:window}
  @see-class{g:file}
  @see-symbol{gtk:dialog-flags}"
  (parent (g:object window))
  (flags dialog-flags)
  (file g:object))

(export 'app-chooser-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_new_for_content_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_dialog_new_for_content_type"
               app-chooser-dialog-new-for-content-type) (g:object widget)
 #+liber-documentation
 "@version{2025-07-07}
  @argument[parent]{a @class{gtk:window} widget, or @code{nil}}
  @argument[flags]{a @sym{gtk:dialog-flags} value for this dialog}
  @argument[content-type]{a content type string}
  @return{The newly created @class{gtk:app-chooser-dialog} widget.}
  @begin{short}
    Creates a new application chooser dialog for the provided content type, to
    allow the user to select an application for it.
  @end{short}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:dialog-flags}"
  (parent (g:object window))
  (flags dialog-flags)
  (content-type :string))

(export 'app-chooser-dialog-new-for-content-type)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_get_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_dialog_get_widget" app-chooser-dialog-widget)
    (g:object widget)
 #+liber-documentation
 "@version{2023-2-18}
  @argument[dialog]{a @class{gtk:app-chooser-dialog} widget}
  @return{The @class{gtk:app-chooser-widget} widget of @arg{dialog}.}
  @begin{short}
    Returns the application chooser widget of the dialog.
  @end{short}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:app-chooser-widget}"
  (dialog (g:object app-chooser-dialog)))

(export 'app-chooser-dialog-widget)

;;; --- End of file gtk3.app-chooser-dialog.lisp -------------------------------
