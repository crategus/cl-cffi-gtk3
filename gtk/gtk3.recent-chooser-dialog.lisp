;;; ----------------------------------------------------------------------------
;;; gtk3.recent-chooser-dialog.lisp
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
;;; GtkRecentChooserDialog
;;;
;;;     Displays recently used files in a dialog
;;;
;;; Types and Values
;;;
;;;     GtkRecentChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_recent_chooser_dialog_new
;;;     gtk_recent_chooser_dialog_new_for_manager
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
;;;                             ╰── GtkRecentChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentChooserDialog implements AtkImplementorIface, GtkBuildable
;;;     and GtkRecentChooser.
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkRecentChooserDialog" recent-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_chooser_dialog_get_type")
  nil)

#+liber-documentation
(setf (documentation 'recent-chooser-dialog 'type)
 "@version{#2023-3-1}
  @image[recentchooserdialog]{}
  @begin{short}
    The @class{gtk:recent-chooser-dialog} widget is a dialog box suitable for
    displaying the recently used documents.
  @end{short}
  This widgets works by putting a @class{gtk:recent-chooser-widget} widget
  inside a @class{gtk:dialog} widget. It exposes the @class{gtk:recent-chooser}
  interface, so you can use all the @class{gtk:recent-chooser} functions on the
  recent chooser dialog as well as those for the @class{gtk:dialog} widget.

  Note that the @class{gtk:recent-chooser-dialog} widget does not have any
  methods of its own. Instead, you should use the functions that work on a
  @class{gtk:recent-chooser} widget.
  @begin{examples}
    In the simplest of cases, you can use the following code to use a
    @class{gtk:recent-chooser-dialog} widget to select a recently used file:
    @begin{pre}
GtkWidget *dialog;

dialog = gtk_recent_chooser_dialog_new (\"Recent Documents\",
                                        parent_window,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                        NULL);

if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {
    GtkRecentInfo *info;

    info= gtk_recent_chooser_get_current_item (GTK_RECENT_CHOOSER (dialog));
    open_file (gtk_recent_info_get_uri (info));
    gtk_recent_info_unref (info);
  @}

gtk_widget_destroy (dialog);
    @end{pre}
  @end{examples}
  @see-constructor{gtk:recent-chooser-dialog-new}
  @see-constructor{gtk:recent-chooser-dialog-new-for-manager}
  @see-class{gtk:recent-chooser}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(defun recent-chooser-dialog-new (title parent &rest buttons)
 #+liber-documentation
 "@version{#2023-3-1}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{transient @class{gtk:window} parent of the dialog, or
    @code{nil}}
  @argument[buttons]{pairs with a button text or stock ID and the response ID
    for the button of type @symbol{gtk:response-type}}
  @return{A new @class{gtk:recent-chooser-dialog} object.}
  @begin{short}
    Creates a new @class{gtk:recent-chooser-dialog} widget.
  @end{short}
  This function is analogous to the @fun{gtk:dialog-new-with-buttons} function.
  @see-class{gtk:recent-chooser-dialog}
  @see-class{gtk:window}
  @see-function{gtk:dialog-new-with-buttons}"
  (let ((dialog (make-instance 'recent-chooser-dialog)))
    (when title
      (setf (window-title dialog) title))
    (when parent
      (setf (window-transient-for dialog) parent))
    (when buttons
      (apply #'dialog-add-buttons (cons dialog buttons)))
    dialog))

(export 'recent-chooser-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_dialog_new_for_manager ()
;;; ----------------------------------------------------------------------------

(defun recent-chooser-dialog-new-for-manager (title parent manager
                                              &rest buttons)
 #+liber-documentation
 "@version{#2023-3-1}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a transient @class{gtk:window} parent of the dialog, or
    @code{nil}}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @argument[buttons]{pairs with a button text or stock ID and the response ID
    for the button of type @symbol{gtk:response-type}}
  @return{A new @class{gtk:recent-chooser-dialog} widget.}
  @begin{short}
    Creates a new @class{gtk:recent-chooser-dialog} widget with a specified
    recent manager.
  @end{short}
  This is useful if you have implemented your own recent manager, or if you
  have a customized instance of a @class{gtk:recent-manager} object.
  @see-class{gtk:recent-chooser-dialog}
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-chooser-dialog-new}"
  (let ((dialog (make-instance 'recent-chooser-dialog)))
    (when title
      (setf (window-title dialog) title))
    (when parent
      (setf (window-transient-for dialog) parent))
    (when manager
      (setf (recent-chooser-recent-manager dialog) manager))
    (when buttons
      (apply #'dialog-add-buttons (cons dialog buttons)))
    dialog))

(export 'recent-chooser-dialog-new-for-manager)

;;; --- End of file gtk3.recent-chooser-dialog.lisp ----------------------------
