;;; ----------------------------------------------------------------------------
;;; gtk3.page-setup-unix-dialog.lisp
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
;;; GtkPageSetupUnixDialog
;;;
;;;     A page setup dialog
;;;
;;; Types and Values
;;;
;;;     GtkPageSetupUnixDialog
;;;
;;; Functions
;;;
;;;     gtk_page_setup_unix_dialog_new
;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup
;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings
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
;;;                             ╰── GtkPageSetupUnixDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPageSetupUnixDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPageSetupUnixDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPageSetupUnixDialog" page-setup-unix-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_page_setup_unix_dialog_get_type")
  nil)

#+liber-documentation
(setf (documentation 'page-setup-unix-dialog 'type)
 "@version{#2023-03-21}
  @begin{short}
    The @class{gtk:page-setup-unix-dialog} widget implements a page setup dialog
    for platforms which do not provide a native page setup dialog, like Unix.
  @end{short}

  @image[pagesetupdialog]{Figure: GtkPageSetupUnixDialog}

  It can be used very much like any other GTK dialog, at the cost of the
  portability offered by the high-level printing API.
  @begin[Examples]{dictionary}
    @begin{pre}
(defun create-page-setup-dialog ()
  (let ((page-setup (gtk:page-setup-new))
        (dialog (make-instance 'gtk:page-setup-unix-dialog
                               :title \"Page Setup Dialog\"
                               :default-height 250
                               :default-width 400)))
    ;; Load and set Page setup from file
    (if (gtk:page-setup-load-file page-setup (rel-path \"page-setup.ini\"))
        (format t \"PAGE SETUP successfully loaded~%\")
        (format t \"PAGE SETUP cannot be loaded, use standard settings~%\"))
    (setf (gtk:page-setup-unix-dialog-page-setup dialog) page-setup)
    ;; Run the dialog
    (when (eq :ok (gtk:dialog-run dialog))
      (setf page-setup (gtk:page-setup-unix-dialog-page-setup dialog))
      (gtk:page-setup-to-file page-setup (rel-path \"page-setup.ini\")))
    ;; Destroy the dialog
    (gtk:widget-destroy dialog)))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:page-setup-unix-dialog-new}
  @see-class{gtk:page-setup}")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_new
;;; ----------------------------------------------------------------------------

(defun page-setup-unix-dialog-new (title parent)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[title]{a string for the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog,
    or @code{nil}}
  @return{The new @class{gtk:page-setup-unix-dialog} widget.}
  @begin{short}
    Creates a new page setup dialog.
  @end{short}
  @see-class{gtk:page-setup-unix-dialog}"
  (let ((dialog (make-instance 'page-setup-unix-dialog)))
    (when title
      (setf (window-title dialog) title))
    (when parent
      (setf (window-transient-for dialog) parent))
    dialog))

(export 'page-setup-unix-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_page_setup
;;; gtk_page_setup_unix_dialog_get_page_setup
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-unix-dialog-page-setup) (page-setup dialog)
  (cffi:foreign-funcall "gtk_page_setup_unix_dialog_set_page_setup"
                        (g:object page-setup-unix-dialog) dialog
                        (g:object page-setup) page-setup :void)
  page-setup)

(cffi:defcfun ("gtk_page_setup_unix_dialog_get_page_setup"
               page-setup-unix-dialog-page-setup) (g:object page-setup)
 #+liber-documentation
 "@version{#2023-03-21}
  @syntax{(gtk:page-setup-unix-dialog-page-setup dialog) => setup}
  @syntax{(setf (gtk:page-setup-unix-dialog-page-setup dialog) setup)}
  @argument[dialog]{a @class{gtk:page-setup-unix-dialog} widget}
  @argument[setup]{a @class{gtk:page-setup} object}
  @begin{short}
    Accessor for the page setup of the page setup dialog.
  @end{short}
  The @fun{gtk:page-setup-unix-dialog-page-setup} function gets the currently
  selected page setup from the page setup dialog. The
  @setf{gtk:page-setup-unix-dialog-page-setup} function sets the page setup
  object from which the page setup dialog takes its values.
  @see-class{gtk:page-setup-unix-dialog}
  @see-class{gtk:page-setup}"
  (dialog (g:object page-setup-unix-dialog)))

(export 'page-setup-unix-dialog-page-setup)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_unix_dialog_set_print_settings
;;; gtk_page_setup_unix_dialog_get_print_settings
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-unix-dialog-print-settings) (settings dialog)
  (cffi:foreign-funcall "gtk_page_setup_unix_dialog_set_print_settings"
                        (g:object page-setup-unix-dialog) dialog
                        (g:object print-settings) settings :void)
  settings)

(cffi:defcfun ("gtk_page_setup_unix_dialog_get_print_settings"
               page-setup-unix-dialog-print-settings) :void
 #+liber-documentation
 "@version{#2023-03-21}
  @syntax{(gtk:page-setup-unix-dialog-print-settings dialog) => settings}
  @syntax{(setf (gtk:page-setup-unix-dialog-print-settings dialog) settings)}
  @argument[dialog]{a @class{gtk:page-setup-unix-dialog} widget}
  @argument[settings]{a @class{gtk:print-settings} object}
  @begin{short}
    Accesor for the print settings of the page setup dialog.
  @end{short}
  The @fun{gtk:page-setup-unix-dialog-print-settings} function gets the current
  print settings from the page setup dialog. The
  @setf{gtk:page-setup-unix-dialog-print-settings} function sets the print
  settings from which the page setup dialog takes its values.
  @see-class{gtk:page-setup-unix-dialog}
  @see-class{gtk:print-settings}"
  (dialog (g:object page-setup-unix-dialog)))

(export 'page-setup-unix-dialog-print-settings)

;;; --- End of file gtk3.page-setup-unix-dialog.lisp ---------------------------
