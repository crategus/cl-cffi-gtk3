;;; ----------------------------------------------------------------------------
;;; gtk3.font-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkFontChooserDialog
;;;
;;;     A dialog for selecting fonts
;;;
;;; Types and Values
;;;
;;;     GtkFontChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_dialog_new
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
;;;                             ╰── GtkFontChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFontChooserDialog implements AtkImplementorIface, GtkBuildable and
;;;     GtkFontChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserDialog
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontChooserDialog" font-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_dialog_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font-chooser-dialog 'type)
 "@version{#2023-3-5}
  @begin{short}
    The @sym{gtk:font-chooser-dialog} widget is a dialog for selecting a font.
  @end{short}
  It implements the @class{gtk:font-chooser} interface.

  @image[font-chooser-dialog]{}
  @begin[GtkFontChooserDialog as GtkBuildable]{dictionary}
    The @sym{gtk:font-chooser-dialog} implementation of the
    @class{gtk:buildable} interface exposes the buttons with the names
    @code{select_button} and @code{cancel_button}.
  @end{dictionary}
  @see-constructor{gtk:font-chooser-dialog-new}
  @see-class{gtk:font-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-chooser-dialog-new))

(defun font-chooser-dialog-new (title parent)
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog, or
    @code{nil}}
  @return{A new @class{gtk:font-chooser-dialog} widget.}
  @short{Creates a new font chooser dialog.}
  @see-class{gtk:font-chooser-dialog}
  @see-class{gtk:window}"
  (make-instance 'font-chooser-dialog
                 :title title
                 :parent parent))

(export 'font-chooser-dialog-new)

;;; --- End of file gtk3.font-chooser-dialog.lisp ------------------------------
