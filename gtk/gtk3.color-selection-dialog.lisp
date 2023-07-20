;;; ----------------------------------------------------------------------------
;;; gtk3.color-selection-dialog.lisp
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
;;; GtkColorSelectionDialog
;;;
;;;     Deprecated dialog box for selecting a color
;;;
;;; Types and Values
;;;
;;;     GtkColorSelectionDialog
;;;
;;; Functions
;;;
;;;    gtk_color_selection_dialog_new
;;;    gtk_color_selection_dialog_get_color_selection
;;;
;;; Properties
;;;
;;;     cancel-button
;;;     color-selection
;;;     help-button
;;;     ok-button
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
;;;                             ╰── GtkColorSelectionDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorSelectionDialog implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorSelection
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkColorSelectionDialog" color-selection-dialog
  (:superclass dialog
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable")
   :type-initializer "gtk_color_selection_dialog_get_type")
  ((cancel-button
    color-selection-dialog-cancel-button
    "cancel-button" "GtkWidget" t nil)
   (color-selection
    color-selection-dialog-color-selection
    "color-selection" "GtkWidget" t nil)
   (help-button
    color-selection-dialog-help-button
    "help-button" "GtkWidget" t nil)
   (ok-button
    color-selection-dialog-ok-button
    "ok-button" "GtkWidget" t nil)))

#+liber-documentation
(setf (documentation 'color-selection-dialog 'type)
 "@version{2023-6-15}
  @begin{short}
    The @sym{gtk:color-selection-dialog} widget provides a standard dialog
    which allows the user to select a color much like the
    @class{gtk:file-chooser-dialog} widget provides a standard dialog for file
    selection.
  @end{short}

  Use the @fun{gtk:color-selection-dialog-color-selection} function to get the
  @class{gtk:color-selection} widget contained within the dialog. Use this
  widget and its @fun{gtk:color-selection-current-color} function to gain
  access to the selected color. Connect a handler for the \"color-changed\"
  signal of the widget to be notified when the color changes.
  @begin[GtkColorSelectionDialog as GtkBuildable]{dictionary}
    The @sym{gtk:color-selection-dialog} implementation of the
    @class{gtk:buildable} interface exposes the embedded
    @class{gtk:color-selection} widget as internal child with the name
    @code{color_selection}. It also exposes the buttons with the names
    @code{ok_button}, @code{cancel_button} and @code{help_button}.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @sym{gtk:color-selection-dialog} widget is deprecated since GTK 3.4 and
    should not be used in newly written code. Use the
    @class{gtk:color-chooser-dialog} widget instead.
  @end{dictionary}
  @see-constructor{gtk:color-selection-dialog-new}
  @see-slot{gtk:color-selection-dialog-cancel-button}
  @see-slot{gtk:color-selection-dialog-color-selection}
  @see-slot{gtk:color-selection-dialog-help-button}
  @see-slot{gtk:color-selection-dialog-ok-button}
  @see-class{gtk:color-selection}
  @see-class{gtk:color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- color-selection-dialog-cancel-button -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cancel-button"
                                               'color-selection-dialog) t)
 "The @code{cancel-button} property of type @class{gtk:widget} (Read) @br{}
  The Cancel button of the dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-dialog-cancel-button)
      "Accessor"
      (documentation 'color-selection-dialog-cancel-button 'function)
 "@version{2023-6-15}
  @syntax[]{(gtk:color-selection-dialog-cancel-button object) => button}
  @syntax[]{(setf (gtk:color-selection-dialog-cancel-button object) button)}
  @argument[object]{a @class{gtk:color-selection-dialog} widget}
  @argument[button]{a @class{gtk:widget} button}
  @begin{short}
    Accessor of the @slot[gtk:color-selection-dialog]{cancel-button} slot of
    the @class{gtk:color-selection-dialog} class.
  @end{short}
  The Cancel button of the color selection dialog.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-selection-dialog-cancel-button} function is deprecated
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection-dialog}
  @see-class{gtk:widget}")

;;; --- color-selection-dialog-color-selection ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "color-selection"
                                               'color-selection-dialog) t)
 "The @code{color-selection} property of type @class{gtk:widget} (Read) @br{}
  The color selection embedded in the dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-dialog-color-selection)
      "Accessor"
      (documentation 'color-selection-dialog-color-selection 'function)
 "@version{2023-6-15}
  @syntax[]{(gtk:color-selection-dialog-color-selection object) => selection}
  @syntax[]{(setf (gtk:color-selection-dialog-color-selection object) selection)}
  @argument[object]{a @class{gtk:color-selection-dialog} widget}
  @argument[selection]{a @class{gtk:color-selection} widget}
  @begin{short}
    Accessor of the @slot[gtk:color-selection-dialog]{color-selection} slot of
    the @class{gtk:color-selection-dialog} class.
  @end{short}
  The @sym{gtk:color-selection-dialog-color-selection} function retrieves the
  @class{gtk:color-selection} widget embedded in the dialog.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-selection-dialog-color-selection} function is deprecated
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection-dialog}
  @see-class{gtk:color-selection}")

;;; --- color-selection-dialog-help-button -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "help-button"
                                               'color-selection-dialog) t)
 "The @code{help-button} property of type @class{gtk:widget} (Read) @br{}
  The Help button of the dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-dialog-help-button)
      "Accessor"
      (documentation 'color-selection-dialog-help-button 'function)
 "@version{2023-6-15}
  @syntax[]{(gtk:color-selection-dialog-help-button object) => button}
  @syntax[]{(setf (gtk:color-selection-dialog-help-button object) button)}
  @argument[object]{a @class{gtk:color-selection-dialog} widget}
  @argument[button]{a @class{gtk:widget} button}
  @begin{short}
    Accessor of the @slot[gtk:color-selection-dialog]{help-button} slot of the
    @class{gtk:color-selection-dialog} class.
  @end{short}
  The Help button of the color selection dialog.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-selection-dialog-help-button} function is deprecated
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection-dialog}
  @see-class{gtk:widget}")

;;; --- color-selection-dialog-ok-button ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ok-button"
                                               'color-selection-dialog) t)
 "The @code{ok-button} property of type @class{gtk:widget} (Read) @br{}
  The Ok button of the dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-dialog-ok-button)
      "Accessor"
      (documentation 'color-selection-dialog-ok-button 'function)
 "@version{2023-6-15}
  @syntax[]{(gtk:color-selection-dialog-ok-button object) => button}
  @syntax[]{(setf (gtk:color-selection-dialog-ok-button object) button)}
  @argument[object]{a @class{gtk:color-selection-dialog} widget}
  @argument[button]{a @class{gtk:widget} button}
  @begin{short}
    Accessor of the @slot[gtk:color-selection-dialog]{ok-button} slot of the
    @class{gtk:color-selection-dialog} class.
  @end{short}
  The Ok button of the color selection dialog.
  @begin[Warning]{dictionary}
    The @sym{gtk:color-selection-dialog-ok-button} function is deprecated
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection-dialog}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_dialog_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline color-selection-dialog-new))

(defun color-selection-dialog-new (title)
 #+liber-documentation
 "@version{2023-6-15}
  @argument[title]{a string containing the title text for the dialog}
  @return{A @class{gtk:color-selection-dialog} widget.}
  @begin{short}
    Creates a new color selection dialog.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk:color-selection-dialog-new} function is deprecated and should
    not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection-dialog}"
  (make-instance 'color-selection-dialog
                 :title (if title title (cffi:null-pointer))))

(export 'color-selection-dialog-new)

;;; --- End of file gtk3.color-selection-dialog.lisp ---------------------------
