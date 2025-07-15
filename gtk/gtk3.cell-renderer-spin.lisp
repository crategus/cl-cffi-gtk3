;;; ----------------------------------------------------------------------------
;;; gtk3.cell-renderer-spin.lisp
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
;;; GtkCellRendererSpin
;;;
;;;     Renders a spin button in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererSpin
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_spin_new
;;;
;;; Properties
;;;
;;;     adjustment
;;;     climb-rate
;;;     digits
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererText
;;;                 ╰── GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCellRendererSpin" cell-renderer-spin
  (:superclass cell-renderer-text
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_spin_get_type")
  ((adjustment
    cell-renderer-spin-adjustment
    "adjustment" "GtkAdjustment" t t)
   (climb-rate
    cell-renderer-spin-climb-rate
    "climb-rate" "gdouble" t t)
   (digits
    cell-renderer-spin-digits
    "digits" "guint" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-spin 'type)
 "@version{2024-03-17}
  @begin{short}
    The @class{gtk:cell-renderer-spin} object renders text in a cell like
    the @class{gtk:cell-renderer-text} object from which it is derived.
  @end{short}
  But while the @class{gtk:cell-renderer-text} object offers a simple entry to
  edit the text, the @class{gtk:cell-renderer-spin} object offers a
  @class{gtk:spin-button} widget. Of course, that means that the text has to be
  parseable as a floating point number.

  The range of the spin button is taken from the adjustment property of the
  cell renderer, which can be set explicitly or mapped to a column in the tree
  model, like all properties of cell renders. The @class{gtk:cell-renderer-spin}
  object also has the @slot[gtk:cell-renderer-spin]{climb-rate} and
  @slot[gtk:cell-renderer-spin]{digits} properties to display. Other
  @class{gtk:spin-button} properties can be set in a handler for the
  @sig[gtk:cell-renderer]{editing-started} signal.
  @see-constructor{gtk:cell-renderer-spin-new}
  @see-slot{gtk:cell-renderer-spin-adjustment}
  @see-slot{gtk:cell-renderer-spin-climb-rate}
  @see-slot{gtk:cell-renderer-spin-digits}
  @see-class{gtk:cell-renderer-text}
  @see-class{gtk:spin-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-renderer-spin-adjustment --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment"
                                               'cell-renderer-spin) t)
 "The @code{adjustment} property of type @class{gtk:adjustment} (Read / Write)
  @br{}
  The adjustment that holds the value of the spin button. This must be
  non-@code{nil} for the cell renderer to be editable.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spin-adjustment)
      "Accessor"
      (documentation 'cell-renderer-spin-adjustment 'function)
 "@version{2024-03-17}
  @syntax{(gtk:cell-renderer-spin-adjustment object) => adjustment}
  @syntax{(setf (gtk:cell-renderer-spin-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:cell-renderer-spin} object}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spin]{adjustment} slot of the
    @class{gtk:cell-renderer-spin} class.
  @end{short}
  The adjustment that holds the value of the spin button. This must be
  non-@code{nil} for the cell renderer to be editable.
  @see-class{gtk:cell-renderer-spin}
  @see-class{gtk:adjustment}")

;;; --- gtk:cell-renderer-spin-climb-rate --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "climb-rate"
                                               'cell-renderer-spin) t)
 "The @code{climb-rate} property of type @code{:double} (Read / Write) @br{}
  The acceleration rate when you hold down a button. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spin-climb-rate)
      "Accessor"
      (documentation 'cell-renderer-spin-climb-rate 'function)
 "@version{2025-07-07}
  @syntax{(gtk:cell-renderer-spin-climb-rate object) => rate}
  @syntax{(setf (gtk:cell-renderer-spin-climb-rate object) rate)}
  @argument[object]{a @class{gtk:cell-renderer-spin} object}
  @argument[rate]{a number coerced to a double float for the acceleration rate}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spin]{climb-rate} slot of the
    @class{gtk:cell-renderer-spin} class.
  @end{short}
  The acceleration rate when you hold down a button.
  @see-class{gtk:cell-renderer-spin}")

;;; --- gtk:cell-renderer-spin-digits ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "digits" 'cell-renderer-spin) t)
 "The @code{digits} property of type @code{:uint} (Read / Write) @br{}
  The number of decimal places to display. @br{}
  Allowed values: <= 20 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spin-digits)
      "Accessor"
      (documentation 'cell-renderer-spin-digits 'function)
 "@version{2025-07-07}
  @syntax{(gtk:cell-renderer-spin-digits object) => digits}
  @syntax{(setf (gtk:cell-renderer-spin-digits object) digits)}
  @argument[object]{a @class{gtk:cell-renderer-spin} object}
  @argument[digits]{an unsigned integer for the number of decimal places to
    display}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spin]{digits} slot of the
    @class{gtk:cell-renderer-spin} class.
  @end{short}
  The number of decimal places to display.
  @see-class{gtk:cell-renderer-spin}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spin_new
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-spin-new))

(defun cell-renderer-spin-new ()
 #+liber-documentation
 "@version{2025-07-07}
  @return{The new @class{gtk:cell-renderer-spin} object.}
  @short{Creates a new cell renderer spin object.}
  @see-class{gtk:cell-renderer-spin}"
  (make-instance 'cell-renderer-spin))

(export 'cell-renderer-spin-new)

;;; --- End of file gtk3.cell-renderer-spin.lisp -------------------------------
