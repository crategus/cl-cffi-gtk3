;;; ----------------------------------------------------------------------------
;;; gtk.cell-editable.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkCellEditable
;;;
;;;     Interface for widgets which can are used for editing cells
;;;
;;; Types and Values
;;;
;;;     GtkCellEditable
;;;
;;; Functions
;;;
;;;     gtk_cell_editable_start_editing
;;;     gtk_cell_editable_editing_done
;;;     gtk_cell_editable_remove_widget
;;;
;;; Properties
;;;
;;;     editing-canceled
;;;
;;; Signals
;;;
;;;     editing-done
;;;     remove-widget
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkCellEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellEditable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkCellEditable" cell-editable
  (:export t
   :type-initializer "gtk_cell_editable_get_type")
  ((editing-canceled
    cell-editable-editing-canceled
    "editing-canceled" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'cell-editable)
      "Interface"
      (documentation 'cell-editable 'type)
 "@version{#2023-1-21}
  @begin{short}
    The @sym{gtk:cell-editable} interface must be implemented for widgets to be
    usable when editing the contents of a @class{gtk:tree-view} cell.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-done\" signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      The signal is a sign for the cell renderer to update its value from the
      @arg{editable} argument. Implementations of the @sym{gtk:cell-editable}
      interface are responsible for emitting the signal when they are done
      editing, e.g. the @class{gtk:entry} widget is emitting it when the user
      presses the @kbd{Enter} key. The @fun{gtk:cell-editable-editing-done}
      function is a convenience method for emitting the \"editing-done\" signal.
      @begin[code]{table}
        @entry[editable]{The @sym{gtk:cell-editable} object on which the signal
          was emitted.}
      @end{table}
    @subheading{The \"remove-widget\" signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      The signal is meant to indicate that the cell is finished editing, and
      the widget may now be destroyed. Implementations of the
      @sym{gtk:cell-editable} interface are responsible for emitting the signal
      when they are done editing. It must be emitted after the \"editing-done\"
      signal, to give the cell renderer a chance to update the value of the cell
      before the widget is removed. The @fun{gtk:cell-editable-remove-widget}
      function is a convenience method for emitting the \"remove-widget\"
      signal.
      @begin[code]{table}
        @entry[editable]{The @sym{gtk:cell-editable} object on which the signal
           was emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:cell-editable-editing-canceled}
  @see-function{gtk:cell-editable-editing-done}
  @see-function{gtk:cell-editable-remove-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editing-canceled"
                                               'cell-editable) t)
 "The @code{editing-canceled} property of type @code{:boolean} (Read / Write)
  @br{}
  Indicates whether editing on the cell has been canceled. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-editable-editing-canceled)
      "Accessor"
      (documentation 'cell-editable-editing-canceled 'function)
 "@version{#2023-1-21}
  @syntax[]{(gtk:cell-editable-editing-canceled object) => canceled}
  @syntax[]{(setf (gtk:cell-editable-editing-canceled object) canceled)}
  @argument[object]{a @class{gtk:cell-editable} object}
  @argument[canceled]{a boolean whether editing on the cell has been canceled}
  @begin{short}
    Accessor of the @slot[gtk:cell-editable]{editing-canceled} slot of the
    @class{gtk:cell-editable} class.
  @end{short}
  Indicates whether editing on the cell has been canceled.
  @see-class{gtk:cell-editable}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_start_editing ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_start_editing" cell-editable-start-editing) :void
 #+liber-documentation
 "@version{#2021-1-23}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @argument[event]{a @class{gdk:event} instance, or @code{nil}}
  @begin{short}
    Begins editing on a cell editable.
  @end{short}
  The @arg{event} argument is the @class{gdk:event} instance that began the
  editing process. It may be @code{nil}, in the instance that editing was
  initiated through programatic means.
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable))
  (event (g:boxed gdk:event)))

(export 'cell-editable-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_editing_done ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_editing_done" cell-editable-editing-done) :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @begin{short}
    Emits the \"editing-done\" signal.
  @end{short}
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable)))

(export 'cell-editable-editing-done)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_editable_remove_widget" cell-editable-remove-widget) :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @begin{short}
    Emits the \"remove-widget\" signal.
  @end{short}
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable)))

(export 'cell-editable-remove-widget)

;;; --- End of file gtk3.cell-editable.lisp ------------------------------------
