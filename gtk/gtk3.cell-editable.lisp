;;; ----------------------------------------------------------------------------
;;; gtk3.cell-editable.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
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

(gobject:define-ginterface "GtkCellEditable" cell-editable
  (:export t
   :type-initializer "gtk_cell_editable_get_type")
  ((editing-canceled
    cell-editable-editing-canceled
    "editing-canceled" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'cell-editable)
      "Interface"
      (documentation 'cell-editable 'type)
 "@version{2024-3-17}
  @begin{short}
    The @class{gtk:cell-editable} interface must be implemented for widgets to
    be usable when editing the contents of a @class{gtk:tree-view} cell.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"editing-done\" signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      The signal is a sign for the cell renderer to update its value from the
      @arg{editable} argument. Implementations of the @class{gtk:cell-editable}
      interface are responsible for emitting the signal when they are done
      editing, e.g. the @class{gtk:entry} widget is emitting it when the user
      presses the @kbd{Enter} key. The @fun{gtk:cell-editable-editing-done}
      function is a convenience method for emitting the @code{\"editing-done\"}
      signal.
      @begin[code]{table}
        @entry[editable]{The @class{gtk:cell-editable} object on which the
          signal was emitted.}
      @end{table}
    @subheading{The \"remove-widget\" signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      The signal is meant to indicate that the cell is finished editing, and
      the widget may now be destroyed. Implementations of the
      @class{gtk:cell-editable} interface are responsible for emitting the
      signal when they are done editing. It must be emitted after the
      @code{\"editing-done\"} signal, to give the cell renderer a chance to
      update the value of the cell before the widget is removed. The
      @fun{gtk:cell-editable-remove-widget} function is a convenience method
      for emitting the @code{\"remove-widget\"} signal.
      @begin[code]{table}
        @entry[editable]{The @class{gtk:cell-editable} object on which the
          signal was emitted.}
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
 "@version{2024-3-17}
  @syntax{(gtk:cell-editable-editing-canceled object) => canceled}
  @syntax{(setf (gtk:cell-editable-editing-canceled object) canceled)}
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

(cffi:defcfun ("gtk_cell_editable_start_editing" cell-editable-start-editing)
    :void
 #+liber-documentation
 "@version{#2024-3-17}
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

(cffi:defcfun ("gtk_cell_editable_editing_done" cell-editable-editing-done)
    :void
 #+liber-documentation
 "@version{#2024-3-17}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @begin{short}
    Emits the @code{\"editing-done\"} signal.
  @end{short}
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable)))

(export 'cell-editable-editing-done)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_editable_remove_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_editable_remove_widget" cell-editable-remove-widget)
    :void
 #+liber-documentation
 "@version{#2024-3-17}
  @argument[editable]{a @class{gtk:cell-editable} object}
  @begin{short}
    Emits the @code{\"remove-widget\"} signal.
  @end{short}
  @see-class{gtk:cell-editable}"
  (editable (g:object cell-editable)))

(export 'cell-editable-remove-widget)

;;; --- End of file gtk3.cell-editable.lisp ------------------------------------
