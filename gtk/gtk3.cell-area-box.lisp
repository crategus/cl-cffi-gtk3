;;; ----------------------------------------------------------------------------
;;; gtk3.cell-area-box.lisp
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
;;; GtkCellAreaBox
;;;
;;;     A cell area that renders GtkCellRenderers into a row or a column
;;;
;;; Types and Values
;;;
;;;     GtkCellAreaBox
;;;     GtkCellAreaBoxClass
;;;
;;; Functions
;;;
;;;     gtk_cell_area_box_new
;;;     gtk_cell_area_box_pack_start
;;;     gtk_cell_area_box_pack_end
;;;     gtk_cell_area_box_get_spacing                      Accessor
;;;     gtk_cell_area_box_set_spacing                      Accessor
;;;
;;; Properties
;;;
;;;     spacing
;;;
;;; Child Properties
;;;
;;;     align
;;;     expand
;;;     fixed-size
;;;     pack-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellArea
;;;             ╰── GtkCellAreaBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCellAreaBox implements GtkCellLayout, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellAreaBox
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellAreaBox" cell-area-box
  (:superclass cell-area
   :export t
   :interfaces ("GtkCellLayout"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_cell_area_box_get_type")
  ((spacing
    cell-area-box-spacing
    "spacing" "gint" t t)))

#+liber-documentation
(setf (documentation 'cell-area-box 'type)
 "@version{2023-2-20}
  @begin{short}
    The @sym{gtk:cell-area-box} object renders cell renderers into a row or a
    column depending on its orientation, which is a value of the
    @symbol{gtk:orientation} enumeration.
  @end{short}
  The @sym{gtk:cell-area-box} object uses a notion of packing. Packing refers
  to adding cell renderers with reference to a particular position in a
  @sym{gtk:cell-area-box} object. There are two reference positions: the start
  and the end of the box. When the @sym{gtk:cell-area-box} object is oriented in
  the @code{:vertical} orientation, the start is defined as the top of the box
  and the end is defined as the bottom. In the @code{:horizontal} orientation
  start is defined as the left side and the end is defined as the right side.

  Alignments of @class{gtk:cell-renderer} objects rendered in adjacent rows can
  be configured by configuring the @code{align} child cell property with the
  @fun{gtk:cell-area-cell-property} function or by specifying the @arg{align}
  argument to the @fun{gtk:cell-area-box-pack-start} and
  @fun{gtk:cell-area-box-pack-end} functions.
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[align]{entry}
        The @code{align} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the cell renderer should be aligned in adjacent rows. @br{}
        Default value: @em{false} @br{}
      @end{entry}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the cell renderer should receive extra space when the area
        receives more than its natural size. @br{}
        Default value: @em{false} @br{}
      @end{entry}
      @begin[fixed-size]{entry}
        The @code{fixed-size} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the cell renderer should require the same size for all rows for
        which it was requested. @br{}
        Default value: @em{true} @br{}
      @end{entry}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk:pack-type}
        (Read / Write) @br{}
        A pack type indicating whether the cell renderer is packed with
        reference to the start or end of the area. @br{}
        Default value: @code{:start} @br{}
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:cell-area-box-new}
  @see-slot{gtk:cell-area-box-spacing}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:orientation}
  @see-function{gtk:cell-area-cell-property}
  @see-function{gtk:cell-area-box-pack-start}
  @see-function{gtk:cell-area-box-pack-end}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'cell-area-box) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space to reserve between cells. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-box-spacing)
      "Accessor"
      (documentation 'cell-area-box-spacing 'function)
 "@version{#2023-2-20}
  @syntax[]{(gtk:cell-area-box-spacing object) => spacing}
  @syntax[]{(setf (gtk:cell-area-box-spacing object) spacing)}
  @argument[object]{a @class{gtk:cell-area-box} widget}
  @argument[spacing]{an integer with the space to add between
    @class{gtk:cell-renderer} objects.}
  @begin{short}
    Accessor of the @slot[gtk:cell-area-box]{spacing} slot of the
    @class{gtk:cell-area-box} class.
  @end{short}
  The @sym{gtk:cell-area-box-spacing} function gets the spacing added between
  cell renderers. The @sym{(setf gtk:cell-area-box-spacing)} function sets the
  spacing.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- cell-area-box-child-align ----------------------------------------------

(define-child-property cell-area-box-child-align "align" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-box-child-align)
      "Accessor"
      (documentation 'cell-area-box-child-align 'function)
 "@version{#2023-2-20}
  @syntax[]{(gtk:cell-area-box-child-align container child) => align}
  @syntax[]{(setf (gtk:cell-area-box-child-align container child) align)}
  @argument[container]{a @class{gtk:cell-area-box} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[align]{a boolean whether the cell renderer should be aligned in
    admacent rows}
  @begin{short}
    Accessor of the @code{align} child property of the
    @class{gtk:cell-area-box} class.
  @end{short}
  Whether the cell renderer should be aligned in adjacent rows.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:widget}")

;;; --- cell-area-box-child-expand ---------------------------------------------

(define-child-property cell-area-box-child-expand "expand" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-box-child-expand)
      "Accessor"
      (documentation 'cell-area-box-child-expand 'function)
 "@version{#2023-2-20}
  @syntax[]{(gtk:cell-area-box-child-expand container child) => expand}
  @syntax[]{(setf (gtk:cell-area-box-child-expand container child) expand)}
  @argument[container]{a @class{gtk:cell-area-box} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[expand]{a boolean whether the cell renderer should receive extra
    space}
  @begin{short}
    Accessor of the @code{expand} child property of the
    @class{gtk:cell-area-box} class.
  @end{short}
  Whether the cell renderer should receive extra space when the area receives
  more than its natural size.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:widget}")

;;; --- cell-area-box-child-fixed-size -----------------------------------------

(define-child-property cell-area-box-child-fixed-size
                       "fixed-size" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-box-child-fixed-size)
      "Accessor"
      (documentation 'cell-area-box-child-fixed-size 'function)
 "@version{#2023-2-20}
  @syntax[]{(gtk:cell-area-box-child-fixed-size container child) => size}
  @syntax[]{(setf (gtk:cell-area-box-child-fixed-size container child) size)}
  @argument[container]{a @class{gtk:cell-area-box} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[size]{a boolean whether the cell renderer should require the same
    size for all rows}
  @begin{short}
    Accessor of the @code{fixed-size} child property of the
    @class{gtk:cell-area-box} class.
  @end{short}
  Whether the cell renderer should require the same size for all rows for which
  it was requested.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:widget}")

;;; --- cell-area-box-child-pack-type ------------------------------------------

(define-child-property cell-area-box-child-pack-type
                       "pack-type" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-box-child-pack-type)
      "Accessor"
      (documentation 'cell-area-box-child-pack-type 'function)
 "@version{#2023-2-20}
  @syntax[]{(gtk:cell-area-box-child-pack-type container child) => pack-type}
  @syntax[]{(setf (gtk:cell-area-box-child-pack-type container child) pack-type)}
  @argument[container]{a @class{gtk:cell-area-box} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[pack-type]{a @symbol{gtk:pack-type} value}
  @begin{short}
    Accessor of the @code{pack-type} child property of the
    @class{gtk:cell-area-box} class.
  @end{short}
  A @symbol{gtk:pack-type} value indicating whether the cell renderer is packed
  with reference to the start or end of the area.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-area-box-new))

(defun cell-area-box-new ()
 #+liber-documentation
 "@version{#2023-2-20}
  @return{A newly created @class{gtk:cell-area-box} object.}
  @short{Creates a new cell area box.}
  @see-class{gtk:cell-area-box}"
  (make-instance 'cell-area-box))

(export 'cell-area-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_box_pack_start" %cell-area-box-pack-start) :void
  (box (g:object cell-area-box))
  (renderer (g:object cell-renderer))
  (expand :boolean)
  (align :boolean)
  (fixed :boolean))

(defun cell-area-box-pack-start (box renderer
                                     &key (expand t) (align t) (fixed t))
 #+liber-documentation
 "@version{#2023-2-20}
  @argument[box]{a @class{gtk:cell-area-box} widget}
  @argument[renderer]{a @class{gtk:cell-renderer} object to add}
  @argument[expand]{a boolean whether @arg{renderer} should receive extra space
    when the area receives more than its natural size}
  @argument[align]{a boolean whether @arg{renderer} should be aligned in
    adjacent rows}
  @argument[fixed]{a boolean whether @arg{renderer} should have the same size
    in all rows}
  @begin{short}
    Adds a renderer to the cell area box, packed with reference to the start of
    the box.
  @end{short}
  The renderer is packed after any other @class{gtk:cell-renderer} object
  packed with reference to the start of the box.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-box-pack-end}"
  (%cell-area-box-pack-start box renderer expand align fixed))

(export 'cell-area-box-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_box_pack_end ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_box_pack_end" %cell-area-box-pack-end) :void
  (box (g:object cell-area-box))
  (renderer (g:object cell-renderer))
  (expand :boolean)
  (align :boolean)
  (fixed :boolean))

(defun cell-area-box-pack-end (box renderer
                                   &key (expand t) (align t) (fixed t))
 #+liber-documentation
 "@version{#2023-2-20}
  @argument[box]{a @class{gtk:cell-area-box} widget}
  @argument[renderer]{a @class{gtk:cell-renderer} object to add}
  @argument[expand]{a boolean whether @arg{renderer} should receive extra space
    when the area receives more than its natural size}
  @argument[align]{a boolean whether @arg{renderer} should be aligned in
    adjacent rows}
  @argument[fixed]{a boolean whether @arg{renderer} should have the same size
    in all rows}
  @begin{short}
    Adds a renderer to cell area box, packed with reference to the end of
    the box.
  @end{short}
  The renderer is packed after, away from end of, any other
  @class{gtk:cell-renderer} object packed with reference to the end of the box.
  @see-class{gtk:cell-area-box}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-box-pack-start}"
  (%cell-area-box-pack-end box renderer expand align fixed))

(export 'cell-area-box-pack-end)

;;; --- End of file gtk3.cell-area-box.lisp ------------------------------------
