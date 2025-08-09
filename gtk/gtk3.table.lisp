;;; ----------------------------------------------------------------------------
;;; gtk3.table.lisp
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
;;; GtkTable
;;;
;;;     Pack widgets in regular patterns
;;;
;;; Types and Values
;;;
;;;     GtkTable
;;;     GtkAttachOptions
;;;
;;; Accessors
;;;
;;;     gtk_table_get_homogeneous
;;;     gtk_table_set_homogeneous
;;;
;;; Functions
;;;
;;;     gtk_table_new
;;;     gtk_table_resize
;;;     gtk_table_get_size
;;;     gtk_table_attach
;;;     gtk_table_attach_defaults                           not implemented
;;;     gtk_table_set_row_spacing
;;;     gtk_table_set_col_spacing
;;;     gtk_table_set_row_spacings
;;;     gtk_table_set_col_spacings
;;;     gtk_table_get_default_row_spacing
;;;     gtk_table_get_row_spacing
;;;     gtk_table_get_col_spacing
;;;     gtk_table_get_default_col_spacing
;;;
;;; Properties
;;;
;;;     column-spacing
;;;     homogeneous
;;;     n-columns
;;;     n-rows
;;;     row-spacing
;;;
;;; Child Properties
;;;
;;;     bottom-attach
;;;     left-attach
;;;     right-attach
;;;     top-attach
;;;     x-options
;;;     x-padding
;;;     y-options
;;;     y-padding
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkTable
;;;
;;; Implemented Interfaces
;;;
;;;     GtkTable implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAttachOptions
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkAttachOptions" attach-options
  (:export t
   :type-initializer "gtk_attach_options_get_type")
  (:expand #.(ash 1 0))
  (:shrink #.(ash 1 1))
  (:fill #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'attach-options)
      "GFlags"
      (liber:symbol-documentation 'attach-options)
 "@version{2025-06-27}
  @begin{declaration}
(gobject:define-gflags \"GtkAttachOptions\" attach-options
  (:export t
   :type-initializer \"gtk_attach_options_get_type\")
  (:expand #.(ash 1 0))
  (:shrink #.(ash 1 1))
  (:fill #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:expand]{The widget should expand to take up any extra space in
        its container that has been allocated.}
      @entry[:shrink]{The widget should shrink as and when possible.}
      @entry[:fill]{The widget should fill the space allocated to it.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Denotes the expansion properties that a widget will have in a
    @class{gtk:table} widget when it or its parent is resized.
  @end{short}
  @see-class{gtk:table}")

;;; ----------------------------------------------------------------------------
;;; GtkTable
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTable" table
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_table_get_type")
  ((column-spacing
    table-column-spacing
    "column-spacing" "guint" t t)
   (homogeneous
    table-homogeneous
    "homogeneous" "gboolean" t t)
   (n-columns
    table-n-columns
    "n-columns" "guint" t t)
   (n-rows
    table-n-rows
    "n-rows" "guint" t t)
   (row-spacing
    table-row-spacing
    "row-spacing" "guint" t t)))

#+liber-documentation
(setf (documentation 'table 'type)
 "@version{2025-07-11}
  @begin{short}
    The @class{gtk:table} widget allows the programmer to arrange widgets in
    rows and columns, making it easy to align many widgets next to each other,
    horizontally and vertically.
  @end{short}

  Tables are created with a call to the @fun{gtk:table-new} function, the size
  of which can later be changed with the @fun{gtk:table-resize} function.
  Widgets can be added to a table using the @fun{gtk:table-attach} function.

  To alter the space next to a specific row, use the
  @fun{gtk:table-set-row-spacing} function, and for a column the
  @fun{gtk:table-set-col-spacing} function. The gaps between all rows or
  columns can be changed by calling the @fun{gtk:table-row-spacing} or
  @fun{gtk:table-column-spacing} functions respectively. Note that spacing is
  added between the children, while padding added by the @fun{gtk:table-attach}
  function is added on either side of the widget it belongs to.

  The @fun{gtk:table-homogeneous} function can be used to set whether all
  cells in the table will resize themselves to the size of the largest widget
  in the table.
  @begin[Warning]{dictionary}
    The @class{gtk:table} widget has been deprecated since GTK 3.4. Use the
    @class{gtk:grid} widget instead. It provides the same capabilities as the
    @class{gtk:table} widget for arranging widgets in a rectangular grid, but
    does support height-for-width geometry management.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[table:bottom-attach]{property}
      The @code{bottom-attach} child property of type @code{:uint}
      (Read / Write) @br{}
      The row number to attach the bottom of the child widget to. @br{}
      Allowed values: [1,65535] @br{}
      Default value: 1
    @end{property}
    @begin[table:left-attach]{property}
      The @code{left-attach} child property of type @code{:uint}
      (Read / Write) @br{}
      The column number to attach the left side of the child widget to. @br{}
      Allowed values: <= 65535 @br{}
      Default value: 0
    @end{property}
    @begin[table:right-attach]{property}
      The @code{right-attach} child property of type @code{:uint}
      (Read / Write) @br{}
      The column number to attach the right side of a child widget to. @br{}
      Allowed values: [1,65535] @br{}
      Default value: 1
    @end{property}
    @begin[table:top-attach]{property}
      The @code{top-attach} child property of type @code{:uint}
      (Read / Write) @br{}
      The row number to attach the top of a child widget to. @br{}
      Allowed values: <= 65535 @br{}
      Default value: 0
    @end{property}
    @begin[table:x-options]{property}
      The @code{x-options} child property of type @sym{gtk:attach-options}
      (Read / Write) @br{}
      The options specifying the horizontal behaviour of the child widget. @br{}
      Default value: @val[gtk:attach-options]{'(:expand :fill)}
    @end{property}
    @begin[table:x-padding]{property}
      The @code{x-padding} child property of type @code{:uint}
      (Read / Write) @br{}
      Extra space to put between the child widget and its left and right
      neighbors, in pixels. @br{}
      Allowed values: <= 65535 @br{}
      Default value: 0
    @end{property}
    @begin[table:y-options]{property}
      The @code{y-options} child property of type @sym{gtk:attach-options}
      (Read / Write) @br{}
      The options specifying the vertical behaviour of the child widget. @br{}
      Default value: @val[gtk:attach-options]{'(:expand :fill)}
    @end{property}
    @begin[table:y-padding]{property}
      The @code{y-padding} child property of type @code{:uint}
      (Read / Write) @br{}
      The extra space to put between the child widget and its upper and lower
      neighbors, in pixels. @br{}
      Allowed values: <= 65535 @br{}
      Default value: 0
    @end{property}
  @end{dictionary}
  @see-constructor{gtk:table-new}
  @see-slot{gtk:table-column-spacing}
  @see-slot{gtk:table-homogeneous}
  @see-slot{gtk:table-n-columns}
  @see-slot{gtk:table-n-rows}
  @see-slot{gtk:table-row-spacing}
  @see-class{gtk:grid}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:table-column-spacing -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-spacing" 'table) t)
 "The @code{column-spacing} property of type @code{:uint}
  (Read / Write) @br{}
  The amount of space between two consecutive columns. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'table-column-spacing)
      "Accessor"
      (documentation 'table-column-spacing 'function)
 "@version{2025-07-06}
  @syntax{(gtk:table-column-spacing object) => spacing}
  @syntax{(setf (gtk:table-column-spacing object) spacing)}
  @argument[table]{a @class{gtk:table} widget}
  @argument[spacing]{an unsigned integer for the number of pixels of space to
    place between every column in the table}
  @begin{short}
    Accessor of the @slot[gtk:table]{column-spacing} property of the
    @class{gtk:table} class.
  @end{short}
  The @fun{gtk:table-column-spacing} function gets the default column spacing
  for the table. The @setf{gtk:table-column-spacing} function sets the column
  spacing. This is the spacing that will be used for newly added columns.
  @begin[Lisp implementation]{dictionary}
    The C library has the @code{gtk_table_get_default_col_spacing ()}
    and @code{gtk_table_set_col_spacings ()} functions, which correspond to the
    @fun{gtk:table-column-spacing} function. These C functions are not
    implemented in the Lisp library.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-column-spacing} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget with the @fun{gtk:grid-column-spacing} function.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-function{gtk:grid-column-spacing}")

;;; --- gtk:table-homogeneous --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "homogeneous" 'table) t)
 "The @code{homogeneous} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, the table cells are all the same width/height. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'table-homogeneous)
      "Accessor"
      (documentation 'table-homogeneous 'function)
 "@version{2024-6-27}
  @syntax{(gtk:table-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:table-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:table} widget}
  @argument[homogeneous]{set to @em{true} to ensure all table cells are the
    same size, set to @em{false} if this is not the desired behaviour}
  @begin{short}
    Accessor of the @slot[gtk:table]{homogeneous} slot of the
    @class{gtk:table} class.
  @end{short}
  The @fun{gtk:table-homogeneous} function returns whether the table cells are
  all constrained to the same width and height. The @setf{gtk:table-homogeneous}
  function changes the property.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-homogeneous} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the @class{gtk:grid}
    widget with the @fun{gtk:grid-row-homogeneous} and
    @fun{gtk:grid-column-homogeneous} functions.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-function{gtk:grid-column-homogeneous}
  @see-function{gtk:grid-row-homogeneous}")

;;; --- gtk:table-n-columns ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-columns" 'table) t)
 "The @code{n-columns} property of type @code{:uint} (Read / Write) @br{}
  The number of columns in the table. @br{}
  Allowed values: [1,65535] @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'table-n-columns)
      "Accessor"
      (documentation 'table-n-columns 'function)
 "@version{2025-07-06}
  @syntax{(gtk:table-n-columns object) => n-columns}
  @syntax{(setf (gtk:table-n-columns object) n-columns)}
  @argument[object]{a @class{gtk:table} widget}
  @argument[n-columns]{an unsigned integer for the number of columns}
  @begin{short}
    Accessor of the @slot[gtk:table]{n-columns} slot of the @class{gtk:table}
    class.
  @end{short}
  The number of columns in the table.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-n-columns} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the @class{gtk:grid}
    widget which does not expose the number of columns and rows.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}")

;;; --- gtk:table-n-rows -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-rows" 'table) t)
 "The @code{n-rows} property of type @code{:uint} (Read / Write) @br{}
  The number of rows in the table. @br{}
  Allowed values: [1,65535] @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'table-n-rows)
      "Accessor"
      (documentation 'table-n-rows 'function)
 "@version{2025-07-06}
  @syntax{(gtk:table-n-rows object) => n-rows}
  @syntax{(setf (gtk:table-n-rows object) n-rows)}
  @argument[object]{a @class{gtk:table} widget}
  @argument[n-rows]{an unsigned integer for the number of rows}
  @begin{short}
    Accessor of the @slot[gtk:table]{n-rows} slot of the @class{gtk:table}
    class.
  @end{short}
  The number of rows in the table.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-n-rows} function has been deprecated since version 3.4
    and should not be used in newly written code. Use the @class{gtk:grid}
    widget which does not expose the number of columns and rows.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}")

;;; --- gtk:table-row-spacing --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-spacing" 'table) t)
 "The @code{row-spacing} property of type @code{:uint}  (Read / Write) @br{}
  The amount of space between two consecutive rows. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'table-row-spacing)
      "Accessor"
      (documentation 'table-row-spacing 'function)
 "@version{2025-07-06}
  @syntax{(gtk:table-row-spacing object) => spacing}
  @syntax{(setf (gtk:table-row-spacing object) spacing)}
  @argument[table]{a @class{gtk:table} widget}
  @argument[spacing]{an unsigned integer for the number of pixels of space to
    place between every row in the table}
  @begin{short}
    Accessor of the @slot[gtk:table]{row-spacing} slot of the
    @class{gtk:table} class.
  @end{short}
  The @fun{gtk:table-row-spacing} function gets the default row spacing for the
  table. The @fun{gtk:table-row-spacing} function sets the row spacing. This is
  the spacing that will be used for newly added rows.
  @begin[Lisp implementation]{dictionary}
    The C library has the @code{gtk_table_get_default_row_spacing ()} and
    @code{gtk_table_set_row_spacings ()} functions, which correspond to the
    @fun{gtk:table-row-spacing} function. These C functions are not implemented
    in the Lisp library.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-row-spacing} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the @class{gtk:grid}
    widget with the @fun{gtk:grid-row-spacing} function.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-function{gtk:grid-row-spacing}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk:table-child-bottom-attach ------------------------------------------

(define-child-property table-child-bottom-attach "bottom-attach" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-bottom-attach)
      "Accessor"
      (documentation 'table-child-bottom-attach 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-bottom-attach container child) => attach}
  @syntax{(setf (gtk:table-child-bottom-attach container child) attach)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[attach]{an unsigned integer for the row number to attach the
    bottom of the child widget to}
  @begin{short}
    Accessor of the @prop[gtk:table]{bottom-attach} child property of the
    @class{gtk:table} class.
  @end{short}
  The row number to attach the bottom of the child widget to.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-bottom-attach} function has been deprecated since
    version 3.4. and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}")

;;; --- gtk:table-child-left-attach --------------------------------------------

(define-child-property table-child-left-attach "left-attach" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-left-attach)
      "Accessor"
      (documentation 'table-child-left-attach 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-left-attach container child) => attach}
  @syntax{(setf (gtk:table-child-left-attach container child) attach)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[attach]{an unsigned integer for the column number to attach the
    left side of the child widget to}
  @begin{short}
    Accessor of the @prop[gtk:table]{left-attach} child property of the
    @class{gtk:table} class.
  @end{short}
  The column number to attach the left side of the child widget to.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-left-attach} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}")

;;; --- gtk:table-child-right-attach -------------------------------------------

(define-child-property table-child-right-attach "right-attach" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-right-attach)
      "Accessor"
      (documentation 'table-child-right-attach 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-right-attach container child) => attach}
  @syntax{(setf (gtk:table-child-right-attach container child) attach)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[attach]{an unsigned integer for the column number to attach the
    right side of the child widget to}
  @begin{short}
    Accessor of the @prop[gtk:table]{right-attach} child property of the
    @class{gtk:table} class.
  @end{short}
  The column number to attach the right side of the child widget to.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-right-attach} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}")

;;; --- gtk:table-child-top-attach ---------------------------------------------

(define-child-property table-child-top-attach "top-attach" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-top-attach)
      "Accessor"
      (documentation 'table-child-top-attach 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-top-attach container child) => attach}
  @syntax{(setf (gtk:table-child-top-attach container child) attach)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[attach]{an unsigned integer for the row number to attach the top
    of the child widget to}
  @begin{short}
    Accessor of the @prop[gtk:table]{top-attach} child property of the
    @class{gtk:table} class.
  @end{short}
  The row number to attach the top of the child widget to.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-top-attach} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}")

;;; --- gtk:table-child-x-options ----------------------------------------------

(define-child-property table-child-x-options
                       "x-options" "GtkAttachOptions" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-x-options)
      "Accessor"
      (documentation 'table-child-x-options 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-x-options container child) => options}
  @syntax{(setf (gtk:table-child-x-options container child) options)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[options]{a @sym{gtk:attach-options} value}
  @begin{short}
    Accessor of the @prop[gtk:table]{x-options} child property of the
    @class{gtk:table} class.
  @end{short}
  Options specifying the horizontal behaviour of the child widget.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-x-options} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}
  @see-symbol{gtk:attach-options}")

;;; --- gtk:table-child-y-options ----------------------------------------------

(define-child-property table-child-y-options
                       "y-options" "GtkAttachOptions" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-y-options)
      "Accessor"
      (documentation 'table-child-y-options 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-y-options container child) => options}
  @syntax{(setf (gtk:table-child-y-options container child) options)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[options]{a @sym{gtk:attach-options} value}
  @begin{short}
    Accessor of the @prop[gtk:table]{y-options} child property of the
    @class{gtk:table} class.
  @end{short}
  Options specifying the vertical behaviour of the child widget.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-y-options} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}
  @see-symbol{gtk:attach-options}")

;;; --- gtk:table-child-x-padding ----------------------------------------------

(define-child-property table-child-x-padding "x-padding" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-x-padding)
      "Accessor"
      (documentation 'table-child-x-padding 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-x-padding container child) => padding}
  @syntax{(setf (gtk:table-child-x-padding container child) padding)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[padding]{an unsigned integer for the space to put between the
    child widget and its neighbors}
  @begin{short}
    Accessor of the @prop[gtk:table]{x-padding} child property of the
    @class{gtk:table} class.
  @end{short}
  Extra space to put between the child widget and its left and right neighbors,
  in pixels.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-x-padding} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}")

;;; --- gtk:table-child-y-padding ----------------------------------------------

(define-child-property table-child-y-padding "y-padding" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'table-child-y-padding)
      "Accessor"
      (documentation 'table-child-y-padding 'function)
 "@version{2025-07-11}
  @syntax{(gtk:table-child-y-padding container child) => padding}
  @syntax{(setf (gtk:table-child-y-padding container child) padding)}
  @argument[container]{a @class{gtk:table} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[padding]{an unsigned integer for the space to put between the
    child widget and its neighbors}
  @begin{short}
    Accessor of the @prop[gtk:table]{y-padding} child property of the
    @class{gtk:table} class.
  @end{short}
  Extra space to put between the child widget and its upper and lower
  neighbors, in pixels.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-child-y-padding} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @class{gtk:grid} widget instead.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:widget}
  @see-class{gtk:grid}")

;;; ----------------------------------------------------------------------------
;;; gtk_table_new
;;; ----------------------------------------------------------------------------

(declaim (inline table-new))

(defun table-new (rows columns &optional homogeneous)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[rows]{an unsigned integer for the number of rows the new table
    should have}
  @argument[columns]{an unsigned integer for the number of columns the new
    table should have}
  @argument[homogeneous]{if set to @em{true}, all table cells are resized to
    the size of the cell containing the largest widget, the default is
    @em{false}}
  @return{The the newly created @class{gtk:table} widget.}
  @begin{short}
    Used to create a new table.
  @end{short}
  An initial size must be given by specifying how many rows and columns the
  table should have, although this can be changed later with the
  @fun{gtk:table-resize} function. The @arg{rows} and @arg{columns} arguments
  must both be in the range 1 ... 65535. For historical reasons, 0 is accepted
  as well and is silently interpreted as 1.
  @begin[Warning]{dictionary}
    The @fun{gtk:table-new} function has been deprecated since version 3.4 and
    should not be used in newly written code. Use the @class{gtk:grid} widget
    with the @fun{gtk:grid-new} function.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-function{gtk:table-resize}
  @see-function{gtk:grid-new}"
  (make-instance 'table
                 :n-rows rows
                 :n-columns columns
                 :homogeneous homogeneous))

(export 'table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_table_resize
;;; ----------------------------------------------------------------------------

(declaim (inline table-resize))

(defun table-resize (table rows columns)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[table]{a @class{gtk:table} widget you wish to change the size of}
  @argument[rows]{an unsigned integer for the new number of rows}
  @argument[columns]{an unsigned integer for the new number of columns}
  @begin{short}
    If you need to change the size of the table after it has been created, this
    function allows you to do so.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-resize} function has been deprecated since version 3.4
    and should not be used in newly written code. Use the @class{gtk:grid}
    widget which resizes automatically.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}"
  (setf (table-n-rows table) rows
        (table-n-columns table) columns)
  nil)

(export 'table-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_size
;;; ----------------------------------------------------------------------------

(defun table-size (table)
 #+liber-documentation
 "@version{2024-6-27}
  @argument[table]{a @class{gtk:table} widget}
  @begin{return}
    @arg{n-rows} -- an unsigned integer with the number of rows @br{}
    @arg{n-columns} -- an unsigned integer with the number of columns
  @end{return}
  @short{Gets the number of rows and columns in the table.}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-size} function has been deprecated since version 3.4
    and should not be used in newly written code. Use the @class{gtk:grid}
    widget which does not expose the number of columns and rows.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}"
  (values (table-n-rows table)
          (table-n-columns table)))

(export 'table-size)

;;; ----------------------------------------------------------------------------
;;; gtk_table_attach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_table_attach" %table-attach) :void
  (table (g:object table))
  (child (g:object widget))
  (left :uint)
  (right :uint)
  (top :uint)
  (bottom :uint)
  (x-options attach-options)
  (y-options attach-options)
  (x-padding :uint)
  (y-padding :uint))

(defun table-attach (table child left right top bottom
                                 &key (xoptions '(:expand :fill))
                                      (yoptions '(:expand :fill))
                                      (xpadding 0)
                                      (ypadding 0))
 #+liber-documentation
 "@version{2025-07-06}
  @argument[table]{a @class{gtk:table} widget to add a new widget to}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[left]{an unsigned integer for the column number to attach the left
    side of a child widget to}
  @argument[right]{an unsigned integer for the column number to attach the
    right side of a child widget to}
  @argument[top]{an unsigned ineger for the row number to attach the top of a
    child widget to}
  @argument[bottom]{an unsigned integer for the row number to attach the bottom
    of a child widget to}
  @argument[xoptions]{a @sym{gtk:attach-options} value used to specify the
    properties of the child widget when the table is resized}
  @argument[yoptions]{same as @arg{xoptions}, except this field determines
    behaviour of vertical resizing}
  @argument[xpadding]{an unsigned integer specifying the padding on the left
    and right of the child widget being added to the table}
  @argument[ypadding]{an unsigned integer for the amount of padding above and
    below the child widget}
  @begin{short}
    Adds a child widget to a table.
  @end{short}
  The number of cells that a child widget will occupy is specified by the
  arguments @arg{left}, @arg{right}, @arg{top} and @arg{bottom}. These each
  represent the leftmost, rightmost, uppermost and lowest column and row
  numbers of the table. Columns and rows are indexed from zero.

  The @code{xoptions} and @code{yoptions} keyword arguments have the default
  value @code{'(:expand :fill)}. The @code{xpadding} and @code{ypadding} keyword
  arguments have the default value 0.
  @begin[Examples]{dictionary}
    To make a button occupy the lower right cell of a 2 x 2 table, use
    @begin{pre}
(gtk:table-attach table button 1 2 1 2)
    @end{pre}
    If you want to make the button span the entire bottom row, use
    @begin{pre}
(gtk:table-attach table button 0 2 1 2)
    @end{pre}
  @end{dictionary}
  @begin[Lisp implementation]{dictionary}
    The C library has the @code{gtk_table_attach_default ()} function. This
    function is included in the Lisp library via keyword arguments with default
    values for the @fun{gtk:table-attach} function.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-attach} function has been deprecated since version 3.4
    and should not be used in newly written code. Use the @class{gtk:grid}
    widget with the @fun{gtk:grid-attach} function. Note that the attach
   arguments differ between those two functions.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-symbol{gtk:attach-options}
  @see-function{gtk:grid-attach}"
  (%table-attach table child
                       left right top bottom
                       xoptions yoptions
                       xpadding ypadding))

(export 'table-attach)

;;; ----------------------------------------------------------------------------
;;; gtk_table_attach_defaults                              not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_row_spacing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_table_set_row_spacing" table-set-row-spacing) :void
 #+liber-documentation
 "@version{2025-07-06}
  @argument[table]{a @class{gtk:table} widget containing the row whose
    properties you wish to change}
  @argument[row]{an unsigned integer for the row number whose spacing will be
    changed}
  @argument[spacing]{an unsigned integer for the number of pixels that the
    spacing should take up}
  @begin{short}
    Changes the space between a given table row and the subsequent row.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-set-row-spacing} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-top} and @fun{gtk:widget-margin-bottom} functions on
    the widgets contained in the row if you need this functionality.
    The @class{gtk:grid} widget does not support per-row spacing.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-function{gtk:widget-margin-top}
  @see-function{gtk:widget-margin-bottom}"
  (table (g:object table))
  (row :uint)
  (spacing :uint))

(export 'table-set-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_set_col_spacing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_table_set_col_spacing" table-set-col-spacing) :void
 #+liber-documentation
 "@version{2025-07-06}
  @argument[table]{a @class{gtk:table} widget}
  @argument[column]{an unsigned integer for the column whose spacing should be
    changed}
  @argument[spacing]{an unsigned integer for the number of pixels that the
    spacing should take up}
  @begin{short}
    Alters the amount of space between a given table column and the following
    column.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-set-col-spacing} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-start} and @fun{gtk:widget-margin-end} functions on
    the widgets contained in the row if you need this functionality.
    The @class{gtk:grid} widget does not support per-column spacing.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}
  @see-function{gtk:widget-margin-start}
  @see-function{gtk:widget-margin-end}"
  (table (g:object table))
  (column :uint)
  (spacing :uint))

(export 'table-set-col-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_row_spacing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_table_get_row_spacing" table-get-row-spacing) :uint
 #+liber-documentation
 "@version{2025-07-17}
  @argument[table]{a @class{gtk:table} widget}
  @argument[row]{an unsigned integer for a row in the table, 0 indicates the
    first row}
  @return{The unsigned integer for the row spacing.}
  @begin{short}
    Gets the amount of space between @arg{row}, and @arg{row + 1}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-get-row-spacing} function has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk:grid} widget does not offer a replacement for this functionality.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}"
  (table (g:object table))
  (row :uint))

(export 'table-get-row-spacing)

;;; ----------------------------------------------------------------------------
;;; gtk_table_get_col_spacing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_table_get_col_spacing" table-get-col-spacing) :uint
 #+liber-documentation
 "@version{2025-07-17}
  @argument[table]{a @class{gtk:table} widget}
  @argument[column]{an unsigned integer for a column in the table, 0 indicates
    the first column}
  @return{The unsigned integer for the column spacing.}
  @begin{short}
    Gets the amount of space between @arg{column}, and @arg{column + 1}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:table-get-col-spacing} function has been deprecated since
    version 3.4 and should not be used in newly written code. The
    @class{gtk:grid} widget does not offer a replacement for this functionality.
  @end{dictionary}
  @see-class{gtk:table}
  @see-class{gtk:grid}"
  (table (g:object table))
  (column :uint))

(export 'table-get-col-spacing)

;;; --- End of file gtk3.table.lisp --------------------------------------------
