;;; ----------------------------------------------------------------------------
;;; gtk3.layout.lisp
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
;;; GtkLayout
;;;
;;;     Infinite scrollable area containing child widgets and/or custom drawing
;;;
;;; Types and Values
;;;
;;;     GtkLayout
;;;
;;; Functions
;;;
;;;     gtk_layout_new
;;;     gtk_layout_put
;;;     gtk_layout_move
;;;     gtk_layout_set_size
;;;     gtk_layout_get_size
;;;     gtk_layout_get_hadjustment                         deprecated
;;;     gtk_layout_get_vadjustment                         deprecated
;;;     gtk_layout_set_hadjustment                         deprecated
;;;     gtk_layout_set_vadjustment                         deprecated
;;;     gtk_layout_get_bin_window
;;;
;;; Properties
;;;
;;;     height
;;;     width
;;;
;;; Child Properties
;;;
;;;     x
;;;     y
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkLayout
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLayout implements AtkImplementorIface, GtkBuildable and GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkLayout
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkLayout" layout
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_layout_get_type")
  ((height
    layout-height
    "height" "guint" t t)
   (width
    layout-width
    "width" "guint" t t)))

#+liber-documentation
(setf (documentation 'layout 'type)
 "@version{#2023-3-20}
  @begin{short}
    The @sym{gtk:layout} widget is similar to the @class{gtk:drawing-area}
    widget in that it is a \"blank slate\" and does not do anything but paint
    a blank background by default.
  @end{short}
  It is different in that it supports scrolling natively, you can add it to a
  @class{gtk:scrolled-window} widget, and it can contain child widgets, since
  it is a @class{gtk:container} widget. However if you are just going to draw,
  a @class{gtk:drawing-area} widget is a better choice since it has lower
  overhead.

  When handling expose events on a @sym{gtk:layout} widget, you must draw to
  the @class{gdk:window} object returned by the @fun{gtk:layout-bin-window}
  function, rather than to the one returned by the @fun{gtk:widget-window}
  function as you would for a drawing area.
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[x]{entry}
        The @code{x} child property of type @code{:int} (Read / Write) @br{}
        x position of the child widget. @br{}
        Default value: 0
      @end{entry}
      @begin[y]{entry}
        The @code{y} child property of type @code{:int} (Read / Write) @br{}
        y position of the child widget. @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:layout-new}
  @see-slot{gtk:layout-height}
  @see-slot{gtk:layout-width}
  @see-class{gtk:drawing-area}
  @see-class{gtk:container}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- layout-height ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'layout) t)
 "The @code{height} property of type @code{:uint} (Read / Write) @br{}
  The height of the layout. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 100")

#+liber-documentation
(setf (liber:alias-for-function 'layout-height)
      "Accessor"
      (documentation 'layout-height 'function)
 "@version{#2023-3-20}
  @syntax{(gtk:layout-height object) => height}
  @syntax{(setf (gtk:layout-height object) height)}
  @argument[object]{a @class{gtk:layout} widget}
  @argument[height]{an unsigned integer with the height of the layout}
  @begin{short}
    Accessor of the @slot[gtk:layout]{height} slot of the @class{gtk:layout}
    class.
  @end{short}

  The @sym{gtk:layout-height} function gets the height of the layout. The
  @sym{(setf gtk:layout-height)} function sets the height.
  @see-class{gtk:layout}")

;;; --- layout-width -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width" 'layout) t)
 "The @code{width} property of type @code{:uint} (Read / Write) @br{}
  The width of the layout. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 100")

#+liber-documentation
(setf (liber:alias-for-function 'layout-width)
      "Accessor"
      (documentation 'layout-width 'function)
 "@version{#2023-3-20}
  @syntax{(gtk:layout-width object) => width}
  @syntax{(setf (gtk:layout-width object) width)}
  @argument[object]{a @class{gtk:layout} widget}
  @argument[width]{an unsigned integer with the width of the layout}
  @begin{short}
    Accessor of the @slot[gtk:layout]{width} slot of the @class{gtk:layout}
    class.
  @end{short}
  The @sym{gtk:layout-width} function gets the width of the layout. The
  @sym{(setf gtk:layout-width)} function sets the width.
  @see-class{gtk:layout}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- layout-child-x ---------------------------------------------------------

(define-child-property layout-child-x "x" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'layout-child-x)
      "Accessor"
      (documentation 'layout-child-x 'function)
 "@version{#2023-3-20}
  @syntax{(gtk:layout-child-x container cild) => x}
  @syntax{(setf (gtk:layout-child-x container child) x)}
  @argument[container]{a @class{gtk:layout} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[x]{an integer with the x position of the child widget}
  @begin{short}
    Accessor of the @code{x} child property of the @class{gtk:layout} class.
  @end{short}
  The x position of the child widget in the layout.
  @see-class{gtk:layout}
  @see-class{gtk:widget}")

;;; --- layout-child-y ---------------------------------------------------------

(define-child-property layout-child-y "y" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'layout-child-y)
      "Accessor"
      (documentation 'layout-child-y 'function)
 "@version{#2023-3-20}
  @syntax{(gtk:layout-child-y container cild) => y}
  @syntax{(setf (gtk:layout-child-y container child) y)}
  @argument[container]{a @class{gtk:layout} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[y]{an integer with the y position of the child widget}
  @begin{short}
    Accessor of the child property @code{y} of the @class{gtk:layout} class.
  @end{short}
  The y position of the child widget in the layout.
  @see-class{gtk:layout}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_layout_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline layout-new))

(defun layout-new (&optional (hadjustment nil) (vadjustment nil))
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[hadjustment]{a horizontal scroll @class{gtk:adjustment} object}
  @argument[vadjustment]{a vertical scroll @class{gtk:adjustment} object}
  @return{A new @class{gtk:layout} widget.}
  @begin{short}
    Creates a new layout.
  @end{short}
  Unless you have a specific adjustment you would like the layout to use for
  scrolling, pass @code{nil} for the @arg{hadjustment} and @arg{vadjustment}
  arguments.
  @begin[Lisp binding]{dictionary}
   In the Lisp binding the adjustments are optional arguments with the
   @code{nil} default value.
  @end{dictionary}
  @see-class{gtk:layout}
  @see-class{gtk:adjustment}"
  (make-instance 'layout
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'layout-new)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_put ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_layout_put" layout-put) :void
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[layout]{a @class{gtk:layout} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[x]{an integer with the x position of the child widget}
  @argument[y]{an integer with the y position of the child widget}
  @begin{short}
    Adds a child widget to the layout, at the given position in pixels.
  @end{short}
  The layout becomes the new parent container of the child widget.
  @see-class{gtk:layout}
  @see-class{gtk:widget}
  @see-function{gtk:layout-move}"
  (layout (g:object layout))
  (widget (g:object widget))
  (x :int)
  (y :int))

(export 'layout-put)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_move ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_layout_move" layout-move) :void
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[layout]{a @class{gtk:layout} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[x]{an integer with the x position to move to}
  @argument[y]{an integer with the y position to move to}
  @begin{short}
    Moves a child widget of the layout to a new position in pixels.
  @end{short}
  @see-class{gtk:layout}
  @see-class{gtk:widget}
  @see-function{gtk:layout-put}"
  (layout (g:object layout))
  (widget (g:object widget))
  (x :int)
  (y :int))

(export 'layout-move)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_size ()
;;; gtk_layout_set_size () -> layout-size
;;; ----------------------------------------------------------------------------

(defun (setf layout-size) (value layout)
  (destructuring-bind (width height) value
    (cffi:foreign-funcall "gtk_layout_set_size"
                          (g:object layout) layout
                          :uint width
                          :uint height
                          :void)
  (values width height)))

(defun layout-size (layout)
 #+liber-documentation
 "@version{#2023-3-20}
  @syntax{(gtk:layout-size layout) => width, height}
  @syntax{(setf (gtk:layout-size layout) '(width height))}
  @argument[layout]{a @class{gtk:layout} widget}
  @argument[width]{an unsigned integer with the width of the entire scrollable
    area}
  @argument[height]{an unsigned integer with the height of the entire scrollable
    area}
  @begin{short}
    Accessor of the width and height of the scrollable area.
  @end{short}
  The @sym{gtk:layout-size} function gets the size in pixels that has been set
  on the layout, and that determines the total extents of the scrollbar of the
  layout area. The @sym{(setf gtk:layout-size)} function sets the size.
  @begin[Lisp binding]{dictionary}
    In the Lisp binding the @fun{gtk:layout-width} and @fun{gtk:layout-height}
    functions get or set the width and height of the scrollable area.
  @end{dictionary}
  @see-class{gtk:layout}
  @see-function{gtk:layout-height}
  @see-function{gtk:layout-width}"
  (values (layout-width layout)
          (layout-height layout)))

(export 'layout-size)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_layout_get_hadjustment (GtkLayout *layout);
;;;
;;; Warning
;;;
;;; gtk_layout_get_hadjustment has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gtk_scrollable_get_hadjustment()
;;;
;;; This function should only be called after the layout has been placed in a
;;; GtkScrolledWindow or otherwise configured for scrolling. It returns the
;;; GtkAdjustment used for communication between the horizontal scrollbar and
;;; layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; Returns :
;;;     horizontal scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_layout_get_vadjustment (GtkLayout *layout);
;;;
;;; Warning
;;;
;;; gtk_layout_get_vadjustment has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gtk_scrollable_get_vadjustment()
;;;
;;; This function should only be called after the layout has been placed in a
;;; GtkScrolledWindow or otherwise configured for scrolling. It returns the
;;; GtkAdjustment used for communication between the vertical scrollbar and
;;; layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; Returns :
;;;     vertical scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_hadjustment ()
;;;
;;; void gtk_layout_set_hadjustment (GtkLayout *layout,
;;;                                  GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_layout_set_hadjustment has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gtk_scrollable_set_hadjustment()
;;;
;;; Sets the horizontal scroll adjustment for the layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; adjustment :
;;;     new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_set_vadjustment ()
;;;
;;; void gtk_layout_set_vadjustment (GtkLayout *layout,
;;;                                  GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_layout_set_vadjustment has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gtk_scrollable_set_vadjustment()
;;;
;;; Sets the vertical scroll adjustment for the layout.
;;;
;;; See GtkScrolledWindow, GtkScrollbar, GtkAdjustment for details.
;;;
;;; layout :
;;;     a GtkLayout
;;;
;;; adjustment :
;;;     new scroll adjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_layout_get_bin_window () -> layout-bin-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_layout_get_bin_window" layout-bin-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[layout]{a @class{gtk:layout} widget}
  @return{A @class{gdk:window} object.}
  @begin{short}
    Retrieve the GDK window of the layout used for drawing operations.
  @end{short}
  @see-class{gtk:layout}
  @see-class{gdk:window}"
  (layout (g:object layout)))

(export 'layout-bin-window)

;;; --- End of file gtk3.layout.lisp -------------------------------------------
