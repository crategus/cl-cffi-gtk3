;;; ----------------------------------------------------------------------------
;;; gtk3.viewport.lisp
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
;;; GtkViewport
;;;
;;;     An adapter which makes widgets scrollable
;;;
;;; Types and Values
;;;
;;;     GtkViewport
;;;     GtkViewportClass
;;;
;;; Functions
;;;
;;;     gtk_viewport_new
;;;     gtk_viewport_get_hadjustment                       deprecated
;;;     gtk_viewport_get_vadjustment                       deprecated
;;;     gtk_viewport_set_hadjustment                       deprecated
;;;     gtk_viewport_set_vadjustment                       deprecated
;;;     gtk_viewport_set_shadow_type                       Accessor
;;;     gtk_viewport_get_shadow_type                       Accessor
;;;     gtk_viewport_get_bin_window
;;;     gtk_viewport_get_view_window
;;;
;;; Properties
;;;
;;;     shadow-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkViewport
;;;
;;; Implemented Interfaces
;;;
;;;     GtkViewport implements AtkImplementorIface, GtkBuildable and
;;;     GtkScrollable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkViewport
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkViewport" viewport
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_viewport_get_type")
  ((shadow-type
    viewport-shadow-type
    "shadow-type" "GtkShadowType" t t)))

#+liber-documentation
(setf (documentation 'viewport 'type)
 "@version{#2023-3-29}
  @begin{short}
    The @sym{gtk:viewport} widget acts as an adaptor class, implementing
    scrollability for child widgets that lack their own scrolling capabilities.
  @end{short}
  Use the @sym{gtk:viewport} widget to scroll child widgets such as the widgets
  @class{gtk:grid}, @class{gtk:box}, and so on.

  If a widget has native scrolling abilities, such as the @class{gtk:text-view},
  @class{gtk:tree-view} or @class{gtk:icon-view} widgets, it can be added to a
  @class{gtk:scrolled-window} widget with the @fun{gtk:container-add} function.
  If a widget does not, you must first add the widget to a @sym{gtk:viewport}
  widget, then add the viewport to the scrolled window. The
  @fun{gtk:container-add} function does this automatically if a child that does
  not implement the @class{gtk:scrollable} interface is added to a
  @class{gtk:scrolled-window} widget, so you can ignore the presence of the
  viewport.

  The @sym{gtk:viewport} widget will start scrolling content only if allocated
  less than the child widget's minimum size in a given orientation.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:viewport} widget has a single CSS node with name
    @code{viewport}.
  @end{dictionary}
  @see-constructor{gtk:viewport-new}
  @see-slot{gtk:viewport-shadow-type}
  @see-class{gtk:scrolled-window}
  @see-class{gtk:scrollable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- viewport-shadow-type ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shadow-type" 'viewport) t)
 "The @code{shadow-type} property of type @symbol{gtk:shadow-type}
  (Read / Write) @br{}
  Determines how the shadowed box around the viewport is drawn. @br{}
  Default value: @code{:in}")

#+liber-documentation
(setf (liber:alias-for-function 'viewport-shadow-type)
      "Accessor"
      (documentation 'viewport-shadow-type 'function)
 "@version{#2023-3-29}
  @syntax[]{(gtk:viewport-shadow-type object) => type}
  @syntax[]{(setf (gtk:viewport-shadow-type object) type)}
  @argument[viewport]{a @class{gtk:viewport} widget}
  @argument[type]{a @symbol{gtk:shadow-type} value for the shadow type}
  @begin{short}
    Accessor of the @slot[gtk:viewport]{shadow-type} slot of the
    @class{gtk:viewport} class.
  @end{short}
  The @sym{gtk:viewport-shadow-type} function gets the shadow type of the
  viewport. The @sym{(setf gtk:viewport-shadow-type)} function sets the shadow
  type.
  @see-class{gtk:viewport}
  @see-symbol{gtk:shadow-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline viewport-new))

(defun viewport-new (&optional (hadjustment nil) (vadjustment nil))
 #+liber-documentation
 "@version{#2023-3-29}
  @argument[hadjustment]{horizontal @class{gtk:adjustment} object}
  @argument[vadjustment]{vertical @class{gtk:adjustment} object}
  @return{A new @class{gtk:viewport} widget.}
  @begin{short}
    Creates a new viewport with the given adjustments.
  @end{short}
  @see-class{gtk:viewport}
  @see-class{gtk:adjustment}"
  (make-instance 'viewport
                 :hadjustment hadjustment
                 :vadjustment vadjustment))

(export 'viewport-new)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_viewport_get_hadjustment (GtkViewport *viewport);
;;;
;;; Warning
;;;
;;; gtk_viewport_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gtk_scrollable_get_hadjustment()
;;;
;;; Returns the horizontal adjustment of the viewport.
;;;
;;; viewport :
;;;     a GtkViewport.
;;;
;;; Returns :
;;;     the horizontal adjustment of viewport
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_viewport_get_vadjustment (GtkViewport *viewport);
;;;
;;; Warning
;;;
;;; gtk_viewport_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gtk_scrollable_get_vadjustment()
;;;
;;; Returns the vertical adjustment of the viewport.
;;;
;;; viewport :
;;;     a GtkViewport.
;;;
;;; Returns :
;;;     the vertical adjustment of viewport
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_hadjustment ()
;;;
;;; void gtk_viewport_set_hadjustment (GtkViewport *viewport,
;;;                                    GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_viewport_set_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gtk_scrollable_set_hadjustment()
;;;
;;; Sets the horizontal adjustment of the viewport.
;;;
;;; viewport :
;;;     a GtkViewport.
;;;
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_set_vadjustment ()
;;;
;;; void gtk_viewport_set_vadjustment (GtkViewport *viewport,
;;;                                    GtkAdjustment *adjustment);
;;;
;;; Warning
;;;
;;; gtk_viewport_set_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gtk_scrollable_set_vadjustment()
;;;
;;; Sets the vertical adjustment of the viewport.
;;;
;;; viewport :
;;;     a GtkViewport.
;;;
;;; adjustment :
;;;     a GtkAdjustment
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_bin_window () -> viewport-bin-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_viewport_get_bin_window" viewport-bin-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{#2023-3-29}
  @argument[viewport]{a @class{gtk:viewport} widget}
  @return{A @class{gdk:window} object.}
  @short{Gets the bin window of the viewport.}
  @see-class{gtk:viewport}
  @see-class{gdk:window}"
  (viewport (g:object viewport)))

(export 'viewport-bin-window)

;;; ----------------------------------------------------------------------------
;;; gtk_viewport_get_view_window () -> viewport-view-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_viewport_get_view_window" viewport-view-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{#2023-3-29}
  @argument[viewport]{a @class{gtk:viewport} widget}
  @return{A @class{gdk:window} object.}
  @short{Gets the view window of the viewport.}
  @see-class{gtk:viewport}
  @see-class{gdk:window}"
  (viewport (g:object viewport)))

(export 'viewport-view-window)

;;; --- End of file gtk3.viewport.lisp -----------------------------------------
