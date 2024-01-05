;;; ----------------------------------------------------------------------------
;;; gtk3.drawing-area.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkDrawingArea
;;;
;;;     A widget for custom user interface elements
;;;
;;; Values and Types
;;;
;;;     GtkDrawingArea
;;;
;;; Functions
;;;
;;;     gtk_drawing_area_new
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkDrawingArea
;;;
;;; Implemented Interfaces
;;;
;;;     GtkDrawingArea implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkDrawingArea
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkDrawingArea" drawing-area
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_drawing_area_get_type")
  nil)

#+liber-documentation
(setf (documentation 'drawing-area 'type)
 "@version{2023-12-30}
  @begin{short}
    The @class{gtk:drawing-area} widget is used for creating custom user
    interface elements. It is essentially a blank widget. You can draw on it.
  @end{short}
  After creating a drawing area, the application may want to connect to:
  @begin{itemize}
    @begin{item}
      Mouse and button press signals to respond to input from the user. Use the
      @fun{gtk:widget-add-events} function to enable events you wish to receive.
    @end{item}
    @begin{item}
      The @code{\"realize\"} signal to take any necessary actions when the
      widget is instantiated on a particular display. Create GDK resources in
      response to this signal.
    @end{item}
    @begin{item}
      The @code{\"configure-event\"} signal to take any necessary actions when
      the widget changes size.
    @end{item}
    @begin{item}
      The @code{\"draw\"} signal to handle redrawing the contents of the widget.
    @end{item}
  @end{itemize}
  Draw signals are normally delivered when a drawing area first comes onscreen,
  or when it is covered by another window and then uncovered. You can also force
  an expose event by adding to the \"damage region\" of the drawing area's
  window. The @fun{gtk:widget-queue-draw-area} and
  @fun{gdk:window-invalidate-rect} functions are equally good ways to do this.
  You will then get a draw signal for the invalid region.

  To receive mouse events on a drawing area, you will need to enable them with
  the @fun{gtk:widget-add-events} function. To receive keyboard events, you
  will need to set the @slot[gtk:widget]{can-focus} property on the drawing
  area, and you should probably draw some user visible indication that the
  drawing area is focused. Use the @fun{gtk:widget-has-focus} function in your
  expose event handler to decide whether to draw the focus indicator. See the
  @fun{gtk:render-focus} function for one way to draw focus.
  @begin[Example]{dictionary}
    The following example demonstrates using a drawing area to display a
    circle in the normal widget foreground color.

    Note that GDK automatically clears the exposed area before sending the
    expose event, and that drawing is implicitly clipped to the exposed area.
    If you want to have a theme-provided background, you need to call the
    @fun{gtk:render-background} function in your @code{\"draw\"} signal handler.
    @begin{pre}
(defun example-drawing-area ()
  (gtk:within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title \"Example Drawing Area\"
                                 :default-width 400
                                 :default-height 300))
          ;; Create the drawing area
          (area (make-instance 'gtk:drawing-area)))
      ;; Signal handler for the drawing area
      (g:signal-connect area \"draw\"
          (lambda (widget cr)
            (let* ((cr (pointer cr))
                   (width (gtk:widget-allocated-width widget))
                   (height (gtk:widget-allocated-height widget))
                   (context (gtk:widget-style-context widget))
                   (color (gtk:style-context-color context :focused)))
                ;; Set the color from the style context of the widget
                (gdk:cairo-set-source-rgba cr color)
                ;; Draw and fill a circle on the drawing area
                (cairo-arc cr
                           (/ width 2.0)
                           (/ height 2.0)
                           (- (/ (min width height) 2.0) 12)
                           0.0
                           (* 2.0 pi))
                (cairo-fill cr)
                ;; Destroy the Cairo context
                (cairo-destroy cr))))
      ;; Signal handler for the window to handle the signal \"destroy\"
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Show the window
      (gtk:container-add window area)
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @see-function{gtk:widget-add-events}
  @see-function{gtk:widget-queue-draw-area}
  @see-function{gdk:window-invalidate-rect}
  @see-function{gtk:widget-has-focus}
  @see-function{gtk:render-focus}
  @see-function{gtk:render-background}")

;;; ----------------------------------------------------------------------------
;;; gtk_drawing_area_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline drawing-area-new))

(defun drawing-area-new ()
 #+liber-documentation
 "@version{#2023-3-17}
  @return{The new @class{gtk:drawing-area} widget.}
  @begin{short}
    Creates a new drawing area.
  @end{short}
  @see-class{gtk:drawing-area}"
  (make-instance 'drawing-area))

(export 'drawing-area-new)

;;; --- End of file gtk3.drawing-area.lisp -------------------------------------
