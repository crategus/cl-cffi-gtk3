;;; ----------------------------------------------------------------------------
;;; gtk3.gesture-stylus.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GtkGestureStylus
;;;
;;;     Gesture for stylus input
;;;
;;; Types and Values
;;;
;;;     GtkGestureStylus
;;;
;;; Functions
;;;
;;;     gtk_gesture_stylus_new
;;;     gtk_gesture_stylus_get_axis
;;;     gtk_gesture_stylus_get_axes                         not implemented
;;;     gtk_gesture_stylus_get_device_tool
;;;
;;; Signals
;;;
;;;     down
;;;     motion
;;;     proximity
;;;     up
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureStylus
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGestureStylus
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureStylus" gesture-stylus
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_stylus_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-stylus 'type)
 "@version{#2025-07-15}
  @begin{short}
    The @class{gtk:gesture-stylus} object is a @class{gtk:gesture}
    implementation specific to stylus input.
  @end{short}
  The provided signals just provide the basic information.
  @begin[Signal Details]{dictionary}
    @begin[gesture-stylus::down]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
      Emitted when the stylus touches the device.
    @end{signal}
    @begin[gesture-stylus::motion]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
      Emitted when the stylus moves while touching the device.
    @end{signal}
    @begin[gesture-stylus]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
      Emitted when the stylus is in proximity of the device.
    @end{signal}
    @begin[gesture-stylus::up]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
    @end{signal}
  @end{dictionary}
  @see-class{gtk:gesture}
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_new
;;; ----------------------------------------------------------------------------

(defun gesture-stylus-new (widget)
 #+liber-documentation
 "@version{#2025-01-15}
  @argument[widget]{a @class{gtk:gesture-stylus} object}
  @return{The newly created @class{gtk:gesture-stylus} object.}
  @begin{short}
    Creates a new stylus gesture.
  @end{short}
  @see-class{gtk:gesture-stylus}"
  (make-instance 'gesture-stylus
                 :widget widget))

(export 'gesture-stylus-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_stylus_get_axis" %gesture-stylus-axis) :boolean
  (gesture (g:object gesture-stylus))
  (axis gdk:axis-use)
  (value (:pointer :double)))

(defun gesture-stylus-axis (gesture axis)
 #+liber-documentation
 "@version{#2025-07-16}
  @argument[gesture]{a @class{gtk:gesture-stylus} object}
  @argument[axis]{a @sym{gdk:axis-use} value for the requested device axis}
  @return{The double float for the current value for the axis.}
  @begin{short}
    Returns the current value for the requested axis.
  @end{short}
  This function must be called from either the @sig[gtk:gesture-stylus]{down},
  @sig[gtk:gesture-stylus]{motion}, @sig[gtk:gesture-stylus]{up} or
  @code{\"proximity\"} signals.
  @see-class{gtk:gesture-stylus}
  @see-symbol{gdk:axis-use}"
  (cffi:with-foreign-object (value :double)
    (when (%gesture-stylus-axis gesture axis value)
      (cffi:mem-ref value :double))))

(export 'gesture-stylus-axis)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axes
;;;
;;; Returns the current values for the requested axes .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_device_tool
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_stylus_get_device_tool" gesture-stylus-device-tool)
    (g:object gdk:device-tool)
 #+liber-documentation
 "@version{#2025-07-16}
  @argument[gesture]{a @class{gtk:gesture-stylus} object}
  @return{The current @class{gdk:device-tool} object.}
  @begin{short}
    Returns the device tool currently driving input through this gesture.
  @end{short}
  This function must be called from either the @sig[gtk:gesture-stylus]{down},
  @sig[gtk:gesture-stylus]{motion}, @sig[gtk:gesture-stylus]{up} or
  @code{\"proximity\"} signal handlers.
  @see-class{gtk:gesture-stylus}
  @see-class{gdk:device-tool}"
  (gesture (g:object gesture-stylus)))

(export 'gesture-stylus-device-tool)

;;; --- End of file gtk3.gesture-stylus.lisp -----------------------------------
