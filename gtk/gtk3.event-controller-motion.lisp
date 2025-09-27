;;; ----------------------------------------------------------------------------
;;; gtk3.event-controller-motion.lisp
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
;;; GtkEventControllerMotion
;;;
;;;     Event controller for motion events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerMotion
;;;
;;; Functions
;;;
;;;     gtk_event_controller_motion_new
;;;
;;; Signals
;;;
;;;     enter
;;;     leave
;;;     motion
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEventControllerMotion" event-controller-motion
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_motion_get_type")
  nil)

#+liber-documentation
(setf (documentation 'event-controller-motion 'type)
 "@version{#2025-07-15}
  @begin{short}
    The @class{gtk:event-controller-motion} object is an event controller meant
    for situations where you need to track the position of the pointer.
  @end{short}
  @begin[Signal Details]{dictionary}
    @begin[event-controller-motion::enter]{signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-motion} object that
          received the signal.}
        @entry[x]{The double float for the x coordinate}
        @entry[y]{The double float for the y coordinate}
      @end{simple-table}
      Signals that the pointer has entered the widget.
    @end{signal}
    @begin[event-controller-motion::leave]{signal}
      @begin{pre}
lambda (controller)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-motion} object that
          received the signal.}
      @end{simple-table}
      Signals that pointer has left the widget.
    @end{signal}
    @begin[event-controller-motion::motion]{signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-motion} object that
          received the signal.}
        @entry[x]{The double float for the x coordinate}
        @entry[y]{The double float for the y coordinate}
      @end{simple-table}
      Emitted when the pointer moves inside the widget.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:event-controller-motion-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_motion_new
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-motion-new))

(defun event-controller-motion-new (widget)
 #+liber-documentation
 "@version{#2023-03-01}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The new @class{gtk:event-controller-motion} object.}
  @begin{short}
    Creates a new event controller that will handle motion events for the given
    @arg{widget}.
  @end{short}
  @see-class{gtk:event-controller-motion}
  @see-class{gtk:widget}"
  (make-instance 'event-controller-motion
                 :widget widget))

(export 'event-controller-motion-new)

;;; --- End of file gtk3.event-controller-motion.lisp --------------------------
