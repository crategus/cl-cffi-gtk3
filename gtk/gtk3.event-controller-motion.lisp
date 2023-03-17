;;; ----------------------------------------------------------------------------
;;; gtk3.event-controller-motion.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; struct GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventControllerMotion" event-controller-motion
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_motion_get_type")
  nil)

#+liber-documentation
(setf (documentation 'event-controller-motion 'type)
 "@version{#2023-3-1}
  @begin{short}
    The @sym{gtk:event-controller-motion} object is an event controller meant
    for situations where you need to track the position of the pointer.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"enter\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Signals that the pointer has entered the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object that
          received the signal.}
        @entry[x]{a double float with the x coordinate}
        @entry[y]{a double float with the y coordinate}
      @end{table}
    @subheading{The \"leave\" signal}
      @begin{pre}
lambda (controller)    :run-first
      @end{pre}
      Signals that pointer has left the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object that
          received the signal.}
      @end{table}
    @subheading{The \"motion\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Emitted when the pointer moves inside the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object that
          received the signal.}
        @entry[x]{a double float with the x coordinate}
        @entry[y]{a double float with the y coordinate}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:event-controller-motion-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_motion_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-motion-new))

(defun event-controller-motion-new (widget)
 #+liber-documentation
 "@version{#2023-3-1}
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
