;;; ----------------------------------------------------------------------------
;;; gtk3.event-controller-motion.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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

  This object was added in 3.24.
  @begin[Signal Details]{dictionary}
    @subheading{The \"enter\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Signals that the pointer has entered the widget. Since 3.24
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
      Signals that pointer has left the widget. Since 3.24
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object that
          received the signal.}
      @end{table}
    @subheading{The \"motion\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Emitted when the pointer moves inside the widget. Since 3.24
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

  Since 3.24
  @see-class{gtk:event-controller-motion}
  @see-class{gtk:widget}"
  (make-instance 'event-controller-motion
                 :widget widget))

(export 'event-controller-motion-new)

;;; --- End of file gtk3.event-controller-motion.lisp --------------------------
