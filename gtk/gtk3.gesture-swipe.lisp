;;; ----------------------------------------------------------------------------
;;; gtk3.gesture-swipe.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
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
;;; GtkGestureSwipe
;;;
;;;     Swipe gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureSwipe
;;;
;;; Functions
;;;
;;;     gtk_gesture_swipe_new
;;;     gtk_gesture_swipe_get_velocity
;;;
;;; Signals
;;;
;;;     swipe
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureSwipe" gesture-swipe
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_swipe_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-swipe 'type)
 "@version{#2025-1-25}
  @begin{short}
    The @class{gtk:gesture-swipe} object is a @class{gtk:gesture} implementation
    able to recognize swipes, after a press/move/.../move/release sequence
    happens, the @code{\"swipe\"} signal will be emitted, providing the velocity
    and directionality of the sequence at the time it was lifted.
  @end{short}

  If the velocity is desired in intermediate points, the
  @fun{gtk:gesture-swipe-velocity} function can be called on, for example, a
  @code{\"update\"} handler.

  All velocities are reported in pixels/sec units.
  @begin[Signal Details]{dictionary}
    @subheading{The \"swipe\" signal}
    @begin{pre}
lambda (gesture xvelocity yvelocity)    :run-last
    @end{pre}
    @begin[code]{table}
      @entry[gesture]{The @class{gtk:gesture-multi-press} object which
        received the signal.}
      @entry[xvelocity]{The double float with the velocity in the x axis,
        in pixels/sec.}
      @entry[yvelocity]{The double float with the velocity in the y axis,
        in pixels/sec.}
    @end{table}
    The signal is emitted when the recognized gesture is finished, velocity
    and direction are a product of previously recorded events.
  @end{dictionary}
  @see-constructor{gtk:gesture-swipe-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-swipe-new))

(defun gesture-swipe-new (widget orientation)
 #+liber-documentation
 "@version{#2025-1-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The newly created @class{gtk:gesture-swipe} object.}
  @begin{short}
    Returns a newly created gesture that recognizes swipes.
  @end{short}
  @see-class{gtk:gesture-swipe}"
  (make-instance 'gesture-swipe
                 :widget widget
                 :orientation orientation))

(export 'gesture-pan-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_get_velocity
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_swipe_get_velocity" %gesture-swipe-velocity)
    :boolean
  (gesture (g:object gesture-swipe))
  (xvelocity (:pointer :double))
  (yvelocity (:pointer :double)))

(defun gesture-swipe-velocity (gesture)
 #+liber-documentation
 "@version{#2025-1-25}
  @argument[gesture]{a @class{gtk:gesture-swipe} object}
  @begin{return}
    @code{xvelocity} -- a double float with the velocity in the x axis,
    in pixels/sec. @br{}
    @code{yvelocity} -- a double float with the velocity in the y axis,
    in pixels/sec.
  @end{return}
  @begin{short}
    If the gesture is recognized, this function returns the recorded velocity,
    as per the last event(s) processed.
  @end{short}
  @see-class{gtk:gesture-swipe}"
  (cffi:with-foreign-objects ((xvelocity :double) (yvelocity :double))
    (when (%gesture-swipe-velocity gesture xvelocity yvelocity)
      (values (cffi:mem-ref xvelocity :double)
              (cffi:mem-ref yvelocity :double)))))

(export 'gesture-swipe-velocity)

;;; --- End of file gtk3.gesture-swipe.lisp ------------------------------------
