;;; ----------------------------------------------------------------------------
;;; gtk3.gesture-multi-press.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkGestureMultiPress
;;;
;;;     Multipress gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureMultiPress
;;;
;;; Functions
;;;
;;;     gtk_gesture_multi_press_new
;;;     gtk_gesture_multi_press_set_area
;;;     gtk_gesture_multi_press_get_area
;;;
;;; Signals
;;;
;;;     pressed
;;;     released
;;;     stopped
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureMultiPress
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureMultiPress
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureMultiPress" gesture-multi-press
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_multi_press_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-multi-press 'type)
 "@version{#2023-3-6}
  @begin{short}
    The @class{gtk:gesture-multi-press} object is a @class{gtk:gesture}
    implementation able to recognize multiple clicks on a nearby zone, which can
    be listened for through the @code{\"pressed\"} signal.
  @end{short}
  Whenever time or distance between clicks exceed the GTK defaults, \"stopped\"
  is emitted, and the click counter is reset.

  Callers may also restrict the area that is considered valid for a > 1
  touch/button press through the @fun{gtk:gesture-multi-press-area} function,
  so any click happening outside that area is considered to be a first click of
  its own.
  @begin[Signal Details]{dictionary}
    @subheading{The \"pressed\" signal}
      @begin{pre}
lambda (gesture n-press x y)    :run-last
      @end{pre}
      The signal is emitted whenever a button or touch press happens.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-multi-press} object which
          received the signal.}
        @entry[n-press]{An integer with how many touch/button presses happened
          with this one.}
        @entry[x]{A double float with the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{A double float with the y coordinate, in widget allocation
          coordinates.}
      @end{table}
    @subheading{The \"released\" signal}
      @begin{pre}
lambda (gesture n-press x y)    :run-last
      @end{pre}
      The signal is emitted when a button or touch is released. The
      @arg{n-press} argument will report the number of press that is paired to
      this event, note that the @code{\"stopped\"} signal may have been emitted
      between the press and its release, the @arg{n-press} argument will only
      start over at the next press.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-multi-press} object which
          received the signal.}
        @entry[n-press]{An integer with the number of press that is paired with
          this release.}
        @entry[x]{A double float with the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{A double float with the y coordinate, in widget allocation
          coordinates.}
      @end{table}
    @subheading{The \"stopped\" signal}
      @begin{pre}
lambda (gesture)    :run-last
      @end{pre}
      The signal is emitted whenever any time/distance threshold has been
      exceeded.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-multi-press} object which
        received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:gesture-multi-press-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_multi_press_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-multi-press-new))

(defun gesture-multi-press-new (widget)
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[widget]{a @class{gtk:widget} object}
  @return{A newly created @class{gtk:gesture-multi-press} object.}
  @begin{short}
    Returns a newly created gesture that recognizes single and multiple presses.
  @end{short}
  @see-class{gtk:gesture-multi-press}
  @see-class{gtk:widget}"
  (make-instance 'gesture-multi-press
                 :widget widget))

(export 'gesture-multi-press-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_multi_press_get_area ()
;;; gtk_gesture_multi_press_set_area ()
;;; ----------------------------------------------------------------------------

(defun (setf gesture-multi-press-area) (rect gesture)
  (cffi:foreign-funcall "gtk_gesture_multi_press_set_area"
                        (g:object gesture-multi-press) gesture
                        (g:boxed gdk:rectangle) rect
                        :void)
  rect)

(cffi:defcfun ("gtk_gesture_multi_press_get_area" %gesture-multi-press-area)
    :boolean
  (gesture (g:object gesture-multi-press))
  (rect (g:boxed gdk:rectangle)))

(defun gesture-multi-press-area (gesture)
 #+liber-documentation
 "@version{#2023-3-6}
  @syntax{(gtk:gesture-multi-press-area gesture) => rect}
  @syntax{(setf (gtk:gesture-multi-press-area gesture) rect)}
  @argument[gesture]{a @class{gtk:gesture-multi-press} object}
  @argument[rect]{a @class{gdk:rectangle} instance with the press area}
  @begin{short}
    Accessor of the press area of the gesture.
  @end{short}
  The @fun{gtk:gesture-multi-press-area} function gets the press area. The
  @setf{gtk:gesture-multi-press-area} function sets the press area.

  If the @arg{rect} argument is non-@code{nil}, the press area will be checked
  to be confined within the rectangle, otherwise the button count will be reset
  so the press is seen as being the first one. If the @arg{rect} argument is
  @code{nil}, the area will be reset to an unrestricted state.

  Note: The rectangle is only used to determine whether any non-first click
  falls within the expected area. This is not akin to an input shape.
  @see-class{gtk:gesture-multi-press}
  @see-class{gdk:rectangle}"
  (let ((rect (gdk:rectangle-new)))
    (when (%gesture-multi-press-area gesture rect)
      rect)))

(export 'gesture-multi-press-area)

;;; --- End of file gtk3.gesture-multi-press.lisp ------------------------------
