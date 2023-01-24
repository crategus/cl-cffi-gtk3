;;; ----------------------------------------------------------------------------
;;; gtk.gesture-pan.lisp
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
;;; GtkGesturePan
;;;
;;;     Pan gesture
;;;
;;; Types and Values
;;;
;;;     GtkGesturePan
;;;     GtkPanDirection
;;;
;;; Functions
;;;
;;;     gtk_gesture_pan_new
;;;     gtk_gesture_pan_get_orientation                    Accessor
;;;     gtk_gesture_pan_set_orientation                    Accessor
;;;
;;; Properties
;;;
;;;     orientation
;;;
;;; Signals
;;;
;;;     pan
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureDrag
;;;                     ╰── GtkGesturePan
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPanDirection
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPanDirection" pan-direction
  (:export t
   :type-initializer "gtk_pan_direction_get_type")
  :left
  :right
  :up
  :down)

#+liber-documentation
(setf (liber:alias-for-symbol 'pan-direction)
      "GEnum"
      (liber:symbol-documentation 'pan-direction)
 "@version{#2023-1-21}
  @begin{short}
    Describes the panning direction of a @class{gtk:gesture-pan} object.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPanDirection\" pan-direction
  (:export t
   :type-initializer \"gtk_pan_direction_get_type\")
  :left
  :right
  :up
  :down)
  @end{pre}
  @begin[code]{table}
    @entry[:left]{Panned towards the left.}
    @entry[:right]{Panned towards the right.}
    @entry[:up]{Panned upwards.}
    @entry[:down]{Panned downwards.}
  @end{table}
  @see-class{gtk:gesture-pan}")

;;; ----------------------------------------------------------------------------
;;; struct GtkGesturePan
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGesturePan" gesture-pan
  (:superclass gesture-drag
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_pan_get_type")
  ((orientation
    gesture-pan-orientation
    "orientation" "GtkOrientation" t t)))

#+liber-documentation
(setf (documentation 'gesture-pan 'type)
 "@version{#2023-1-21}
  @begin{short}
    The @sym{gtk:gesture-pan} object is a @class{gtk:gesture} implementation
    able to recognize pan gestures, those are drags that are locked to happen
    along one axis.
  @end{short}
  The axis that a @sym{gtk:gesture-pan} object handles is defined at construct
  time, and can be changed through the @fun{gtk:gesture-pan-orientation}
  function.

  When the gesture starts to be recognized, The @sym{gtk:gesture-pan} object
  will attempt to determine as early as possible whether the sequence is moving
  in the expected direction, and denying the sequence if this does not happen.

  Once a panning gesture along the expected axis is recognized, the \"pan\"
  signal will be emitted as input events are received, containing the offset in
  the given axis.
  @begin[Signal Details]{dictionary}
    @subheading{The \"pan\" signal}
    @begin{pre}
lambda (gesture n-press x y)    :run-last
    @end{pre}
    The signal is emitted once a panning gesture along the expected axis is
    detected.
    @begin[code]{table}
      @entry[gesture]{The @sym{gtk:gesture-pan} object which received the
        signal.}
      @entry[direction]{Current @symbol{gtk:pan-direction} value of the pan
        gesture.}
      @entry[offset]{A double float with the offset along the gesture
        orientation.}
    @end{table}
  @end{dictionary}
  @see-slot{gtk:gesture-pan-orientation}
  @see-class{gtk:gesture}
  @see-symbol{gtk:pan-direction}")

;;; --- gesture-pan-orientation ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'gesture-pan) t)
 "The @code{orientation} property of type @symbol{gtk:orientation}
  (Read / Write) @br{}
  The expected orientation of pan gestures. @br{}
  Default value: @code{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-pan-orientation)
      "Accessor"
      (documentation 'gesture-pan-orientation 'function)
 "@version{#2023-1-21}
  @syntax[]{(gtk:gesture-pan-orientation object) => orientation)}
  @syntax[]{(setf (gtk:gesture-pan-orientation object) orientation)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[orientation]{expected orientation of type @symbol{gtk:orientation}}
  @begin{short}
    Accessor of the @slot[gtk:gesture-pan]{orientation} slot of the
    @class{gtk:gesture-pan} class.
  @end{short}
  The @sym{gtk:gesture-pan-orientation} function returns the orientation of the
  pan gestures that this gesture expects. The
  @sym{(setf gtk:gesture-pan-orientation)} function sets the orientation to be
  expected on pan gestures.
  @see-class{gtk:gesture-pan}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_pan_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-pan-new))

(defun gesture-pan-new (widget orientation)
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[orientation]{expected orientation of @symbol{gtk:orientation} type}
  @return{A newly created @class{gtk:gesture-pan} object.}
  @begin{short}
    Returns a newly created gesture that recognizes pan gestures.
  @end{short}
  @see-class{gtk:gesture-pan}
  @see-symbol{gtk:orientation}"
  (make-instance 'gesture-pan
                 :widget widget
                 :orientation orientation))

(export 'gesture-pan-new)

;;; --- End of file gtk3.gesture-pan.lisp --------------------------------------
