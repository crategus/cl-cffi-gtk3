;;; ----------------------------------------------------------------------------
;;; gtk3.gesture.lisp
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
;;; GtkGesture
;;;
;;;     Base class for gestures
;;;
;;; Types and Values
;;;
;;;     GtkGesture
;;;     GtkEventSequenceState
;;;
;;; Functions
;;;
;;;     gtk_gesture_get_device
;;;     gtk_gesture_get_window                             Accessor
;;;     gtk_gesture_set_window                             Accessor
;;;     gtk_gesture_is_active
;;;     gtk_gesture_is_recognized
;;;     gtk_gesture_get_sequence_state
;;;     gtk_gesture_set_sequence_state
;;;     gtk_gesture_set_state
;;;     gtk_gesture_get_sequences
;;;     gtk_gesture_handles_sequence
;;;     gtk_gesture_get_last_updated_sequence
;;;     gtk_gesture_get_last_event
;;;     gtk_gesture_get_point
;;;     gtk_gesture_get_bounding_box
;;;     gtk_gesture_get_bounding_box_center
;;;     gtk_gesture_group
;;;     gtk_gesture_ungroup
;;;     gtk_gesture_get_group
;;;     gtk_gesture_is_grouped_with
;;;
;;; Properties
;;;
;;;     n-points
;;;     window
;;;
;;; Signals
;;;
;;;     begin
;;;     cancel
;;;     end
;;;     sequence-state-changed
;;;     update
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ├── GtkGestureSingle
;;;             ├── GtkGestureRotate
;;;             ╰── GtkGestureZoom
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventSequenceState
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkEventSequenceState" event-sequence-state
  (:export t
   :type-initializer "gtk_event_sequence_state_get_type")
  (:none 0)
  (:claimed 1)
  (:denied 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'event-sequence-state)
      "GEnum"
      (liber:symbol-documentation 'event-sequence-state)
 "@version{2024-03-22}
  @begin{declaration}
(gobject:define-genum \"GtkEventSequenceState\" event-sequence-state
  (:export t
   :type-initializer \"gtk_event_sequence_state_get_type\")
  (:none 0)
  (:claimed 1)
  (:denied 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{The sequence is handled, but not grabbed.}
      @entry[:claimed]{The sequence is handled and grabbed.}
      @entry[:denied]{The sequence is denied.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Describes the state of a @class{gdk:event-sequence} instance in a
    @class{gtk:gesture} object.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}")

;;; ----------------------------------------------------------------------------
;;; GtkGesture
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGesture" gesture
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_get_type")
  ((n-points
    gesture-n-points
    "n-points" "guint" t t)
   (window
    gesture-window
    "window" "GdkWindow" t t)))

#+liber-documentation
(setf (documentation 'gesture 'type)
 "@version{2025-07-15}
  @begin{short}
    The @class{gtk:gesture} object is the base object for gesture recognition,
    although this object is quite generalized to serve as a base for multi-touch
    gestures, it is suitable to implement single-touch and pointer-based
    gestures, using the special @code{nil} value for the
    @class{gdk:event-sequence} instance for these.
  @end{short}

  The number of touches that a @class{gtk:gesture} object need to be recognized
  is controlled by the @slot[gtk:gesture]{n-points} property, if a gesture is
  keeping track of less or more than that number of sequences, it will not check
  whether the gesture is recognized.

  As soon as the gesture has the expected number of touches, it will check
  regularly if it is recognized, the criteria to consider a gesture as
  \"recognized\" is left to GtkGesture subclasses.

  A recognized gesture will then emit the following signals:
  @begin{itemize}
    @item{The @sig[gtk:gesture]{begin} signal when the gesture is recognized.}
    @item{A number of @sig[gtk:gesture]{update} signals, whenever an input event
      is processed.}
    @item{The @sig[gtk:gesture]{end} signal when the gesture is no longer
      recognized.}
  @end{itemize}
  @subheading{Event propagation}
  In order to receive events, a gesture needs to either set a propagation phase
  through the @fun{gtk:event-controller-propagation-phase} function, or feed
  those manually through the @fun{gtk:event-controller-handle-event} function.

  In the capture phase, events are propagated from the toplevel down to the
  target widget, and gestures that are attached to containers above the widget
  get a chance to interact with the event before it reaches the target.

  After the capture phase, GTK emits the traditional
  @sig[gtk:widget]{button-press-event}, @sig[gtk:widget]{button-release-event},
  @sig[gtk:widget]{touch-event} signals. Gestures with the
  @val[gtk:propagation-phase]{:phase-target} phase are fed events from the
  default event handlers.

  In the bubble phase, events are propagated up from the target widget to the
  toplevel, and gestures that are attached to containers above the widget get a
  chance to interact with events that have not been handled yet.

  @subheading{States of a sequence}
  Whenever input interaction happens, a single event may trigger a cascade of
  @class{gtk:gesture} objects, both across the parents of the widget receiving
  the event and in parallel within an individual widget. It is a responsibility
  of the widgets using those gestures to set the state of touch sequences
  accordingly in order to enable cooperation of gestures around the
  @class{gdk:event-sequence} instance triggering those.

  Within a widget, gestures can be grouped through the @fun{gtk:gesture-group}
  function, grouped gestures synchronize the state of sequences, so calling the
  @fun{gtk:gesture-sequence-state} function on one will effectively propagate
  the state throughout the group.

  By default, all sequences start out in the
  @val[gtk:event-sequence-state]{:none} state, sequences in this state trigger
  the gesture event handler, but event propagation will continue unstopped by
  gestures.

  If a sequence enters into the @val[gtk:event-sequence-state]{:denied} state,
  the gesture group will effectively ignore the sequence, letting events go
  unstopped through the gesture, but the \"slot\" will still remain occupied
  while the touch is active.

  If a sequence enters in the @val[gtk:event-sequence-state]{:claimed} state,
  the gesture group will grab all interaction on the sequence, by:
  @begin{itemize}
    @item{Setting the same sequence to @val[gtk:event-sequence-state]{:denied}
      on every other gesture group within the widget, and every gesture on
      parent widgets in the propagation chain.}
    @item{calling \"cancel\" on every gesture in widgets underneath in the
      propagation chain.}
    @item{Stopping event propagation after the gesture group handles the event.}
  @end{itemize}
  Note: if a sequence is set early to @val[gtk:event-sequence-state]{:claimed}
  on @val[gdk:event-type]{:touch-begin}/@val[gdk:event-type]{:button-press}
  (so those events are captured before reaching the event widget, this implies
  @val[gtk:propagation-phase]{:phase-capture}), one similar event will emulated
  if the sequence changes to @val[gtk:event-sequence-state]{:denied}. This way
  event coherence is preserved before event propagation is unstopped again.

  Sequence states cannot be changed freely, see the
  @fun{gtk:gesture-sequence-state} function to know about the possible
  lifetimes of a @class{gdk:event-sequence} instance.

  @subheading{Touchpad gestures}
  On the platforms that support it, the @class{gtk:gesture} object will handle
  transparently touchpad gesture events. The only precautions users of the
  @class{gtk:gesture} object should do to enable this support are:
  @begin{itemize}
    @item{Enabling @val[gdk:event-mask]{:touchpad-gesture-mask} on their
      @class{gdk:window} objects.}
    @item{If the gesture has the @val[gtk:propagation-phase]{:phase-none}
      propagation phase, ensuring @val[gdk:event-type]{:touchpad-swipe} and
      @val[gdk:event-type]{:touchpad-pinch} events are handled by the
      @class{gtk:gesture} object}
  @end{itemize}
  @begin[Signal Details]{dictionary}
    @begin[gesture::begin]{signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture} object that received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that made the
          gesture to be recognized.}
      @end{simple-table}
      The signal is emitted when the gesture is recognized. This means the
      number of touch sequences matches @slot[gtk:gesture]{n-points}. Note:
      These conditions may also happen when an extra touch, for example, a third
      touch on a 2-touches gesture, is lifted, in that situation sequence will
      not pertain to the current set of active touches, so do not rely on this
      being true.
    @end{signal}
    @begin[gesture::cancel]{signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture} object that received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was
          cancelled.}
      @end{simple-table}
      The signal is emitted whenever a sequence is cancelled. This usually
      happens on active touches when the @fun{gtk:event-controller-reset}
      function is called on gesture, manually, due to grabs, or the individual
      sequence was claimed by controllers of the parent widgets, see the
      @fun{gtk:gesture-sequence-state} function. The @arg{gesture} argument
      must forget everything about @arg{sequence} as a reaction to the signal.
    @end{signal}
    @begin[gesture::end]{signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture} object that received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that made
          gesture recognition to finish.}
      @end{simple-table}
      The signal is emitted when gesture either stopped recognizing the event
      sequences as something to be handled, or the number of touch sequences
      became higher or lower than @slot[gtk:gesture]{n-points}. Note: The
      @arg{sequence} argument might not pertain to the group of sequences that
      were previously triggering recognition on gesture, that is, a just pressed
      touch sequence that exceeds @slot[gtk:gesture]{n-points}. This situation
      may be detected by checking through the @fun{gtk:gesture-handles-sequence}
      function.
    @end{signal}
    @begin[gesture::sequence-state-changed]{signal}
      @begin{pre}
lambda (gesture sequence state)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture} object that received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was
          cancelled.}
        @entry[state]{The new @sym{gtk:event-sequence-state} value.}
      @end{simple-table}
      The signal is emitted whenever a sequence state changes. See the
      @fun{gtk:gesture-sequence-state} function to know more about the
      expectable sequence lifetimes.
    @end{signal}
    @begin[gesture::update]{signal}
      @begin{pre}
lambda (gesture sequence)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture} object that received the
          signal.}
        @entry[sequence]{The @class{gdk:event-sequence} event that was updated.}
      @end{simple-table}
      The signal is emitted whenever an event is handled while the gesture is
      recognized. The @arg{sequence} argument is guaranteed to pertain to the
      set of active touches.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:gesture-n-points}
  @see-slot{gtk:gesture-window}
  @see-class{gtk:event-controller}
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gesture-n-points ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-points" 'gesture) t)
 "The @code{n-points} property of type @code{:int} (Read / Write) @br{}
  The number of touch points that trigger recognition on this gesture. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1 @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-n-points)
      "Accessor"
      (documentation 'gesture-n-points 'function)
 "@version{2025-07-06}
  @syntax{(gtk:gesture-n-points object) => n-points)}
  @syntax{(setf (gtk:gesture-n-points object) n-points)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[n-points]{an integer for the number of touch points}
  @begin{short}
    Accessor of the @slot[gtk:gesture]{n-points} slot of the
    @class{gtk:gesture} class.
  @end{short}
  The number of touch points that trigger recognition on this gesture.
  @see-class{gtk:gesture}")

;;; --- gtk:gesture-window -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "window" 'gesture) t)
 "The @code{window} property of type @class{gdk:window} (Read / Write) @br{}
  If non-@code{nil}, the gesture will only listen for events that happen on
  this @class{gdk:window} object, or a child of it.")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-window)
      "Accessor"
      (documentation 'gesture-window 'function)
 "@version{2023-03-05}
  @syntax{(gtk:gesture-window object) => window)}
  @syntax{(setf (gtk:gesture-window object) window)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Accessor of the @slot[gtk:gesture]{window} slot of the
    @class{gtk:gesture} class.
  @end{short}
  The @fun{gtk:gesture-window} function returns the user-defined window that
  receives the events handled by the gesture. The @setf{gtk:gesture-window}
  function sets a specific window to receive events about, so gesture will
  effectively handle only events targeting window, or a child of it. The
  @arg{window} argument must pertain to the @fun{gtk:event-controller-widget}
  function.
  @see-class{gtk:gesture}
  @see-class{gdk:window}
  @see-function{gtk:event-controller-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_device
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_device" gesture-device) (g:object gdk:device)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The @class{gdk:device} object, or @code{nil}.}
  @begin{short}
    Returns the master @class{gdk:device} object that is currently operating on
    @arg{gesture}, or @code{nil} if the gesture is not being interacted.
  @end{short}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-device)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_active
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_is_active" gesture-is-active) :boolean
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{@em{True} if @arg{gesture} is active.}
  @begin{short}
    Returns @em{true} if the gesture is currently active.
  @end{short}
  A gesture is active meanwhile there are touch sequences interacting with it.
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-is-active)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_recognized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_is_recognized" gesture-is-recognized) :boolean
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{@em{True} if @arg{gesture} is recognized.}
  @begin{short}
    Returns @em{true} if the gesture is currently recognized.
  @end{short}
  A gesture is recognized if there are as many interacting touch sequences as
  required by @arg{gesture}.
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-is-recognized)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_set_sequence_state
;;; gtk_gesture_get_sequence_state
;;; ----------------------------------------------------------------------------

(defun (setf gesture-sequence-state) (state gesture sequence)
  (when (cffi:foreign-funcall "gtk_gesture_set_sequence_state"
                              (g:object gesture) gesture
                              (g:boxed gdk:event-sequence) sequence
                              event-sequence-state state
                              :boolean)
    state))

(cffi:defcfun ("gtk_gesture_get_sequence_state" gesture-sequence-state)
    event-sequence-state
 #+liber-documentation
 "@version{#2025-07-11}
  @syntax{(gtk:gesture-sequence-state gesture sequence) => state}
  @syntax{(setf (gtk:gesture-sequence-state gesture sequence) state)}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance}
  @argument[state]{a @sym{gtk:event-sequence-state} value}
  @begin{short}
  The @fun{gtk:gesture-sequence-state} function returns the sequence state, as
  seen by @arg{gesture}.
  @end{short}
  The @setf{gtk:gesture-sequence-state} function sets the state of
  @arg{sequence} in @arg{gesture}. Sequences start in @code{:none} state, and
  whenever they change state, they can never go back to that state. Likewise,
  sequences in @code{:denied} cannot turn back to a not denied state. With these
  rules, the lifetime of an event sequence is constrained to the next four:
  @begin{itemize}
    @item{None}
    @item{None → Denied}
    @item{None → Claimed}
    @item{None → Claimed → Denied}
  @end{itemize}
  Note: Due to event handling ordering, it may be unsafe to set the state on
  another gesture within a @sig[gtk:gesture]{begin} signal handler, as the
  callback might be executed before the other gesture knows about the sequence.
  A safe way to perform this could be:
  @begin{pre}
static void
first_gesture_begin_cb (GtkGesture       *first_gesture,
                        GdkEventSequence *sequence,
                        gpointer          user_data)
{
  gtk_gesture_set_sequence_state (first_gesture, sequence,
                                  GTK_EVENT_SEQUENCE_CLAIMED);
  gtk_gesture_set_sequence_state (second_gesture, sequence,
                                  GTK_EVENT_SEQUENCE_DENIED);
@}

static void
second_gesture_begin_cb (GtkGesture       *second_gesture,
                         GdkEventSequence *sequence,
                         gpointer          user_data)
{
  if (gtk_gesture_get_sequence_state (first_gesture, sequence)
     == GTK_EVENT_SEQUENCE_CLAIMED)
     gtk_gesture_set_sequence_state (second_gesture, sequence,
                                     GTK_EVENT_SEQUENCE_DENIED);
@}
  @end{pre}
  If both gestures are in the same group, just set the state on the gesture
  emitting the event, the sequence will be already be initialized to the
  group's global state when the second gesture processes the event.
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}
  @see-symbol{gtk:event-sequence-state}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-sequence-state)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_set_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_set_state" gesture-set-state) :boolean
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[state]{a @sym{gtk:event-sequence-state} value}
  @return{@em{True} if the state of at least one sequence was changed
    successfully.}
  @begin{short}
    Sets the state of all sequences that @arg{gesture} is currently interacting
    with.
  @end{short}
  See the @fun{gtk:gesture-sequence-state} function for more details on sequence
  states.
  @see-class{gtk:gesture}
  @see-symbol{gtk:event-sequence-state}
  @see-function{gtk:gesture-sequence-state}"
  (gesture (g:object gesture))
  (state event-sequence-state))

(export 'gesture-set-state)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_sequences
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_sequences" gesture-sequences)
    (g:list-t (g:boxed gdk:event-sequence))
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The list of @class{gdk:event-sequence} instances.}
  @begin{short}
    Returns the list of @class{gdk:event-sequence} instances currently being
    interpreted by @arg{gesture}.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture)))

(export 'gesture-sequences)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_handles_sequence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_handles_sequence" gesture-handles-sequence) :boolean
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance}
  @return{@em{True} if @arg{gesture} is handling @arg{sequence}, @em{false}
    otherwise.}
  @begin{short}
    Returns @em{true} if @arg{gesture} is currently handling events
    corresponding to @arg{sequence}.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-handles-sequence)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_updated_sequence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_last_updated_sequence"
               gesture-last-updated-sequence) (g:boxed gdk:event-sequence)
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The last @class{gdk:event-sequence} udate sequence.}
  @begin{short}
    Returns the @class{gdk:event-sequence} instance that was last updated on
    @arg{gesture}.
  @end{short}
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (gesture (g:object gesture)))

(export 'gesture-last-updated-sequence)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_last_event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_last_event" gesture-last-event)
    (g:boxed gdk:event)
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance}
  @return{The last @class{gdk:event} event from @arg{sequence}.}
  @begin{short}
    Returns the last event that was processed for @arg{sequence}.
  @end{short}
  Note that the returned event is only valid as long as the sequence is still
  interpreted by the gesture. If in doubt, you should make a copy of the event.
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}
  @see-class{gdk:event}"
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence)))

(export 'gesture-last-event)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_point" %gesture-point) :boolean
  (gesture (g:object gesture))
  (sequence (g:boxed gdk:event-sequence))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gesture-point (gesture sequence)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[sequence]{a @class{gdk:event-sequence} instance, or @code{nil}
    for pointer events}
  @begin{return}
    @arg{x} -- a double float for the x axis for the sequence coordinates @br{}
    @arg{y} -- a double float for the y axis for the sequence coordinates
  @end{return}
  @begin{short}
    If @arg{sequence} is currently being interpreted by @arg{gesture}, this
    function returns @arg{x} and @arg{y} with the last coordinates stored for
    that event sequence.
  @end{short}
  The coordinates are always relative to the widget allocation.
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%gesture-point gesture sequence x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'gesture-point)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_bounding_box" %gesture-bounding-box) :boolean
  (gesture (g:object gesture))
  (rect (g:boxed gdk:rectangle)))

(defun gesture-bounding-box (gesture)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[gesture]{a @class{gtk:gesture} object}
  @begin{return}
    The @class{gdk:rectangle} instance with bounding box containing all active
    touches.
  @end{return}
  @begin{short}
    If there are touch sequences being currently handled by @arg{gesture}, this
    function returns the bounding box containing all active touches.
  @end{short}
  Otherwise, @em{false} will be returned.

  Note: This function will yield unexpected results on touchpad gestures. Since
  there is no correlation between physical and pixel distances, these will look
  as if constrained in an infinitely small area, rect width and height will thus
  be 0 regardless of the number of touchpoints.
  @see-class{gtk:gesture}
  @see-class{gdk:rectangle}"
  (let ((rect (gdk:rectangle-new)))
    (when (%gesture-bounding-box gesture rect)
      rect)))

(export 'gesture-bounding-box)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_bounding_box_center
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_bounding_box_center"
               %gesture-bounding-box-center) :boolean
  (gesture (g:object gesture))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gesture-bounding-box-center (gesture)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[gesture]{a @class{gtk:gesture} object}
  @begin{return}
    @arg{x} -- a double float for the x coordinate for the bounding box center
    @br{}
    @arg{y} -- a double float for the y coordinate for the bounding box center
  @end{return}
  @begin{short}
    If there are touch sequences being currently handled by @arg{gesture}, this
    function returns @arg{x} and @arg{y} with the center of the bounding box
    containing all active touches.
  @end{short}
  Otherwise, @em{false} will be returned.
  @see-class{gtk:gesture}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%gesture-bounding-box-center gesture x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'gesture-bounding-box-center)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_group" gesture-group) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[group]{a @class{gtk:gesture} object to group @arg{gesture} with}
  @argument[gesture]{a @class{gtk:gesture} object}
  @begin{short}
    Adds @arg{gesture} to the same group than @arg{group}.
  @end{short}
  Gestures are by default isolated in their own groups.

  When gestures are grouped, the state of @class{gdk:event-sequence} instances
  is kept in sync for all of those, so calling the
  @fun{gtk:gesture-sequence-state} function, on one will transfer the same
  value to the others.

  Groups also perform an \"implicit grabbing\" of sequences, if a
  @class{gdk:event-sequence} state is set to @code{:claimed} on one group,
  every other gesture group attached to the same GtkWidget will switch the
  state for that sequence to @code{:denied}.
  @see-class{gtk:gesture}
  @see-class{gdk:event-sequence}
  @see-function{gtk:gesture-sequence-state}"
  (group (g:object gesture))
  (gesture (g:object gesture)))

(export 'gesture-group)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_ungroup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_ungroup" gesture-ungroup) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @begin{short}
    Separates @arg{gesture} into an isolated group.
  @end{short}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-ungroup)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_get_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_get_group" gesture-get-group)
    (g:list-t (g:object gesture))
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[gesture]{a @class{gtk:gesture} object}
  @return{The list of @class{gtk:gesture} objects.}
  @begin{short}
    Returns all gestures in the group of @arg{gesture}.
  @end{short}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture)))

(export 'gesture-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_is_grouped_with
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_is_grouped_with" gesture-is-grouped-with) :boolean
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[gesture]{a @class{gtk:gesture} object}
  @argument[other]{a @class{gtk:gesture} object}
  @return{The boolean whether the gestures are grouped.}
  @begin{short}
    Returns @em{true} if both gestures pertain to the same group.
  @end{short}
  @see-class{gtk:gesture}"
  (gesture (g:object gesture))
  (other (g:object gesture)))

(export 'gesture-is-grouped-with)

;;; --- End of file gtk3.gesture.lisp ------------------------------------------
