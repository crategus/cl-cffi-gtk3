;;; ----------------------------------------------------------------------------
;;; gdk3.events.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; Events
;;;
;;;     Functions for handling events from the window system
;;;
;;; Types and Values
;;;
;;;     GdkEventType           --> gdk.event-structures.lisp
;;;     GdkEventMask           --> gdk.event-structures.lisp
;;;     GdkEventSequence       --> gdk.event-structures.lisp
;;;
;;;     GDK_CURRENT_TIME
;;;     GDK_PRIORITY_EVENTS
;;;     GDK_PRIORITY_REDRAW
;;;     GDK_EVENT_PROPAGATE
;;;     GDK_EVENT_STOP
;;;     GDK_BUTTON_PRIMARY
;;;     GDK_BUTTON_MIDDLE
;;;     GDK_BUTTON_SECONDARY
;;;
;;; Functions
;;;
;;;     gdk_events_pending
;;;     gdk_event_peek
;;;     gdk_event_get
;;;     gdk_event_put
;;;     gdk_event_new
;;;     gdk_event_copy
;;;     gdk_event_free
;;;     gdk_event_get_axis
;;;     gdk_event_get_button
;;;     gdk_event_get_click_count
;;;     gdk_event_get_coords
;;;     gdk_event_get_keycode
;;;     gdk_event_get_keyval
;;;     gdk_event_get_root_coords
;;;     gdk_event_get_scroll_direction
;;;     gdk_event_get_scroll_deltas
;;;     gdk_event_is_scroll_stop_event
;;;     gdk_event_get_state
;;;     gdk_event_get_time
;;;     gdk_event_get_window
;;;     gdk_event_get_event_type
;;;     gdk_event_get_event_sequence
;;;     gdk_event_request_motions
;;;     gdk_events_get_angle
;;;     gdk_events_get_center
;;;     gdk_events_get_distance
;;;     gdk_event_triggers_context_menu
;;;     gdk_event_get_seat
;;;     gdk_event_get_scancode
;;;     gdk_event_get_pointer_emulated
;;;     gdk_event_handler_set
;;;     gdk_get_show_events
;;;     gdk_set_show_events
;;;     gdk_event_set_screen
;;;     gdk_event_get_screen
;;;     gdk_event_get_device
;;;     gdk_event_set_device
;;;     gdk_event_get_source_device
;;;     gdk_event_set_source_device
;;;     gdk_event_get_device_tool
;;;     gdk_event_set_device_tool
;;;     gdk_setting_get
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GDK_CURRENT_TIME
;;; ----------------------------------------------------------------------------

(defconstant +current-time+ 0
 #+liber-documentation
 "@version{2024-04-02}
  @begin{short}
    Represents the current time, and can be used anywhere a time is expected.
  @end{short}")

#+liber-documentation
(setf (liber:alias-for-variable '+current-time+) "Constant")

(export '+current-time+)

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_EVENTS
;;;
;;; #define GDK_PRIORITY_EVENTS
;;;
;;; This is the priority that events from the X server are given in the GLib
;;; Main Loop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PRIORITY_REDRAW
;;; ----------------------------------------------------------------------------

(defconstant +priority-redraw+ (+ glib:+priority-high-idle+ 20)
 #+liber-documentation
 "@version{2024-04-02}
  @begin{short}
    This is the priority that the idle handler processing window updates is
    given in the GLib Main Loop.
  @end{short}")

#+liber-documentation
(setf (liber:alias-for-variable '+priority-redraw+) "Constant")

(export '+priority-redraw+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_PROPAGATE
;;; ----------------------------------------------------------------------------

(defconstant +event-propagate+ nil
 #+liber-documentation
 "@version{2024-04-02}
  @variable-value{@em{false}}
  @begin{short}
    Use this value as the return value for continuing the propagation of an
    event handler.
  @end{short}
  @see-variable{gdk:+event-stop+}")

#+liber-documentation
(setf (liber:alias-for-variable '+event-propagate+) "Constant")

(export '+event-propagate+)

;;; ----------------------------------------------------------------------------
;;; GDK_EVENT_STOP
;;; ----------------------------------------------------------------------------

(defconstant +event-stop+ t
 #+liber-documentation
 "@version{2024-04-02}
  @variable-value{@em{true}}
  @begin{short}
    Use this value as the return value for stopping the propagation of an event
    handler.
  @end{short}
  @begin[Example]{dictionary}
    This event handler for the @code{\"delete-event\"} signal of a window stops
    the propagation of the event and the window is not closed.
    @begin{pre}
(g:signal-connect window \"delete-event\"
                  (lambda (widget event)
                    (declare (ignore widget event))
                    gdk:+event-stop+))
    @end{pre}
  @end{dictionary}
  @see-variable{gdk:+event-propagate+}")

#+liber-documentation
(setf (liber:alias-for-variable '+event-stop+) "Constant")

(export '+event-stop+)

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_PRIMARY
;;;
;;; #define GDK_BUTTON_PRIMARY (1)
;;;
;;; The primary button. This is typically the left mouse button, or the right
;;; button in a left-handed setup.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_MIDDLE
;;;
;;; #define GDK_BUTTON_MIDDLE (2)
;;;
;;; The middle button.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_BUTTON_SECONDARY
;;;
;;; #define GDK_BUTTON_SECONDARY (3)
;;;
;;; The secondary button. This is typically the right mouse button, or the left
;;; button in a left-handed setup.
;;;
;;; Since 3.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_events_pending
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_pending" events-pending) :boolean
 #+liber-documentation
 "@version{2023-02-26}
  @return{@em{True} if any events are pending.}
  @begin{short}
    Checks if any events are ready to be processed for any display.
  @end{short}
  @begin[Example]{dictionary}
    The @code{clear-event-loop} function looks for pending events.
    @begin{pre}
(defun clear-event-loop ()
  (loop while (gdk:events-pending)
        do (gtk-main-iteration-do nil)))
    @end{pre}
  @end{dictionary}
  @see-class{gdk:event}
  @see-function{gdk:event-peek}")

(export 'events-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_event_peek
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_peek" event-peek) (g:boxed event :return)
 #+liber-documentation
 "@version{2025-10-09}
  @begin{return}
    The copy of the first @class{gdk:event} instance on some event queue, or
    @code{nil} if no events are in any queues.
  @end{return}
  @begin{short}
    If there is an event waiting in the event queue of some open display,
    returns a copy of it.
  @end{short}
  See the @fun{gdk:display-peek-event} function.
  @see-class{gdk:event}
  @see-function{gdk:events-pending}
  @see-function{gdk:display-peek-event}")

(export 'event-peek)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get" event-get) (g:boxed event :return)
 #+liber-documentation
 "@version{2023-02-26}
  @begin{return}
    The next @class{gdk:event} instance to be processed, or @code{nil} if no
    events are pending.
  @end{return}
  @begin{short}
    Checks all open displays for an event to be processed, fetching events from
    the windowing system if necessary.
  @end{short}
  See also the @fun{gdk:display-event} function.
  @see-class{gdk:event}
  @see-function{gdk:display-event}")

(export 'event-get)

;;; ----------------------------------------------------------------------------
;;; gdk_event_put
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_put" event-put) :void
 #+liber-documentation
 "@version{2023-02-26}
  @argument[event]{a @class{gdk:event} instance}
  @begin{short}
    Appends a copy of the given event onto the front of the event queue for
    the display, or the default event queue if the event has no window.
  @end{short}
  See also the @fun{gdk:display-put-event} function.
  @see-class{gdk:event}
  @see-function{gdk:display-put-event}"
  (event (g:boxed event)))

(export 'event-put)

;;; ----------------------------------------------------------------------------
;;; gdk_event_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_new" %event-new) (g:boxed event)
  (event-type event-type))

(defun event-new (event-type &rest args)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event-type]{a value of the @symbol{gdk:event-type} enumeration}
  @argument[args]{pairs of property name and property value}
  @return{The new  @class{gdk:event} instance.}
  @begin{short}
    Creates a new event of the given type.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:event-new :button-press :x 10.0d0 :y 20.0d0)
=>
#S(GDK-EVENT-BUTTON
   :TYPE :BUTTON-PRESS
   :WINDOW NIL
   :SEND-EVENT NIL
   :TIME 0
   :X 10.0d0
   :Y 20.0d0
   :AXES (0.0d0 0.0d0)
   :STATE 0
   :BUTTON 0
   :DEVICE #.(SB-SYS:INT-SAP #X00000000)
   :X-ROOT 0.0d0
   :Y-ROOT 0.0d0)
    @end{pre}
  @end{dictionary}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}"
  (cond ((member event-type '(:key-press :key-release))
         (apply #'make-event-key :type event-type args))
        ((member event-type
                 '(:button-press :2button-press :double-button-press
                   :3button-press :triple-button-press :button-release))
         (apply #'make-event-button :type event-type args))
        ((member event-type '(:motion-notify))
         (apply #'make-event-motion :type event-type args))
        ((member event-type '(:enter-notify :leave-notify))
         (apply #'make-event-crossing :type event-type args))
        ((member event-type '(:focus-change))
         (apply #'make-event-focus :type event-type args))
        ((member event-type '(:configure))
         (apply #'make-event-configure :type event-type args))
        ((member event-type '(:property-notify))
         (apply #'make-event-property :type event-type args))
        ((member event-type
                 '(:selection-clear :selection-notify :selection-request))
         (apply #'make-event-selection :type event-type args))
        ((member event-type '(:proximity-in :proximity-out))
         (apply #'make-event-proximity :type event-type args))
        ((member event-type
                 '(:drag-enter :drag-leave :drag-motion :drag-status
                   :drop-start :drop-finished))
         (apply #'make-event-dnd :type event-type args))
        ((member event-type '(:visibility-notify))
         (apply #'make-event-visibility :type event-type args))
        ((member event-type '(:scroll))
         (apply #'make-event-scroll :type event-type args))
        ((member event-type '(:window-state))
         (apply #'make-event-window-state :type event-type args))
        ((member event-type '(:setting))
         (apply #'make-event-setting :type event-type args))
        ((member event-type '(:owner-change))
         (apply #'make-event-owner-change :type event-type args))
        ((member event-type '(:grab-broken))
         (apply #'make-event-grab-broken :type event-type args))
        ((member event-type
                 '(:touch-begin :touch-update :touch-end :touch-cancel))
         (apply #'make-event-touch :type event-type args))
        ((member event-type '(:touchpad-swipe))
         (apply #'make-event-touchpad-swipe :type event-type args))
        ((member event-type '(:touchpad-pinch))
         (apply #'make-event-touchpad-pinch :type event-type args))
        ((member event-type '(:pad-button-press :pad-button-release))
         (apply #'make-event-pad-button :type event-type args))
        ((member event-type '(:pad-ring :pad-strip))
         (apply #'make-event-pad-axis :type event-type args))
        ((member event-type '(:pad-group-mode))
         (apply #'make-event-pad-group-mode :type event-type args))
        (t
         (%event-new event-type))))

(export 'event-new)

;;; ----------------------------------------------------------------------------
;;; gdk_event_copy
;;; ----------------------------------------------------------------------------

(declaim (inline event-copy))

(defun event-copy (event)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event]{a @class{gdk:event} instance}
  @return{The copy of the @class{gdk:event} instance.}
  @begin{short}
    Copies an event.
  @end{short}
  @see-class{gdk:event}"
  (copy-event event))

(export 'event-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_event_free ()
;;;
;;; void gdk_event_free (GdkEvent *event);
;;;
;;; Frees a GdkEvent, freeing or decrementing any resources associated with it.
;;; Note that this function should only be called with events returned from
;;; functions such as gdk_event_peek(), gdk_event_get(), gdk_event_copy() and
;;; gdk_event_new().
;;;
;;; event :
;;;     a GdkEvent.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_axis" %event-axis) :boolean
  (event (g:boxed event))
  (axis axis-use)
  (value (:pointer :double)))

(defun event-axis (event axis)
 #+liber-documentation
 "@version{2025-08-31}
  @argument[event]{a @class{gdk:event} instance}
  @argument[axis]{a @sym{gdk:axis-use} value for the axis use to look for}
  @return{The double float for the value found.}
  @short{Extract the axis value for a particular axis use from an event.}
  @see-class{gdk:event}
  @see-symbol{gdk:axis-use}"
  (cffi:with-foreign-object (value :double)
    (when (%event-axis event axis value)
      (cffi:mem-ref value :double))))

(export 'event-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_button" %event-button) :boolean
  (event (g:boxed event))
  (button (:pointer :uint)))

(defun event-button (event)
 #+liber-documentation
 "@version{2025-07-17}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    The unsigned integer for the mouse button number, or @code{nil} if the
    event does not deliver a button number.
  @end{return}
  @short{Extract the button number from an event.}
  @see-class{gdk:event}
  @see-class{gdk:event-button}
  @see-function{gdk:event-click-count}"
  (cffi:with-foreign-object (button :uint)
    (when (%event-button event button)
      (cffi:mem-ref button :uint))))

(export 'event-button)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_click_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_click_count" %event-click-count) :boolean
  (event (g:boxed event))
  (click-count (:pointer :uint)))

(defun event-click-count (event)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    Click count, or @code{nil} if the event does not deliver a click count.
  @end{return}
  @begin{short}
    Extracts the click count from an event.
  @end{short}
  @see-class{gdk:event}
  @see-class{gdk:event-button}
  @see-function{gdk:event-button}"
  (cffi:with-foreign-object (click-count :uint)
    (when (%event-click-count event click-count)
      (cffi:mem-ref click-count :uint))))

(export 'event-click-count)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_coords" %event-coords) :boolean
  (event (g:boxed event))
  (xwin (:pointer :double))
  (ywin (:pointer :double)))

;; The Lisp implementation returns the values.

(defun event-coords (event)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @code{xwin} -- a double float for the event window x coordinate @br{}
    @code{ywin} -- a double float for the event window y coordinate
  @end{return}
  @begin{short}
    Extract the event window relative x/y coordinates from an event.
  @end{short}
  @see-class{gdk:event}
  @see-function{gdk:event-root-coords}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%event-coords event x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'event-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_keycode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_keycode" %event-keycode) :boolean
  (event (g:boxed event))
  (keycode (:pointer :uint16)))

(defun event-keycode (event)
 "@version{2025-08-31}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer for the keycode.}
  @short{Extracts the hardware keycode from an event.}
  @see-class{gdk:event}
  @see-class{gdk:event-key}
  @see-function{gdk:event-keyval}"
  (cffi:with-foreign-object (keycode :uint16)
    (when (%event-keycode event keycode)
      (cffi:mem-ref keycode :uint16))))

(export 'event-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_keyval
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_keyval" %event-keyval) :boolean
  (event (g:boxed event))
  (keyval (:pointer :uint)))

(defun event-keyval (event)
 "@version{2025-08-31}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer for the keyval.}
  @begin{short}
    Extracts the keyval from an event.
  @end{short}
  @see-class{gdk:event}
  @see-class{gdk:event-key}
  @see-function{gdk:event-keycode}"
  (cffi:with-foreign-object (keyval :uint)
    (when (%event-keyval event keyval)
      (cffi:mem-ref keyval :uint))))

(export 'event-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_root_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_root_coords" %event-root-coords) :boolean
  (event (g:boxed event))
  (xroot (:pointer :double))
  (yroot (:pointer :double)))

(defun event-root-coords (event)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @code{xroot} -- a double float for the root window x coordinate @br{}
    @code{yroot} -- a double float for the root window y coordinate
  @end{return}
  @begin{short}
    Extract the root window relative x/y coordinates from an event.
  @end{short}
  @see-class{gdk:event}
  @see-function{gdk:event-coords}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%event-root-coords event x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'event-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scroll_direction
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_scroll_direction" %event-get-scroll-direction) :boolean
  (event (g:boxed event))
  (direction (:pointer scroll-direction)))

(defun event-get-scroll-direction (event)
 "@version{2025-08-31}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @sym{gdk:scroll-direction} value for the scroll direction.}
  @short{Extracts the scroll direction from an event.}
  @see-class{gdk:event}
  @see-class{gdk:event-scroll}"
  (cffi:with-foreign-object (direction 'scroll-direction)
    (when (%event-get-scroll-direction event direction)
      (cffi:mem-ref direction 'scroll-direction))))

(export 'event-get-scroll-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scroll_deltas
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_scroll_deltas" %event-scroll-deltas) :boolean
  (event (g:boxed event))
  (xdelta (:pointer :double))
  (ydelta (:pointer :double)))

(defun event-scroll-deltas (event)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    @code{xdelta} -- a double float for x delta @br{}
    @code{ydelta} -- a double float for y delta
  @end{return}
  @begin{short}
    Retrieves the scroll deltas from an event.
  @end{short}
  See also the @fun{gdk:event-get-scroll-direction} function.
  @see-class{gdk:event-scroll}
  @see-function{gdk:event-get-scroll-direction}
  @see-function{gdk:event-scroll-delta-x}
  @see-function{gdk:event-scroll-delta-y}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%event-scroll-deltas event x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'event-scroll-deltas)

;;; ----------------------------------------------------------------------------
;;; gdk_event_is_scroll_stop_event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_is_scroll_stop_event" event-is-scroll-stop-event) :boolean
 "@version{2025-07-01}
  @argument[event]{a @class{gdk:event} instance}
  @return{The boolean whether a scroll event is a stop scroll event.}
  @begin{short}
    Check whether a scroll event is a stop scroll event.
  @end{short}
  Scroll sequences with smooth scroll information may provide a stop scroll
  event once the interaction with the device finishes, for example, by lifting
  a finger. This stop scroll event is the signal that a widget may trigger
  kinetic scrolling based on the current velocity.

  Stop scroll events always have a a delta of 0/0.
  @see-class{gdk:event}"
  (event (g:boxed event)))

(export 'event-is-scroll-stop-event)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_state" %event-state) :boolean
  (event (g:boxed event))
  (state (:pointer modifier-type)))

(defun event-state (event)
 #+liber-documentation
 "@version{2025-08-31}
  @argument[event]{a @class{gdk:event} instance or @code{nil}}
  @return{The @sym{gdk:modifier-type} value for the state.}
  @begin{short}
    If the event contains a \"state\" field, returns the value.
  @end{short}
  Otherwise returns an empty state (0). @arg{event} may be @code{nil}, in which
  case it is treated as if the event had no state field.
  @see-class{gdk:event}"
  (cffi:with-foreign-object (state 'modifier-type)
    (if (%event-state event state)
        (cffi:mem-ref state 'modifier-type)
        0)))

(export 'event-state)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_time" event-time) :uint32
 #+liber-documentation
 "@version{2025-08-31}
  @argument[event]{a @class{gdk:event} instance}
  @return{The unsigned integer for the time stamp field from the event.}
  @begin{short}
    Returns the current time of the event.
  @end{short}
  If @arg{event} is @code{nil}, returns the value of the
  @var{gdk:+current-time+} constant.
  @see-class{gdk:event}"
  (event (g:boxed event)))

(export 'event-time)

;;; ----------------------------------------------------------------------------
;;; GdkEventSequence
;;;
;;; typedef struct _GdkEventSequence GdkEventSequence;
;;; ----------------------------------------------------------------------------

;;; Implementation moved to gtk.event-structures.lisp

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_window ()
;;;
;;; GdkWindow *
;;; gdk_event_get_window (const GdkEvent *event);
;;;
;;; Extracts the GdkWindow associated with an event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     The GdkWindow associated with the event.
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor event-window

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_type ()
;;;
;;; GdkEventType gdk_event_get_event_type (const GdkEvent *event);
;;;
;;; Retrieves the type of the event.
;;;
;;; event :
;;;     a GdkEvent
;;;
;;; Returns :
;;;     a GdkEventType
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;; Implemented as the accessor event-type

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_event_sequence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_event_sequence" event-event-sequence)
    (g:boxed event-sequence)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[event]{a @class{gdk:event} instance}
  @begin{return}
    The @class{gdk:event-sequence} event sequence that the event belongs to.
  @end{return}
  @begin{short}
    If the event is of type @code{:touch-begin}, @code{:touch-update},
    @code{:touch-end} or @code{:touch-cancel}, returns the
    @class{gdk:event-sequence} instance to which the event belongs.
  @end{short}
  Otherwise, return @code{nil}.
  @see-class{gdk:event}
  @see-class{gdk:event-sequence}"
  (event (g:boxed event)))

(export 'event-event-sequence)

;;; ----------------------------------------------------------------------------
;;; gdk_event_request_motions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_request_motions" event-request-motions) :void
 #+liber-documentation
 "@version{#2023-02-26}
  @argument[event]{a @class{gdk:event} instance}
  @begin{short}
    Request more motion notifies if the event is a motion notify hint event.
  @end{short}
  This function should be used instead of the @fun{gdk:window-pointer} function
  to request further motion notifies, because it also works for extension events
  where motion notifies are provided for devices other than the core pointer.

  Coordinate extraction, processing and requesting more motion events from a
  @code{GDK_MOTION_NOTIFY} event usually works like this:
  @begin{pre}
{
  /* motion_event handler */
  x = motion_event->x;
  y = motion_event->y;
  /* handle (x,y) motion */
  gdk_event_request_motions (motion_event); /* handles is_hint events */
@}
  @end{pre}
  @see-class{gdk:event}
  @see-function{gdk:window-pointer}"
  (event (g:boxed event)))

(export 'event-request-motions)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_angle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_get_angle" %events-angle) :boolean
  (event1 (g:boxed event))
  (event2 (g:boxed event))
  (angle (:pointer :double)))

(defun events-angle (event1 event2)
 "@version{#2025-10-09}
  @argument[event1]{a @class{gdk:event} instance}
  @argument[event2]{a @class{gdk:event} instance}
  @return{The double float for the relative angle between both events.}
  @begin{short}
    If both events contain X/Y information, this function will return the
    relative angle from @arg{event1} to @arg{event2}.
  @end{short}
  The rotation direction for positive angles is from the positive X axis
  towards the positive Y axis.
  @see-class{gdk:event}"
  (cffi:with-foreign-object (angle :double)
    (when (%events-angle event1 event2 angle)
      (cffi:mem-ref angle :double))))

(export 'events-angle)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_center
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_get_center" %events-center) :boolean
  (event1 (g:boxed event))
  (event2 (g:boxed event))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun events-center (event1 event2)
 "@version{#2025-10-09}
  @argument[event1]{a @class{gdk:event} instance}
  @argument[event2]{a @class{gdk:event} instance}
  @begin{return}
    @code{x} -- a double float for the x coordinate of the center @br{}
    @code{y} -- a double float for the y coordinate of the center
  @end{return}
  @begin{short}
    If both events contain X/Y information, the center of both coordinates will
    be returned in x and y.
  @end{short}
  @see-class{gdk:event}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%events-center event1 event2 x y)
     (values (cffi:mem-ref x :double)
             (cffi:mem-ref y :double)))))

(export 'events-center)

;;; ----------------------------------------------------------------------------
;;; gdk_events_get_distance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_events_get_distance" %events-distance) :boolean
  (event1 (g:boxed event))
  (event2 (g:boxed event))
  (distance (:pointer :double)))

(defun events-distance (event1 event2)
 "@version{#2025-10-09}
  @argument[event1]{a @class{gdk:event} instance}
  @argument[event2]{a @class{gdk:event} instance}
  @return{The double float for the distance.}
  @begin{short}
    If both events have X/Y information, the distance between both coordinates,
    will be returned, as in a straight line going from @code{event1} to
    @code{event2}.
  @end{short}
  @see-class{gdk:event}"
  (cffi:with-foreign-object (distance :double)
    (when (%events-distance event1 event2 distance)
      (cffi:mem-ref distance :double))))

(export 'events-distance)

;;; ----------------------------------------------------------------------------
;;; gdk_event_triggers_context_menu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_triggers_context_menu" event-triggers-context-menu)
    :boolean
 "@version{#2023-02-26}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if the event should trigger a contect menu.}
  @begin{short}
    This function returns whether a @class{gdk:event-button} instance should
    trigger a context menu, according to platform conventions.
  @end{short}
  The right mouse button always triggers context menus. Additionally, if
  the @fun{gdk:keymap-modifier-mask} function returns a non-0 mask for the
  @code{:context-menu} value of the @symbol{gdk:modifier-intent} enumeration,
  then the left mouse button will also trigger a context menu if this modifier
  is pressed.

  This function should always be used instead of simply checking for
  @code{event->button == GDK_BUTTON_SECONDARY}.
  @see-class{gdk:event}
  @see-class{gdk:event-button}
  @see-symbol{gdk:modifier-intent}
  @see-function{gdk:keymap-modifier-mask}"
  (event (g:boxed event)))

(export 'event-triggers-context-menu)

;; -----------------------------------------------------------------------------
;;; gdk_event_get_seat
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_seat" event-seat) (g:object gdk-seat)
 "@version{#2023-03-13}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gdk:seat} object of this event.}
  @begin{short}
    Returns the seat this event was generated for.
  @end{short}
  @see-class{gdk:event}
  @see-class{gdk:seat}"
  (event (g:boxed event)))

(export 'event-seat)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_scancode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_scancode" event-scancode) :int
 "@version{#2025-08-31}
  @argument[event]{a @class{gdk:event} instance}
  @return{The integer for the associated keyboard scancode or 0.}
  @begin{short}
    Gets the keyboard low-level scancode of a key event.
  @end{short}
  This is usually the @code{hardware-keycode} slot of the event. On Windows
  this is the high word of @code{WM_KEY{DOWN,UP@}} which contains the scancode
  and some extended flags.
  @see-class{gdk:event}"
  (event (g:boxed event)))

(export 'event-scancode)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_pointer_emulated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_get_pointer_emulated" event-pointer-emulated) :boolean
 "@version{#2023-03-13}
  @argument[event]{a @class{gdk:event} instance}
  @return{@em{True} if this event is emulated.}
  @begin{short}
    Returns whether this event is an 'emulated' pointer event (typically from a
    touch event), as opposed to a real one.
  @end{short}
  @see-class{gdk:event}"
  (event (g:boxed event)))

(export 'event-pointer-emulated)

;;; ----------------------------------------------------------------------------
;;; GdkEventFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback event-func :void
    ((event (g:boxed event))
     (data :pointer))
  (restart-case
      (funcall (glib:get-stable-pointer-value data) event)
    (return-from-callback () nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'event-func)
      "Callback"
      (liber:symbol-documentation 'event-func)
 "@version{#2025-07-03}
  @syntax{lambda (event)}
  @argument[event]{a @class{gdk:event} instance to process}
  @begin{short}
    Specifies the type of callback function passed to the
    @fun{gdk:event-handler-set} function to handle all GDK events.
  @end{short}
  @see-class{gdk:event}
  @see-function{gdk:event-handler-set}")

(export 'event-func)

;;; ----------------------------------------------------------------------------
;;; gdk_event_handler_set
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_event_handler_set" %event-handler-set) :void
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun event-handler-set (func)
 #+liber-documentation
 "@version{#2023-02-26}
  @argument[func]{a @symbol{gdk:event-func} callback function to call to handle
    events from GDK}
  @begin{short}
    Sets the callback function to call to handle all events from GDK.
  @end{short}
  Note that GTK uses this to install its own event handler, so it is usually
  not useful for GTK applications. Although an application can call this
  function then call the @fun{gtk:main-do-event} function to pass events to GTK.
  @see-class{gdk:event}
  @see-function{gtk:main-do-event}"
  (%event-handler-set (cffi:callback event-func)
                      (glib:allocate-stable-pointer func)
                      (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'event-handler-set)

;;; ----------------------------------------------------------------------------
;;; gdk_get_show_events
;;; gdk_set_show_events
;;; ----------------------------------------------------------------------------

(defun (setf show-events) (show-events)
  (cffi:foreign-funcall "gdk_set_show_events" :boolean show-events :void)
  show-events)

(cffi:defcfun ("gdk_get_show_events" show-events) :boolean
 #+liber-documentation
 "@version{#2023-02-26}
  @syntax{(gdk:show-events) => show-events}
  @syntax{(setf (gdk:show-events) show-events)}
  @argument[show-events]{@em{True} to output event debugging information.}
  @begin{short}
    Whether event debugging output is enabled.
  @end{short}
  The @fun{gdk:show-events} function gets whether event debugging output is
  enabled. The @setf{gdk:show-events} function sets whether a trace of received
  events is output.

  Note that GTK must be compiled with debugging (that is, configured using the
  @code{--enable-debug} option) to use this option.
  @see-class{gdk:event}")

(export 'show-events)

;;; ----------------------------------------------------------------------------
;;; gdk_event_set_screen
;;; gdk_event_get_screen
;;; ----------------------------------------------------------------------------

(defun (setf event-screen) (screen event)
  (cffi:foreign-funcall "gdk_event_set_screen"
                        (g:boxed event) event
                        (g:object screen) screen
                        :void)
  screen)

(cffi:defcfun ("gdk_event_get_screen" event-screen) (g:object screen)
 #+liber-documentation
 "@version{2025-10-09}
  @syntax{(gdk:event-screen event) => screen}
  @syntax{(setf (gdk:event-screen event) screen)}
  @argument[event]{a @class{gdk:event} instance}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Gets or sets the screen of an event.
  @end{short}
  The event must have been allocated by GTK, for instance, by the
  @fun{gdk:event-copy} function.

  The screen is typically the screen for @code{event->any.window}, but for
  events such as mouse events, it is the screen where the pointer was when the
  event occurs - that is, the screen which has the root window to which
  @code{event->motion.x_root} and @code{event->motion.y_root} are relative.
  @see-class{gdk:event}
  @see-class{gdk:screen}"
  (event (g:boxed event)))

(export 'event-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device
;;; gdk_event_set_device
;;; ----------------------------------------------------------------------------

(defun (setf event-device) (device event)
  (cffi:foreign-funcall "gdk_event_set_device"
                        (g:boxed event) event
                        (g:object device) device
                        :void)
  device)

(cffi:defcfun ("gdk_event_get_device" event-device) (g:object device)
 #+liber-documentation
 "@version{2025-10-09}
  @syntax{(gdk:event-device event) => device}
  @syntax{(setf (gdk:event-device event) device)}
  @argument[event]{a @class{gdk:event} instance}
  @argument[device]{a @class{gdk:device} object}
  @begin{short}
    Gets or sets the \"device\" field of an event.
  @end{short}
  If the event contains a \"device\" field, this function will return it, else
  it will return @code{nil}.

  The event must have been allocated by GTK, for instance, by the
  @fun{gdk:event-copy} function.
  @see-class{gdk:event}
  @see-class{gdk:device}"
  (event (g:boxed event)))

(export 'event-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_source_device
;;; gdk_event_set_source_device
;;; ----------------------------------------------------------------------------

(defun (setf event-source-device) (device event)
  (cffi:foreign-funcall "gdk_event_set_source_device"
                        (g:boxed event) event
                        (g:object device) device
                        :void)
  device)

(cffi:defcfun ("gdk_event_get_source_device" event-source-device) (g:object device)
 #+liber-documentation
 "@version{2025-10-09}
  @syntax{(gdk:event-source-device event) => device}
  @syntax{(setf (gdk:event-source-device event) device)}
  @argument[event]{a @class{gdk:event} instance}
  @argument[device]{a @class{gdk:device} object}
  @begin{short}
    Gets or sets the slave device for the event.
  @end{short}
  This function returns the hardware (slave) device that has triggered the
  event, falling back to the virtual (master) device, as in the
  @fun{gdk:event-device} function, if the event was not caused by interaction
  with a hardware device.

  This may happen for example in synthesized crossing events after a
  @class{gdk:window} object updates its geometry or a grab is acquired/released.

  If the event does not contain a device field, this function will return
  @code{nil}.

  The event must have been allocated by GTK, for instance by the
  @fun{gdk:event-copy} function.
  @see-class{gdk:event}
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-function{gdk:event-device}"
  (event (g:boxed event)))

(export 'event-source-device)

;;; ----------------------------------------------------------------------------
;;; gdk_event_get_device_tool
;;; gdk_event_set_device_tool
;;; ----------------------------------------------------------------------------

(defun (setf event-device-tool) (tool event)
  (cffi:foreign-funcall "gdk_event_set_device_tool"
                        (g:boxed event) event
                        (g:object device-tool) tool
                        :void)
  tool)

(cffi:defcfun ("gdk_event_get_device_tool" event-device-tool) (g:object device-tool)
 "@version{#2025-10-09}
  @syntax{(gdk:event-device-tool event) => tool}
  @syntax{(setf (gdk:event-device-tool event) tool)}
  @argument[event]{a @class{gdk:event} instance}
  @argument[tool]{a @class{gdk:device-tool} object}
  @begin{short}
    Gets or sets the device tool representing the tool that caused the event.
  @end{short}
  If the event was generated by a device that supports different tools, for
  example a tablet, this function will return a @class{gdk:device-tool} object
  representing the tool that caused the event. Otherwise, @code{nil} will be
  returned.
  @begin[Notes]{dictionary}
    The @class{gdk:device-tool} object will be constant during the application
    lifetime, if settings must be stored persistently across runs, see the
    @fun{gdk:device-tool-serial} function.
  @end{dictionary}
  @see-class{gdk:event}
  @see-class{gdk:device-tool}
  @see-function{gdk:device-tool-serial}"
  (event (g:boxed event)))

(export 'event-device-tool)

;;; ----------------------------------------------------------------------------
;;; gdk_setting_get
;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement a macro for using a gvalue.

(cffi:defcfun ("gdk_setting_get" %setting-get) :boolean
  (name :string)
  (value (:pointer (:struct g:value))))

(defun setting-get (name gtype)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[name]{a string for the name of the setting}
  @argument[gtype]{a string for the GType of the setting}
  @begin{return}
    The value of the setting.
  @end{return}
  @begin{short}
    Obtains a desktop-wide setting, such as the double-click time, for the
    default screen.
  @end{short}
  See the @fun{gdk:screen-setting} function.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:setting-get \"gtk-double-click-time\" \"gint\") => 400
    @end{pre}
  @end{dictionary}
  @see-function{gdk:screen-setting}"
  (cffi:with-foreign-object (value '(:struct g:value))
    (gobject:value-init value gtype)
    (when (%setting-get name value)
      (prog1
        (g:value-get value)
        (g:value-unset value)))))

(export 'setting-get)

;;; --- gdk3.events.lisp -------------------------------------------------------
