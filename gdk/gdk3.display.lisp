;;; ----------------------------------------------------------------------------
;;; gdk3.display.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GdkDisplay
;;;
;;;     Controls a set of GdkScreens and their associated input devices
;;;
;;; Types and Values
;;;
;;;     GdkDisplay
;;;
;;; Functions
;;;
;;;     gdk_display_open
;;;     gdk_display_get_default
;;;     gdk_display_get_name
;;;     gdk_display_get_n_screens                          deprecated
;;;     gdk_display_get_screen                             deprecated
;;;     gdk_display_get_default_screen
;;;     gdk_display_get_device_manager                     deprecated
;;;     gdk_display_pointer_ungrab                         deprecated
;;;     gdk_display_keyboard_ungrab                        deprecated
;;;     gdk_display_pointer_is_grabbed                     deprecated
;;;     gdk_display_device_is_grabbed
;;;     gdk_display_beep
;;;     gdk_display_sync
;;;     gdk_display_flush
;;;     gdk_display_close
;;;     gdk_display_is_closed
;;;     gdk_display_get_event
;;;     gdk_display_peek_event
;;;     gdk_display_put_event
;;;     gdk_display_has_pending
;;;     gdk_display_set_double_click_time
;;;     gdk_display_set_double_click_distance
;;;     gdk_display_get_pointer                            deprecated
;;;     gdk_display_list_devices                           deprecated
;;;     gdk_display_get_window_at_pointer                  deprecated
;;;     gdk_display_warp_pointer                           deprecated
;;;     gdk_display_supports_cursor_color
;;;     gdk_display_supports_cursor_alpha
;;;     gdk_display_get_default_cursor_size
;;;     gdk_display_get_maximal_cursor_size
;;;     gdk_display_get_default_group
;;;     gdk_display_supports_selection_notification
;;;     gdk_display_request_selection_notification
;;;     gdk_display_supports_clipboard_persistence
;;;     gdk_display_store_clipboard
;;;     gdk_display_supports_shapes
;;;     gdk_display_supports_input_shapes
;;;     gdk_display_supports_composite                     deprecated
;;;     gdk_display_get_app_launch_context
;;;     gdk_display_notify_startup_complete
;;;     gdk_display_get_default_seat
;;;     gdk_display_list_seats
;;;     gdk_display_get_n_monitors
;;;     gdk_display_get_monitor
;;;     gdk_display_get_primary_monitor
;;;     gdk_display_get_monitor_at_point
;;;     gdk_display_get_monitor_at_window
;;;
;;; Signals
;;;
;;;     closed
;;;     monitor-added
;;;     monitor-removed
;;;     opened
;;;     seat-added
;;;     seat-removed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDisplay
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplay
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDisplay" display
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_get_type")
  nil)

#+liber-documentation
(setf (documentation 'display 'type)
 "@version{2023-3-4}
  @begin{short}
    The @class{gdk:display} object purpose is two fold:
    @begin{itemize}
      @item{To manage and provide information about input devices (pointers and
        keyboards).}
      @item{To manage and provide information about the available
        @class{gdk:screen} objects.}
    @end{itemize}
  @end{short}
  The @class{gdk:display} object is the GDK representation of an X Display,
  which can be described as a workstation consisting of a keyboard, a pointing
  device (such as a mouse) and one or more screens. It is used to open and keep
  track of various @class{gdk:screen} objects currently instantiated by the
  application. It is also used to access the keyboard(s) and mouse pointer(s)
  of the display.

  Most of the input device handling has been factored out into the separate
  @class{gdk:device-manager} object. Every display has a device manager, which
  you can obtain using the @fun{gdk:display-device-manager} function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"closed\" signal}
      @begin{pre}
lambda (display is-error)    :run-last
      @end{pre}
      The signal is emitted when the connection to the windowing system for
      @arg{display} is closed.
      @begin[code]{table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
       @entry[is-error]{A boolean that is @em{true} if @arg{display} was closed
         due to an error.}
      @end{table}
    @subheading{The \"monitor-added\" signal}
      @begin{pre}
lambda (display monitor)    :run-last
      @end{pre}
      The signal is emitted whenever a monitor is added. Since 3.22
      @begin[code]{table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[monitor]{The @class{gdk:monitor} object that was just added.}
      @end{table}
    @subheading{The \"monitor-removed\" signal}
      @begin{pre}
lambda (display monitor)    :run-last
      @end{pre}
      The signal is emitted whenever a monitor is removed. Since 3.22
      @begin[code]{table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[monitor]{The @class{gdk:monitor} object that was just removed.}
      @end{table}
    @subheading{The \"opened\" signal}
      @begin{pre}
lambda (display)   :run-last
      @end{pre}
      The signal is emitted when the connection to the windowing system for
      @arg{display} is opened.
      @begin[code]{table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"seat-added\" signal}
      @begin{pre}
lambda (display seat)    :run-last
      @end{pre}
      The signal is emitted whenever a new seat is made known to the windowing
      system. Since 3.20
      @begin[code]{table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[seat]{The @class{gdk:seat} object that was just added.}
      @end{table}
    @subheading{The \"seat-removed\" signal}
      @begin{pre}
lambda (display seat)    :run-last
      @end{pre}
      The signal is emitted whenever a seat is removed by the windowing system.
      Since 3.20
      @begin[code]{table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[seat]{The @class{gdk:seat} object that was just removed.}
      @end{table}
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gdk:device-manager}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_open ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_open" display-open) (g:object display)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[name]{a string with the name of the display to open}
  @begin{return}
    A @class{gdk:display} object, or @code{nil} if the display could not be
    opened.
  @end{return}
  @short{Opens a display named by @arg{display-name}.}
  @see-class{gdk:display}"
  (name :string))

(export 'display-open)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default () -> display-default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default" display-default)
    (g:object display)
 #+liber-documentation
 "@version{2023-3-4}
  @begin{return}
    A @class{gdk:display} object, or @code{nil} if there is no default display.
  @end{return}
  @begin{short}
    Gets the default display.
  @end{short}
  This is a convenience function for the call
  @begin{pre}
(gdk:display-manager-default-display (gdk:display-manager-get))
  @end{pre}
  @see-class{gdk:display}
  @see-function{gdk:display-manager-get}
  @see-function{gdk:display-manager-default-display}")

(export 'display-default)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_name () -> display-name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_name" display-name) :string
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    A string representing the display name.
  @end{return}
  @short{Gets the name of the display.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_n_screens () -> display-n-screens
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_n_screens" display-n-screens) :int
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{An integer with the number of screens.}
  @short{Gets the number of screens managed by the display.}
  @begin[Warning]{dictionary}
    The @fun{gdk:display-n-screens} function has been deprecated since
    version 3.10 and should not be used in newly written code. The number of
    screens is always 1.
  @end{dictionary}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-n-screens)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_screen () -> display-screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_screen" display-screen) (g:object screen)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[num]{an integer with the screen number}
  @return{The @class{gdk:screen} object.}
  @short{Returns a screen object for one of the screens of the display.}
  @begin[Warning]{dictionary}
    The @fun{gdk:display-screen} function has been deprecated since version
    3.20 and should not be used in newly written code. There is only one
    screen. Use the @fun{gdk:display-default-screen} function to get it.
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:screen}
  @see-function{gdk:display-default-screen}"
  (display (g:object display))
  (num :int))

(export 'display-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_screen () -> display-default-screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default_screen" display-default-screen)
    (g:object screen)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{The default @class{gdk:screen} object for @arg{display}.}
  @short{Get the default screen for the display.}
  @see-class{gdk:display}
  @see-class{gdk:screen}"
  (display (g:object display)))

(export 'display-default-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_device_manager () -> display-device-manager
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_device_manager" display-device-manager)
    (g:object device-manager)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    A @class{gdk:device-manager} object, or @code{nil}.
  @end{return}
  @begin{short}
    Returns the device manager associated to the display.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:display-device-manager} function has been deprecated since
    version 3.20 and should not be used in newly written code. Use the
    @fun{gdk:display-default-seat} function and @class{gdk:seat} operations.
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:device-manager}
  @see-class{gdk:seat}
  @see-function{gdk:display-default-seat}"
  (display (g:object display)))

(export 'display-device-manager)

;;; ----------------------------------------------------------------------------
;;; gdk_display_pointer_ungrab ()
;;;
;;; void gdk_display_pointer_ungrab (GdkDisplay *display, guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_display_pointer_ungrab has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gdk_device_ungrab(), together with
;;; gdk_device_grab() instead.
;;;
;;; Release any pointer grab.
;;;
;;; display :
;;;     a GdkDisplay.
;;;
;;; time_ :
;;;     a timestap (e.g. GDK_CURRENT_TIME).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_keyboard_ungrab ()
;;;
;;; void gdk_display_keyboard_ungrab (GdkDisplay *display, guint32 time_);
;;;
;;; Warning
;;;
;;; gdk_display_keyboard_ungrab has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use gdk_device_ungrab(), together
;;; with gdk_device_grab() instead.
;;;
;;; Release any keyboard grab
;;;
;;; display :
;;;     a GdkDisplay.
;;;
;;; time_ :
;;;     a timestap (e.g GDK_CURRENT_TIME).
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_pointer_is_grabbed ()
;;;
;;; gboolean gdk_display_pointer_is_grabbed (GdkDisplay *display);
;;;
;;; Warning
;;;
;;; gdk_display_pointer_is_grabbed has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gdk_display_device_is_grabbed() instead.
;;;
;;; Test if the pointer is grabbed.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     TRUE if an active X pointer grab is in effect
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_device_is_grabbed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_device_is_grabbed" display-device-is-grabbed) :boolean
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[device]{a @class{gdk:device} object}
  @return{A boolean that is @em{true} if there is a grab in effect for
    @arg{device}.}
  @begin{short}
    Returns @em{true} if there is an ongoing grab on the device for the
    display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:device}"
  (display (g:object display))
  (device (g:object device)))

(export 'display-device-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_beep ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_beep" display-beep) :void
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @short{Emits a short beep on the display.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_display_sync ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_sync" display-sync) :void
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Flushes any requests queued for the windowing system and waits until all
    requests have been handled.
  @end{short}
  This is often used for making sure that the display is synchronized with the
  current state of the program. Calling the @fun{gdk:display-sync} function
  before the @code{gdk_error_trap_pop()} function makes sure that any errors
  generated from earlier requests are handled before the error trap is removed.

  This is most useful for X11. On windowing systems where requests are handled
  synchronously, this function will do nothing.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-sync)

;;; ----------------------------------------------------------------------------
;;; gdk_display_flush ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_flush" display-flush) :void
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Flushes any requests queued for the windowing system.
  @end{short}
  This happens automatically when the main loop blocks waiting for new events,
  but if your application is drawing without returning control to the main
  loop, you may need to call this function explicitely. A common case where
  this function needs to be called is when an application is executing drawing
  commands from a thread other than the thread where the main loop is running.

  This is most useful for X11. On windowing systems where requests are handled
  synchronously, this function will do nothing.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_display_close ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_close" display-close) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Closes the connection to the windowing system for the given @arg{display},
    and cleans up associated resources.
  @end{short}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-close)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_closed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_is_closed" display-is-closed) :boolean
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean that is @em{true} if @arg{display} is closed.}
  @short{Finds out if the display has been closed.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-is-closed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_event () -> display-event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_event" display-event) (g:boxed event :return)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    The next @class{gdk:event} event to be processed, or @code{nil} if no events
    are pending.
  @end{return}
  @begin{short}
    Gets the next event to be processed for the display, fetching events from
    the windowing system if necessary.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:event}
  @see-function{gdk:event-get}"
  (display (g:object display)))

(export 'display-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_peek_event ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_peek_event" display-peek-event)
    (g:boxed event :return)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    A copy of the first @class{gdk:event} event on the event queue, or
    @code{nil} if no events are in the queue.
  @end{return}
  @begin{short}
    Gets a copy of the first event in the event queue of the display, without
    removing the event from the queue.
  @end{short}
  Note that this function will not get more events from the windowing system.
  It only checks the events that have already been moved to the GDK event queue.
  @see-class{gdk:display}
  @see-class{gdk:event}
  @see-function{gdk:event-peek}"
  (display (g:object display)))

(export 'display-peek-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_put_event ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_put_event" display-put-event) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[event]{a @class{gdk:event} event}
  @begin{short}
    Appends a copy of the given event onto the front of the event queue for the
    display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:event}
  @see-function{gdk:event-put}"
  (display (g:object display))
  (event (g:boxed event)))

(export 'display-put-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_has_pending ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_has_pending" display-has-pending) :boolean
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean that is @em{true} if there are events ready to be
    processed.}
  @begin{short}
    Returns whether the display has events that are waiting to be processed.
  @end{short}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-has-pending)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_time ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_set_double_click_time"
               display-set-double-click-time) :void
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[msec]{an unsigned integer with the double click time in
    milliseconds}
  @begin{short}
    Sets the double click time.
  @end{short}
  Two clicks within this time interval count as a double click and result in a
  @code{:double-button-press} event. Applications should not set the double
  click time, it is a global user configured setting.
  @see-class{gdk:display}
  @see-function{gdk:display-set-double-click-distance}"
  (display (g:object display))
  (msec :uint))

(export 'display-set-double-click-time)

;;; ----------------------------------------------------------------------------
;;; gdk_display_set_double_click_distance ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_set_double_click_distance"
          display-set-double-click-distance) :void
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[distance]{an unsigned integer with the distance in pixels}
  @begin{short}
    Sets the double click distance.
  @end{short}
  Two clicks within this distance count as a double click and result in a
  @code{:double-button-press} event. See also the
  @fun{gdk:display-set-double-click-time} function. Applications should not set
  this, it is a global user-configured setting.
  @see-class{gdk:display}
  @see-function{gdk:display-set-double-click-time}"
  (display (g:object display))
  (distance :uint))

(export 'display-set-double-click-distance)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_pointer ()
;;;
;;; void gdk_display_get_pointer (GdkDisplay *display,
;;;                               GdkScreen **screen,
;;;                               gint *x,
;;;                               gint *y,
;;;                               GdkModifierType *mask);
;;;
;;; Warning
;;;
;;; gdk_display_get_pointer has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gdk_device_get_position() instead.
;;;
;;; Gets the current location of the pointer and the current modifier mask for
;;; a given display.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; screen :
;;;     location to store the screen that the cursor is on, or NULL
;;;
;;; x :
;;;     location to store root window X coordinate of pointer, or NULL
;;;
;;; y :
;;;     location to store root window Y coordinate of pointer, or NULL
;;;
;;; mask :
;;;     location to store current modifier mask, or NULL
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_list_devices ()
;;;
;;; GList * gdk_display_list_devices (GdkDisplay *display);
;;;
;;; Warning
;;;
;;; gdk_display_list_devices has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gdk_device_manager_list_devices()
;;; instead.
;;;
;;; Returns the list of available input devices attached to display. The list
;;; is statically allocated and should not be freed.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; Returns :
;;;     a list of GdkDevice
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_window_at_pointer ()
;;;
;;; GdkWindow * gdk_display_get_window_at_pointer (GdkDisplay *display,
;;;                                                gint *win_x,
;;;                                                gint *win_y);
;;;
;;; Warning
;;;
;;; gdk_display_get_window_at_pointer has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gdk_device_get_window_at_position() instead.
;;;
;;; Obtains the window underneath the mouse pointer, returning the location of
;;; the pointer in that window in win_x, win_y for screen. Returns NULL if the
;;; window under the mouse pointer is not known to GDK (for example, belongs to
;;; another application).
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; win_x :
;;;     return location for x coordinate of the pointer location relative to
;;;     the window origin, or NULL
;;;
;;; win_y :
;;;     return location for y coordinate of the pointer location relative & to
;;;     the window origin, or NULL
;;;
;;; Returns :
;;;     the window under the mouse pointer, or NULL
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_warp_pointer ()
;;;
;;; void gdk_display_warp_pointer (GdkDisplay *display,
;;;                                GdkScreen *screen,
;;;                                gint x,
;;;                                gint y);
;;;
;;; Warning
;;;
;;; gdk_display_warp_pointer has been deprecated since version 3.0 and should
;;; not be used in newly written code. Use gdk_device_warp() instead.
;;;
;;; Warps the pointer of display to the point x,y on the screen screen, unless
;;; the pointer is confined to a window by a grab, in which case it will be
;;; moved as far as allowed by the grab. Warping the pointer creates events as
;;; if the user had moved the mouse instantaneously to the destination.
;;;
;;; Note that the pointer should normally be under the control of the user.
;;; This function was added to cover some rare use cases like keyboard
;;; navigation support for the color picker in the GtkColorSelectionDialog.
;;;
;;; display :
;;;     a GdkDisplay
;;;
;;; screen :
;;;     the screen of display to warp the pointer to
;;;
;;; x :
;;;     the x coordinate of the destination
;;;
;;; y :
;;;     the y coordinate of the destination
;;;
;;; Since 2.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_cursor_color"
               display-supports-cursor-color) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean whether cursors can have multiple colors.}
  @begin{short}
    Returns @em{true} if multicolored cursors are supported on the display.
  @end{short}
  Otherwise, cursors have only a forground and a background color.
  @see-class{gdk:display}
  @see-function{gdk:display-supports-cursor-alpha}"
  (display (g:object display)))

(export 'display-supports-cursor-color)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_cursor_alpha ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_cursor_alpha"
               display-supports-cursor-alpha) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean whether cursors can have alpha channels.}
  @begin{short}
    Returns @em{true} if cursors can use an 8 bit alpha channel on the display.
  @end{short}
  Otherwise, cursors are restricted to bilevel alpha, i.e. a mask.
  @see-class{gdk:display}
  @see-function{gdk:display-supports-cursor-color}"
  (display (g:object display)))

(export 'display-supports-cursor-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_cursor_size () -> display-default-cursor-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default_cursor_size"
               display-default-cursor-size) :uint
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{An unsigned integer with the default cursor size.}
  @short{Returns the default size to use for cursors on the display.}
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:display-default-cursor-size (gdk:display-default)) => 24
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}
  @see-function{gdk:display-maximal-cursor-size}"
  (display (g:object display)))

(export 'display-default-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_maximal_cursor_size () -> display-maximal-cursor-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_maximal_cursor_size"
               %display-maximal-cursor-size) :void
  (display (g:object display))
  (width (:pointer :uint))
  (height (:pointer :uint)))

(defun display-maximal-cursor-size (display)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    @arg{width} -- an unsigned integer with the maximal cursor width @br{}
    @arg{height} -- an unsigned integer with the maximal cursor height
  @end{return}
  @short{Gets the maximal size to use for cursors on the display.}
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:display-maximal-cursor-size (gdk:display-default))
=> 128
=> 128
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}
  @see-function{gdk:display-default-cursor-size}"
  (cffi:with-foreign-objects ((width :uint) (height :uint))
    (%display-maximal-cursor-size display width height)
    (values (cffi:mem-ref width :uint)
            (cffi:mem-ref height :uint))))

(export 'display-maximal-cursor-size)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_group () -> display-default-group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default_group" display-default-group)
    (g:object window)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{The default group leader @class{gdk:window} object for @arg{display}.}
  @begin{short}
    Returns the default group leader window for all toplevel windows on the
    display.
  @end{short}
  This window is implicitly created by GDK. See the @fun{gdk:window-group}
  function.
  @see-class{gdk:display}
  @see-class{gdk:window}
  @see-function{gdk:window-group}"
  (display (g:object display)))

(export 'display-default-group)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_selection_notification ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_selection_notification"
               display-supports-selection-notification) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean whether @class{gdk:event-owner-change} events will be sent.}
  @begin{short}
    Returns whether @class{gdk:event-owner-change} events will be sent when the
    owner of a selection changes.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:event-owner-change}
  @see-function{gdk:display-request-selection-notification}"
  (display (g:object display)))

(export 'display-supports-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_request_selection_notification ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_request_selection_notification"
               display-request-selection-notification) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[selection]{an atom as a string naming the selection for which
    ownership change notification is requested}
  @return{A boolean whether @class{gdk:event-owner-change} events will be sent.}
  @begin{short}
    Request @class{gdk:event-owner-change} events for ownership changes of the
    selection named by the given atom.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:event-owner-change}
  @see-function{gdk:display-supports-selection-notification}"
  (display (g:object display))
  (selection atom-as-string))

(export 'display-request-selection-notification)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_clipboard_persistence ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_clipboard_persistence"
               display-supports-clipboard-persistence) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean that is @em{true} if @arg{display} supports clipboard
    persistance.}
  @begin{short}
    Returns whether the specified display supports clipboard persistance.
  @end{short}
  I.e. if it is possible to store the clipboard data after an application has
  quit. On X11 this checks if a clipboard daemon is running.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-supports-clipboard-persistence)

;;; ----------------------------------------------------------------------------
;;; gdk_display_store_clipboard ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_store_clipboard" %display-store-clipboard) :void
  (display (g:object display))
  (clipboard-window (g:object window))
  (time :uint32)
  (targets :pointer)
  (n-targets :int))

(defun display-store-clipboard (display window time targets)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[window]{a @class{gdk:window} object belonging to the clipboard
    owner}
  @argument[time]{an unsigned integer with the timestamp}
  @argument[targets]{a list of atoms as strings with the targets that should
    be saved, or @code{nil} if all available targets should be saved}
  @begin{short}
    Issues a request to the clipboard manager to store the clipboard data.
  @end{short}
  On X11, this is a special program that works according to the freedesktop
  clipboard specification, available at
  @a[http://www.freedesktop.org/wiki/ClipboardManager]{freedesktop.org}.
  @see-class{gdk:display}
  @see-class{gdk:window}"
  (let ((n-targets (length targets)))
    (cffi:with-foreign-object (targets-ptr 'atom-as-string n-targets)
      (loop for str in targets
            for i from 0
            do (setf (cffi:mem-aref targets-ptr 'atom-as-string i) str))
      (%display-store-clipboard display
                                window
                                time
                                targets-ptr
                                n-targets))))

(export 'display-store-clipboard)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_shapes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_shapes" display-supports-shapes) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean that is @em{true} if shaped windows are supported.}
  @begin{short}
    Returns @em{true} if the @fun{gdk:window-shape-combine-region} function can
    be used to create shaped windows on the display.
  @end{short}
  @see-class{gdk:display}
  @see-function{gdk:window-shape-combine-region}"
  (display (g:object display)))

(export 'display-supports-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_input_shapes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_input_shapes"
               display-supports-input-shapes) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean that is @em{true} if windows with modified input shape are
    supported.}
  @begin{short}
    Returns @em{true} if the @fun{gdk:window-input-shape-combine-region}
    function can be used to modify the input shape of windows on the display.
  @end{short}
  @see-class{gdk:display}
  @see-function{gdk:window-input-shape-combine-region}"
  (display (g:object display)))

(export 'display-supports-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_composite ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_composite" display-supports-composite)
    :boolean
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{A boolean that is @em{true} if windows may be composited.}
  @begin{short}
    Returns @em{true} if the @fun{gdk:window-composited} function can be used
    to redirect drawing on the window using compositing.
  @end{short}
  Currently this only works on X11 with XComposite and XDamage extensions
  available.
  @begin[Warning]{dictionary}
    The @fun{gdk:display-supports-composite} function has been deprecated since
    version 3.16 and should not be used in newly written code. Compositing is
    an outdated technology that only ever worked on X11.
  @end{dictionary}
  @see-class{gdk:display}
  @see-function{gdk:window-composited}"
  (display (g:object display)))

(export 'display-supports-composite)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_app_launch_context () -> display-app-launch-context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_app_launch_context"
               display-app-launch-context) (g:object app-launch-context)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    A new @class{gdk:app-launch-context} object for @arg{display}.
  @end{return}
  @begin{short}
    Returns a @class{gdk:app-launch-context} object suitable for launching
    applications on the given display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:app-launch-context}"
  (display (g:object display)))

(export 'display-app-launch-context)

;;; ----------------------------------------------------------------------------
;;; gdk_display_notify_startup_complete ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_notify_startup_complete"
               display-notify-startup-complete) :void
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @argument[startup]{a string with a startup notification identifier, for which
    notification process should be completed}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading,
    using a given identifier.
  @end{short}
  GTK will call this function automatically for @class{gtk:window} widgets
  with a custom startup notification identifier unless the
  @fun{gtk:window-set-auto-startup-notification} function is called to disable
  that feature.
  @see-class{gdk:display}
  @see-class{gtk:window}
  @see-function{gtk:window-set-auto-startup-notification}"
  (display (g:object display))
  (startup :string))

(export 'display-notify-startup-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_seat () -> display-default-seat
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default_seat" display-default-seat)
    (g:object seat)
 #+liber-documentation
 "@version{2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @return{The default @class{gdk:seat} object.}
  @begin{short}
    Returns the default seat object for this display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:seat}"
  (display (g:object display)))

(export 'display-default-seat)

;;; ----------------------------------------------------------------------------
;;; gdk_display_list_seats ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_list_seats" display-list-seats)
    (g:list-t (g:object seat))
 #+liber-documentation
 "@version{2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @return{The list of @class{gdk:seat} objects known to @arg{display}.}
  @begin{short}
    Returns the list of seats known to @arg{display}.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:seat}"
  (display (g:object display)))

(export 'display-list-seats)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_n_monitors () -> display-n-monitors
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_n_monitors" display-n-monitors) :int
 #+liber-documentation
 "@version{2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @return{An integer with the number of monitors.}
  @begin{short}
    Gets the number of monitors that belong to display .
  @end{short}
  The returned number is valid until the next emission of the
  @code{\"monitor-added\"} or @code{\"monitor-removed\"} signal.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-n-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_monitor () -> display-monitor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_monitor" display-monitor) (g:object monitor)
 #+liber-documentation
 "@version{2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @argument[monitor-num]{an integer with the number of the monitor}
  @return{The @class{gdk:monitor} object, or @code{nil} if the
  @code{monitor-num} arugment is not a valid monitor number.}
  @begin{short}
    Gets a monitor associated with this display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:monitor}"
  (display (g:object display))
  (monitor-num :int))

(export 'display-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_primary_monitor () -> display-primary-monitor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_primary_monitor" display-primary-monitor)
    (g:object monitor)
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @return{The primary @class{gdk:monitor} object, or @code{nil} if no primary
    monitor is configured by the user.}
  @begin{short}
    Gets the primary monitor for the display.
  @end{short}
  The primary monitor is considered the monitor where the \"main desktop\"
  lives. While normal application windows typically allow the window manager
  to place the windows, specialized desktop applications such as panels should
  place themselves on the primary monitor.
  @see-class{gdk:display}
  @see-class{gdk:monitor}"
  (display (g:object display)))

(export 'display-primary-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_monitor_at_point () -> display-monitor-at-point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_monitor_at_point" display-monitor-at-point)
    (g:object monitor)
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @argument[x]{an integer with the x coordinate of the point}
  @argument[y]{an integer with the y coordinate of the point}
  @return{The @class{gdk:monitor} object containing the point
    (@arg{x}, @arg{y}).}
  @begin{short}
    Gets the monitor in which the point (@arg{x}, @arg{y}) is located, or a
    nearby monitor if the point is not in any monitor.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:monitor}"
  (display (g:object display))
  (x :int)
  (y :int))

(export 'display-monitor-at-point)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_monitor_at_window () -> display-monitor-at-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_monitor_at_window" display-monitor-at-window)
    (g:object monitor)
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[display]{a @class{gdk:display} object}
  @argument[window]{a @class{gdk:window} object}
  @return{The @class{gdk:monitor} object with the largest overlap with
    @arg{window}.}
  @begin{short}
    Gets the monitor in which the largest area of window resides, or a monitor
    close to the window if it is outside of all monitors.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:window}
  @see-class{gdk:monitor}"
  (display (g:object display))
  (window (g:object window)))

(export 'display-monitor-at-window)

;;; --- End of file gdk3.display.lisp ------------------------------------------
