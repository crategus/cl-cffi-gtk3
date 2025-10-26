;;; ----------------------------------------------------------------------------
;;; gdk3.seat.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
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
;;; GdkSeat
;;;
;;;     Object representing an user seat.
;;;
;;; Types and Values
;;;
;;;     GdkSeat
;;;     GdkSeatCapabilities
;;;
;;; Functions
;;;
;;;     GdkSeatGrabPrepareFunc
;;;
;;;     gdk_seat_get_display                               Accessor
;;;     gdk_seat_grab
;;;     gdk_seat_ungrab
;;;     gdk_seat_get_capabilities
;;;     gdk_seat_get_pointer
;;;     gdk_seat_get_keyboard
;;;     gdk_seat_get_slaves
;;;
;;; Properties
;;;
;;;     display
;;;
;;; Signals
;;;
;;;     device-added
;;;     device-removed
;;;     tool-added
;;;     tool-removed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkSeat
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkSeatCapabilities
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GdkSeatCapabilities" seat-capabilities
  (:export t
   :type-initializer "gdk_seat_capabilities_get_type")
  (:none 0)
  (:pointer       #.(ash 1 0))
  (:touch         #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard      #.(ash 1 3))
  (:all-pointing 7)                    ; :pointer | :touch | :tablet-stylus
  (:all 15))                           ; :all-pointing | :keyboard

#+liber-documentation
(setf (liber:alias-for-symbol 'seat-capabilities)
      "GFlags"
      (liber:symbol-documentation 'seat-capabilities)
 "@version{#2025-07-01}
  @begin{declaration}
(gobject:define-gflags \"GdkSeatCapabilities\" seat-capabilities
  (:export t
   :type-initializer \"gdk_seat_capabilities_get_type\")
  (:none 0)
  (:pointer #.(ash 1 0))
  (:touch #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard #.(ash 1 3))
  (:all-pointing 7)
  (:all 15))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{No input capabilities.}
      @entry[:pointer]{The seat has a pointer, for example mouse.}
      @entry[:touch]{The seat has touchscreen(s) attached.}
      @entry[:tablet-stylus]{The seat has drawing tablet(s) attached.}
      @entry[:keyboard]{The seat has keyboard(s) attached.}
      @entry[:all-pointing]{The union of all pointing capabilities.}
      @entry[:all]{The union of all capabilities.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Flags describing the seat capabilities.
  @end{short}
  @see-class{gdk:seat}")

;;; ----------------------------------------------------------------------------
;;; GdkSeat
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkSeat" seat
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_seat_get_type")
  ((display
    seat-display
    "display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'seat 'type)
 "@version{#2025-06-29}
  @begin{short}
    The @class{gdk:seat} object represents a collection of input devices that
    belong to a user.
  @end{short}

  Since 3.20
  @begin[Signal Details]{dictionary}
    @begin[seat::device-added]{signal}
      @begin{pre}
lambda (seat device)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[seat]{The @class{gdk:seat} object on which the signal is
          emitted.}
        @entry[device]{The newly added @class{gdk:device} object.}
      @end{simple-table}
      The signal is emitted when a new input device is related to this seat.
    @end{signal}
    @begin[seat::device-removed]{signal}
      @begin{pre}
lambda (seat device)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[seat]{The @class{gdk:seat} object on which the signal is
          emitted.}
        @entry[device]{The just removed @class{gdk:device} object.}
      @end{simple-table}
      The signal is emitted when an input device is removed, for example
      unplugged.
    @end{signal}
    @begin[seat::tool-added]{signal}
      @begin{pre}
lambda (seat tool)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[seat]{The @class{gdk:seat} object on which the signal is
          emitted.}
        @entry[tool]{The new @class{gdk:device-tool} object known to the seat.}
      @end{simple-table}
      The signal is emitted whenever a new tool is made known to the seat. The
      tool may later be assigned to a device, i.e. on proximity with a tablet.
      The device will emit the @code{\"tool-changed\"} signal accordingly. A
      same tool may be used by several devices.
     @end{signal}
    @begin[seat::tool-removed]{signal}
      @begin{pre}
lambda (seat tool)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[seat]{The @class{gdk:seat} object on which the signal is
          emitted.}
        @entry[tool]{The just removed @class{gdk:device-tool} object.}
      @end{simple-table}
      The signal is emitted whenever a tool is no longer known to this seat.
      Since 3.22
    @end{signal}
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:seat-display -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'seat) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct) @br{}
  The display of this seat.")

#+liber-documentation
(setf (liber:alias-for-function 'seat-display)
      "Accessor"
      (documentation 'seat-display 'function)
 "@version{#2025-10-09}
  @syntax{(gdk:seat-display object) => display}
  @argument[object]{a @class{gdk:seat} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    The accessor for the @slot[gdk:seat]{display} slot of the
    @class{gdk:seat} class returns the display this seat belongs to.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; GdkSeatGrabPrepareFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback seat-grab-prepare-func :void
    ((seat (g:object seat))
     (window (g:object window))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func seat window)))

#+liber-documentation
(setf (liber:alias-for-symbol 'seat-grab-prepare-func)
      "Callback"
      (liber:symbol-documentation 'seat-grab-prepare-func)
 "@version{#2025-06-29}
  @syntax{lambda (seat window)}
  @argument[seat]{a @class{gdk:seat} object being grabbed}
  @argument[window]{a @class{gdk:window} object being grabbed}
  @begin{short}
    Type of the callback function used to set up @arg{window} so it can be
    grabbed.
  @end{short}
  A typical action would be ensuring the window is visible, although there is
  room for other initialization actions.
  @see-class{gdk:seat}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; gdk_seat_grab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_seat_grab" %seat-grab) grab-status
  (seat (g:object seat))
  (window (g:object window))
  (capabilities seat-capabilities)
  (owner-events :boolean)
  (cursor (g:object cursor))
  (event (g:boxed event))
  (func :pointer)
  (data :pointer))

(defun seat-grab (seat window capabilities owner-events cursor event func)
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[seat]{a @class{gdk:seat} object}
  @argument[window]{a @class{gdk:window} object that will own the grab}
  @argument[capabilities]{a @sym{gdk:seat-capabilities} value for the
    capabilities that will be grabbed}
  @argument[owner-events]{if @em{false} then all device events are reported
    with respect to @arg{window} and are only reported if selected by the event
    mask, if @em{true} then pointer events for this application are reported as
    normal, but pointer events outside this application are reported with
    respect to @arg{window} and only if selected by the event mask, in either
    mode, unreported events are discarded}
  @argument[cursor]{a @class{gdk:cursor} object to display while the grab is
    active, if this is @code{nil} then the normal cursors are used for
    @arg{window} and its descendants, and the cursor for @arg{window} is used
    elsewhere}
  @argument[event]{a @class{gdk:event} event that is triggering the grab, or
    @code{nil} if none is available}
  @argument[func]{a @sym{gdk:seat-grab-prepare-func} callback function to
    prepare the window to be grabbed, it can be @code{nil} if @arg{window} is
    visible before this call}
  @begin{return}
    The @val[gdk:grap-status]{:success} value of the @sym{gdk:grab-status}
    enumeration if the grab was successful.
  @end{return}
  @begin{short}
    Grabs the seat so that all events corresponding to the given capabilities
    are passed to this application until the seat is ungrabbed with the
    @fun{gdk:seat-ungrab} function, or the window becomes hidden.
  @end{short}
  This overrides any previous grab on the seat by this client.

  As a rule of thumb, if a grab is desired over
  @val[gdk:seat-capabilities]{:pointer}, all other \"pointing\" capabilities,
  for example @val[gdk:seat-capabilities]{:touch}, should be grabbed too, so the
  user is able to interact with all of those while the grab holds, you should
  thus use @val[gdk:seat-capabilities]{:all-pointing} most commonly.

  Grabs are used for operations which need complete control over the events
  corresponding to the given capabilities. For example in GTK this is used
  for drag and drop operations, popup menus and such.

  Note that if the event mask of a @class{gdk:window} object has selected both
  button press and button release events, or touch begin and touch end, then a
  press event will cause an automatic grab until the button is released,
  equivalent to a grab on the window with @arg{owner-events} set to @em{true}.
  This is done because most applications expect to receive paired press and
  release events.

  If you set up anything at the time you take the grab that needs to be cleaned
  up when the grab ends, you should handle the @class{gdk:event-grab-broken}
  events that are emitted when the grab ends unvoluntarily.
  @see-class{gdk:seat}
  @see-class{gdk:window}
  @see-class{gdk:cursor}
  @see-class{gdk:event}
  @see-class{gdk:event-grab-broken}
  @see-symbol{gdk:seat-capabilities}
  @see-symbol{gdk:grab-status}
  @see-function{gdk:seat-ungrab}"
  (if func
      (glib:with-stable-pointer (ptr func)
        (%seat-grab seat
                    window
                    capabilities
                    owner-events
                    cursor
                    event
                    (cffi:callback seat-grab-prepare-func)
                    ptr))
      (%seat-grab seat
                  window
                  capabilities
                  owner-events
                  cursor
                  event
                  (cffi:null-pointer)
                  (cffi:null-pointer))))

(export 'seat-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_ungrab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_seat_ungrab" seat-ungrab) :void
 #+liber-documentation
 "@version{#2023-03-13}
  @argument[seat]{a @class{gdk:seat} object}
  @short{Releases a grab added through the @fun{gdk:seat-grab} function.}
  @see-class{gdk:seat}
  @see-function{gdk:seat-grab}"
  (seat (g:object seat)))

(export 'seat-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_capabilities
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_seat_get_capabilities" seat-capabilities) seat-capabilities
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[seat]{a @class{gdk:seat} object}
  @return{The @sym{gdk:seat-capabilities} value for the seat capabilities.}
  @begin{short}
    Returns the capabilities this @class{gdk:seat} object currently has.
  @end{short}
  @see-class{gdk:seat}
  @see-symbol{gdk:seat-capabilities}"
  (seat (g:object seat)))

(export 'seat-capabilities)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_pointer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_seat_get_pointer" seat-pointer) (g:object device)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[seat]{a @class{gdk:seat} object}
  @return{The master @class{gdk:device} object for pointer capabilities.}
  @begin{short}
    Returns the master device that routes pointer events.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat)))

(export 'seat-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_keyboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_seat_get_keyboard" seat-keyboard) (g:object device)
 #+liber-documentation
 "@version{#2025-10-09}
  @argument[seat]{a @class{gdk:seat} object}
  @return{The master @class{gdk:device} object for keyboard capabilities.}
  @begin{short}
    Returns the master device that routes keyboard events.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat)))

(export 'seat-keyboard)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_slaves
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_seat_get_slaves" seat-slaves) (g:list-t (g:object device))
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[seat]{a @class{gdk:seat} object}
  @argument[capabilities]{a @sym{gdk:seat-capabilities} value for the
    capabilities to get devices for}
  @return{The list of @class{gdk:device} objects.}
  @begin{short}
    Returns the slave devices that match the given capabilities.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat))
  (capabilities seat-capabilities))

(export 'seat-slaves)

;;; --- End of file gdk3.seat.lisp ---------------------------------------------
