;;; ----------------------------------------------------------------------------
;;; gdk3.seat.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; enum GdkSeatCapabilities
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkSeatCapabilities" seat-capabilities
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
 "@version{#2023-3-7}
  @begin{short}
    Flags describing the seat capabilities.
  @end{short}
  @begin{pre}
(define-g-flags \"GdkSeatCapabilities\" seat-capabilities
  (:export t
   :type-initializer \"gdk_seat_capabilities_get_type\")
  (:none 0)
  (:pointer #.(ash 1 0))
  (:touch #.(ash 1 1))
  (:tablet-stylus #.(ash 1 2))
  (:keyboard #.(ash 1 3))
  (:all-pointing 7)
  (:all 15))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No input capabilities.}
    @entry[:pointer]{The seat has a pointer, e.g. mouse.}
    @entry[:touch]{The seat has touchscreen(s) attached.}
    @entry[:tablet-stylus]{The seat has drawing tablet(s) attached.}
    @entry[:keyboard]{The seat has keyboard(s) attached.}
    @entry[:all-pointing]{The union of all pointing capabilities.}
    @entry[:all]{The union of all capabilities.}
  @end{table}
  @see-class{gdk:seat}")

;;; ----------------------------------------------------------------------------
;;; struct GdkSeat
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkSeat" seat
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_seat_get_type")
  ((display
    seat-display
    "display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'seat 'type)
 "@version{#2023-3-7}
  @begin{short}
    The @sym{gdk:seat} object represents a collection of input devices that
    belong to a user.
  @end{short}

  Since 3.20
  @begin[Signal Details]{dictionary}
    @subheading{The \"device-added\" signal}
      @begin{pre}
lambda (seat device)    :run-last
      @end{pre}
      The signal is emitted when a new input device is related to this seat.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[device]{The newly added @class{gdk:device} object.}
      @end{table}
    @subheading{The \"device-removed\" signal}
      @begin{pre}
lambda (seat device)    :run-last
      @end{pre}
      The signal is emitted when an input device is removed, e.g. unplugged.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[device]{The just removed @class{gdk:device} object.}
      @end{table}
    @subheading{The \"tool-added\" signal}
      @begin{pre}
lambda (seat tool)    :run-last
      @end{pre}
      The signal is emitted whenever a new tool is made known to the seat. The
      tool may later be assigned to a device, i.e. on proximity with a tablet.
      The device will emit the \"tool-changed\" signal accordingly. A same tool
      may be used by several devices.
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[tool]{The new @class{gdk:device-tool} object known to the seat.}
      @end{table}
    @subheading{The \"tool-removed\" signal}
      @begin{pre}
lambda (seat tool)    :run-last
      @end{pre}
      The signal is emitted whenever a tool is no longer known to this seat.
      Since 3.22
      @begin[code]{table}
        @entry[seat]{The @sym{gdk:seat} object on which the signal is emitted.}
        @entry[tool]{The just removed @class{gdk:device-tool} object.}
      @end{table}
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- seat-display -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'seat) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct) @br{}
  The display of this seat.")

#+liber-documentation
(setf (liber:alias-for-function 'seat-display)
      "Accessor"
      (documentation 'seat-display 'function)
 "@version{#2023-3-7}
  @syntax[]{(gdk:seat-display object) => display}
  @argument[object]{a @class{gdk:seat} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:seat]{display} slot of the
    @class{gdk:seat} class.
  @end{short}
  The @sym{gdk:seat-display} function returns the display this seat belongs to.
  @see-class{gdk:seat}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; GdkSeatGrabPrepareFunc ()
;;; ----------------------------------------------------------------------------

(defcallback seat-grab-prepare-func :void
    ((seat (g:object seat))
     (window (g:object window))
     (data :pointer))
  (funcall (glib:get-stable-pointer-value data) seat window))

#+liber-documentation
(setf (liber:alias-for-symbol 'seat-grab-prepare-func)
      "Callback"
      (liber:symbol-documentation 'seat-grab-prepare-func)
 "@version{#2023-3-13}
  @begin{short}
    Type of the callback function used to set up @arg{window} so it can be
    grabbed.
  @end{short}
  A typical action would be ensuring the window is visible, although there is
  room for other initialization actions.
  @begin{pre}
lambda (seat window)
  @end{pre}
  @begin[code]{table}
    @entry[seat]{The @class{gdk:seat} object being grabbed.}
    @entry[window]{The @class{gdk:window} object being grabbed.}
  @end{table}
  @see-class{gdk:seat}")

;;; ----------------------------------------------------------------------------
;;; gdk_seat_grab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_grab" %seat-grab) grab-status
  (seat (g:object seat))
  (window (g:object window))
  (capabilities seat-capabilities)
  (owner-events :boolean)
  (cursor (g:object cursor))
  (event (g:boxed event))
  (prepare-func :pointer)
  (prepare-func-data :pointer))

(defun seat-grab (seat window capabilities owner-events cursor event func)
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[seat]{a @class{gdk:seat} object}
  @argument[window]{a @class{gdk:window} object which will own the grab}
  @argument[capabilities]{a @symbol{gdk:seat-capabilities} value with the
    capabilities that will be grabbed}
  @argument[owner-events]{if @em{false} then all device events are reported
    with respect to @arg{window} and are only reported if selected by the event
    mask, if @em{true} then pointer events for this application are reported as
    normal, but pointer events outside this application are reported with
    respect to @arg{window} and only if selected by the event mask. In either
    mode, unreported events are discarded}
  @argument[cursor]{a @class{gdk:cursor} object to display while the grab is
    active, if this is @code{nil} then the normal cursors are used for
    @arg{window} and its descendants, and the cursor for @arg{window} is used
    elsewhere}
  @argument[event]{a @class{gdk:event} event that is triggering the grab, or
    @code{nil} if none is available}
  @argument[func]{a @symbol{gdk:seat-grab-prepare-func} callback function to
    prepare the window to be grabbed, it can be @code{nil} if @arg{window} is
    visible before this call}
  @return{The @code{:success} value of the @symbol{gdk:grab-status} enumeration
    if the grab was successful.}
  @begin{short}
    Grabs the seat so that all events corresponding to the given capabilities
    are passed to this application until the seat is ungrabbed with the
    @fun{gdk:seat-ungrab} function, or the window becomes hidden.
  @end{short}
  This overrides any previous grab on the seat by this client.

  As a rule of thumb, if a grab is desired over @code{:pointer}, all other
  \"pointing\" capabilities, e.g. @code{:touch}, should be grabbed too, so the
  user is able to interact with all of those while the grab holds, you should
  thus use @code{:all-pointing} most commonly.

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
      (with-stable-pointer (ptr func)
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
;;; gdk_seat_ungrab ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_ungrab" seat-ungrab) :void
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[seat]{a @class{gdk:seat} object}
  @short{Releases a grab added through the @fun{gdk:seat-grab} function.}
  @see-class{gdk:seat}
  @see-function{gdk:seat-grab}"
  (seat (g:object seat)))

(export 'seat-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_capabilities () -> seat-capabilities
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_capabilities" seat-capabilities) seat-capabilities
 #+liber-documentation
 "@version{#2023-3-7}
  @argument[seat]{a @class{gdk:seat} object}
  @return{The @symbol{gdk:seat-capabilities} value with the seat capabilities.}
  @begin{short}
    Returns the capabilities this @class{gdk:seat} object currently has.
  @end{short}
  @see-class{gdk:seat}
  @see-symbol{gdk:seat-capabilities}"
  (seat (g:object seat)))

(export 'seat-capabilities)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_pointer () -> seat-pointer
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_pointer" seat-pointer) (g:object device)
 #+liber-documentation
 "@version{#2023-3-7}
  @argument[seat]{a @class{gdk:seat} object}
  @return{A master @class{gdk:device} object with pointer capabilities.}
  @begin{short}
    Returns the master device that routes pointer events.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat)))

(export 'seat-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_keyboard () -> seat-keyboard
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_keyboard" seat-keyboard) (g:object device)
 #+liber-documentation
 "@version{#2023-3-7}
  @argument[seat]{a @class{gdk:seat} object}
  @return{A master @class{gdk:device} object with keyboard capabilities.}
  @begin{short}
    Returns the master device that routes keyboard events.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat)))

(export 'seat-keyboard)

;;; ----------------------------------------------------------------------------
;;; gdk_seat_get_slaves () -> seat-slaves
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_seat_get_slaves" seat-slaves) (g:list-t (g:object device))
 #+liber-documentation
 "@version{#2023-3-7}
  @argument[seat]{a @class{gdk:seat} object}
  @argument[capabilities]{a @symbol{gdk:seat-capabilities} value with the
    capabilities to get devices for}
  @return{A list of @class{gdk:device} objects.}
  @begin{short}
    Returns the slave devices that match the given capabilities.
  @end{short}
  @see-class{gdk:seat}
  @see-class{gdk:device}"
  (seat (g:object seat))
  (capabilities seat-capabilities))

(export 'seat-slaves)

;;; --- End of file gdk3.seat.lisp ---------------------------------------------
