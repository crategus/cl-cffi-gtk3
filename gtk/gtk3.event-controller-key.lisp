;;; ----------------------------------------------------------------------------
;;; gtk3.event-controller-key.lisp
;;;
;;; The documentation in this file is taken from the GTK Reference Manual
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
;;; GtkEventControllerKey
;;;
;;;     Event controller for key events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerKey
;;;
;;; Functions
;;;
;;;     gtk_event_controller_key_new
;;;
;;; Signals
;;;
;;;     focus-in
;;;     focus-out
;;;     im-update
;;;     key-pressed
;;;     key-released
;;;     modifiers
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerKey
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventControllerKey
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEventControllerKey" event-controller-key
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_key_get_type")
  nil)

#+liber-documentation
(setf (documentation 'event-controller-key 'type)
 "@version{#2025-09-26}
  @begin{short}
    The @class{gtk:event-controller-key} object is an event controller meant for
    situations where you need access to key events.
  @end{short}
  @begin[Signal Details]{dictionary}
    @begin[event-controller-key::focus-in]{signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-key} object on which
          the signal is emitted.}
      @end{simple-table}
    @end{signal}
    @begin[event-controller-key::focus-out]{signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-key} object on
          which the signal is emitted.}
      @end{simple-table}
    @end{signal}
    @begin[event-controller-key::im-update]{signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-key} object on
          which the signal is emitted.}
      @end{simple-table}
    @end{signal}
    @begin[event-controller-key::key-pressed]{signal}
      @begin{pre}
lambda (controller keyval keycode state)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-key} object on
          which received the signal.}
        @entry[keyval]{The unsigned integer for the pressed key.}
        @entry[keycode]{The unsigned integer for the raw code of the pressed
          key.}
        @entry[state]{The @sym{gdk:modifier-type} bitmask representing the
          state of modifier keys and pointer buttons.}
        @entry[Returns]{@em{True} if the key press was handled, @em{false}
          otherwise.}
      @end{simple-table}
      The signal is emitted whenever a key is pressed.
    @end{signal}
    @begin[event-controller-key::key-released]{signal}
      @begin{pre}
lambda (controller keyval keycode state)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-key} object on
          which received the signal.}
        @entry[keyval]{The unsigned integer for the released key.}
        @entry[keycode]{The unsigned integer for the raw code of the released
          key.}
        @entry[state]{The @sym{gdk:modifier-type} bitmask representing the
          state of modifier keys and pointer buttons.}
      @end{simple-table}
      The signal is emitted whenever a key is released.
    @end{signal}
    @begin[event-controller-key::modifiers]{signal}
      @begin{pre}
lambda (controller state)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-key} object on
          which received the signal.}
        @entry[state]{The @sym{gdk:modifier-type} bitmask, representing the
          state of modifier keys and pointer buttons.}
        @entry[Returns]{a not documented boolean}
      @end{simple-table}
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:event-controller-key-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_key_new
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-key-new))

(defun event-controller-key-new ()
 #+liber-documentation
 "@version{#2023-03-01}
  @return{The new @class{gtk:event-controller-key} object.}
  @short{Creates a new event controller.}
  @see-class{gtk:event-controller-key}"
  (make-instance 'event-controller-key))

(export 'event-controller-key-new)

;;; --- End of File gtk3.event-controller-key.lisp -----------------------------
