;;; ----------------------------------------------------------------------------
;;; gdk3.event-structures.lisp
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
;;; Event Structures
;;;
;;;     Data structures specific to each type of event
;;;
;;; Types and Values
;;;
;;;     GdkScrollDirection
;;;     GdkVisibilityState
;;;     GdkCrossingMode
;;;     GdkNotifyType
;;;     GdkPropertyState
;;;     GdkWindowState
;;;     GdkSettingAction
;;;     GdkOwnerChange
;;;
;;;     GdkEventType        <-- gdk.events.lisp
;;;     GdkModifierType     <-- gdk.window.lisp
;;;     GdkEventMask        <-- gdk.events.lisp
;;;     GdkEventSequence    <-- gdk-events.lisp
;;;
;;;     GdkEvent
;;;     GdkEventAny
;;;     GdkEventKey
;;;     GdkEventButton
;;;     GdkEventTouch
;;;     GdkEventScroll
;;;     GdkEventMotion
;;;     GdkEventExpose
;;;     GdkEventVisibility
;;;     GdkEventCrossing
;;;     GdkEventFocus
;;;     GdkEventConfigure
;;;     GdkEventProperty
;;;     GdkEventSelection
;;;     GdkEventDND
;;;     GdkEventProximity
;;;     GdkEventWindowState
;;;     GdkEventSetting
;;;     GdkEventOwnerChange
;;;     GdkEventGrabBroken
;;;     GdkEventTouchpadSwipe
;;;     GdkEventTouchpadPinch
;;;     GdkEventPadButton
;;;     GdkEventPadAxis
;;;     GdkEventPadGroupMode
;;;
;;; Description
;;;
;;; The event structs contain data specific to each type of event in GDK.
;;;
;;; Note
;;;
;;; A common mistake is to forget to set the event mask of a widget so that the
;;; required events are received. See gtk_widget_set_events().
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;; "CFFI foreign type for an array of a fixed length. Slot element-type
;; specifies the type of elements and array-size specifies the size of array
;; (in elements).

(cffi:define-foreign-type fixed-array ()
  ((element-type :reader fixed-array-element-type
                 :initarg :element-type
                 :initform (error "Element type must be specified"))
   (array-size :reader fixed-array-array-size
               :initarg :array-size
               :initform (error "Array size must be specified")))
  (:actual-type :pointer)
  (:documentation ""))

(cffi:define-parse-method fixed-array (element-type array-size)
  (make-instance 'fixed-array
                 :element-type element-type
                 :array-size array-size))

(defmethod cffi:translate-from-foreign (ptr (type fixed-array))
  (when (not (cffi:null-pointer-p ptr))
    (let ((result (make-array (fixed-array-array-size type)))
          (el-type (fixed-array-element-type type)))
      (loop
         for i from 0 below (fixed-array-array-size type)
         do (setf (aref result i) (cffi:mem-aref ptr el-type i)))
      result)))

(defmethod cffi:translate-to-foreign (value (type fixed-array))
  (if (null value)
      (cffi:null-pointer)
      (cffi:foreign-alloc (fixed-array-element-type type)
                          :count (length value)
                          :initial-contents value)))

(defmethod cffi:free-translated-object (value (type fixed-array) param)
  (declare (ignore param))
  (unless (cffi:null-pointer-p value)
    (cffi:foreign-free value)))

;;; ----------------------------------------------------------------------------
;;; enum GdkScrollDirection
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkScrollDirection" scroll-direction
  (:export t
   :type-initializer "gdk_scroll_direction_get_type")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'scroll-direction)
      "GEnum"
      (liber:symbol-documentation 'scroll-direction)
 "@version{#2021-12-13}
  @short{Specifies the direction for a @class{gdk:event-scroll} event.}
  @begin{pre}
(gobject:define-g-enum \"GdkScrollDirection\" scroll-direction
  (:export t
   :type-initializer \"gdk_scroll_direction_get_type\")
  (:up 0)
  (:down 1)
  (:left 2)
  (:right 3)
  (:smooth 4))
  @end{pre}
  @begin[code]{table}
    @entry[:up]{The window is scrolled up.}
    @entry[:down]{The window is scrolled down.}
    @entry[:left]{The window is scrolled to the left.}
    @entry[:right]{The window is scrolled to the right.}
    @entry[:smooth]{The scrolling is determined by the delta values in the
      @class{gdk:event-scroll} event. See the @fun{gdk:event-scroll-deltas}
      function.}
  @end{table}
  @see-class{gdk:event-scroll}
  @see-function{gdk:event-scroll-deltas}")

;;; ----------------------------------------------------------------------------
;;; enum GdkVisibilityState
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkVisibilityState" visibility-state
  (:export t
   :type-initializer "gdk_visibility_state_get_type")
  (:unobscured 0)
  (:partial 1)
  (:fully-obscured 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'visibility-state)
      "GEnum"
      (liber:symbol-documentation 'visibility-state)
 "@version{#2021-12-13}
  @begin{short}
    Specifies the visiblity status of a window for a
    @class{gdk:event-visibility} event.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkVisibilityState\" visibility-state
  (:export t
   :type-initializer \"gdk_visibility_state_get_type\")
  (:unobscured 0)
  (:partial 1)
  (:fully-obscured 2))
  @end{pre}
  @begin[code]{table}
    @entry[:unobscured]{The window is completely visible.}
    @entry[:partial]{The window is partially visible.}
    @entry[:fully-obscured]{The window is not visible at all.}
  @end{table}
  @see-class{gdk:event-visibility}")

;;; ----------------------------------------------------------------------------
;;; enum GdkCrossingMode
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkCrossingMode" crossing-mode
  (:export t
   :type-initializer "gdk_crossing_mode_get_type")
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed
  :touch-begin
  :touch-end
  :device-switch)

#+liber-documentation
(setf (liber:alias-for-symbol 'crossing-mode)
      "GEnum"
      (liber:symbol-documentation 'crossing-mode)
 "@version{#2021-12-13}
  @short{Specifies the crossing mode for a @class{gdk:event-crossing} event.}
  @begin{pre}
(gobject:define-g-enum \"GdkCrosssingMode\" crossing-mode
  (:export t
   :type-initializer \"gdk_crossing_mode_get_type\")
  :normal
  :grab
  :ungrab
  :gtk-grab
  :gtk-ungrab
  :state-changed
  :touch-begin
  :touch-end
  :device-switch)
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Crossing because of pointer motion.}
    @entry[:grab]{Crossing because a grab is activated.}
    @entry[:ungrab]{Crossing because a grab is deactivated.}
    @entry[:gtk-grab]{Crossing because a GTK grab is activated.}
    @entry[:gtk-ungrab]{Crossing because a GTK grab is deactivated.}
    @entry[:state-changed]{Crossing because a GTK widget changed state, e.g.
      sensitivity.}
    @entry[:touch-begin]{Crossing because a touch sequence has begun, this
      event is synthetic as the pointer might have not left the window.}
    @entry[:touch-end]{Crossing because a touch sequence has ended, this event
      is synthetic as the pointer might have not left the window.}
    @entry[:device-switch]{Crossing because of a device switch, i.e. a mouse
      taking control of the pointer after a touch device, this event is
      synthetic as the pointer did not leave the window.}
  @end{table}
  @see-class{gdk:event-crossing}")

;;; ----------------------------------------------------------------------------
;;; enum GdkNotifyType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkNotifyType" notify-type
  (:export t
   :type-initializer "gdk_notify_type_get_type")
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)

#+liber-documentation
(setf (liber:alias-for-symbol 'notify-type)
      "GEnum"
      (liber:symbol-documentation 'notify-type)
 "@version{#2021-12-13}
  @short{Specifies the kind of crossing for a @class{gdk:event-crossing} event.}
  See the X11 protocol specification of @code{LeaveNotify} for full details of
  crossing event generation.
  @begin{pre}
(gobject:define-g-enum \"GdkNotifyType\" notify-type
  (:export t
   :type-initializer \"gdk_notify_type_get_type\")
  (:ancestor 0)
  :virtual
  :inferior
  :nonlinear
  :nonlinear-virtual
  :unknown)
  @end{pre}
  @begin[code]{table}
    @entry[:ancestor]{The window is entered from an ancestor or left towards
      an ancestor.}
    @entry[:virtual]{The pointer moves between an ancestor and an inferior of
      the window.}
    @entry[:inferior]{The window is entered from an inferior or left towards
      an inferior.}
    @entry[:nonlinear]{The window is entered from or left towards a window
      which is neither an ancestor nor an inferior.}
    @entry[:nonlinear-virtual]{The pointer moves between two windows which are
      not ancestors of each other and the window is part of the ancestor chain
      between one of these windows and their least common ancestor.}
    @entry[:unknown]{An unknown type of enter/leave event occurred.}
  @end{table}
  @see-class{gdk:event-crossing}")

;;; ----------------------------------------------------------------------------
;;; enum GdkPropertyState
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkPropertyState" property-state
  (:export t
   :type-initializer "gdk_property_state_get_type")
  :new-value
  :delete)

#+liber-documentation
(setf (liber:alias-for-symbol 'property-state)
      "GEnum"
      (liber:symbol-documentation 'property-state)
 "@version{#2021-12-13}
  @begin{short}
    Specifies the type of a property change for a @class{gdk:event-property}
    event.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkPropertyState\" property-state
  (:export t
   :type-initializer \"gdk_property_state_get_type\")
  :new-value
  :delete)
  @end{pre}
  @begin[code]{table}
    @entry[:new-value]{The property value was changed.}
    @entry[:delete]{The property was deleted.}
  @end{table}
  @see-class{gdk:event-property}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowState
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkWindowState" window-state
  (:export t
   :type-initializer "gdk_window_state_get_type")
  (:withdrawn        #.(ash 1 0))
  (:iconified        #.(ash 1 1))
  (:maximized        #.(ash 1 2))
  (:sticky           #.(ash 1 3))
  (:fullscreen       #.(ash 1 4))
  (:above            #.(ash 1 5))
  (:below            #.(ash 1 6))
  (:focused          #.(ash 1 7))
  (:tiled            #.(ash 1 8))
  (:top-tiled        #.(ash 1 9))
  (:top-resizeable   #.(ash 1 10))
  (:right-tiled      #.(ash 1 11))
  (:right-resizable  #.(ash 1 12))
  (:bottom-tiled     #.(ash 1 13))
  (:bottom-resizable #.(ash 1 14))
  (:left-tiled       #.(ash 1 15))
  (:left-resizable   #.(ash 1 16)))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-state)
      "GFlags"
      (liber:symbol-documentation 'window-state)
 "@version{#2023-3-13}
  @short{Specifies the state of a toplevel window.}
  @begin{pre}
(gobject:define-g-flags \"GdkWindowState\" window-state
  (:export t
   :type-initializer \"gdk_window_state_get_type\")
  (:withdrawn        #.(ash 1 0))
  (:iconified        #.(ash 1 1))
  (:maximized        #.(ash 1 2))
  (:sticky           #.(ash 1 3))
  (:fullscreen       #.(ash 1 4))
  (:above            #.(ash 1 5))
  (:below            #.(ash 1 6))
  (:focused          #.(ash 1 7))
  (:tiled            #.(ash 1 8))
  (:top-tiled        #.(ash 1 9))
  (:top-resizeable   #.(ash 1 10)
  (:right-tiled      #.(ash 1 11))
  (:right-resizable  #.(ash 1 12))
  (:bottom-tiled     #.(ash 1 13))
  (:bottom-resizable #.(ash 1 14))
  (:left-tiled       #.(ash 1 15))
  (:left-resizable   #.(ash 1 16)))
  @end{pre}
  @begin[code]{table}
    @entry[:withdrawn]{The window is not shown.}
    @entry[:iconified]{The window is minimized.}
    @entry[:maximized]{The window is maximized.}
    @entry[:sticky]{The window is sticky.}
    @entry[:fullscreen]{The window is maximized without decorations.}
    @entry[:above]{The window is kept above other windows.}
    @entry[:below]{The window is kept below other windows.}
    @entry[:focused]{The window is presented as focused (with active
      decorations).}
    @entry[:tiled]{The window is in a tiled state. Since 3.22, this is
      deprecated in favor of per-edge information.}
    @entry[:top-tiled]{Whether the top edge is tiled.}
    @entry[:top-resizable]{Whether the top edge is resizable.}
    @entry[:right-tiled]{Whether the right edge is tiled.}
    @entry[:right-resizable]{Whether the right edge is resizable.}
    @entry[:bottom-tiled]{Whether the bottom edge is tiled.}
    @entry[:bottom-resizable]{Whether the bottom edge is resizable.}
    @entry[:left-tiled]{Whether the left edge is tiled.}
    @entry[:left-resizable]{Whether the left edge is resizable.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkSettingAction
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkSettingAction" setting-action
  (:export t
   :type-initializer "gdk_setting_action_get_type")
  (:new 0)
  (:changed 1)
  (:deleted 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'setting-action)
      "GEnum"
      (liber:symbol-documentation 'setting-action)
 "@version{#2021-12-13}
  @begin{short}
    Specifies the kind of modification applied to a setting in a
    @class{gdk:event-setting} event.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkSettingAction\" setting-action
  (:export t
   :type-initializer \"gdk_setting_action_get_type\")
  (:new 0)
  (:changed 1)
  (:deleted 2))
  @end{pre}
  @begin[code]{table}
    @entry[:new]{A setting was added.}
    @entry[:changes]{A setting was changed.}
    @entry[:deleted]{A setting was deleted.}
  @end{table}
  @see-class{gdk:event-setting}")

;;; ----------------------------------------------------------------------------
;;; enum GdkOwnerChange
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkOwnerChange" owner-change
  (:export t
   :type-initializer "gdk_owner_change_get_type")
  (:new-owner 0)
  (:destroy 1)
  (:close 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'owner-change)
      "GEnum"
      (liber:symbol-documentation 'owner-change)
 "@version{#2021-12-13}
  @short{Specifies why a selection ownership was changed.}
  @begin{pre}
(gobject:define-g-enum \"GdkOwnerChange\" owner-change
  (:export t
   :type-initializer \"gdk_owner_change_get_type\")
  (:new-owner 0)
  (:destroy 1)
  (:close 2))
  @end{pre}
  @begin[code]{table}
    @entry[:new-owner]{Some other application claimed the ownership.}
    @entry[:destroy]{The window was destroyed.}
    @entry[:close]{The client was closed.}
  @end{table}
  @see-class{gdk:event-owner-change}")

;;; ----------------------------------------------------------------------------
;;; enum GdkEventType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkEventType" event-type
  (:export t
   :type-initializer "gdk_event_type_get_type")
  (:nothing -1)
  (:delete 0)
  (:destroy 1)
  (:expose 2)
  (:motion-notify 3)
  (:button-press 4)
  (:2button-press 5)
  (:double-button-press 5) ; Alias for :2button-press
  (:3button-press 6)
  (:triple-button-press 6) ; Alias for :3button-press
  (:button-release 7)
  (:key-press 8)
  (:key-release 9)
  (:enter-notify 10)
  (:leave-notify 11)
  (:focus-change 12)
  (:configure 13)
  (:map 14)
  (:unmap 15)
  (:property-notify 16)
  (:selection-clear 17)
  (:selection-request 18)
  (:selection-notify 19)
  (:proximity-in 20)
  (:proximity-out 21)
  (:drag-enter 22)
  (:drag-leave 23)
  (:drag-motion 24)
  (:drag-status 25)
  (:drop-start 26)
  (:drop-finished 27)
  (:client-event 28)
  (:visibility-notify 29)
  (:not-used 30)          ; not used
  (:scroll 31)
  (:window-state 32)
  (:setting 33)
  (:owner-change 34)
  (:grab-broken 35)
  (:damage 36)
  (:touch-begin 37)
  (:touch-update 38)
  (:touch-end 39)
  (:touch-cancel 40)
  (:touchpad-swipe 41)
  (:touchpad-pinch 42)
  (:pad-button-press 43)
  (:pad-button-release 44)
  (:pad-ring 45)
  (:pad-strip 46)
  (:pad-group-mode 47)
  (:event-last 48))

#+liber-documentation
(setf (liber:alias-for-symbol 'event-type)
      "GEnum"
      (liber:symbol-documentation 'event-type)
 "@version{#2023-3-13}
  @short{Specifies the type of a @class{gdk:event} instance.}
  Do not confuse these events with the signals that GTK widgets emit.
  Although many of these events result in corresponding signals being emitted,
  the events are often transformed or filtered along the way.
  @begin{pre}
(gobject:define-g-enum \"GdkEventType\" event-type
  (:export t
   :type-initializer \"gdk_event_type_get_type\")
  (:nothing -1)
  (:delete 0)
  (:destroy 1)
  (:expose 2)
  (:motion-notify 3)
  (:button-press 4)
  (:2button-press 5)
  (:double-button-press 5) ; Alias for :2button-press
  (:3button-press 6)
  (:triple-button-press 6) ; Alias for :3button-press
  (:button-release 7)
  (:key-press 8)
  (:key-release 9)
  (:enter-notify 10)
  (:leave-notify 11)
  (:focus-change 12)
  (:configure 13)
  (:map 14)
  (:unmap 15)
  (:property-notify 16)
  (:selection-clear 17)
  (:selection-request 18)
  (:selection-notify 19)
  (:proximity-in 20)
  (:proximity-out 21)
  (:drag-enter 22)
  (:drag-leave 23)
  (:drag-motion 24)
  (:drag-status 25)
  (:drop-start 26)
  (:drop-finished 27)
  (:client-event 28)
  (:visibility-notify 29)
  (:not-used 30)          ; not used
  (:scroll 31)
  (:window-state 32)
  (:setting 33)
  (:owner-change 34)
  (:grab-broken 35)
  (:damage 36)
  (:touch-begin 37)
  (:touch-update 38)
  (:touch-end 39)
  (:touch-cancel 40)
  (:touchpad-swipe 41)
  (:touchpad-pinch 42)
  (:pad-button-press 43)
  (:pad-button-release 44)
  (:pad-ring 45)
  (:pad-strip 46)
  (:pad-group-mode 47)
  (:event-last 48))
  @end{pre}
  @begin[code]{table}
    @entry[:nothing]{A special code to indicate a null event.}
    @entry[:delete]{The window manager has requested that the toplevel window
      be hidden or destroyed, usually when the user clicks on a special icon
      in the title bar.}
    @entry[:destroy]{The window has been destroyed.}
    @entry[:expose]{All or part of the window has become visible and needs to
      be redrawn.}
    @entry[:motion-notify]{The pointer, usually a mouse, has moved.}
    @entry[:button-press]{A mouse button has been pressed.}
    @entry[:2button-press]{A mouse button has been double-clicked. Note that
      each click also generates a @code{:button-press} event.}
    @entry[:double-button-press]{Alias for @code{:2button-press}.}
    @entry[:3button-press]{A mouse button has been clicked 3 times in a short
      period of time. Note that each click also generates a @code{:button-press}
      event.}
    @entry[:triple-button-press]{Alias for the @code{:3button-press} event.}
    @entry[:button-release]{A mouse button has been released.}
    @entry[:key-press]{A key has been pressed.}
    @entry[:key-release]{A key has been released.}
    @entry[:enter-notifiy]{The pointer has entered the window.}
    @entry[:leave-notify]{The pointer has left the window.}
    @entry[:focus-change]{The keyboard focus has entered or left the window.}
    @entry[:configure]{The size, position or stacking order of the window has
      changed. Note that GTK discards these events for windows with the
      @code{:child} value of the @symbol{gdk:window-type} enumeration.}
    @entry[:map]{The window has been mapped.}
    @entry[:unmap]{The window has been unmapped.}
    @entry[:property-notify]{A property on the window has been changed or
      deleted.}
    @entry[:selection-clear]{The application has lost ownership of a
      selection.}
    @entry[:selection-request]{Another application has requested a selection.}
    @entry[:selection-notify]{A selection has been received.}
    @entry[:proximity-in]{An input device has moved into contact with a
      sensing surface, e.g. a touchscreen or graphics tablet.}
    @entry[:proximity-out]{An input device has moved out of contact with a
      sensing surface.}
    @entry[:drag-enter]{The mouse has entered the window while a drag is in
      progress.}
    @entry[:drag-leave]{The mouse has left the window while a drag is in
      progress.}
    @entry[:drag-motion]{The mouse has moved in the window while a drag is in
      progress.}
    @entry[:drag-status]{The status of the drag operation initiated by the
      window has changed.}
    @entry[:drop-start]{A drop operation onto the window has started.}
    @entry[:drop-finished]{The drop operation initiated by the window has
      completed.}
    @entry[:client-event]{A message has been received from another
      application.}
    @entry[:visibility-notify]{The window visibility status has changed.}
    @entry[:scroll]{The scroll wheel was turned.}
    @entry[:window-state]{The state of a window has changed. See the
      @symbol{gdk:window-state} flags for the possible window states.}
    @entry[:setting]{A setting has been modified.}
    @entry[:owner-change]{The owner of a selection has changed.}
    @entry[:grab-broken]{A pointer or keyboard grab was broken.}
    @entry[:damage]{The content of the window has been changed.}
    @entry[:touch-begin]{A new touch event sequence has just started.}
    @entry[:touch-update]{A touch event sequence has been updated.}
    @entry[:touch-end]{A touch event sequence has finished.}
    @entry[:touch-cancel]{A touch event sequence has been canceled.}
    @entry[:touchpad-swipe]{A touchpad swipe gesture event, the current state
      is determined by its phase field.}
    @entry[:touchpad-pinch]{A touchpad pinch gesture event, the current state
      is determined by its phase field.}
    @entry[:pad-button-press]{A tablet pad button press event.}
    @entry[:pad-button-release]{A tablet pad button release event.}
    @entry[:pad-ring]{A tablet pad axis event from a \"ring\".}
    @entry[:pad-strip]{A tablet pad axis event from a \"strip\".}
    @entry[:pad-group-mode]{A tablet pad group mode change.}
    @entry[:event-last]{Marks the end of the @symbol{gdk:event-type}
      enumeration.}
  @end{table}
  @see-struct{gdk:event}
  @see-symbol{gdk:window-type}
  @see-symbol{gdk:window-state}")

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierType
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkModifierType" modifier-type
  (:export t
   :type-initializer "gdk_modifier_type_get_type")
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:mod1-mask    #.(ash 1 3))
  (:mod2-mask    #.(ash 1 4))
  (:mod3-mask    #.(ash 1 5))
  (:mod4-mask    #.(ash 1 6))
  (:mod5-mask    #.(ash 1 7))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28))
  (:release-mask #.(ash 1 30))
  (:modifier-mask #x5c001fff))

#+liber-documentation
(setf (liber:alias-for-symbol 'modifier-type)
      "GFlags"
      (liber:symbol-documentation 'modifier-type)
 "@version{#2021-12-22}
  @begin{short}
    A set of bit-flags to indicate the state of modifier keys and mouse buttons
    in various event types.
  @end{short}
  Typical modifier keys are @kbd{Shift}, @kbd{Control}, @kbd{Meta}, @kbd{Super},
  @kbd{Hyper}, @kbd{Alt}, @kbd{Compose}, @kbd{Apple}, @kbd{CapsLock} or
  @kbd{ShiftLock} keys.

  Like the X Window System, GDK supports 8 modifier keys and 5 mouse buttons.

  GDK recognizes which of the @kbd{Meta}, @kbd{Super} or @kbd{Hyper} keys are
  mapped to Mod2 - Mod5, and indicates this by setting the @code{:super-mask},
  @code{:hyper-mask} or @code{:meta-mask} mask in the state field of key events.

  Note that GDK may add internal values to events which include reserved
  values such as @code{GDK_MODIFIER_RESERVED_13_MASK}. Your code should
  preserve and ignore them. You can use the @code{:modifier-mask} mask to
  remove all reserved values.

  Also note that the GDK X backend interprets button press events for button
  4-7 as scroll events, so the @code{:button4-mask} and @code{:button5-mask}
  masks will never be set.
  @begin{pre}
(gobject:define-g-flags \"GdkModifierType\" modifier-type
  (:export t
   :type-initializer \"gdk_modifier_type_get_type\")
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:mod1-mask    #.(ash 1 3))
  (:mod2-mask    #.(ash 1 4))
  (:mod3-mask    #.(ash 1 5))
  (:mod4-mask    #.(ash 1 6))
  (:mod5-mask    #.(ash 1 7))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28))
  (:release-mask #.(ash 1 30))
  (:modifier-mask #x5c001fff))
  @end{pre}
  @begin[code]{table}
    @entry[:shift-mask]{The @kbd{Shift} key.}
    @entry[:lock-mask]{A Lock key, depending on the modifier mapping of the
      X server this may either be the @kbd{CapsLock} or @kbd{ShiftLock} key.}
    @entry[:control-mask]{The @kbd{Control} key.}
    @entry[:mod1-mask]{The fourth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier, but
      normally it is the @kbd{Alt} key.}
    @entry[:mod2-mask]{The fifth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:mod3-mask]{The sixth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:mod4-mask]{The seventh modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:mod5-mask]{The eighth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier.}
    @entry[:button1-mask]{The first mouse button.}
    @entry[:button2-mask]{The second mouse button.}
    @entry[:button3-mask]{The third mouse button.}
    @entry[:button4-mask]{The fourth mouse button.}
    @entry[:button5-mask]{The fifth mouse button.}
    @entry[:super-mask]{The Super modifier.}
    @entry[:hyper-mask]{The Hyper modifier.}
    @entry[:meta-mask]{The Meta modifier.}
    @entry[:release-mask]{Not used in GDK itself. GTK uses it to differentiate
      between (keyval, modifiers) pairs from key press and release events.}
    @entry[:modifier-mask]{A mask covering all modifier types.}
  @end{table}
  @see-class{gdk:event}")

;;; ----------------------------------------------------------------------------
;;; enum GdkEventMask
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkEventMask" event-mask
  (:export t
   :type-initializer "gdk_event_mask_get_type")
  (:exposure-mask            #.(ash 1 1))
  (:pointer-motion-mask      #.(ash 1 2))
  (:pointer-motion-hint-mask #.(ash 1 3))
  (:button-motion-mask       #.(ash 1 4))
  (:button1-motion-mask      #.(ash 1 5))
  (:button2-motion-mask      #.(ash 1 6))
  (:button3-motion-mask      #.(ash 1 7))
  (:button-press-mask        #.(ash 1 8))
  (:button-release-mask      #.(ash 1 9))
  (:key-press-mask           #.(ash 1 10))
  (:key-release-mask         #.(ash 1 11))
  (:enter-notify-mask        #.(ash 1 12))
  (:leave-notify-mask        #.(ash 1 13))
  (:focus-change-mask        #.(ash 1 14))
  (:structure-mask           #.(ash 1 15))
  (:property-change-mask     #.(ash 1 16))
  (:visibility-notify-mask   #.(ash 1 17))
  (:proximity-in-mask        #.(ash 1 18))
  (:proximity-out-mask       #.(ash 1 19))
  (:substructure-mask        #.(ash 1 20))
  (:scroll-mask              #.(ash 1 21))
  (:touch-mask               #.(ash 1 22))
  (:smooth-scroll-mask       #.(ash 1 23))
  (:touchpad-gesture-maske   #.(ash 1 24))
  (:tabled-pad-mask          #.(ash 1 25))
  (:all-events-mask #x3FFFFFE))

#+liber-documentation
(setf (liber:alias-for-symbol 'event-mask)
      "GFlags"
      (liber:symbol-documentation 'event-mask)
 "@version{#2023-3-13}
  @begin{short}
    A set of bit-flags to indicate which events a window is to receive.
  @end{short}
  Most of these masks map onto one or more of the @symbol{gdk:event-type} types.

  The @code{:pointer-motion-hint-mask} mask is a special mask which is used to
  reduce the number of @code{:motion-notifiy} events received. Normally a
  @code{:motion-notify} event is received each time the mouse moves. However,
  if the application spends a lot of time processing the event, updating the
  display, for example, it can lag behind the position of the mouse. When using
  the @code{:pointer-motion-hint-mask} mask, fewer @code{:motion-notify} events
  will be sent, some of which are marked as a hint. To receive more motion
  events after a motion hint event, the application needs to asks for more, by
  calling the @fun{gdk:event-request-motions} function.

  Motion events are already compressed by default, independent of this
  mechanism. This compression can be disabled with the
  @fun{gdk:window-event-compression} function.

  If the @code{:touch-mask} mask is enabled, the window will receive touch
  events from touch-enabled devices. Those will come as sequences of
  @class{gdk:event-touch} events with the @code{:touch-update} type, enclosed
  by two events with @code{:touch-begin} and @code{:touch-end} types, or
  @code{:touch-cancel} type. The @fun{gdk:event-event-sequence} function
  returns the event sequence for these events, so different sequences may be
  distinguished.
  @begin{pre}
(gobject:define-g-flags \"GdkEventMask\" gdk:event-mask
  (:export t
   :type-initializer \"gdk_event_mask_get_type\")
  (:exposure-mask            #.(ash 1 1))
  (:pointer-motion-mask      #.(ash 1 2))
  (:pointer-motion-hint-mask #.(ash 1 3))
  (:button-motion-mask       #.(ash 1 4))
  (:button1-motion-mask      #.(ash 1 5))
  (:button2-motion-mask      #.(ash 1 6))
  (:button3-motion-mask      #.(ash 1 7))
  (:button-press-mask        #.(ash 1 8))
  (:button-release-mask      #.(ash 1 9))
  (:key-press-mask           #.(ash 1 10))
  (:key-release-mask         #.(ash 1 11))
  (:enter-notify-mask        #.(ash 1 12))
  (:leave-notify-mask        #.(ash 1 13))
  (:focus-change-mask        #.(ash 1 14))
  (:structure-mask           #.(ash 1 15))
  (:property-change-mask     #.(ash 1 16))
  (:visibility-notify-mask   #.(ash 1 17))
  (:proximity-in-mask        #.(ash 1 18))
  (:proximity-out-mask       #.(ash 1 19))
  (:substructure-mask        #.(ash 1 20))
  (:scroll-mask              #.(ash 1 21))
  (:touch-mask               #.(ash 1 22))
  (:smooth-scroll-mask       #.(ash 1 23))
  (:touchpad-gesture-maske   #.(ash 1 24))
  (:tabled-pad-mask          #.(ash 1 25))
  (:all-events-mask #x3FFFFFE))
  @end{pre}
  @begin[code]{table}
    @entry[:exposure-mask]{Receive expose events.}
    @entry[:pointer-motion-mask]{Receive all pointer motion events.}
    @entry[:pointer-motion-hint-mask]{See the explanation above.}
    @entry[:button-motion-mask]{Receive pointer motion events while any
      button is pressed.}
    @entry[:button1-motion-mask]{Receive pointer motion events while 1
      button is pressed.}
    @entry[:button2-motion-mask]{Receive pointer motion events while 2
      button is pressed.}
    @entry[:button3-motion-mask]{Receive pointer motion events while 3
      button is pressed.}
    @entry[:button-press-mask]{Receive button press events.}
    @entry[:button-release-mask]{Receive button release events.}
    @entry[:key-press-mask]{Receive key press events.}
    @entry[:key-release-mask]{Receive key release events.}
    @entry[:enter-notify-mask]{Receive window enter events.}
    @entry[:leave-notify-mask]{Receive window leave events.}
    @entry[:focus-change-mask]{Receive focus change events.}
    @entry[:structure-mask]{Receive events about window configuration
      change.}
    @entry[:property-change-mask]{Receive property change events.}
    @entry[:visibility-notify-mask]{Receive visibility change events.}
    @entry[:proximity-in-mask]{Receive proximity in events.}
    @entry[:proximity-out-mask]{Receive proximity out events.}
    @entry[:substructure-mask]{Receive events about window configuration
      changes of child windows.}
    @entry[:scroll-mask]{Receive scroll events.}
    @entry[:touch-mask]{Receive touch events.}
    @entry[:smooth-scroll-mask]{Receive smooth scrolling events.}
    @entry[:touchpad-gesture-mask]{Receive touchpad gesture events.}
    @entry[:tablet-pad]{Receive tablet pad events.}
    @entry[:all-events-mask]{The combination of all the above event masks.}
  @end{table}
  @see-symbol{gdk:event-type}
  @see-class{gdk:event-touch}
  @see-function{gdk:event-request-motions}
  @see-function{gdk:event-event-sequence}")

;;; ----------------------------------------------------------------------------
;;; GdkTouchpadGesturePhase
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkTouchpadGesturePhase" touchpad-gesture-phase
  (:export t
   :type-initializer "gdk_touchpad_gesture_phase_get_type")
  (:begin 0)
  (:update 1)
  (:end 2)
  (:cancel 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'touchpad-gesture-phase)
      "GEnum"
      (liber:symbol-documentation 'touchpad-gesture-phase)
 "@version{#2021-12-13}
  @begin{short}
    The @symbol{gdk:touchpad-gesture-phase} enumeration specifies the current
    state of a touchpad gesture.
  @end{short}
  All gestures are guaranteed to begin with an event with @code{:begin}
  phase, followed by 0 or several events with @code{:update} phase.

  A finished gesture may have 2 possible outcomes, an event with @code{:end}
  phase will be emitted when the gesture is considered successful, this should
  be used as the hint to perform any permanent changes.

  Cancelled gestures may be so for a variety of reasons, due to hardware or the
  compositor, or due to the gesture recognition layers hinting the gesture did
  not finish resolutely, e.g. a 3rd finger being added during a pinch gesture.
  In these cases, the last event will report the @code{:cancel} phase, this
  should be used as a hint to undo any visible/permanent changes that were done
  throughout the progress of the gesture.
  @begin{pre}
(gobject:define-g-enum \"GdkTouchpadGesturePhase\" touchpad-gesture-phase
  (:export t
   :type-initializer \"gdk_touchpad_gesture_phase_get_type\")
  (:begin 0)
  (:update 1)
  (:end 2)
  (:cancel 3))
  @end{pre}
  @begin[code]{table}
    @entry[:begin]{The gesture has begun.}
    @entry[:update]{The gesture has been updated.}
    @entry[:end]{The gesture was finished, changes should be permanently
      applied.}
    @entry[:cancel]{The gesture was cancelled, all changes should be undone.}
  @end{table}
  @see-class{gdk:event-touchpad-swipe}
  @see-class{gdk:event-touchpad-pinch}")

;;; ----------------------------------------------------------------------------
;;; GdkEventSequence
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque event-sequence "GdkEventSequence"
  :export t
  :type-initializer "gdk_event_sequence_get_type"
  :alloc (error "GdkEventSequence cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'event-sequence)
      "GBoxed"
      (documentation 'event-sequence 'type)
 "@version{#2021-12-13}
  @begin{short}
    The @class{gdk:event-sequence} structure is opaque, and has no user visible
    fields.
  @end{short}
  An instance cannot be created from the Lisp side. See the
  @fun{gdk:event-event-sequence} documentation.
  @see-function{gdk:event-event-sequence}")

;;; ----------------------------------------------------------------------------
;;; union GdkEvent
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-variant-cstruct event "GdkEvent"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ;; GdkEventKey
            ((:key-press :key-release) event-key
             (time :uint32 :initform 0)
             (state modifier-type :initform 0)
             (keyval :uint :initform 0)
             (length :int :initform 0)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil)
                     :initform "")
             (hardware-keycode :uint16 :initform 0)
             (group :uint8 :initform 0)
             (is-modifier :uint :initform 0))
            ;; GdkEventButton
            ((:button-press
              :2button-press
              :double-button-press
              :3button-press
              :triple-button-press
              :button-release) event-button
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (axes (fixed-array :double 2) :initform '(0.0d0 0.0d0))
             (state modifier-type :initform 0)
             (button :uint :initform 0)
             (device (g:object device) :initform (cffi:null-pointer))
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0))
            ;; GdkEventTouch
            ((:touch-begin
              :touch-update
              :touch-end
              :touch-cancel) event-touch
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (axes (fixed-array :double 2) :initform '(0.0d0 0.0d0))
             (state modifier-type)
             ;; FIXME: We can not initialize sequence from the Lisp side.
             (sequence (g:boxed event-sequence))
             (emulating-pointer :boolean)
             (device (g:object device))
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0))
            ;; GdkEventScroll
            ((:scroll) event-scroll
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (state modifier-type)
             (direction scroll-direction :initform :up)
             (device (g:object device))
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0)
             (delta-x :double :initform 0.0d0)
             (delta-y :double :initform 0.0d0))
            ;; GdkEventMotion
            ((:motion-notify) event-motion
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (axes (fixed-array :double 2) :initform '(0.0d0 0.0d0))
             (state modifier-type :initform 0)
             (is-hint :int16 :initform 0)
             (device (g:object device) :initform (cffi:null-pointer))
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0))
            ;; GdkEventExpose
            ((:expose) event-expose
             (area rectangle :inline t :initform (rectangle-new))
             (region (:pointer (:struct cairo:region-t))
                     :initform (cffi:null-pointer))
             (count :int :initform 0))
            ;; GdkEventVisibility
            ((:visibility-notify) event-visibility
             (state visibility-state :initform :unobscured))
            ;; GdkEventCrossing
            ((:enter-notify :leave-notify) event-crossing
             (subwindow (g:object window) :initform (cffi:null-pointer))
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0)
             (mode crossing-mode :initform :normal)
             (detail notify-type :initform :ancestor)
             (focus :boolean :initform nil)
             (state modifier-type :initform 0))
            ;; GdkEventFocus
            ((:focus-change) event-focus
             (in :int16 :initform 0))
            ;; GdkEventConfigure
            ((:configure) event-configure
             (x :int :initform 0)
             (y :int :initform 0)
             (width :int :initform 0)
             (height :int :initform 0))
            ;; GdkEventProperty
            ((:property-notify) event-property
             (atom :pointer :initform (cffi:null-pointer))
             (time :uint32 :initform 0)
             (state property-state :initform :new-value))
            ;; GdkEventSelection
            ((:selection-clear
              :selection-notify
              :selection-request) event-selection
             (selection :pointer :initform (cffi:null-pointer))
             (target :pointer :initform (cffi:null-pointer))
             (property :pointer :initform (cffi:null-pointer))
             (time :uint32 :initform 0)
             (requestor (g:object window)))
            ;; GdkEventDND
            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) event-dnd
             (context (g:object drag-context))
             (time :uint32 :initform 0)
             (x-root :short :initform 0)
             (y-root :short :initform 0))
            ;; GdkEventProximity
            ((:proximity-in
              :proximity-out) event-proximity
             (time :uint32 :initform 0)
             (device (g:object device)))
            ;; GdkEventWindowState
            ((:window-state) event-window-state
             (changed-mask window-state)
             (new-window-state window-state))
            ;; GdkEventSetting
            ((:setting) event-setting
             (action setting-action :initform :new)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))
            ;; GdkEventOwnerChange
            ((:owner-change) event-owner-change
             (owner (g:object window))
             (reason owner-change :initform :new-owner)
             (selection :pointer :initform (cffi:null-pointer))
             (time :uint32 :initform 0)
             (selection-time :uint32 :initform 0))
            ;; GdkEventGrabBroken
            ((:grab-broken) event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g:object window)))
            ;; GdkEventTouchpadSwipe
            ((:touchpad-swipe) event-touchpad-swipe
             (phase :int8 :initform 0)
             (n-fingers :int8 :initform 0)
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (dx :double :initform 0.0d0)
             (dy :double :initform 0.0d0)
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0)
             (state modifier-type))
            ;; GdkEventTouchpadPinch
            ((:touchpad-pinch) event-touchpad-pinch
             (phase :int8 :initform 0)
             (n-fingers :int8 :initform 0)
             (time :uint32 :initform 0)
             (x :double :initform 0.0d0)
             (y :double :initform 0.0d0)
             (dx :double :initform 0.0d0)
             (dy :double :initform 0.0d0)
             (angle-delta :double :initform 0.0d0)
             (scale :double :initform 0.0d0)
             (x-root :double :initform 0.0d0)
             (y-root :double :initform 0.0d0)
             (state modifier-type))
            ;; GdkEventPadButton
            ((:pad-button-press :pad-button-release) event-pad-button
             (time :uint32 :initform 0)
             (group :uint :initform 0)
             (button :uint :initform 0)
             (mode :uint :initform 0)) ; TODO: Check the type of mode
            ;; GdkEventPadAxis
            ((:pad-ring :pad-strip) event-pad-axis
             (time :uint32 :initform 0)
             (group :uint :initform 0)
             (index :uint :initform 0)
             (mode :uint :initform 0)
             (value :double :initform 0.0d0))
            ;; GdkEventPadGroupMode
            ((:pad-group-mode) event-pad-group-mode
             (time :uint32 :initform 0)
             (group :uint :initform 0)
             (mode :uint :initform 0))))

#+liber-documentation
(setf (liber:alias-for-class 'event)
      "GBoxed"
      (documentation 'event 'type)
 "@version{#2021-12-13}
  @begin{short}
    The @class{gdk:event} structure contains a union of all of the event
    structures, and allows access to the data fields in a number of ways.
  @end{short}

  The event type is always the first field in all of the event structures, and
  can always be accessed with the following code, no matter what type of event
  it is:
  @begin{pre}
(let ((event (gdk:event-new :button-press))) (gdk:event-type event))
=> :BUTTON-PRESS
  @end{pre}
  To access other fields of the event structures, the appropriate event
  structure accesor can be used. For example if the event type is
  @code{:button-press} then the x coordinate of the button press can be
  accessed with:
  @begin{pre}
(let ((event (gdk:event-new :button-press :x 10.0))) (gdk:event-button-x event))
=> 10.0
  @end{pre}
  The complete variant structure which contains all event structures is as
  follows:
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ;; GdkEventKey
            ((:key-press :key-release) event-key
             (time :uint32)
             (state modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))
            ;; GdkEventButton
            ((:button-press
              :2button-press
              :double-button-press
              :3button-press
              :triple-button-press
              :button-release) event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (button :uint)
             (device (g:object device))
             (x-root :double)
             (y-root :double))
            ;; GdkEventTouch
            ((:touch-begin
              :touch-update
              :touch-end
              :touch-cancel) event-touch
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (sequence (g:boxed event-sequence))
             (emulating-pointer :boolean)
             (device (g:object device))
             (x-root :double)
             (y-root :double))
            ;; GdkEventScroll
            ((:scroll) event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state modifier-type)
             (direction scroll-direction)
             (device (g:object device))
             (x-root :double)
             (y-root :double)
             (delta-x :double)
             (delta-y :double))
            ;; GdkEventMotion
            ((:motion-notify) event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (is-hint :int16)
             (device (g:object device))
             (x-root :double)
             (y-root :double))
            ;; GdkEventExpose
            ((:expose) event-expose
             (area rectangle :inline t)
             (region (:pointer (:struct cairo:region-t)))
             (count :int))
            ;; GdkEventVisibity
            ((:visibility-notify) event-visibility
             (state visibility-state))
            ;; GdkEventCrossing
            ((:enter-notify :leave-notify) event-crossing
             (subwindow (g:object window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode crossing-mode)
             (detail notify-type)
             (focus :boolean)
             (state modifier-type))
            ;; GdkEventFocus
            ((:focus-change) event-focus
             (in :int16))
            ;; GdkEventConfigure
            ((:configure) event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))
            ;; GdkEventProperty
            ((:property-notify) event-property
             (atom :pointer)
             (time :uint32)
             (state property-state))
            ;; GdkEventSelection
            ((:selection-clear
              :selection-notify
              :selection-request) event-selection
             (selection :pointer)
             (target :pointer)
             (property :pointer)
             (time :uint32)
             (requestor (g:object window)))
            ;; GdkEventDND
            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) event-dnd
             (context (g:object drag-context))
             (time :uint32)
             (x-root :short)
             (y-root :short))
            ;; GdkEventProximity
            ((:proximity-in
              :proximity-out) event-proximity
             (time :uint32)
             (device (g:object device)))
            ;; GdkEventWindowState
            ((:window-state) event-window-state
             (changed-mask window-state)
             (new-window-state window-state))
            ;; GdkEventSetting
            ((:setting) event-setting
             (action setting-action)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))
            ;; GdkEventOwnerChange
            ((:owner-change) event-owner-change
             (owner (g:object window))
             (reason owner-change)
             (selection :pointer)
             (time :uint32)
             (selection-time :uint32))
            ;; GdkEventGrabBroken
            ((:grab-broken) event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g:object window)))
            ;; GdkEventTouchpadSwipe
            ((:touchpad-swipe) event-touchpad-swipe
             (phase :int8)
             (n-fingers :int8)
             (time :uint32)
             (x :double)
             (y :double)
             (dx :double)
             (dy :double)
             (x-root :double)
             (y-root :double)
             (state modifier-type))
            ;; GdkEventTouchpadPinch
            ((:touchpad-pinch) event-touchpad-pinch
             (phase :int8)
             (n-fingers :int8)
             (time :uint32)
             (x :double)
             (y :double)
             (dx :double)
             (dy :double)
             (angle-delta :double)
             (scale :double)
             (x-root :double)
             (y-root :double)
             (state modifier-type))
            ;; GdkEventPadButton
            ((:pad-button-press :pad-button-release) event-pad-button
             (time :uint32)
             (group :uint)
             (button :uint)
             (mode :uint)) ; TODO: Check the type of mode
            ;; GdkEventPadAxis
            ((:pad-ring :pad-strip) event-pad-axis
             (time :uint32)
             (group :uint)
             (index :uint)
             (mode :uint)
             (value :double))
            ;; GdkEventPadGroupMode
            ((:pad-group-mode) event-pad-group-mode
             (time :uint32)
             (group :uint)
             (mode :uint))))
  @end{pre}
  The following fields are common to all event structures.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  ... )
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-class{gdk:window}
  @see-symbol{gdk:event-type}")

(export 'event)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventAny
;;; ----------------------------------------------------------------------------

;;; --- event-type ---------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-type)
      "Accessor"
      (documentation 'event-type 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-type instance) => type}
  @syntax[]{(setf (gdk:event-type instance) type)}
  @argument[instance]{a @class{gdk:event} instance}
  @argument[type]{a value of the @symbol{gdk:event-type} enumeration}
  @begin{short}
    Accessor of the @code{type} slot of the @class{gdk:event} structure.
  @end{short}

  The type of the event as a value of the @symbol{gdk:event-type} enumeration.
  @begin[Example]{dictionary}
    Check for a button press event in a handler for the @code{\"event\"} signal
    on a drawing area.
    @begin{pre}
(defun drawing-area-event (widget event)
  (declare (ignore widget))
  ;; Check for a button press event on the drawing area
  (when (eq (gdk:event-type event) :button-press)
    ... ))
    @end{pre}
  @end{dictionary}
  @see-struct{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-type)

;;; --- event-window -------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-window)
      "Accessor"
      (documentation 'event-window 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-window instance) => window}
  @syntax[]{(setf (gdk:event-window instance) window)}
  @argument[instance]{a @class{gdk:event} instance}
  @argument[window]{a @class{gdk:window} object which received the event}
  @begin{short}
    Accessor of the @code{window} slot of the @class{gdk:event} structure.
  @end{short}
  @see-class{gdk:event}
  @see-class{gdk:window}")

(export 'event-window)

;;; --- event-send-event ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-send-event)
      "Accessor"
      (documentation 'event-send-event 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-send-event instance) => send-event}
  @syntax[]{(setf (gdk:event-send-event instance) send-event)}
  @argument[instance]{a @class{gdk:event} instance}
  @argument[send-event]{a boolean whether the event was sent explicitly}
  @begin{short}
    Accessor of the @code{send-event} slot of the @class{gdk:event} structure.
  @end{short}
  @see-class{gdk:event}")

(export 'event-send-event)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventKey
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-key)
      "GBoxed"
      (documentation 'event-key 'type)
 "@version{#2021-12-13}
  @short{Describes a key press or key release event.}
  Possible event types are the @code{:key-press} or @code{:key-release} values.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:key-press :key-release) event-key
             (time :uint32)
             (state modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event, one of the
      values @code{:key-press}, @code{:key-release}.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
    @entry[keyval]{The key that was pressed or released. See the
      @file{gdk/gdkkeysyms.h} header file for a complete list of GDK key codes.}
    @entry[length]{An integer with the length of the @code{string} field.}
    @entry[string]{A string containing an approximation of the text that would
      result from this keypress. The only correct way to handle text input is
      using input methods, see the @class{gtk-im-context} API, so this field is
      deprecated and should never be used. The @fun{gdk:unicode-to-keyval}
      function provides a non-deprecated way of getting an
      approximate translation for a key. The @code{string} field is encoded
      in the encoding of the current locale. Note this for backwards
      compatibility: strings in GTK and GDK are typically in UTF-8 and
      @code{NUL}-terminated. In some cases, the translation of the key code will
      be a single @code{NUL} byte, in which case looking at length is necessary
      to distinguish it from an empty translation.}
    @entry[hardware-keycode]{An unsigned integer with the raw code of the key
      that was pressed or released.}
    @entry[group]{An unsigned integer with the keyboard group.}
    @entry[is-modifier]{A flag that indicates if the @code{hardware-keycode}
      field is mapped to a modifier.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-key-time}
  @see-slot{gdk:event-key-state}
  @see-slot{gdk:event-key-keyval}
  @see-slot{gdk:event-key-length}
  @see-slot{gdk:event-key-string}
  @see-slot{gdk:event-key-hardware-keycode}
  @see-slot{gdk:event-key-group}
  @see-slot{gdk:event-key-is-modifier}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:modifier-type}")

(export 'event-key)

;;; --- event-key-time -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-time)
      "Accessor"
      (documentation 'event-key-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-time instance) => time}
  @syntax[]{(setf (gdk:event-key-time instance) time)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-key} structure.
  @end{short}
  @see-class{gdk:event-key}")

(export 'event-key-time)

;;; --- event-key-state ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-state)
      "Accessor"
      (documentation 'event-key-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-state instance) => state}
  @syntax[]{(setf (gdk:event-key-state instance) state)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-key} structure.
  @end{short}
  @see-class{gdk:event-key}
  @see-symbol{gdk:modifier-type}")

(export 'event-key-state)

;;; --- event-key-keyval ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-keyval)
      "Accessor"
      (documentation 'event-key-keyval 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-keyval instance) => keyval}
  @syntax[]{(setf (gdk:event-key-keyval instance) keyval)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[keyval]{an unsigned integer with the key that was pressed or
    released}
  @begin{short}
    Accessor of the @code{keyval} slot of the @class{gdk:event-key} structure.
  @end{short}
  @see-class{gdk:event-key}")

(export 'event-key-keyval)

;;; --- event-key-length ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-length)
      "Accessor"
      (documentation 'event-key-length 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-length instance) => length}
  @syntax[]{(setf (gdk:event-key-length instance) length)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[length]{an integer with the length of the @code{string} field}
  @begin{short}
    Accessor of the @code{length} slot of the @class{gdk:event-key} structure.
  @end{short}
  @see-class{gdk:event-key}
  @see-function{gdk:event-key-string}")

(export 'event-key-length)

;;; --- event-key-string ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-string)
      "Accessor"
      (documentation 'event-key-string 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-string instance) => string}
  @syntax[]{(setf (gdk:event-key-string instance) string)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[string]{a string containing an approximation of the text that
    would result from the keypress}
  @begin{short}
    Accessor of the @code{string} slot of the @class{gdk:event-key} structure.
  @end{short}
  @see-class{gdk:event-key}")

(export 'event-key-string)

;;; --- event-key-hardware-keycode -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-hardware-keycode)
      "Accessor"
      (documentation 'event-key-hardware-keycode 'function)
 "@version{#2021-4-3}
  @syntax[]{(gdk:event-key-hardware-keycode instance) => keycode}
  @syntax[]{(setf (gdk:event-key-hardware-keycode instance) keycode)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[keycode]{an unsigned integer with the raw code of the key that was
    pressed or released}
  @begin{short}
    Accessor of the @code{keycode} slot of the @class{gdk:event-key}
    structure.
  @end{short}
  @see-class{gdk:event-key}")

(export 'event-key-hardware-keycode)

;;; --- event-key-group ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-group)
      "Accessor"
      (documentation 'event-key-group 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-group instance) => group}
  @syntax[]{(setf (gdk:event-key-group instance) group)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[group]{an unsigned integer with the keyboard group}
  @begin{short}
    Accessor of the @code{group} slot of the @class{gdk:event-key} structure.
  @end{short}
  @see-class{gdk:event-key}")

(export 'event-key-group)

;;; --- event-key-is-modifier ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-key-is-modifier)
      "Accessor"
      (documentation 'event-key-is-modifier 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-key-is-modifier instance) => is-modifier}
  @syntax[]{(setf (gdk:event-key-is-modifier instance) is-modifier)}
  @argument[instance]{a @class{gdk:event-key} instance}
  @argument[is-modifier]{a flag that indicates if the @code{hardware-keycode}
    field is mapped to a modifier}
  @begin{short}
    Accessor of the @code{is-modifier} slot of the @class{gdk:event-key}
    structure.
  @end{short}
  @see-class{gdk:event-key}
  @see-function{gdk:event-key-hardware-keycode}")

(export 'event-key-is-modifier)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventButton
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-button)
      "GBoxed"
      (documentation 'event-button 'type)
 "@version{#2021-12-13}
  @short{Used for button press and button release events.}
  The type field will be one of the @code{:button-press},
  @code{:double-button-press}, @code{triple-button-press}, or
  @code{:button-release} values.

  Double and triple-clicks result in a sequence of events being received. For
  double-clicks the order of events will be:
  @begin{pre}
 :button-press
 :button-release
 :button-press
 :double-button-press
 :button-release
  @end{pre}
  Note that the first click is received just like a normal button press, while
  the second click results in a @code{:double-button-press} event being received
  just after the @code{:button-press} event.

  Triple-clicks are very similar to double-clicks, except that the
  @code{:triple-button-press} event is inserted after the third click. The order
  of the events is:
  @begin{pre}
 :button-press
 :button-release
 :button-press
 :double-button-press
 :button-release
 :button-press
 :triple-button-press
 :button-release
  @end{pre}
  For a double click to occur, the second button press must occur within 1/4
  of a second of the first. For a triple click to occur, the third button
  press must also occur within 1/2 second of the first button press.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:button-press
              :2button-press
              :double-button-press
              :3button-press
              :triple-button-press
              :button-release) event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (button :uint)
             (device (g:object device))
             (x-root :double)
             (y-root :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event, one of the
     values @code{:button-press}, @code{:2button-press},
     @code{:double-button-press}, @code{:3button-press},
     @code{:triple-button-press}, @code{:button-release}.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[x]{The double float x coordinate of the pointer relative to the
      window.}
    @entry[y]{The double float y coordinate of the pointer relative to the
      window.}
    @entry[axes]{The @arg{x}, @arg{y} fields translated to the axes of the
      device.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
    @entry[button]{An unsigned integer with the button which was pressed or
      released, numbered from 1 to 5. Normally button 1 is the left mouse
      button, 2 is the middle button, and 3 is the right button. On 2-button
      mice, the middle button can often be simulated by pressing both mouse
      buttons together.}
    @entry[device]{The @class{gdk:device} object where the event originated.}
    @entry[x-root]{The double float x coordinate of the pointer relative to the
      root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer relative to the
      root of the screen.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-button-time}
  @see-slot{gdk:event-button-x}
  @see-slot{gdk:event-button-y}
  @see-slot{gdk:event-button-axes}
  @see-slot{gdk:event-button-state}
  @see-slot{gdk:event-button-button}
  @see-slot{gdk:event-button-device}
  @see-slot{gdk:event-button-x-root}
  @see-slot{gdk:event-button-y-root}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:modifier-type}")

(export 'event-button)

;;; --- event-button-time --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-time)
      "Accessor"
      (documentation 'event-button-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-time instance) => time}
  @syntax[]{(setf (gdk:event-button-time instance) time)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-button}
    structure.
  @end{short}
  @see-class{gdk:event-button}")

(export 'event-button-time)

;;; --- event-button-x -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-x)
      "Accessor"
      (documentation 'event-button-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-x instance) => x}
  @syntax[]{(setf (gdk:event-button-x instance) x)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[x]{a double float with the x coordinate of the pointer relative to
    the window}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-button} structure.
  @end{short}
  @see-class{gdk:event-button}")

(export 'event-button-x)

;;; --- event-button-y -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-y)
      "Accessor"
      (documentation 'event-button-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-y instance) => y}
  @syntax[]{(setf (gdk:event-button-y instance) y)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[y]{a double float with the y coordinate of the pointer relative to
    the window}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-button} structure.
  @end{short}
  @see-class{gdk:event-button}")

(export 'event-button-y)

;;; --- event-button-axes --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-axes)
      "Accessor"
      (documentation 'event-button-axes 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-axes instance) => axes}
  @syntax[]{(setf (gdk:event-button-axes instance) axes)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[axes]{an array of double float with the x,y coordinates translated
    to the axes of the device}
  @begin{short}
    Accessor of the @code{axes} slot of the @class{gdk:event-button} structure.
  @end{short}
  @see-class{gdk:event-button}")

(export 'event-button-axes)

;;; --- event-button-state -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-state)
      "Accessor"
      (documentation 'event-button-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-state instance) => state}
  @syntax[]{(setf (gdk:event-button-state instance) state)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bit-mask representing the
    state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
    @kbd{Alt} keys, and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-button} structure.
  @end{short}
  @see-class{gdk:event-button}
  @see-symbol{gdk:modifier-type}")

(export 'event-button-state)

;;; --- event-button-button ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-button)
      "Accessor"
      (documentation 'event-button-button 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-button instance) => button}
  @syntax[]{(setf (gdk:event-button-button instance) button)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[button]{an unsigned integer with the button which was pressed}
  @begin{short}
    Accessor of the @code{button} slot of the @class{gdk:event-button}
    structure.
  @end{short}

  The button which was pressed or released, numbered from 1 to 5. Normally
  button 1 is the left mouse button, 2 is the middle button, and 3 is the right
  button. On 2-button mice, the middle button can often be simulated by
  pressing both mouse buttons together.
  @see-class{gdk:event-button}")

(export 'event-button-button)

;;; --- event-button-device ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-device)
      "Accessor"
      (documentation 'event-button-device 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-device instance) => device}
  @syntax[]{(setf (gdk:event-button-device instance) device)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[device]{a @class{gdk:device} master device that the event
    originated from}
  @begin{short}
    Accessor of the @code{device} slot of the @class{gdk:event-button}
    structure.
  @end{short}
  Use the @fun{gdk:event-source-device} function to get the slave device.
  @see-class{gdk:event-button}
  @see-class{gdk:device}
  @see-function{gdk:event-source-device}")

(export 'event-button-device)

;;; --- event-button-x-root ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-x-root)
      "Accessor"
      (documentation 'event-button-x-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-button-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-button}
    structure.
  @end{short}
  @see-class{gdk:event-button}")

(export 'event-button-x-root)

;;; --- event-button-y-root ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-button-y-root)
      "Accessor"
      (documentation 'event-button-y-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-button-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-button-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-button} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-button}
    structure.
  @end{short}
  @see-class{gdk:event-button}")

(export 'event-button-y-root)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventTouch
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-touch)
      "GBoxed"
      (documentation 'event-touch 'type)
 "@version{#2021-12-13}
  @begin{short}
    Used for touch events.
  @end{short}
  The type field will be one of the @code{:touch-begin}, @code{:touch-update},
  @code{:touch-end} or @code{:touch-cancel} values.

  Touch events are grouped into sequences by means of the sequence field,
  which can also be obtained with the @fun{gdk:event-event-sequence} function.
  Each sequence begins with a @code{:touch-begin} event, followed by any number
  of @code{:touch-update} events, and ends with a @code{:touch-end} event or
  @code{:touch-cancel} event. With multitouch devices, there may be several
  active sequences at the same time.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:touch-begin
              :touch-update
              :touch-end
              :touch-cancel) event-touch
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (sequence (g:boxed event-sequence))
             (emulating-pointer :boolean)
             (device (g:object device))
             (x-root :double)
             (y-root :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event, one of the
      values @code{:touch-begin}, @code{:touch-update}, @code{:touch-end},
      @code{:touch-cancel}.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x]{The double float x coordinate of the pointer relative to the
      window.}
    @entry[y]{The double float y coordinate of the pointer relative to the
      window.}
    @entry[axes]{The @arg{x}, @arg{y} fields translated to the axes of the
      device.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
    @entry[sequence]{The @class{gdk:event-sequence} event sequence that the
      event belongs to.}
    @entry[emulating-pointer]{Whether the event should be used for emulating
      pointer event.}
    @entry[device]{The @class{gdk:device} object where the event originated.}
    @entry[x-root]{The double float x coordinate of the pointer relative to the
      root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer relative to the
      root of the screen.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-touch-time}
  @see-slot{gdk:event-touch-x}
  @see-slot{gdk:event-touch-y}
  @see-slot{gdk:event-touch-axes}
  @see-slot{gdk:event-touch-state}
  @see-slot{gdk:event-touch-sequence}
  @see-slot{gdk:event-touch-emulating-pointer}
  @see-slot{gdk:event-touch-device}
  @see-slot{gdk:event-touch-x-root}
  @see-slot{gdk:event-touch-y-root}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}
  @see-function{gdk:event-event-sequence}")

(export 'event-touch)

;;; --- event-touch-time ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-time)
      "Accessor"
      (documentation 'event-touch-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-time instance) => time}
  @syntax[]{(setf (gdk:event-touch-time instance) time)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-time)

;;; --- event-touch-x ------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-x)
      "Accessor"
      (documentation 'event-touch-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-x instance) => x}
  @syntax[]{(setf (gdk:event-touch-x instance) x)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[x]{a double float with the x coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-x)

;;; --- event-touch-y ------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-y)
      "Accessor"
      (documentation 'event-touch-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-y instance) => y}
  @syntax[]{(setf (gdk:event-touch-y instance) y)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[y]{a double float with the y coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-y)

;;; --- event-touch-axes ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-axes)
      "Accessor"
      (documentation 'event-touch-axes 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-axes instance) => axes}
  @syntax[]{(setf (gdk:event-touch-axes instance) axes)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[axes]{an array of double float with the x,y coordinates translated
    to the axes of the device}
  @begin{short}
    Accessor of the @code{axes} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-axes)

;;; --- event-touch-state --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-state)
      "Accessor"
      (documentation 'event-touch-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-state instance) => state}
  @syntax[]{(setf (gdk:event-touch-state instance) state)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bit-mask representing the state
    of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and @kbd{Alt}
    keys, and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}
  @see-symbol{gdk:modifier-type}")

(export 'event-touch-state)

;;; --- event-touch-sequence -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-sequence)
      "Accessor"
      (documentation 'event-touch-sequence 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-sequence instance) => sequence}
  @syntax[]{(setf (gdk:event-touch-sequence instance) sequence)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[sequence]{a @class{gdk:event-sequence} event sequence that the
    event belongs to}
  @begin{short}
    Accessor of the @code{sequence} slot of the @class{gdk:event-touch}
    structure.
  @end{short}
  @see-class{gdk:event-touch}
  @see-class{gdk:event-sequence}")

(export 'event-touch-sequence)

;;; --- event-touch-emulating-pointer --------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-emulating-pointer)
      "Accessor"
      (documentation 'event-touch-emulating-pointer 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-emulating-pointer instance) => emulating}
  @syntax[]{(setf (gdk:event-touch-emulating-pointer instance) emulating)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[emulating]{a boolean whether the event should be used for emulating
    pointer event}
  @begin{short}
    Accessor of the @code{emulating-pointer} slot of the @class{gdk:event-touch}
    structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-emulating-pointer)

;;; --- event-touch-device -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-device)
      "Accessor"
      (documentation 'event-touch-device 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-device instance) => device}
  @syntax[]{(setf (gdk:event-touch-device instance) device)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[device]{a @class{gdk:device} master device that the event
    originated from}
  @begin{short}
    Accessor of the @code{device} slot of the @class{gdk:event-touch} structure.
  @end{short}
  Use the @fun{gdk:event-source-device} function to get the slave device.
  @see-class{gdk:event-touch}
  @see-class{gdk:device}
  @see-function{gdk:event-source-device}")

(export 'event-touch-device)

;;; --- event-touch-x-root -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-x-root)
      "Accessor"
      (documentation 'event-touch-x-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-touch-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-x-root)

;;; --- event-touch-y-root -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touch-y-root)
      "Accessor"
      (documentation 'event-touch-y-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-touch-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-touch-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-touch} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-touch} structure.
  @end{short}
  @see-class{gdk:event-touch}")

(export 'event-touch-y-root)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventScroll
;;; ----------------------------------------------------------------------------

;; FIXME: The Gtk documentation additionally has the field is_stop of type guint

#+liber-documentation
(setf (liber:alias-for-class 'event-scroll)
      "GBoxed"
      (documentation 'event-scroll 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated from button presses for the buttons 4 to 7.
  @end{short}
  Wheel mice are usually configured to generate button press events for buttons
  4 and 5 when the wheel is turned.

  Some GDK backends can also generate 'smooth' scroll events, which can be
  recognized by the @code{:smooth} direction. For these, the scroll deltas can
  be obtained with the @fun{gdk:event-scroll-deltas} function.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:scroll) event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state modifier-type)
             (direction scroll-direction)
             (device (g:object device))
             (x-root :double)
             (y-root :double)
             (delta-x :double)
             (delta-y :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the scroll event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x]{The double float x coordinate of the pointer relative to the
      window.}
    @entry[y]{The double float y coordinate of the pointer relative to the
      window.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
    @entry[direction]{The @symbol{gdk:scroll-direction} direction to scroll to,
      one of the @code{:up}, @code{:down}, @code{:left}, @code{:right} or
      @code{:smooth} values.}
    @entry[device]{The @class{gdk:device} object where the event originated.}
    @entry[x-root]{The double float x coordinate of the pointer relative to the
      root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer relative to the
      root of the screen.}
    @entry[delta-x]{The double float x coordinate of the scroll delta.}
    @entry[delta-y]{The double float y coordinate of the scroll delta.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-scroll-time}
  @see-slot{gdk:event-scroll-x}
  @see-slot{gdk:event-scroll-y}
  @see-slot{gdk:event-scroll-state}
  @see-slot{gdk:event-scroll-direction}
  @see-slot{gdk:event-scroll-device}
  @see-slot{gdk:event-scroll-x-root}
  @see-slot{gdk:event-scroll-y-root}
  @see-slot{gdk:event-scroll-delta-x}
  @see-slot{gdk:event-scroll-delta-y}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}
  @see-function{gdk:event-scroll-deltas}")

(export 'event-scroll)

;;; --- event-scroll-time --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-time)
      "Accessor"
      (documentation 'event-scroll-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-time instance) => time}
  @syntax[]{(setf (gdk:event-scroll-time instance) time)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-scroll} structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-time)

;;; --- event-scroll-x -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-x)
      "Accessor"
      (documentation 'event-scroll-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-x instance) => x}
  @syntax[]{(setf (gdk:event-scroll-x instance) x)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[x]{a double float with the x coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-scroll} structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-x)

;;; --- event-scroll-y -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-y)
      "Accessor"
      (documentation 'event-scroll-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-y instance) => y}
  @syntax[]{(setf (gdk:event-scroll-y instance) y)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[y]{a double float with the y coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-scroll} structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-y)

;;; --- event-scroll-state -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-state)
      "Accessor"
      (documentation 'event-scroll-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-state instance) => state}
  @syntax[]{(setf (gdk:event-scroll-state instance) state)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bit-mask representing the
    state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
    @kbd{Alt} keys, and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-scroll} structure.
  @end{short}
  @see-class{gdk:event-scroll}
  @see-symbol{gdk:modifier-type}")

(export 'event-scroll-state)

;;; --- event-scroll-direction ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-direction)
      "Accessor"
      (documentation 'event-scroll-direction 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-direction instance) => direction}
  @syntax[]{(setf (gdk:event-scroll-direction instance) direction)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[direction]{a value of the @symbol{gdk:scroll-direction} enumeration}
  @begin{short}
    Accessor of the @code{direction} slot of the @class{gdk:event-scroll}
    structure.
  @end{short}
  The @fun{gdk:scroll-direction} function extracts the scroll direction from a
  scroll event.

  If you wish to handle both discrete and smooth scrolling, you should check
  the return value of this function, or of the @fun{gdk:event-scroll-deltas}
  function. For instance:
  @begin{pre}
GdkScrollDirection direction;
double vscroll_factor = 0.0;
double x_scroll, y_scroll;

if (gdk_event_get_scroll_direction (event, &direction))
  {
    // Handle discrete scrolling with a known constant delta;
    const double delta = 12.0;

    switch (direction)
      {
      case GDK_SCROLL_UP:
        vscroll_factor = -delta;
        break;
      case GDK_SCROLL_DOWN:
        vscroll_factor = delta;
        break;
      default:
        // no scrolling
        break;
      @}
  @}
else if (gdk_event_get_scroll_deltas (event, &x_scroll, &y_scroll))
  {
    // Handle smooth scrolling directly
    vscroll_factor = y_scroll;
  @}
  @end{pre}
  @see-class{gdk:event-scroll}
  @see-symbol{gdk:scroll-direction}
  @see-function{gdk:event-scroll-deltas}")

(export 'event-scroll-direction)

;;; --- event-scroll-device ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-device)
      "Accessor"
      (documentation 'event-scroll-device 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-device instance) => device}
  @syntax[]{(setf (gdk:event-scroll-device instance) device)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[device]{a @class{gdk:device} master device that the event
    originated from}
  @begin{short}
    Accessor of the @code{device} of the @class{gdk:event-scroll} structure.
  @end{short}
  Use the @fun{gdk:event-source-device} function to get the slave device.
  @see-class{gdk:event-scroll}
  @see-class{gdk:device}")

(export 'event-scroll-device)

;;; --- event-scroll-x-root ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-x-root)
      "Accessor"
      (documentation 'event-scroll-x-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-scroll-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-scroll}
    structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-x-root)

;;; --- event-scroll-y-root ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-y-root)
      "Accessor"
      (documentation 'event-scroll-y-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-scroll-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-scroll}
    structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-y-root)

;;; --- event-scroll-delta-x -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-delta-x)
      "Accessor"
      (documentation 'event-scroll-delta-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-delta-x instance) => delta-x}
  @syntax[]{(setf (gdk:event-scroll-delta-x instance) delta-x)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[delta-x]{a double float with the x coordinate of the scroll deltas}
  @begin{short}
    Accessor of the @code{delta-x} of the @class{gdk:event-scroll} structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-delta-x)

;;; --- event-scroll-delta-y -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-scroll-delta-y)
      "Accessor"
      (documentation 'event-scroll-delta-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-scroll-delta-y instance) => delta-y}
  @syntax[]{(setf (gdk:event-scroll-delta-y instance) delta-y)}
  @argument[instance]{a @class{gdk:event-scroll} instance}
  @argument[delta-y]{a double float with the y coordinate of the scroll deltas}
  @begin{short}
    Accessor of the @code{delta-y} slot of the @class{gdk:event-scroll}
    structure.
  @end{short}
  @see-class{gdk:event-scroll}")

(export 'event-scroll-delta-y)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventMotion
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-motion)
      "GBoxed"
      (documentation 'event-motion 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when the pointer moves.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:motion-notify) event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (is-hint :int16)
             (device (g:object device))
             (x-root :double)
             (y-root :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x]{The double float x coordinate of the pointer relative to the
      window.}
    @entry[y]{The double float y coordinate of the pointer relative to the
      window.}
    @entry[axes]{The @arg{x}, @arg{y} fields translated to the axes of the
      device.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
    @entry[is-hint]{Set to 1 if this event is just a hint, see the
      @code{:pointer-motion-hint-mask} value of the @symbol{gdk:event-mask}
      flags.}
    @entry[device]{The @class{gdk:device} object where the event originated.}
    @entry[x-root]{The double float x coordinate of the pointer relative to the
      root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer relative to the
      root of the screen.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-motion-time}
  @see-slot{gdk:event-motion-x}
  @see-slot{gdk:event-motion-y}
  @see-slot{gdk:event-motion-axes}
  @see-slot{gdk:event-motion-state}
  @see-slot{gdk:event-motion-is-hint}
  @see-slot{gdk:event-motion-device}
  @see-slot{gdk:event-motion-x-root}
  @see-slot{gdk:event-motion-y-root}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-motion)

;;; --- event-motion-time --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-time)
      "Accessor"
      (documentation 'event-motion-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-time instance) => time}
  @syntax[]{(setf (gdk:event-motion-time instance) time)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-motion} structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-time)

;;; --- event-motion-x -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-x)
      "Accessor"
      (documentation 'event-motion-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-x instance) => x}
  @syntax[]{(setf (gdk:event-motion-x instance) x)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[x]{a double float with the x coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-motion} structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-x)

;;; --- event-motion-y -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-y)
      "Accessor"
      (documentation 'event-motion-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-y instance) => y}
  @syntax[]{(setf (gdk:event-motion-y instance) y)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[y]{a double float with the y coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-motion} structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-y)

;;; --- event-motion-axes --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-axes)
      "Accessor"
      (documentation 'event-motion-axes 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-axes instance) => axes}
  @syntax[]{(setf (gdk:event-motion-axes instance) axes)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[axes]{an array of double float with the x,y coordinates translated
    to the axes of the device}
  @begin{short}
    Accessor of the @code{axes} slot of the @class{gdk:event-motion} structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-axes)

;;; --- event-motion-state -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-state)
      "Accessor"
      (documentation 'event-motion-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-state instance) => state}
  @syntax[]{(setf (gdk:event-motion-state instance) state)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bit-mask representing the state
    of the modifier keys, e.g. @kbd{Control}, @kbd{Shift} and @kbd{Alt} keys,
    and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-motion} structure.
  @end{short}
  @see-class{gdk:event-motion}
  @see-symbol{gdk:modifier-state}")

(export 'event-motion-state)

;;; --- event-motion-is-hint -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-is-hint)
      "Accessor"
      (documentation 'event-motion-is-hint 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-is-hint instance) => is-hint}
  @syntax[]{(setf (gdk:event-motion-is-hint instance) is-hint)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[is-hint]{set to 1 if this event is just a hint, see the
      @code{:pointer-motion-hint-mask} value of the @symbol{gdk:event-mask}
      flags}
  @begin{short}
    Accessor of the @code{is-hint} slot of the @class{gdk:event-motion}
    structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-is-hint)

;;; --- event-motion-device ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-device)
      "Accessor"
      (documentation 'event-motion-device 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-device instance) => device}
  @syntax[]{(setf (gdk:event-motion-device instance) device)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[device]{a @class{gdk:device} master device that the event
    originated from}
  @begin{short}
    Accessor of the @code{device} slot of the @class{gdk:event-motion}
    structure.
  @end{short}
  Use the @fun{gdk:event-source-device} function to get the slave device.
  @see-class{gdk:event-motion}
  @see-class{gdk:device}
  @see-function{gdk:event-source-device}")

(export 'event-motion-device)

;;; --- event-motion-x-root ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-x-root)
      "Accessor"
      (documentation 'event-motion-x-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-motion-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-motion}
    structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-x-root)

;;; --- event-motion-y-root ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-motion-y-root)
      "Accessor"
      (documentation 'event-motion-y-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-motion-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-motion-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-motion} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-motion}
    structure.
  @end{short}
  @see-class{gdk:event-motion}")

(export 'event-motion-y-root)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventExpose
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-expose)
      "GBoxed"
      (documentation 'event-expose 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when all or part of a window becomes visible and needs to be
    redrawn.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:expose) event-expose
             (area rectangle :inline t)
             (region (:pointer (:struct cairo:region-t)))
             (count :int))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[area]{Bounding @symbol{gdk:rectangle} box of the region.}
    @entry[region]{The region of type @symbol{cairo:region-t} that needs to be
      redrawn.}
    @entry[count]{The number of contiguous expose events following this one.
      The only use for this is \"exposure compression\", i.e. handling all
      contiguous expose events in one go, though GDK performs some exposure
      compression so this is not normally needed.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-expose-area}
  @see-slot{gdk:event-expose-region}
  @see-slot{gdk:event-expose-count}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-class{gdk:rectangle}
  @see-symbol{cairo:region-t}
  @see-symbol{gdk:event-type}")

(export 'event-expose)

;;; ----------------------------------------------------------------------------
;;; Constructors for the GdkEventExpose structure
;;; ----------------------------------------------------------------------------

;;; --- event-expose-area --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-expose-area)
      "Accessor"
      (documentation 'event-expose-area 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-expose-area instance) => area}
  @syntax[]{(setf (gdk:event-expose-area instance) area)}
  @argument[instance]{a @class{gdk:event-expose} instance}
  @argument[area]{bounding @class{gdk:rectangle} box of the region that
    needs to be redrawn}
  @begin{short}
    Accessor of the @code{area} slot of the @class{gdk:event-expose} structure.
  @end{short}
  @see-class{gdk:event-expose}
  @see-class{gdk:rectangle}")

(export 'event-expose-area)

;;; --- event-expose-region ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-expose-region)
      "Accessor"
      (documentation 'event-expose-region 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-expose-region instance) => region}
  @syntax[]{(setf (gdk:event-expose-region instance) region)}
  @argument[instance]{a @class{gdk:event-expose} instance}
  @argument[region]{a @symbol{cairo:region-t} region that needs to be redrawn}
  @begin{short}
    Accessor of the @code{region} slot of the @class{gdk:event-expose}
    structure.
  @end{short}
  @see-class{gdk:event-expose}
  @see-symbol{cairo:region-t}")

(export 'event-expose-region)

;;; --- event-expose-count -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-expose-count)
      "Accessor"
      (documentation 'event-expose-count 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-expose-count instance) => count}
  @syntax[]{(setf (gdk:event-expose-count instance) count)}
  @argument[instance]{a @class{gdk:event-expose} instance}
  @argument[count]{an integer with the the number of contiguous
    @class{gdk:event-expose} events following this one}
  @begin{short}
    Accessor of the @code{count} of the @class{gdk:event-expose} structure.
  @end{short}

  The number of contiguous @class{gdk:event-expose} events following this one.
  The only use for this is \"exposure compression\", i.e. handling all
  contiguous @class{gdk:event-expose} events in one go, though GDK performs
  some exposure compression so this is not normally needed.
  @see-class{gdk:event-expose}")

(export 'event-expose-count)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventVisibility
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-visibility)
      "GBoxed"
      (documentation 'event-visibility 'type)
 "@version{#2021-12-13}
  @short{Generated when the window visibility status has changed.}
  @begin[Warning]{dictionary}
    The @class{gdk:event-visibility} structure has been deprecated since version
    3.12 and should not be used in newly written code. Modern composited
    windowing systems with pervasive transparency make it impossible to track
    the visibility of a window reliably, so this event can not be guaranteed to
    provide useful information.
  @end{dictionary}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:visibility-notify) event-visibility
             (state visibility-state))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[state]{The new @symbol{gdk:visibility-state} state, possible values
      are the @code{:fully-obscured}, @code{:partial} or @code{:unobscured}
      values.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-visibility-state}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:visibility-state}")

(export 'event-visibility)

;;; --- event-visibility-state ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-visibility-state)
      "Accessor"
      (documentation 'event-visibility-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-visibility-state instance) => visibility}
  @syntax[]{(setf (gdk:event-visibility-state instance) visibility)}
  @argument[instance]{a @class{gdk:event-visibility} instance}
  @argument[visibility]{a @symbol{gdk:visibility-state} value}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-visibility}
    structure.
  @end{short}
  @see-class{gdk:event-visibility}
  @see-symbol{gdk:visibility-state}")

(export 'event-visibility-state)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventCrossing
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-crossing)
      "GBoxed"
      (documentation 'event-crossing 'type)
 "@version{#2021-12-13}
  @short{Generated when the pointer enters or leaves a window.}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:enter-notify :leave-notify) event-crossing
             (subwindow (g:object window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode crossing-mode)
             (detail notify-type)
             (focus :boolean)
             (state modifier-type))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[subwindow]{The @class{gdk:window} object that was entered or left.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x]{The double float x coordinate of the pointer relative to the
      window.}
    @entry[y]{The double float y coordinate of the pointer relative to the
      window.}
    @entry[x-root]{The double float x coordinate of the pointer relative to the
      root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer relative to the
      root of the screen.}
    @entry[mode]{The @symbol{gdk:crossing-mode} value.}
    @entry[detail]{The @symbol{gdk:notify-type} kind of crossing that happened.}
    @entry[focus]{@em{True} if @arg{window} is the focus window or an inferior.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-crossing-subwindow}
  @see-slot{gdk:event-crossing-time}
  @see-slot{gdk:event-crossing-x}
  @see-slot{gdk:event-crossing-y}
  @see-slot{gdk:event-crossing-x-root}
  @see-slot{gdk:event-crossing-y-root}
  @see-slot{gdk:event-crossing-mode}
  @see-slot{gdk:event-crossing-detail}
  @see-slot{gdk:event-crossing-focus}
  @see-slot{gdk:event-crossing-state}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:crossing-mode}
  @see-symbol{gdk:notify-type}")

(export 'event-crossing)

;;; --- event-crossing-subwindow -------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-subwindow)
      "Accessor"
      (documentation 'event-crossing-subwindow 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-subwindow instance) => subwindow}
  @syntax[]{(setf (gdk:event-crossing-subwindow instance) subwindow)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[subwindow]{a @class{gdk:window} object that was entered or left}
  @begin{short}
    Accessor of the @code{subwindow} slot of the @class{gdk:event-crossing}
    structure
  @end{short}
  @see-class{gdk:event-crossing}
  @see-class{gdk:window}")

(export 'event-crossing-subwindow)

;;; --- event-crossing-time ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-time)
      "Accessor"
      (documentation 'event-crossing-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-time instance) => time}
  @syntax[]{(setf (gdk:event-crossing-time instance) time)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-crossing}
    structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-time)

;;; --- event-crossing-x ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-x)
      "Accessor"
      (documentation 'event-crossing-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-x instance) => x}
  @syntax[]{(setf (gdk:event-crossing-x instance) x)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[x]{a double float with the x coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-crossing} structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-x)

;;; --- event-crossing-y ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-y)
      "Accessor"
      (documentation 'event-crossing-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-y instance) => y}
  @syntax[]{(setf (gdk:event-crossing-y instance) y)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[y]{a double float with the y coordinate of the pointer relative
    to the window}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-crossing} structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-y)

;;; --- event-crossing-x-root ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-x-root)
      "Accessor"
      (documentation 'event-crossing-x-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-crossing-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} of the @class{gdk:event-crossing} structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-x-root)

;;; --- event-crossing-y-root ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-y-root)
      "Accessor"
      (documentation 'event-crossing-y-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-crossing-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-crossing}
    structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-y-root)

;;; --- event-crossing-mode ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-mode)
      "Accessor"
      (documentation 'event-crossing-mode 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-mode instance) => mode}
  @syntax[]{(setf (gdk:event-crossing-mode instance) mode)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[mode]{a @symbol{gdk:crossing-mode} value}
  @begin{short}
    Accessor of the @code{mode} slot of the @class{gdk:event-crossing}
    structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-mode)

;;; --- event-crossing-detail ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-detail)
      "Accessor"
      (documentation 'event-crossing-detail 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-detail instance) => detail}
  @syntax[]{(setf (gdk:event-crossing-detail instance) detail)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[detail]{a @symbol{gdk:notify-type} value}
  @begin{short}
    Accessor of the @code{detail} slot of the @class{gdk:event-crossing}
    structure.
  @end{short}
  @see-class{gdk:event-crossing}
  @see-symbol{gdk:notify-type}")

(export 'event-crossing-detail)

;;; --- event-crossing-focus -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-focus)
      "Accessor"
      (documentation 'event-crossing-focus 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-focus instance) => focus}
  @syntax[]{(setf (gdk:event-crossing-focus instance) focus)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[focus]{@em{true} if the window is focus window or an inferior}
  @begin{short}
    Accessor of the @code{focus} slot of the @class{gdk:event-crossing}
    structure.
  @end{short}
  @see-class{gdk:event-crossing}")

(export 'event-crossing-focus)

;;; --- event-crossing-state -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-crossing-state)
      "Accessor"
      (documentation 'event-crossing-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-crossing-state instance) => state}
  @syntax[]{(setf (gdk:event-crossing-state instance) state)}
  @argument[instance]{a @class{gdk:event-crossing} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bit-mask representing the state
    of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and @kbd{Alt}
    keys, and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-crossing}
    structure.
  @end{short}
  @see-class{gdk:event-crossing}
  @see-symbol{gdk:modifier-type}")

(export 'event-crossing-state)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventFocus
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-focus)
      "GBoxed"
      (documentation 'event-focus 'type)
 "@version{#2021-12-13}
  @short{Describes a change of keyboard focus.}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:focus-change) event-focus
             (in :int16))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[in]{@em{True} if the window has gained the keyboard focus, @em{false}
      if it has lost the focus.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-focus-in}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-focus)

;;; --- event-focus-in -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-focus-in)
      "Accessor"
      (documentation 'event-focus-in 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-focus-in instance) => focus}
  @syntax[]{(setf (gdk:event-focus-in instance) focus)}
  @argument[instance]{a @class{gdk:event-focus} instance}
  @argument[focus]{@em{true} if the window has gained the keyboard focus,
    @em{false} if it has lost the focus}
  @begin{short}
    Accessor of the @code{in} slot of the @class{gdk:event-focus} structure.
  @end{short}
  @see-class{gdk:event-focus}")

(export 'event-focus-in)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventConfigure
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-configure)
      "GBoxed"
      (documentation 'event-configure 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when a window size or position has changed.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:configure) event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[x]{The new integer x coordinate of the window, relative to its
      parent.}
    @entry[y]{The new integer y coordinate of the window, relative to its
      parent.}
    @entry[width]{The new integer width of the window.}
    @entry[height]{The new integer height of the window.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-configure-x}
  @see-slot{gdk:event-configure-y}
  @see-slot{gdk:event-configure-width}
  @see-slot{gdk:event-configure-height}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-configure)

;;; --- event-configure-x --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-configure-x)
      "Accessor"
      (documentation 'event-configure-x 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-configure-x instance) => x}
  @syntax[]{(setf (gdk:event-configure-x instance) x)}
  @argument[instance]{a @class{gdk:event-configure} instance}
  @argument[x]{an integer with the new x coordinate of the window, relative
    to its parent}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-configure} structure.
  @end{short}
  @see-class{gdk:event-configure}")

(export 'event-configure-x)

;;; --- event-configure-y --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-configure-y)
      "Accessor"
      (documentation 'event-configure-y 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-configure-y instance) => y}
  @syntax[]{(setf (gdk:event-configure-y instance) y)}
  @argument[instance]{a @class{gdk:event-configure} instance}
  @argument[y]{an integer with the new y coordinate of the window, relative
    to its parent}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-configure} structure.
  @end{short}
  @see-class{gdk:event-configure}")

(export 'event-configure-y)

;;; --- event-configure-width ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-configure-width)
      "Accessor"
      (documentation 'event-configure-width 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-configure-width instance) => width}
  @syntax[]{(setf (gdk:event-configure-width instance) width)}
  @argument[instance]{a @class{gdk:event-configure} instance}
  @argument[width]{an integer with the new width of the window}
  @begin{short}
    Accessor of the @code{width} of the @class{gdk:event-configure} structure.
  @end{short}
  @see-class{gdk:event-configure}")

(export 'event-configure-width)

;;; --- event-configure-height ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-configure-height)
      "Accessor"
      (documentation 'event-configure-height 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-configure-height instance) => height}
  @syntax[]{(setf (gdk:event-configure-height instance) height)}
  @argument[instance]{a @class{gdk:event-configure} instance}
  @argument[height]{an integer with the new height of the window}
  @begin{short}
    Accessor of the @code{height} of the @class{gdk:event-configure} structure.
  @end{short}
  @see-class{gdk:event-configure}")

(export 'event-configure-height)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventProperty
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-property)
      "GBoxed"
      (documentation 'event-property 'type)
 "@version{#2021-12-13}
  @begin{short}
    Describes a property change on a window.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:property-notify) event-property
             (atom :pointer)
             (time :uint32)
             (state property-state))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[atom]{A @symbol{gdk:atom} property that was changed.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[state]{A value of the @symbol{gdk:property-state} enumeration.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-property-atom}
  @see-slot{gdk:event-property-time}
  @see-slot{gdk:event-property-state}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:property-state}")

(export 'event-property)

;;; --- event-property-atom ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-property-atom)
      "Accessor"
      (documentation 'event-property-atom 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-property-atom instance) => atom}
  @syntax[]{(setf (gdk:event-property-atom instance) atom)}
  @argument[instance]{a @class{gdk:event-property} instance}
  @argument[atom]{a @symbol{gdk:atom} property that was changed}
  @begin{short}
    Accessor of the @code{atom} slot of the @class{gdk:event-property}
    structure.
  @end{short}
  @see-class{gdk:event-property}
  @see-symbol{gdk:atom}")

(export 'event-property-atom)

;;; --- event-property-time ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-property-time)
      "Accessor"
      (documentation 'event-property-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-property-time instance) => time}
  @syntax[]{(setf (gdk:event-property-time instance) time)}
  @argument[instance]{a @class{gdk:event-property} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-property}
    structure.
  @end{short}
  @see-class{gdk:event-property}")

(export 'event-property-time)

;;; --- event-property-state -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-property-state)
      "Accessor"
      (documentation 'event-property-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-property-state instance) => state}
  @syntax[]{(setf (gdk:event-property-state instance) state)}
  @argument[instance]{a @class{gdk:event-property} instance}
  @argument[state]{a value of the @symbol{gdk:property-state} enumeration}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-property}
    structure.
  @end{short}
  @see-class{gdk:event-property}
  @see-symbol{gdk:property-state}")

(export 'event-property-state)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventSelection
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-selection)
      "GBoxed"
      (documentation 'event-selection 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when a selection is requested or ownership of a selection is
    taken over by another client application.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:selection-clear
              :selection-notify
              :selection-request) event-selection
             (selection gdk:atom)
             (target gdk:atom)
             (property gdk:atom)
             (time :uint32)
             (requestor (g:object window)))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[selection]{The @symbol{gdk:atom} selection.}
    @entry[target]{The @symbol{gdk:atom} target to which the selection should
      be converted.}
    @entry[property]{The @symbol{gdk:atom} property in which to place the
      result of the conversion.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[requestor]{The @class{gdk:window} object on which to place
      @arg{property} or @code{nil} if none.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-selection-selection}
  @see-slot{gdk:event-selection-target}
  @see-slot{gdk:event-selection-property}
  @see-slot{gdk:event-selection-time}
  @see-slot{gdk:event-selection-requestor}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-symbol{gdk:atom}
  @see-symbol{gdk:event-type}")

(export 'event-selection)

;;; --- event-selection-selection ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-selection-selection)
      "Accessor"
      (documentation 'event-selection-selection 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-selection-selection instance) => selection}
  @syntax[]{(setf (gdk:event-selection-selection instance) selection)}
  @argument[instance]{a @class{gdk:event-selection} instance}
  @argument[selection]{a @symbol{gdk:atom} with the selection}
  @begin{short}
    Accessor of the @code{selection} slot of the @class{gdk:event-selection}
    structure.
  @end{short}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:atom}")

(export 'event-selection-selection)

;;; --- event-selection-target ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-selection-target)
      "Accessor"
      (documentation 'event-selection-target 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-selection-target instance) => target}
  @syntax[]{(setf (gdk:event-selection-target instance) target)}
  @argument[instance]{a @class{gdk:event-selection} instance}
  @argument[selection]{a @symbol{gdk:atom} with the target to which the
    selection should be converted}
  @begin{short}
    Accessor of the @code{target} slot of the @class{gdk:event-selection}
    structure.
  @end{short}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:atom}")

(export 'event-selection-target)

;;; --- event-selection-property -------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-selection-property)
      "Accessor"
      (documentation 'event-selection-property 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-selection-property instance) => property}
  @syntax[]{(setf (gdk:event-selection-property instance) property)}
  @argument[instance]{a @class{gdk:event-selection} instance}
  @argument[property]{a @symbol{gdk:atom} with the property in which to place
    the result of the conversion}
  @begin{short}
    Accessor of the @code{property} slot of the @class{gdk:event-selection}
    structure.
  @end{short}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:atom}")

(export 'event-selection-property)

;;; --- event-selection-time -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-selection-time)
      "Accessor"
      (documentation 'event-selection-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-selection-time instance) => time}
  @syntax[]{(setf (gdk:event-selection-time instance) time)}
  @argument[instance]{a @class{gdk:event-selection} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-selection}
    structure.
  @end{short}
  @see-class{gdk:event-selection}")

(export 'event-selection-time)

;;; --- event-selection-requestor ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-selection-requestor)
      "Accessor"
      (documentation 'event-selection-requestor 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-selection-requestor instance) => requestor}
  @syntax[]{(setf (gdk:event-selection-requestor instance) requestor)}
  @argument[instance]{a @class{gdk:event-selection} instance}
  @argument[requestor]{a @class{gdk:window} object on which to place the
    property}
  @begin{short}
    Accessor of the @code{requestor} slot of the @class{gdk:event-selection}
    structure.
  @end{short}
  @see-class{gdk:event-selection}
  @see-symbol{gdk:window}")

(export 'event-selection-requestor)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventDND
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-dnd)
      "GBoxed"
      (documentation 'event-dnd 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated during DND operations.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) event-dnd
             (context (g:object drag-context))
             (time :uint32)
             (x-root :short)
             (y-root :short))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[context]{The @class{gdk:drag-context} object for the current DND
      operation.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x-root]{A short integer with the x coordinate of the pointer
      relative to the root of the screen, only set for @code{:drag-motion} and
      @code{:drop-start} events.}
    @entry[y-root]{A short integer with the y coordinate of the pointer
      relative to the root of the screen, only set for @code{:drag-motion} and
      @code{:drop-start} events.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-dnd-context}
  @see-slot{gdk:event-dnd-time}
  @see-slot{gdk:event-dnd-x-root}
  @see-slot{gdk:event-dnd-y-root}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-class{gdk:drag-context}
  @see-symbol{gdk:event-type}")

(export 'event-dnd)

;;; --- event-dnd-context --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-dnd-context)
      "Accessor"
      (documentation 'event-dnd-context 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-dnd-context instance) => context}
  @syntax[]{(setf (gdk:event-dnd-context instance) context)}
  @argument[instance]{a @class{gdk:event-dnd} instance}
  @argument[context]{a @class{gdk:drag-context} object for the current DND
    operation}
  @begin{short}
    Accessor of the @code{context} slot of the @class{gdk:event-dnd}
    structure.
  @end{short}
  @see-class{gdk:event-dnd}
  @see-class{gdk:drag-context}")

(export 'event-dnd-context)

;;; --- event-dnd-time -----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-dnd-time)
      "Accessor"
      (documentation 'event-dnd-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-dnd-time instance) => time}
  @syntax[]{(setf (gdk:event-dnd-time instance) time)}
  @argument[instance]{a @class{gdk:event-dnd} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-dnd}
    structure.
  @end{short}
  @see-class{gdk:event-dnd}")

(export 'event-dnd-time)

;;; --- event-dnd-x-root ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-dnd-x-root)
      "Accessor"
      (documentation 'event-dnd-x-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-dnd-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-dnd-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-dnd} instance}
  @argument[x-root]{a short integer with the x coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-dnd} structure.
  @end{short}
  @see-class{gdk:event-dnd}")

(export 'event-dnd-x-root)

;;; --- event-dnd-y-root ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-dnd-y-root)
      "Accessor"
      (documentation 'event-dnd-y-root 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-dnd-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-dnd-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-dnd} instance}
  @argument[y-root]{a short integer with the y coordinate of the pointer
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-dnd} structure.
  @end{short}
  @see-class{gdk:event-dnd}")

(export 'event-dnd-y-root)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventProximity
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-proximity)
      "GBoxed"
      (documentation 'event-proximity 'type)
 "@version{#2021-12-13}
  @begin{short}
    Proximity events are generated when using the wrapper for the XInput
    extension for GDK.
  @end{short}
  The XInput extension is an add-on for standard X that allows you to use
  nonstandard devices such as graphics tablets. A proximity event indicates
  that the stylus has moved in or out of contact with the tablet, or perhaps
  that the finger of the user has moved in or out of contact with a touch
  screen.

  This event type will be used pretty rarely. It only is important for XInput
  aware programs that are drawing their own cursor.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:proximity-in
              :proximity-out) event-proximity
             (time :uint32)
             (device (g:object device)))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[device]{The @class{gdk:device} object where the event originated.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-proximity-time}
  @see-slot{gdk:event-proximity-device}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-symbol{gdk:event-type}")

(export 'event-proximity)

;;; --- event-proximity-time -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-proximity-time)
      "Accessor"
      (documentation 'event-proximity-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-proximity-time instance) => time}
  @syntax[]{(setf (gdk:event-proximity-time instance) time)}
  @argument[instance]{a @class{gdk:event-proximity} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-proximity}
    structure.
  @end{short}
  @see-class{gdk:event-proximity}")

(export 'event-proximity-time)

;;; --- event-proximity-device ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-proximity-device)
      "Accessor"
      (documentation 'event-proximity-device 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-proximity-device instance) => device}
  @syntax[]{(setf (gdk:event-proximity-device instance) device)}
  @argument[instance]{a @class{gdk:event-proximity} instance}
  @argument[device]{a @class{gdk:device} master device that the event
    originated from}
  @begin{short}
    Accessor of the @code{device} slot of the @class{gdk:event-proximity}
    structure.
  @end{short}
  @see-class{gdk:event-proximity}
  @see-class{gdk:device}")

(export 'event-proximity-device)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventWindowState
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-window-state)
      "GBoxed"
      (documentation 'event-window-state 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when the state of a toplevel window changes.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:window-state) event-window-state
             (changed-mask window-state)
             (new-window-state window-state))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[changed-mask]{The @symbol{window-state} mask specifying what
      flags have changed.}
    @entry[new-window-state]{The new window state, a combination of
      @symbol{gdk:window-state} bits.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-window-state-changed-mask}
  @see-slot{gdk:event-window-state-new-window-state}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:window-state}")

(export 'event-window-state)

;;; --- event-window-state-changed-mask ------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-window-state-changed-mask)
      "Accessor"
      (documentation 'event-window-state-changed-mask 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-window-state-changed-mask instance) => mask}
  @syntax[]{(setf (gdk:event-window-state-changed-mask instance) mask)}
  @argument[instance]{a @class{gdk:event-window-state} instance}
  @argument[mask]{a @symbol{gdk:window-state} mask specifying what flags have
    changed}
  @begin{short}
    Accessor of the  @code{changed-mask} slot of the
    @class{gdk:event-window-state} structure.
  @end{short}
  @see-class{gdk:event-window-state}
  @see-symbol{gdk:window-state}")

(export 'event-window-state-changed-mask)

;;; --- event-window-state-new-window-state --------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-window-state-new-window-state)
      "Accessor"
      (documentation 'event-window-state-new-window-state 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-window-state-new-window-state instance) => state}
  @syntax[]{(setf (gdk:event-window-state-new-window-state instance) state)}
  @argument[instance]{a @class{gdk:event-window-state} instance}
  @argument[state]{a new @symbol{gdk:window-state} mask}
  @begin{short}
    Accessor of the @code{new-window-state} slot of the
    @class{gdk:event-window-state} structure.
  @end{short}
  @see-class{gdk:event-window-state}
  @see-symbol{gdk:window-state}")

(export 'event-window-state-new-window-state)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventSetting
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-setting)
      "GBoxed"
      (documentation 'event-setting 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when a setting is modified.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:setting) event-setting
             (action setting-action)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[action]{What happened to the setting as a value of the
      @symbol{gdk:setting-action} enumeration.}
    @entry[name]{A string with the name of the setting.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-setting-action}
  @see-slot{gdk:event-setting-name}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-symbol{gdk:event-type}
  @see-symbol{gdk:setting-action}")

(export 'event-setting)

;;; --- event-setting-action -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-setting-action)
      "Accessor"
      (documentation 'event-setting-action 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-setting-action instance) => action}
  @syntax[]{(setf (gdk:event-setting-action instance) action)}
  @argument[instance]{a @class{gdk:event-setting} instance}
  @argument[action]{a @symbol{gdk:setting-action} value}
  @begin{short}
    Accessor of the @code{action} slot of the @class{gdk:event-setting}
    structure.
  @end{short}
  @see-class{gdk:event-setting}
  @see-symbol{gdk:setting-action}")

(export 'event-setting-action)

;;; --- event-setting-name -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-setting-name)
      "Accessor"
      (documentation 'event-setting-name 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-setting-name instance) => name}
  @syntax[]{(setf (gdk:event-setting-name instance) name)}
  @argument[instance]{a @class{gdk:event-setting} instance}
  @argument[name]{a string with the name of the setting}
  @begin{short}
    Accessor of the @code{name} slot of the @class{gdk:event-setting}
    structure.
  @end{short}
  @see-class{gdk:event-setting}")

(export 'event-setting-name)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventOwnerChange
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-owner-change)
      "GBoxed"
      (documentation 'event-owner-change 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when the owner of a selection changes.
  @end{short}
  On X11, this information is only available if the X server supports the
  XFIXES extension.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:owner-change) event-owner-change
             (owner (g:object window))
             (reason owner-change)
             (selection gdk:atom)
             (time :uint32)
             (selection-time :uint32))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[owner]{The new @class{gdk:window} owner of the selection, or
      @code{nil} if there is none.}
    @entry[reason]{The reason for the ownership change as a
      @symbol{gdk:owner-change} value.}
    @entry[selection]{The @symbol{gdk:atom} identifying the selection.}
    @entry[time]{An unsigned integer with the timestamp of the event.}
    @entry[selection-time]{An unsigned integer with the time at which the
      selection ownership was taken over.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-owner-change-owner}
  @see-slot{gdk:event-owner-change-reason}
  @see-slot{gdk:event-owner-change-selection}
  @see-slot{gdk:event-owner-change-time}
  @see-slot{gdk:event-owner-change-selection-time}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-symbol{gdk:atom}
  @see-symbol{gdk:event-type}")

(export 'event-owner-change)

;;; --- event-owner-change-owner -------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-owner-change-owner)
      "Accessor"
      (documentation 'event-owner-change-owner 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-owner-change-owner instance) => owner}
  @syntax[]{(setf (gdk:event-owner-change-owner instance) owner)}
  @argument[instance]{a @class{gdk:event-owner-change} instance}
  @argument[owner]{a @class{gdk:window} owner of the selection}
  @begin{short}
    Accessor of the @code{owner} slot of the @class{gdk:event-owner-change}
    structure.
  @end{short}
  @see-class{gdk:event-owner-change}
  @see-class{gdk:window}")

(export 'event-owner-change-owner)

;;; --- event-owner-change-reason ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-owner-change-reason)
      "Accessor"
      (documentation 'event-owner-change-reason 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-owner-change-reason instance) => reason}
  @syntax[]{(setf (gdk:event-owner-change-reason instance) reason)}
  @argument[instance]{a @class{gdk:event-owner-change} instance}
  @argument[reason]{a reason for the ownership change as a
    @symbol{gdk:owner-change} value}
  @begin{short}
    Accessor of the @code{reason} slot of the @class{gdk:event-owner-change}
    structure.
  @end{short}
  @see-class{gdk:event-owner-change}
  @see-symbol{gdk:owner-change}")

(export 'event-owner-change-reason)

;;; --- event-owner-change-selection ---------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-owner-change-selection)
      "Accessor"
      (documentation 'event-owner-change-selection 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-owner-change-selection instance) => selection}
  @syntax[]{(setf (gdk:event-owner-change-selection instance) selection)}
  @argument[instance]{a @class{gdk:event-owner-change} instance}
  @argument[selection]{a @symbol{gdk:atom} identifying the selection}
  @begin{short}
    Accessor of the @code{selection} slot of the @class{gdk:event-owner-change}
    structure.
  @end{short}
  @see-class{gdk:event-owner-change}
  @see-symbol{gdk:atom}")

(export 'event-owner-change-selection)

;;; --- event-owner-change-time --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-owner-change-time)
      "Accessor"
      (documentation 'event-owner-change-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-owner-change-time instance) => time}
  @syntax[]{(setf (gdk:event-owner-change-time instance) time)}
  @argument[instance]{a @class{gdk:event-owner-change} instance}
  @argument[time]{an unsigned integer with the timestamp of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-owner-change}
    structure.
  @end{short}
  @see-class{gdk:event-owner-change}")

(export 'event-owner-change-time)

;;; --- event-owner-change-selection-time ----------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-owner-change-selection-time)
      "Accessor"
      (documentation 'event-owner-change-selection-time 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-owner-change-selection-time instance) => time}
  @syntax[]{(setf (gdk:event-owner-change-selection-time instance) time)}
  @argument[instance]{a @class{gdk:event-owner-change} instance}
  @argument[time]{an unsigned integer with the time at which the selection
    ownership was taken over}
  @begin{short}
    Accessor of the @code{selection-time} slot of the
    @class{gdk:event-owner-change} structure.
  @end{short}
  @see-class{gdk:event-owner-change}")

(export 'event-owner-change-selection-time)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventGrabBroken
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-grab-broken)
      "GBoxed"
      (documentation 'event-grab-broken 'type)
 "@version{#2021-12-13}
  @begin{short}
    Generated when a pointer or keyboard grab is broken.
  @end{short}
  On X11, this happens when the grab window becomes unviewable, i.e. it or one
  of its ancestors is unmapped, or if the same application grabs the pointer
  or keyboard again. Note that implicit grabs, which are initiated by button
  presses, can also cause @class{gdk:event-grab-broken} events.
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
            ...
            ((:grab-broken) event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g:object window)))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event.}
    @entry[window]{The @class{gdk:window} object which received the event,
      i.e. the window that previously owned the grab.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[keyboard]{@em{True} if a keyboard grab was broken, @em{false} if a
      pointer grab was broken.}
    @entry[implicit]{@code{True} if the broken grab was implicit.}
    @entry[grab-window]{If this event is caused by another grab in the same
      application, @arg{grab-window} contains the new @class{gdk:window} grab
      window. Otherwise @arg{grab-window} is @code{nil}.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-grab-broken-keyboard}
  @see-slot{gdk:event-grab-broken-implicit}
  @see-slot{gdk:event-grab-broken-grab-window}
  @see-class{gdk:event}
  @see-class{gdk:window}
  @see-symbol{gdk:event-type}")

(export 'event-grab-broken)

;;; --- event-grab-broken-keyboard -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-grab-broken-keyboard)
      "Accessor"
      (documentation 'event-grab-broken-keyboard 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-grab-broken-keyboard instance) => keyboard}
  @syntax[]{(setf (gdk:event-grab-broken-keyboard instance) keyboard)}
  @argument[instance]{a @class{gdk:event-grab-broken} instance}
  @argument[keyboard]{@em{true} if a keyboard grab was broken, @em{false}
    if a pointer grab was broken}
  @begin{short}
    Accessor of the @code{keyboard} slot of the @class{gdk:event-grab-broken}
    structure.
  @end{short}
  @see-class{gdk:event-grab-broken}")

(export 'event-grab-broken-keyboard)

;;; --- event-grab-broken-implicit -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-grab-broken-implicit)
      "Accessor"
      (documentation 'event-grab-broken-implicit 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-grab-broken-implicit instance) => implicit}
  @syntax[]{(setf (gdk:event-grab-broken-implicit instance) implicit)}
  @argument[instance]{a @class{gdk:event-grab-broken} instance}
  @argument[implicit]{@em{true} if the broken grab was implicit}
  @begin{short}
    Accessor of the @code{implicit} slot of the @class{gdk:event-grab-broken}
    structure.
  @end{short}
  @see-class{gdk:event-grab-broken}")

(export 'event-grab-broken-implicit)

;;; --- event-grab-broken-grab-window --------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-grab-broken-grab-window)
      "Accessor"
      (documentation 'event-grab-broken-grab-window 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:event-grab-broken-grab-window instance) => grab-window}
  @syntax[]{(setf (gdk:event-grab-broken-grab-window instance) grab-window)}
  @argument[instance]{a @class{gdk:event-grab-broken} instance}
  @argument[grab-window]{if this event is caused by another grab in the same
    application, @arg{grab-window} contains the new @class{gdk:window} grab
    window}
  @begin{short}
    Accessor of the @code{grab-window} slot of the @class{gdk:event-grab-broken}
    structure.
  @end{short}
  @see-class{gdk:event-grab-broken}
  @see-class{gdk:window}")

(export 'event-grab-broken-grab-window)

;;; ----------------------------------------------------------------------------
;;; GdkEventTouchpadSwipe
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-touchpad-swipe)
      "GBoxed"
      (documentation 'event-touchpad-swipe 'type)
 "@version{#2023-3-13}
  @begin{short}
    Generated during touchpad swipe gestures.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ((:touchpad-swipe) event-touchpad-swipe
             (phase :int8)
             (n-fingers :int8)
             (time :uint32)
             (x :double)
             (y :double)
             (dx :double)
             (dy :double)
             (x-root :double)
             (y-root :double)
             (state modifier-type))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} type of the event, the
      @code{:touchpad-swipe} value.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[phase]{An integer with the current phase of the gesture.}
    @entry[n-fingers]{An integer with the number of fingers triggering the
      swipe.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x]{The double float x coordinate of the pointer.}
    @entry[y]{The double float y coordinate of the pointer.}
    @entry[dx]{A double float with the movement delta in the x axis of the
      swipe focal point.}
    @entry[dy]{A double float with the movement delta in the y axis of the
      swipe focal point.}
    @entry[x-root]{The double float x coordinate of the pointer, relative to
      the root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer, relative to
      the root of the screen.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-touchpad-swipe-phase}
  @see-slot{gdk:event-touchpad-swipe-n-fingers}
  @see-slot{gdk:event-touchpad-swipe-time}
  @see-slot{gdk:event-touchpad-swipe-x}
  @see-slot{gdk:event-touchpad-swipe-y}
  @see-slot{gdk:event-touchpad-swipe-dx}
  @see-slot{gdk:event-touchpad-swipe-dy}
  @see-slot{gdk:event-touchpad-swipe-x-root}
  @see-slot{gdk:event-touchpad-swipe-y-root}
  @see-slot{gdk:event-touchpad-swipe-state}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-touchpad-swipe)

;;; --- event-touchpad-swipe-phase ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-phase)
      "Accessor"
      (documentation 'event-touchpad-swipe-phase 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-phase instance) => phase}
  @syntax[]{(setf (gdk:event-touchpad-swipe-phase instance) phase)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[phase]{an integer with the current phase of the gesture}
  @begin{short}
    Accessor of the @code{phase} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-phase)

;;; --- event-touchpad-swipe-n-fingers -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-n-fingers)
      "Accessor"
      (documentation 'event-touchpad-swipe-n-fingers 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-n-fingers instance) => n-fingers}
  @syntax[]{(setf (gdk:event-touchpad-swipe-n-fingers instance) n-fingers)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[n-fingers]{an integer with the number of fingers triggering the
    swipe}
  @begin{short}
    Accessor of the @code{n-fingers} slot of the
    @class{gdk:event-touchpad-swipe} structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-n-fingers)

;;; --- event-touchpad-swipe-time ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-time)
      "Accessor"
      (documentation 'event-touchpad-swipe-time 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-time instance) => time}
  @syntax[]{(setf (gdk:event-touchpad-swipe-time instance) time)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-time)

;;; --- event-touchpad-swipe-x -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-x)
      "Accessor"
      (documentation 'event-touchpad-swipe-x 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-x instance) => x}
  @syntax[]{(setf (gdk:event-touchpad-swipe-x instance) x)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[x]{a double float with the x coordinate of the pointer}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-x)

;;; --- event-touchpad-swipe-y -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-y)
      "Accessor"
      (documentation 'event-touchpad-swipe-y 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-y instance) => y}
  @syntax[]{(setf (gdk:event-touchpad-swipe-y instance) y)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[y]{a double float with the y coordinate of the pointer}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-y)

;;; --- event-touchpad-swipe-dx ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-dx)
      "Accessor"
      (documentation 'event-touchpad-swipe-dx 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-dx instance) => dx}
  @syntax[]{(setf (gdk:event-touchpad-swipe-dx instance) dx)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[dx]{a double float with the movement delta in the x axis of the
    swipe focal point}
  @begin{short}
    Accessor of the @code{dx} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-dx)

;;; --- event-touchpad-swipe-dy ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-dy)
      "Accessor"
      (documentation 'event-touchpad-swipe-dy 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-dy instance) => dy}
  @syntax[]{(setf (gdk:event-touchpad-swipe-dy instance) dy)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[dy]{a double float with the movement delta in the y axis of the
    swipe focal point}
  @begin{short}
    Accessor of the @code{dy} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-dy)

;;; --- event-touchpad-swipe-x-root --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-x-root)
      "Accessor"
      (documentation 'event-touchpad-swipe-x-root 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-touchpad-swipe-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer,
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-x-root)

;;; --- event-touchpad-swipe-y-root --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-y-root)
      "Accessor"
      (documentation 'event-touchpad-swipe-y-root 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-touchpad-swipe-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer,
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}")

(export 'event-touchpad-swipe-y-root)

;;; --- event-touchpad-swipe-state ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-swipe-state)
      "Accessor"
      (documentation 'event-touchpad-swipe-state 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-swipe-state instance) => state}
  @syntax[]{(setf (gdk:event-touchpad-swipe-state instance) state)}
  @argument[instance]{a @class{gdk:event-touchpad-swipe} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bitmask representing the state
    of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and @kbd{Alt}
    keys, and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-touchpad-swipe}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-swipe}
  @see-symbol{gdk:modifier-type}")

(export 'event-touchpad-swipe-state)

;;; ----------------------------------------------------------------------------
;;; GdkEventTouchpadPinch
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-touchpad-pinch)
      "GBoxed"
      (documentation 'event-touchpad-pinch 'type)
 "@version{#2023-3-13}
  @begin{short}
    Generated during touchpad swipe gestures.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ;; GdkEventTouchpadPinch
            ((:touchpad-pinch) event-touchpad-pinch
             (phase :int8)
             (n-fingers :int8)
             (time :uint32)
             (x :double)
             (y :double)
             (dx :double)
             (dy :double)
             (angle-delta :double)
             (scale :double)
             (x-root :double)
             (y-root :double)
             (state modifier-type))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} of the event, the
      @code{:touchpad-pinch} value.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[phase]{The current phase of the gesture.}
    @entry[n-fingers]{The number of fingers triggering the swipe.}
    @entry[time]{An unsigned integer with the time of the event in
      milliseconds.}
    @entry[x]{The double float x coordinate of the pointer.}
    @entry[y]{The double float y coordinate of the pointer.}
    @entry[dx]{A double float movement delta in the x axis of the swipe focal
      point.}
    @entry[dy]{A double float movement delta in the y axis of the swipe focal
      point.}
    @entry[angle-delta]{A double float with the angle change in radians,
      negative angles denote counter-clockwise movements.}
    @entry[scale]{A double float with the current scale, relative to that at
      the time of the corresponding @code{:begin} value of the
      @symbol{gdk:touchpad-gesture-phase} enumeration.}
    @entry[x-root]{The double float x coordinate of the pointer, relative to
      the root of the screen.}
    @entry[y-root]{The double float y coordinate of the pointer, relative to
      the root of the screen.}
    @entry[state]{The @symbol{gdk:modifier-type} bit-mask representing the
      state of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and
      @kbd{Alt} keys, and the pointer buttons.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-touchpad-pinch-phase}
  @see-slot{gdk:event-touchpad-pinch-n-fingers}
  @see-slot{gdk:event-touchpad-pinch-time}
  @see-slot{gdk:event-touchpad-pinch-x}
  @see-slot{gdk:event-touchpad-pinch-y}
  @see-slot{gdk:event-touchpad-pinch-dx}
  @see-slot{gdk:event-touchpad-pinch-dy}
  @see-slot{gdk:event-touchpad-pinch-angle-delta}
  @see-slot{gdk:event-touchpad-pinch-scale}
  @see-slot{gdk:event-touchpad-pinch-x-root}
  @see-slot{gdk:event-touchpad-pinch-y-root}
  @see-slot{gdk:event-touchpad-pinch-state}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-touchpad-pinch)

;;; --- event-touchpad-pinch-phase ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-phase)
      "Accessor"
      (documentation 'event-touchpad-pinch-phase 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-phase instance) => phase}
  @syntax[]{(setf (gdk:event-touchpad-pinch-phase instance) phase)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[phase]{an integer with the current phase of the gesture}
  @begin{short}
    Accessor of the @code{phase} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-phase)

;;; --- event-touchpad-pinch-n-fingers -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-n-fingers)
      "Accessor"
      (documentation 'event-touchpad-pinch-n-fingers 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-n-fingers instance) => n-fingers}
  @syntax[]{(setf (gdk:event-touchpad-pinch-n-fingers instance) n-fingers)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[n-fingers]{an integer with the number of fingers triggering the
    event}
  @begin{short}
    Accessor of the @code{n-fingers} slot of the
    @class{gdk:event-touchpad-pinch} structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-n-fingers)

;;; --- event-touchpad-pinch-time ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-time)
      "Accessor"
      (documentation 'event-touchpad-pinch-time 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-time instance) => time}
  @syntax[]{(setf (gdk:event-touchpad-pinch-time instance) time)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-time)

;;; --- event-touchpad-pinch-x -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-x)
      "Accessor"
      (documentation 'event-touchpad-pinch-x 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-x instance) => x}
  @syntax[]{(setf (gdk:event-touchpad-pinch-x instance) x)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[x]{a double float with the x coordinate of the pointer}
  @begin{short}
    Accessor of the @code{x} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-x)

;;; --- event-touchpad-pinch-y -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-y)
      "Accessor"
      (documentation 'event-touchpad-pinch-y 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-y instance) => y}
  @syntax[]{(setf (gdk:event-touchpad-pinch-y instance) y)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[y]{a double float with the y coordinate of the pointer}
  @begin{short}
    Accessor of the @code{y} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-y)

;;; --- event-touchpad-pinch-dx ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-dx)
      "Accessor"
      (documentation 'event-touchpad-pinch-dx 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-dx instance) => dx}
  @syntax[]{(setf (gdk:event-touchpad-pinch-dx instance) dx)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[dx]{a double float with the movement delta in the x axis of the
    swipe focal point}
  @begin{short}
    Accessor of the @code{dx} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-dx)

;;; --- event-touchpad-pinch-dy ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-dy)
      "Accessor"
      (documentation 'event-touchpad-pinch-dy 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-dy instance) => dy}
  @syntax[]{(setf (gdk:event-touchpad-pinch-dy instance) dy)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[dy]{a double float with the movement delta in the y axis of the
    swipe focal point}
  @begin{short}
    Accessor of the @code{dy} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-dy)

;;; --- event-touchpad-pinch-angle-delta ---------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-angle-delta)
      "Accessor"
      (documentation 'event-touchpad-pinch-angle-delta 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-angle-delta instance) => angle-delta}
  @syntax[]{(setf (gdk:event-touchpad-pinch-angle-delta instance) angle-delta)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[angle-delta]{a double float with the angle change in radians,
    negative anlges denote counter-clockwise movements}
  @begin{short}
    Accessor of the @code{angle-delta} slot of the
    @class{gdk:event-touchpad-pinch} structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-angle-delta)

;;; --- event-touchpad-pinch-scale ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-scale)
      "Accessor"
      (documentation 'event-touchpad-pinch-scale 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-scale instance) => scale}
  @syntax[]{(setf (gdk:event-touchpad-pinch-scale instance) scale)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[scale]{a double float with the current scale, relative to that at
    the time of the corresponding @code{:begin} value of the
    @symbol{gdk:touchpad-gesture-phase} enumeration}
  @begin{short}
    Accessor of the @code{scale} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-scale)

;;; --- event-touchpad-pinch-x-root --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-x-root)
      "Accessor"
      (documentation 'event-touchpad-pinch-x-root 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-x-root instance) => x-root}
  @syntax[]{(setf (gdk:event-touchpad-pinch-x-root instance) x-root)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[x-root]{a double float with the x coordinate of the pointer,
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{x-root} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-x-root)

;;; --- event-touchpad-pinch-y-root --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-y-root)
      "Accessor"
      (documentation 'event-touchpad-pinch-y-root 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-y-root instance) => y-root}
  @syntax[]{(setf (gdk:event-touchpad-pinch-y-root instance) y-root)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[y-root]{a double float with the y coordinate of the pointer,
    relative to the root of the screen}
  @begin{short}
    Accessor of the @code{y-root} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}")

(export 'event-touchpad-pinch-y-root)

;;; --- event-touchpad-pinch-state ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-touchpad-pinch-state)
      "Accessor"
      (documentation 'event-touchpad-pinch-state 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-touchpad-pinch-state instance) => state}
  @syntax[]{(setf (gdk:event-touchpad-pinch-state instance) state)}
  @argument[instance]{a @class{gdk:event-touchpad-pinch} instance}
  @argument[state]{a @symbol{gdk:modifier-type} bitmask representing the state
    of the modifier keys, e.g. the @kbd{Control}, @kbd{Shift} and @kbd{Alt}
    keys, and the pointer buttons}
  @begin{short}
    Accessor of the @code{state} slot of the @class{gdk:event-touchpad-pinch}
    structure.
  @end{short}
  @see-class{gdk:event-touchpad-pinch}
  @see-symbol{gdk:modifier-type}")

(export 'event-touchpad-pinch-state)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventPadButton
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-pad-button)
      "GBoxed"
      (documentation 'event-pad-button 'type)
 "@version{#2023-3-13}
  @begin{short}
    Generated during @code{:tablet-pad} button presses and releases.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ;; GdkEventPadButton
            ((:pad-button-press :pad-button-release) event-pad-button
             (time :uint32)
             (group :uint)
             (button :uint)
             (mode :uint))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} of the event, one of the
      @code{:pad-button-press}, @code{:pad-button-release} values.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[group]{The pad group the button belongs to. A @code{:tablet-pad}
      device may have one or more groups containing a set of
      buttons/rings/strips each.}
    @entry[button]{The pad button that was pressed.}
    @entry[mode]{The current mode of @arg{group}. Different groups in a
      @code{:tablet-pad} device may have different current modes.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-pad-button-time}
  @see-slot{gdk:event-pad-button-group}
  @see-slot{gdk:event-pad-button-button}
  @see-slot{gdk:event-pad-button-mode}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-pad-button)

;;; --- event-pad-button-time --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-button-time)
      "Accessor"
      (documentation 'event-pad-button-time 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-button-time instance) => time}
  @syntax[]{(setf (gdk:event-pad-button-time instance) time)}
  @argument[instance]{a @class{gdk:event-pad-button} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-pad-button}
    structure.
  @end{short}
  @see-class{gdk:event-pad-button}")

(export 'event-pad-button-time)

;;; --- event-pad-button-group -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-button-group)
      "Accessor"
      (documentation 'event-pad-button-group 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-button-group instance) => group}
  @syntax[]{(setf (gdk:event-pad-button-group instance) group)}
  @argument[instance]{a @class{gdk:event-pad-button} instance}
  @argument[group]{an unsigned integer with the pad group the button belongs
    to, a @code{:tablet-pad} device may have one or more groups containing a
    set of buttons/ring/strips each}
  @begin{short}
    Accessor of the @code{group} slot of the @class{gdk:event-pad-button}
    structure.
  @end{short}
  @see-class{gdk:event-pad-button}")

(export 'event-pad-button-group)

;;; --- event-pad-button-button ------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-button-button)
      "Accessor"
      (documentation 'event-pad-button-button 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-button-button instance) => button}
  @syntax[]{(setf (gdk:event-pad-button-button instance) button)}
  @argument[instance]{a @class{gdk:event-pad-button} instance}
  @argument[button]{an unsigned integer with the pad button that was pressed}
  @begin{short}
    Accessor of the @code{button} slot of the @class{gdk:event-pad-button}
    structure.
  @end{short}
  @see-class{gdk:event-pad-button}")

(export 'event-pad-button-button)

;;; --- event-pad-button-mode --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-button-mode)
      "Accessor"
      (documentation 'event-pad-button-mode 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-button-mode instance) => mode}
  @syntax[]{(setf (gdk:event-pad-button-mode instance) mode)}
  @argument[instance]{a @class{gdk:event-pad-button} instance}
  @argument[mode]{an unsigned integer with the current mode of the pad group,
    different groups in a @code{:tablet-pad} device may have different current
    modes}
  @begin{short}
    Accessor of the @code{mode} slot of the @class{gdk:event-pad-button}
    structure.
  @end{short}
  @see-class{gdk:event-pad-button}")

(export 'event-pad-button-mode)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventPadAxis
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-pad-axis)
      "GBoxed"
      (documentation 'event-pad-axis 'type)
 "@version{#2023-3-13}
  @begin{short}
    Generated during @code{:tablet-pad} interaction with tactile sensors.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ;; GdkEventPadAxis
            ((:pad-ring :pad-strip) event-pad-axis
             (time :uint32)
             (group :uint)
             (index :uint)
             (mode :uint)
             (value :double))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} of the event, one of the
      @code{:pad-ring}, @code{:pad-strip} values.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[group]{The pad group the ring/strip belongs to. A @code{:tablet-pad}
      device may have one or more groups containing a set of
      buttons/rings/strips each.}
    @entry[index]{Number of strip/ring that was interacted. This number is
      zero-indexed.}
    @entry[mode]{The current mode of @arg{group}. Different groups in a
      @code{:tablet-pad} device may have different current modes.}
    @entry[:value]{The current value for the given axis.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-pad-axis-time}
  @see-slot{gdk:event-pad-axis-group}
  @see-slot{gdk:event-pad-axis-index}
  @see-slot{gdk:event-pad-axis-mode}
  @see-slot{gdk:event-pad-axis-value}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-pad-axis)

;;; --- event-pad-axis-time ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-axis-time)
      "Accessor"
      (documentation 'event-pad-axis-time 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-axis-time instance) => time}
  @syntax[]{(setf (gdk:event-pad-axis-time instance) time)}
  @argument[instance]{a @class{gdk:event-pad-axis} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} of the @class{gdk:event-pad-axis}
    structure.
  @end{short}
  @see-class{gdk:event-pad-axis}")

(export 'event-pad-axis-time)

;;; --- event-pad-axis-group ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-axis-group)
      "Accessor"
      (documentation 'event-pad-axis-group 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-axis-group instance) => group}
  @syntax[]{(setf (gdk:event-pad-axis-group instance) group)}
  @argument[instance]{a @class{gdk:event-pad-axis} instance}
  @argument[group]{an unsigned integer with the pad group the ring/strip belongs
    to. A @code{:table-pad} device may have one or more groups containing a set
    of buttons/rings/strips each}
  @begin{short}
    Accessor of the @code{group} slot of the @class{gdk:event-pad-axis}
    structure.
  @end{short}
  @see-class{gdk:event-pad-axis}")

(export 'event-pad-axis-group)

;;; --- event-pad-axis-index ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-axis-index)
      "Accessor"
      (documentation 'event-pad-axis-index 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-axis-index instance) => index}
  @syntax[]{(setf (gdk:event-pad-axis-index instance) index)}
  @argument[instance]{a @class{gdk:event-pad-axis} instance}
  @argument[index]{an unsigned integer with the number of strip/ring that was
    interacted, this number is zero-indexed}
  @begin{short}
    Accessor of the @code{index} slot of the @class{gdk:event-pad-axis}
    structure.
  @end{short}
  @see-class{gdk:event-pad-axis}")

(export 'event-pad-axis-index)

;;; --- event-pad-axis-mode ----------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-axis-mode)
      "Accessor"
      (documentation 'event-pad-axis-mode 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-axis-mode instance) => mode}
  @syntax[]{(setf (gdk:event-pad-axis-mode instance) mode)}
  @argument[instance]{a @class{gdk:event-pad-axis} instance}
  @argument[mode]{an unsigned integer with the currrent mode of group, different
    groups in a @code{:tablet-pad} device may have a different current modes}
  @begin{short}
    Accessor of the @code{mode} slot of the @class{gdk:event-pad-axis}
    structure.
  @end{short}
  @see-class{gdk:event-pad-axis}")

(export 'event-pad-axis-mode)

;;; --- event-pad-axis-value ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-axis-value)
      "Accessor"
      (documentation 'event-pad-axis-value 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-axis-value instance) => value}
  @syntax[]{(setf (gdk:event-pad-axis-value instance) value)}
  @argument[instance]{a @class{gdk:event-pad-axis} instance}
  @argument[value]{a double float with the currrent value for the given axis}
  @begin{short}
    Accessor of the @code{value} slot of the @class{gdk:event-pad-axis}
    structure.
  @end{short}
  @see-class{gdk:event-pad-axis}")

(export 'event-pad-axis-value)

;;; ----------------------------------------------------------------------------
;;; struct GdkEventPadGroupMode
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-class 'event-pad-group-mode)
      "GBoxed"
      (documentation 'event-pad-group-mode 'type)
 "@version{#2023-3-13}
  @begin{short}
    Generated during @code{:tablet-pad} mode switches in a group.
  @end{short}
  @begin{pre}
(define-g-boxed-variant-cstruct event \"GdkEvent\"
  (type event-type)
  (window (g:object window))
  (send-event (:boolean :int8))
  (:variant type
            ...
            ;; GdkEventPadGroupMode
            ((:pad-group-mode) event-pad-group-mode
             (time :uint32)
             (group :uint)
             (mode :uint))
  ... ))
  @end{pre}
  @begin[code]{table}
    @entry[type]{The @symbol{gdk:event-type} of the event, the value
      @code{:pad-group-mode}.}
    @entry[window]{The @class{gdk:window} object which received the event.}
    @entry[send-event]{@em{True} if the event was sent explicitly.}
    @entry[time]{The time of the event in milliseconds.}
    @entry[group]{The pad group that is switching mode. A @code{:tablet-pad}
      device may have one or more groups containing a set of
      buttons/rings/strips each.}
    @entry[mode]{The new mode of @arg{group}. Different groups in a
      @code{:tablet-pad} device may have different current modes.}
  @end{table}
  @see-slot{gdk:event-type}
  @see-slot{gdk:event-window}
  @see-slot{gdk:event-send-event}
  @see-slot{gdk:event-pad-group-mode-time}
  @see-slot{gdk:event-pad-group-mode-group}
  @see-slot{gdk:event-pad-group-mode-mode}
  @see-class{gdk:event}
  @see-symbol{gdk:event-type}")

(export 'event-pad-group-mode)

;;; --- event-pad-group-mode-time ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-group-mode-time)
      "Accessor"
      (documentation 'event-pad-group-mode-time 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-group-mode-time instance) => time}
  @syntax[]{(setf (gdk:event-pad-group-mode-time instance) time)}
  @argument[instance]{a @class{gdk:event-pad-group-mode} instance}
  @argument[time]{an unsigned integer with the time of the event in
    milliseconds}
  @begin{short}
    Accessor of the @code{time} slot of the @class{gdk:event-pad-group-mode}
    structure.
  @end{short}
  @see-class{gdk:event-pad-group-mode}")

(export 'event-pad-group-mode-time)

;;; --- event-pad-group-mode-group ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-group-mode-group)
      "Accessor"
      (documentation 'event-pad-group-mode-group 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-group-mode-group instance) => group}
  @syntax[]{(setf (gdk:event-pad-group-mode-group instance) group)}
  @argument[instance]{a @class{gdk:event-pad-group-mode} instance}
  @argument[group]{an unsigned integer with the pad group the ring/strip belongs
    to. A @code{:table-pad} device may have one or more groups containing a set
    of buttons/rings/strips each}
  @begin{short}
    Accessor of the @code{group} slot of the @class{gdk:event-pad-group-mode}
    structure.
  @end{short}
  @see-class{gdk:event-pad-group-mode}")

(export 'event-pad-group-mode-group)

;;; --- event-pad-group-mode-mode ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'event-pad-group-mode-mode)
      "Accessor"
      (documentation 'event-pad-group-mode-mode 'function)
 "@version{#2023-3-13}
  @syntax[]{(gdk:event-pad-group-mode-mode instance) => mode}
  @syntax[]{(setf (gdk:event-pad-group-mode-mode instance) mode)}
  @argument[instance]{a @class{gdk:event-pad-group-mode} instance}
  @argument[mode]{an unsigned integer with the current mode of the pad group,
    different groups in a @code{:tablet-pad} device may have different current
    modes}
  @begin{short}
    Accessor of the @code{mode} slot of the @class{gdk:event-pad-group-mode}
    structure.
  @end{short}
  @see-class{gdk:event-pad-group-mode}")

(export 'event-pad-group-mode-mode)

;;; --- End of file gdk3.event-structures.lisp ---------------------------------
