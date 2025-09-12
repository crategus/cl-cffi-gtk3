;;; ----------------------------------------------------------------------------
;;; gdk3.device.lisp
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
;;; GdkDevice
;;;
;;;     Object representing an input device
;;;
;;; Types and Values
;;;
;;;     GdkDevice
;;;     GdkDeviceTool                                      Since 3.22
;;;
;;;     GdkInputSource
;;;     GdkInputMode
;;;     GdkAxisUse
;;;     GdkAxisFlags                                       Since 3.22
;;;     GdkDeviceToolType                                  Since 3.22
;;;     GdkDeviceType
;;;     GdkGrabOwnership
;;;     GdkTimeCoord
;;;
;;;     GdkGrabStatus                                      <- gdk.general.lisp
;;;
;;; Accessors
;;;
;;;     gdk_device_get_name
;;;     gdk_device_get_vendor_id
;;;     gdk_device_get_product_id
;;;     gdk_device_get_associated_device
;;;     gdk_device_get_device_type
;;;     gdk_device_get_display
;;;     gdk_device_get_has_cursor
;;;     gdk_device_get_n_axes
;;;     gdk_device_get_axes
;;;     gdk_device_get_seat
;;;
;;; Functions
;;;
;;;     gdk_device_get_source
;;;     gdk_device_set_mode
;;;     gdk_device_get_mode
;;;     gdk_device_set_key
;;;     gdk_device_get_key
;;;     gdk_device_set_axis_use
;;;     gdk_device_get_axis_use
;;;     gdk_device_list_slave_devices
;;;     gdk_device_get_n_keys
;;;     gdk_device_warp
;;;     gdk_device_grab
;;;     gdk_device_ungrab
;;;     gdk_device_get_state
;;;     gdk_device_get_position
;;;     gdk_device_get_position_double
;;;     gdk_device_get_window_at_position
;;;     gdk_device_get_window_at_position_double
;;;     gdk_device_get_history
;;;     gdk_device_free_history
;;;     gdk_device_get_axis
;;;     gdk_device_list_axes
;;;     gdk_device_get_axis_value
;;;     gdk_device_get_last_event_window
;;;     gdk_device_tool_get_serial
;;;     gdk_device_tool_get_tool_type
;;;     gdk_device_tool_get_hardware_id
;;;
;;; Properties for GdkDeviceTool
;;;
;;;     axes
;;;     hardware-id
;;;     serial
;;;     tool-type
;;;
;;; Properties for GdkDevice
;;;
;;;     associated-device
;;;     axes
;;;     device-manager
;;;     display
;;;     has-cursor
;;;     input-mode
;;;     input-source
;;;     n-axes
;;;     name
;;;     num-touches
;;;     product-id
;;;     seat
;;;     tool
;;;     type
;;;     vendor-id
;;;
;;; Signals
;;;
;;;     changed
;;;     tool-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDevice
;;;
;;; Known Derived Interfaces
;;;
;;;     GdkDevice is required by GdkDevicePad.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkInputSource
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkInputSource" input-source
  (:export t
   :type-initializer "gdk_input_source_get_type")
  :mouse
  :pen
  :eraser
  :cursor
  :keyboard
  :touchscreen
  :touchpad
  :trackpoint
  :tablet-pad)

#+liber-documentation
(setf (liber:alias-for-symbol 'input-source)
      "GEnum"
      (liber:symbol-documentation 'input-source)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkInputSource\" input-source
  (:export t
   :type-initializer \"gdk_input_source_get_type\")
  :mouse
  :pen
  :eraser
  :cursor
  :keyboard
  :touchscreen
  :touchpad
  :trackpoint
  :tablet-pad)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:mouse]{The device is a mouse. This will be reported for the core
        pointer, even if it is something else, such as a trackball.}
      @entry[:pen]{The device is a stylus of a graphics tablet or similar
        device.}
      @entry[:eraser]{The device is an eraser. Typically, this would be the
        other end of a stylus on a graphics tablet.}
      @entry[:cursor]{The device is a graphics tablet \"puck\" or similar
        device.}
      @entry[:keyboard]{The device is a keyboard.}
      @entry[:touchscreen]{The device is a direct-input touch device, such as a
        touchscreen or tablet. This device type has been added in 3.4.}
      @entry[:touchpad]{The device is an indirect touch device, such as a
        touchpad. This device type has been added in 3.4.}
      @entry[:trackpoint]{The device is a trackpoint. This device type has been
        added in 3.22.}
      @entry[:tablet-pad]{The device is a \"pad\", a collection of buttons,
        rings and strips found in drawing tablets. This device type has been
        added in 3.22.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration describing the type of an input device in general terms.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkInputMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkInputMode" input-mode
  (:export t
   :type-initializer "gdk_input_mode_get_type")
  :disabled
  :screen
  :window)

#+liber-documentation
(setf (liber:alias-for-symbol 'input-mode)
      "GEnum"
      (liber:symbol-documentation 'input-mode)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkInputMode\" input-mode
  (:export t
   :type-initializer \"gdk_input_mode_get_type\")
  (:disabled 0)
  (:screen 1)
  (:window 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:disabled]{The device is disabled and will not report any events.}
      @entry[:screen]{The device is enabled. The device's coordinate space maps
        to the entire screen.}
      @entry[:window]{The device is enabled. The device's coordinate space is
        mapped to a single window. The manner in which this window is chosen is
        undefined, but it will typically be the same way in which the focus
        window for key events is determined.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration that describes the mode of an input device.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkAxisUse
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkAxisUse" axis-use
  (:export t
   :type-initializer "gdk_axis_use_get_type")
  :ignore
  :x
  :y
  :pressure
  :xtilt
  :ytilt
  :wheel
  :distance
  :rotation
  :slider
  :last)

#+liber-documentation
(setf (liber:alias-for-symbol 'axis-use)
      "GEnum"
      (liber:symbol-documentation 'axis-use)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkAxisUse\" axis-use
  (:export t
   :type-initializer \"gdk_axis_use_get_type\")
  :ignore
  :x
  :y
  :pressure
  :xtilt
  :ytilt
  :wheel
  :distance
  :rotation
  :slider
  :last)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:ignore]{The axis is ignored.}
      @entry[:x]{The axis is used as the x axis.}
      @entry[:y]{The axis is used as the y axis.}
      @entry[:pressure]{The axis is used for pressure information.}
      @entry[:xtilt]{The axis is used for x tilt information.}
      @entry[:ytilt]{The axis is used for y tilt information.}
      @entry[:wheel]{The axis is used for wheel information.}
      @entry[:distance]{The axis is used for pen/tablet distance information.
        Since 3.22}
      @entry[:rotation]{The axis is used for pen rotation information.
        Since 3.22}
      @entry[:slider]{The axis is used for pen slider information. Since 3.22}
      @entry[:last]{A constant equal to the numerically highest axis value.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration describing the way in which a device axis (valuator) maps
    onto the predefined valuator types that GTK understands.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkAxisFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GdkAxisFlags" axis-flags
  (:export t
   :type-initializer "gdk_axis_flags_get_type")
  (:x        #.(ash 1 1))
  (:y        #.(ash 1 2))
  (:pressure #.(ash 1 3))
  (:xtilt    #.(ash 1 4))
  (:ytilt    #.(ash 1 5))
  (:wheel    #.(ash 1 6))
  (:distance #.(ash 1 7))
  (:rotation #.(ash 1 8))
  (:slider   #.(ash 1 9)))

#+liber-documentation
(setf (liber:alias-for-symbol 'axis-flags)
      "GFlags"
      (liber:symbol-documentation 'axis-flags)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-gflags \"GdkAxisFlags\" axis-flags
  (:export t
   :type-initializer \"gdk_axis_flags_get_type\")
  (:x        #.(ash 1 1))
  (:y        #.(ash 1 2))
  (:pressure #.(ash 1 3))
  (:xtilt    #.(ash 1 4))
  (:ytilt    #.(ash 1 5))
  (:wheel    #.(ash 1 6))
  (:distance #.(ash 1 7))
  (:rotation #.(ash 1 8))
  (:slider   #.(ash 1 9)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:x]{x axis is present.}
      @entry[:y]{y axis is present.}
      @entry[:pressure]{Pressure axis is present.}
      @entry[:xtilt]{x tilt axis is present.}
      @entry[:ytilt]{y tilt axis is present.}
      @entry[:wheel]{Wheel axis is present.}
      @entry[:distance]{Distance axis is present.}
      @entry[:rotation]{z-axis rotation is present.}
      @entry[:slider]{Slider axis is present.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Flags describing the current capabilities of a device/tool.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkDeviceToolType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkDeviceToolType" device-tool-type
  (:export t
   :type-initializer "gdk_device_tool_type_get_type")
  :unknown
  :pen
  :eraser
  :brush
  :pencil
  :airbrush
  :mouse
  :lens)

#+liber-documentation
(setf (liber:alias-for-symbol 'device-tool-type)
      "GEnum"
      (liber:symbol-documentation 'device-tool-type)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkDeviceToolType\" device-tool-type
  (:export t
   :type-initializer \"gdk_device_tool_type_get_type\")
  :unknown
  :pen
  :eraser
  :brush
  :pencil
  :airbrush
  :mouse
  :lens)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unkown]{Tool is of an unknown type.}
      @entry[:pen]{Tool is a standard tablet stylus.}
      @entry[:eraser]{Tool is standard tablet eraser.}
      @entry[:brush]{Tool is a brush stylus.}
      @entry[:pencil]{Tool is a pencil stylus.}
      @entry[:airbrush]{Tool is an airbrush stylus.}
      @entry[:mouse]{Tool is a mouse.}
      @entry[:lens]{Tool is a lens cursor.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Indicates the specific type of tool being used being a tablet. Such as an
    airbrush, pencil, etc.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkDeviceType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkDeviceType" device-type
  (:export t
   :type-initializer "gdk_device_type_get_type")
  (:master 0)
  (:slave 1)
  (:floating 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'device-type)
      "GEnum"
      (liber:symbol-documentation 'device-type)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkDeviceType\" device-type
  (:export t
   :type-initializer \"gdk_device_type_get_type\")
  (:master 0)
  (:slave 1)
  (:floating 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:master]{Device is a master (or virtual) device. There will be an
        associated focus indicator on the screen.}
      @entry[:slave]{Device is a slave (or physical) device.}
      @entry[:floating]{Device is a physical device, currently not attached to
        any virtual device.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Indicates the device type. See above for more information about the meaning
    of these device types.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkGrabOwnership
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkGrabOwnership" grab-ownership
  (:export t
   :type-initializer "gdk_grab_ownership_get_type")
  (:none 0)
  (:window 1)
  (:application 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'grab-ownership)
      "GEnum"
      (liber:symbol-documentation 'grab-ownership)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkGrabOwnership\" grab-ownership
  (:export t
   :type-initializer \"gdk_grab_ownership_get_type\")
  (:none 0)
  (:window 1)
  (:application 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{All other events of the device are allowed.}
      @entry[:window]{Other events of the device are blocked for the grab
        window.}
      @entry[:application]{Other events of the device are blocked for the whole
        application.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Defines how device grabs interact with other devices.
  @end{short}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkTimeCoord
;;; ----------------------------------------------------------------------------

(cffi:defcstruct time-coord
  (time :uint32)
  (axes :double :count 128))

#+liber-documentation
(setf (liber:alias-for-symbol 'time-coord)
      "CStruct"
      (liber:symbol-documentation 'time-coord)
 "@version{2025-06-29}
  @begin{declaration}
(cffi:defcstruct gdk:time-coord
  (time :uint32)
  (axes :double :count 128))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[time]{The timestamp for this event.}
      @entry[axes]{The values of the axes of the device.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @symbol{gdk:time-coord} structure stores a single event in a motion
    history.
  @end{short}
  @see-class{gdk:device}
  @see-function{gdk:device-history}")

(export 'time-coord)

;;; ----------------------------------------------------------------------------
;;; GdkGrabStatus
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkGrabStatus" grab-status
  (:export t
   :type-initializer "gdk_grab_status_get_type")
  (:success 0)
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen
  :failed)

#+liber-documentation
(setf (liber:alias-for-symbol 'grab-status)
      "GEnum"
      (liber:symbol-documentation 'grab-status)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkGrabStatus\" grab-status
  (:export t
   :type-initializer \"gdk_grab_status_get_type\")
  :success
  :already-grabbed
  :invalid-time
  :not-viewable
  :frozen)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:success]{The resource was successfully grabbed.}
      @entry[:already-grabbed]{The resource is actively grabbed by another
        client.}
      @entry[:invalid-time]{The resource was grabbed more recently than the
        specified time.}
      @entry[:not-viewable]{The grab window or the @arg{confine-to} window are
        not viewable.}
      @entry[:frozen]{The resource is frozen by an active grab of another
        client.}
      @entry[:failed]{The grab failed for some other reason.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Returned by the @fun{gdk:seat-grab} function to indicate success or the
    reason for the failure of the grab attempt.
  @end{short}
  @see-class{gdk:device}
  @see-function{gdk:seat-grab}")

;;; ----------------------------------------------------------------------------
;;; GdkDeviceTool
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDeviceTool" device-tool
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_tool_get_type")
  ((axes
    device-tool-axes
    "axes" "GdkAxisFlags" t nil)
   (hardware-id
    device-tool-hardware-id
    "hardware-id" "guint64" t nil)
   (serial
    device-tool-serial
    "serial" "guint64" t nil)
   (tool-type
    device-tool-tool-type
    "tool-type" "GdkDeviceToolType" t nil)))

#+liber-documentation
(setf (documentation 'device-tool 'type)
 "@version{2024-6-27}
  @short{The physical tool associated to a @class{gdk:device} object.}
  @see-slot{gdk:device-tool-axes}
  @see-slot{gdk:device-tool-hardware-id}
  @see-slot{gdk:device-tool-serial}
  @see-slot{gdk:device-tool-tool-type}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;;-----------------------------------------------------------------------------

;;; --- gdk:device-tool-axes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "axes" 'device-tool) t)
 "The @code{axes} property of type @symbol{gdk:axis-flags}
  (Read / Write / Construct only) @br{}
  The axes of the device tool.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-axes)
      "Accessor"
      (documentation 'device-tool-axes 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-tool-axes object) => axes}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[axes]{a value of the @sym{gdk:axis-flags} flags}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{axes} slot of the
    @class{gdk:device-tool} class returns the axes of the device tool.
  @end{short}
  @see-class{gdk:device-tool}
  @see-symbol{gdk:axis-flags}")

;;; --- gdk:device-tool-hardware-id --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hardware-id" 'device-tool) t)
 "The @code{hardware-id} property of type @code{:uint64}
  (Read / Write / Construct only) @br{}
  The hardware ID of the device tool.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-hardware-id)
      "Accessor"
      (documentation 'device-tool-hardware-id 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-tool-hardware-id object) => hardware}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[hardware]{an unsigned integer for the hardware ID of the device
    tool}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{axes} slot of the
    @class{gdk:device-tool} class returns the hardware ID of the device tool,
    or 0 if it is not known.
  @end{short}
  When non-zero, the identificator is unique for the given device tool model,
  meaning that two identical device tools will share the same @arg{hardware}
  value, but will have different serial numbers, see the
  @fun{gdk:device-tool-serial} function.

  This is a more concrete, and device specific, method to identify a
  @class{gdk:device-tool} object than the @fun{gdk:device-tool-tool-type}
  function, as a tablet may support multiple devices with the same
  @sym{gdk:device-tool-type} type, but different hardware identificators.
  @see-class{gdk:device-tool}
  @see-symbol{gdk:device-tool-type}
  @see-function{gdk:device-tool-serial}
  @see-function{gdk:device-tool-tool-type}")

;;; --- gdk:device-tool-serial -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "serial" 'device-tool) t)
 "The @code{serial} property of type @code{:uint64}
  (Read / Write / Construct only) @br{}
  The serial number of the device tool.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-serial)
      "Accessor"
      (documentation 'device-tool-serial 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-tool-serial object) => serial}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[serial]{an unsigned integer for the serial number of the device
    tool}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{serial} slot of the
    @class{gdk:device-tool} class returns the serial number of the device tool.
  @end{short}
  This value can be used to identify a physical tool, for example a tablet pen,
  across program executions.
  @see-class{gdk:device-tool}")

;;; --- gdk:device-tool-tool-type ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tool-type" 'device-tool) t)
 "The @code{tool-type} property of type @symbol{gdk:device-tool-type}
  (Read / Write / Construct only) @br{}
  The type of the device tool.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-tool-type)
      "Accessor"
      (documentation 'device-tool-tool-type 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-tool-tool-type object) => tool-type}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[tool-type]{a value of the @sym{gdk:device-tool-type} enumeration}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{tool-type} slot of the
    @class{gdk:device-tool} class returns the type of the device tool.
  @end{short}
  This can be used to figure out what sort of pen is being used, such as an
  airbrush or a pencil.
  @see-class{gdk:device-tool}
  @see-symbol{gdk:device-tool-type}")

;;; ----------------------------------------------------------------------------
;;; GdkDevice
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDevice" device
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_get_type")
  ((associated-device
    device-associated-device
    "associated-device" "GdkDevice" t nil)
   (axes
    device-axes
    "axes" "GdkAxisFlags" t nil)
   (device-manager
    device-device-manager
    "device-manager" "GdkDeviceManager" t t)
   (display
    device-display
    "display" "GdkDisplay" t t)
   (has-cursor
    device-has-cursor
    "has-cursor" "gboolean" t t)
   (input-mode
    device-input-mode
    "input-mode" "GdkInputMode" t t)
   (input-source
    device-input-source
    "input-source" "GdkInputSource" t t)
   (n-axes
    device-n-axes
    "n-axes" "gint" t nil)
   (name
    device-name
    "name" "gchararray" t t)
   (num-touches
    device-num-touches
    "num-touches" "guint" t t)
   (product-id
    device-product-id
    "product-id" "gchararray" t t)
   (seat
    device-seat
    "seat" "GdkSeat" t t)
   (tool
    device-tool
    "tool" "GdkDeviceTool" t nil)
   (type
    device-type
    "type" "GdkDeviceType" t t)
   (vendor-id
    device-vendor-id
    "vendor-id" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'device 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{gdk:device} object represents a single input device, such as a
    keyboard, a mouse, a touchpad, etc.
  @end{short}

  See the @class{gdk:device-manager} documentation for more information about
  the various kinds of master and slave devices, and their
  relationships.
  @begin[Signal Details]{dictionary}
    @begin[device::changed]{signal}
      @begin{pre}
lambda (device)   :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[device]{The @class{gdk:device} object that changed.}
      @end{simple-table}
      The signal is emitted either when the @class{gdk:device} object has
      changed the number of either axes or keys. For example In X this will
      normally happen when the slave device routing events through the master
      device changes, for example, user switches from the USB mouse to a tablet,
      in that case the master device will change to reflect the new slave device
      axes and keys.
    @end{signal}
    @begin[device::tool-changed]{signal}
      @begin{pre}
lambda (device tool)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[device]{The @class{gdk:device} object that changed.}
        @entry[tool]{The new @class{gdk:device-tool} current device tool.}
      @end{simple-table}
      The signal is emitted on pen/eraser devices whenever tools enter or leave
      proximity. Since 3.22
    @end{signal}
  @end{dictionary}
  @see-slot{gdk:device-associated-device}
  @see-slot{gdk:device-axes}
  @see-slot{gdk:device-device-manager}
  @see-slot{gdk:device-display}
  @see-slot{gdk:device-has-cursor}
  @see-slot{gdk:device-input-mode}
  @see-slot{gdk:device-input-source}
  @see-slot{gdk:device-n-axes}
  @see-slot{gdk:device-name}
  @see-slot{gdk:device-num-touches}
  @see-slot{gdk:device-product-id}
  @see-slot{gdk:device-seat}
  @see-slot{gdk:device-tool}
  @see-slot{gdk:device-type}
  @see-slot{gdk:device-vendor-id}
  @see-class{gdk:device-manager}
  @see-class{gdk:device-tool}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;;-----------------------------------------------------------------------------

;;; --- gdk:device-associated-device -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "associated-device" 'device) t)
 "The @code{associated-device} property of type @class{gdk:device} (Read) @br{}
  Associated pointer or keyboard with this device, if any. Devices of type
  @code{:master} always come in keyboard/pointer pairs. Other device types will
  have a @code{nil} associated device.")

#+liber-documentation
(setf (liber:alias-for-function 'device-associated-device)
      "Accessor"
      (documentation 'device-associated-device 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-associated-device object) => device}
  @argument[object]{a @class{gdk:device} object}
  @argument[device]{an associated @class{gdk:device} object}
  @begin{short}
    The accessor for the @slot[gdk:device]{associated-device} slot of the
    @class{gdk:device} class returns the associated device to @arg{device}.
  @end{short}
  If the device is of type @code{:master}, it will return the paired pointer or
  keyboard. If the device is of type @code{:slave}, it will return the master
  device to which the device is attached to. If the device is of type
  @code{:floating}, @code{nil} will be returned, as there is no associated
  device.
  @see-class{gdk:device}")

;;; --- gdk:device-axes --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "axes" 'device) t)
 "The @code{axes} property of type @symbol{gdk:axis-flags} (Read) @br{}
  The axes currently available for this device.")

#+liber-documentation
(setf (liber:alias-for-function 'device-axes)
      "Accessor"
      (documentation 'device-axes 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-axes object) => axes}
  @argument[object]{a @class{gdk:device} object}
  @argument[axes]{a value of the @sym{gdk:axis-flags} flags}
  @begin{short}
    The accessor for the @slot[gdk:device]{axes} slot of the @class{gdk:device}
    class returns the axes currently available on the device.
  @end{short}
  @see-class{gdk:device}
  @see-symbol{gdk:axis-flags}")

;;; --- gdk:device-device-manager ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "device-manager" 'device) t)
 "The @code{device-manager} property of type @class{gdk:device-manager}
  (Read / Write / Construct) @br{}
  The device manager the device pertains to.")

#+liber-documentation
(setf (liber:alias-for-function 'device-device-manager)
      "Accessor"
      (documentation 'device-device-manager 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-device-manager object) => manager}
  @syntax{(setf (gdk:device-device-manager object) manager)}
  @argument[object]{a @class{gdk:device} object}
  @argument[manager]{a @class{gdk:device-manager} object}
  @begin{short}
    The accessor for the @slot[gdk:device]{device-manager} slot of the
    @class{gdk:device} class gets or sets the device manager the device
    pertains to.
  @end{short}
  @see-class{gdk:device}
  @see-class{gdk:device-manager}")

;;; --- gdk:device-display -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'device) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct) @br{}
  The display the device pertains to.")

#+liber-documentation
(setf (liber:alias-for-function 'device-display)
      "Accessor"
      (documentation 'device-display 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-display object) => display}
  @syntax{(setf (gdk:device-display object) display)}
  @argument[object]{a @class{gdk:device} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    The accessor for the @slot[gdk:device]{display} slot of the
    @class{gdk:device} class returns the display to which the device pertains.
  @end{short}
  @see-class{gdk:device}
  @see-class{gdk:display}")

;;; --- gdk:device-has-cursor --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-cursor" 'device) t)
 "The @code{has-cursor} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the device is represented by a cursor on the screen. Devices of type
  @code{:master} will have @em{true} here. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'device-has-cursor)
      "Accessor"
      (documentation 'device-has-cursor 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-has-cursor object) => has-cursor}
  @syntax{(setf (gdk:device-has-cursor object) has-cursor)}
  @argument[object]{a @class{gdk:device} object}
  @argument[has-cursor]{a boolean whether the device is represented by a cursor
    on the screen}
  @begin{short}
    The accessor for the @slot[gdk:device]{has-cursor} slot of the
    @class{gdk:device} class returns @em{true} if the pointer follows device
    motion.
  @end{short}
  @see-class{gdk:device}")

;;; --- gdk:device-input-mode --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-mode" 'device) t)
 "The @code{input-mode} property of type @sym{gdk:input-mode} (Read / Write)
  @br{}
  The input mode for the device. @br{}
  Default value: @code{:disabled}")

#+liber-documentation
(setf (liber:alias-for-function 'device-input-mode)
      "Accessor"
      (documentation 'device-input-mode 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-input-mode object) => mode}
  @syntax{(setf (gdk:device-input-mode object) mode)}
  @argument[object]{a @class{gdk:device} object}
  @argument[mode]{a value of the @sym{gdk:input-mode} enumeration}
  @begin{short}
    The accessor for the @slot[gdk:device]{input-mode} slot of the
    @class{gdk:device} class gets or sets the mode of the device.
  @end{short}

  The mode controls if the device is active and whether the range of the device
  is mapped to the entire screen or to a single window.
  @see-class{gdk:device}
  @see-symbol{gdk:input-mode}")

;;; --- gdk:device-input-source ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-source" 'device) t)
 "The @code{input-source} property of type @sym{gdk:input-source}
  (Read / Write / Construct) @br{}
  The source type for the device. @br{}
  Default value: @code{:mouse}")

#+liber-documentation
(setf (liber:alias-for-function 'device-input-source)
      "Accessor"
      (documentation 'device-input-source 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-input-source object) => source}
  @syntax{(setf (gdk:device-input-source object) source)}
  @argument[object]{a @class{gdk:device} object}
  @argument[source]{a value of the @sym{gdk:input-source} enumeration}
  @begin{short}
    The accessor for the @slot[gdk:device]{input-source} slot of the
    @class{gdk:device} class returns the source type of the device.
  @end{short}
  @see-class{gdk:device}
  @see-symbol{gdk:input-source}")

;;; --- gdk:device-n-axes ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-axes" 'device) t)
 "The @code{n-axes} property of type @code{:uint} (Read) @br{}
  The number of axes in the device. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'device-n-axes)
      "Accessor"
      (documentation 'device-n-axes 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-n-axis object) => n-axis}
  @argument[object]{a @class{gdk:device} object}
  @argument[n-axis]{an unsigned integer for the number of axes in the device}
  @begin{short}
    The accessor for the @slot[gdk:device]{n-axes} slot of the
    @class{gdk:device} class returns the number of axes the device currently
    has.
  @end{short}
  @see-class{gdk:device}")

;;; --- gdk:device-name --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'device) t)
 "The @code{name} property of type @code{:string} (Read / Write / Construct)
  @br{}
  The device name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'device-name)
      "Accessor"
      (documentation 'device-name 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-name object) => name}
  @syntax{(setf (gdk:device-name object) name)}
  @argument[object]{a @class{gdk:device} object}
  @argument[name]{a string for the device name}
  @begin{short}
    The accessor for the @slot[gdk:device]{name} slot of the @class{gdk:device}
    class returns the name of the device.
  @end{short}
  @see-class{gdk:device}")

;;; --- gdk:device-num-touches -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "num-touches" 'device) t)
 "The @code{num-touches} property of type @code{:uint}
  (Read / Write / Construct) @br{}
  The maximal number of concurrent touches on a touch device. Will be 0 if the
  device is not a touch device or if the number of touches is unknown. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'device-num-touches)
      "Accessor"
      (documentation 'device-num-touches 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-num-touches object) => num-touches}
  @argument[object]{a @class{gdk:device} object}
  @argument[num-touches]{an unsigned integer for the number of touches}
  @begin{short}
    The accessor for the @slot[gdk:device]{num-touches} slot of the
    @class{gdk:device} class returns the maximal number of concurrent touches
    on a touch device.
  @end{short}
  Will be 0 if the device is not a touch device or if the number of touches is
  unknown.
  @see-class{gdk:device}")

;;; --- gdk:device-product-id --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "product-id" 'device) t)
 "The @code{product-id} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The product ID of the device. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'device-product-id)
      "Accessor"
      (documentation 'device-product-id 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-product-id object) => product-id}
  @argument[object]{a @class{gdk:device} object}
  @argument[product-id]{a string for the product ID}
  @begin{short}
    The accessor for the @slot[gdk:device]{product-id} slot of the
    @class{gdk:device} class returns the product ID of this device, or
    @code{nil} if this information could not be obtained.
  @end{short}
  This ID is retrieved from the device, and is thus constant for it. See the
  @fun{gdk:device-vendor-id} function for more information.
  @see-class{gdk:device}
  @see-function{gdk:device-vendor-id}")

;;; --- gdk:device-seat --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "seat" 'device) t)
 "The @code{seat} property of type @class{gdk:seat} (Read / Write) @br{}
  The seat of the device.")

#+liber-documentation
(setf (liber:alias-for-function 'device-seat)
      "Accessor"
      (documentation 'device-seat 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-seat object) => seat}
  @argument[object]{a @class{gdk:device} object}
  @argument[seat]{a @class{gdk:seat} object}
  @begin{short}
    The accessor for the @slot[gdk:device]{seat} slot of the @class{gdk:device}
    class returns the seat the device belongs to.
  @end{short}
  @see-class{gdk:device}
  @see-class{gdk:seat}")

;;; --- gdk:device-tool --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tool" 'device) t)
 "The @code{tool} property of type @class{gdk:device-tool} (Read) @br{}
  The device tool that is currently used with this device.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool)
      "Accessor"
      (documentation 'device-tool 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-tool object) => tool}
  @argument[object]{a @class{gdk:device} object}
  @argument[tool]{a @class{gdk:device-tool} object}
  @begin{short}
    The accessor for the @slot[gdk:device]{tool} slot of the @class{gdk:device}
    class gets the device tool that is currently used with this device.
  @end{short}
  @see-class{gdk:device}
  @see-class{gdk:device-tool}")

;;; --- gdk:device-type --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "type" 'device) t)
 "The @code{type} property of type @symbol{gdk:device-type}
  (Read / Write / Construct) @br{}
  Device role in the device manager. @br{}
  Default value: @code{:master}")

#+liber-documentation
(setf (liber:alias-for-function 'device-type)
      "Accessor"
      (documentation 'device-type 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-type object) => type}
  @argument[object]{a @class{gdk:device} object}
  @argument[type]{a @sym{gdk:device-type} value}
  @begin{short}
    The accessor for the @slot[gdk:device]{type} slot of the @class{gdk:device}
    class returns the device type.
  @end{short}
  @see-class{gdk:device}
  @see-symbol{gdk:device-type}")

;;; --- gdk:device-vendor-id ---------------------------------------------------

;; TODO: Rewrite the example in Lisp or remove it

#+liber-documentation
(setf (documentation (liber:slot-documentation "vendor-id" 'device) t)
 "The @code{vendor-id} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The vendor ID of this device. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'device-vendor-id)
      "Accessor"
      (documentation 'device-vendor-id 'function)
 "@version{2025-08-31}
  @syntax{(gdk:device-vendor-id object) => vendor-id}
  @argument[object]{a @class{gdk:device} object}
  @argument[vendor-id]{a string for the vendor ID}
  @begin{short}
    The accessor for the @slot[gdk:device]{vendor-id} slot of the
    @class{gdk:device} class returns the vendor ID of this device, or @code{nil}
    if this information could not be obtained.
  @end{short}
  This ID is retrieved from the device, and is thus constant for it.

  This function, together with the @fun{gdk:device-product-id} function, can be
  used to, for example, compose @code{GSettings} paths to store settings for
  this device.
  @begin[Examples]{dictionary}
    @begin{pre}
static GSettings *
get_device_settings (GdkDevice *device)
{
  const gchar *vendor, *product;
  GSettings *settings;
  GdkDevice *device;
  gchar *path;

  vendor = gdk_device_get_vendor_id (device);
  product = gdk_device_get_product_id (device);

  path = g_strdup_printf (\"/org/example/app/devices/%s:%s/\", vendor, product);
  settings = g_settings_new_with_path (DEVICE_SCHEMA, path);
  g_free (path);

  return settings;
@}
    @end{pre}
  @end{dictionary}
  @see-class{gdk:device}
  @see-function{gdk:device-product-id}")

;;; ----------------------------------------------------------------------------

#-windows
(gobject:define-gobject "GdkX11DeviceXI2" x11-device-xi2
  (:superclass device
   :export t
   :interfaces nil
   :type-initializer "gdk_x11_device_xi2_get_type")
  ((device-id
    x11-device-xi2-device-id
    "device-id" "gint" t nil)))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_key
;;; gdk_device_set_key
;;; ----------------------------------------------------------------------------

(defun (setf device-key) (value device index)
  (destructuring-bind (keyval modifiers) value
    (cffi:foreign-funcall "gdk_device_set_key"
                          (g:object device) device
                          :uint index
                          :uint keyval
                          modifier-type modifiers
                          :void)
    (values keyval modifiers)))

(cffi:defcfun ("gdk_device_get_key" %device-key) :boolean
  (device (g:object device))
  (index :int)
  (keyval (:pointer :uint))
  (modifiers (:pointer modifier-type)))

(defun device-key (device index)
 #+liber-documentation
 "@version{#2025-08-31}
  @syntax{(gdk:device-key device index) => keyval, modifiers}
  @syntax{(setf (gdk:device-key device index) (list keyval modifiers))}
  @argument[device]{a @class{gdk:device} object}
  @argument[index]{an unsigned integer for the index of the macro button to get}
  @argument[keyval]{an unsigned integer for the keyval to generate}
  @argument[modifiers]{a value of the @sym{gdk:modifier-type} flags}
  @begin{short}
    If the @arg{index} argument has a valid keyval, the @fun{gdk:device-key}
    function will return @arg{keyval} and @arg{modifiers} with the keyval
    settings.
  @end{short}
  The @setf{gdk:device-key} function specifies the X key event to generate when
  a macro button of a device is pressed.
  @see-class{gdk:device}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-objects ((keyval :uint)
                              (modifiers 'modifier-type))
    (when (%device-key device index keyval modifiers)
      (values (cffi:mem-ref keyval :uint)
              (cffi:mem-ref modifiers 'modifier-type)))))

(export 'device-key)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_use
;;; gdk_device_set_axis_use
;;; ----------------------------------------------------------------------------

(defun (setf device-axis-use) (use device index)
  (cffi:foreign-funcall "gdk_device_set_axis_use"
                        (g:object device) device
                        :uint index
                        axis-use use
                        :void)
  use)

(cffi:defcfun ("gdk_device_get_axis_use" device-axis-use) axis-use
 #+liber-documentation
 "@version{2025-08-31}
  @syntax{(gdk:device-axis-use device index) => use}
  @syntax{(setf (gdk:device-axis-use device index) use)}
  @argument[device]{a @class{gdk:device} pointer device}
  @argument[index]{an unsigned integer for the index of the axis}
  @argument[use]{a @sym{gdk:axis-use} value specifying how the axis is used}
  @short{Gets or sets the axis use for @arg{index}.}
  @see-class{gdk:device}
  @see-symbol{gdk:axis-use}"
  (device (g:object device))
  (index :uint))

(export 'device-axis-use)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_slave_devices
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_list_slave_devices" device-list-slave-devices)
    (g:list-t (g:object device))
 #+liber-documentation
 "@version{2024-6-27}
  @argument[device]{a @class{gdk:device} object}
  @return{The list of slave @class{gdk:device} objects, or @code{nil}.}
  @begin{short}
    If the device is of type @code{:master}, it will return the list of
    slave devices attached to it, otherwise it will return @code{nil}.
  @end{short}
  @see-class{gdk:device}"
  (device (g:object device)))

(export 'device-list-slave-devices)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_n_keys
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_n_keys" device-n-keys) :int
 #+liber-documentation
 "@version{2025-07-17}
  @argument[device]{a @class{gdk:device} object}
  @return{The integer for the number of keys.}
  @short{Returns the number of keys the device currently has.}
  @see-class{gdk:device}"
  (device (g:object device)))

(export 'device-n-keys)

;;; ----------------------------------------------------------------------------
;;; gdk_device_warp
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_warp" device-warp) :void
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[device]{a @class{gdk:device} object to warp}
  @argument[screen]{a @class{gdk:screen} object to warp @arg{device} to}
  @argument[x]{an integer for the x coordinate of the destination}
  @argument[y]{an integer for the y coordinate of the destination}
  @begin{short}
    Warps the device to the point @arg{x},@arg{y} on the screen.
  @end{short}
  Unless the device is confined to a window by a grab, in which case it will be
  moved as far as allowed by the grab. Warping the pointer creates events as if
  the user had moved the mouse instantaneously to the destination.

  Note that the pointer should normally be under the control of the user. This
  function was added to cover some rare use cases.
  @see-class{gdk:device}
  @see-class{gdk:screen}"
  (device (g:object device))
  (screen (g:object screen))
  (x :int)
  (y :int))

(export 'device-warp)

;;; ----------------------------------------------------------------------------
;;; gdk_device_grab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_grab" device-grab) grab-status
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[device]{a @class{gdk:device} object, to get the device you can use
    the @fun{gtk:current-event-device} or @fun{gdk:event-device} functions
    if the grab is in reaction to an event, also, you can use the
    @fun{gdk:device-manager-client-pointer} function but only in code that is
    not triggered by a @class{gdk:event} event and there are not other means to
    get a meaningful @class{gdk:device} object to operate on}
  @argument[window]{a @class{gdk:window} object that will own the grab}
  @argument[grab-ownership]{a @symbol{gdk:grab-ownership} value which specifies
    the grab ownership}
  @argument[owner-events]{if @em{false} then all device events are reported
    with respect to @arg{window} and are only reported if selected by
    @arg{event-mask}, if @em{true} then pointer events for this application are
    reported as normal, but pointer events outside this application are
    reported with respect to @arg{window} and only if selected by
    @arg{event-mask}, in either mode, unreported events are discarded}
  @argument[event-mask]{specifies the @symbol{gdk:event-mask} value which is
    used in accordance with @arg{owner-events}}
  @argument[cursor]{a @class{gdk:cursor} object to display while the grab is
    active if the device is a pointer, if this is @code{nil} then the normal
    cursors are used for window and its descendants, and the cursor for window
    is used elsewhere}
  @argument[time]{an unsigned integer for the timestamp of the event which led
    to this pointer grab. This usually comes from the @class{gdk:event} event,
    though @var{gdk:+current-time+} can be used if the time is not known.}
  @return{The @code{:sucess} value if the grab was successful.}
  @begin{short}
    Grabs the device so that all events coming from this device are passed to
    this application until the device is ungrabbed with the
    @fun{gdk:device-ungrab} function, or the window becomes unviewable.
  @end{short}
  This overrides any previous grab on the device by this client.

  Device grabs are used for operations which need complete control over the
  given device events, either pointer or keyboard. For example in GTK this
  is used for drag and drop operations, popup menus and such.

  Note that if the event mask of an X window has selected both button press
  and button release events, then a button press event will cause an automatic
  pointer grab until the button is released. X does this automatically since
  most applications expect to receive button press and release events in
  pairs. It is equivalent to a pointer grab on the window with
  @arg{owner-events} set to @em{true}.

  If you set up anything at the time you take the grab that needs to be
  cleaned up when the grab ends, you should handle the
  @class{gdk:event-grab-broken} events that are emitted when the grab ends
  unvoluntarily.
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-class{gdk:event}
  @see-class{gdk:cursor}
  @see-class{gdk:event-grab-broken}
  @see-function{gdk:device-ungrab}
  @see-function{gdk:event-device}
  @see-function{gtk:current-event-device}
  @see-function{gdk:device-manager-client-pointer}
  @see-variable{gdk:+current-time+}"
  (device (g:object device))
  (window (g:object window))
  (grab-ownership grab-ownership)
  (owner-events :boolean)
  (event-mask event-mask)
  (cursor (g:object cursor))
  (time :uint32))

(export 'device-grab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_ungrab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_ungrab" device-ungrab) :void
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[device]{a @class{gdk:device} object}
  @argument[time]{an unsigned integer for the timestamp, for example,
    @var{gdk:+current-time+}}
  @short{Release any grab on the device.}
  @see-class{gdk:device}
  @see-function{gdk:device-grab}
  @see-variable{gdk:+current-time+}"
  (device (g:object device))
  (time :uint32))

(export 'device-ungrab)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_state" %device-state) :void
  (device (g:object device))
  (window (g:object window))
  (axes (:pointer :double))
  (mask (:pointer modifier-type)))

(defun device-state (device window)
 #+liber-documentation
 "@version{#2024-6-27}
  @argument[device]{a @class{gdk:device} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    @arg{axes} -- a list of double float numbers with the axes value @br{}
    @arg{mask} -- a value of the @symbol{gdk:modifier-type} flags, or @code{nil}
  @end{return}
  @begin{short}
    Gets the current state of a pointer device relative to the window.
  @end{short}
  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the @fun{gdk:device-grab} function.
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-symbol{gdk:modifier-type}
  @see-function{gdk:device-grab}"
  (cffi:with-foreign-objects ((axes :double (device-n-axes device))
                              (mask 'modifier-type))
    (%device-state device window axes mask)
    (values (iter (for i from 0 below (device-n-axes device))
                  (collect (cffi:mem-aref axes :double i)))
            (cffi:mem-ref mask 'modifier-type))))

(export 'device-state)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_position" %device-position) :void
  (display (g:object device))
  (screen :pointer)
  (x :pointer)
  (y :pointer))

(defun device-position (device)
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[device]{a @class{gdk:device} pointer device to query status about}
  @begin{return}
    @arg{screen} -- a @class{gdk:screen} object the device is on,
      or @code{nil} @br{}
    @arg{x} -- an integer for the root window x coordinate of device,
      or @code{nil} @br{}
    @arg{y} -- an integer for the root window y coordinate of device,
      or @code{nil}
  @end{return}
  @begin{short}
    Gets the current location of the device.
  @end{short}
  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the @fun{gdk:device-grab} function.
  @see-class{gdk:device}
  @see-class{gdk:screen}
  @see-function{gdk:device-grab}"
  (cffi:with-foreign-objects ((screen :pointer) (x :int) (y :int))
    (%device-position device screen x y)
    (values (cffi:mem-ref screen '(g:object screen))
            (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'device-position)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_position_double
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_position_double" %device-position-double) :void
  (device (g:object device))
  (screen (g:object screen))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun device-position-double (device)
 #+liber-documentation
 "@version{#2024-6-27}
  @argument[device]{a @class{gdk:device} pointer device to query status about}
  @begin{return}
    @arg{screen} -- a @class{gdk:screen} object the device is on, or @code{nil}
      @br{}
    @arg{x} -- a double float with the root window x coordinate of the device,
      or @code{nil} @br{}
    @arg{y} -- a double float with the root window y coordinate of the device,
      or @code{nil}
  @end{return}
  @begin{short}
    Gets the current location of the device in double precision.
  @end{short}
  As a slave device's coordinates are those of its master pointer, this
  function may not be called on devices of type @code{:slave}, unless there is
  an ongoing grab on them. See the @fun{gdk:device-grab} function.
  @see-class{gdk:device}
  @see-class{gdk:screen}
  @see-function{gdk:device-grab}"
  (cffi:with-foreign-objects ((screen :pointer) (x :double) (y :double))
    (%device-position-double device screen x y)
    (values (cffi:mem-ref screen '(g:object screen))
            (cffi:mem-ref x :double)
            (cffi:mem-ref y :double))))

(export 'device-position-double)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_window_at_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_window_at_position" %device-window-at-position)
    (g:object window)
  (device (g:object device))
  (xwin (:pointer :int))
  (ywin (:pointer :int)))

(defun device-window-at-position (device)
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[device]{a @class{gdk:device} pointer device to query info to}
  @begin{return}
    @arg{window} -- a @class{gdk:window} object @br{}
    @arg{xwin} -- an integer for the x coordinate of the device location,
      relative to the window origin, or @code{nil} @br{}
    @arg{ywin} -- an integer for the y coordinate of the device location,
      relative to the window origin, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the window underneath the device, returning the location of the
    device.
  @end{short}
  Returns @code{nil} if the window tree under the device is not known to GDK
  (for example, belongs to another application).

  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the @fun{gdk:device-grab} function.
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-function{gdk:device-grab}"
  (cffi:with-foreign-objects ((xwin :int) (ywin :int))
    (let ((window (%device-window-at-position device xwin ywin)))
      (values window
              (cffi:mem-ref xwin :int)
              (cffi:mem-ref ywin :int)))))

(export 'device-window-at-position)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_window_at_position_double
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_window_at_position_double"
          %device-window-at-position-double) (g:object window)
  (device (g:object device))
  (xwin (:pointer :double))
  (ywin (:pointer :double)))

(defun device-window-at-position-double (device)
 #+liber-documentation
 "@version{#2024-6-27}
  @argument[device]{a @class{gdk:device} pointer device to query info to}
  @begin{return}
    @arg{window} -- a @class{gdk:window} object under the device position,
      or @code{nil} @br{}
    @arg{xwin} -- a double float for the x coordinate of the device location,
      relative to the window origin, or @code{nil} @br{}
    @arg{ywin} -- a double float for the y coordinate of the device location,
      relative to the window origin, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the window underneath the device, returning the location of the
    device in double precision.
  @end{short}
  Returns @code{nil} if the window tree under the device is not known to GDK,
  for example, belongs to another application.

  As a slave device coordinates are those of its master pointer. This function
  may not be called on devices of type @code{:slave}, unless there is an
  ongoing grab on them. See the @fun{gdk:device-grab} function.
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-function{gdk:device-grab}"
  (cffi:with-foreign-objects ((xwin :double) (ywin :double))
    (let ((window (%device-window-at-position-double device xwin ywin)))
      (values window
              (cffi:mem-ref xwin :double)
              (cffi:mem-ref ywin :double)))))

(export 'device-window-at-position-double)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_history
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_history" %device-history) :boolean
  (device (g:object device))
  (window (g:object window))
  (start :uint32)
  (stop :uint32)
  (events (:pointer (:pointer (:pointer (:struct time-coord)))))
  (n-events (:pointer :int)))

(defun device-history (device window start stop)
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[device]{a @class{gdk:device} object}
  @argument[window]{a @class{gdk:window} object with respect to which which
    the event coordinates will be reported}
  @argument[start]{an unsigned integer for the starting timestamp for range of
    events to return}
  @argument[stop]{an unsigned integer for the ending timestamp for the range
    of events to return}
  @begin{return}
    The list of @class{gdk:time-coord} instances if the windowing system
    supports motion history and at least one event was found, or @code{nil}.
  @end{return}
  @begin{short}
    Obtains the motion history for a pointer device.
  @end{short}
  Given a starting and ending timestamp, return all events in the motion
  history for the device in the given range of time.

  Some windowing systems do not support motion history, in which case,
  @code{nil} will be returned. This is not distinguishable from the case where
  motion history is supported and no events were found.
  @see-class{gdk:device}
  @see-class{gdk:window}
  @see-symbol{gdk:time-coord}"
  (cffi:with-foreign-objects ((events :pointer) (n-events :int))
    (when (%device-history device window start stop events n-events)
      (prog1
        (iter (with events-ar = (cffi:mem-ref events :pointer))
              (for i from 0 below (cffi:mem-ref n-events :int))
              (for coord = (cffi:mem-aref events-ar
                                          '(:struct time-coord)
                                          i))
              (collect coord))
        (%device-free-history (cffi:mem-ref events :pointer)
                              (cffi:mem-ref n-events :int))))))

(export 'device-history)

;;; ----------------------------------------------------------------------------
;;; gdk_device_free_history                                 not exported
;;; ----------------------------------------------------------------------------

;; For internal use in device-history and not exported.

(cffi:defcfun ("gdk_device_free_history" %device-free-history) :void
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[events]{an array for @class{gdk:time-coord} instances}
  @argument[n-events]{an integer for the length of the array}
  @begin{short}
    Frees an array of @class{gdk:time-coord} instances that was returned by the
    @fun{gdk:device-history} function.
  @end{short}
  @see-class{gdk:time-coord}
  @see-function{gdk:device-history}"
  (events (:pointer (:pointer (:struct time-coord))))
  (n-events :int))

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_axis" %device-axis) :boolean
  (device (g:object device))
  (axes (:pointer :double))
  (use axis-use)
  (value (:pointer :double)))

(defun device-axis (device axes axis-use)
 #+liber-documentation
 "@version{#2025-1-25}
  @argument[device]{a @class{gdk:device} object}
  @argument[axes]{a list of double floats of axes}
  @argument[use]{a @symbol{gdk:axis-use} value use to look for}
  @return{The found double float, otherwise @code{nil}.}
  @begin{short}
    Interprets a list of double floats as axis values for a given device, and
    locates the value in the array for a given axis use.
  @end{short}
  @see-class{gdk:device}
  @see-symbol{gdk:axis-use}"
  (assert (= (device-n-axes device) (length axes)))
  (cffi:with-foreign-objects ((axes-ar :double (device-n-axes device))
                         (value :double))
    (let ((i 0))
      (map nil
           (lambda (v)
             (setf (cffi:mem-aref axes-ar :double i) v)
             (incf i))
           axes))
    (when (%device-axis device axes-ar axis-use value)
      (cffi:mem-ref value :double))))

(export 'device-axis)

;;; ----------------------------------------------------------------------------
;;; gdk_device_list_axes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_list_axes" device-list-axes) (g:list-t atom-as-string)
 #+liber-documentation
 "@version{#2024-6-27}
  @argument[device]{a @class{gdk:device} object}
  @return{The list of strings containing the labels.}
  @begin{short}
    Returns a list of strings containing the labels for the axes that the
    device currently has.
  @end{short}
  @see-class{gdk:device}"
  (device (g:object device)))

(export 'device-list-axes)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_axis_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_axis_value" %device-axis-value) :boolean
  (device (g:object device))
  (axes (:pointer :double))
  (label atom-as-string)
  (value (:pointer :double)))

(defun device-axis-value (device axes axis-label)
 #+liber-documentation
 "@version{#2024-6-27}
  @argument[device]{a @class{gdk:device} object for a pointer device}
  @argument[axes]{a list of double float for the axes}
  @argument[label]{a string with the axis label}
  @return{The double float with the found value, or @code{nil}.}
  @begin{short}
    Interprets a list of double floats as axis values for a given device, and
    locates the value in the array for a given axis label, as returned by the
    @fun{gdk:device-list-axes} function.
  @end{short}
  @see-class{gdk:device}
  @see-function{gdk:device-list-axes}"
  (assert (= (device-n-axes device) (length axes)))
  (cffi:with-foreign-objects ((axes-ar :double (device-n-axes device))
                              (value :double))
    (let ((i 0))
      (map nil
           (lambda (v)
             (setf (cffi:mem-aref axes-ar :double i) v)
             (incf i))
           axes))
    (when (%device-axis-value device axes-ar axis-label value)
      (cffi:mem-ref value :double))))

(export 'device-axis-value)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_last_event_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_last_event_window" device-last-event-window)
    (g:object window)
 #+liber-documentation
 "@version{#2024-6-27}
  @argument[device]{a @class{gdk:device} object with a source other than
    @code{:keyboard}}
  @return{The last @class{gdk:window} object the device is in.}
  @begin{short}
    Gets information about which window the given pointer device is in, based
    on events that have been received so far from the display server.
  @end{short}
  If another application has a pointer grab, or this application has a grab
  with @code{owner-events} is @em{false}, @code{nil} may be returned even if
  the pointer is physically over one of this application's windows.
  @see-class{gdk:device}
  @see-class{gdk:window}"
  (device (g:object device)))

(export 'device-last-event-window)

;;; --- End of file gdk3.device.lisp -------------------------------------------
