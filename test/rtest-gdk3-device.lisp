(in-package :gtk-test)

(def-suite gdk-device :in gdk-suite)
(in-suite gdk-device)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkInputSource

(test input-source
  ;; Check the type
  (is (g:type-is-enum "GdkInputSource"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkInputSource")
          (g:gtype (cffi:foreign-funcall "gdk_input_source_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:input-source
          (gobject:symbol-for-gtype "GdkInputSource")))
  ;; Check the names
  (is (equal '("GDK_SOURCE_MOUSE" "GDK_SOURCE_PEN" "GDK_SOURCE_ERASER"
               "GDK_SOURCE_CURSOR" "GDK_SOURCE_KEYBOARD"
               "GDK_SOURCE_TOUCHSCREEN" "GDK_SOURCE_TOUCHPAD"
               "GDK_SOURCE_TRACKPOINT" "GDK_SOURCE_TABLET_PAD")
             (list-enum-item-name "GdkInputSource")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (list-enum-item-value "GdkInputSource")))
  ;; Check the nick names
  (is (equal '("mouse" "pen" "eraser" "cursor" "keyboard" "touchscreen"
               "touchpad" "trackpoint" "tablet-pad")
             (list-enum-item-nick "GdkInputSource")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkInputSource"
                             GDK-INPUT-SOURCE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_input_source_get_type")
                             (:MOUSE 0)
                             (:PEN 1)
                             (:ERASER 2)
                             (:CURSOR 3)
                             (:KEYBOARD 4)
                             (:TOUCHSCREEN 5)
                             (:TOUCHPAD 6)
                             (:TRACKPOINT 7)
                             (:TABLET-PAD 8))
             (gobject:get-g-type-definition "GdkInputSource"))))

;;;     GdkInputMode

(test input-mode
  ;; Check the type
  (is (g:type-is-enum "GdkInputMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkInputMode")
          (g:gtype (cffi:foreign-funcall "gdk_input_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:input-mode
          (gobject:symbol-for-gtype "GdkInputMode")))
  ;; Check the names
  (is (equal '("GDK_MODE_DISABLED" "GDK_MODE_SCREEN" "GDK_MODE_WINDOW")
             (list-enum-item-name "GdkInputMode")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GdkInputMode")))
  ;; Check the nick names
  (is (equal '("disabled" "screen" "window")
             (list-enum-item-nick "GdkInputMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkInputMode"
                             GDK-INPUT-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_input_mode_get_type")
                             (:DISABLED 0)
                             (:SCREEN 1)
                             (:WINDOW 2))
             (gobject:get-g-type-definition "GdkInputMode"))))

;;;     GdkAxisUse

(test axis-use
  ;; Check the type
  (is-true (g:type-is-enum "GdkAxisUse"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkAxisUse")
          (g:gtype (cffi:foreign-funcall "gdk_axis_use_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:axis-use
          (gobject:symbol-for-gtype "GdkAxisUse")))
  ;; Check the names
  (is (equal '("GDK_AXIS_IGNORE" "GDK_AXIS_X" "GDK_AXIS_Y" "GDK_AXIS_PRESSURE"
               "GDK_AXIS_XTILT" "GDK_AXIS_YTILT" "GDK_AXIS_WHEEL"
               "GDK_AXIS_DISTANCE" "GDK_AXIS_ROTATION" "GDK_AXIS_SLIDER"
               "GDK_AXIS_LAST")
             (list-enum-item-name "GdkAxisUse")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10)
             (list-enum-item-value "GdkAxisUse")))
  ;; Check the nick names
  (is (equal '("ignore" "x" "y" "pressure" "xtilt" "ytilt" "wheel" "distance"
               "rotation" "slider" "last")
             (list-enum-item-nick "GdkAxisUse")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkAxisUse"
                             GDK-AXIS-USE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_axis_use_get_type")
                             (:IGNORE 0)
                             (:X 1)
                             (:Y 2)
                             (:PRESSURE 3)
                             (:XTILT 4)
                             (:YTILT 5)
                             (:WHEEL 6)
                             (:DISTANCE 7)
                             (:ROTATION 8)
                             (:SLIDER 9)
                             (:LAST 10))
             (gobject:get-g-type-definition "GdkAxisUse"))))

;;;     GdkAxisFlags                                       Since 3.22

(test axis-flags
  ;; Check the type
  (is (g:type-is-flags "GdkAxisFlags"))
  ;; Check the registered name
  (is (eq 'gdk:axis-flags
          (gobject:symbol-for-gtype "GdkAxisFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkAxisFlags")
          (g:gtype (cffi:foreign-funcall "gdk_axis_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_AXIS_FLAG_X" "GDK_AXIS_FLAG_Y" "GDK_AXIS_FLAG_PRESSURE"
               "GDK_AXIS_FLAG_XTILT" "GDK_AXIS_FLAG_YTILT" "GDK_AXIS_FLAG_WHEEL"
               "GDK_AXIS_FLAG_DISTANCE" "GDK_AXIS_FLAG_ROTATION"
               "GDK_AXIS_FLAG_SLIDER")
             (list-flags-item-name "GdkAxisFlags")))
  ;; Check the values
  (is (equal '(2 4 8 16 32 64 128 256 512)
             (list-flags-item-value "GdkAxisFlags")))
  ;; Check the nick names
  (is (equal '("x" "y" "pressure" "xtilt" "ytilt" "wheel" "distance" "rotation"
               "slider")
             (list-flags-item-nick "GdkAxisFlags")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkAxisFlags"
                              GDK-AXIS-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_axis_flags_get_type")
                              (:X 2)
                              (:Y 4)
                              (:PRESSURE 8)
                              (:XTILT 16)
                              (:YTILT 32)
                              (:WHEEL 64)
                              (:DISTANCE 128)
                              (:ROTATION 256)
                              (:SLIDER 512))
             (gobject:get-g-type-definition "GdkAxisFlags"))))

;;;     GdkDeviceToolType                                  Since 3.22

(test device-tool-type
  ;; Check the type
  (is (g:type-is-enum "GdkDeviceToolType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceToolType")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:device-tool-type
          (gobject:symbol-for-gtype "GdkDeviceToolType")))
  ;; Check the names
  (is (equal '("GDK_DEVICE_TOOL_TYPE_UNKNOWN" "GDK_DEVICE_TOOL_TYPE_PEN"
               "GDK_DEVICE_TOOL_TYPE_ERASER" "GDK_DEVICE_TOOL_TYPE_BRUSH"
               "GDK_DEVICE_TOOL_TYPE_PENCIL" "GDK_DEVICE_TOOL_TYPE_AIRBRUSH"
               "GDK_DEVICE_TOOL_TYPE_MOUSE" "GDK_DEVICE_TOOL_TYPE_LENS")
             (list-enum-item-name "GdkDeviceToolType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GdkDeviceToolType")))
  ;; Check the nick names
  (is (equal '("unknown" "pen" "eraser" "brush" "pencil" "airbrush" "mouse"
               "lens")
             (list-enum-item-nick "GdkDeviceToolType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkDeviceToolType"
                             GDK-DEVICE-TOOL-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_device_tool_type_get_type")
                             (:UNKNOWN 0)
                             (:PEN 1)
                             (:ERASER 2)
                             (:BRUSH 3)
                             (:PENCIL 4)
                             (:AIRBRUSH 5)
                             (:MOUSE 6)
                             (:LENS 7))
             (gobject:get-g-type-definition "GdkDeviceToolType"))))

;;;     GdkDeviceType

(test device-type
  ;; Check the type
  (is (g:type-is-enum "GdkDeviceType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceType")
          (g:gtype (cffi:foreign-funcall "gdk_device_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:device-type
          (gobject:symbol-for-gtype "GdkDeviceType")))
  ;; Check the names
  (is (equal '("GDK_DEVICE_TYPE_MASTER" "GDK_DEVICE_TYPE_SLAVE"
               "GDK_DEVICE_TYPE_FLOATING")
             (list-enum-item-name "GdkDeviceType")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GdkDeviceType")))
  ;; Check the nick names
  (is (equal '("master" "slave" "floating")
             (list-enum-item-nick "GdkDeviceType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkDeviceType"
                             GDK-DEVICE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_device_type_get_type")
                             (:MASTER 0)
                             (:SLAVE 1)
                             (:FLOATING 2))
             (gobject:get-g-type-definition "GdkDeviceType"))))

;;;     GdkGrabOwnership

(test grab-ownership
  ;; Check the type
  (is (g:type-is-enum "GdkGrabOwnership"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkGrabOwnership")
          (g:gtype (cffi:foreign-funcall "gdk_grab_ownership_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:grab-ownership
          (gobject:symbol-for-gtype "GdkGrabOwnership")))
  ;; Check the names
  (is (equal '("GDK_OWNERSHIP_NONE" "GDK_OWNERSHIP_WINDOW"
               "GDK_OWNERSHIP_APPLICATION")
             (list-enum-item-name "GdkGrabOwnership")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GdkGrabOwnership")))
  ;; Check the nick names
  (is (equal '("none" "window" "application")
             (list-enum-item-nick "GdkGrabOwnership")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkGrabOwnership"
                             GDK-GRAB-OWNERSHIP
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_grab_ownership_get_type")
                             (:NONE 0)
                             (:WINDOW 1)
                             (:APPLICATION 2))
             (gobject:get-g-type-definition "GdkGrabOwnership"))))

;;;     GdkTimeCoord

;;;     GdkGrabStatus

(test grab-status
  ;; Check the type
  (is (g:type-is-enum "GdkGrabStatus"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkGrabStatus")
          (g:gtype (cffi:foreign-funcall "gdk_grab_status_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:grab-status
          (gobject:symbol-for-gtype "GdkGrabStatus")))
  ;; Check the names
  (is (equal '("GDK_GRAB_SUCCESS" "GDK_GRAB_ALREADY_GRABBED"
               "GDK_GRAB_INVALID_TIME" "GDK_GRAB_NOT_VIEWABLE"
               "GDK_GRAB_FROZEN" "GDK_GRAB_FAILED")
             (list-enum-item-name "GdkGrabStatus")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GdkGrabStatus")))
  ;; Check the nick names
  (is (equal '("success" "already-grabbed" "invalid-time" "not-viewable"
               "frozen" "failed")
             (list-enum-item-nick "GdkGrabStatus")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkGrabStatus"
                             GDK-GRAB-STATUS
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_grab_status_get_type")
                             (:SUCCESS 0)
                             (:ALREADY-GRABBED 1)
                             (:INVALID-TIME 2)
                             (:NOT-VIEWABLE 3)
                             (:FROZEN 4)
                             (:FAILED 5))
             (gobject:get-g-type-definition "GdkGrabStatus"))))

;;;     GdkDeviceTool                                      Since 3.22

(test device-tool-class
  ;; Type check
  (is (g:type-is-object "GdkDeviceTool"))
  ;; Check the registered name
  (is (eq 'gdk:device-tool
          (gobject:symbol-for-gtype "GdkDeviceTool")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceTool")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
      (g:type-parent "GdkDeviceTool")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g:type-name (g:type-children "GdkDeviceTool"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g:type-name (g:type-interfaces "GdkDeviceTool"))))
  ;; Check the class properties
  (is (equal '("axes" "hardware-id" "serial" "tool-type")
             (sort (mapcar #'g:param-spec-name
                           (g:object-class-list-properties "GdkDeviceTool"))
                   #'string-lessp)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDeviceTool" GDK-DEVICE-TOOL
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_tool_get_type")
                       ((AXES GDK-DEVICE-TOOL-AXES "axes" "GdkAxisFlags" T NIL)
                        (HARDWARE-ID GDK-DEVICE-TOOL-HARDWARE-ID "hardware-id"
                         "guint64" T NIL)
                        (SERIAL GDK-DEVICE-TOOL-SERIAL "serial" "guint64" T
                         NIL)
                        (TOOL-TYPE GDK-DEVICE-TOOL-TOOL-TYPE "tool-type"
                         "GdkDeviceToolType" T NIL)))
             (gobject:get-g-type-definition "GdkDeviceTool"))))

;;;     GdkDevice

(test device-class
  ;; Type check
  (is (g:type-is-object "GdkDevice"))
  ;; Check the registered name
  (is (eq 'gdk:device
          (gobject:symbol-for-gtype "GdkDevice")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDevice")
          (g:gtype (cffi:foreign-funcall "gdk_device_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDevice")))
  ;; Check the children
  #-windows
  (is (equal '("GdkWaylandDevice" "GdkX11DeviceXI2")
             (list-children "GdkDevice")))
  #+windows
  (is (equal '("GdkDeviceVirtual" "GdkDeviceWin32")
             (list-children "GdkDevice")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDevice")))
  ;; Check the class properties
  (is (equal '("associated-device" "axes" "device-manager" "display"
               "has-cursor" "input-mode" "input-source" "n-axes" "name"
               "num-touches" "product-id" "seat" "tool" "type" "vendor-id")
             (list-properties "GdkDevice")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDevice" GDK-DEVICE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_get_type")
                       ((ASSOCIATED-DEVICE GDK-DEVICE-ASSOCIATED-DEVICE
                         "associated-device" "GdkDevice" T NIL)
                        (AXES GDK-DEVICE-AXES "axes" "GdkAxisFlags" T NIL)
                        (DEVICE-MANAGER GDK-DEVICE-DEVICE-MANAGER
                         "device-manager" "GdkDeviceManager" T NIL)
                        (DISPLAY GDK-DEVICE-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (HAS-CURSOR GDK-DEVICE-HAS-CURSOR "has-cursor"
                         "gboolean" T NIL)
                        (INPUT-MODE GDK-DEVICE-INPUT-MODE "input-mode"
                         "GdkInputMode" T T)
                        (INPUT-SOURCE GDK-DEVICE-INPUT-SOURCE "input-source"
                         "GdkInputSource" T NIL)
                        (N-AXES GDK-DEVICE-N-AXES "n-axes" "guint" T NIL)
                        (NAME GDK-DEVICE-NAME "name" "gchararray" T NIL)
                        (NUM-TOUCHES GDK-DEVICE-NUM-TOUCHES "num-touches"
                         "guint" T NIL)
                        (PRODUCT-ID GDK-DEVICE-PRODUCT-ID "product-id"
                         "gchararray" T NIL)
                        (SEAT GDK-DEVICE-SEAT "seat" "GdkSeat" T T)
                        (TOOL GDK-DEVICE-TOOL "tool" "GdkDeviceTool" T NIL)
                        (TYPE GDK-DEVICE-TYPE "type" "GdkDeviceType" T NIL)
                        (VENDOR-ID GDK-DEVICE-VENDOR-ID "vendor-id"
                         "gchararray" T NIL)))
             (gobject:get-g-type-definition "GdkDevice"))))

;;; --- Properties -------------------------------------------------------------

(test device-properties.1
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-keyboard seat)))
    (is (typep (gdk:device-associated-device device) 'gdk:device))
    (is-false (gdk:device-axes device))
    (is (typep (gdk:device-device-manager device) 'gdk:device-manager))
    (is (typep (gdk:device-display device) 'gdk:display))
    (is-false (gdk:device-has-cursor device))
    (is (eq :screen (gdk:device-input-mode device)))
    (is (eq :keyboard (gdk:device-input-source device)))
    (is (= 0 (gdk:device-n-axes device)))
    #-windows
    (is (string= "Core Keyboard" (gdk:device-name device)))
    #+windows
    (is (string= "Virtual Core Keyboard" (gdk:device-name device)))
    (is (= 0 (gdk:device-num-touches device)))
    (is-false (gdk:device-product-id device))
    (is (typep (gdk:device-seat device) 'gdk:seat))
    (is-false (gdk:device-tool device))
    (is (eq :master (gdk:device-type device)))
    (is-false (gdk:device-vendor-id device))))

(test device-properties.2
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat)))
    (is (typep (gdk:device-associated-device device) 'gdk:device))
    #-windows
    (is (equal '(:X :Y) (gdk:device-axes device)))
    #+windows
    (is (equal '(:x :y) (gdk:device-axes device)))
    (is (typep (gdk:device-device-manager device) 'gdk:device-manager))
    (is (typep (gdk:device-display device) 'gdk:display))
    (is-true (gdk:device-has-cursor device))
    (is (eq :screen (gdk:device-input-mode device)))
    (is (eq :mouse (gdk:device-input-source device)))
    #-windows
    (is (= 2 (gdk:device-n-axes device)))
    #+windows
    (is (= 2 (gdk:device-n-axes device)))
    #-windows
    (is (string= "Core Pointer" (gdk:device-name device)))
    #+windows
    (is (string= "Virtual Core Pointer" (gdk:device-name device)))
    (is (= 0 (gdk:device-num-touches device)))
    (is-false (gdk:device-product-id device))
    (is (typep (gdk:device-seat device) 'gdk:seat))
    (is-false (gdk:device-tool device))
    (is (eq :master (gdk:device-type device)))
    (is-false (gdk:device-vendor-id device))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

#+nil
(test gtk-device-changed-signal
  (let* ((message nil)
         (seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (handler-id (g-signal-connect device "changed"
                       (lambda (device)
                         (setf message "Signal changed")
                         (is (typep device 'gdk:device))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit device "changed"))
    (is (string= "Signal changed" message))
    (is-false (g-signal-handler-disconnect device handler-id))))

;;;     tool-changed

#+nil
(test gtk-device-tool-changed-signal
  (let* ((message nil)
         (seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (handler-id (g-signal-connect device "tool-changed"
                       (lambda (device tool)
                         (setf message "Signal tool-changed")
                         (is (typep device 'gdk:device))
                         (is (typep tool 'gdk:device-tool))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit device "tool-changed"
                             (make-instance 'gdk:device-tool)))
    (is (string= "Signal tool-changed" message))
    (is-false (g-signal-handler-disconnect device handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-device-key

;; TODO: Error on Windows, check this again
;; Gdk-CRITICAL **: 13:18:50.059: gdk_device_get_key: assertion
;; 'index_ < device->num_keys' failed

#+nil
(test device-key
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-keyboard seat)))
    (is (equal '(97 :control-mask)
               (multiple-value-list (setf (gdk:device-key device 0)
                                          '(97 :control-mask)))))
    (is (equal '(97 (:control-mask))
               (multiple-value-list (gdk:device-key device 0))))
    (is (equal '(65 (:shift-mask :control-mask))
               (multiple-value-list (setf (gdk:device-key device 0)
                                          '(65 (:shift-mask :control-mask))))))
    (is (equal '(65 (:SHIFT-MASK :CONTROL-MASK))
               (multiple-value-list (gdk:device-key device 0))))))

;;;     gdk-device-axis-use

(test device-axis-use
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat)))
    (is (eq :x (setf (gdk:device-axis-use device 0) :x)))
    (is (eq :x (gdk:device-axis-use device 0)))
    (is (eq :ignore (setf (gdk:device-axis-use device 0) :ignore)))
    (is (eq :ignore (gdk:device-axis-use device 0)))))

;;;     gdk-device-list-slave-devices

(test device-list-slave-devices
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat)))
    (is (every #'stringp
               (mapcar #'gdk:device-name
                       (gdk:device-list-slave-devices device))))))

;;;     gdk-device-n-keys

(test device-n-keys
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat)))
    (is (= 0 (gdk:device-n-keys device)))))

;;;     gdk_device_warp
;;;     gdk_device_grab
;;;     gdk_device_ungrab

;;;     gdk-device-state

#+nil
(test device-state
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (window (gdk:default-root-window)))
    (is (listp (multiple-value-list (gdk:device-state device window))))
    (is (every (lambda (x) (typep x 'double-float))
               (first (multiple-value-list (gdk:device-state device window)))))
    #-windows
    (is (equal '(:MOD2-MASK)
               (second (multiple-value-list (gdk:device-state device window)))))
    #+windows
    (is (equal '()
               (second (multiple-value-list (gdk:device-state device window)))))))

;;;     gdk_device_get_position
;;;     gdk_device_get_position_double
;;;     gdk_device_get_window_at_position
;;;     gdk_device_get_window_at_position_double

;;;     gdk-device-history

;; TODO: We need an example to test the functions.

(test device-history
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (window (gdk:default-root-window)))
    (is-false (gdk:device-history device window 0 1000))))

;;;     gdk-device-axis

;; TODO: Find an example with a result different from nil.

#+nil
(test device-axis
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (window (gdk:default-root-window))
         (axes (gdk:device-state device window)))
    (is (every #'numberp axes))
    (is-false (gdk:device-axis device axes :x))
    #-windows
    (is-false (gdk:device-axis device axes :y))
    #+windows ; On Windows we get a double float for :y. Why?
    (is (numberp (gdk:device-axis device axes :y)))
    (is-false (gdk:device-axis device axes :pressure))
    (is-false (gdk:device-axis device axes :xtilt))
    (is-false (gdk:device-axis device axes :ytilt))
    (is-false (gdk:device-axis device axes :wheel))
    (is-false (gdk:device-axis device axes :distance))
    (is-false (gdk:device-axis device axes :rotation))
    (is-false (gdk:device-axis device axes :slider))
    (is-false (gdk:device-axis device axes :last))))

;;;     gdk-device-list-axes

#+nil
(test device-list-axes
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat)))
    #-windows
    (is (equal '("Rel X" "Rel Y" "Rel Horiz Scroll" "Rel Vert Scroll")
                (gdk:device-list-axes device)))
    #+windows
    (is (equal '("NONE" "NONE")
                (gdk:device-list-axes device)))))

;;;     gdk-device-axis-value

;; On Windows we get "NONE" for the device axes

#+nil
(test device-axis-value
  (let* ((seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (window (gdk:default-root-window))
         (axes (gdk:device-state device window)))
    (is (typep (gdk:device-axis-value device axes "Rel X") 'double-float))
    (is (typep (gdk:device-axis-value device axes "Rel Y") 'double-float))
    (is (typep (gdk:device-axis-value device axes "Rel Horiz Scroll")
               'double-float))
    (is (typep (gdk:device-axis-value device axes "Rel Vert Scroll")
               'double-float))))

;;;     gdk_device_get_last_event_window

;;;     gdk_device_tool_get_serial
;;;     gdk_device_tool_get_tool_type

;;; --- 2023-1-8 ---------------------------------------------------------------
