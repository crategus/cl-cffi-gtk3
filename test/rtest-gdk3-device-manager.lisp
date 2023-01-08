(in-package :gtk-test)

(def-suite gdk-device-manager :in gdk-suite)
(in-suite gdk-device-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDeviceManager

(test g-object-class
  ;; Type check
  (is (g:type-is-object "GdkDeviceManager"))
  ;; Check the registered name
  (is (eq 'gdk:device-manager
          (gobject:symbol-for-gtype "GdkDeviceManager")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceManager")
          (g:gtype (cffi:foreign-funcall "gdk_device_manager_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDeviceManager")))
  ;; Check the children
  #-windows
  (is (equal '("GdkWaylandDeviceManager" "GdkX11DeviceManagerCore")
             (list-children "GdkDeviceManager")))
  #+windows
  (is (equal '("GdkDeviceManagerWin32")
             (list-children "GdkDeviceManager")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDeviceManager")))
  ;; Check the class properties
  (is (equal '("display")
             (list-properties "GdkDeviceManager")))
  ;; Check the signals
  (is (equal '("device-added" "device-changed" "device-removed")
             (list-signals "GdkDeviceManager")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDeviceManager" GDK-DEVICE-MANAGER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_manager_get_type")
                       ((DISPLAY GDK-DEVICE-MANAGER-DISPLAY "display"
                         "GdkDisplay" T NIL)))
             (gobject:get-g-type-definition "GdkDeviceManager"))))

;;; Functions
;;;
;;;     gdk_disable_multidevice
;;;     gdk_device_manager_get_display                     Accessor
;;;     gdk_device_manager_list_devices
;;;     gdk_device_manager_get_client_pointer
;;;
;;; Properties
;;;
;;;     display
;;;
;;; Signals
;;;
;;;     device-added
;;;     device-changed
;;;     device-removed

;;; --- 2023-1-8 ---------------------------------------------------------------
