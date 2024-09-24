(in-package :gtk-test)

(def-suite gdk-device-manager :in gdk-suite)
(in-suite gdk-device-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDeviceManager

(test gdk-device-manager-class
  ;; Check type
  (is (g:type-is-object "GdkDeviceManager"))
  ;; Check registered name
  (is (eq 'gdk:device-manager
          (glib:symbol-for-gtype "GdkDeviceManager")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDeviceManager")
          (g:gtype (cffi:foreign-funcall "gdk_device_manager_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDeviceManager")))
  ;; Check children
  #-windows
  (is (or (equal '("GdkX11DeviceManagerCore")
                 (glib-test:list-children "GdkDeviceManager"))
          (equal '("GdkWaylandDeviceManager" "GdkX11DeviceManagerCore")
                 (glib-test:list-children "GdkDeviceManager"))))
  #+windows
  (is (equal '("GdkDeviceManagerWin32")
             (glib-test:list-children "GdkDeviceManager")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDeviceManager")))
  ;; Check class properties
  (is (equal '("display")
             (glib-test:list-properties "GdkDeviceManager")))
  ;; Check signals
  (is (equal '("device-added" "device-changed" "device-removed")
             (glib-test:list-signals "GdkDeviceManager")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDeviceManager" GDK:DEVICE-MANAGER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_manager_get_type")
                       ((DISPLAY DEVICE-MANAGER-DISPLAY
                         "display" "GdkDisplay" T NIL)))
             (gobject:get-gtype-definition "GdkDeviceManager"))))

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

;;; 2024-9-22
