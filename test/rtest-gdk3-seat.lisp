(in-package :gtk-test)

(def-suite gdk-seat :in gdk-suite)
(in-suite gdk-seat)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSeatCapabilities

(test gdk-seat-capabilities-flags
  ;; Check type
  (is (g:type-is-flags "GdkSeatCapabilities"))
  ;; Check registered name
  (is (eq 'gdk:seat-capabilities
          (glib:symbol-for-gtype "GdkSeatCapabilities")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSeatCapabilities")
          (g:gtype (cffi:foreign-funcall "gdk_seat_capabilities_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_SEAT_CAPABILITY_NONE" "GDK_SEAT_CAPABILITY_POINTER"
               "GDK_SEAT_CAPABILITY_TOUCH" "GDK_SEAT_CAPABILITY_TABLET_STYLUS"
               "GDK_SEAT_CAPABILITY_KEYBOARD" "GDK_SEAT_CAPABILITY_ALL_POINTING"
               "GDK_SEAT_CAPABILITY_ALL")
             (glib-test:list-flags-item-names "GdkSeatCapabilities")))
  ;; Check values
  (is (equal '(0 1 2 4 8 7 15)
             (glib-test:list-flags-item-values "GdkSeatCapabilities")))
  ;; Check nick names
  (is (equal '("none" "pointer" "touch" "tablet-stylus" "keyboard"
               "all-pointing" "all")
             (glib-test:list-flags-item-nicks "GdkSeatCapabilities")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkSeatCapabilities" GDK:SEAT-CAPABILITIES
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_seat_capabilities_get_type")
                       (:NONE 0)
                       (:POINTER 1)
                       (:TOUCH 2)
                       (:TABLET-STYLUS 4)
                       (:KEYBOARD 8)
                       (:ALL-POINTING 7)
                       (:ALL 15))
             (gobject:get-gtype-definition "GdkSeatCapabilities"))))

;;;     GdkSeat

(test gdk-seat-class
  ;; Check type
  (is (g:type-is-object "GdkSeat"))
  ;; Check registered name
  (is (eq 'gdk:seat
          (glib:symbol-for-gtype "GdkSeat")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSeat")
          (g:gtype (cffi:foreign-funcall "gdk_seat_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkSeat")))
  ;; Check children
  (is (or (equal '("GdkSeatDefault")
                 (glib-test:list-children "GdkSeat"))
          (equal '("GdkWaylandSeat")
                 (glib-test:list-children "GdkSeat"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkSeat")))
  ;; Check class properties
  (is (equal '("display")
             (glib-test:list-properties "GdkSeat")))
  ;; Check signals
  (is (equal '("device-added" "device-removed" "tool-added" "tool-removed")
             (glib-test:list-signals "GdkSeat")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkSeat" GDK:SEAT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_seat_get_type")
                       ((DISPLAY SEAT-DISPLAY "display" "GdkDisplay" T NIL)))
             (gobject:get-gtype-definition "GdkSeat"))))

;;; --- Properties -------------------------------------------------------------

;;;     gdk-seat-display

(test gdk-seat-display
  (let ((seat (gdk:display-default-seat (gdk:display-default))))
    (is (typep (gdk:seat-display seat) 'gdk:display))))

;;; --- Signals ----------------------------------------------------------------

;;;     device-added

#+nil
(test gdk-seat-device-added-signal
  (let* ((message nil)
         (seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (handler-id (g-signal-connect seat "device-added"
                       (lambda (seat device)
                         (setf message "Signal device-added")
                         (is (typep seat 'gdk:seat))
                         (is (typep device 'gdk:device))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "device-added" device))
    (is (string= "Signal device-added" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;;     device-removed

#+nil
(test gdk-seat-device-removed-signal
  (let* ((message nil)
         (seat (gdk:display-default-seat (gdk:display-default)))
         (device (gdk:seat-pointer seat))
         (handler-id (g-signal-connect seat "device-removed"
                       (lambda (seat device)
                         (setf message "Signal device-removed")
                         (is (typep seat 'gdk:seat))
                         (is (typep device 'gdk:device))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "device-removed" device))
    (is (string= "Signal device-removed" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;;     tool-added

#+nil
(test gdk-seat-tool-added-signal
  (let* ((message nil)
         (seat (gdk:display-default-seat (gdk:display-default)))
         (tool (make-instance 'gdk:device-tool))
         (handler-id (g-signal-connect seat "tool-added"
                       (lambda (seat tool)
                         (setf message "Signal tool-added")
                         (is (typep seat 'gdk:seat))
                         (is (typep tool 'gdk:device-tool))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "tool-added" tool))
    (is (string= "Signal tool-added" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;;     tool-removed

#+nil
(test gdk-seat-tool-removed-signal
  (let* ((message nil)
         (seat (gdk:display-default-seat (gdk:display-default)))
         (tool (make-instance 'gdk:device-tool))
         (handler-id (g-signal-connect seat "tool-removed"
                       (lambda (seat tool)
                         (setf message "Signal tool-removed")
                         (is (typep seat 'gdk:seat))
                         (is (typep tool 'gdk:device-tool))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit seat "tool-removed" tool))
    (is (string= "Signal tool-removed" message))
    (is-false (g-signal-handler-disconnect seat handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-seat-grab
;;;     gdk-seat-ungrab

;; TODO: Causes a warning:
;; GLib-CRITICAL: g_hash_table_lookup: assertion 'hash_table != NULL' failed

#+nil
(test gdk-seat-grab/ungrab
  (let ((seat (gdk:display-default-seat (gdk:display-default)))
        (window (gdk:default-root-window)))
    (is-false (gdk:window-show window))
    (is (eq :success (gdk:seat-grab seat window :all nil nil nil nil)))
    (is-false (gdk:seat-ungrab seat))))

;;;     gdk-seat-capabilities

(test gdk-seat-capabilities
  (let ((seat (gdk:display-default-seat (gdk:display-default))))
    (is (equal '(:POINTER :KEYBOARD)
               (gdk:seat-capabilities seat)))))

;;;     gdk-seat-pointer

(test gdk-seat-pointer
  (let ((seat (gdk:display-default-seat (gdk:display-default))))
    (is (typep (gdk:seat-pointer seat) 'gdk:device))))

;;;     gdk-seat-keyboard

(test gdk-seat-keyboard
  (let ((seat (gdk:display-default-seat (gdk:display-default))))
    (is (typep (gdk:seat-keyboard seat) 'gdk:device))))

;;;     gdk-seat-slaves

(test gdk-seat-slaves
  (let ((seat (gdk:display-default-seat (gdk:display-default))))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat :pointer)))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat :touch)))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat :tablet-stylus)))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat :keyboard)))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat :all-pointing)))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat :all)))
    (is (every (lambda (x) (typep x 'gdk:device))
               (gdk:seat-slaves seat '(:pointer :keyboard))))))

;;; 2024-9-22
