(in-package :gtk-test)

(def-suite gdk-monitor :in gdk-suite)
(in-suite gdk-monitor)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSubpixelLayout

(test subpixel-layout
  ;; Check the type
  (is (g:type-is-enum "GdkSubpixelLayout"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkSubpixelLayout")
          (g:gtype (cffi:foreign-funcall "gdk_subpixel_layout_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:subpixel-layout
          (glib:symbol-for-gtype "GdkSubpixelLayout")))
  ;; Check the names
  (is (equal '("GDK_SUBPIXEL_LAYOUT_UNKNOWN"
               "GDK_SUBPIXEL_LAYOUT_NONE"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR")
             (list-enum-item-name "GdkSubpixelLayout")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GdkSubpixelLayout")))
  ;; Check the nick names
  (is (equal '("unknown" "none" "horizontal-rgb" "horizontal-bgr" "vertical-rgb"
               "vertical-bgr")
             (list-enum-item-nick "GdkSubpixelLayout")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkSubpixelLayout"
                             GDK-SUBPIXEL-LAYOUT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_subpixel_layout_get_type")
                             (:UNKNOWN 0)
                             (:NONE 1)
                             (:HORIZONTAL-RGB 2)
                             (:HORIZONTAL-BGR 3)
                             (:VERTICAL-RGB 4)
                             (:VERTICAL-BGR 5))
             (gobject:get-g-type-definition "GdkSubpixelLayout"))))

;;;     GdkMonitor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gdk_broadway_monitor_get_type" :size))

(test monitor-class
  ;; Type check
  (is (g:type-is-object "GdkMonitor"))
  ;; Check the registered name
  (is (eq 'gdk:monitor
          (glib:symbol-for-gtype "GdkMonitor")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkMonitor")
          (g:gtype (cffi:foreign-funcall "gdk_monitor_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkMonitor")))
  ;; Check the children
  #-windows
  (is (or (equal '("GdkBroadwayMonitor" "GdkX11Monitor")
                 (list-children "GdkMonitor"))
          (equal '("GdkBroadwayMonitor" "GdkWaylandMonitor")
                 (list-children "GdkMonitor"))))
  #+windows
  (is (equal '("GdkBroadwayMonitor" "GdkWin32Monitor")
             (list-children "GdkMonitor")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkMonitor")))
  ;; Check the class properties
  (is (equal '("display" "geometry" "height-mm" "manufacturer" "model"
               "refresh-rate" "scale-factor" "subpixel-layout" "width-mm"
               "workarea")
             (list-properties "GdkMonitor")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkMonitor" GDK-MONITOR
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_monitor_get_type")
                       ((DISPLAY GDK-MONITOR-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (GEOMETRY GDK-MONITOR-GEOMETRY "geometry"
                         "GdkRectangle" T NIL)
                        (HEIGHT-MM GDK-MONITOR-HEIGHT-MM "height-mm" "gint" T
                         NIL)
                        (MANUFACTURER GDK-MONITOR-MANUFACTURER "manufacturer"
                         "gchararray" T NIL)
                        (MODEL GDK-MONITOR-MODEL "model" "gchararray" T NIL)
                        (REFRESH-RATE GDK-MONITOR-REFRESH-RATE "refresh-rate"
                         "gint" T NIL)
                        (SCALE-FACTOR GDK-MONITOR-SCALE-FACTOR "scale-factor"
                         "gint" T NIL)
                        (SUBPIXEL-LAYOUT GDK-MONITOR-SUBPIXEL-LAYOUT
                         "subpixel-layout" "GdkSubpixelLayout" T NIL)
                        (WIDTH-MM GDK-MONITOR-WIDTH-MM "width-mm" "gint" T NIL)
                        (WORKAREA GDK-MONITOR-WORKAREA "workarea"
                         "GdkRectangle" T NIL)))
             (gobject:get-g-type-definition "GdkMonitor"))))

;;; --- Properties -------------------------------------------------------------

#+nil
(test monitor-properties
  (let ((monitor (gdk:display-primary-monitor (gdk:display-default))))
    ;; gdk:monitor-display
    (is (typep (gdk:monitor-display monitor) 'gdk:display))
    ;; gdk:monitor-geometry
    (is (typep (gdk:monitor-geometry monitor) 'gdk:rectangle))
    ;; gdk:monitor-height-mm
    (is (<= 193 (gdk:monitor-height-mm monitor)))
    ;; gdk:monitor-manufacturer
    (is (stringp (gdk:monitor-manufacturer monitor)))
    ;; gdk:monitor-model
    (is (stringp (gdk:monitor-model monitor)))
    ;; gdk:monitor-refresh-rate
    (is (integerp (gdk:monitor-refresh-rate monitor)))
    ;; gdk:monitor-scale-factor
    (is (= 1 (gdk:monitor-scale-factor monitor)))
    ;; gdk:monitor-subpixel-layout
    (is (eq :unknown (gdk:monitor-subpixel-layout monitor)))
    ;; gdk:monitor-width-mm
    (is (<= 344 (gdk:monitor-width-mm monitor)))
    ;; gdk:monitor-workarea
    (is (typep (gdk:monitor-workarea monitor) 'gdk:rectangle))))

;;; --- Signals ----------------------------------------------------------------

#+nil
(test monitor-invalidate-signal
  (let* ((message nil)
         (monitor (gdk:display-primary-monitor (gdk:display-default)))
         (handler-id (g-signal-connect monitor "invalidate"
                       (lambda (object)
                         (setf message "Signal invalidate")
                         (is (typep object 'gdk:monitor))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit monitor "invalidate"))
    (is (string= "Signal invalidate" message))
    (is-false (g-signal-handler-disconnect monitor handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-monitor-is-primary

#+nil
(test monitor-is-primary
  (let ((monitor (gdk:display-primary-monitor (gdk:display-default))))
    (is-true (gdk:monitor-is-primary monitor))))

;;; --- 2023-5-29 --------------------------------------------------------------
