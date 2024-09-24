(in-package :gtk-test)

(def-suite gdk-monitor :in gdk-suite)
(in-suite gdk-monitor)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSubpixelLayout

(test gdk-subpixel-layout
  ;; Check type
  (is (g:type-is-enum "GdkSubpixelLayout"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSubpixelLayout")
          (g:gtype (cffi:foreign-funcall "gdk_subpixel_layout_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:subpixel-layout
          (glib:symbol-for-gtype "GdkSubpixelLayout")))
  ;; Check names
  (is (equal '("GDK_SUBPIXEL_LAYOUT_UNKNOWN"
               "GDK_SUBPIXEL_LAYOUT_NONE"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR")
             (glib-test:list-enum-item-names "GdkSubpixelLayout")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GdkSubpixelLayout")))
  ;; Check nick names
  (is (equal '("unknown" "none" "horizontal-rgb" "horizontal-bgr" "vertical-rgb"
               "vertical-bgr")
             (glib-test:list-enum-item-nicks "GdkSubpixelLayout")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkSubpixelLayout" GDK:SUBPIXEL-LAYOUT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_subpixel_layout_get_type")
                       (:UNKNOWN 0)
                       (:NONE 1)
                       (:HORIZONTAL-RGB 2)
                       (:HORIZONTAL-BGR 3)
                       (:VERTICAL-RGB 4)
                       (:VERTICAL-BGR 5))
             (gobject:get-gtype-definition "GdkSubpixelLayout"))))

;;;     GdkMonitor

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:foreign-funcall "gdk_broadway_monitor_get_type" :size))

(test gdk-monitor-class
  ;; Check type
  (is (g:type-is-object "GdkMonitor"))
  ;; Check registered name
  (is (eq 'gdk:monitor
          (glib:symbol-for-gtype "GdkMonitor")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkMonitor")
          (g:gtype (cffi:foreign-funcall "gdk_monitor_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkMonitor")))
  ;; Check children
  #-windows
  (is (or (equal '("GdkBroadwayMonitor" "GdkX11Monitor")
                 (glib-test:list-children "GdkMonitor"))
          (equal '("GdkBroadwayMonitor" "GdkWaylandMonitor")
                 (glib-test:list-children "GdkMonitor"))))
  #+windows
  (is (equal '("GdkBroadwayMonitor" "GdkWin32Monitor")
             (glib-test:list-children "GdkMonitor")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkMonitor")))
  ;; Check class properties
  (is (equal '("display" "geometry" "height-mm" "manufacturer" "model"
               "refresh-rate" "scale-factor" "subpixel-layout" "width-mm"
               "workarea")
             (glib-test:list-properties "GdkMonitor")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkMonitor" GDK:MONITOR
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_monitor_get_type")
                       ((DISPLAY MONITOR-DISPLAY "display" "GdkDisplay" T NIL)
                        (GEOMETRY MONITOR-GEOMETRY
                         "geometry" "GdkRectangle" T NIL)
                        (HEIGHT-MM MONITOR-HEIGHT-MM "height-mm" "gint" T NIL)
                        (MANUFACTURER MONITOR-MANUFACTURER
                         "manufacturer" "gchararray" T NIL)
                        (MODEL MONITOR-MODEL "model" "gchararray" T NIL)
                        (REFRESH-RATE MONITOR-REFRESH-RATE
                         "refresh-rate" "gint" T NIL)
                        (SCALE-FACTOR MONITOR-SCALE-FACTOR
                         "scale-factor" "gint" T NIL)
                        (SUBPIXEL-LAYOUT MONITOR-SUBPIXEL-LAYOUT
                         "subpixel-layout" "GdkSubpixelLayout" T NIL)
                        (WIDTH-MM MONITOR-WIDTH-MM "width-mm" "gint" T NIL)
                        (WORKAREA MONITOR-WORKAREA
                         "workarea" "GdkRectangle" T NIL)))
             (gobject:get-gtype-definition "GdkMonitor"))))

;;; --- Properties -------------------------------------------------------------

#+nil
(test gdk-monitor-properties
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
(test gdk-monitor-invalidate-signal
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
(test gdk-monitor-is-primary
  (let ((monitor (gdk:display-primary-monitor (gdk:display-default))))
    (is-true (gdk:monitor-is-primary monitor))))

;;; 2024-9-22
