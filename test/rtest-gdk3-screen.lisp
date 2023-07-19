(in-package :gtk-test)

(def-suite gdk-screen :in gdk-suite)
(in-suite gdk-screen)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkScreen

;; Create a new screen to make sure that GdkWaylandCursor is present.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gdk:screen-default))

(test screen-class
  ;; Type check
  (is (g:type-is-object "GdkScreen"))
  ;; Check the registered name
  (is (eq 'gdk:screen
          (glib:symbol-for-gtype "GdkScreen")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkScreen")
          (g:gtype (cffi:foreign-funcall "gdk_screen_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkScreen")))
  ;; Check the children
  #-windows
  (is (or (equal '("GdkX11Screen")
                 (list-children "GdkScreen"))
          (equal '("GdkWaylandScreen" "GdkX11Screen")
                 (list-children "GdkScreen"))))
  #+windows
  (is (equal '("GdkWin32Screen")
             (list-children "GdkScreen")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkScreen")))
  ;; Check the class properties
  (is (equal '("font-options" "resolution")
             (list-properties "GdkScreen")))
  (is (equal '("composited-changed" "monitors-changed" "size-changed")
             (list-signals "GdkScreen")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkScreen" GDK-SCREEN
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_screen_get_type")
                       ((FONT-OPTIONS GDK-SCREEN-FONT-OPTIONS "font-options"
                         "gpointer" T T)
                        (RESOLUTION GDK-SCREEN-RESOLUTION "resolution"
                         "gdouble" T T)))
             (gobject:get-g-type-definition "GdkScreen"))))

;;; --- Properties -------------------------------------------------------------

(test screen-properties
  (let ((screen (gdk:screen-default)))
    (is (cffi:pointerp  (gdk:screen-font-options screen)))
    (is (typep (gdk:screen-resolution screen) 'double-float))))

(test screen-font-options
  (let ((screen (gdk:screen-default))
        (options (cairo:font-options-create)))
    (is (cairo:font-options-equal options
                                  (setf (gdk:screen-font-options screen)
                                        options)))
    (is (cairo:font-options-equal options
                                 (gdk:screen-font-options screen)))
    ;; The NIL values signals an error
    (signals (error) (setf (gdk:screen-font-options screen) nil))
    ;; Use the NULL-POINTER as a value
    (is (cffi:null-pointer-p (setf (gdk:screen-font-options screen)
                                   (cffi:null-pointer))))
    (is (cffi:null-pointer-p (gdk:screen-font-options screen)))
    (is-false (cairo:font-options-destroy options))))

;;; --- Signals ----------------------------------------------------------------

;;;         composited-changed

#+nil
(test screen-composited-changed-signal
  (let* ((message nil)
         (screen (gdk:screen-default))
         (handler-id (g-signal-connect screen "composited-changed"
                       (lambda (screen)
                         (setf message "Signal composited-changed")
                         (is (typep screen 'gdk:screen))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit screen "composited-changed"))
    (is (string= "Signal composited-changed" message))
    (is-false (g-signal-handler-disconnect screen handler-id))))

;;;         monitors-changed

#+nil
(test screen-monitors-changed-signal
  (let* ((message nil)
         (screen (gdk:screen-default))
         (handler-id (g-signal-connect screen "monitors-changed"
                       (lambda (screen)
                         (setf message "Signal monitors-changed")
                         (is (typep screen 'gdk:screen))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit screen "monitors-changed"))
    (is (string= "Signal monitors-changed" message))
    (is-false (g-signal-handler-disconnect screen handler-id))))

;;;         size-changed

#+nil
(test screen-size-changed-signal
  (let* ((message nil)
         (screen (gdk:screen-default))
         (handler-id (g-signal-connect screen "size-changed"
                       (lambda (screen)
                         (setf message "Signal size-changed")
                         (is (typep screen 'gdk:screen))
                         t))))
    ;; Emit the signal
    (is-false (g-signal-emit screen "size-changed"))
    (is (string= "Signal size-changed" message))
    (is-false (g-signal-handler-disconnect screen handler-id))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-screen-default

(test screen-default
  (is (typep (gdk:screen-default) 'gdk:screen)))

;;;     gdk-screen-system-visual

(test screen-system-visual
  (is (typep (gdk:screen-system-visual (gdk:screen-default)) 'gdk:visual)))

;;;     gdk-screen-rgba-visual

(test screen-rgba-visual
  (is (typep (gdk:screen-rgba-visual (gdk:screen-default)) 'gdk:visual)))

;;;     gdk-screen-is-composited

(test screen-is-composited
  (is (gdk:screen-is-composited (gdk:screen-default))))

;;;     gdk-screen-root-window

(test screen-root-window
  (is (typep (gdk:screen-root-window (gdk:screen-default)) 'gdk:window)))

;;;     gdk-screen-display

(test screen-display
  (is (typep (gdk:screen-display (gdk:screen-default)) 'gdk:display)))

;;;     gdk-screen-number                                  deprecated

(test screen-number
  (is (integerp (gdk:screen-number (gdk:screen-default)))))

;;;     gdk-screen-width                                   deprecated

(test screen-width
  (is (integerp (gdk:screen-width)))
  (is (= (gdk:screen-width) (gdk:screen-width (gdk:screen-default)))))

;;;     gdk-screen-height                                  deprecated

(test screen-height
  (is (integerp (gdk:screen-height)))
  (is (= (gdk:screen-height) (gdk:screen-height (gdk:screen-default)))))

;;;     gdk-screen-width-mm                                deprecated

(test screen-width-mm
  (is (integerp (gdk:screen-width-mm)))
  (is (>= (gdk:screen-width-mm) (gdk:screen-width-mm (gdk:screen-default)))))

;;;     gdk-screen-height-mm                               deprecated

(test screen-height-mm
  (is (integerp (gdk:screen-height-mm)))
  (is (>= (gdk:screen-height-mm) (gdk:screen-height-mm (gdk:screen-default)))))

;;;     gdk-screen-list-visuals

(test screen-list-visuals
  (let ((screen (gdk:screen-default)))
    (is (> (length (gdk:screen-list-visuals screen)) 0))
    (is (every (lambda (x) (typep x 'gdk:visual))
               (gdk:screen-list-visuals screen)))))

;;;     gdk-screen-toplevel-windows

(test screen-toplevel-windows
  (is (listp (gdk:screen-toplevel-windows (gdk:screen-default))))
  (is (every (lambda (x) (typep x 'gdk:window))
             (gdk:screen-toplevel-windows (gdk:screen-default)))))

;;;     gdk-screen-make-display-name                       deprecated

(test screen-make-display-name
  (is (stringp (gdk:screen-make-display-name (gdk:screen-default)))))

;;;     gdk-screen-n-monitors                              deprecated

(test screen-n-monitors
  (is (<= 1 (gdk:screen-n-monitors (gdk:screen-default)))))

;;;     gdk_screen_get_primary_monitor                     deprecated

(test screen-primary-monitor
  (is (= 0 (gdk:screen-primary-monitor (gdk:screen-default)))))

;;;     gdk-screen-monitor-geometry                        deprecated

(test screen-monitor-geometry
  (is (typep (gdk:screen-monitor-geometry (gdk:screen-default) 0) 'gdk:rectangle))
  (let ((rect (gdk:screen-monitor-geometry (gdk:screen-default) 0)))
    (is (= 0 (gdk:rectangle-x rect)))
    (is (= 0 (gdk:rectangle-y rect)))
    (is (>= (gdk:screen-width) (gdk:rectangle-width rect)))
    (is (>= (gdk:screen-height) (gdk:rectangle-height rect)))))

;;;     gdk-screen-monitor-workarea                        deprecated

(test screen-monitor-workarea
  (is (typep (gdk:screen-monitor-workarea (gdk:screen-default) 0) 'gdk:rectangle))
  (let ((rect (gdk:screen-monitor-workarea (gdk:screen-default) 0)))
    (is (<= 0 (gdk:rectangle-x rect)))
    (is (<= 0 (gdk:rectangle-y rect)))
    (is (>= (gdk:screen-width) (gdk:rectangle-width rect)))
    (is (>= (gdk:screen-height) (gdk:rectangle-height rect)))))

;;;     gdk-screen-monitor-at-point                        deprecated

(test screen-monitor-at-point
  (is (= 0 (gdk:screen-monitor-at-point (gdk:screen-default)  0  0)))
  (is (= 0 (gdk:screen-monitor-at-point (gdk:screen-default) 10 10))))

;;;     gdk-screen-monitor-at-window                       deprecated

#-windows
(test screen-monitor-at-window
  (let ((screen (gdk:screen-default)))
    (is (= 0 (gdk:screen-monitor-at-window screen (gdk:screen-root-window screen))))))

;;;     gdk-screen-monitor-height-mm                       deprecated

(test screen-monitor-height-mm
  (is (<= 193 (gdk:screen-monitor-height-mm (gdk:screen-default) 0))))

;;;     gdk-screen-monitor-width-mm                        deprecated

(test screen-monitor-width-mm
  (is (<= 344 (gdk:screen-monitor-width-mm (gdk:screen-default) 0))))

;;;     gdk-screen-monitor-plug-name                       deprecated

#-windows
(test screen-monitor-plug-name
  (is (stringp (gdk:screen-monitor-plug-name (gdk:screen-default) 0))))

;;;     gdk-screen-monitor-scale-factor                    deprecated

(test screen-monitor-scale-factor
  (is (= 1 (gdk:screen-monitor-scale-factor (gdk:screen-default) 0))))

;;;     gdk-screen-setting

(test screen-setting
  (let ((screen (gdk:display-default-screen (gdk:display-default))))
    (is (integerp (gdk:screen-setting screen "gtk-double-click-time" "gint")))))

;;;     gdk-screen-active-window                           deprecated

#+nil
(test screen-active-window
  (is (typep (gdk:screen-active-window (gdk:screen-default)) 'gdk:window)))

;;;     gdk-screen-window-stack

(test screen-window-stack
  (is (listp (gdk:screen-window-stack (gdk:screen-default))))
  (is (every (lambda (x) (typep x 'gdk:window))
             (gdk:screen-window-stack (gdk:screen-default)))))

;;; --- 2023-5-29 --------------------------------------------------------------
