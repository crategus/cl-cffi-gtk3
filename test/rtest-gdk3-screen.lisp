(in-package :gtk-test)

(def-suite gdk-screen :in gdk-suite)
(in-suite gdk-screen)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkScreen

;; Create a new screen to make sure that GdkWaylandCursor is present.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (gdk:screen-default))

(test gdk-screen-class
  ;; Check type
  (is (g:type-is-object "GdkScreen"))
  ;; Check registered name
  (is (eq 'gdk:screen
          (glib:symbol-for-gtype "GdkScreen")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkScreen")
          (g:gtype (cffi:foreign-funcall "gdk_screen_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkScreen")))
  ;; Check children
  #+crategus
  (when *first-run-testsuite*
    (is (equal '("GdkWaylandScreen")
               (glib-test:list-children "GdkScreen"))))
  #+windows
  (is (equal '("GdkWin32Screen")
             (glib-test:list-children "GdkScreen")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkScreen")))
  ;; Check class properties
  (is (equal '("font-options" "resolution")
             (glib-test:list-properties "GdkScreen")))
  ;; Check signals
  (is (equal '("composited-changed" "monitors-changed" "size-changed")
             (glib-test:list-signals "GdkScreen")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkScreen" GDK:SCREEN
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_screen_get_type")
                       ((FONT-OPTIONS SCREEN-FONT-OPTIONS
                         "font-options" "gpointer" T T)
                        (RESOLUTION SCREEN-RESOLUTION
                         "resolution" "gdouble" T T)))
             (gobject:get-gtype-definition "GdkScreen"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-screen-properties
  (let ((screen (gdk:screen-default)))
    (is (cffi:pointerp  (gdk:screen-font-options screen)))
    (is (typep (gdk:screen-resolution screen) 'double-float))))

;; TODO: We cannot pass NIL for a NULL-POINTER for an instance like
;; CAIRO:FONT-OPTIONS-T. Can we improve this?!

(test gdk-screen-font-options
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

;;;     composited-changed

(test gdk-screen-composited-changed-signal
  (let* ((name "composited-changed")
         (gtype (g:gtype "GdkScreen"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     monitors-changed

(test gdk-screen-monitors-changed-signal
  (let* ((name "monitors-changed")
         (gtype (g:gtype "GdkScreen"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     size-changed

(test gdk-screen-size-changed-signal
  (let* ((name "size-changed")
         (gtype (g:gtype "GdkScreen"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk-screen-default

(test gdk-screen-default
  (is (typep (gdk:screen-default) 'gdk:screen)))

;;;     gdk-screen-system-visual

(test gdk-screen-system-visual
  (is (typep (gdk:screen-system-visual (gdk:screen-default)) 'gdk:visual)))

;;;     gdk-screen-rgba-visual

(test gdk-screen-rgba-visual
  (is (typep (gdk:screen-rgba-visual (gdk:screen-default)) 'gdk:visual)))

;;;     gdk-screen-is-composited

(test gdk-screen-is-composited
  (is (gdk:screen-is-composited (gdk:screen-default))))

;;;     gdk-screen-root-window

(test gdk-screen-root-window
  (is (typep (gdk:screen-root-window (gdk:screen-default)) 'gdk:window)))

;;;     gdk-screen-display

(test gdk-screen-display
  (is (typep (gdk:screen-display (gdk:screen-default)) 'gdk:display)))

;;;     gdk-screen-number                                   deprecated
;;;     gdk-screen-width                                    deprecated
;;;     gdk-screen-height                                   deprecated
;;;     gdk-screen-width-mm                                 deprecated
;;;     gdk-screen-height-mm                                deprecated

;;;     gdk-screen-list-visuals

(test gdk-screen-list-visuals
  (let ((screen (gdk:screen-default)))
    (is (> (length (gdk:screen-list-visuals screen)) 0))
    (is (every (lambda (x) (typep x 'gdk:visual))
               (gdk:screen-list-visuals screen)))))

;;;     gdk-screen-toplevel-windows

(test gdk-screen-toplevel-windows
  (is (listp (gdk:screen-toplevel-windows (gdk:screen-default))))
  (is (every (lambda (x) (typep x 'gdk:window))
             (gdk:screen-toplevel-windows (gdk:screen-default)))))

;;;     gdk-screen-make-display-name                        deprecated
;;;     gdk-screen-n-monitors                               deprecated
;;;     gdk_screen_get_primary_monitor                      deprecated
;;;     gdk-screen-monitor-geometry                         deprecated
;;;     gdk-screen-monitor-workarea                         deprecated
;;;     gdk-screen-monitor-at-point                         deprecated
;;;     gdk-screen-monitor-at-window                        deprecated
;;;     gdk-screen-monitor-height-mm                        deprecated
;;;     gdk-screen-monitor-width-mm                         deprecated
;;;     gdk-screen-monitor-plug-name                        deprecated
;;;     gdk-screen-monitor-scale-factor                     deprecated

;;;     gdk-screen-setting

(test gdk-screen-setting
  (let ((screen (gdk:display-default-screen (gdk:display-default))))
    (is (integerp (gdk:screen-setting screen "gtk-double-click-time" "gint")))))

;;;     gdk-screen-active-window                            deprecated

;;;     gdk-screen-window-stack

(test gdk-screen-window-stack
  (is (listp (gdk:screen-window-stack (gdk:screen-default))))
  (is (every (lambda (x) (typep x 'gdk:window))
             (gdk:screen-window-stack (gdk:screen-default)))))

;;; 2025-4-26
