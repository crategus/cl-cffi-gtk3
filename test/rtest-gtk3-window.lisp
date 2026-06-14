(in-package :gtk-test)

(def-suite gtk-window :in gtk-suite)
(in-suite gtk-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowType

(test gtk-window-type
  ;; Check type
  (is (g:type-is-enum "GtkWindowType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowType")
          (g:gtype (cffi:foreign-funcall "gtk_window_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:window-type
          (glib:symbol-for-gtype "GtkWindowType")))
  ;; Check names
  (is (equal '("GTK_WINDOW_TOPLEVEL" "GTK_WINDOW_POPUP")
             (glib-test:list-enum-item-names "GtkWindowType")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkWindowType")))
  ;; Check nick names
  (is (equal '("toplevel" "popup")
             (glib-test:list-enum-item-nicks "GtkWindowType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkWindowType" GTK:WINDOW-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_window_type_get_type")
                                    (:TOPLEVEL 0)
                                    (:POPUP 1))
             (gobject:get-gtype-definition "GtkWindowType"))))

;;;     GtkWindowPosition

(test gtk-window-position
  ;; Check type
  (is (g:type-is-enum "GtkWindowPosition"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowPosition")
          (g:gtype (cffi:foreign-funcall "gtk_window_position_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:window-position
          (glib:symbol-for-gtype "GtkWindowPosition")))
  ;; Check names
  (is (equal '("GTK_WIN_POS_NONE" "GTK_WIN_POS_CENTER" "GTK_WIN_POS_MOUSE"
               "GTK_WIN_POS_CENTER_ALWAYS" "GTK_WIN_POS_CENTER_ON_PARENT")
             (glib-test:list-enum-item-names "GtkWindowPosition")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (glib-test:list-enum-item-values "GtkWindowPosition")))
  ;; Check nick names
  (is (equal '("none" "center" "mouse" "center-always" "center-on-parent")
             (glib-test:list-enum-item-nicks "GtkWindowPosition")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkWindowPosition" GTK:WINDOW-POSITION
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_window_position_get_type")
                                    (:NONE 0)
                                    (:CENTER 1)
                                    (:MOUSE 2)
                                    (:CENTER-ALWAYS 3)
                                    (:CENTER-ON-PARENT 4))
             (gobject:get-gtype-definition "GtkWindowPosition"))))

;;;     GtkWindow

(test gtk-window-class
  ;; Check type
  (is (g:type-is-object "GtkWindow"))
  ;; Check registered name
  (is (eq 'gtk:window
          (glib:symbol-for-gtype "GtkWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindow")
          (g:gtype (cffi:foreign-funcall "gtk_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkWindow")))
  ;; Check children
  #-windows
  (is (equal '("GtkApplicationWindow" "GtkAssistant" "GtkDialog"
               "GtkOffscreenWindow" "GtkPlug" "GtkShortcutsWindow")
             (glib-test:list-children "GtkWindow")))
  #+windows
  (is (equal '("GtkApplicationWindow" "GtkAssistant" "GtkDialog"
               "GtkOffscreenWindow" "GtkShortcutsWindow")
             (glib-test:list-children "GtkWindow")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkWindow")))
  ;; Check class properties
  (is (equal '("accept-focus" "application" "attached-to" "decorated"
               "default-height" "default-width" "deletable"
               "destroy-with-parent" "focus-on-map" "focus-visible" "gravity"
               "has-resize-grip" "has-toplevel-focus"
               "hide-titlebar-when-maximized" "icon" "icon-name" "is-active"
               "is-maximized" "mnemonics-visible" "modal" "resizable"
               "resize-grip-visible" "role" "screen" "skip-pager-hint"
               "skip-taskbar-hint" "startup-id" "title" "transient-for"
               "type" "type-hint" "urgency-hint" "window-position")
             (glib-test:list-properties "GtkWindow")))
  ;; Check style properties
  (is (equal '("decoration-button-layout" "decoration-resize-handle")
             (gtk-test:list-style-properties "GtkWindow")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkWindow")))
  ;; Check signals
  (is (equal '("activate-default" "activate-focus" "enable-debugging"
               "keys-changed" "set-focus")
             (glib-test:list-signals "GtkWindow")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindow" GTK:WINDOW
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_window_get_type")
                      ((ACCEPT-FOCUS WINDOW-ACCEPT-FOCUS
                        "accept-focus" "gboolean" T T)
                       (APPLICATION WINDOW-APPLICATION
                        "application" "GtkApplication" T T)
                       (ATTACHED-TO WINDOW-ATTACHED-TO
                        "attached-to" "GtkWidget" T T)
                       (DECORATED WINDOW-DECORATED "decorated" "gboolean" T T)
                       (DEFAULT-HEIGHT WINDOW-DEFAULT-HEIGHT
                        "default-height" "gint" T T)
                       (DEFAULT-WIDTH WINDOW-DEFAULT-WIDTH
                        "default-width" "gint" T T)
                       (DELETABLE WINDOW-DELETABLE "deletable" "gboolean" T T)
                       (DESTROY-WITH-PARENT WINDOW-DESTROY-WITH-PARENT
                        "destroy-with-parent" "gboolean" T T)
                       (FOCUS-ON-MAP WINDOW-FOCUS-ON-MAP
                        "focus-on-map" "gboolean" T T)
                       (FOCUS-VISIBLE WINDOW-FOCUS-VISIBLE
                        "focus-visible" "gboolean" T T)
                       (GRAVITY WINDOW-GRAVITY "gravity" "GdkGravity" T T)
                       (HAS-RESIZE-GRIP WINDOW-HAS-RESIZE-GRIP
                        "has-resize-grip" "gboolean" T T)
                       (HAS-TOPLEVEL-FOCUS WINDOW-HAS-TOPLEVEL-FOCUS
                        "has-toplevel-focus" "gboolean" T NIL)
                       (HIDE-TITLEBAR-WHEN-MAXIMIZED
                        WINDOW-HIDE-TITLEBAR-WHEN-MAXIMIZED
                        "hide-titlebar-when-maximized" "gboolean" T T)
                       (ICON WINDOW-ICON "icon" "GdkPixbuf" T T)
                       (ICON-NAME WINDOW-ICON-NAME
                        "icon-name" "gchararray" T T)
                       (IS-ACTIVE WINDOW-IS-ACTIVE
                        "is-active" "gboolean" T NIL)
                       (IS-MAXIMIZED WINDOW-IS-MAXIMIZED
                        "is-maximized" "gboolean" T NIL)
                       (MNEMONICS-VISIBLE WINDOW-MNEMONICS-VISIBLE
                        "mnemonics-visible" "gboolean" T T)
                       (MODAL WINDOW-MODAL "modal" "gboolean" T T)
                       (RESIZABLE WINDOW-RESIZABLE "resizable" "gboolean" T T)
                       (RESIZE-GRIP-VISIBLE WINDOW-RESIZE-GRIP-VISIBLE
                        "resize-grip-visible" "gboolean" T NIL)
                       (ROLE WINDOW-ROLE "role" "gchararray" T T)
                       (SCREEN WINDOW-SCREEN "screen" "GdkScreen" T T)
                       (SKIP-PAGER-HINT WINDOW-SKIP-PAGER-HINT
                        "skip-pager-hint" "gboolean" T T)
                       (SKIP-TASKBAR-HINT WINDOW-SKIP-TASKBAR-HINT
                        "skip-taskbar-hint" "gboolean" T T)
                       (STARTUP-ID WINDOW-STARTUP-ID
                        "startup-id" "gchararray" NIL T)
                       (TITLE WINDOW-TITLE "title" "gchararray" T T)
                       (TRANSIENT-FOR WINDOW-TRANSIENT-FOR
                        "transient-for" "GtkWindow" T T)
                       (TYPE WINDOW-TYPE "type" "GtkWindowType" T NIL)
                       (TYPE-HINT WINDOW-TYPE-HINT
                        "type-hint" "GdkWindowTypeHint" T T)
                       (URGENCY-HINT WINDOW-URGENCY-HINT
                        "urgency-hint" "gboolean" T T)
                       (WINDOW-POSITION WINDOW-WINDOW-POSITION
                        "window-position" "GtkWindowPosition" T T)))
             (gobject:get-gtype-definition "GtkWindow"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-window-activate-default-signal
  (let* ((name "activate-default")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gtk-window-activate-focus-signal
  (let* ((name "activate-focus")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gtk-window-enable-debugging-signal
  (let* ((name "enable-debugging")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gtk-window-keys-changed-signal
  (let* ((name "keys-changed")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

(test gtk-window-set-fcous-signal
  (let* ((name "set-focus")
         (gtype (g:gtype "GtkWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-window-properties
  (glib-test:with-check-memory (window window1)
    (setf window (make-instance 'gtk:window))
    ;; Property ACCEPT-FOCUS
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "accept-focus")))
    (is-true  (gtk:window-accept-focus window))
    (is-false (setf (gtk:window-accept-focus window) nil))
    ;; Property APPLICATION
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "application")))
    (is-false (gtk:window-application window))
;   Only settable after "STARTUP" signal handler has been emitted
    ;; Property ATTACHED-TO
    (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "attached-to")))
    (is-false (gtk:window-attached-to window))
    (is (typep (setf (gtk:window-attached-to window) (gtk:menu-new)) 'gtk:menu))
    ;; Property DECORATED
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "decorated")))
    (is-true  (gtk:window-decorated window))
    (is-false (setf (gtk:window-decorated window) nil))
    ;; Property DEFAULT-HEIGHT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "default-height")))
    (is (= -1 (gtk:window-default-height window)))
    (is (= 10 (setf (gtk:window-default-height window) 10)))
    ;; Property DEFAULT-WIDTH
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "default-width")))
    (is (= -1 (gtk:window-default-width window)))
    (is (= 10 (setf (gtk:window-default-width window) 10)))
    ;; Property DELETABLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "deletable")))
    (is-true  (gtk:window-deletable window))
    (is-false (setf (gtk:window-deletable window) nil))
    ;; Property DESTROY-WITH-PARENT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "destroy-with-parent")))
    (is-false (gtk:window-destroy-with-parent window))
    (is-true (setf (gtk:window-destroy-with-parent window) t))
    ;; Property FOCUS-ON-MAP
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "focus-on-map")))
    (is-true  (gtk:window-focus-on-map window))
    (is-false (setf (gtk:window-focus-on-map window) nil))
    ;; Property FOCUS-VISIBLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "focus-visible")))
    (is-true  (gtk:window-focus-visible window))
    (is-false (setf (gtk:window-focus-visible window) nil))
    ;; Property GRAVITY
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "gravity")))
    (is (eq :north-west (gtk:window-gravity window)))
    (is (eq :north (setf (gtk:window-gravity window) :north)))
    ;; Property HAS-RESIZE-GRIP
    (is (equal '(:DEPRECATED :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "has-resize-grip")))
    (is-false (gtk:window-has-resize-grip window))
    (is-true (setf (gtk:window-has-resize-grip window) t))
    ;; Property HAS-TOPLEVEL-FOCUS
    (is (equal '(:READABLE)
               (glib-test:list-param-flags "GtkWindow" "has-toplevel-focus")))
    (is-false (gtk:window-has-toplevel-focus window))
    (signals (error) (setf (gtk:window-has-toplevel-focus window) t))
    ;; Property HAS-TITLEBAR-WHEN-MAXIMIZED
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "hide-titlebar-when-maximized")))
    (is-false (gtk:window-hide-titlebar-when-maximized window))
    (is-true (setf (gtk:window-hide-titlebar-when-maximized window) t))
    ;; Property ICON
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "icon")))
    (is-false (gtk:window-icon window))
    (is (typep (setf (gtk:window-icon window)
                     (gdk:pixbuf-new-from-file
                         (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
               'gdk:pixbuf))
    ;; Property ICON-NAME
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "icon-name")))
    (is-false (gtk:window-icon-name window))
    (is (string= "edit-find" (setf (gtk:window-icon-name window) "edit-find")))
    ;; Property IS-ACTIVE
    (is (equal '(:READABLE)
               (glib-test:list-param-flags "GtkWindow" "is-active")))
    (is-false (gtk:window-is-active window))
    (signals (error) (setf (gtk:window-is-active window) t))
    ;; Property IS-MAXIMIZED
    (is (equal '(:READABLE)
               (glib-test:list-param-flags "GtkWindow" "is-maximized")))
    (is-false (gtk:window-is-maximized window))
    (signals (error) (setf (gtk:window-is-maximized window) t))
    ;; Property MNEMONICS-VISIBLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "mnemonics-visible")))
    (is-true (gtk:window-mnemonics-visible window))
    (is-false (setf (gtk:window-mnemonics-visible window) nil))
    ;; Property MODAL
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "modal")))
    (is-false (gtk:window-modal window))
    (is-true (setf (gtk:window-modal window) t))
    ;; Property OPACITY
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "opacity")))
    (is (= 1.0d0 (gtk:window-opacity window)))
    (is (= 0.5d0 (setf (gtk:window-opacity window) 1/2)))
    ;; Property RESIZABLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "resizable")))
    (is-true  (gtk:window-resizable window))
    (is-false (setf (gtk:window-resizable window) nil))
    ;; Property RESIZE-GRIP-VISIBLE
    (is (equal '(:DEPRECATED :READABLE)
               (glib-test:list-param-flags "GtkWindow" "resize-grip-visible")))
    (is-false (gtk:window-resize-grip-visible window))
    (signals (error) (setf (gtk:window-resize-grip-visible window) t))
    ;; Property ROLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "role")))
    (is-false (gtk:window-role window))
    (is (string= "role" (setf (gtk:window-role window) "role")))
    ;; Property SCREEN
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "screen")))
    (is (typep (gtk:window-screen window) 'gdk:screen))
    (is (typep (setf (gtk:window-screen window)
                     (gdk:screen-default)) 'gdk:screen))
    ;; Property SKIP-PAGER-HINT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "skip-pager-hint")))
    (is-false (gtk:window-skip-pager-hint window))
    (is-true (setf (gtk:window-skip-pager-hint window) t))
    ;; Property STARTUP-ID
    (is (equal '(:WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "startup-id")))
    (signals (error) (gtk:window-startup-id window))
    (is (string= "ID" (setf (gtk:window-startup-id window) "ID")))
    ;; Property TITLE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "title")))
    (is-false (gtk:window-title window))
    (is (string= "title" (setf (gtk:window-title window) "title")))
    ;; Property TRANSIENT-FOR
    (is (equal '(:CONSTRUCT :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "transient-for")))
    (is-false (gtk:window-transient-for window))
    (is (typep (setf (gtk:window-transient-for window)
                     (setf window1 (make-instance 'gtk:window))) 'gtk:window))
    ;; Property TYPE
    (is (equal '(:CONSTRUCT-ONLY :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "type")))
    (is (eq :toplevel (gtk:window-type window)))
    (signals (error) (setf (gtk:window-type window) :popup))
    ;; Property TYPE-HINT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "type-hint")))
    (is (eq :normal (gtk:window-type-hint window)))
    (is (eq :dialog (setf (gtk:window-type-hint window) :dialog)))
    ;; Property URGENCY-HINT
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "urgency-hint")))
    (is-false (gtk:window-urgency-hint window))
    (is-true (setf (gtk:window-urgency-hint window) t))
    ;; Property WINDOW-POSITION
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkWindow" "window-position")))
    (is (eq :none (gtk:window-window-position window)))
    (is (eq :center (setf (gtk:window-window-position window) :center)))
    ;; Destroy windows
    (is-false (gtk:widget-destroy window1))
    (is-false (gtk:widget-destroy window))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-window-style-properties
  (glib-test:with-check-memory (win)
    (setf win (make-instance 'gtk:window))
    (is (string= "menu:close"
                 (gtk:widget-style-property win "decoration-button-layout")))
    (is (= 20 (gtk:widget-style-property win "decoration-resize-handle")))
    ;; Destroy winndow
    (is-false (gtk:widget-destroy win))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_new

(test gtk-window-new
  (glib-test:with-check-memory (window)
    (is (typep (setf window (gtk:window-new :toplevel)) 'gtk:window))
    (is-false (gtk:widget-destroy window))
    (is (typep (setf window (gtk:window-new :popup)) 'gtk:window))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_close

(test gtk-window-close
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-close window))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_present

(test gtk-window-present
  (glib-test:with-check-memory (window)
    (setf window (make-instance 'gtk:window))
    (is-false (gtk:window-present window))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_present_with_time

(test gtk-window-present-with-time
  (glib-test:with-check-memory (window)
    (setf window (make-instance 'gtk:window))
    (is-false (gtk:window-present-with-time window gdk:+current-time+))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_list_toplevels

(test gtk-window-list-toplevels
  (glib-test:with-check-memory (window :strong 1)
    (setf window (gtk:window-new :toplevel))
    (is (member window (gtk:window-list-toplevels) :test #'eq))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_add_accel_group
;;;     gtk_window_remove_accel_group

(test gtk-window-add-accel-group
  (glib-test:with-check-memory (window group)
    (setf window (gtk:window-new :toplevel))
    (setf group (gtk:accel-group-new))
    (is-false (gtk:window-add-accel-group window group))
    (is-false (gtk:window-remove-accel-group window group))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_default_size

(test gtk-window-default-size
  (glib-test:with-check-memory (window)
    (setf window (make-instance 'gtk:window))
    (is (equal '(-1 -1)
               (multiple-value-list (gtk:window-default-size window))))
    (is (equal '(100 200)
               (multiple-value-list (setf (gtk:window-default-size window)
                                          '(100 200)))))
    (is (equal '(100 200)
               (multiple-value-list (gtk:window-default-size window))))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_get_size
;;;     gtk_window_get_position

(test gtk-window-size/position
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    #-windows
    (is (equal '(148 200) (multiple-value-list (gtk:window-size window))))
    #+windows
    (is (equal '(200 200) (multiple-value-list (gtk:window-size window))))
    #-windows
    (is (equal '(26 23) (multiple-value-list (gtk:window-position window))))
    #+windows
    (is (equal '(0 0) (multiple-value-list (gtk:window-position window))))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_set_default_geometry                     not implemented

;;;     gtk_window_set_geometry_hints

(test gtk-window-set-geometry-hints
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (let ((mask '(:win-gravity)))
      (cffi:with-foreign-object (geometry '(:struct gdk:geometry))
        (cffi:with-foreign-slots ((gdk::win-gravity) geometry (:struct gdk:geometry))
          ;; Set the gravity value
          (setf gdk::win-gravity :north)
          ;; The default value
          (is (eq :north-west (gtk:window-gravity window)))
          ;; Set the geometry
          (is-false (gtk:window-set-geometry-hints window geometry mask))
          ;; The new value
          (is (eq :north (gtk:window-gravity window)))
          (is-false (gtk:widget-destroy window)))))))

;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_activate_focus

(test gtk-window-focus
  (glib-test:with-check-memory (window button)
    (setf window (gtk:window-new :toplevel))
    (setf button (gtk:button-new))
    (gtk:container-add window button)
    (is-false (gtk:window-focus window))
    (is (eq button (setf (gtk:window-focus window) button)))
    (is (eq button (gtk:window-focus window)))
    ;; Does not return a TRUE value
    (is-false (gtk:window-activate-focus window))
    ;; Remove button and destroy window
    (is-false (gtk:container-remove window button))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_get_default_widget
;;;     gtk_window_set_default
;;;     gtk_window_activate_default

(test gtk-window-default-widget
  (glib-test:with-check-memory (window button)
    (setf window (gtk:window-new :toplevel))
    (setf button (gtk:button-new))
    (setf (gtk:widget-can-default button) t)
    (gtk:container-add window button)
    (is-false (gtk:window-default-widget window))
    (is (eq button (setf (gtk:window-default-widget window) button)))
    (is (eq button (gtk:window-default-widget window)))
    ;; Remove button and destroy window
    (is-false (gtk:container-remove window button))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_iconify
;;;     gtk_window_deiconify

(test gtk-window-iconify
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-iconify window))
    (is-false (gtk:window-deiconify window))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_stick
;;;     gtk_window_unstick

(test gtk-window-stick
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-stick window))
    (is-false (gtk:window-unstick window))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_maximize
;;;     gtk_window_unmaximize

(test gtk-window-maximize
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-is-maximized window))
    (is-false (gtk:window-maximize window))
    ;; Does not maximize
    (is-false (gtk:window-is-maximized window))
    (is-false (gtk:window-unmaximize window))
    (is-false (gtk:window-is-maximized window))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor
;;;     gtk_window_unfullscreen

(test gtk-window-fullscreen
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-fullscreen window))
    (is-false (gtk:window-fullscreen-on-monitor window (gdk:screen-default) 0))
    (is-false (gtk:window-unfullscreen window))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below

(test gtk-window-set-keep-above/below
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-set-keep-above window t))
    (is-false (gtk:window-set-keep-below window t))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_get_group
;;;     gtk_window_has_group

;; TODO: Can we remove the reference for GROUP?

(test gtk-window-has-group
  (glib-test:with-check-memory (window group :strong 1) ; for default group
    (setf window (gtk:window-new :toplevel))
    (setf group (gtk:window-group-new))
    (is (= 2 (g:object-ref-count window)))
    (is (= 1 (g:object-ref-count group)))
    ;; Not in a group
    (is-false (gtk:window-has-group window))
    ;; But in the default group
    (is (typep (gtk:window-group window) 'gtk:window-group))
;   (is (member window
;               (gtk:window-group-list-windows (gtk:window-group window))
;               :test #'eq))
    ;; Add window to a group
    (is-false (gtk:window-group-add-window group window))
    (is (= 2 (g:object-ref-count window)))
    (is-true (gtk:window-has-group window))
    (is (eq group (gtk:window-group window)))
    (is (member window (gtk:window-group-list-windows group) :test #'eq))
    (is-false (gtk:window-group-remove-window group window))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_move
;;;     gtk_window_resize

(test gtk-window-move/resize
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-resize window 100 150))
    (is (equal '(100 150) (multiple-value-list (gtk:window-size window))))
    (is-false (gtk:window-move window 10 20))
    (is (equal '(10 20) (multiple-value-list (gtk:window-position window))))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_resize_to_geometry                       not implemented
;;;     gtk_window_parse_geometry                           not implemented
;;;     gtk_window_reshow_with_initial_size                 not implemented

;;;     gtk_window_add_mnemonic
;;;     gtk_window_remove_mnemonic
;;;     gtk_window_mnemonic_activate
;;;     gtk_window_activate_key
;;;     gtk_window_propagate_key_event

(test gtk-window-add-mnemonic
  (glib-test:with-check-memory (window button)
    (setf window (gtk:window-new :toplevel))
    (setf button (gtk:button-new))
    (is-false (gtk:container-add window button))
    (is-false (gtk:window-add-mnemonic window 97 button))
    (is-false (gtk:window-mnemonic-activate window 97 :shift-mask))
    (is-false (gtk:window-remove-mnemonic window 97 button))
    (let ((event (gdk:event-new :key-release :keyval 97 :state :shift-mask)))
      (is-false (gtk:window-activate-key window event))
      (is-false (gtk:window-propagate-key-event window event)))
    ;; Remove button and destroy window
    (is-false (gtk:container-remove window button))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_set_mnemonic_modifier

(test gtk-window-mnemonic-modifier
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is (equal '(:MOD1-MASK) (gtk:window-mnemonic-modifier window)))
    (is (eq :shift-mask (setf (gtk:window-mnemonic-modifier window) :shift-mask)))
    (is (equal '(:shift-mask) (gtk:window-mnemonic-modifier window)))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_get_default_icon_name
;;;     gtk_window_set_default_icon_name

(test gtk-window-default-icon-name
  (glib-test:with-check-memory ()
    (is-false (gtk:window-default-icon-name))
    (is (string= "applications-utilities"
                 (setf (gtk:window-default-icon-name) "applications-utilities")))
    (is (string= "applications-utilities"
                 (gtk:window-default-icon-name)))
    (is-false (setf (gtk:window-default-icon-name) nil))))

;;;     gtk_window_get_default_icon_list
;;;     gtk_window_set_default_icon_list

(test gtk-window-default-icon-list
  (glib-test:with-check-memory (icon1 icon2)
    (setf icon1 (gdk:pixbuf-new-from-file
                    (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
    (setf icon2 (gdk:pixbuf-new-from-file
                    (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
    (is-false (gtk:window-default-icon-list))
    (is (every (lambda (x) (typep x 'gdk-pixbuf:pixbuf))
               (setf (gtk:window-default-icon-list) (list icon1 icon2))))
    (is (every (lambda (x) (typep x 'gdk-pixbuf:pixbuf))
               (gtk:window-default-icon-list)))
    (is-false (setf (gtk:window-default-icon-list) nil))))

;;;     gtk_window_get_icon_list
;;;     gtk_window_set_icon_list

(test gtk-window-icon-list
  (glib-test:with-check-memory (window icon1 icon2)
    (setf window (gtk:window-new :toplevel))
    (setf icon1 (gdk:pixbuf-new-from-file
                    (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
    (setf icon2 (gdk:pixbuf-new-from-file
                    (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
    (is-false (gtk:window-icon-list window))
    (is (every (lambda (x) (typep x 'gdk-pixbuf:pixbuf))
               (setf (gtk:window-icon-list window) (list icon1 icon2))))
    (is (every (lambda (x) (typep x 'gdk-pixbuf:pixbuf))
               (gtk:window-icon-list window)))
    (is-false (setf (gtk:window-icon-list window) nil))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_set_default_icon

(test gtk-window-set-default-icon
  (glib-test:with-check-memory (icon)
    (setf icon (gdk:pixbuf-new-from-file
                   (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
    (is-false (gtk:window-set-default-icon icon))
    (is (eq icon (first (gtk:window-default-icon-list))))
    (is-false (setf (gtk:window-default-icon-list) nil))))

;;;     gtk_window_set_icon_from_file

(test gtk-window-set-icon-from-file
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-true (gtk:window-set-icon-from-file window
                 (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
    (is (typep (gtk:window-icon window) 'gdk:pixbuf))
    (is-false (setf (gtk:window-icon window) nil))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_set_default_icon_from_file

(test gtk-window-set-default-icon-from-file
  (is-true (gtk:window-set-default-icon-from-file
               (glib-sys:sys-path "test/resource/gtk-logo-24.png")))
  (is (typep (first (gtk:window-default-icon-list)) 'gdk-pixbuf:pixbuf))
  (is-false (setf (gtk:window-default-icon-list) nil)))

;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag

;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity

(test gtk-window-opacity
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is (= 1.0d0 (gtk:window-opacity window)))
    (is (= 0.5d0 (setf (gtk:window-opacity window) 1/2)))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_set_titlebar
;;;     gtk_window_get_titlebar

(test gtk-window-titlebar
  (glib-test:with-check-memory (window)
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-titlebar window))
    (is (typep (setf (gtk:window-titlebar window) (gtk:box-new :horizontal)) 'gtk:box))
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_set_wmclass
;;;     gtk_window_resize_grip_is_visible
;;;     gtk_window_get_resize_grip_area
;;;     gtk_window_set_has_user_ref_count

;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_set_interactive_debugging

;;; 2026-06-10
