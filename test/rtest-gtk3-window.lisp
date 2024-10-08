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
                        :TYPE-INITIALIZER "gtk_window_position_get_type")
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

;;; --- Properties -------------------------------------------------------------

(test gtk-window-properties
  (let ((window (make-instance 'gtk:window)))
    (is-true  (gtk:window-accept-focus window))
    (is-false (gtk:window-application window))
    (is-false (gtk:window-attached-to window))
    (is-true  (gtk:window-decorated window))
    (is (= -1 (gtk:window-default-height window)))
    (is (= -1 (gtk:window-default-width window)))
    (is-true  (gtk:window-deletable window))
    (is-false (gtk:window-destroy-with-parent window))
    (is-true  (gtk:window-focus-on-map window))
    (is-true  (gtk:window-focus-visible window))
    (is (eq :north-west (gtk:window-gravity window)))
    (is-false (gtk:window-has-resize-grip window))
    (is-false (gtk:window-has-toplevel-focus window))
    (is-false (gtk:window-hide-titlebar-when-maximized window))
    (is-false (gtk:window-icon window))
    (is-false (gtk:window-icon-name window))
    (is-false (gtk:window-is-active window))
    (is-false (gtk:window-is-maximized window))
    (is-true  (gtk:window-mnemonics-visible window))
    (is-false (gtk:window-modal window))
    (is-true  (gtk:window-resizable window))
    (is-false (gtk:window-resize-grip-visible window))
    (is-false (gtk:window-role window))
    (is (typep (gtk:window-screen window) 'gdk:screen))
    (is-false (gtk:window-skip-pager-hint window))
    (is-false (gtk:window-skip-taskbar-hint window))
    ;; startup-id is not readable
    (signals (error) (gtk:window-startup-id window))
    (is-false (gtk:window-title window))
    (is-false (gtk:window-transient-for window))
    (is (eq :toplevel (gtk:window-type window)))
    ;; type cannot be set after construction, this test gives  warning
;   (is (eq :popup (setf (gtk:window-type window) :popup)))
    (is (eq :normal (gtk:window-type-hint window)))
    (is-false (gtk:window-urgency-hint window))
    (is (eq :none (gtk:window-window-position window)))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-window-style-properties
  (let ((win (make-instance 'gtk:window)))
    (is (string= "menu:close"
                 (gtk:widget-style-property win "decoration-button-layout")))
    (is (= 20 (gtk:widget-style-property win "decoration-resize-handle")))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-window-activate-default-signal
  (let ((query (g:signal-query (g:signal-lookup "activate-default"
                                                "GtkWindow"))))
    (is (string= "activate-default" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-window-activate-focus-signal
  (let ((query (g:signal-query (g:signal-lookup "activate-focus" "GtkWindow"))))
    (is (string= "activate-focus" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-window-enable-debugging-signal
  (let ((query (g:signal-query (g:signal-lookup "enable-debugging"
                                                "GtkWindow"))))
    (is (string= "enable-debugging" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST :ACTION)
               (g:signal-query-signal-flags query)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gboolean")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-window-keys-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "keys-changed" "GtkWindow"))))
    (is (string= "keys-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

(test gtk-window-set-focus-signal
  (let ((query (g:signal-query (g:signal-lookup "set-focus" "GtkWindow"))))
    (is (string= "set-focus" (g:signal-query-signal-name query)))
    (is (string= "GtkWindow" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (g:signal-query-signal-flags query)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkWidget")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_new

(test gtk-window-new
  (is (typep (gtk:window-new :toplevel) 'gtk:window))
  (is (typep (gtk:window-new :popup) 'gtk:window)))

;;;     gtk_window_set_wmclass

;;;     gtk_window_add_accel_group
;;;     gtk_window_remove_accel_group

(test gtk-window-add-accel-group
  (let ((window (gtk:window-new :toplevel))
        (group (gtk:accel-group-new)))
    (is-false (gtk:window-add-accel-group window group))
    (is-false (gtk:window-remove-accel-group window group))))

;;;     gtk_window_activate_focus
;;;     gtk_window_activate_default

;;;     gtk_window_default_size

(test gtk-window-default-size
  (let ((window (make-instance 'gtk:window)))
    (is (equal '(-1 -1)
               (multiple-value-list (gtk:window-default-size window))))
    (is (equal '(100 200)
               (multiple-value-list (setf (gtk:window-default-size window)
                                          '(100 200)))))
    (is (equal '(100 200)
               (multiple-value-list (gtk:window-default-size window))))))


;;;     gtk_window_set_default_geometry

;; This function is deprecated and does nothing.

;;;     gtk_window_set_geometry_hints

(test gtk-window-set-geometry-hints
  (let ((window (gtk:window-new :toplevel))
        (mask '(:win-gravity)))
    (cffi:with-foreign-object (geometry '(:struct gdk:geometry))
      (cffi:with-foreign-slots ((gdk::win-gravity)
                                geometry (:struct gdk:geometry))
        ;; Set the gravity value
        (setf gdk::win-gravity :north)
        ;; The default value
        (is (eq :north-west (gtk:window-gravity window)))
        ;; Set the geometry
        (is-false (gtk:window-set-geometry-hints window geometry mask))
        ;; The new value
        (is (eq :north (gtk:window-gravity window)))))))

;;;     gtk_window_set_position

;;;     gtk_window_list_toplevels

#+nil
(test gtk-window-list-toplevels
  (is (every (lambda (obj) (typep obj 'gtk:window))
             (gtk:window-list-toplevels))))

;;;     gtk_window_add_mnemonic
;;;     gtk_window_remove_mnemonic
;;;     gtk_window_mnemonic_activate
;;;     gtk_window_activate_key
;;;     gtk_window_propagate_key_event
;;;     gtk_window_get_focus
;;;     gtk_window_set_focus
;;;     gtk_window_get_default_widget
;;;     gtk_window_set_default
;;;     gtk_window_present
;;;     gtk_window_present_with_time
;;;     gtk_window_close
;;;     gtk_window_iconify
;;;     gtk_window_deiconify
;;;     gtk_window_stick
;;;     gtk_window_unstick
;;;     gtk_window_maximize
;;;     gtk_window_unmaximize
;;;     gtk_window_fullscreen
;;;     gtk_window_fullscreen_on_monitor ()
;;;     gtk_window_unfullscreen
;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below
;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag
;;;     gtk_window_set_mnemonic_modifier
;;;     gtk_window_get_default_icon_list
;;;     gtk_window_get_default_icon_name

;;;     gtk_window_get_icon_list
;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_get_position
;;;     gtk_window_get_size
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_get_window_type
;;;     gtk_window_move
;;;     gtk_window_parse_geometry
;;;     gtk_window_reshow_with_initial_size
;;;     gtk_window_resize
;;;     gtk_window_resize_to_geometry
;;;     gtk_window_set_default_icon_list
;;;     gtk_window_set_default_icon
;;;     gtk_window_set_default_icon_from_file
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_set_icon_list
;;;     gtk_window_set_icon_from_file
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity
;;;     gtk_window_resize_grip_is_visible
;;;     gtk_window_get_resize_grip_area
;;;     gtk_window_set_has_user_ref_count
;;;     gtk_window_set_titlebar
;;;     gtk_window_get_titlebar
;;;     gtk_window_set_interactive_debugging

;;; 2024-9-21
