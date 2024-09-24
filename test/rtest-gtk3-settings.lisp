(in-package :gtk-test)

(def-suite gtk-settings :in gtk-suite)
(in-suite gtk-settings)

;;; --- Types and Values ------------------------------------------------------

;;;     GtkSettingsValue                                   deprecated

;;;     GtkIMPreeditStyle

(test gtk-im-preedit-style
  ;; Check type
  (is (g:type-is-enum "GtkIMPreeditStyle"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIMPreeditStyle")
          (g:gtype (cffi:foreign-funcall "gtk_im_preedit_style_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:im-preedit-style
          (glib:symbol-for-gtype "GtkIMPreeditStyle")))
  ;; Check names
  (is (equal '("GTK_IM_PREEDIT_NOTHING" "GTK_IM_PREEDIT_CALLBACK"
               "GTK_IM_PREEDIT_NONE")
             (glib-test:list-enum-item-names "GtkIMPreeditStyle")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkIMPreeditStyle")))
  ;; Check nick names
  (is (equal '("nothing" "callback" "none")
             (glib-test:list-enum-item-nicks "GtkIMPreeditStyle")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkIMPreeditStyle" GTK:IM-PREEDIT-STYLE
                       (:EXPORT T)
                       (:NOTHING 0)
                       (:CALLBACK 1)
                       (:NONE 2))
             (gobject:get-gtype-definition "GtkIMPreeditStyle"))))

;;;     GtkIMStatusStyle

(test gtk-im-status-style
  ;; Check type
  (is (g:type-is-enum "GtkIMStatusStyle"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkIMStatusStyle")
          (g:gtype (cffi:foreign-funcall "gtk_im_status_style_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:im-status-style
          (glib:symbol-for-gtype "GtkIMStatusStyle")))
  ;; Check names
  (is (equal '("GTK_IM_STATUS_NOTHING" "GTK_IM_STATUS_CALLBACK"
               "GTK_IM_STATUS_NONE")
             (glib-test:list-enum-item-names "GtkIMStatusStyle")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkIMStatusStyle")))
  ;; Check nick names
  (is (equal '("nothing" "callback" "none")
             (glib-test:list-enum-item-nicks "GtkIMStatusStyle")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkIMStatusStyle" GTK:IM-STATUS-STYLE
                       (:EXPORT T)
                       (:NOTHING 0)
                       (:CALLBACK 1)
                       (:NONE 2))
             (gobject:get-gtype-definition "GtkIMStatusStyle"))))

;;;     GtkSettings

(test gtk-settings-class
  ;; Check type
  (is (g:type-is-object "GtkSettings"))
  ;; Check registered name
  (is (eq 'gtk:settings
          (glib:symbol-for-gtype "GtkSettings")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSettings")
          (g:gtype (cffi:foreign-funcall "gtk_settings_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkSettings")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSettings")))
  ;; Check interfaces
  (is (equal '("GtkStyleProvider" "GtkStyleProviderPrivate")
             (glib-test:list-interfaces "GtkSettings")))
  ;; Check class properties
  (is (equal '("color-hash" "gtk-alternative-button-order"
               "gtk-alternative-sort-arrows" "gtk-application-prefer-dark-theme"
               "gtk-auto-mnemonics" "gtk-button-images" "gtk-can-change-accels"
               "gtk-color-palette" "gtk-color-scheme" "gtk-cursor-aspect-ratio"
               "gtk-cursor-blink" "gtk-cursor-blink-time"
               "gtk-cursor-blink-timeout" "gtk-cursor-theme-name"
               "gtk-cursor-theme-size" "gtk-decoration-layout"
               "gtk-dialogs-use-header" "gtk-dnd-drag-threshold"
               "gtk-double-click-distance" "gtk-double-click-time"
               "gtk-enable-accels" "gtk-enable-animations"
               "gtk-enable-event-sounds" "gtk-enable-input-feedback-sounds"
               "gtk-enable-mnemonics" "gtk-enable-primary-paste"
               "gtk-enable-tooltips" "gtk-entry-password-hint-timeout"
               "gtk-entry-select-on-focus" "gtk-error-bell"
               "gtk-fallback-icon-theme" "gtk-file-chooser-backend"
               "gtk-font-name" "gtk-fontconfig-timestamp" "gtk-icon-sizes"
               "gtk-icon-theme-name" "gtk-im-module" "gtk-im-preedit-style"
               "gtk-im-status-style" "gtk-key-theme-name"
               "gtk-keynav-cursor-only" "gtk-keynav-use-caret"
               "gtk-keynav-wrap-around" "gtk-label-select-on-focus"
               "gtk-long-press-time" "gtk-menu-bar-accel"
               "gtk-menu-bar-popup-delay" "gtk-menu-images"
               "gtk-menu-popdown-delay" "gtk-menu-popup-delay" "gtk-modules"
               "gtk-overlay-scrolling" "gtk-primary-button-warps-slider"
               "gtk-print-backends" "gtk-print-preview-command"
               "gtk-recent-files-enabled" "gtk-recent-files-limit"
               "gtk-recent-files-max-age" "gtk-scrolled-window-placement"
               "gtk-shell-shows-app-menu" "gtk-shell-shows-desktop"
               "gtk-shell-shows-menubar" "gtk-show-input-method-menu"
               "gtk-show-unicode-menu" "gtk-sound-theme-name" "gtk-split-cursor"
               "gtk-theme-name" "gtk-timeout-expand" "gtk-timeout-initial"
               "gtk-timeout-repeat" "gtk-titlebar-double-click"
               "gtk-titlebar-middle-click" "gtk-titlebar-right-click"
               "gtk-toolbar-icon-size" "gtk-toolbar-style"
               "gtk-tooltip-browse-mode-timeout" "gtk-tooltip-browse-timeout"
               "gtk-tooltip-timeout" "gtk-touchscreen-mode" "gtk-visible-focus"
               "gtk-xft-antialias" "gtk-xft-dpi" "gtk-xft-hinting"
               "gtk-xft-hintstyle" "gtk-xft-rgba")
             (glib-test:list-properties "GtkSettings")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSettings" GTK:SETTINGS
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES
                        ("GtkStyleProvider" "GtkStyleProviderPrivate")
                        :TYPE-INITIALIZER "gtk_settings_get_type")
                       ((COLOR-HASH SETTINGS-COLOR-HASH
                         "color-hash" "GHashTable" T NIL)
                        (GTK-ALTERNATIVE-BUTTON-ORDER
                         SETTINGS-GTK-ALTERNATIVE-BUTTON-ORDER
                         "gtk-alternative-button-order" "gboolean" T T)
                        (GTK-ALTERNATIVE-SORT-ARROWS
                         SETTINGS-GTK-ALTERNATIVE-SORT-ARROWS
                         "gtk-alternative-sort-arrows" "gboolean" T T)
                        (GTK-APPLICATION-PREFER-DARK-THEME
                         SETTINGS-GTK-APPLICATION-PREFER-DARK-THEME
                         "gtk-application-prefer-dark-theme" "gboolean" T T)
                        (GTK-AUTO-MNEMONICS SETTINGS-GTK-AUTO-MNEMONICS
                         "gtk-auto-mnemonics" "gboolean" T T)
                        (GTK-BUTTON-IMAGES SETTINGS-GTK-BUTTON-IMAGES
                         "gtk-button-images" "gboolean" T T)
                        (GTK-CAN-CHANGE-ACCELS SETTINGS-GTK-CAN-CHANGE-ACCELS
                         "gtk-can-change-accels" "gboolean" T T)
                        (GTK-COLOR-PALETTE SETTINGS-GTK-COLOR-PALETTE
                         "gtk-color-palette" "gchararray" T T)
                        (GTK-COLOR-SCHEME SETTINGS-GTK-COLOR-SCHEME
                         "gtk-color-scheme" "gchararray" T T)
                        (GTK-CURSOR-ASPECT-RATIO
                         SETTINGS-GTK-CURSOR-ASPECT-RATIO
                         "gtk-cursor-aspect-ratio" "gfloat" T T)
                        (GTK-CURSOR-BLINK SETTINGS-GTK-CURSOR-BLINK
                         "gtk-cursor-blink" "gboolean" T T)
                        (GTK-CURSOR-BLINK-TIME SETTINGS-GTK-CURSOR-BLINK-TIME
                         "gtk-cursor-blink-time" "gint" T T)
                        (GTK-CURSOR-BLINK-TIMEOUT
                         SETTINGS-GTK-CURSOR-BLINK-TIMEOUT
                         "gtk-cursor-blink-timeout" "gint" T T)
                        (GTK-CURSOR-THEME-NAME SETTINGS-GTK-CURSOR-THEME-NAME
                         "gtk-cursor-theme-name" "gchararray" T T)
                        (GTK-CURSOR-THEME-SIZE SETTINGS-GTK-CURSOR-THEME-SIZE
                         "gtk-cursor-theme-size" "gint" T T)
                        (GTK-DECORATION-LAYOUT SETTINGS-GTK-DECORATION-LAYOUT
                         "gtk-decoration-layout" "gchararray" T T)
                        (GTK-DIALOGS-USE-HEADER SETTINGS-GTK-DIALOGS-USE-HEADER
                         "gtk-dialogs-use-header" "gboolean" T T)
                        (GTK-DND-DRAG-THRESHOLD SETTINGS-GTK-DND-DRAG-THRESHOLD
                         "gtk-dnd-drag-threshold" "gint" T T)
                        (GTK-DOUBLE-CLICK-DISTANCE
                         SETTINGS-GTK-DOUBLE-CLICK-DISTANCE
                         "gtk-double-click-distance" "gint" T T)
                        (GTK-DOUBLE-CLICK-TIME SETTINGS-GTK-DOUBLE-CLICK-TIME
                         "gtk-double-click-time" "gint" T T)
                        (GTK-ENABLE-ACCELS SETTINGS-GTK-ENABLE-ACCELS
                         "gtk-enable-accels" "gboolean" T T)
                        (GTK-ENABLE-ANIMATIONS SETTINGS-GTK-ENABLE-ANIMATIONS
                         "gtk-enable-animations" "gboolean" T T)
                        (GTK-ENABLE-EVENT-SOUNDS
                         SETTINGS-GTK-ENABLE-EVENT-SOUNDS
                         "gtk-enable-event-sounds" "gboolean" T T)
                        (GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                         SETTINGS-GTK-ENABLE-INPUT-FEEDBACK-SOUNDS
                         "gtk-enable-input-feedback-sounds" "gboolean" T T)
                        (GTK-ENABLE-MNEMONICS SETTINGS-GTK-ENABLE-MNEMONICS
                         "gtk-enable-mnemonics" "gboolean" T T)
                        (GTK-ENABLE-PRIMARY-PASTE
                         SETTINGS-GTK-ENABLE-PRIMARY-PASTE
                         "gtk-enable-primary-paste" "gboolean" T T)
                        (GTK-ENABLE-TOOLTIPS SETTINGS-GTK-ENABLE-TOOLTIPS
                         "gtk-enable-tooltips" "gboolean" T T)
                        (GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                         SETTINGS-GTK-ENTRY-PASSWORD-HINT-TIMEOUT
                         "gtk-entry-password-hint-timeout" "guint" T T)
                        (GTK-ENTRY-SELECT-ON-FOCUS
                         SETTINGS-GTK-ENTRY-SELECT-ON-FOCUS
                         "gtk-entry-select-on-focus" "gboolean" T T)
                        (GTK-ERROR-BELL SETTINGS-GTK-ERROR-BELL
                         "gtk-error-bell" "gboolean" T T)
                        (GTK-FALLBACK-ICON-THEME
                         SETTINGS-GTK-FALLBACK-ICON-THEME
                         "gtk-fallback-icon-theme" "gchararray" T T)
                        (GTK-FILE-CHOOSER-BACKEND
                         SETTINGS-GTK-FILE-CHOOSER-BACKEND
                         "gtk-file-chooser-backend" "gchararray" T T)
                        (GTK-FONT-NAME SETTINGS-GTK-FONT-NAME
                         "gtk-font-name" "gchararray" T T)
                        (GTK-FONTCONFIG-TIMESTAMP
                         SETTINGS-GTK-FONTCONFIG-TIMESTAMP
                         "gtk-fontconfig-timestamp" "guint" T T)
                        (GTK-ICON-SIZES SETTINGS-GTK-ICON-SIZES
                         "gtk-icon-sizes" "gchararray" T T)
                        (GTK-ICON-THEME-NAME SETTINGS-GTK-ICON-THEME-NAME
                         "gtk-icon-theme-name" "gchararray" T T)
                        (GTK-IM-MODULE SETTINGS-GTK-IM-MODULE
                         "gtk-im-module" "gchararray" T T)
                        (GTK-IM-PREEDIT-STYLE SETTINGS-GTK-IM-PREEDIT-STYLE
                         "gtk-im-preedit-style" "GtkIMPreeditStyle" T T)
                        (GTK-IM-STATUS-STYLE SETTINGS-GTK-IM-STATUS-STYLE
                         "gtk-im-status-style" "GtkIMStatusStyle" T T)
                        (GTK-KEY-THEME-NAME SETTINGS-GTK-KEY-THEME-NAME
                         "gtk-key-theme-name" "gchararray" T T)
                        (GTK-KEYNAV-CURSOR-ONLY
                         SETTINGS-GTK-KEYNAV-CURSOR-ONLY
                         "gtk-keynav-cursor-only" "gboolean" T T)
                        (GTK-KEYNAV-USE-CARET SETTINGS-GTK-KEYNAV-USE-CARET
                         "gtk-keynav-use-caret" "gboolean" T T)
                        (GTK-KEYNAV-WRAP-AROUND
                         SETTINGS-GTK-KEYNAV-WRAP-AROUND
                         "gtk-keynav-wrap-around" "gboolean" T T)
                        (GTK-LABEL-SELECT-ON-FOCUS
                         SETTINGS-GTK-LABEL-SELECT-ON-FOCUS
                         "gtk-label-select-on-focus" "gboolean" T T)
                        (GTK-LONG-PRESS-TIME SETTINGS-GTK-LONG-PRESS-TIME
                         "gtk-long-press-time" "guint" T T)
                        (GTK-MENU-BAR-ACCEL SETTINGS-GTK-MENU-BAR-ACCEL
                         "gtk-menu-bar-accel" "gchararray" T T)
                        (GTK-MENU-BAR-POPUP-DELAY
                         SETTINGS-GTK-MENU-BAR-POPUP-DELAY
                         "gtk-menu-bar-popup-delay" "gint" T T)
                        (GTK-MENU-IMAGES SETTINGS-GTK-MENU-IMAGES
                         "gtk-menu-images" "gboolean" T T)
                        (GTK-MENU-POPDOWN-DELAY SETTINGS-GTK-MENU-POPDOWN-DELAY
                         "gtk-menu-popdown-delay" "gint" T T)
                        (GTK-MENU-POPUP-DELAY SETTINGS-GTK-MENU-POPUP-DELAY
                         "gtk-menu-popup-delay" "gint" T T)
                        (GTK-MODULES SETTINGS-GTK-MODULES
                         "gtk-modules" "gchararray" T T)
                        (GTK-OVERLAY-SCROLLING SETTINGS-GTK-OVERLAY-SCROLLING
                         "gtk-overlay-scrolling" "gboolean" T T)
                        (GTK-PRIMARY-BUTTON-WARPS-SLIDER
                         SETTINGS-GTK-PRIMARY-BUTTON-WARPS-SLIDER
                         "gtk-primary-button-warps-slider" "gboolean" T T)
                        (GTK-PRINT-BACKENDS SETTINGS-GTK-PRINT-BACKENDS
                         "gtk-print-backends" "gchararray" T T)
                        (GTK-PRINT-PREVIEW-COMMAND
                         SETTINGS-GTK-PRINT-PREVIEW-COMMAND
                         "gtk-print-preview-command" "gchararray" T T)
                        (GTK-RECENT-FILES-ENABLED
                         SETTINGS-GTK-RECENT-FILES-ENABLED
                         "gtk-recent-files-enabled" "gboolean" T T)
                        (GTK-RECENT-FILES-LIMIT
                         SETTINGS-GTK-RECENT-FILES-LIMIT
                         "gtk-recent-files-limit" "gint" T T)
                        (GTK-RECENT-FILES-MAX-AGE
                         SETTINGS-GTK-RECENT-FILES-MAX-AGE
                         "gtk-recent-files-max-age" "gint" T T)
                        (GTK-SCROLLED-WINDOW-PLACEMENT
                         SETTINGS-GTK-SCROLLED-WINDOW-PLACEMENT
                         "gtk-scrolled-window-placement" "GtkCornerType" T T)
                        (GTK-SHELL-SHOWS-APP-MENU
                         SETTINGS-GTK-SHELL-SHOWS-APP-MENU
                         "gtk-shell-shows-app-menu" "gboolean" T T)
                        (GTK-SHELL-SHOWS-DESKTOP
                         SETTINGS-GTK-SHELL-SHOWS-DESKTOP
                         "gtk-shell-shows-desktop" "gboolean" T T)
                        (GTK-SHELL-SHOWS-MENUBAR
                         SETTINGS-GTK-SHELL-SHOWS-MENUBAR
                         "gtk-shell-shows-menubar" "gboolean" T T)
                        (GTK-SHOW-INPUT-METHOD-MENU
                         SETTINGS-GTK-SHOW-INPUT-METHOD-MENU
                         "gtk-show-input-method-menu" "gboolean" T T)
                        (GTK-SHOW-UNICODE-MENU
                         SETTINGS-GTK-SHOW-UNICODE-MENU
                         "gtk-show-unicode-menu" "gboolean" T T)
                        (GTK-SOUND-THEME-NAME SETTINGS-GTK-SOUND-THEME-NAME
                         "gtk-sound-theme-name" "gchararray" T T)
                        (GTK-SPLIT-CURSOR SETTINGS-GTK-SPLIT-CURSOR
                         "gtk-split-cursor" "gboolean" T T)
                        (GTK-THEME-NAME SETTINGS-GTK-THEME-NAME
                         "gtk-theme-name" "gchararray" T T)
                        (GTK-TIMEOUT-EXPAND SETTINGS-GTK-TIMEOUT-EXPAND
                         "gtk-timeout-expand" "gint" T T)
                        (GTK-TIMEOUT-INITIAL SETTINGS-GTK-TIMEOUT-INITIAL
                         "gtk-timeout-initial" "gint" T T)
                        (GTK-TIMEOUT-REPEAT SETTINGS-GTK-TIMEOUT-REPEAT
                         "gtk-timeout-repeat" "gint" T T)
                        (GTK-TITLEBAR-DOUBLE-CLICK
                         SETTINGS-GTK-TITLEBAR-DOUBLE-CLICK
                         "gtk-titlebar-double-click" "gchararray" T T)
                        (GTK-TITLEBAR-MIDDLE-CLICK
                         SETTINGS-GTK-TITLEBAR-MIDDLE-CLICK
                         "gtk-titlebar-middle-click" "gchararray" T T)
                        (GTK-TITLEBAR-RIGHT-CLICK
                         SETTINGS-GTK-TITLEBAR-RIGHT-CLICK
                         "gtk-titlebar-right-click" "gchararray" T T)
                        (GTK-TOOLBAR-ICON-SIZE SETTINGS-GTK-TOOLBAR-ICON-SIZE
                         "gtk-toolbar-icon-size" "GtkIconSize" T T)
                        (GTK-TOOLBAR-STYLE SETTINGS-GTK-TOOLBAR-STYLE
                         "gtk-toolbar-style" "GtkToolbarStyle" T T)
                        (GTK-TOOLTIP-BROWSE-MODE-TIMEOUT
                         SETTINGS-GTK-TOOLTIP-BROWSE-MODE-TIMEOUT
                         "gtk-tooltip-browse-mode-timeout" "gint" T T)
                        (GTK-TOOLTIP-BROWSE-TIMEOUT
                         SETTINGS-GTK-TOOLTIP-BROWSE-TIMEOUT
                         "gtk-tooltip-browse-timeout" "gint" T T)
                        (GTK-TOOLTIP-TIMEOUT SETTINGS-GTK-TOOLTIP-TIMEOUT
                         "gtk-tooltip-timeout" "gint" T T)
                        (GTK-TOUCHSCREEN-MODE SETTINGS-GTK-TOUCHSCREEN-MODE
                         "gtk-touchscreen-mode" "gboolean" T T)
                        (GTK-VISIBLE-FOCUS SETTINGS-GTK-VISIBLE-FOCUS
                         "gtk-visible-focus" "GtkPolicyType" T T)
                        (GTK-XFT-ANTIALIAS SETTINGS-GTK-XFT-ANTIALIAS
                         "gtk-xft-antialias" "gint" T T)
                        (GTK-XFT-DPI SETTINGS-GTK-XFT-DPI
                         "gtk-xft-dpi" "gint" T T)
                        (GTK-XFT-HINTING SETTINGS-GTK-XFT-HINTING
                         "gtk-xft-hinting" "gint" T T)
                        (GTK-XFT-HINTSTYLE SETTINGS-GTK-XFT-HINTSTYLE
                         "gtk-xft-hintstyle" "gchararray" T T)
                        (GTK-XFT-RGBA SETTINGS-GTK-XFT-RGBA
                         "gtk-xft-rgba" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkSettings"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-settings-properties
  (let ((settings (gtk:settings-default)))
    ;; GHashTable is not implemented
    (signals (error) (gtk:settings-color-hash settings))
    (is (typep (gtk:settings-gtk-alternative-button-order settings) 'boolean))
    (is (typep (gtk:settings-gtk-alternative-sort-arrows settings) 'boolean))
    (is-false (gtk:settings-gtk-application-prefer-dark-theme settings))
    (is-true (gtk:settings-gtk-auto-mnemonics settings))
    (is-false (gtk:settings-gtk-button-images settings))
    (is-false (gtk:settings-gtk-can-change-accels settings))
    (is (stringp (gtk:settings-gtk-color-palette settings)))
    (is (stringp (gtk:settings-gtk-color-scheme settings)))
    (is-true (gtk:settings-gtk-cursor-blink settings))
    (is (integerp (gtk:settings-gtk-cursor-blink-time settings)))
    (is (integerp (gtk:settings-gtk-cursor-blink-timeout settings)))
    (is (typep (gtk:settings-gtk-cursor-theme-name settings) '(or null string)))
    (is (integerp (gtk:settings-gtk-cursor-theme-size settings)))
    (is-true (gtk:settings-gtk-decoration-layout settings))
    (is (typep (gtk:settings-gtk-dialogs-use-header settings) 'boolean))
    (is (integerp (gtk:settings-gtk-dnd-drag-threshold settings)))
    (is (integerp (gtk:settings-gtk-double-click-distance settings)))
    (is (integerp (gtk:settings-gtk-double-click-time settings)))
    (is-true (gtk:settings-gtk-enable-accels settings))
    (is-true (gtk:settings-gtk-enable-animations settings))
    (is-true (gtk:settings-gtk-enable-event-sounds settings))
    (is-true (gtk:settings-gtk-enable-input-feedback-sounds settings))
    (is-true (gtk:settings-gtk-enable-mnemonics settings))
    (is-true (gtk:settings-gtk-enable-primary-paste settings))
    (is-true (gtk:settings-gtk-enable-tooltips settings))
    (is-true (gtk:settings-gtk-entry-password-hint-timeout settings))
    (is-true (gtk:settings-gtk-entry-select-on-focus settings))
    (is-true (gtk:settings-gtk-error-bell settings))
    (is-false (gtk:settings-gtk-fallback-icon-theme settings))
    (is-false (gtk:settings-gtk-file-chooser-backend settings))
    (is (stringp (gtk:settings-gtk-font-name settings)))
    (is (integerp (gtk:settings-gtk-fontconfig-timestamp settings)))
    (is (typep (gtk:settings-gtk-icon-sizes settings) '(or null string)))
    (is (stringp (gtk:settings-gtk-icon-theme-name settings)))
    (is (stringp (gtk:settings-gtk-im-module settings)))
    (is (eq :callback (gtk:settings-gtk-im-preedit-style settings)))
    (is (eq :callback (gtk:settings-gtk-im-status-style settings)))
    (is (typep (gtk:settings-gtk-key-theme-name settings) '(or null string)))
    (is-false (gtk:settings-gtk-keynav-cursor-only settings))
    (is-false (gtk:settings-gtk-keynav-use-caret settings))
    (is-true  (gtk:settings-gtk-keynav-wrap-around settings))
    (is-true  (gtk:settings-gtk-label-select-on-focus settings))
    (is (integerp (gtk:settings-gtk-long-press-time settings)))
    (is (stringp (gtk:settings-gtk-menu-bar-accel settings)))
    (is (integerp (gtk:settings-gtk-menu-bar-popup-delay settings)))
    (is-false (gtk:settings-gtk-menu-images settings))
    (is (integerp (gtk:settings-gtk-menu-popdown-delay settings)))
    (is (integerp (gtk:settings-gtk-menu-popup-delay settings)))
    (is (typep (gtk:settings-gtk-modules settings) '(or null string)))
    (is-true  (gtk:settings-gtk-primary-button-warps-slider settings))
    (is (stringp (gtk:settings-gtk-print-backends settings)))
    (is (stringp (gtk:settings-gtk-print-preview-command settings)))
    (is (typep (gtk:settings-gtk-recent-files-enabled settings) 'boolean))
    (is (integerp (gtk:settings-gtk-recent-files-limit settings)))
    (is (integerp (gtk:settings-gtk-recent-files-max-age settings)))
    (is-true  (gtk:settings-gtk-scrolled-window-placement settings))
    (is-false (gtk:settings-gtk-shell-shows-app-menu settings))
    (is (typep (gtk:settings-gtk-shell-shows-desktop settings) 'boolean))
    (is-false (gtk:settings-gtk-shell-shows-menubar settings))
    (is-false (gtk:settings-gtk-show-input-method-menu settings))
    (is-false (gtk:settings-gtk-show-unicode-menu settings))
    (is (stringp (gtk:settings-gtk-sound-theme-name settings)))
    (is (typep (gtk:settings-gtk-split-cursor settings) 'boolean))
    (is (stringp (gtk:settings-gtk-theme-name settings)))
    (is (integerp (gtk:settings-gtk-timeout-expand settings)))
    (is (integerp (gtk:settings-gtk-timeout-initial settings)))
    (is (integerp (gtk:settings-gtk-timeout-repeat settings)))
    (is (stringp (gtk:settings-gtk-titlebar-double-click settings)))
    (is (stringp (gtk:settings-gtk-titlebar-middle-click settings)))
    (is (stringp (gtk:settings-gtk-titlebar-right-click settings)))
    (is (eq :large-toolbar (gtk:settings-gtk-toolbar-icon-size settings)))
    (is (eq :both-horiz (gtk:settings-gtk-toolbar-style settings)))
    (is (integerp (gtk:settings-gtk-tooltip-browse-mode-timeout settings)))
    (is (integerp (gtk:settings-gtk-tooltip-browse-timeout settings)))
    (is (integerp (gtk:settings-gtk-tooltip-timeout settings)))
    (is-false (gtk:settings-gtk-touchscreen-mode settings))
    (is (eq :automatic (gtk:settings-gtk-visible-focus settings)))
    (is (integerp (gtk:settings-gtk-xft-antialias settings)))
    (is (integerp (gtk:settings-gtk-xft-dpi settings)))
    (is (integerp (gtk:settings-gtk-xft-hinting settings)))
    (is (stringp (gtk:settings-gtk-xft-hintstyle settings)))
    (is (stringp (gtk:settings-gtk-xft-rgba settings)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_settings_get_default

(test gtk-settings-default
  (is (typep (gtk:settings-default) 'gtk:settings)))

;;;     gtk_settings_get_for_screen

(test gtk-settings-for-screen
  (is (typep (gtk:settings-for-screen (gdk:screen-default)) 'gtk:settings)))

;;;     gtk_settings_install_property                      deprecated
;;;     gtk_settings_install_property_parser               deprecated
;;;     gtk_rc_property_parse_color
;;;     gtk_rc_property_parse_enum
;;;     gtk_rc_property_parse_flags
;;;     gtk_rc_property_parse_requisition
;;;     gtk_rc_property_parse_border
;;;     gtk_settings_set_property_value                    deprecated
;;;     gtk_settings_set_string_property                   deprecated
;;;     gtk_settings_set_long_property                     deprecated
;;;     gtk_settings_set_double_property                   deprecated
;;;     gtk_settings_reset_property ()

;;; 2024-9-21
