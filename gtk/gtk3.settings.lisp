;;; ----------------------------------------------------------------------------
;;; gtk3.settings.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Settings
;;;
;;;     Sharing settings between applications
;;;
;;; Types and Values
;;;
;;;     GtkSettings
;;;     GtkIMPreeditStyle                                   deprecated
;;;     GtkIMStatusStyle                                    deprecated
;;;
;;; Functions
;;;
;;;     gtk_settings_get_default
;;;     gtk_settings_get_for_screen
;;;     gtk_settings_install_property                       deprecated
;;;     gtk_settings_install_property_parser                deprecated
;;;     gtk_rc_property_parse_color
;;;     gtk_rc_property_parse_enum
;;;     gtk_rc_property_parse_flags
;;;     gtk_rc_property_parse_requisition
;;;     gtk_rc_property_parse_border
;;;     gtk_settings_set_property_value                     deprecated
;;;     gtk_settings_set_string_property                    deprecated
;;;     gtk_settings_set_long_property                      deprecated
;;;     gtk_settings_set_double_property                    deprecated
;;;     gtk_settings_reset_property
;;;
;;; Properties
;;;
;;;     color-hash
;;;     gtk-alternative-button-order
;;;     gtk-alternative-sort-arrows
;;;     gtk-application-prefer-dark-theme
;;;     gtk-auto-mnemonics
;;;     gtk-button-images
;;;     gtk-can-change-accels
;;;     gtk-color-palette
;;;     gtk-color-scheme
;;;     gtk-cursor-aspect-ratio
;;;     gtk-cursor-blink
;;;     gtk-cursor-blink-time
;;;     gtk-cursor-blink-timeout
;;;     gtk-cursor-theme-name
;;;     gtk-cursor-theme-size
;;;     gtk-decoration-layout
;;;     gtk-dialogs-use-header
;;;     gtk-dnd-drag-threshold
;;;     gtk-double-click-distance
;;;     gtk-double-click-time
;;;     gtk-enable-accels
;;;     gtk-enable-animations
;;;     gtk-enable-event-sounds
;;;     gtk-enable-input-feedback-sounds
;;;     gtk-enable-mnemonics
;;;     gtk-enable-primary-paste
;;;     gtk-enable-tooltips
;;;     gtk-entry-password-hint-timeout
;;;     gtk-entry-select-on-focus
;;;     gtk-error-bell
;;;     gtk-fallback-icon-theme
;;;     gtk-file-chooser-backend
;;;     gtk-font-name
;;;     gtk-fontconfig-timestamp
;;;     gtk-icon-sizes
;;;     gtk-icon-theme-name
;;;     gtk-im-module
;;;     gtk-im-preedit-style
;;;     gtk-im-status-style
;;;     gtk-key-theme-name
;;;     gtk-keynav-cursor-only
;;;     gtk-keynav-use-caret
;;;     gtk-keynav-wrap-around
;;;     gtk-label-select-on-focus
;;;     gtk-long-press-time
;;;     gtk-menu-bar-accel
;;;     gtk-menu-bar-popup-delay
;;;     gtk-menu-images
;;;     gtk-menu-popdown-delay
;;;     gtk-menu-popup-delay
;;;     gtk-modules
;;;     gtk-overlay-scrolling
;;;     gtk-primary-button-warps-slider
;;;     gtk-print-backends
;;;     gtk-print-preview-command
;;;     gtk-recent-files-enabled
;;;     gtk-recent-files-limit
;;;     gtk-recent-files-max-age
;;;     gtk-scrolled-window-placement
;;;     gtk-shell-shows-app-menu
;;;     gtk-shell-shows-desktop
;;;     gtk-shell-shows-menubar
;;;     gtk-show-input-method-menu
;;;     gtk-show-unicode-menu
;;;     gtk-sound-theme-name
;;;     gtk-split-cursor
;;;     gtk-theme-name
;;;     gtk-timeout-expand
;;;     gtk-timeout-initial
;;;     gtk-timeout-repeat
;;;     gtk-titlebar-double-click
;;;     gtk-titlebar-middle-click
;;;     gtk-titlebar-right-click
;;;     gtk-toolbar-icon-size
;;;     gtk-toolbar-style
;;;     gtk-tooltip-browse-mode-timeout
;;;     gtk-tooltip-browse-timeout
;;;     gtk-tooltip-timeout
;;;     gtk-touchscreen-mode
;;;     gtk-visible-focus
;;;     gtk-xft-antialias
;;;     gtk-xft-dpi
;;;     gtk-xft-hinting
;;;     gtk-xft-hintstyle
;;;     gtk-xft-rgba
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSettings
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSettings implements GtkStyleProvider and GtkStyleProviderPrivate.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkIMPreeditStyle
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkIMPreeditStyle" im-preedit-style
  (:export t
   :type-initializer "gtk_im_preedit_style_get_type")
  (:nothing 0)
  (:callback 1)
  (:none 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'im-preedit-style)
      "GEnum"
      (liber:symbol-documentation 'im-preedit-style)
 "@version{2025-07-03}
  @begin{declaration}
(gobject:define-genum \"GtkIMPreeditStyle\" im-preedit-style
  (:export t
   :type-initializer \"gtk_im_preedit_style_get_type\")
  (:nothing 0)
  (:callback 1)
  (:none 2))
  @end{declaration}
  @short{Style for input method preedit.}
  See also the @slot[gtk:settings]{gtk-im-preedit-style} property.
  @begin[Warning]{dictionary}
    The @sym{gtk:im-preedit-style} enumeration has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-im-preedit-style}")

;;; ----------------------------------------------------------------------------
;;; GtkIMStatusStyle
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkIMStatusStyle" im-status-style
  (:export t
   :type-initializer "gtk_im_status_style_get_type")
  (:nothing 0)
  (:callback 1)
  (:none 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'im-status-style)
      "GEnum"
      (liber:symbol-documentation 'im-status-style)
 "@version{2025-07-03}
  @begin{declaration}
(gobject:define-genum \"GtkIMStatusStyle\" gtk-im-status-style
  (:export t
   :type-initializer \"gtk_im_status_style_get_type\")
  (:nothing 0)
  (:callback 1)
  (:none 2))
  @end{declaration}
  @short{Style for input method status.}
  See also the @slot[gtk:settings]{gtk-im-status-style} property.
  @begin[Warning]{dictionary}
    The @sym{gtk:im-status-style} enumeration has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-im-status-style}")

;;; ----------------------------------------------------------------------------
;;; GtkSettings
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSettings" settings
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_settings_get_type")
  ((color-hash
    settings-color-hash
    "color-hash" "GHashTable" t nil)
   (gtk-alternative-button-order
    settings-gtk-alternative-button-order
    "gtk-alternative-button-order" "gboolean" t t)
   (gtk-alternative-sort-arrows
    settings-gtk-alternative-sort-arrows
    "gtk-alternative-sort-arrows" "gboolean" t t)
   (gtk-application-prefer-dark-theme
    settings-gtk-application-prefer-dark-theme
    "gtk-application-prefer-dark-theme" "gboolean" t t)
   (gtk-auto-mnemonics
    settings-gtk-auto-mnemonics
    "gtk-auto-mnemonics" "gboolean" t t)
   (gtk-button-images
    settings-gtk-button-images
    "gtk-button-images" "gboolean" t t)
   (gtk-can-change-accels
    settings-gtk-can-change-accels
    "gtk-can-change-accels" "gboolean" t t)
   (gtk-color-palette
    settings-gtk-color-palette
    "gtk-color-palette" "gchararray" t t)
   (gtk-color-scheme
    settings-gtk-color-scheme
    "gtk-color-scheme" "gchararray" t t)
   (gtk-cursor-aspect-ratio
    settings-gtk-cursor-aspect-ratio
    "gtk-cursor-aspect-ratio" "gfloat" t t)
   (gtk-cursor-blink
    settings-gtk-cursor-blink
    "gtk-cursor-blink" "gboolean" t t)
   (gtk-cursor-blink-time
    settings-gtk-cursor-blink-time
    "gtk-cursor-blink-time" "gint" t t)
   (gtk-cursor-blink-timeout
    settings-gtk-cursor-blink-timeout
    "gtk-cursor-blink-timeout" "gint" t t)
   (gtk-cursor-theme-name
    settings-gtk-cursor-theme-name
    "gtk-cursor-theme-name" "gchararray" t t)
   (gtk-cursor-theme-size
    settings-gtk-cursor-theme-size
    "gtk-cursor-theme-size" "gint" t t)
   (gtk-decoration-layout
    settings-gtk-decoration-layout
    "gtk-decoration-layout" "gchararray" t t)
   (gtk-dialogs-use-header
    settings-gtk-dialogs-use-header
    "gtk-dialogs-use-header" "gboolean" t t)
   (gtk-dnd-drag-threshold
    settings-gtk-dnd-drag-threshold
    "gtk-dnd-drag-threshold" "gint" t t)
   (gtk-double-click-distance
    settings-gtk-double-click-distance
    "gtk-double-click-distance" "gint" t t)
   (gtk-double-click-time
    settings-gtk-double-click-time
    "gtk-double-click-time" "gint" t t)
   (gtk-enable-accels
    settings-gtk-enable-accels
    "gtk-enable-accels" "gboolean" t t)
   (gtk-enable-animations
    settings-gtk-enable-animations
    "gtk-enable-animations" "gboolean" t t)
   (gtk-enable-event-sounds
    settings-gtk-enable-event-sounds
    "gtk-enable-event-sounds" "gboolean" t t)
   (gtk-enable-input-feedback-sounds
    settings-gtk-enable-input-feedback-sounds
    "gtk-enable-input-feedback-sounds" "gboolean" t t)
   (gtk-enable-mnemonics
    settings-gtk-enable-mnemonics
    "gtk-enable-mnemonics" "gboolean" t t)
   (gtk-enable-primary-paste
    settings-gtk-enable-primary-paste
    "gtk-enable-primary-paste" "gboolean" t t)
   (gtk-enable-tooltips
    settings-gtk-enable-tooltips
    "gtk-enable-tooltips" "gboolean" t t)
   (gtk-entry-password-hint-timeout
    settings-gtk-entry-password-hint-timeout
    "gtk-entry-password-hint-timeout" "guint" t t)
   (gtk-entry-select-on-focus
    settings-gtk-entry-select-on-focus
    "gtk-entry-select-on-focus" "gboolean" t t)
   (gtk-error-bell
    settings-gtk-error-bell
    "gtk-error-bell" "gboolean" t t)
   (gtk-fallback-icon-theme
    settings-gtk-fallback-icon-theme
    "gtk-fallback-icon-theme" "gchararray" t t)
   (gtk-file-chooser-backend
    settings-gtk-file-chooser-backend
    "gtk-file-chooser-backend" "gchararray" t t)
   (gtk-font-name
    settings-gtk-font-name
    "gtk-font-name" "gchararray" t t)
   (gtk-fontconfig-timestamp
    settings-gtk-fontconfig-timestamp
    "gtk-fontconfig-timestamp" "guint" t t)
   (gtk-icon-sizes
    settings-gtk-icon-sizes
    "gtk-icon-sizes" "gchararray" t t)
   (gtk-icon-theme-name
    settings-gtk-icon-theme-name
    "gtk-icon-theme-name" "gchararray" t t)
   (gtk-im-module
    settings-gtk-im-module "gtk-im-module"
    "gchararray" t t)
   (gtk-im-preedit-style
    settings-gtk-im-preedit-style
    "gtk-im-preedit-style" "GtkIMPreeditStyle" t t)
   (gtk-im-status-style
    settings-gtk-im-status-style
    "gtk-im-status-style" "GtkIMStatusStyle" t t)
   (gtk-key-theme-name
    settings-gtk-key-theme-name
    "gtk-key-theme-name" "gchararray" t t)
   (gtk-keynav-cursor-only
    settings-gtk-keynav-cursor-only
    "gtk-keynav-cursor-only" "gboolean" t t)
   (gtk-keynav-use-caret
    settings-gtk-keynav-use-caret
    "gtk-keynav-use-caret" "gboolean" t t)
   (gtk-keynav-wrap-around
    settings-gtk-keynav-wrap-around
    "gtk-keynav-wrap-around" "gboolean" t t)
   (gtk-label-select-on-focus
    settings-gtk-label-select-on-focus
    "gtk-label-select-on-focus" "gboolean" t t)
   (gtk-long-press-time
    settings-gtk-long-press-time
    "gtk-long-press-time" "guint" t t)
   (gtk-menu-bar-accel
    settings-gtk-menu-bar-accel
    "gtk-menu-bar-accel" "gchararray" t t)
   (gtk-menu-bar-popup-delay
    settings-gtk-menu-bar-popup-delay
    "gtk-menu-bar-popup-delay" "gint" t t)
   (gtk-menu-images
    settings-gtk-menu-images
    "gtk-menu-images" "gboolean" t t)
   (gtk-menu-popdown-delay
    settings-gtk-menu-popdown-delay
    "gtk-menu-popdown-delay" "gint" t t)
   (gtk-menu-popup-delay
    settings-gtk-menu-popup-delay
    "gtk-menu-popup-delay" "gint" t t)
   (gtk-modules
    settings-gtk-modules
    "gtk-modules" "gchararray" t t)
   (gtk-overlay-scrolling
    settings-gtk-overlay-scrolling
    "gtk-overlay-scrolling" "gboolean" t t)
   (gtk-primary-button-warps-slider
    settings-gtk-primary-button-warps-slider
    "gtk-primary-button-warps-slider" "gboolean" t t)
   (gtk-print-backends
    settings-gtk-print-backends
    "gtk-print-backends" "gchararray" t t)
   (gtk-print-preview-command
    settings-gtk-print-preview-command
    "gtk-print-preview-command" "gchararray" t t)
   (gtk-recent-files-enabled
    settings-gtk-recent-files-enabled
    "gtk-recent-files-enabled" "gboolean" t t)
   (gtk-recent-files-limit
    settings-gtk-recent-files-limit
    "gtk-recent-files-limit" "gint" t t)
   (gtk-recent-files-max-age
    settings-gtk-recent-files-max-age
    "gtk-recent-files-max-age" "gint" t t)
   (gtk-scrolled-window-placement
    settings-gtk-scrolled-window-placement
    "gtk-scrolled-window-placement" "GtkCornerType" t t)
   (gtk-shell-shows-app-menu
    settings-gtk-shell-shows-app-menu
    "gtk-shell-shows-app-menu" "gboolean" t t)
   (gtk-shell-shows-desktop
    settings-gtk-shell-shows-desktop
    "gtk-shell-shows-desktop" "gboolean" t t)
   (gtk-shell-shows-menubar
    settings-gtk-shell-shows-menubar
    "gtk-shell-shows-menubar" "gboolean" t t)
   (gtk-show-input-method-menu
    settings-gtk-show-input-method-menu
    "gtk-show-input-method-menu" "gboolean" t t)
   (gtk-show-unicode-menu
    settings-gtk-show-unicode-menu
    "gtk-show-unicode-menu" "gboolean" t t)
   (gtk-sound-theme-name
    settings-gtk-sound-theme-name
    "gtk-sound-theme-name" "gchararray" t t)
   (gtk-split-cursor
    settings-gtk-split-cursor
    "gtk-split-cursor" "gboolean" t t)
   (gtk-theme-name
    settings-gtk-theme-name
    "gtk-theme-name" "gchararray" t t)
   (gtk-timeout-expand
    settings-gtk-timeout-expand
    "gtk-timeout-expand" "gint" t t)
   (gtk-timeout-initial
    settings-gtk-timeout-initial
    "gtk-timeout-initial" "gint" t t)
   (gtk-timeout-repeat
    settings-gtk-timeout-repeat
    "gtk-timeout-repeat" "gint" t t)
   (gtk-titlebar-double-click
    settings-gtk-titlebar-double-click
    "gtk-titlebar-double-click" "gchararray" t t)
   (gtk-titlebar-middle-click
    settings-gtk-titlebar-middle-click
    "gtk-titlebar-middle-click" "gchararray" t t)
   (gtk-titlebar-right-click
    settings-gtk-titlebar-right-click
    "gtk-titlebar-right-click" "gchararray" t t)
   (gtk-toolbar-icon-size
    settings-gtk-toolbar-icon-size
    "gtk-toolbar-icon-size" "GtkIconSize" t t)
   (gtk-toolbar-style
    settings-gtk-toolbar-style
    "gtk-toolbar-style" "GtkToolbarStyle" t t)
   (gtk-tooltip-browse-mode-timeout
    settings-gtk-tooltip-browse-mode-timeout
    "gtk-tooltip-browse-mode-timeout" "gint" t t)
   (gtk-tooltip-browse-timeout
    settings-gtk-tooltip-browse-timeout
    "gtk-tooltip-browse-timeout" "gint" t t)
   (gtk-tooltip-timeout
    settings-gtk-tooltip-timeout
    "gtk-tooltip-timeout" "gint" t t)
   (gtk-touchscreen-mode
    settings-gtk-touchscreen-mode
    "gtk-touchscreen-mode" "gboolean" t t)
   (gtk-visible-focus
    settings-gtk-visible-focus
    "gtk-visible-focus" "GtkPolicyType" t t)
   (gtk-xft-antialias
    settings-gtk-xft-antialias
    "gtk-xft-antialias" "gint" t t)
   (gtk-xft-dpi
    settings-gtk-xft-dpi
    "gtk-xft-dpi" "gint" t t)
   (gtk-xft-hinting
    settings-gtk-xft-hinting
    "gtk-xft-hinting" "gint" t t)
   (gtk-xft-hintstyle
    settings-gtk-xft-hintstyle
    "gtk-xft-hintstyle" "gchararray" t t)
   (gtk-xft-rgba
    settings-gtk-xft-rgba
    "gtk-xft-rgba" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'settings 'type)
 "@version{2025-07-03}
  @begin{short}
    The @class{gtk:settings} object provide a mechanism to share global settings
    between applications.
  @end{short}
  On the X window system, this sharing is realized by an XSettings manager
  that is usually part of the desktop environment, along with utilities that
  let the user change these settings. In the absence of an XSettings manager,
  GTK reads default values for settings from @file{settings.ini} files in
  @file{/etc/gtk-3.0} and @file{$XDG_CONFIG_HOME/gtk-3.0}. These files must
  be valid key files, see the @type{g:key-file} API, and have a section called
  \"Settings\". Themes can also provide default values for settings by
  installing a @file{settings.ini} file next to their @file{gtk.css} file.

  Applications can override system-wide settings with the accessor functions
  of the slots. This should be restricted to special cases though. The
  @class{gtk:settings} settings are not meant as an application configuration
  facility. When doing so, you need to be aware that settings that are specific
  to individual widgets may not be available before the widget type has been
  realized at least once. The following example demonstrates a way to do this:
  @begin{pre}
;; Make sure the type is realized
(g:type-class-unref (g:type-class-ref \"GtkMenuItem\"))
(setf (gtk:settings-gtk-menu-images (gtk:settings-default)) t)
  @end{pre}
  There is one @class{gtk:settings} object per screen. It can be obtained with
  the @fun{gtk:settings-for-screen} function, but in many cases, it is more
  convenient to use the @fun{gtk:widget-settings} function. The
  @fun{gtk:settings-default} function returns the @class{gtk:settings} object
  for the default screen.
  @see-constructor{gtk:settings-default}
  @see-constructor{gtk:settings-for-screen}
  @see-slot{gtk:settings-color-hash}
  @see-slot{gtk:settings-gtk-alternative-button-order}
  @see-slot{gtk:settings-gtk-alternative-sort-arrows}
  @see-slot{gtk:settings-gtk-application-prefer-dark-theme}
  @see-slot{gtk:settings-gtk-auto-mnemonics}
  @see-slot{gtk:settings-gtk-button-images}
  @see-slot{gtk:settings-gtk-can-change-accels}
  @see-slot{gtk:settings-gtk-color-palette}
  @see-slot{gtk:settings-gtk-color-scheme}
  @see-slot{gtk:settings-gtk-cursor-aspect-ratio}
  @see-slot{gtk:settings-gtk-cursor-blink}
  @see-slot{gtk:settings-gtk-cursor-blink-time}
  @see-slot{gtk:settings-gtk-cursor-blink-timeout}
  @see-slot{gtk:settings-gtk-cursor-theme-name}
  @see-slot{gtk:settings-gtk-cursor-theme-size}
  @see-slot{gtk:settings-gtk-decoration-layout}
  @see-slot{gtk:settings-gtk-dialogs-use-header}
  @see-slot{gtk:settings-gtk-dnd-drag-threshold}
  @see-slot{gtk:settings-gtk-double-click-distance}
  @see-slot{gtk:settings-gtk-double-click-time}
  @see-slot{gtk:settings-gtk-enable-accels}
  @see-slot{gtk:settings-gtk-enable-animations}
  @see-slot{gtk:settings-gtk-enable-event-sounds}
  @see-slot{gtk:settings-gtk-enable-input-feedback-sounds}
  @see-slot{gtk:settings-gtk-enable-mnemonics}
  @see-slot{gtk:settings-gtk-enable-primary-paste}
  @see-slot{gtk:settings-gtk-enable-tooltips}
  @see-slot{gtk:settings-gtk-entry-password-hint-timeout}
  @see-slot{gtk:settings-gtk-entry-select-on-focus}
  @see-slot{gtk:settings-gtk-error-bell}
  @see-slot{gtk:settings-gtk-fallback-icon-theme}
  @see-slot{gtk:settings-gtk-file-chooser-backend}
  @see-slot{gtk:settings-gtk-font-name}
  @see-slot{gtk:settings-gtk-fontconfig-timestamp}
  @see-slot{gtk:settings-gtk-icon-sizes}
  @see-slot{gtk:settings-gtk-icon-theme-name}
  @see-slot{gtk:settings-gtk-im-module}
  @see-slot{gtk:settings-gtk-im-preedit-style}
  @see-slot{gtk:settings-gtk-im-status-style}
  @see-slot{gtk:settings-gtk-key-theme-name}
  @see-slot{gtk:settings-gtk-keynav-cursor-only}
  @see-slot{gtk:settings-gtk-keynav-use-caret}
  @see-slot{gtk:settings-gtk-keynav-wrap-around}
  @see-slot{gtk:settings-gtk-label-select-on-focus}
  @see-slot{gtk:settings-gtk-long-press-time}
  @see-slot{gtk:settings-gtk-menu-bar-accel}
  @see-slot{gtk:settings-gtk-menu-bar-popup-delay}
  @see-slot{gtk:settings-gtk-menu-images}
  @see-slot{gtk:settings-gtk-menu-popdown-delay}
  @see-slot{gtk:settings-gtk-menu-popup-delay}
  @see-slot{gtk:settings-gtk-modules}
  @see-slot{gtk:settings-gtk-overlay-scrolling}
  @see-slot{gtk:settings-gtk-primary-button-warps-slider}
  @see-slot{gtk:settings-gtk-print-backends}
  @see-slot{gtk:settings-gtk-print-preview-command}
  @see-slot{gtk:settings-gtk-recent-files-enabled}
  @see-slot{gtk:settings-gtk-recent-files-limit}
  @see-slot{gtk:settings-gtk-recent-files-max-age}
  @see-slot{gtk:settings-gtk-scrolled-window-placement}
  @see-slot{gtk:settings-gtk-shell-shows-app-menu}
  @see-slot{gtk:settings-gtk-shell-shows-desktop}
  @see-slot{gtk:settings-gtk-shell-shows-menubar}
  @see-slot{gtk:settings-gtk-show-input-method-menu}
  @see-slot{gtk:settings-gtk-show-unicode-menu}
  @see-slot{gtk:settings-gtk-sound-theme-name}
  @see-slot{gtk:settings-gtk-split-cursor}
  @see-slot{gtk:settings-gtk-theme-name}
  @see-slot{gtk:settings-gtk-timeout-expand}
  @see-slot{gtk:settings-gtk-timeout-initial}
  @see-slot{gtk:settings-gtk-timeout-repeat}
  @see-slot{gtk:settings-gtk-titlebar-double-click}
  @see-slot{gtk:settings-gtk-titlebar-middle-click}
  @see-slot{gtk:settings-gtk-titlebar-right-click}
  @see-slot{gtk:settings-gtk-toolbar-icon-size}
  @see-slot{gtk:settings-gtk-toolbar-style}
  @see-slot{gtk:settings-gtk-tooltip-browse-mode-timeout}
  @see-slot{gtk:settings-gtk-tooltip-browse-timeout}
  @see-slot{gtk:settings-gtk-tooltip-timeout}
  @see-slot{gtk:settings-gtk-touchscreen-mode}
  @see-slot{gtk:settings-gtk-visible-focus}
  @see-slot{gtk:settings-gtk-xft-antialias}
  @see-slot{gtk:settings-gtk-xft-dpi}
  @see-slot{gtk:settings-gtk-xft-hinting}
  @see-slot{gtk:settings-gtk-xft-hintstyle}
  @see-slot{gtk:settings-gtk-xft-rgba}
  @see-class{g:key-file}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:settings-color-hash ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "color-hash" 'settings) t)
 "The @code{color-hash} property of type @code{GHashTable} (Read) @br{}
  Holds a hash table representation of the @slot[gtk:settings]{gtk-color-scheme}
  setting, mapping color names to @class{gdk:color} colors. @br{}
  @em{Warning:} The @code{color-hash} property has been deprecated since
  version 3.8 and should not be used in newly written code. Will always return
  an empty hash table.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-color-hash)
      "Accessor"
      (documentation 'settings-color-hash 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-color-hash object) => setting}
  @syntax{(setf (gtk:settings-color-hash object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a @code{GHashTable} instance}
  @begin{short}
    Accessor of the @slot[gtk:settings]{color-hash} slot of the
    @class{gtk:settings} class.
  @end{short}
  Holds a hash table representation of the @slot[gtk:settings]{gtk-color-scheme}
  setting, mapping color names to @class{gdk:color} colors.
  @begin[Warning]{dictionary}
    The @code{color-hash} property has been deprecated since version 3.8 and
    should not be used in newly written code. Will always return an empty hash
    table.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-class{gdk:color}")

;;; --- gtk:settings-gtk-alternative-button-order ------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-alternative-button-order"
                                               'settings) t)
 "The @code{gtk-alternative-button-order} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether buttons in dialogs should use the alternative button order. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-alternative-button-order)
      "Accessor"
      (documentation 'settings-gtk-alternative-button-order 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-alternative-button-order object) => setting}
  @syntax{(setf (gtk:settings-gtk-alternative-button-order object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether buttons in dialogs should use the
   alternative button order}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-alternative-button-order} slot of
    the @class{gtk:settings} class.
  @end{short}
  Whether buttons in dialogs should use the alternative button order.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-alternative-sort-arrows -------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-alternative-sort-arrows"
                                               'settings) t)
 "The @code{gtk-alternative-sort-arrows} property of type @code{:boolean}
  (Read / Write) @br{}
  Controls the direction of the sort indicators in sorted list and tree views.
  By default an arrow pointing down means the column is sorted in ascending
  order. When set to @em{true}, this order will be inverted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-alternative-sort-arrows)
      "Accessor"
      (documentation 'settings-gtk-alternative-sort-arrows 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-alternative-sort-arrows object) => setting}
  @syntax{(setf (gtk:settings-gtk-alternative-sort-arrows object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean which controls the direction of sort indicators}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-alternative-sort-arrows} slot of
    the @class{gtk:settings} class.
  @end{short}
  Controls the direction of the sort indicators in sorted list and tree views.
  By default an arrow pointing down means the column is sorted in ascending
  order. When set to @em{true}, this order will be inverted.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-application-prefer-dark-theme -------------------------

#+liber-documentation
(setf (documentation
        (liber:slot-documentation "gtk-application-prefer-dark-theme"
                                  'settings) t)
 "The @code{gtk-application-prefer-dark-theme} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the application prefers to use a dark theme. If a GTK theme includes
  a dark variant, it will be used instead of the configured theme. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-application-prefer-dark-theme)
      "Accessor"
      (documentation 'settings-gtk-application-prefer-dark-theme 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-application-prefer-dark-theme object) => setting}
  @syntax{(setf (gtk:settings-gtk-application-prefer-dark-theme object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the application prefers to use a dark
    theme}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-application-prefer-dark-theme} slot
    of the @class{gtk:settings} class.
  @end{short}
  Whether the application prefers to use a dark theme. If a GTK theme includes
  a dark variant, it will be used instead of the configured theme.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-auto-mnemonics ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-auto-mnemonics"
                                               'settings) t)
 "The @code{gtk-auto-mnemonics} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether mnemonics should be automatically shown and hidden when the user
  presses the mnemonic activator. @br{}
  @em{Warning:} The @code{gtk-auto-mnemonics} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-auto-mnemonics)
      "Accessor"
      (documentation 'settings-gtk-auto-mnemonics 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-auto-mnemonics object) => setting}
  @syntax{(setf (gtk:settings-gtk-auto-mnemonics object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether mnemonics should be automically shown}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-auto-mnemonics} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether mnemonics should be automatically shown and hidden when the user
  presses the mnemonic activator.
  @begin[Warning]{dictionary}
    The @code{gtk-auto-mnemonics} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-button-images -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-button-images" 'settings) t)
 "The @code{gtk-button-images} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether images should be shown on buttons. @br{}
  @em{Warning:} The @code{gtk-button-images} property has been deprecated since
  version 3.10 and should not be used in newly written code. This setting is
  deprecated. Application developers control whether a button should show an
  icon or not, on a per-button basis. If a button should show an icon, use the
  @slot[gtk-button]{always-show-image} property of the @class{gtk:button}
  widget, and pack a @class{gtk:image} widget inside the button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-button-images)
      "Accessor"
      (documentation 'settings-gtk-button-images 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-button-images object) => setting}
  @syntax{(setf gtk:settings-gtk-button-images object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether images should be shown on buttons}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-button-images} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether images should be shown on buttons.
  @begin[Warning]{dictionary}
    The @fun{gtk:settings-gtk-button-images} function has been deprecated since
    version 3.10 and should not be used in newly written code. This setting is
    deprecated. Application developers control whether a button should show an
    icon or not, on a per-button basis. If a button should show an icon, use
    the @slot[gtk-button]{always-show-image} property of the @class{gtk:button}
    widget, and pack a @class{gtk:image} widget inside the button.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-class{gtk:button}
  @see-class{gtk:image}
  @see-function{gtk:button-always-show-image}")

;;; --- gtk:settings-gtk-can-change-accels -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-can-change-accels"
                                               'settings) t)
 "The @code{gtk-can-change-accels} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether menu accelerators can be changed by pressing a key over the menu
  item. @br{}
  @em{Warning:} The @code{gtk-can-change-accels} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-can-change-accels)
      "Accessor"
      (documentation 'settings-gtk-can-change-accels 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-can-change-accels object) => setting}
  @syntax{(setf gtk:settings-gtk-can-change-accels object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether menu accelerators can be changed}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-can-change-accels} slot of the
    @class{gtk:settings} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @code{gtk-can-change-accels} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-color-palette -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-color-palette" 'settings) t)
 "The @code{gtk-color-palette} property of type @code{:string} (Read / Write)
  @br{}
  The palette to use in the deprecated color selector. @br{}
  @em{Warning:} The @code{gtk-color-palette} property has been deprecated since
  version 3.10 and should not be used in newly written code. Only used by
  the deprecated color selector widget.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-color-palette)
      "Accessor"
      (documentation 'settings-gtk-color-palette 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-color-palette object) => setting}
  @syntax{(setf gtk:settings-gtk-color-palette object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the palette to use}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-color-palette} slot of the
    @class{gtk:settings} class.
  @end{short}
  Palette to use in the deprecated color selector.
  @begin[Warning]{dictionary}
    The @code{gtk-color-palette} property has been deprecated since
    version 3.10 and should not be used in newly written code. Only used by
    the deprecated color selector widget.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-color-scheme ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-color-scheme" 'settings) t)
 "The @code{gtk-color-scheme} property of type @code{:string} (Read / Write)
  @br{}
  The palette of named colors for use in themes. @br{}
  @em{Warning:}
  The @code{gtk-color-scheme} property has been deprecated since version 3.8
  and should not be used in newly written code. Color scheme support was
  dropped and is no longer supported. You can still set this property, but it
  will be ignored. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-color-scheme)
      "Accessor"
      (documentation 'settings-gtk-color-scheme 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-color-scheme object) => setting}
  @syntax{(setf gtk:settings-gtk-color-scheme object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for a palette of named colors}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-color-scheme} slot of the
    @class{gtk:settings} class.
  @end{short}
  A palette of named colors for use in themes.
  @begin[Warning]{dictionary}
    The @code{gtk-color-scheme} property has been deprecated since version 3.8
    and should not be used in newly written code. Color scheme support was
    dropped and is no longer supported. You can still set this property, but it
    will be ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-aspect-ratio -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-aspect-ratio"
                                               'settings) t)
 "The @code{gtk-cursor-aspect-ratio} property of type @code{:float}
  (Read / Write) @br{}
  The aspect ratio of the text caret. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.04")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-aspect-ratio)
      "Accessor"
      (documentation 'settings-gtk-cursor-aspect-ratio 'function)
 "@version{2025-05-07}
  @syntax{(gtk:settings-gtk-cursor-aspect-ratio object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-aspect-ratio object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a number coerced to a single float for the aspect ratio
    of the text caret}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-aspect-ratio} slot of the
    @class{gtk:settings} class.
  @end{short}
  The aspect ratio of the text caret.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-blink ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-blink" 'settings) t)
 "The @code{gtk-cursor-blink} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the cursor should blink. Also see the
  @slot[gtk:settings]{gtk-cursor-blink-timeout} setting, which allows more
  flexible control over cursor blinking. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-blink)
      "Accessor"
      (documentation 'settings-gtk-cursor-blink 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-cursor-blink object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-blink object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the cursor should blink}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-blink} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether the cursor should blink. Also see the
  @slot[gtk:settings]{gtk-cursor-blink-timeout} setting, which allows more
  flexible control over cursor blinking.
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-cursor-blink-timeout}")

;;; --- gtk:settings-gtk-cursor-blink-time -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-blink-time"
                                               'settings) t)
 "The @code{gtk-cursor-blink-time} property of type @code{:int} (Read / Write)
  @br{}
  The length of the cursor blink cycle, in milliseconds. @br{}
  Allowed values: >= 100 @br{}
  Default value: 1200")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-blink-time)
      "Accessor"
      (documentation 'settings-gtk-cursor-blink-time 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-cursor-blink-time object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-blink-time object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the length of the cursor blink cycle,
    in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-blink-time} slot of the
    @class{gtk:settings} class.
  @end{short}
  Length of the cursor blink cycle, in milliseconds.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-blink-timeout ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-blink-timeout"
                                               'settings) t)
 "The @code{gtk-cursor-blink-timeout} property of type @code{:int}
  (Read / Write) @br{}
  The time after which the cursor stops blinking, in seconds. The timer is reset
  after each user interaction. Setting this to zero has the same effect as
  setting the @slot[gtk:settings]{gtk-cursor-blink} property to @em{false}.
  @br{}
  Allowed values: >= 1 @br{}
  Default value: 10")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-blink-timeout)
      "Accessor"
      (documentation 'settings-gtk-cursor-blink-timeout 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-cursor-blink-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-blink-timeout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the time after which the cursor stops
    blinking, in seconds}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-blink-timeout} slot of the
    @class{gtk:settings} class.
  @end{short}
  Time after which the cursor stops blinking, in seconds. The timer is reset
  after each user interaction. Setting this to zero has the same effect as
  setting the @slot[gtk:settings]{gtk-cursor-blink} property to @em{false}.
  @see-class{gtk:settings}
  @see-function{gtk:settings-gtk-cursor-blink}")

;;; --- gtk:settings-gtk-cursor-theme-name -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-theme-name"
                                               'settings) t)
 "The @code{gtk-cursor-theme-name} property of type @code{:string}
  (Read / Write) @br{}
  The name of the cursor theme to use, or @code{nil} to use the default theme.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-theme-name)
      "Accessor"
      (documentation 'settings-gtk-cursor-theme-name 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-cursor-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the cursor theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the cursor theme to use, or @code{nil} to use the default theme.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-cursor-theme-size -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-cursor-theme-size"
                                               'settings) t)
 "The @code{gtk-cursor-theme-size} property of type @code{:int} (Read / Write)
  @br{}
  The size to use for cursors, or 0 to use the default size. @br{}
  Allowed values: [0,128] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-cursor-theme-size)
      "Accessor"
      (documentation 'settings-gtk-cursor-theme-size 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-cursor-theme-size object) => setting}
  @syntax{(setf (gtk:settings-gtk-cursor-theme-size object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the size to use for cursors}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-cursor-theme-size} slot of the
    @class{gtk:settings} class.
  @end{short}
  Size to use for cursors, or 0 to use the default size.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-decoration-layout -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-decoration-layout"
                                               'settings) t)
 "The @code{gtk-decoration-layout} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines which buttons should be put in the titlebar of
  client-side decorated windows, and whether they should be placed at the left
  or right. The format of the string is button names, separated by commas. A
  colon separates the buttons that should appear on the left from those on the
  right. Recognized button names are @code{minimize}, @code{maximize},
  @code{close}, @code{icon} (the window icon) and @code{menu} (a Menu button for
  the fallback application menu). For example,
  @code{\"menu:minimize,maximize,close\"} specifies a Menu button on the left,
  and Minimize, Maximize and Close buttons on the right. Note that buttons will
  only be shown when they are meaningful. E.g. a Menu button only appears when
  the desktop shell does not show the application menu, and a Close button only
  appears on a window that can be closed. Also note that the setting can be
  overridden with the @slot[gtk:header-bar]{decoration-layout} property of the
  header bar. @br{}
  Default value: @code{\"menu:minimize,maximize,close\"}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-decoration-layout)
      "Accessor"
      (documentation 'settings-gtk-decoration-layout 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-decoration-layout object) => setting}
  @syntax{(setf gtk:settings-gtk-decoration-layout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the settings of buttons}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-decoration-layout} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines which buttons should be put in the titlebar of
  client-side decorated windows, and whether they should be placed at the left
  of right.

  The format of the string is button names, separated by commas. A colon
  separates the buttons that should appear on the left from those on the right.
  Recognized button names are @code{minimize}, @code{maximize}, @code{close},
  @code{icon} (the window icon) and @code{menu} (a Menu button for the fallback
  application menu).

  For example, @code{\"menu:minimize,maximize,close\"} specifies a Menu button
  on the left, and Minimize, Maximize and Close buttons on the right.

  Note that buttons will only be shown when they are meaningful. E.g. a Menu
  button only appears when the desktop shell does not show the application menu,
  and a Close button only appears on a window that can be closed.

  Also note that the setting can be overridden with the
  @slot[gtk:header-bar]{decoration-layout} property of the header bar. See also
  the @slot[gtk:window]{decorated} property and the @fun{gtk:window-decorated}
  function.
  @see-class{gtk:settings}
  @see-class{gtk:header-bar}
  @see-function{gtk:header-bar-decoration-layout}
  @see-function{gtk:window-decorated}")

;;; --- gtk:settings-gtk-dialogs-use-header ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-dialogs-use-header"
                                               'settings) t)
 "The @code{gtk-dialogs-use-header} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether built-in GTK dialogs such as the file chooser, the color chooser or
  the font chooser will use a header bar at the top to show action widgets, or
  an action area at the bottom. This setting does not affect custom dialogs
  using the @class{gtk:dialog} widget directly, or message dialogs. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-dialogs-use-header)
      "Accessor"
      (documentation 'settings-gtk-dialogs-use-header 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-dialogs-use-header object) => setting}
  @syntax{(setf gtk:settings-gtk-dialogs-use-header object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether dialogs use a header bar}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-dialogs-use-header} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether built-in GTK dialogs such as the file chooser, the color chooser or
  the font chooser will use a header bar at the top to show action widgets, or
  an action area at the bottom. This setting does not affect custom dialogs
  using the @class{gtk:dialog} widget directly, or message dialogs.
  @see-class{gtk:settings}
  @see-class{gtk:dialog}")

;;; --- gtk:settings-gtk-dnd-drag-threshold ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-dnd-drag-threshold"
                                               'settings) t)
 "The @code{gtk-dnd-drag-threshold} property of type @code{:int} (Read / Write)
  @br{}
  The number of pixels the cursor can move before dragging. @br{}
  Allowed values: >= 1 @br{}
  Default value: 8")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-dnd-drag-threshold)
      "Accessor"
      (documentation 'settings-gtk-dnd-drag-threshold 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-dnd-drag-threshold object) => setting}
  @syntax{(setf gtk:settings-gtk-dnd-drag-threshold object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the number of pixels the cursor can move}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-dnd-drag-threshold} slot of the
    @class{gtk:settings} class.
  @end{short}
  Number of pixels the cursor can move before dragging.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-double-click-distance ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-double-click-distance"
                                               'settings) t)
 "The @code{gtk-double-click-distance} property of type @code{:int}
  (Read / Write) @br{}
  The maximum distance in pixels allowed between two clicks for them to be
  considered a double click. @br{}
  Allowed values: >= 0 @br{}
  Default value: 5")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-double-click-distance)
      "Accessor"
      (documentation 'settings-gtk-double-click-distance 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-double-click-distance object) => setting}
  @syntax{(setf gtk:settings-gtk-double-click-distance object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the maximum distance in pixels}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-double-click-distance} slot of the
    @class{gtk:settings} class.
  @end{short}
  Maximum distance in pixels allowed between two clicks for them to be
  considered a double click.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-double-click-time -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-double-click-time"
                                               'settings) t)
 "The @code{gtk-double-click-time} property of type @code{:int} (Read / Write)
  @br{}
  The maximum time allowed between two clicks for them to be considered a double
  click, in milliseconds. @br{}
  Allowed values: >= 0 @br{}
  Default value: 250")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-double-click-time)
      "Accessor"
      (documentation 'settings-gtk-double-click-time 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-double-click-time object) => setting}
  @syntax{(setf gtk:settings-gtk-double-click-time object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the maximum time allowed between two clicks}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-double-click-time} slot of the
    @class{gtk:settings} class.
  @end{short}
  Maximum time allowed between two clicks for them to be considered a double
  click, in milliseconds.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-accels -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-accels"
                                               'settings) t)
 "The @code{gtk-enable-accels} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether menu items should have visible accelerators which can be
  activated. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-accels)
      "Accessor"
      (documentation 'settings-gtk-enable-accels 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-accels object) => setting}
  @syntax{(setf gtk:settings-gtk-enable-accels object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether menu items should have visible
    accelerators}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-accels} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether menu items should have visible accelerators which can be
  activated.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-animations -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-animations"
                                               'settings) t)
 "The @code{gtk-enable-animations} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to enable toolkit-wide animations. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-animations)
      "Accessor"
      (documentation 'settings-gtk-enable-animations 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-animations object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-animations object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to enable animations}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-animations} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to enable toolkit-wide animations. The default value is @em{true}.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-event-sounds -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-event-sounds"
                                               'settings) t)
 "The @code{gtk-enable-event-sounds} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to play any event sounds at all. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-event-sounds)
      "Accessor"
      (documentation 'settings-gtk-enable-event-sounds 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-event-sounds object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-event-sounds object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to play any event sounds at all}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-event-sounds} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to play any event sounds at all. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-input-feedback-sounds --------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-input-feedback-sounds"
                                               'settings) t)
 "The @code{gtk-enable-input-feedback-sounds} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to play event sounds as feedback to user input. See the Sound Theme
  specification for more information on event sounds and sound themes. GTK
  itself does not support event sounds, you have to use a loadable module like
  the one that comes with the @code{libcanberra} library. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-input-feedback-sounds)
      "Accessor"
      (documentation 'settings-gtk-enable-input-feedback-sounds 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-input-feedback-sounds object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-input-feedback-sounds object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to play any event sounds at all}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-input-feedback-sounds} slot
    of the @class{gtk:settings} class.
  @end{short}
  Whether to play event sounds as feedback to user input. See the Sound Theme
  specification for more information on event sounds and sound themes. GTK
  itself does not support event sounds, you have to use a loadable module like
  the one that comes with the @code{libcanberra} library.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-mnemonics --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-mnemonics"
                                               'settings) t)
 "The @code{gtk-enable-mnemonics} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether labels and menu items should have visible mnemonics which can be
  activated. @br{}
  @em{Warning:} The @code{gtk-enable-mnemonics} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  can still be used for application overrides, but will be ignored in the
  future. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-mnemonics)
      "Accessor"
      (documentation 'settings-gtk-enable-mnemonics 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-mnemonics object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-mnemonics object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether labels and menu items should have
    visible mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-mnemonics} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether labels and menu items should have visible mnemonics which can be
  activated.
  @begin[Warning]{dictionary}
    The @code{gtk-enable-mnemonics} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting can still
    be used for application overrides, but will be ignored in the future.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-primary-paste ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-primary-paste"
                                               'settings) t)
 "The @code{gtk-enable-primary-paste} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a middle click on a mouse should paste the \"PRIMARY\" clipboard
  content at the cursor location. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-primary-paste)
      "Accessor"
      (documentation 'settings-gtk-enable-primary-paste 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-primary-paste object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-primary-paste object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether a middle click should paste the
    \"PRIMARY\" clipboard}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-primary-paste} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether a middle click on a mouse should paste the \"PRIMARY\" clipboard
  content at the cursor location.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-enable-tooltips ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-enable-tooltips"
                                               'settings) t)
 "The @code{gtk-enable-tooltips} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether tooltips should be shown on widgets. @br{}
  @em{Warning:} The @code{gtk-enable-tooltips} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-enable-tooltips)
      "Accessor"
      (documentation 'settings-gtk-enable-tooltips 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-enable-tooltips object) => setting}
  @syntax{(setf (gtk:settings-gtk-enable-tooltips object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether tooltips should be shown}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-enable-tooltips} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether tooltips should be shown on widgets.
  @begin[Warning]{dictionary}
    The @code{gtk-enable-tooltips} property has been deprecated since version
     3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-entry-password-hint-timeout ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-entry-password-hint-timeout"
                                               'settings) t)
 "The @code{gtk-entry-password-hint-timeout} property of type @code{:uint}
  (Read / Write) @br{}
  How long to show the last input character in hidden entries. This value is
  in milliseconds. The value 0 disables showing the last char. The value 600 is
  a good value for enabling it. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-entry-password-hint-timeout)
      "Accessor"
      (documentation 'settings-gtk-entry-password-hint-timeout 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-entry-password-hint-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-entry-password-hint-timeout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an unsigned integer for how long to show the last input}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-entry-password-hint-timeout} slot
    of the @class{gtk:settings} class.
  @end{short}
  How long to show the last input character in hidden entries. This value is
  in milliseconds. The value 0 disables showing the last char. The value 600 is
  a good value for enabling it.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-entry-select-on-focus ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-entry-select-on-focus"
                                               'settings) t)
 "The @code{gtk-entry-select-on-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to select the contents of an entry when it is focused. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-entry-select-on-focus)
      "Accessor"
      (documentation 'settings-gtk-entry-select-on-focus 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-entry-select-on-focus object) => setting}
  @syntax{(setf (gtk:settings-gtk-entry-select-on-focus object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to select the contents of an entry}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-entry-select-on-focus} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to select the contents of an entry when it is focused.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-error-bell --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-error-bell"
                                               'settings) t)
 "The @code{gtk-error-bell} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, keyboard navigation and other input-related errors will cause
  a beep. Since the error bell is implemented using the @fun{gdk:window-beep}
  function, the windowing system may offer ways to configure the error bell in
  many ways, such as flashing the window or similar visual effects. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-error-bell)
      "Accessor"
      (documentation 'settings-gtk-error-bell 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-error-bell object) => setting}
  @syntax{(setf (gtk:settings-gtk-error-bell object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether errors well cause a beep}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-error-bell} slot of the
    @class{gtk:settings} class.
  @end{short}
  When @em{true}, keyboard navigation and other input-related errors will cause
  a beep. Since the error bell is implemented using the @fun{gdk:window-beep}
  function, the windowing system may offer ways to configure the error bell in
  many ways, such as flashing the window or similar visual effects.
  @see-class{gtk:settings}
  @see-function{gdk:window-beep}")

;;; --- gtk:settings-gtk-fallback-icon-theme -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-fallback-icon-theme"
                                               'settings) t)
 "The @code{gtk-fallback-icon-theme} property of type @code{:string}
  (Read / Write) @br{}
  The name of an icon theme to fall back to. @br{}
  @em{Warning:} The @code{gtk-fallback-icon-theme} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-fallback-icon-theme)
      "Accessor"
      (documentation 'settings-gtk-fallback-icon-theme 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-fallback-icon-theme object) => setting}
  @syntax{(setf (gtk:settings-gtk-fallback-icon-theme object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the name of an icon theme}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-fallback-icon-theme} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of an icon theme to fall back to.
  @begin[Warning]{dictionary}
    The @code{gtk-fallback-icon-theme} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting
    is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-file-chooser-backend ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-file-chooser-backend"
                                               'settings) t)
 "The @code{gtk-file-chooser-backend} property of type @code{:string}
  (Read / Write) @br{}
  The name of the backend to use by default for the @class{gtk:file-chooser}
  widget. @br{}
  @em{Warning:} The @code{gtk-file-chooser-backend} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. The @class{gtk:file-chooser} widget uses GIO by default. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-file-chooser-backend)
      "Accessor"
      (documentation 'settings-gtk-file-chooser-backend 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-file-chooser-backend object) => setting}
  @syntax{(setf (gtk:settings-gtk-file-chooser-backend object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the backend to use by default}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-file-chooser-backend} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the backend to use by default for the @class{gtk:file-chooser} widget.
  @begin[Warning]{dictionary}
    The @code{gtk-file-chooser-backend} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting
    is ignored. The @class{gtk:file-chooser} widget uses GIO by default.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-class{gtk:file-chooser}")

;;; --- gtk:settings-gtk-font-name ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-font-name" 'settings) t)
 "The @code{gtk-font-name} property of type @code{:string} (Read / Write) @br{}
  The name of default font to use.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-font-name)
      "Accessor"
      (documentation 'settings-gtk-font-name 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-font-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-font-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the name of the default font to use}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-font-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the default font to use.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-fontconfig-timestamp ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-fontconfig-timestamp"
                                               'settings) t)
 "The @code{gtk-fontconfig-timestamp} property of type @code{:uint}
  (Read / Write) @br{}
  The timestamp of the current fontconfig configuration. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-fontconfig-timestamp)
      "Accessor"
      (documentation 'settings-gtk-fontconfig-timestamp 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-fontconfig-timestamp object) => setting}
  @syntax{(setf (gtk:settings-gtk-fontconfig-timestamp object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an unsigned integer for the timestamp}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-fontconfig-timestamp} slot of the
    @class{gtk:settings} class.
  @end{short}
  Timestamp of the current fontconfig configuration.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-icon-sizes --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-icon-sizes"
                                               'settings) t)
 "The @code{gtk-icon-sizes} property of type @code{:string} (Read / Write) @br{}
  The list of icon sizes, for example,
  @code{\"gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48\"}. @br{}
  @em{Warning:} The @code{gtk-icon-sizes} property has been deprecated since
  version 3.10 and should not be used in newly written code. This setting is
  ignored. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-icon-sizes)
      "Accessor"
      (documentation 'settings-gtk-icon-sizes 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-icon-sizes object) => setting}
  @syntax{(setf (gtk:settings-gtk-icon-sizes object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for a list of icon sizes}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-icon-sizes} slot of the
    @class{gtk:settings} class.
  @end{short}
  A list of icon sizes, e.g.
  @code{\"gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48\"}.
  @begin[Warning]{dictionary}
    The @code{gtk-icon-sizes} property has been deprecated since version 3.10
    and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-icon-theme-name ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-icon-theme-name"
                                               'settings) t)
 "The @code{gtk-icon-theme-name} property of type @code{:string} (Read / Write)
  @br{}
  The name of the icon theme to use.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-icon-theme-name)
      "Accessor"
      (documentation 'settings-gtk-icon-theme-name 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-icon-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-icon-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the icon theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-icon-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the icon theme to use.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-im-module ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-im-module" 'settings) t)
 "The @code{gtk-im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used by default. This is the input
  method that will be used if the user has not explicitly chosen another input
  method from the IM context menu. This also can be a colon-separated list of
  input methods, which GTK will try in turn until it finds one available on
  the system. See the @class{gtk:im-context} class and the
  @slot[gtk:settings]{gtk-show-input-method-menu} setting. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-im-module)
      "Accessor"
      (documentation 'settings-gtk-im-module 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-im-module object) => setting}
  @syntax{(setf (gtk:settings-gtk-im-module object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the IM module}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-im-module} slot of the
    @class{gtk:settings} class.
  @end{short}
  Which IM (input method) module should be used by default. This is the input
  method that will be used if the user has not explicitly chosen another input
  method from the IM context menu. This also can be a colon-separated list of
  input methods, which GTK will try in turn until it finds one available on
  the system. See the @class{gtk:im-context} class and the
  @slot[gtk:settings]{gtk-show-input-method-menu} setting.
  @see-class{gtk:settings}
  @see-class{gtk:im-context}
  @see-function{gtk:settings-gtk-show-input-method-menu}")

;;; --- gtk:settings-gtk-im-preedit-style --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-im-preedit-style"
                                               'settings) t)
 "The @code{gtk-im-preedit-style} property of type @sym{gtk:im-preedit-style}
  (Read / Write) @br{}
  How to draw the input method preedit string. @br{}
  @em{Warning:} The @code{gtk-im-preedit-style} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @code{:callback}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-im-preedit-style)
      "Accessor"
      (documentation 'settings-gtk-im-preedit-style 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-im-preedit-style object) => setting}
  @syntax{(setf (gtk:settings-gtk-im-preedit-style object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a @sym{gtk:im-preedit-style} value}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-im-preedit-style} slot of the
    @class{gtk:settings} class.
  @end{short}
  How to draw the input method preedit string.
  @begin[Warning]{dictionary}
    The @fun{gtk:settings-gtk-im-preedit-style} function has been deprecated
    since version 3.10 and should not be used in newly written code. This
    setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-symbol{gtk:im-preedit-style}")

;;; --- gtk:settings-gtk-im-status-style ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-im-status-style"
                                               'settings) t)
 "The @code{gtk-im-status-style} property of type @sym{gtk-im-status-style}
  (Read / Write) @br{}
  How to draw the input method statusbar. @br{}
  @em{Warning:} The @code{gtk-im-status-style} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @code{:callback}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-im-status-style)
      "Accessor"
      (documentation 'settings-gtk-im-status-style 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-im-status-style object) => setting}
  @syntax{(setf (gtk:settings-gtk-im-status-style object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a @sym{gtk:im-status-style} value}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-im-status-style} slot of the
    @class{gtk:settings} class.
  @end{short}
  How to draw the input method statusbar.
  @begin[Warning]{dictionary}
    The @fun{gtk:settings-gtk-im-status-style} function has been deprecated
    since version 3.10 and should not be used in newly written code. This
    setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-symbol{gtk:im-status-style}")

;;; --- gtk:settings-gtk-key-theme-name ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-key-theme-name"
                                               'settings) t)
 "The @code{gtk-key-theme-name} property of type @code{:string} (Read / Write)
  @br{}
  The name of the key theme to load.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-key-theme-name)
      "Accessor"
      (documentation 'settings-gtk-key-theme-name 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-key-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-key-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the key theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-key-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the key theme to load.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-keynav-cursor-only ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-keynav-cursor-only"
                                               'settings) t)
 "The @code{gtk-keynav-cursor-only} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, keyboard navigation should be able to reach all widgets by
  using the cursor keys only. the @kbd{Tab}, @kbd{Shift} etc. keys cannot be
  expected to be present on the used input device. @br{}
  @em{Warning:} The @code{gtk-keynav-cursor-only} property has been deprecated
  since version 3.10 and should not be used in newly written code. Generally,
  the behavior for touchscreen input should be performed dynamically based on
  the @fun{gdk:event-source-device} function. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-keynav-cursor-only)
      "Accessor"
      (documentation 'settings-gtk-keynav-cursor-only 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-keynav-cursor-only object) => setting}
  @syntax{(setf (gtk:settings-gtk-keynav-cursor-only object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether using the cursor keys only}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-keynav-cursor-only} slot of the
    @class{gtk:settings} class.
  @end{short}
  When @em{true}, keyboard navigation should be able to reach all widgets by
  using the cursor keys only. The @kbd{Tab}, @kbd{Shift} etc. keys cannot be
  expected to be present on the used input device.
  @begin[Warning]{dictionary}
    The @fun{gtk:settings-gtk-keynav-cursor-only} function has been deprecated
    since version 3.10 and should not be used in newly written code. Generally,
    the behavior for touchscreen input should be performed dynamically based on
    the @fun{gdk:event-source-device} function.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-function{gdk:event-source-device}")

;;; --- gtk:settings-gtk-keynav-use-caret --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-keynav-use-caret"
                                               'settings) t)
 "The @code{gtk-keynav-use-caret} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether GTK should make sure that text can be navigated with a caret, even
  if it is not editable. This is useful when using a screen reader. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-keynav-use-caret)
      "Accessor"
      (documentation 'settings-gtk-keynav-use-caret 'function)
 "@version{2023-03-13}
  @syntax{(gtk:settings-gtk-keynav-use-caret object) => setting}
  @syntax{(setf (gtk:settings-gtk-keynav-use-caret object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether GTK should make sure that text can be
    navigated with a caret}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-keynav-use-caret} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether GTK should make sure that text can be navigated with a caret, even
  if it is not editable. This is useful when using a screen reader.
  @see-class{gtk:settings}")

;;;  --- gtk:settings-gtk-keynav-wrap-around -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-keynav-wrap-around"
                                               'settings) t)
 "The @code{gtk-keynav-wrap-around} property of type @code{:boolean}
  (Read / Write) @br{}
  When @em{true}, some widgets will wrap around when doing keyboard
  navigation, such as menus, menubars and notebooks. @br{}
  @em{Warning:} The @code{gtk-keynav-wrap-around} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-keynav-wrap-around)
      "Accessor"
      (documentation 'settings-gtk-keynav-wrap-around 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-keynav-wrap-around object) => setting}
  @syntax{(setf (gtk:settings-gtk-keynav-wrap-around object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether widgets will wrap around when doing
    keyboard navigation}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-keynav-wrap-around} slot of the
    @class{gtk:settings} class.
  @end{short}
  When @em{true}, some widgets will wrap around when doing keyboard
  navigation, such as menus, menubars and notebooks.
  @begin[Warning]{dictionary}
    The @code{gtk-keynav-wrap-around} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-label-select-on-focus ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-label-select-on-focus"
                                               'settings) t)
 "The @code{gtk-label-select-on-focus} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to select the contents of a selectable label when it is focused. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-label-select-on-focus)
      "Accessor"
      (documentation 'settings-gtk-label-select-on-focus 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-label-select-on-focus object) => setting}
  @syntax{(setf (gtk:settings-gtk-label-select-on-focus object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to select the contents of a selectable
    label}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-label-select-on-focus} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to select the contents of a selectable label when it is focused.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-long-press-time ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-long-press-time"
                                               'settings) t)
 "The @code{gtk-long-press-time} property of type @code{:uint} (Read / Write)
  @br{}
  The time for a button or touch press to be considered a \"long press\". @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 50")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-long-press-time)
      "Accessor"
      (documentation 'settings-gtk-long-press-time 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-long-press-time object) => setting}
  @syntax{(setf (gtk:settings-gtk-long-press-time object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an unsigned integer for the time for a button or touch
    press to be considered a \"long press\"}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-long-press-time} slot of the
    @class{gtk:settings} class.
  @end{short}
  The time for a button or touch press to be considered a \"long press\".
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-menu-bar-accel ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-menu-bar-accel"
                                               'settings) t)
 "The @code{gtk-menu-bar-accel} property of type @code{:string} (Read / Write)
  @br{}
  The keybinding to activate the menu bar. @br{}
  @em{Warning:} The @code{gtk-menu-bar-accel} property has been deprecated since
  version 3.10 and should not be used in newly written code. This setting can
  still be used for application overrides, but will be ignored in the future.
  @br{}
  Default value: \"F10\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-menu-bar-accel)
      "Accessor"
      (documentation 'settings-gtk-menu-bar-accel 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-menu-bar-accel object) => setting}
  @syntax{(setf (gtk:settings-gtk-menu-bar-accel object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the keybinding to activate the menu bar}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-menu-bar-accel} slot of the
    @class{gtk:settings} class.
  @end{short}
  Keybinding to activate the menu bar.
  @begin[Warning]{dictionary}
    The @code{gtk-menu-bar-accel} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting can
    still be used for application overrides, but will be ignored in the future.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-menu-bar-popup-delay ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-menu-bar-popup-delay"
                                               'settings) t)
 "The @code{gtk-menu-bar-popup-delay} property of type @code{:int}
  (Read / Write) @br{}
  The delay before the submenus of a menu bar appear. @br{}
  @em{Warning:} The @code{gtk-menu-bar-popup-delay} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-menu-bar-popup-delay)
      "Accessor"
      (documentation 'settings-gtk-menu-bar-popup-delay 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-menu-bar-popup-delay object) => setting}
  @syntax{(setf (gtk:settings-gtk-menu-bar-popup-delay object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the delay before the submenus of a menu
    bar appear}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-menu-bar-popup-delay} slot of the
    @class{gtk:settings} class.
  @end{short}
  Delay before the submenus of a menu bar appear.
  @begin[Warning]{dictionary}
    The @code{gtk-menu-bar-popup-delay} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting
    is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-menu-images -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-menu-images"
                                               'settings) t)
 "The @code{gtk-menu-images} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether images should be shown in menus. @br{}
  @em{Warning:} The @code{gtk-menu-images} setting has been deprecated since
  version 3.10 and should not be used in newly written code. This setting is
  deprecated. Application developers control whether or not a menu item should
  have an icon or not, on a per widget basis. Either use a @class{gtk:menu-item}
  widget with a @class{gtk:box} widget containing a @class{gtk:image} widget and
  a @class{gtk:accel-label} widget, or describe your menus using a
  @class{g:menu} XML description. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-menu-images)
      "Accessor"
      (documentation 'settings-gtk-menu-images 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-menu-images object) => setting}
  @syntax{(setf (gtk:settings-gtk-menu-images object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether images should be shown in menus}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-menu-images} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether images should be shown in menus.
  @begin[Warning]{dictionary}
    The @code{gtk-menu-images} setting has been deprecated since version 3.10
    and should not be used in newly written code. This setting is deprecated.
    Application developers control whether or not a menu item should have an
    icon or not, on a per widget basis. Either use a @class{gtk:menu-item}
    widget with a @class{gtk:box} widget containing a @class{gtk:image} widget
    and a @class{gtk:accel-label} widget, or describe your menus using a
    @class{g:menu} XML description.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-class{gtk:menu-item}
  @see-class{g:menu}")

;;; --- gtk:settings-gtk-menu-popdown-delay ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-menu-popdown-delay"
                                               'settings) t)
 "The @code{gtk-menu-popdown-delay} property of type @code{:int} (Read / Write)
  @br{}
  The time before hiding a submenu when the pointer is moving towards the
  submenu. @br{}
  @em{Warning:} The @code{gtk-menu-popdown-delay} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1000")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-menu-popdown-delay)
      "Accessor"
      (documentation 'settings-gtk-menu-popdown-delay 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-menu-popdown-delay object) => setting}
  @syntax{(setf (gtk:settings-gtk-menu-popdown-delay object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the time before hiding a submenu}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-menu-popdown-delay} slot of the
    @class{gtk:settings} class.
  @end{short}
  The time before hiding a submenu when the pointer is moving towards the
  submenu.
  @begin[Warning]{dictionary}
    The @code{gtk-menu-popdown-delay} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting
    is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-menu-popup-delay --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-menu-popup-delay"
                                               'settings) t)
 "The @code{gtk-menu-popup-delay} property of type @code{:int} (Read / Write)
  @br{}
  The minimum time the pointer must stay over a menu item before the submenu
  appear. @br{}
  @em{Warning:} The @code{gtk-menu-popup-delay} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 225")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-menu-popup-delay)
      "Accessor"
      (documentation 'settings-gtk-menu-popup-delay 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-menu-popup-delay object) => setting}
  @syntax{(setf (gtk:settings-gtk-menu-popup-delay object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the minimum time the pointer must stay
    over a menu item before the submenu appear}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-menu-popup-delay} slot of the
    @class{gtk:settings} class.
  @end{short}
  Minimum time the pointer must stay over a menu item before the submenu
  appear.
  @begin[Warning]{dictionary}
    The @code{gtk-menu-popup-delay} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-modules -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-modules" 'settings) t)
 "The @code{gtk-modules} property of type @code{:string} (Read / Write) @br{}
  The list of currently active GTK modules. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-modules)
      "Accessor"
      (documentation 'settings-gtk-modules 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-modules object) => setting}
  @syntax{(setf (gtk:settings-gtk-modules object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for a list of currently active GTK modules}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-modules} slot of the
    @class{gtk:settings} class.
  @end{short}
  List of currently active GTK modules.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-overlay-scrolling -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-overlay-scrolling"
                                               'settings) t)
 "The @code{gtk-overlay-scrolling} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether scrolled windows may use overlayed scrolling indicators. If this is
  set to @em{false}, scrolled windows will have permanent scrollbars. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-overlay-scrolling)
      "Accessor"
      (documentation 'settings-gtk-overlay-scrolling 'function)
 "@version{2023-03-13}
  @syntax{(gtk:settings-gtk-overlay-scrolling object) => setting}
  @syntax{(setf (gtk:settings-gtk-overlay-scrolling object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether scrolled windows may use overlayed
    scrolled indicators}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-overlay-scrolling} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether scrolled windows may use overlayed scrolling indicators. If this is
  set to @em{false}, scrolled windows will have permanent scrollbars.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-primary-button-warps-slider ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-primary-button-warps-slider"
                                               'settings) t)
 "The @code{gtk-primary-button-warps-slider} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a click in a @class{gtk:range} widget trough should scroll to the
  click position or scroll by a single page in the respective direction. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-primary-button-warps-slider)
      "Accessor"
      (documentation 'settings-gtk-primary-button-warps-slider 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-primary-button-warps-slider object) => setting}
  @syntax{(setf (gtk:settings-gtk-primary-button-warps-slider object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether a click should scroll to the click
    position}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-primary-buton-warps-slider} slot of
    the @class{gtk:settings} class.
  @end{short}
  Whether a click in a @class{gtk:range} widget trough should scroll to the
  click position or scroll by a single page in the respective direction.
  @see-class{gtk:settings}
  @see-class{gtk:range}")

;;; --- gtk:settings-gtk-print-backends ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-print-backends"
                                               'settings) t)
 "The @code{gtk-print-backends} property of type @code{:string} (Read / Write)
  @br{}
  The comma-separated list of print backends to use in the print dialog.
  Available print backends depend on the GTK installation, and may include
  @code{\"file\"}, @code{\"cups\"}, @code{\"lpr\"} or @code{\"papi\"}. @br{}
  Default value: @code{\"file,cups\"}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-print-backends)
      "Accessor"
      (documentation 'settings-gtk-print-backends 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-print-backends object) => setting}
  @syntax{(setf (gtk:settings-gtk-print-backends object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for a list of print backends}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-print-backends} slot of the
    @class{gtk:settings} class.
  @end{short}
  A comma-separated list of print backends to use in the print dialog.
  Available print backends depend on the GTK installation, and may include
  @code{\"file\"}, @code{\"cups\"}, @code{\"lpr\"} or @code{\"papi\"}.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-print-preview-command ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-print-preview-command"
                                               'settings) t)
 "The @code{gtk-print-preview-command} property of type @code{:string}
  (Read / Write) @br{}
  The command to run for displaying the print preview. The command should
  contain a @code{f} placeholder, which will get replaced by the path to the
  PDF file. The command may also contain a @code{s} placeholder, which will get
  replaced by the path to a file containing the print settings in the format
  produced by the @fun{gtk:print-settings-to-file} function. The preview
  application is responsible for removing the PDF file and the print settings
  file when it is done. @br{}
  Default value:
  @code{\"evince --unlink-tempfile --preview --print-settings %s %f\"}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-print-preview-command)
      "Accessor"
      (documentation 'settings-gtk-print-preview-command 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-print-preview-command object) => setting}
  @syntax{(setf (gtk:settings-gtk-print-preview-command object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for a command to run for displaying the print
    preview}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-print-preview-command} slot of the
    @class{gtk:settings} class.
  @end{short}
  A command to run for displaying the print preview. The command should
  contain a @code{f} placeholder, which will get replaced by the path to the
  PDF file. The command may also contain a @code{s} placeholder, which will get
  replaced by the path to a file containing the print settings in the format
  produced by the @fun{gtk:print-settings-to-file} function. The preview
  application is responsible for removing the PDF file and the print settings
  file when it is done.
  @see-class{gtk:settings}
  @see-function{gtk:print-settings-to-file}")

;;; --- gtk:settings-gtk-recent-files-enabled ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-recent-files-enabled"
                                               'settings) t)
 "The @code{gtk-recent-files-enabled} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether GTK should keep track of items inside the recently used resources
  list. If set to @em{false}, the list will always be empty. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-recent-files-enabled)
      "Accessor"
      (documentation 'settings-gtk-recent-files-enabled 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-recent-files-enabled object) => setting}
  @syntax{(setf (gtk:settings-gtk-recent-files-enabled object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether GTK should keep track of items inside
    the recently used resources list}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-recent-files-enabled} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether GTK should keep track of items inside the recently used resources
  list. If set to @em{false}, the list will always be empty.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-recent-files-limit ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-recent-files-limit"
                                               'settings) t)
 "The @code{gtk-recent-files-limit} property of type @code{:int} (Read / Write)
  @br{}
  The number of recently used files that should be displayed by default by
  @class{gtk:recent-chooser} implementations and by @class{gtk:file-chooser}
  widgets. A value of -1 means every recently used file stored. @br{}
  @em{Warning:} The @code{gtk-recent-files-limit} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 50 @br{}
  Default value: 50")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-recent-files-limit)
      "Accessor"
      (documentation 'settings-gtk-recent-files-limit 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-recent-files-limit object) => setting}
  @syntax{(setf (gtk:settings-gtk-recent-files-limit object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the number of recently used files}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-recent-files-limit} slot of the
    @class{gtk:settings} class.
  @end{short}
  The number of recently used files that should be displayed by default by
  @class{gtk:recent-chooser} implementations and by @class{gtk:file-chooser}
  widgets. A value of -1 means every recently used file stored.
  @begin[Warning]{dictionary}
    The @code{gtk-recent-files-limit} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-class{gtk:recent-chooser}
  @see-class{gtk:file-chooser}")

;;; --- gtk:settings-gtk-recent-files-max-age ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-recent-files-max-age"
                                               'settings) t)
 "The @code{gtk-recent-files-max-age} property of type @code{:int}
  (Read / Write) @br{}
  The maximum age, in days, of the items inside the recently used resources
  list. Items older than this setting will be excised from the list. If set to
  0, the list will always be empty; if set to -1, no item will be
  removed. @br{}
  Allowed values: >= 30 @br{}
  Default value: 30")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-recent-files-max-age)
      "Accessor"
      (documentation 'settings-gtk-recent-files-max-age 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-recent-files-max-age object) => setting}
  @syntax{(setf (gtk:settings-gtk-recent-files-max-age object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the maximum age, in days}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-recent-files-max-page} slot of the
    @class{gtk:settings} class.
  @end{short}
  The maximum age, in days, of the items inside the recently used resources
  list. Items older than this setting will be excised from the list. If set to
  0, the list will always be empty, if set to -1, no item will be
  removed.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-scrolled-window-placement -----------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-scrolled-window-placement"
                                               'settings) t)
 "The @code{gtk-scrolled-window-placement} property of type
  @sym{gtk:corner-type} (Read / Write) @br{}
  Where the contents of scrolled windows are located with respect to the
  scrollbars, if not overridden by the scrolled window's own placement. @br{}
  @em{Warning:} The @code{gtk-scrolled-window-placement} property has been
  deprecated since version 3.10 and should not be used in newly written code.
  This setting is ignored. @br{}
  Default value: @val[gtk:corner-type]{:top-left}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-scrolled-window-placement)
      "Accessor"
      (documentation 'settings-gtk-scrolled-window-placement 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-scrolled-window-placement object) => setting}
  @syntax{(setf (gtk:settings-gtk-scrolled-window-placement object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a @sym{gtk:corner-type} value}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-scrolled-window-placement} slot of
    the @class{gtk:settings} class.
  @end{short}
  Where the contents of scrolled windows are located with respect to the
  scrollbars, if not overridden by the scrolled window's own placement.
  @begin[Warning]{dictionary}
    The @code{gtk-scrolled-window-placement} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting is
    ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-symbol{gtk:corner-type}")

;;; --- gtk:settings-gtk-shell-shows-app-menu ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-shell-shows-app-menu"
                                               'settings) t)
 "The @code{gtk-shell-shows-app-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the application
  menu, @em{false} if the application should display it itself. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-shell-shows-app-menu)
      "Accessor"
      (documentation 'settings-gtk-shell-shows-app-menu 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-shell-shows-app-menu object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-app-menu object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the environment is displaying the
    application menu}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-app-menu} slot of the
    @class{gtk:settings} class.
  @end{short}
  Set to @em{true} if the desktop environment is displaying the application
  menu, @em{false} if the application should display it itself.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-shell-shows-desktop -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-shell-shows-desktop"
                                               'settings) t)
 "The @code{gtk-shell-shows-desktop} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the desktop folder,
  @em{false} if not. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-shell-shows-desktop)
      "Accessor"
      (documentation 'settings-gtk-shell-shows-desktop 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-shell-shows-desktop object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-desktop object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the environment is displaying the
    desktop folder}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-desktop} slot of the
    @class{gtk:settings} class.
  @end{short}
  Set to @em{true} if the desktop environment is displaying the desktop folder,
  @em{false} if not.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-shell-shows-menubar -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-shell-shows-menubar"
                                               'settings) t)
 "The @code{gtk-shell-shows-menubar} property of type @code{:boolean}
  (Read / Write) @br{}
  Set to @em{true} if the desktop environment is displaying the menubar,
  @em{false} if the application should display it itself. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-shell-shows-menubar)
      "Accessor"
      (documentation 'settings-gtk-shell-shows-menubar 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-shell-shows-menubar object) => setting}
  @syntax{(setf (gtk:settings-gtk-shell-shows-menubar object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the environment is displaying the
    menubar}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-shell-shows-menubar} slot of the
    @class{gtk:settings} class.
  @end{short}
  Set to @em{true} if the desktop environment is displaying the menubar,
  @em{false} if the application should display it itself.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-show-input-method-menu --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-show-input-method-menu"
                                               'settings) t)
 "The @code{gtk-show-input-method-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the context menus of entries and text views should offer to change
  the input method. @br{}
  @em{Warning:} The @code{gtk-show-input-method-menu} property has been
  deprecated since version 3.10 and should not be used in newly written code.
  This setting is ignored. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-show-input-method-menu)
      "Accessor"
      (documentation 'settings-gtk-show-input-method-menu 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-show-input-method-menu object) => setting}
  @syntax{(setf (gtk:settings-gtk-show-input-method-menu object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether context menus should offer to change
    the input method}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-show-input-method-menu} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether the context menus of entries and text views should offer to change
  the input method.
  @begin[Warning]{dictionary}
    The @code{gtk-show-input-method-menu} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting is
    ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-show-unicode-menu -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-show-unicode-menu"
                                               'settings) t)
 "The @code{gtk-show-unicode-menu} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the context menus of entries and text views should offer to insert
  control characters. @br{}
  @em{Warning:} The @code{gtk-show-unicode-menu} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-show-unicode-menu)
      "Accessor"
      (documentation 'settings-gtk-show-unicode-menu 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-show-unicode-menu object) => setting}
  @syntax{(setf (gtk:settings-gtk-show-unicode-menu object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether the context menus should offer to
    insert control characters}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-show-unicode-menu} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether the context menus of entries and text views should offer to insert
  control characters.
  @begin[Warning]{dictionary}
    The @code{gtk-show-unicode-menu} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-sound-theme-name --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-sound-theme-name"
                                               'settings) t)
 "The @code{gtk-sound-theme-name} property of type @code{:string} (Read / Write)
  @br{}
  The XDG sound theme to use for event sounds. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-sound-theme-name)
      "Accessor"
      (documentation 'settings-gtk-sound-theme-name 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-sound-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-sound-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the sound theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-sound-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  The XDG sound theme to use for event sounds. See the Sound Theme specification
  for more information on event sounds and sound themes. GTK itself does not
  support event sounds, you have to use a loadable module like the one that
  comes with the @code{libcanberra} library.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-split-cursor ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-split-cursor" 'settings) t)
 "The @code{gtk-split-cursor} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether two cursors should be displayed for mixed left-to-right and
  right-to-left text. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-split-cursor)
      "Accessor"
      (documentation 'settings-gtk-split-cursor 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-split-cursor object) => setting}
  @syntax{(setf (gtk:settings-gtk-split-cursor object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether two cursors should be displayed for
    mixed left-to-right and right-to-left text}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-split-cursor} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether two cursors should be displayed for mixed left-to-right and
  right-to-left text.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-theme-name --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-theme-name" 'settings) t)
 "The @code{gtk-theme-name} property of type @code{:string} (Read / Write) @br{}
  Name of the theme to load.")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-theme-name)
      "Accessor"
      (documentation 'settings-gtk-theme-name 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-theme-name object) => setting}
  @syntax{(setf (gtk:settings-gtk-theme-name object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the theme name}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-theme-name} slot of the
    @class{gtk:settings} class.
  @end{short}
  Name of the theme to load.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-timeout-expand ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-timeout-expand"
                                               'settings) t)
 "The @code{gtk-timeout-expand} property of type @code{:int} (Read / Write)
  @br{}
  The expand value for timeouts, when a widget is expanding a new region. @br{}
  @em{Warning:} The @code{gtk-timeout-expand} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 500")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-timeout-expand)
      "Accessor"
      (documentation 'settings-gtk-timeout-expand 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-timeout-expand object) => setting}
  @syntax{(setf (gtk:settings-gtk-timeout-expand object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the expand value for timeouts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-timeout-expand} slot of the
    @class{gtk:settings} class.
  @end{short}
  Expand value for timeouts, when a widget is expanding a new region.
  @begin[Warning]{dictionary}
    The @code{gtk-timeout-expand} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-timeout-initial ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-timeout-initial"
                                               'settings) t)
 "The @code{gtk-timeout-initial} property of type @code{:int} (Read / Write)
  @br{}
  The starting value for timeouts, when button is pressed. @br{}
  @em{Warning:} The @code{gtk-timeout-initial} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 200")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-timeout-initial)
      "Accessor"
      (documentation 'settings-gtk-timeout-initial 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-timeout-initial object) => setting}
  @syntax{(setf (gtk:settings-gtk-timeout-initial object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the starting value for timeouts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-timeout-initial} slot of the
    @class{gtk:settings} class.
  @end{short}
  Starting value for timeouts, when button is pressed.
  @begin[Warning]{dictionary}
    The @code{gtk-timeout-initial} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-timeout-repeat ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-timeout-repeat"
                                               'settings) t)
 "The @code{gtk-timeout-repeat} property of type @code{:int} (Read / Write)
  @br{}
  The repeat value for timeouts, when button is pressed. @br{}
  @em{Warning:} The @code{gtk-timeout-repeat} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 20")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-timeout-repeat)
      "Accessor"
      (documentation 'settings-gtk-timeout-repeat 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-timeout-repeat object) => setting}
  @syntax{(setf (gtk:settings-gtk-timeout-repeat object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the repeat value of timeouts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-timeout-repeat} slot of the
    @class{gtk:settings} class.
  @end{short}
  Repeat value for timeouts, when button is pressed.
  @begin[Warning]{dictionary}
    The @code{gtk-timeout-repeat} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-titlebar-double-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-titlebar-double-click"
                                               'settings) t)
 "The @code{gtk-titlebar-double-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a double click occures on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"toggle-maximize\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-titlebar-double-click )
      "Accessor"
      (documentation 'settings-gtk-titlebar-double-click 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-titlebar-double-click object) => setting}
  @syntax{(setf (gtk:settings-gtk-titlebar-double-click object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for an action}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-titlebar-double-click} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines the action to take when a double click occures on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\".
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-titlebar-middle-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-titlebar-middle-click"
                                               'settings) t)
 "The @code{gtk-titlebar-middle-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a middle-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"none\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-titlebar-middle-click)
      "Accessor"
      (documentation 'settings-gtk-titlebar-middle-click 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-titlebar-middle-click object) => setting}
  @syntax{(setf (gtk:settings-gtk-titlebar-middle-click object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for an action}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-titlebar-middle-click} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines the action to take when a middle-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-titlebar-right-click ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-titlebar-right-click"
                                               'settings) t)
 "The @code{gtk-titlebar-right-click} property of type @code{:string}
  (Read / Write) @br{}
  This setting determines the action to take when a right-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\". @br{}
  Default value: \"menu\"")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-titlebar-right-click)
      "Accessor"
      (documentation 'settings-gtk-titlebar-right-click 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-titlebar-right-click object) => setting}
  @syntax{(setf (gtk:settings-gtk-titlebar-right-click object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for an action}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-titlebar-right-click} slot of the
    @class{gtk:settings} class.
  @end{short}
  This setting determines the action to take when a right-click occurs on the
  titlebar of client-side decorated windows. Recognized actions are
  \"minimize\", \"toggle-maximize\", \"menu\", \"lower\" or \"none\".
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-toolbar-icon-size -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-toolbar-icon-size"
                                               'settings) t)
 "The @code{gtk-toolbar-icon-size} property of type @sym{gtk:icon-size}
  (Read / Write) @br{}
  The size of icons in default toolbars. @br{}
  @em{Warning:} The @code{gtk-toolbar-icon-size} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Default value: @val[gtk:icon-size]{:large-toolbar}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-toolbar-icon-size)
      "Accessor"
      (documentation 'settings-gtk-toolbar-icon-size 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-toolbar-icon-size object) => size}
  @syntax{(setf (gtk:settings-gtk-toolbar-icon-size object) size)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[size]{a @sym{gtk:icon-size} value for the icon size}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-toolbar-icon-size} slot of the
    @class{gtk:settings} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @code{gtk-toolbar-icon-size} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-symbol{gtk:icon-size}")

;;; --- gtk:settings-gtk-toolbar-style -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-toolbar-style"
                                               'settings) t)
 "The @code{gtk-toolbar-style} property of type @sym{gtk:toolbar-style}
  (Read / Write) @br{}
  The size of icons in default toolbars. @br{}
  @em{Warning:} The @code{gtk-toolbar-style} property has been deprecated since
  version 3.10 and should not be used in newly written code. This setting is
  ignored. @br{}
  Default value: @val[gtk:toolbar-style]{:both}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-toolbar-style)
      "Accessor"
      (documentation 'settings-gtk-toolbar-style 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-toolbar-style object) => style}
  @syntax{(setf (gtk:settings-gtk-toolbar-style object) style)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[style]{a @sym{gtk:toolbar-style} value for the size of icons}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-toolbar-style} slot of the
    @class{gtk:settings} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @code{gtk-toolbar-style} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-class{gtk:toolbar}
  @see-function{gtk:toolbar-toolbar-style}")

;;; --- gtk:settings-gtk-tooltip-browse-mode-timeout ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-tooltip-browse-mode-timeout"
                                               'settings) t)
 "The @code{gtk-tooltip-browse-mode-timeout} property of type @code{:int}
  (Read / Write) @br{}
  The amount of time, in milliseconds, after which the browse mode will be
  disabled. @br{}
  @em{Warning:} The @code{gtk-tooltip-browse-mode-timeout} property has been
  deprecated since version 3.10 and should not be used in newly written code.
  This setting is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 500")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-tooltip-browse-mode-timeout)
      "Accessor"
      (documentation 'settings-gtk-tooltip-browse-mode-timeout 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-tooltip-browse-mode-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-tooltip-browse-mode-timeout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the amount of time, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-tooltip-browse-mode-timeout} slot
    of the @class{gtk:settings} class.
  @end{short}
  Amount of time, in milliseconds, after which the browse mode will be
  disabled.
  @begin[Warning]{dictionary}
    The @code{gtk-tooltip-browse-mode-timeout} property has been deprecated
    since version 3.10 and should not be used in newly written code.
    This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-tooltip-browse-timeout --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-tooltip-browse-timeout"
                                               'settings) t)
 "The @code{gtk-tooltip-browse-timeout} property of type @code{:int}
  (Read / Write) @br{}
  Controls the time after which tooltips will appear when browse mode is
  enabled, in milliseconds. Browse mode is enabled when the mouse pointer moves
  off an object where a tooltip was currently being displayed. If the mouse
  pointer hits another object before the browse mode timeout expires, it will
  take the amount of milliseconds specified by this setting to popup the tooltip
  for the new object. @br{}
  @em{Warning:} The @code{gtk-tooltip-browse-timeout} property has been
  deprecated since version 3.10 and should not be used in newly written code.
  This setting is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 60")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-tooltip-browse-timeout)
      "Accessor"
      (documentation 'settings-gtk-tooltip-browse-timeout 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-tooltip-browse-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-tooltip-browse-timeout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the amount of time, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-tooltip-browse-timeout} slot of the
    @class{gtk:settings} class.
  @end{short}
  Controls the time after which tooltips will appear when browse mode is
  enabled, in milliseconds. Browse mode is enabled when the mouse pointer moves
  off an object where a tooltip was currently being displayed. If the mouse
  pointer hits another object before the browse mode timeout expires, it will
  take the amount of milliseconds specified by this setting to popup the
  tooltip for the new object.
  @begin[Warning]{dictionary}
    The @code{gtk-tooltip-browse-timeout} property has been deprecated since
    version 3.10 and should not be used in newly written code. This setting is
    ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-tooltip-timeout ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-tooltip-timeout"
                                               'settings) t)
 "The @code{gtk-tooltip-timeout} property of type @code{:int} (Read / Write)
  @br{}
  The time, in milliseconds, after which a tooltip could appear if the cursor
  is hovering on top of a widget. @br{}
  @em{Warning:} The @code{gtk-tooltip-timeout} property has been deprecated
  since version 3.10 and should not be used in newly written code. This setting
  is ignored. @br{}
  Allowed values: >= 0 @br{}
  Default value: 500")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-tooltip-timeout)
      "Accessor"
      (documentation 'settings-gtk-tooltip-timeout 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-tooltip-timeout object) => setting}
  @syntax{(setf (gtk:settings-gtk-tooltip-timeout object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the time after which a tooltip could appear}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-tooltip-timeout} slot of the
    @class{gtk:settings} class.
  @end{short}
  Time, in milliseconds, after which a tooltip could appear if the cursor is
  hovering on top of a widget.
  @begin[Warning]{dictionary}
    The @code{gtk-tooltip-timeout} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-touchscreen-mode --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-touchscreen-mode"
                                               'settings) t)
 "The @code{gtk-touchscreen-mode} property of type @code{:boolean}
  (Read / Write) @br{}
  If @em{true}, there are no motion notify events delivered on this screen,
  and widgets cannot use the pointer hovering them for any essential
  functionality. @br{}
  @em{Warning:} The @code{gtk-touchscreen-mode} property is deprecated since
  version 3.4 and should not be used in newly written code. Generally the
  behavior touchscreen input should be performed dynamically based on the
  @fun{gdk:event-source-device} function. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-touchscreen-mode)
      "Accessor"
      (documentation 'settings-gtk-touchscreen-mode 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-touchscreen-mode object) => setting}
  @syntax{(setf (gtk:settings-gtk-touchscreen-mode object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether motion notify events are delivered on
    this screen}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-touchscreen-mode} slot of the
    @class{gtk:settings} class.
  @end{short}
  When @em{true}, there are no motion notify events delivered on this screen,
  and widgets cannot use the pointer hovering them for any essential
  functionality.
  @begin[Warning]{dictionary}
    The @code{gtk-touchscreen-mode} property is deprecated since version 3.4
    and should not be used in newly written code. Generally the behavior
    touchscreen input should be performed dynamically based on the
    @fun{gdk:event-source-device} function. @br{}
  @end{dictionary}
  @see-class{gtk:settings}
  @see-function{gdk:event-source-device}")

;;; --- gtk:settings-gtk-visible-focus -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-visible-focus" 'settings) t)
 "The @code{gtk-visible-focus} property of type @sym{gtk:policy-type}
  (Read / Write) @br{}
  Whether focus rectangles should be always visible, never visible, or
  hidden until the user starts to use the keyboard. @br{}
  @em{Warning:} The @code{gtk-visible-focus} property has been deprecated since
  version 3.10 and should not be used in newly written code. This setting is
  ignored. @br{}
  Default value: @val[gtk:policy-type]{:always}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-visible-focus)
      "Accessor"
      (documentation 'settings-gtk-visible-focus 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-visible-focus object) => setting}
  @syntax{(setf (gtk:settings-gtk-visible-focus object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a @sym{gtk:policy-type} value whether focus rectangles
    should be visible}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-visible-focus} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether focus rectangles should be always visible, never visible, or hidden
  until the user starts to use the keyboard.
  @begin[Warning]{dictionary}
    The @code{gtk-visible-focus} property has been deprecated since version
    3.10 and should not be used in newly written code. This setting is ignored.
  @end{dictionary}
  @see-class{gtk:settings}
  @see-symbol{gtk:policy-type}")

;;; --- gtk:settings-gtk-xft-antialias -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-antialias"
                                               'settings) t)
 "The @code{gtk-xft-antialias} property of type @code{:int} (Read / Write) @br{}
  Whether to antialias Xft fonts: 0 = no, 1 = yes, -1 = default. @br{}
  Allowed values: [-1,1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-antialias)
      "Accessor"
      (documentation 'settings-gtk-xft-antialias 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-xft-antialias object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-antialias object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a boolean whether to antialias Xft fonts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-antialias} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to antialias Xft fonts: 0 = no, 1 = yes, -1 = default.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-dpi -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-dpi" 'settings) t)
 "The @code{gtk-xft-dpi} property of type @code{:int} (Read / Write) @br{}
  The resolution for Xft, in 1024 * dots/inch. -1 to use default value. @br{}
  Allowed values: [-1,1048576] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-dpi)
      "Accessor"
      (documentation 'settings-gtk-xft-dpi 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-xft-dpi object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-dpi object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer for the resolution for Xft}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-dpi} slot of the
    @class{gtk:settings} class.
  @end{short}
  Resolution for Xft, in 1024 * dots/inch. -1 to use default value.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-hinting -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-hinting" 'settings) t)
 "The @code{gtk-xft-hinting} property of type @code{:int} (Read / Write) @br{}
  Whether to hint Xft fonts: 0 = no, 1 = yes, -1 = default. @br{}
  Allowed values: [-1,1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-hinting)
      "Accessor"
      (documentation 'settings-gtk-xft-hinting 'function)
 "@version{2023-03-07}
  @syntax{(gtk:settings-gtk-xft-hinting object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-hinting object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{an integer whether to hint Xft fonts}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-hinting} slot of the
    @class{gtk:settings} class.
  @end{short}
  Whether to hint Xft fonts: 0 = no, 1 = yes, -1 = default.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-hintstyle -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-hintstyle"
                                               'settings) t)
 "The @code{gtk-xft-hintstyle} property of type @code{:string} (Read / Write)
  @br{}
  What degree of hinting to use: @code{hintnone}, @code{hintslight},
  @code{hintmedium}, or @code{hintfull}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-hintstyle)
      "Accessor"
      (documentation 'settings-gtk-xft-hintstyle 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-xft-hintstyle object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-hintstyle object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the deegree of hinting}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-hintstyle} slot of the
    @class{gtk:settings} class.
  @end{short}
  What degree of hinting to use: @code{hintnone}, @code{hintslight},
  @code{hintmedium}, or @code{hintfull}.
  @see-class{gtk:settings}")

;;; --- gtk:settings-gtk-xft-rgba ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gtk-xft-rgba" 'settings) t)
 "The @code{gtk-xft-rgba} property of type @code{:string} (Read / Write) @br{}
  The type of subpixel antialiasing:
  @code{none}, @code{rgb}, @code{bgr}, @code{vrgb}, @code{vbgr}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'settings-gtk-xft-rgba)
      "Accessor"
      (documentation 'settings-gtk-xft-rgba 'function)
 "@version{2025-07-03}
  @syntax{(gtk:settings-gtk-xft-rgba object) => setting}
  @syntax{(setf (gtk:settings-gtk-xft-rgba object) setting)}
  @argument[object]{a @class{gtk:settings} object}
  @argument[setting]{a string for the type of subpixel antialiasing}
  @begin{short}
    Accessor of the @slot[gtk:settings]{gtk-xft-rgba} slot of the
    @class{gtk:settings} class.
  @end{short}
  Type of subpixel antialiasing:
  @code{none}, @code{rgb}, @code{bgr}, @code{vrgb}, @code{vbgr}.
  @see-class{gtk:settings}")

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_settings_get_default" settings-default) (g:object settings)
 #+liber-documentation
 "@version{2025-07-03}
  @begin{return}
    The @class{gtk:settings} object. If there is no default screen, then returns
    @code{nil}.
  @end{return}
  @begin{short}
    Gets the @class{gtk:settings} object for the default GDK screen, creating
    it if necessary.
  @end{short}
  See the @fun{gtk:settings-for-screen} function.
  @see-class{gtk:settings}
  @see-function{gtk:settings-for-screen}")

(export 'settings-default)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_get_for_screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_settings_get_for_screen" settings-for-screen)
    (g:object settings)
 #+liber-documentation
 "@version{2023-03-07}
  @argument[screen]{a @class{gdk:screen} object}
  @return{The @class{gtk:settings} object.}
  @begin{short}
    Gets the @class{gtk:settings} object for @arg{screen}, creating it if
    necessary.
  @end{short}
  @see-class{gtk:settings}
  @see-class{gdk:screen}
  @see-function{gtk:settings-default}"
  (screen (g:object gdk:screen)))

(export 'settings-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_settings_install_property ()
;;;
;;; void gtk_settings_install_property (GParamSpec *pspec);
;;;
;;; Warning
;;;
;;; gtk_settings_install_property has been deprecated since version 3.16 and
;;; should not be used in newly written code.
;;;
;;; This function is not useful outside GTK.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_install_property_parser ()
;;;
;;; void gtk_settings_install_property_parser (GParamSpec *pspec,
;;;                                            GtkRcPropertyParser parser)
;;;
;;; Warning
;;;
;;; gtk_settings_install_property_parser has been deprecated since version 3.16
;;; and should not be used in newly written code.
;;;
;;; This function is not useful outside GTK.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_color ()
;;;
;;; gboolean gtk_rc_property_parse_color (const GParamSpec *pspec,
;;;                                       const GString *gstring,
;;;                                       GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser()
;;; or gtk_widget_class_install_style_property_parser() which parses a color
;;; given either by its name or in the form { red, green, blue } where red,
;;; green and blue are integers between 0 and 65535 or floating-point numbers
;;; between 0 and 1.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold GdkColor values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GdkColor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_enum ()
;;;
;;; gboolean gtk_rc_property_parse_enum (const GParamSpec *pspec,
;;;                                      const GString *gstring,
;;;                                      GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser()
;;; or gtk_widget_class_install_style_property_parser() which parses a single
;;; enumeration value.
;;;
;;; The enumeration value can be specified by its name, its nickname or its
;;; numeric value. For consistency with flags parsing, the value may be
;;; surrounded by parentheses.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold enum values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GEnumValue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_flags ()
;;;
;;; gboolean gtk_rc_property_parse_flags (const GParamSpec *pspec,
;;;                                       const GString *gstring,
;;;                                       GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser()
;;; or gtk_widget_class_install_style_property_parser() which parses flags.
;;;
;;; Flags can be specified by their name, their nickname or numerically.
;;; Multiple flags can be specified in the form "( flag1 | flag2 | ... )".
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold flags values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting flags value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_requisition ()
;;;
;;; gboolean gtk_rc_property_parse_requisition (const GParamSpec *pspec,
;;;                                             const GString *gstring,
;;;                                             GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser()
;;; or gtk_widget_class_install_style_property_parser() which parses a
;;; requisition in the form "{ width, height }" for integers width and height.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold boxed values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GtkRequisition.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_rc_property_parse_border ()
;;;
;;; gboolean gtk_rc_property_parse_border (const GParamSpec *pspec,
;;;                                        const GString *gstring,
;;;                                        GValue *property_value);
;;;
;;; A GtkRcPropertyParser for use with gtk_settings_install_property_parser()
;;; or gtk_widget_class_install_style_property_parser() which parses borders in
;;; the form "{ left, right, top, bottom }" for integers left, right, top and
;;; bottom.
;;;
;;; pspec :
;;;     a GParamSpec
;;;
;;; gstring :
;;;     the GString to be parsed
;;;
;;; property_value :
;;;     a GValue which must hold boxed values.
;;;
;;; Returns :
;;;     TRUE if gstring could be parsed and property_value has been set to the
;;;     resulting GtkBorder.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_property_value ()
;;;
;;; void gtk_settings_set_property_value (GtkSettings *settings,
;;;                                       const gchar *name,
;;;                                       const GtkSettingsValue *svalue);
;;;
;;; Warning
;;;
;;; gtk_settings_set_property_value has been deprecated since version 3.16 and
;;; should not be used in newly written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_string_property ()
;;;
;;; void gtk_settings_set_string_property (GtkSettings *settings,
;;;                                        const gchar *name,
;;;                                        const gchar *v_string,
;;;                                        const gchar *origin);
;;;
;;; Warning
;;;
;;; gtk_settings_set_string_property has been deprecated since version 3.16 and
;;; should not be used in newly written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_long_property ()
;;;
;;; void gtk_settings_set_long_property (GtkSettings *settings,
;;;                                      const gchar *name,
;;;                                      glong v_long,
;;;                                      const gchar *origin);
;;;
;;; Warning
;;;
;;; gtk_settings_set_long_property has been deprecated since version 3.16 and
;;; should not be used in newly written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_set_double_property ()
;;;
;;; void gtk_settings_set_double_property (GtkSettings *settings,
;;;                                        const gchar *name,
;;;                                        gdouble v_double,
;;;                                        const gchar *origin);
;;;
;;; Warning
;;;
;;; gtk_settings_set_double_property has been deprecated since version 3.16 and
;;; should not be used in newly written code.
;;;
;;; Use g_object_set() instead.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_settings_reset_property ()
;;;
;;; void
;;; gtk_settings_reset_property (GtkSettings *settings, const gchar *name);
;;;
;;; Undoes the effect of calling g_object_set() to install an
;;; application specific value for a setting. After this call, the setting will
;;; again follow the session-wide value for this setting.
;;;
;;; settings :
;;;     a GtkSettings object
;;;
;;; name :
;;;     the name of the setting to reset
;;;
;;; Since 3.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.settings.lisp -----------------------------------------
