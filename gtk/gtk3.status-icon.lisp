;;; ----------------------------------------------------------------------------
;;; gtk3.status-icon.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkStatusIcon
;;;
;;;     Display an icon in the system tray
;;;
;;; Types and Values
;;;
;;;     GtkStatusIcon
;;;
;;; Functions
;;;
;;;     gtk_status_icon_new
;;;     gtk_status_icon_new_from_pixbuf
;;;     gtk_status_icon_new_from_file
;;;     gtk_status_icon_new_from_stock
;;;     gtk_status_icon_new_from_icon_name
;;;     gtk_status_icon_new_from_gicon
;;;     gtk_status_icon_set_from_pixbuf
;;;     gtk_status_icon_set_from_file
;;;     gtk_status_icon_set_from_stock
;;;     gtk_status_icon_set_from_icon_name
;;;     gtk_status_icon_set_from_gicon
;;;     gtk_status_icon_get_storage_type
;;;     gtk_status_icon_get_pixbuf
;;;     gtk_status_icon_get_stock
;;;     gtk_status_icon_get_icon_name
;;;     gtk_status_icon_get_gicon
;;;     gtk_status_icon_get_size
;;;     gtk_status_icon_set_screen
;;;     gtk_status_icon_get_screen
;;;     gtk_status_icon_set_tooltip_text
;;;     gtk_status_icon_get_tooltip_text
;;;     gtk_status_icon_set_tooltip_markup
;;;     gtk_status_icon_get_tooltip_markup
;;;     gtk_status_icon_set_has_tooltip
;;;     gtk_status_icon_get_has_tooltip
;;;     gtk_status_icon_set_title
;;;     gtk_status_icon_get_title
;;;     gtk_status_icon_set_name
;;;     gtk_status_icon_set_visible
;;;     gtk_status_icon_get_visible
;;;     gtk_status_icon_is_embedded
;;;     gtk_status_icon_position_menu
;;;     gtk_status_icon_get_geometry
;;;     gtk_status_icon_get_x11_window_id
;;;
;;; Properties
;;;
;;;     embedded
;;;     file
;;;     gicon
;;;     has-tooltip
;;;     icon-name
;;;     orientation
;;;     pixbuf
;;;     screen
;;;     size
;;;     stock
;;;     storage-type
;;;     title
;;;     tooltip-markup
;;;     tooltip-text
;;;     visible
;;;
;;; Signals
;;;
;;;     activate
;;;     button-press-event
;;;     button-release-event
;;;     popup-menu
;;;     query-tooltip
;;;     scroll-event
;;;     size-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkStatusIcon
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStatusIcon
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkStatusIcon" status-icon
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_status_icon_get_type")
  ((embedded
    status-icon-embedded
    "embedded" "gboolean" t nil)
   (file
    status-icon-file
    "file" "gchararray" nil t)
   (gicon
    status-icon-gicon
    "gicon" "GIcon" t t)
   (has-tooltip
    status-icon-has-tooltip
    "has-tooltip" "gboolean" t t)
   (icon-name
    status-icon-icon-name
    "icon-name" "gchararray" t t)
   (orientation
    status-icon-orientation
    "orientation" "GtkOrientation" t nil)
   (pixbuf
    status-icon-pixbuf
    "pixbuf" "GdkPixbuf" t t)
   (screen
    status-icon-screen
    "screen" "GdkScreen" t t)
   (size
    status-icon-size
    "size" "gint" t nil)
   (stock
    status-icon-stock
    "stock" "gchararray" t t)
   (storage-type
    status-icon-storage-type
    "storage-type" "GtkImageType" t nil)
   (title
    status-icon-title
    "title" "gchararray" t t)
   (tooltip-markup
    status-icon-tooltip-markup
    "tooltip-markup" "gchararray" t t)
   (tooltip-text
    status-icon-tooltip-text
    "tooltip-text" "gchararray" t t)
   (visible
    status-icon-visible
    "visible" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'status-icon 'type)
 "@version{#2023-3-27}
  @begin{short}
    The \"system tray\" or notification area is normally used for transient
    icons that indicate some special state.
  @end{short}
  For example, a system tray icon might appear to tell the user that they have
  new mail, or have an incoming instant message, or something along those lines.
  The basic idea is that creating an icon in the notification area is less
  annoying than popping up a dialog.

  A @class{gtk:status-icon} object can be used to display an icon in a
  \"system tray\". The icon can have a tooltip, and the user can interact with
  it by activating it or popping up a context menu.

  It is very important to notice that status icons depend on the existence of a
  notification area being available to the user. You should not use status icons
  as the only way to convey critical information regarding your application, as
  the notification area may not exist on the environment of the user, or may
  have been removed. You should always check that a status icon has been
  embedded into a notification area by using the function
  @fun{gtk:status-icon-is-embedded}, and gracefully recover if the function
  returns @code{nil}.

  On X11, the implementation follows the freedesktop.org \"System Tray\"
  specification. Implementations of the \"tray\" side of this specification can
  be found e.g. in the GNOME 2 and KDE panel applications.

  Note that a status icon is not a widget, but just a @class{g:object} object.
  Making it a widget would be impractical, since the system tray on Win32 does
  not allow to embed arbitrary widgets.
  @begin[Warning]{dictionary}
    The @class{gtk:status-icon} class has been deprecated in 3.14. You should
    consider using notifications or more modern platform-specific APIs instead.
    GLib provides the @code{GNotification} API which works well with the
    @class{gtk:application} class on multiple platforms and environments, and
    should be the preferred mechanism to notify the users of transient status
    updates.
    See @url[https://wiki.gnome.org/HowDoI/GNotification]{GNotification} for
    code examples.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (icon)    :action
      @end{pre}
      Gets emitted when the user activates the status icon. If and how status
      icons can be activated is platform dependent. Unlike most @code{:action}
      signals, this signal is meant to be used by applications and should be
      wrapped by language bindings.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
      @end{table}
    @subheading{The \"button-press-event\" signal}
      @begin{pre}
lambda (icon event)    :run-last
      @end{pre}
      The signal will be emitted when a button, typically from a mouse, is
      pressed. Whether this event is emitted is platform dependent. Use the
      @code{\"activate\"} and @code{\"popup-menu\"} signals in preference.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
        @entry[event]{The @class{gdk:event-button} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"button-release-event\" signal}
      @begin{pre}
lambda (icon event)    :run-last
      @end{pre}
      The signal will be emitted when a button, typically from a mouse, is
      released. Whether this event is emitted is platform dependent. Use the
      @code{\"activate\"} and @code{\"popup-menu\"} signals in preference.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
        @entry[event]{The @class{gdk:event-button} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"popup-menu\" signal}
      @begin{pre}
lambda (icon button time)    :action
      @end{pre}
      Gets emitted when the user brings up the context menu of the status icon.
      Whether status icons can have context menus and how these are activated is
      platform dependent. The @arg{button} and @arg{time} parameters should be
      passed as the last arguments to the @fun{gtk:menu-popup} function. Unlike
      most @code{:action} signals, this signal is meant to be used by
      applications and should be wrapped by language bindings.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
        @entry[button]{An unsigned integer with the button that was pressed, or
          0 if the signal is not emitted in response to a button press event.}
        @entry[time]{An unsigned integer with the timestamp of the event that
          triggered the signal emission.}
      @end{table}
    @subheading{The \"query-tooltip\" signal}
      @begin{pre}
lambda (icon x y mode tooltip)    :run-last
      @end{pre}
      Emitted when the @slot[gtk:settings]{gtk-tooltip-timeout} setting has
      expired with the cursor hovering above the status icon, or emitted when
      the status icon got focus in keyboard mode. Using the given coordinates,
      the signal handler should determine whether a tooltip should be shown for
      the status icon. If this is the case @em{true} should be returned,
      @em{false} otherwise. Note that if @arg{mode} is @em{true}, the values of
      @arg{x} and @arg{y} are undefined and should not be used. The signal
      handler is free to manipulate the tooltip with the therefore destined
      function calls. Whether this signal is emitted is platform dependent. For
      plain text tooltips, use the @code{tooltip-text} property in preference.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
        @entry[x]{An integer with the x coordinate of the cursor position where
          the request has been emitted, relative to @arg{icon}.}
        @entry[y]{An integer with the y coordinate of the cursor position where
          the request has been emitted, relative to @arg{icon}.}
        @entry[mode]{@em{True} if the tooltip was trigged using the keyboard.}
        @entry[tooltip]{A @class{gtk:tooltip} object.}
        @entry[Returns]{@em{True} if the tooltip should be shown right now,
          @em{false} otherwise.}
      @end{table}
    @subheading{The \"scroll-event\" signal}
      @begin{pre}
lambda (icon event)    :run-last
      @end{pre}
      The signal is emitted when a button in the 4 to 7 range is pressed. Wheel
      mice are usually configured to generate button press events for buttons 4
      and 5 when the wheel is turned. Whether this event is emitted is platform
      dependent.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
        @entry[event]{The @class{gdk:event-scroll} event which triggered this
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
    @subheading{The \"size-changed\" signal}
      @begin{pre}
lambda (icon size)    :run-last
      @end{pre}
      Gets emitted when the size available for the image changes, e.g. because
      the notification area got resized.
      @begin[code]{table}
        @entry[icon]{The @class{gtk:status-icon} object which received the
          signal.}
        @entry[size]{An integer with the new size.}
        @entry[Returns]{@em{True} if the icon was updated for the new size.
          Otherwise, GTK will scale the icon as necessary.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:status-icon-new}
  @see-constructor{gtk:status-icon-new-from-pixbuf}
  @see-constructor{gtk:status-icon-new-from-file}
  @see-constructor{gtk:status-icon-new-from-stock}
  @see-constructor{gtk:status-icon-new-from-icon-name}
  @see-constructor{gtk:status-icon-new-from-gicon}
  @see-constructor{gtk:status-icon-new-from-pixbuf}
  @see-slot{gtk:status-icon-embedded}
  @see-slot{gtk:status-icon-file}
  @see-slot{gtk:status-icon-gicon}
  @see-slot{gtk:status-icon-has-tooltip}
  @see-slot{gtk:status-icon-icon-name}
  @see-slot{gtk:status-icon-orientation}
  @see-slot{gtk:status-icon-pixbuf}
  @see-slot{gtk:status-icon-screen}
  @see-slot{gtk:status-icon-size}
  @see-slot{gtk:status-icon-stock}
  @see-slot{gtk:status-icon-storage-type}
  @see-slot{gtk:status-icon-title}
  @see-slot{gtk:status-icon-tooltip-markup}
  @see-slot{gtk:status-icon-tooltip-text}
  @see-slot{gtk:status-icon-visible}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:status-icon-embedded -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "embedded" 'status-icon) t)
 "The @code{embedded} property of type @code{:boolean} (Read) @br{}
  @em{True} if the status icon is embedded in a notification area. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-embedded)
      "Accessor"
      (documentation 'status-icon-embedded 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-embedded object) => embedded}
  @argument[object]{a @class{gtk:status-icon} object}
  @argument[embedded]{a boolean whether the status icon is embedded}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{embedded} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  @em{True} if the status icon is embedded in a notification area.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-embedded} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-is-embedded}")

;;; --- gtk:status-icon-file ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'status-icon) t)
 "The @code{file} property of type @code{:string} (Write) @br{}
  Filename to load and display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-file)
      "Accessor"
      (documentation 'status-icon-file 'function)
 "@version{#2023-3-27}
  @syntax{(setf (gtk:status-icon-file object) file)}
  @argument[object]{a @class{gtk:status-icon} object}
  @argument[file]{a string with a filename}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{file} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  Filename to load and display.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-file} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}")

;;; --- gtk:status-icon-gicon --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'status-icon) t)
 "The @code{gicon} property of type @class{g:icon} (Read / Write) @br{}
  The icon displayed in the status icon. For themed icons, the image will be
  updated automatically if the theme changes.")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-gicon)
      "Accessor"
      (documentation 'status-icon-gicon 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-gicon object) => icon}
  @syntax{(setf (gtk:status-icon-gicon object) icon)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{gicon} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-gicon} function retrieves icon being displayed by the
  status icon. The @setf{gtk:status-icon-gicon} function sets the icon.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:gicon} of the @symbol{gtk:image-type} enumeration. See the
  @fun{gtk:status-icon-storage-type} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-gicon} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-class{g:icon}
  @see-symbol{gtk:image-type}
  @see-function{gtk:status-icon-storage-type}")

;;; --- gtk:status-icon-has-tooltip --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-tooltip" 'status-icon) t)
 "The @code{has-tooltip} property of type @code{:boolean} (Read / Write) @br{}
  Enables or disables the emission of @code{\"query-tooltip\"} signals on the
  status icon. A value of @em{true} indicates that the status icon can have a
  tooltip, in this case the status icon will be queried using the
  @code{\"query-tooltip\"} signal to determine whether it will provide a tooltip
  or not. Note that setting this property to @em{true} for the first time will
  change the event masks of the windows of this status icon to include
  @code{\"leave-notify\"} and @code{\"motion-notify\"} signals. This will not be
  undone when the property is set to @em{false} again. Whether this property is
  respected is platform dependent. For plain text tooltips, use the
  @slot[gtk:status-icon]{tooltip-text} property in preference. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-has-tooltip)
      "Accessor"
      (documentation 'status-icon-has-tooltip 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-has-tooltip object) => has-tooltip}
  @syntax{(setf (gtk:status-icon-has-tooltip object) has-tooltip)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[has-tooltip]{a boolean whether or not the status icon has a tooltip}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{has-tooltip} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-has-tooltip} function returns the current value of
  the @slot[gtk:status-icon]{has-tooltip} property. The
  @setf{gtk:status-icon-has-tooltip} function sets the property.

  Enables or disables the emission of @code{\"query-tooltip\"} signals on the
  status icon. A value of @em{true} indicates that the status icon can have a
  tooltip, in this case the status icon will be queried using the
  @code{\"query-tooltip\"} signal to determine whether it will provide a tooltip
  or not. Note that setting this property to @em{true} for the first time will
  change the event masks of the windows of this status icon to include
  @code{\"leave-notify\"} and @code{\"motion-notify\"} signals. This will not be
  undone when the property is set to @em{false} again. Whether this property is
  respected is platform dependent. For plain text tooltips, use the
  @slot[gtk:status-icon]{tooltip-text} property in preference.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-has-tooltip} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-tooltip-text}")

;;; --- gtk:status-icon-icon-name ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'status-icon) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon from the icon theme. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-icon-name)
      "Accessor"
      (documentation 'status-icon-icon-name 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-icon-name object) => name}
  @syntax{(setf (gtk:status-icon-icon-name object) name)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[name]{a string with the name of the icon from the icon theme}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{icon-name} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-icon-name} function gets the name of the icon being
  displayed by the status icon. The @setf{gtk:status-icon-icon-name} function
  sets the name.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:icon-name} of the @symbol{gtk:image-type} enumeration. See the
  @fun{gtk:status-icon-storage-type} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-icon-name} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-symbol{gtk:image-type}
  @see-function{gtk:status-icon-storage-type}")

;;; --- gtk:status-icon-orientation --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'status-icon) t)
 "The @code{orientation} property of type @symbol{gtk:orientation} (Read) @br{}
  The orientation of the tray in which the status icon is embedded. @br{}
  Default value: @code{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-orientation)
      "Accessor"
      (documentation 'status-icon-orientation 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-orientation object) => orientation}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{orientation} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The orientation of the tray in which the status icon is embedded.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-orientation} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-symbol{gtk:orientation}")

;;; --- gtk:status-icon-pixbuf -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixbuf" 'status-icon) t)
 "The @code{pixbuf} property of type @class{gdk-pixbuf:pixbuf} (Read / Write)
  @br{}
  The pixbuf to display.")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-pixbuf)
      "Accessor"
      (documentation 'status-icon-pixbuf 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-pixbuf object) => pixbuf}
  @syntax{(setf (gtk:status-icon-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{pixbuf} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-pixbuf} function gets the pixbuf being displayed by
  the status icon. The @setf{gtk:status-icon-pixbuf} function sets the pixbuf.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:pixbuf} of the @symbol{gtk:image-type} enumeration. See the
  @fun{gtk:status-icon-storage-type} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-pixbuf} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:image-type}
  @see-function{gtk:status-icon-storage-type}")

;;; --- gtk:status-icon-screen -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "screen" 'status-icon) t)
 "The @code{screen} property of type @class{gdk:screen} (Read / Write) @br{}
  The screen where this status icon will be displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-screen)
      "Accessor"
      (documentation 'status-icon-screen 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-screen object) => screen}
  @syntax{(setf (gtk:status-icon-screen object) screen)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{screen} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-screen} function returns the screen associated with
  the status icon. The @setf{gtk:status-icon-screen} function sets the screen.
  If the icon is already mapped, it will be unmapped, and then remapped on the
  new screen.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-screen} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-class{gdk:screen}")

;;; --- gtk:status-icon-size ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size" 'status-icon) t)
 "The @code{size} property of type @code{:int} (Read) @br{}
  The size of the icon. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-size)
      "Accessor"
      (documentation 'status-icon-size 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-size object) => size}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[size]{an integer with the size of the icon}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{size} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-size} function gets the size in pixels that is
  available for the image.

  Stock icons and named icons adapt their size automatically if the size of the
  notification area changes. For other storage types, the
  @code{\"size-changed\"} signal can be used to react to size changes. Note that
  the returned size is only meaningful while the status icon is embedded. See
  the @fun{gtk:status-icon-is-embedded} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-size} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-is-embedded}")

;;; --- gtk:status-icon-stock --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock" 'status-icon) t)
 "The @code{stock} property of type @code{:string} (Read / Write) @br{}
  Stock ID for a stock image to display. @br{}
  @em{Warning:} The @code{stock} property has been deprecated since version
  3.10 and should not be used in newly written code. Use the
  @code{icon-name} property instead. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-stock)
      "Accessor"
      (documentation 'status-icon-stock 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-stock object) => stock-id}
  @syntax{(setf (gtk:status-icon-stock object) stock-id)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{stock} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-stock} function gets the ID of the stock icon being
  displayed by the status icon. The @setf{gtk:status-icon-stock} function sets
  the stock ID.

  The storage type of the status icon must be the value @code{:empty} or
  @code{:stock} of the @symbol{gtk:image-type} enumeration. See the
  @fun{gtk:status-icon-storage-type} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-stock} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-symbol{gtk:image-type}
  @see-function{gtk:status-icon-storage-type}")

;;; --- gtk:status-icon-storage-type -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "storage-type" 'status-icon) t)
 "The @code{storage-type} property of type @symbol{gtk:image-type} (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-storage-type)
      "Accessor"
      (documentation 'status-icon-storage-type 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-storage-type object) => storage-type}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[storage-type]{a value of the @symbol{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{storage-type} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-storage-type} function gets the type of
  representation being used by the status icon to store image data. If the
  status icon has no image data, the return value will be the @code{:empty}
  value.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-storage-type} value has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-symbol{gtk:image-type}")

;;; --- gtk:status-icon-title --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'status-icon) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of this tray icon. This should be a short, human readable,
  localized string describing the tray icon. It may be used by tools like
  screen readers to render the tray icon. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-title)
      "Accessor"
      (documentation 'status-icon-title 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-title object) => title}
  @syntax{(setf (gtk:status-icon-title object) title)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[title]{a string with the title}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{title} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-title} function gets the title of the status icon.
  The @setf{gtk:status-icon-title} function sets the title.

  This should be a short, human readable, localized string describing the status
  icon. It may be used by tools like screen readers to render the status icon.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-title} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}")

;;; --- gtk:status-icon-tooltip-markup -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-markup" 'status-icon) t)
 "The @code{tooltip-markup} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string, which is marked up with
  the Pango text markup language. Also see the @fun{gtk:tooltip-set-markup}
  function. This is a convenience property which will take care of getting the
  tooltip shown if the given string is not @code{nil}. The @code{has-tooltip}
  property will automatically be set to @em{true} and the default handler for
  the @code{\"query-tooltip\"} signal will take care of displaying the tooltip.
  On some platforms, embedded markup will be ignored. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-tooltip-markup)
      "Accessor"
      (documentation 'status-icon-tooltip-markup 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-tooltip-markup object) => markup}
  @syntax{(setf (gtk:status-icon-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[markup]{a string with the contents of the tooltip for the status
    icon, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{tooltip-markup} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-tooltip-markup} function gets the contents of the
  tooltip for the status icon. The @setf{gtk:status-icon-tooltip-markup}
  function sets @arg{markup} as the contents of the tooltip, which is marked up
  with the Pango text markup language.

  This function will take care of setting the
  @slot[gtk:status-icon]{has-tooltip} property to @em{true} and of the default
  handler for the @code{\"query-tooltip\"} signal. See also the
  @fun{gtk:tooltip-markup} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-tooltip-markup} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:tooltip-markup}
  @see-function{gtk:status-icon-has-tooltip}")

;;; --- gtk:status-icon-tooltip-text -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-text" 'status-icon) t)
 "The @code{tooltip-text} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string. Also see the
  @fun{gtk:tooltip-set-text} function. This is a convenience property which will
  take care of getting the tooltip shown if the given string is not @code{nil}.
  The @code{has-tooltip} property will automatically be set to @em{true} and the
  default handler for the @code{\"query-tooltip\"} signal will take care of
  displaying the tooltip. Note that some platforms have limitations on the
  length of tooltips that they allow on status icons, e.g. Windows only shows
  the first 64 characters. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-tooltip-text)
      "Accessor"
      (documentation 'status-icon-tooltip-text 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-tooltip-text object) => text}
  @syntax{(setf (gtk:status-icon-tooltip-text object) text)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[text]{a string with the contents of the tooltip for the status icon}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{tooltip-text} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-tooltip-text} function gets the contents of the
  tooltip for the status icon. The @setf{gtk:status-icon-tooltip-text} function
  sets the text of the tooltip.

  This function will take care of setting the
  @slot[gtk:status-icon]{has-tooltip} property to @em{true} and of the default
  handler for the @code{\"query-tooltip\"} signal.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-tooltip-text} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-has-tooltip}")

;;; --- gtk:status-icon-visible ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'status-icon) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the status icon is visible. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'status-icon-visible)
      "Accessor"
      (documentation 'status-icon-visible 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:status-icon-visible object) => visible}
  @syntax{(setf (gtk:status-icon-visible object) visible)}
  @argument[object]{a @class{gtk:status-icon} widget}
  @argument[visible]{@em{true} to show the status icon, @em{false} to hide it}
  @begin{short}
    Accessor of the @slot[gtk:status-icon]{visible} slot of the
    @class{gtk:status-icon} class.
  @end{short}
  The @fun{gtk:status-icon-visible} function returns whether the status icon is
  visible or not. The @setf{gtk:status-icon-visible} functon sets the
  visibility.

  Note that being visible does not guarantee that the user can actually see the
  icon, see also the @fun{gtk:status-icon-is-embedded} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-visible} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-is-embedded}")

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline status-icon-new))

(defun status-icon-new ()
 #+liber-documentation
 "@version{#2023-3-27}
  @return{The new @class{gtk:status-icon} object.}
  @short{Creates an empty status icon.}
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-new} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}"
  (make-instance 'status-icon))

(export 'status-icon-new)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_new_from_pixbuf" status-icon-new-from-pixbuf)
    (g:object status-icon)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{The new @class{gtk:status-icon} object.}
  @begin{short}
    Creates a status icon displaying @arg{pixbuf}.
  @end{short}
  The image will be scaled down to fit in the available space in the
  notification area, if necessary.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-new-from-pixbuf} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'status-icon-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_new_from_file" status-icon-new-from-file)
    (g:object status-icon)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[filename]{a string with the filename}
  @return{The new @class{gtk:status-icon} object.}
  @begin{short}
    Creates a status icon displaying the icon from a file.
  @end{short}
  The image will be scaled down to fit in the available space in the
  notification area, if necessary.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-new-from-file} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}"
  (filename :string))

(export 'status-icon-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_stock ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_new_from_stock" status-icon-new-from-stock)
    (g:object status-icon)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[stock-id]{a string with a stock icon ID}
  @return{The new @class{gtk:status-icon} object.}
  @begin{short}
    Creates a status icon displaying a stock icon.
  @end{short}
  Sample stock icon names are \"gtk-open\", \"gtk-quit\".
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-new-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}"
  (stock-id :string))

(export 'status-icon-new-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_icon_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_new_from_icon_name"
               status-icon-new-from-icon-name) (g:object status-icon)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[name]{a string with an icon name}
  @return{The new @class{gtk:status-icon} object.}
  @begin{short}
    Creates a status icon displaying an icon from the current icon theme.
  @end{short}
  If the current icon theme is changed, the icon will be updated appropriately.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-new-from-icon-name} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}"
  (name :string))

(export 'status-icon-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_new_from_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_new_from_gicon" status-icon-new-from-gicon)
    (g:object status-icon)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{g:icon} object}
  @return{The new @class{gtk:status-icon} object.}
  @begin{short}
    Creates a status icon displaying a @class{g:icon} object.
  @end{short}
  If the icon is a themed icon, it will be updated when the theme changes.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-new-from-gicon} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-class{g:icon}"
  (icon (g:object g:icon)))

(export 'status-icon-new-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_set_from_pixbuf" status-icon-set-from-pixbuf)
    :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object or @code{nil}}
  @begin{short}
    Makes the status icon display @arg{pixbuf}.
  @end{short}
  See the @fun{gtk:status-icon-new-from-pixbuf} function for details.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-set-from-pixbuf} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:status-icon-new-from-pixbuf}"
  (icon (g:object status-icon))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'status-icon-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_set_from_file" status-icon-set-from-file) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @argument[filename]{a string with a filename}
  @begin{short}
    Makes  the status icon display the icon from the file @arg{filename}.
  @end{short}
  See the @fun{gtk:status-icon-new-from-file} function for details.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-set-from-file} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-new-from-file}"
  (icon (g:object status-icon))
  (filename :string))

(export 'status-icon-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_stock ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_set_from_stock" status-icon-set-from-stock)
    :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Makes the status icon display the stock icon with the ID @arg{stock-id.}
  @end{short}
  See the @fun{gtk:status-icon-new-from-stock} function for details.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-set-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-new-from-stock}"
  (icon (g:object status-icon))
  (stock-id :string))

(export 'status-icon-set-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_icon_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_set_from_icon_name"
               status-icon-set-from-icon-name) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @argument[name]{a string with an icon name}
  @begin{short}
    Makes the status icon display the icon named @arg{name} from the current
    icon theme.
  @end{short}
  See the @fun{gtk:status-icon-new-from-icon-name} function for details.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-set-from-icon-name} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-new-from-icon-name}"
  (icon (g:object status-icon))
  (name :string))

(export 'status-icon-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_from_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_set_from_gicon" status-icon-set-from-gicon)
    :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @argument[gicon]{a @class{g:icon} object}
  @begin{short}
    Makes the status icon display the @class{g:icon} object.
  @end{short}
  See the @fun{gtk:status-icon-new-from-gicon} function for details.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-set-from-gicon} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}
  @see-function{gtk:status-icon-new-from-gicon}"
  (icon (g:object status-icon))
  (gicon (g:object g:icon)))

(export 'status-icon-set-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_set_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_status_icon_set_name" status-icon-set-name) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @argument[name]{a string with the name}
  @begin{short}
    Sets the name of the tray icon.
  @end{short}
  This should be a string identifying this icon. It may be used for sorting the
  icons in the tray and will not be shown to the user.
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-set-name} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}"
  (icon (g:object status-icon))
  (name :string))

(export 'status-icon-set-name)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_is_embedded ()
;;; ----------------------------------------------------------------------------

;; TODO: Replace this code with a call to the accessor status-icon-embedded

(cffi:defcfun ("gtk_status_icon_is_embedded" status-icon-is-embedded) :boolean
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[icon]{a @class{gtk:status-icon} object}
  @return{@em{True} if the status icon is embedded in a notification area.}
  @begin{short}
    Returns whether the status icon is embedded in a notification area.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:status-icon-is-embedded} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:status-icon}"
  (icon (g:object status-icon)))

(export 'status-icon-is-embedded)

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_position_menu ()
;;;
;;; void gtk_status_icon_position_menu (GtkMenu *menu,
;;;                                     gint *x,
;;;                                     gint *y,
;;;                                     gboolean *push_in,
;;;                                     gpointer user_data);
;;;
;;; gtk_status_icon_position_menu has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;; Use GNotification and GtkApplication to provide status notifications;
;;; notifications do not have menus, but can have buttons, and actions
;;; associated with each button.
;;;
;;; Menu positioning function to use with gtk_menu_popup() to position menu
;;; aligned to the status icon user_data.
;;;
;;; menu :
;;;     the GtkMenu
;;;
;;; x :
;;;     return location for the x position
;;;
;;; y :
;;;     return location for the y position
;;;
;;; push_in :
;;;     whether the first menu item should be offset (pushed in) to be aligned
;;;     with the menu popup position (only useful for GtkOptionMenu)
;;;
;;; user_data :
;;;     the status icon to position the menu on
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_geometry ()
;;;
;;; gboolean gtk_status_icon_get_geometry (GtkStatusIcon *status_icon,
;;;                                        GdkScreen **screen,
;;;                                        GdkRectangle *area,
;;;                                        GtkOrientation *orientation);
;;;
;;; gtk_status_icon_get_geometry has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;; Use GNotification and GtkApplication to provide status notifications; there
;;; is no direct replacement for this function, as the platform is responsible
;;; for the presentation of notifications
;;;
;;; Obtains information about the location of the status icon on screen. This
;;; information can be used to e.g. position popups like notification bubbles.
;;;
;;; See gtk_status_icon_position_menu() for a more convenient alternative for
;;; positioning menus.
;;;
;;; Note that some platforms do not allow GTK to provide this information, and
;;; even on platforms that do allow it, the information is not reliable unless
;;; the status icon is embedded in a notification area, see
;;; gtk_status_icon_is_embedded().
;;;
;;; status_icon :
;;;     a GtkStatusIcon
;;;
;;; screen :
;;;     return location for the screen, or NULL if the information is not
;;;     needed
;;;
;;; area :
;;;     return location for the area occupied by the status icon, or NULL
;;;
;;; orientation :
;;;     return location for the orientation of the panel in which the status
;;;     icon is embedded, or NULL. A panel at the top or bottom of the screen is
;;;     horizontal, a panel at the left or right is vertical
;;;
;;; Returns :
;;;     TRUE if the location information has been filled in
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_status_icon_get_x11_window_id ()
;;;
;;; guint32 gtk_status_icon_get_x11_window_id (GtkStatusIcon *status_icon);
;;;
;;; gtk_status_icon_get_x11_window_id has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;; Use GNotification and GtkApplication to provide status notifications; there
;;; is no direct replacement for this function
;;;
;;; This function is only useful on the X11/freedesktop.org platform. It returns
;;; a window ID for the widget in the underlying status icon implementation.
;;; This is useful for the Galago notification service, which can send a window
;;; ID in the protocol in order for the server to position notification windows
;;; pointing to a status icon reliably.
;;;
;;; This function is not intended for other use cases which are more likely to
;;; be met by one of the non-X11 specific methods, such as
;;; gtk_status_icon_position_menu().
;;;
;;; status_icon :
;;;     a GtkStatusIcon
;;;
;;; Returns :
;;;     An 32 bit unsigned integer identifier for the underlying X11 Window
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.status-icon.lisp --------------------------------------
