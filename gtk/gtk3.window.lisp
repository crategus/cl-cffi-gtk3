;;; ----------------------------------------------------------------------------
;;; gtk3.window.lisp
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
;;; GtkWindow
;;;
;;;     Toplevel which can contain other widgets
;;;
;;; Types and Values
;;;
;;;     GtkWindow
;;;     GtkWindowType
;;;     GtkWindowPosition
;;;
;;; Accessors
;;;
;;;     gtk_window_get_accept_focus
;;;     gtk_window_set_accept_focus
;;;     gtk_window_get_application
;;;     gtk_window_set_application
;;;     gtk_window_get_attached_to
;;;     gtk_window_set_attached_to
;;;     gtk_window_get_decorated
;;;     gtk_window_set_decorated
;;;     gtk_window_get_deletable
;;;     gtk_window_set_deletable
;;;     gtk_window_get_destroy_with_parent
;;;     gtk_window_set_destroy_with_parent
;;;     gtk_window_get_focus_on_map
;;;     gtk_window_set_focus_on_map
;;;     gtk_window_get_focus_visible
;;;     gtk_window_set_focus_visible
;;;     gtk_window_set_gravity
;;;     gtk_window_get_gravity
;;;     gtk_window_get_has_resize_grip
;;;     gtk_window_set_has_resize_grip
;;;     gtk_window_get_hide_titlebar_when_maximized
;;;     gtk_window_set_hide_titlebar_when_maximized
;;;     gtk_window_get_icon
;;;     gtk_window_set_icon
;;;     gtk_window_get_icon_name
;;;     gtk_window_set_icon_name
;;;     gtk_window_is_active
;;;     gtk_window_is_maximized
;;;     gtk_window_get_mnemonics_visible
;;;     gtk_window_set_mnemonics_visible
;;;     gtk_window_get_modal
;;;     gtk_window_set_modal
;;;     gtk_window_get_resizable
;;;     gtk_window_set_resizable
;;;     gtk_window_get_role
;;;     gtk_window_set_role
;;;     gtk_window_set_screen
;;;     gtk_window_get_screen
;;;     gtk_window_get_skip_pager_hint
;;;     gtk_window_set_skip_pager_hint
;;;     gtk_window_get_skip_taskbar_hint
;;;     gtk_window_set_skip_taskbar_hint
;;;     gtk_window_get_title
;;;     gtk_window_set_title
;;;     gtk_window_get_transient_for
;;;     gtk_window_set_transient_for
;;;     gtk_window_get_type_hint
;;;     gtk_window_set_type_hint
;;;     gtk_window_get_urgency_hint
;;;     gtk_window_set_urgency_hint
;;;
;;; Functions
;;;
;;;     gtk_window_new
;;;     gtk_window_set_wmclass                              Deprecated 3.22
;;;     gtk_window_add_accel_group
;;;     gtk_window_remove_accel_group
;;;     gtk_window_activate_focus
;;;     gtk_window_activate_default
;;;     gtk_window_set_default_size
;;;     gtk_window_set_default_geometry                     Deprecated 3.20
;;;     gtk_window_set_geometry_hints
;;;     gtk_window_set_position
;;;     gtk_window_has_toplevel_focus
;;;     gtk_window_list_toplevels
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
;;;     gtk_window_fullscreen_on_monitor
;;;     gtk_window_unfullscreen
;;;     gtk_window_set_keep_above
;;;     gtk_window_set_keep_below
;;;     gtk_window_begin_resize_drag
;;;     gtk_window_begin_move_drag
;;;     gtk_window_set_mnemonic_modifier
;;;     gtk_window_get_default_icon_list
;;;     gtk_window_get_default_icon_name
;;;     gtk_window_get_default_size
;;;     gtk_window_get_icon_list
;;;     gtk_window_get_mnemonic_modifier
;;;     gtk_window_get_position
;;;     gtk_window_get_size
;;;     gtk_window_get_group
;;;     gtk_window_has_group
;;;     gtk_window_get_window_type
;;;     gtk_window_move
;;;     gtk_window_parse_geometry                           Deprecated 3.20
;;;     gtk_window_reshow_with_initial_size                 not exported
;;;     gtk_window_resize
;;;     gtk_window_resize_to_geometry                       Deprecated 3.20
;;;     gtk_window_set_default_icon_list
;;;     gtk_window_set_default_icon
;;;     gtk_window_set_default_icon_from_file
;;;     gtk_window_set_default_icon_name
;;;     gtk_window_set_icon_list
;;;     gtk_window_set_icon_from_file
;;;     gtk_window_set_auto_startup_notification
;;;     gtk_window_get_opacity
;;;     gtk_window_set_opacity
;;;     gtk_window_resize_grip_is_visible                   not exported
;;;     gtk_window_get_resize_grip_area                     not exported
;;;     gtk_window_set_has_user_ref_count                   not implemented
;;;     gtk_window_set_titlebar
;;;     gtk_window_get_titlebar
;;;     gtk_window_set_interactive_debugging
;;
;;; Properties
;;;
;;;     accept-focus
;;;     application
;;;     attached-to
;;;     decorated
;;;     default-height
;;;     default-width
;;;     deletable
;;;     destroy-with-parent
;;;     focus-on-map
;;;     focus-visible
;;;     gravity
;;;     has-resize-grip                                     Deprecated 3.14
;;;     has-toplevel-focus
;;;     hide-titlebar-when-maximized
;;;     icon
;;;     icon-name
;;;     is-active
;;;     is-maximized
;;;     mnemonics-visible
;;;     modal
;;;     resizable
;;;     resize-grip-visible                                 Deprecated 3.14
;;;     role
;;;     screen
;;;     skip-pager-hint
;;;     skip-taskbar-hint
;;;     startup-id
;;;     title
;;;     transient-for
;;;     type
;;;     type-hint
;;;     urgency-hint
;;;     window-position
;;;
;;; Style Properties
;;;
;;;     decoration-button-layout
;;;     decoration-resize-handle
;;;
;;; Signals
;;;
;;;     activate-default
;;;     activate-focus
;;;     enable-debugging
;;;     keys-changed
;;;     set-focus
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ├── GtkDialog
;;;                         ├── GtkApplicationWindow
;;;                         ├── GtkAssistant
;;;                         ├── GtkOffscreenWindow
;;;                         ├── GtkPlug
;;;                         ╰── GtkShortcutsWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkWindow implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindowType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkWindowType" window-type
  (:export t
   :type-initializer "gtk_window_type_get_type")
  (:toplevel 0)
  (:popup 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-type)
      "GEnum"
      (liber:symbol-documentation 'window-type)
 "@version{2025-06-02}
  @begin{declaration}
(gobject:define-genum \"GtkWindowType\" window-type
  (:export t
   :type-initializer \"gtk_window_type_get_type\")
  (:toplevel 0)
  (:popup 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:toplevel]{A regular window, such as a dialog.}
      @entry[:popup]{A special window such as a tooltip.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration for the possible types of a @class{gtk:window} widget.
  @end{short}
  The @class{gtk:window} widget can be one of the
  @val[gtk:window-type]{:toplevel} or @val[gtk:window-type]{:popup} types. Most
  things you would consider a \"window\" should have
  @val[gtk:window-type]{:toplevel} type. Windows with this type are managed by
  the window manager and have a frame by default. Call the
  @fun{gtk:window-decorated} function to toggle the frame. Windows with
  @val[gtk:window-type]{:popup} type are ignored by the window manager. Window
  manager keybindings will not work on them, the window manager will not
  decorate the window with a frame, many GTK features that rely on the window
  manager will not work, for example, resize grips and
  maximization/minimization. The @val[gtk:window-type]{:popup} type is used to
  implement widgets such as @class{gtk:menu} widgets or tooltips that you
  normally do not think of as windows per se. Nearly all windows should be of
  @val[gtk:window-type]{:toplevel} type. In particular, do not use the
  @val[gtk:window-type]{:popup} type just to turn off the window borders. Use
  the @fun{gtk:window-decorated} function for that.
  @see-class{gtk:window}
  @see-class{gtk:menu}
  @see-function{gtk:window-decorated}")

;;; ----------------------------------------------------------------------------
;;; GtkWindowPosition
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkWindowPosition" window-position
  (:export t
   :type-initializer "gtk_window_position_get_type")
  (:none 0)
  (:center 1)
  (:mouse 2)
  (:center-always 3)
  (:center-on-parent 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-position)
      "GEnum"
      (liber:symbol-documentation 'window-position)
 "@version{2024-03-16}
  @begin{declaration}
(gobject:define-genum \"GtkWindowPosition\" window-position
  (:export t
   :type-initializer \"gtk_window_position_get_type\")
  (:none 0)
  (:center 1)
  (:mouse 2)
  (:center-always 3)
  (:center-on-parent 4))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{No influence is made on placement.}
      @entry[:center]{Windows should be placed in the center of the screen.}
      @entry[:mouse]{Windows should be placed at the current mouse position.}
      @entry[:center-always]{Keep window centered as it changes size, etc.}
      @entry[:center-on-parent]{Center the window on its transient parent.
        See the @fun{gtk:window-transient-for} function.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Window placement can be influenced using this enumeration.
  @end{short}
  Note that using the @val[gtk:window-position]{:center-always} value is almost
  always a bad idea. It will not necessarily work well with all window managers
  or on all windowing systems.
  @see-class{gtk:window}
  @see-function{gtk:window-transient-for}")

;;; ----------------------------------------------------------------------------
;;; GtkWindow
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkWindow" window
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_window_get_type")
  ((accept-focus
    window-accept-focus
    "accept-focus" "gboolean" t t)
   (application
    window-application
    "application" "GtkApplication" t t)
   (attached-to
    window-attached-to
    "attached-to" "GtkWidget" t t)
   (decorated
    window-decorated
    "decorated" "gboolean" t t)
   (default-height
    window-default-height
    "default-height" "gint" t t)
   (default-width
    window-default-width
    "default-width" "gint" t t)
   (deletable
    window-deletable
    "deletable" "gboolean" t t)
   (destroy-with-parent
    window-destroy-with-parent
    "destroy-with-parent" "gboolean" t t)
   (focus-on-map
    window-focus-on-map
    "focus-on-map" "gboolean" t t)
   (focus-visible
    window-focus-visible
    "focus-visible" "gboolean" t t)
   (gravity
    window-gravity
    "gravity" "GdkGravity" t t)
   (has-resize-grip
    window-has-resize-grip
    "has-resize-grip" "gboolean" t t)
   (has-toplevel-focus
     window-has-toplevel-focus
     "has-toplevel-focus" "gboolean" t nil)
   (hide-titlebar-when-maximized
    window-hide-titlebar-when-maximized
    "hide-titlebar-when-maximized" "gboolean" t t)
   (icon
    window-icon
    "icon" "GdkPixbuf" t t)
   (icon-name
    window-icon-name
    "icon-name" "gchararray" t t)
   (is-active
    window-is-active
    "is-active" "gboolean" t nil)
   (is-maximized
    window-is-maximized
    "is-maximized" "gboolean" t nil)
   (mnemonics-visible
    window-mnemonics-visible
    "mnemonics-visible" "gboolean" t t)
   (modal
    window-modal
    "modal" "gboolean" t t)
   (opacity
    window-opacity
    "opacity" "gdouble" t t)
   (resizable
    window-resizable
    "resizable" "gboolean" t t)
   (resize-grip-visible
    window-resize-grip-visible
    "resize-grip-visible" "gboolean" t nil)
   (role
    window-role
    "role" "gchararray" t t)
   (screen
    window-screen
    "screen" "GdkScreen" t t)
   (skip-pager-hint
    window-skip-pager-hint
    "skip-pager-hint" "gboolean" t t)
   (skip-taskbar-hint
    window-skip-taskbar-hint
    "skip-taskbar-hint" "gboolean" t t)
   (startup-id
    window-startup-id
    "startup-id" "gchararray" nil t)
   (title
    window-title
    "title" "gchararray"  t t)
   (transient-for
    window-transient-for
    "transient-for" "GtkWindow" t t)
   (type
    window-type
    "type" "GtkWindowType" t nil)
   (type-hint
    window-type-hint
    "type-hint" "GdkWindowTypeHint" t t)
   ;; "ubuntu-no-proxy" is not documented. Special for Ubuntu.
   #+ubuntu
   (ubuntu-no-proxy
    window-ubuntu-no-proxy
    "ubuntu-no-proxy" "gboolean" t nil)
   (urgency-hint
    window-urgency-hint
    "urgency-hint" "gboolean" t t)
   (window-position
    window-window-position
    "window-position" "GtkWindowPosition" t t)))

#+liber-documentation
(setf (documentation 'window 'type)
 "@version{2025-06-23}
  @begin{short}
    The @class{gtk:window} widget is a toplevel window which can contain other
    widgets.
  @end{short}

  @image[window]{Figure: GtkWindow}

  Windows normally have decorations that are under the control of the windowing
  system and allow the user to manipulate the window, for example, to resize it,
  move it, or close it.
  @begin[GtkWindow as GtkBuildable]{dictionary}
    The @class{gtk:window} implementation of the @class{gtk:buildable} interface
    supports a custom @code{<accel-groups>} element, which supports any number
    of @code{<group>} elements representing the @class{gtk:accel-group} objects
    you want to add to your window. This is synonymous with the
    @fun{gtk:window-add-accel-group} function.

  @b{Example:} A UI definition fragment with accelerator groups
    @begin{pre}
 <object class=\"GtkWindow\">
   <accel-groups>
     <group name=\"accelgroup1\"/>
   </accel-groups>
 </object>
 <!-- -->
   ...
 <!-- -->
 <object class=\"GtkAccelGroup\" id=\"accelgroup1\"/>
    @end{pre}
    The @class{gtk:window} implementation of the @class{gtk:buildable} interface
    supports setting a child as the titlebar by specifying @code{\"titlebar\"}
    as the @code{\"type\"} attribute of a @code{<child>} element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 window.background
 ├── decoration
 ├── <titlebar child>.titlebar [.default-decoration]
 ╰── <child>
    @end{pre}
    The @class{gtk:window} implementation has a main CSS node with name
    @code{window} and @code{.background} style class, and a subnode with name
    @code{decoration}.

    Style classes that are typically used with the main CSS node are
    @code{.csd}, when client-side decorations are in use, @code{.solid-csd},
    for client-side decorations without invisible borders, @code{.ssd}, used by
    mutter when rendering server-side decorations. The @class{gtk:window}
    implementation also represents window states with the following style
    classes on the main node: @code{.tiled}, @code{.maximized},
    @code{.fullscreen}. Specialized types of window often add their own
    discriminating style classes, such as @code{.popup} or @code{.tooltip}.

    The @class{gtk:window} implementation adds the @code{.titlebar} and
    @code{.default-decoration} style classes to the widget that is added as a
    titlebar child.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[window:decoration-button-layout]{property}
      The @code{decoration-button-layout} style property of type
      @code{:string} (Read) @br{}
      The decorated button layout. @br{}
      Default value: @code{\"menu:close\"}
    @end{property}
    @begin[window:decoration-resize-handle]{property}
      The @code{decoration-resize-handle} style property of type @code{:int}
      (Read / Write) @br{}
      The decoration resize handle size. @br{}
      Allowed values: >= 0 @br{}
      Default value: 20
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[window::activate-default]{signal}
      @begin{pre}
lambda (window)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      activates the default widget of the window.
    @end{signal}
    @begin[window::activate-focus]{signal}
      @begin{pre}
lambda (window)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      activates the currently focused widget of the window.
    @end{signal}
    @begin[window::enable-debugging]{signal}
      @begin{pre}
lambda (window toggle)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[window]{The @class{gtk:window} widget on which the signal is
          emitted.}
        @entry[toggle]{The boolean which toggles the debugger.}
        @entry[Returns]{The boolean which is @em{true} if the key binding was
          handled.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user enables
      or disables interactive debugging. When the @arg{toggle} argument is
      @em{true}, interactive debugging is toggled on or off, when it is
      @em{false}, the debugger will be pointed at the widget under the pointer.
      The default bindings for this signal are the @kbd{Ctrl-Shift-I} and
      @kbd{Ctrl-Shift-D} keys.
    @end{signal}
    @begin[window::keys-changed]{signal}
      @begin{pre}
lambda (window)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
      @end{simple-table}
      The signal gets emitted when the set of accelerators or mnemonics that are
      associated with the window changes.
    @end{signal}
    @begin[window::set-focus]{signal}
      @begin{pre}
lambda (window widget)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[window]{The @class{gtk:window} widget which received the signal.}
        @entry[widget]{The newly focused @class{gtk:widget} object.}
      @end{simple-table}
      The signal is emitted whenever the currently focused widget in this window
      changes.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:window-new}
  @see-slot{gtk:window-accept-focus}
  @see-slot{gtk:window-application}
  @see-slot{gtk:window-attached-to}
  @see-slot{gtk:window-decorated}
  @see-slot{gtk:window-default-height}
  @see-slot{gtk:window-default-width}
  @see-slot{gtk:window-deletable}
  @see-slot{gtk:window-destroy-with-parent}
  @see-slot{gtk:window-focus-on-map}
  @see-slot{gtk:window-focus-visible}
  @see-slot{gtk:window-gravity}
  @see-slot{gtk:window-has-resize-grip}
  @see-slot{gtk:window-has-toplevel-focus}
  @see-slot{gtk:window-hide-titlebar-when-maximized}
  @see-slot{gtk:window-icon}
  @see-slot{gtk:window-icon-name}
  @see-slot{gtk:window-is-active}
  @see-slot{gtk:window-mnemonics-visible}
  @see-slot{gtk:window-modal}
  @see-slot{gtk:window-opacity}
  @see-slot{gtk:window-resizable}
  @see-slot{gtk:window-resize-grip-visible}
  @see-slot{gtk:window-role}
  @see-slot{gtk:window-screen}
  @see-slot{gtk:window-skip-pager-hint}
  @see-slot{gtk:window-skip-taskbar-hint}
  @see-slot{gtk:window-startup-id}
  @see-slot{gtk:window-title}
  @see-slot{gtk:window-transient-for}
  @see-slot{gtk:window-type}
  @see-slot{gtk:window-type-hint}
  @see-slot{gtk:window-urgency-hint}
  @see-slot{gtk:window-window-position}
  @see-class{gtk:buildable}
  @see-symbol{gtk:window-type}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:window-accept-focus ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accept-focus" 'window) t)
 "The @code{accept-focus} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should receive the input focus. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-accept-focus)
      "Accessor"
      (documentation 'window-accept-focus 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-accept-focus object) => setting}
  @syntax{(setf (gtk:window-accept-focus object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to let this window receive input focus}
  @begin{short}
    Accessor of the @slot[gtk:window]{accept-focus} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-accept-focus} function gets the value of the hint. The
  @setf{gtk:window-accept-focus} function sets the hint. Windows may set a hint
  asking the desktop environment not to receive the input focus.
  @see-class{gtk:window}")

;;; --- gtk:window-application -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "application" 'window) t)
 "The @code{application} property of type @class{gtk:application} (Read / Write)
  @br{}
  The application associated with the window. The application will be kept
  alive for at least as long as it has any windows associated with it. See the
  @fun{g:application-hold} function for a way to keep it alive without windows.
  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it by setting
  the @slot[gtk:window]{application} property to @code{nil}.")

#+liber-documentation
(setf (liber:alias-for-function 'window-application)
      "Accessor"
      (documentation 'window-application 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-application object) => application}
  @syntax{(setf (gtk:window-application object) application)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[application]{a @class{gtk:application} instance, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:window]{application} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-application} function gets the application associated with
  the window. The @setf{gtk:window-application} function sets or unsets the
  application. The application will be kept alive for at least as long as the
  window is open.
  @see-class{gtk:window}
  @see-class{gtk:application}")

;;; --- gtk:window-attached-to -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attached-to" 'window) t)
 "The @code{attached-to} property of type @class{gtk:widget}
  (Read / Write / Construct) @br{}
  The widget to which this window is attached. Examples of places where
  specifying this relation is useful are for instance a @class{gtk:menu} widget
  created by a @class{gtk:combo-box} widget, a completion popup window created
  by a @class{gtk:entry} widget or a typeahead search entry created by a
  @class{gtk:tree-view} widget.")

#+liber-documentation
(setf (liber:alias-for-function 'window-attached-to)
      "Accessor"
      (documentation 'window-attached-to 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-attached-to object) => widget}
  @syntax{(setf (gtk:window-attached-to object) widget)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[widget]{a @class{gtk:widget} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:window]{attached-to} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-attached-to} function returns the widget where the window
  is attached, or @code{nil} if the window is not attached to any widget. The
  @setf{gtk:window-attached-to} function marks the window as attached to
  @arg{widget}. This creates a logical binding between the window and the widget
  it belongs to, which is used by GTK to propagate information such as styling
  or accessibility to the window as if it was a children of @arg{widget}.

  Examples of places where specifying this relation is useful are for instance
  a @class{gtk:menu} widget created by a @class{gtk:combo-box} widget, a
  completion popup window created by a @class{gtk:entry} widget or a typeahead
  search entry created by a @class{gtk:tree-view} widget.

  Note that this function should not be confused with the
  @fun{gtk:window-transient-for} function, which specifies a window manager
  relation between two toplevels instead.

  Passing @code{nil} for the @arg{widget} argument detaches the window.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:window-transient-for}")

;;; --- gtk:window-decorated ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decorated" 'window) t)
 "The @code{decorated} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window should be decorated by the window manager. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-decorated)
      "Accessor"
      (documentation 'window-decorated 'function)
 "@version{2025-06-12}
  @syntax{(gtk:window-decorated object) => setting}
  @syntax{(setf (gtk:window-decorated object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to decorate the window}
  @begin{short}
    Accessor of the @slot[gtk:window]{decorated} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-decorated} function returns whether the window has been
  set to have decorations such as a title bar.

  By default, windows are decorated with a title bar, resize controls, and so
  on. See the @slot[gtk:settings]{gtk-decoration-layout} setting for more
  information. Some window managers allow GTK to disable these decorations,
  creating a borderless window. If you set the @slot[gtk:window]{decorated}
  property to @em{false} using this function, GTK will do its best to convince
  the window manager not to decorate the window. Depending on the system, this
  function may not have any effect when called on a window that is already
  visible, so you should call it before calling the @fun{gtk:widget-show}
  function.

  On Windows, this function always works, since there is no window manager
  policy involved.
  @see-class{gtk:window}
  @see-function{gtk:widget-show}
  @see-function{gtk:settings-gtk-decoration-layout}")

;;; --- gtk:window-default-height ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-height" 'window) t)
 "The @code{default-height} property of type @code{:int} (Read / Write) @br{}
  The default height of the window, used when initially showing the window.@br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'window-default-height)
      "Accessor"
      (documentation 'window-default-height 'function)
 "@version{2025-06-02}
  @syntax{(gtk:window-default-height object) => height}
  @syntax{(setf (gtk:window-default-height object) height)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[height]{an integer for the default height}
  @begin{short}
    Accessor of the @slot[gtk:window]{default-height} slot of the
    @class{gtk:window} class.
  @end{short}
  The default height of the window, used when initially showing the window.
  See the @fun{gtk:window-default-size} function.
  @see-class{gtk:window}
  @see-function{gtk:window-default-size}")

;;; --- gtk:window-default-width -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-width" 'window) t)
 "The @code{default-width} property of type @code{:int} (Read / Write) @br{}
  The default width of the window, used when initially showing the window. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'window-default-width)
      "Accessor"
      (documentation 'window-default-width 'function)
 "@version{2025-06-02}
  @syntax{(gtk:window-default-width object) => width}
  @syntax{(setf (gtk:window-default-width object) width)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[width]{an integer for the default width}
  @begin{short}
    Accessor of the @slot[gtk:window]{default-width} slot of the
    @class{gtk:window} class.
  @end{short}
  The default width of the window, used when initially showing the window.
  See the @fun{gtk:window-default-size} function.
  @see-class{gtk:window}
  @see-function{gtk:window-default-size}")

;;; --- gtk:window-deletable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "deletable" 'window) t)
 "The @code{deletable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the window frame should have a Close button. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-deletable)
      "Accessor"
      (documentation 'window-deletable 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-deletable object) => setting}
  @syntax{(setf (gtk:window-deletable object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to decorate the window as deletable}
  @begin{short}
    Accessor of the @slot[gtk:window]{deletable} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-deletable} function returns whether the window has been
  set to have a Close button.

  By default, windows have a Close button in the window frame. Some window
  managers allow GTK to disable this button. If you set the
  @slot[gtk:window]{deletable} property to @em{false} using this function, GTK
  will do its best to convince the window manager not to show a Close button.

  Depending on the system, this function may not have any effect when called on
  a window that is already visible, so you should call it before calling
  the @fun{gtk:widget-show} function.

  On Windows, this function always works, since there is no window manager
  policy involved.
  @see-class{gtk:window}
  @see-function{gtk:widget-show}")

;;; --- gtk:window-destroy-with-parent -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "destroy-with-parent" 'window) t)
 "The @code{destroy-with-parent} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this window should be destroyed when the parent is destroyed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-destroy-with-parent)
      "Accessor"
      (documentation 'window-destroy-with-parent 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-destroy-with-parent object) => setting}
  @syntax{(setf (gtk:window-destroy-with-parent object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether to destroy the window with its transient
    parent}
  @begin{short}
    Accessor of the @slot[gtk:window]{destroy-with-parent} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-destroy-with-parent} function returns whether the window
  will be destroyed with its transient parent. The
  @setf{gtk:window-destroy-with-parent} function sets whether to destroy the
  window with its transient parent. If the @arg{setting} argument is @em{true},
  then destroying the transient parent of the window will also destroy the
  window itself.

  This is useful for dialogs that should not persist beyond the lifetime of the
  main window they are associated with.
  @see-class{gtk:window}")

;;; --- gtk:window-focus-on-map ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-on-map" 'window) t)
 "The @code{focus-on-map} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the window should receive the input focus when mapped. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-focus-on-map)
      "Accessor"
      (documentation 'window-focus-on-map 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-focus-on-map object) => setting}
  @syntax{(setf (gtk:window-focus-on-map object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to let this window receive input focus on map}
  @begin{short}
    Accessor of the @slot[gtk:window]{focus-on-map} slot of the
    @class{gtk:window} class.
  @end{short}
  Windows may set a hint asking the desktop environment not to receive the
  input focus when the window is mapped.
  @see-class{gtk:window}")

;;; --- gtk:window-focus-visible -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-visible" 'window) t)
 "The @code{focus-visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether focus rectangles are currently visible in this window. This property
  is maintained by GTK based on the @slot[gtk:settings]{gtk-visible-focus}
  setting and user input and should not be set by applications. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-focus-visible)
      "Accessor"
      (documentation 'window-focus-visible 'function)
 "@version{2024-03-20}
  @syntax{(gtk:window-focus-visible object) => setting}
  @syntax{(setf (gtk:window-focus-visible object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether focus rectangles are currently visible
    in the window}
  @begin{short}
    Accessor of the @slot[gtk:window]{focus-visible} slot of the
    @class{gtk:window} class.
  @end{short}
  Whether focus rectangles are currently visible in the window. This property
  is maintained by GTK based on the @slot[gtk:settings]{gtk-visible-focus}
  setting and user input and should not be set by applications.
  @see-class{gtk:window}")

;;; --- gtk:window-gravity -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gravity" 'window) t)
 "The @code{gravity} property of type @sym{gdk:gravity} (Read / Write) @br{}
  The window gravity of the window. See the @fun{gtk:window-move} function and
  the @sym{gdk:gravity} enumeration for more details about window gravity.
  @br{}
  Default value: @val[gdk:gravity]{:north-west}")

#+liber-documentation
(setf (liber:alias-for-function 'window-gravity)
      "Accessor"
      (documentation 'window-gravity 'function)
 "@version{2025-06-23}
  @syntax{(gtk:window-gravity object) => gravity}
  @syntax{(setf (gtk:window-gravity object) gravity)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[gravity]{a @sym{gdk:gravity} value for the window gravity}
  @begin{short}
    Accessor of the @slot[gtk:window]{gravity} slot of the @class{gtk:window}
    class.
  @end{short}
  Window gravity defines the meaning of coordinates passed to the
  @fun{gtk:window-move} function. See the @fun{gtk:window-move} function and
  the @sym{gdk:gravity} enumeration for more details.

  The default window gravity is @val[gdk:gravity]{:north-west} which will
  typically do what you mean.
  @see-class{gtk:window}
  @see-symbol{gdk:gravity}
  @see-function{gtk:window-move}")

;;; --- gtk:window-has-resize-grip ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-resize-grip" 'window) t)
 "The @code{has-resize-grip} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the window has a corner resize grip. @br{}
  @em{Warning:} The @code{has-resize-grip} property has been deprecated since
  version 3.14. Resize grips have been removed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-has-resize-grip)
      "Accessor"
      (documentation 'window-has-resize-grip 'function)
 "@version{2024-03-20}
  @syntax{(gtk:window-has-resize-grip object) => setting}
  @syntax{(setf (gtk:window-has-resize-grip object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to allow a resize grip}
  @begin{short}
    Accessor of the @slot[gtk:window]{has-resize-grip} slot of the
    @class{gtk:window} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-has-resize-grip} function has been deprecated since
    version 3.14. Resize grips have been removed.
  @end{dictionary}
  @see-class{gtk:window}")

;;; --- gtk:window-has-toplevel-focus ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-toplevel-focus" 'window) t)
 "The @code{has-toplevel-focus} property of type @code{:boolean} (Read) @br{}
  Whether the input focus is within the window. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-has-toplevel-focus)
      "Accessor"
      (documentation 'window-has-toplevel-focus 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-has-toplevel-focus object) => setting}
  @syntax{(setf (gtk:window-has-toplevel-focus object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} if the input focus is within this window}
  @begin{short}
    Accessor of the @slot[gtk:window]{has-toplevel-focus} slot of the
    @class{gtk:window} class.
  @end{short}
  For real toplevel windows, this is identical to the
  @fun{gtk:window-is-active} function, but for embedded windows, like
  a @class{gtk:plug} widget, the results will differ.
  @see-class{gtk:window}
  @see-class{gtk:plug}
  @see-function{gtk:window-is-active}")

;;; --- gtk:window-hide-titlebar-when-maximized --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hide-titlebar-when-maximized"
                                               'window) t)
 "The @code{hide-titlebar-when-maximized} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the titlebar should be hidden during maximization. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-hide-titlebar-when-maximized)
      "Accessor"
      (documentation 'window-hide-titlebar-when-maximized 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-hide-titlebar-when-maximized object) => setting}
  @syntax{(setf (gtk:window-hide-titlebar-when-maximized object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether to hide the titlebar when the window is
    maximized}
  @begin{short}
    Accessor of the @slot[gtk:window]{hide-titlebar-when-maximized} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-hide-titlebar-when-maximized} function returns whether
  the window has requested to have its titlebar hidden when maximized. The
  @setf{gtk:window-hide-titlebar-when-maximized} function sets whether to hide
  the titlebar when the window is maximized.

  If the @arg{setting} argument is @em{true}, then the window will request that
  its titlebar should be hidden when maximized. This is useful for windows that
  do not convey any information other than the application name in the
  titlebar, to put the available screen space to better use. If the underlying
  window system does not support the request, the setting will not have any
  effect.
  @see-class{gtk:window}")

;;; --- gtk:window-icon --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon" 'window) t)
 "The @code{icon} property of type @class{gdk-pixbuf:pixbuf} (Read / Write)
  @br{}
  The icon for the window.")

#+liber-documentation
(setf (liber:alias-for-function 'window-icon)
      "Accessor"
      (documentation 'window-icon 'function)
 "@version{2025-06-03}
  @syntax{(gtk:window-icon object) => icon}
  @syntax{(setf (gtk:window-icon object) icon)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[icon]{a @class{gdk-pixbuf:pixbuf} object for the icon image, or
    @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:window]{icon} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-icon} function gets the icon or if you have called the
  @fun{gtk:window-icon-list} function, gets the first icon in the icon list.
  The @setf{gtk:window-icon} function sets up the icon representing the window.
  This icon is used when the window is minimized, also known as iconified. Some
  window managers or desktop environments may also place it in the window frame,
  or display it in other contexts.

  The icon should be provided in whatever size it was naturally drawn. That is,
  do not scale the image before passing it to GTK. Scaling is postponed until
  the last minute, when the desired final size is known, to allow best quality.

  If you have your icon hand drawn in multiple sizes, use the
  @fun{gtk:window-icon-list} function. Then the best size will be used. This
  function is equivalent to calling the @fun{gtk:window-icon-list} function
  with a 1-element list.

  See also the @fun{gtk:window-default-icon-list} function to set the icon
  for all windows in your application in one go.
  @see-class{gtk:window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:window-icon-list}
  @see-function{gtk:window-default-icon-list}")

;;; --- gtk:window-icon-name ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'window) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  Specifies the name of the themed icon to use as the window icon. See the
  @class{gtk:icon-theme} documentation for more details. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-icon-name)
      "Accessor"
      (documentation 'window-icon-name 'function)
 "@version{2025-06-02}
  @syntax{(gtk:window-icon-name object) => name}
  @syntax{(setf (gtk:window-icon-name object) name)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[name]{a string for the name of the themed icon}
  @begin{short}
    Accessor of the @slot[gtk:window]{icon-name} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-icon-name} function returns the name of the themed icon
  for the window. The @setf{gtk:window-icon-name} function sets the icon for
  the window from a named themed icon. See the @class{gtk:icon-theme}
  documentation for more details.
  @see-class{gtk:window}
  @see-class{gtk:icon-theme}")

;;; --- gtk:window-is-active ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-active" 'window) t)
 "The @code{is-active} property of type @code{:boolean} (Read) @br{}
  Whether the toplevel is the current active window. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-is-active)
      "Accessor"
      (documentation 'window-is-active 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-is-active object) => active}
  @syntax{(setf (gtk:window-is-active object) active)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[active]{a boolean whether the window is part of the current active
    window}
  @begin{short}
    Accessor of the @slot[gtk:window]{is-active} slot of the @class{gtk:window}
    class.
  @end{short}
  That is, the toplevel window receiving keystrokes. The return value is
  @em{true} if the window is active toplevel itself, but also if it is, say, a
  @class{gtk:plug} widget embedded in the active toplevel. You might use this
  function if you wanted to draw a widget differently in an active window from a
  widget in an inactive window. See the @fun{gtk:window-has-toplevel-focus}
  function.
  @see-class{gtk:window}
  @see-class{gtk:plug}
  @see-function{gtk:window-has-toplevel-focus}")

;;; --- gtk:window-is-maximized ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-maximized" 'window) t)
 "The @code{is-maximized} property of type @code{:boolean} (Read) @br{}
  Whether the window is maximized. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-is-maximized)
      "Accessor"
      (documentation 'window-is-maximized 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-is-maximized object) => maximized}
  @syntax{(setf (gtk:window-is-maximized object) maximized)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[maximized]{a boolean whether the window has a maximized state}
  @begin{short}
    Accessor of the @slot[gtk:window]{is-maximized} slot of the
    @class{gtk:window} class.
  @end{short}
  Note that since maximization is ultimately handled by the window manager and
  happens asynchronously to an application request, you should not assume the
  return value of this function changing immediately, or at all, as an effect
  of calling the @fun{gtk:window-maximize} or @fun{gtk:window-unmaximize}
  functions.
  @see-class{gtk:window}
  @see-function{gtk:window-maximize}
  @see-function{gtk:window-unmaximize}")

;;; --- gtk:window-mnemonics-visible -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonics-visible" 'window) t)
 "The @code{mnemonics-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether mnemonics are currently visible in the window. This property is
  maintained by GTK based on the @slot[gtk:settings]{gtk-auto-mnemonics}
  setting and user input, and should not be set by applications. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-mnemonics-visible)
      "Accessor"
      (documentation 'window-mnemonics-visible 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-mnemonics-visible object) => setting}
  @syntax{(setf (gtk:window-mnemonics-visible object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether mnemonics are currently visible in the
    window}
  @begin{short}
    Accessor of the @slot[gtk:window]{mnemonics-visible} slot of the
    @class{gtk:window} class.
  @end{short}
  Whether mnemonics are currently visible in the window. This property is
  maintained by GTK based on the @slot[gtk:settings]{gtk-auto-mnemonics}
  setting and user input, and should not be set by applications.
  @see-class{gtk:window}
  @see-function{gtk:settings-gtk-auto-mnemonics}")

;;; --- gtk:window-modal -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'window) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, the window is modal. Other windows are not usable while this
  one is up. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-modal)
      "Accessor"
      (documentation 'window-modal 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-modal object) => modal}
  @syntax{(setf (gtk:window-modal object) modal)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[modal]{a boolean whether the window is modal}
  @begin{short}
    Accessor of the @slot[gtk:window]{modal} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-modal} function returns @em{true} if the window is set to
  be modal and establishes a grab when shown. The @setf{gtk:window-modal}
  function sets a window modal or non-modal.

  Modal windows prevent interaction with other windows in the same application.
  To keep modal dialogs on top of main application windows, use the
  @fun{gtk:window-transient-for} function to make the dialog transient for the
  parent. Most window managers will then disallow lowering the dialog below the
  parent.
  @see-class{gtk:window}
  @see-function{gtk:window-transient-for}")

;;; --- gtk:window-opacity -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "opacity" 'window) t)
 "The @code{opacity} property of type @code{:double} (Read / Write) @br{}
  The requested opacity of the window. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'window-opacity)
      "Accessor"
      (documentation 'window-opacity 'function)
 "@version{2025-06-02}
  @syntax{(gtk:window-opacity object) => opacity}
  @syntax{(setf (gtk:window-opacity object) opacity)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[opacity]{a number coerced to a double float for the desired opacity,
    between 0.0 and 1.0}
  @begin{short}
    Accessor of the @slot[gtk:window]{opacity} slot of the @class{gtk:window}
    class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-opacity} function has been deprecated since version 3.8.
    Use the @fun{gtk:widget-opacity} function instead.
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:widget-opacity}")

;;; --- gtk:window-resizable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resizable" 'window) t)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, users can resize the window. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'window-resizable)
      "Accessor"
      (documentation 'window-resizable 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-resizable object) => resizable}
  @syntax{(setf (gtk:window-resizable object) resizable)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[resizable]{@em{true} if the user can resize this window}
  @begin{short}
    Accessor of the @slot[gtk:window]{resizable} slot of the @class{gtk:window}
    class.
  @end{short}
  Sets whether the user can resize a window. Windows are user resizable by
  default.
  @see-class{gtk:window}")

;;; --- gtk:window-resize-grip-visible -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resize-grip-visible" 'window) t)
 "The @code{resize-grip-visible} property of type @code{:boolean} (Read) @br{}
  Whether a corner resize grip is currently shown. @br{}
  @em{Warning:} The @code{resize-grip-visible} property has been deprecated
  since version 3.14. Resize grips have been removed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-resize-grip-visible)
      "Accessor"
      (documentation 'window-resize-grip-visible 'function)
 "@version{2024-03-20}
  @syntax{(gtk:window-resize-grip-visible object) => setting}
  @syntax{(setf (gtk:window-resize-grip-visible object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether a corner resize grip is shown}
  @begin{short}
    Accessor of the @slot[gtk:window]{resize-grip-visible} slot of the
    @class{gtk:window} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-resize-grip-visible} function has been deprecated
    since version 3.14. Resize grips have been removed.
  @end{dictionary}
  @see-class{gtk:window}")

;;; --- gtk:window-role --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "role" 'window) t)
 "The @code{role} property of type @code{:string} (Read / Write) @br{}
  The unique identifier for the window to be used when restoring a session.@br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-role)
      "Accessor"
      (documentation 'window-role 'function)
 "@version{2025-06-02}
  @syntax{(gtk:window-role object) => role}
  @syntax{(setf (gtk:window-role object) role)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[role]{a string for an unique identifier for the window to be used
    when restoring a session}
  @begin{short}
    Accessor of the @slot[gtk:window]{role} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-role} function returns the role of the window if set, or
  @code{nil}. The @setf{gtk:window-role} function sets an unique identifier for
  the window to be used when restoring a session. This function is only useful
  on X11, not with other GTK targets.

  In combination with the window title, the window role allows a window
  manager to identify \"the same\" window when an application is restarted. So
  for example you might set the \"toolbox\" role on the toolbox window of the
  application, so that when the user restarts the session, the window manager
  can put the toolbox back in the same place.

  If a window already has a unique title, you do not need to set the role,
  since the window manager can use the title to identify the window when
  restoring the session.
  @see-class{gtk:window}")

;;; --- gtk:window-screen ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "screen" 'window) t)
 "The @code{screen} property of type @class{gdk:screen} (Read / Write) @br{}
  The screen where this window will be displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'window-screen)
      "Accessor"
      (documentation 'window-screen 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-screen object) => screen}
  @syntax{(setf (gtk:window-screen object) screen)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Accessor of the @slot[gtk:window]{screen} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-screen} function returns the screen associated with the
  window. The @setf{gtk:window-screen} function sets the screen where the
  window is displayed. If the window is already mapped, it will be unmapped,
  and then remapped on the new screen.
  @see-class{gtk:window}
  @see-class{gdk:screen}")

;;; --- gtk:window-skip-pager-hint ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "skip-pager-hint" 'window) t)
 "The @code{skip-pager-hint} property of type @code{:boolean} (Read / Write)
  @br{}
  @em{True} if the window should not be in the pager. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-skip-pager-hint)
      "Accessor"
      (documentation 'window-skip-pager-hint 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-skip-pager-hint object) => setting}
  @syntax{(setf (gtk:window-skip-pager-hint object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to keep this window from appearing in the pager}
  @begin{short}
    Accessor of the @slot[gtk:window]{skip-pager-hint} slot of the
    @class{gtk:window} class.
  @end{short}
  Windows may set a hint asking the desktop environment not to display the
  window in the pager. This function sets this hint.

  A \"pager\" is any desktop navigation tool such as a workspace switcher that
  displays a thumbnail representation of the windows on the screen.
  @see-class{gtk:window}")

;;; --- gtk:window-skip-taskbar-hint -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "skip-taskbar-hint" 'window) t)
 "The @code{skip-taskbar-hint} property of type @code{:boolean} (Read / Write)
  @br{}
  @em{True} if the window should not be in the task bar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-skip-taskbar-hint)
      "Accessor"
      (documentation 'window-skip-taskbar-hint 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-skip-taskbar-hint object) => setting}
  @syntax{(setf (gtk:window-skip-taskbar-hint object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to keep this window from appearing in the task
    bar}
  @begin{short}
    Accessor of the @slot[gtk:window]{skip-taskbar-hint} slot of the
    @class{gtk:window} class.
  @end{short}
  Windows may set a hint asking the desktop environment not to display the
  window in the task bar. This function sets this hint.
  @see-class{gtk:window}")

;;; --- gtk:window-startup-id --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "startup-id" 'window) t)
 "The @code{startup-id} property of type @code{:string} (Write) @br{}
  The write-only property for setting the startup notification identifier of
  the window. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-startup-id)
      "Accessor"
      (documentation 'window-startup-id 'function)
 "@version{2025-06-02}
  @syntax{(setf (gtk:window-startup-id object) id)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[id]{a string for the startup ID}
  @begin{short}
    Accessor of the @slot[gtk:window]{startup-id} slot of the @class{gtk:window}
    class.
  @end{short}
  The @setf{gtk:window-startup-id} function sets a string with the startup
  notification identifier.

  Startup notification identifiers are used by the desktop environment to track
  application startup, to provide user feedback and other features. This
  function changes the corresponding property on the underlying
  @class{gdk:window} object. Normally, startup identifier is managed
  automatically and you should only use this function in special cases like
  transferring focus from other processes. You should use this function before
  calling the @fun{gtk:window-present} function or any equivalent function
  generating a window map event.

  This function is only useful on X11, not with other GTK targets.
  @see-class{gtk:window}
  @see-class{gdk:window}
  @see-function{gtk:window-present}")

;;; --- gtk:window-title -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'window) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the window. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'window-title)
      "Accessor"
      (documentation 'window-title 'function)
 "@version{2025-06-02}
  @syntax{(gtk:window-title object) => title}
  @syntax{(setf (gtk:window-title object) title)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[title]{a string for the title of the window}
  @begin{short}
    Accessor of the @slot[gtk:window]{title} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-title} function retrieves the title of the window. The
  @setf{gtk:window-title} function sets the title.

  The title of a window will be displayed in the title bar. On the X11 Window
  System, the title bar is rendered by the window manager, so exactly how the
  title appears to users may vary according to the exact configuration. The
  title should help a user distinguish this window from other windows they
  may have open. A good title might include the application name and current
  document filename.
  @see-class{gtk:window}")

;;; --- gtk:window-transient-for -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transient-for" 'window) t)
 "The @code{transient-for} property of type @class{gtk:window}
  (Read / Write / Construct) @br{}
  The transient parent of the window.")

#+liber-documentation
(setf (liber:alias-for-function 'window-transient-for)
      "Accessor"
      (documentation 'window-transient-for 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-transient-for object) => parent}
  @syntax{(setf (gtk:window-transient-for object) parent)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[parent]{a @class{gtk:window} parent window, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:window]{transient-for} slot of the
    @class{gtk:window} class.
  @end{short}
  The @fun{gtk:window-transient-for} function returns the transient parent for
  the window, or @code{nil} if no transient parent has been set. The
  @setf{gtk:window-transient-for} function sets the parent window.

  Dialogs should be set transient for the main application window they were
  spawned from. This allows window managers to, for example, keep the dialog on
  top of the main window, or center the dialog over the main window. The
  @fun{gtk:dialog-new-with-buttons} function and other convenience functions
  in GTK will sometimes call the @fun{gtk:window-transient-for} function on
  your behalf.

  Passing @code{nil} for the @arg{parent} argument unsets the current transient
  window.

  On Windows, this function puts the child window on top of the parent, much
  as the window manager would have done on X11.
  @see-class{gtk:window}
  @see-function{gtk:dialog-new-with-buttons}")

;;; --- gtk:window-type --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "type" 'window) t)
 "The @code{type} property of type @sym{gtk:window-type}
 (Read / Construct only) @br{}
  The type of the window. @br{}
  Default value: @val[gtk:window-type]{:toplevel}")

#+liber-documentation
(setf (liber:alias-for-function 'window-type)
      "Accessor"
      (documentation 'window-type 'function)
 "@version{2025-06-23}
  @syntax{(gtk:window-type object) => type}
  @argument[object]{a @class{gtk:window} widget}
  @argument[type]{a value of the @sym{gtk:window-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:window]{type} slot of the @class{gtk:window}
    class.
  @end{short}
  The type of the window cannot be set after construction.
  @see-class{gtk:window}
  @see-symbol{gtk:window-type}")

;;; --- gtk:window-type-hint ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "type-hint" 'window) t)
 "The @code{type-hint} property of type @sym{gdk:window-type-hint}
  (Read / Write) @br{}
  The hint to help the desktop environment understand what kind of window this
  is and how to treat it. @br{}
  Default value: @val[gdk:window-type-hint]{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'window-type-hint)
      "Accessor"
      (documentation 'window-type-hint 'function)
 "@version{2025-06-23}
  @syntax{(gtk:window-type-hint object) => hint}
  @syntax{(setf (gtk:window-type-hint object) hint)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[hint]{a value of the @sym{gdk:window-type-hint} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:window]{type-hint} slot of the @class{gtk:window}
    class.
  @end{short}
  The @fun{gtk:window-type-hint} function gets the type hint for the window. The
  @setf{gtk:window-type-hint} function sets the type hint. By setting the type
  hint for the window, you allow the window manager to decorate and handle the
  window in a way which is suitable to the function of the window in your
  application.

  This function should be called before the window becomes visible.

  The @fun{gtk:dialog-new-with-buttons} function and other convenience functions
  in GTK will sometimes call this function on your behalf.
  @see-class{gtk:window}
  @see-symbol{gtk:window-type-hint}
  @see-function{gtk:dialog-new-with-buttons}")

;;; --- gtk:window-urgency-hint ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "urgency-hint" 'window) t)
 "The @code{urgency-hint} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the window should be brought to the attention of the user. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'window-urgency-hint)
      "Accessor"
      (documentation 'window-urgency-hint 'function)
 "@version{2024-03-16}
  @syntax{(gtk:window-urgency-hint object) => setting}
  @syntax{(setf (gtk:window-urgency-hint object) setting)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[setting]{@em{true} to mark this window as urgent}
  @begin{short}
    Accessor of the @slot[gtk:window]{urgency-hint} slot of the
    @class{gtk:window} class.
  @end{short}
  Windows may set a hint asking the desktop environment to draw the users
  attention to the window. This function sets this hint.
  @see-class{gtk:window}")

;;; --- gtk:window-window-position ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "window-position" 'window) t)
 "The @code{window-position} property of type @sym{gtk:window-position}
  (Read / Write) @br{}
  The initial position of the window. @br{}
  Default value: @val[gtk:window-position]{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'window-window-position)
      "Accessor"
      (documentation 'window-window-position 'function)
 "@version{2025-06-23}
  @syntax{(gtk:window-window-position object) => position}
  @syntax{(setf (gtk:window-window-position object) position)}
  @argument[object]{a @class{gtk:window} widget}
  @argument[position]{a value of the @sym{gtk:window-position} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:window]{window-position} slot of the
    @class{gtk:window} class.
  @end{short}
  The @setf{gtk:window-window-position} function sets a position contraint for
  this window. If the old or new constraint is the
  @val[gtk:window-position]{:center-always} value, this will also cause the
  window to be repositioned to satisfy the new constraint.
  @see-class{gtk:window}
  @see-symbol{gtk:window-position}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_new
;;; ----------------------------------------------------------------------------

(declaim (inline window-new))

(defun window-new (wtype)
 #+liber-documentation
 "@version{2025-06-23}
  @argument[wtype]{a value of the @sym{gtk:window-type} enumeration}
  @return{The new @class{gtk:window} widget.}
  @begin{short}
    Creates a new window, which is a toplevel window that can contain other
    widgets.
  @end{short}
  Nearly always, the type of the window should be
  @val[gtk:window-type]{:toplevel}. If you are implementing something like a
  popup menu from scratch, which is a bad idea, just use the @class{gtk:menu}
  widget, you might use the @val[gtk:window-type]{:popup} type. The
  @val[gtk:window-type]{:popup} type is not for dialogs, though in some other
  toolkits dialogs are called \"popups\". In GTK, the
  @val[gtk:window-type]{:popup} type means a popup menu or popup tooltip. On
  X11, popup windows are not controlled by the window manager.

  If you simply want an undecorated window with no window borders, use the
  @fun{gtk:window-decorated} function, do not use the
  @val[gtk:window-type]{:popup} type.
  @see-class{gtk:window}
  @see-symbol{gtk:window-type}
  @see-function{gtk:window-decorated}"
  (make-instance 'window :type wtype))

(export 'window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_wmclass                                  Deprecated 3.22
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_accel_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_add_accel_group" window-add-accel-group) :void
 #+liber-documentation
 "@version{2024-03-16}
  @argument[window]{a @class{gtk:window} widget to attach the accelerator
    group to}
  @argument[group]{a @class{gtk:accel-group} object}
  @begin{short}
    Associate an accelerator group with the window, such that calling the
    @fun{gtk:accel-group-activate} function on the window will activate
    accelerators in the accelerator group.
  @end{short}
  @see-class{gtk:window}
  @see-function{gtk:accel-group-activate}"
  (window (g:object window))
  (group (g:object accel-group)))

(export 'window-add-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_accel_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_remove_accel_group" window-remove-accel-group) :void
 #+liber-documentation
 "@version{2024-03-16}
  @argument[window]{a @class{gtk:window} widget}
  @argument[group]{a @class{gtk:accel-group} object}
  @begin{short}
    Reverses the effects of the @fun{gtk:window-add-accel-group} function.
  @end{short}
  @see-class{gtk:window}
  @see-function{gtk:window-add-accel-group}"
  (window (g:object window))
  (group (g:object accel-group)))

(export 'window-remove-accel-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_activate_focus" window-activate-focus) :boolean
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @return{@em{True} if a widget got activated.}
  @begin{short}
    Activates the current focused widget within the window.
  @end{short}
  @see-class{gtk:window}
  @see-function{gtk:window-activate-default}"
  (window (g:object window)))

(export 'window-activate-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_activate_default" window-activate-default) :boolean
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @return{@em{True} if a widget got activated.}
  @begin{short}
    Activates the default widget for the window, unless the current focused
    widget has been configured to receive the default action.
  @end{short}
  See the @fun{gtk:widget-receives-default} function, in which case the focused
  widget is activated.
  @see-class{gtk:window}
  @see-function{gtk:window-activate-focus}
  @see-function{gtk:widget-receives-default}"
  (window (g:object window)))

(export 'window-activate-default)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_size
;;; gtk_window_get_default_size
;;; ----------------------------------------------------------------------------

(defun (setf window-default-size) (size window)
  (destructuring-bind (width height) size
     (values (setf (window-default-width window) width)
             (setf (window-default-height window) height))))

(defun window-default-size (window)
 #+liber-documentation
 "@version{2025-06-02}
  @syntax{(gtk:window-default-size window) => width, height}
  @syntax{(setf (gtk:window-default-size window) (list width height))}
  @argument[window]{a @class{gtk:window} widget}
  @argument[width]{an integer for the default width of the window}
  @argument[height]{an integer for the default height of the window}
  @begin{short}
    The @fun{gtk:window-default-size} function gets the default size of the
    window.
  @end{short}
  The @setf{gtk:window-default-size} function sets the default size. A value of
  -1 for the width or height indicates that a default size has not been
  explicitly set for that dimension, so the \"natural\" size of the window will
  be used.

  If the \"natural\" size of the window, its size request, is larger than the
  default, the default will be ignored. More generally, if the default size
  does not obey the geometry hints for the window, the
  @fun{gtk:window-set-geometry-hints} function can be used to set these
  explicitly, the default size will be clamped to the nearest permitted size.

  Unlike the @fun{gtk:widget-size-request} function, which sets a size request
  for a widget and thus would keep users from shrinking the window, this
  function only sets the initial size, just as if the user had resized the
  window themselves. Users can still shrink the window again as they normally
  would. Setting a default size of -1 means to use the \"natural\" default
  size, the size request of the window.

  For more control over the initial size of the window and how resizing works,
  investigate the @fun{gtk:window-set-geometry-hints} function.

  For some uses, the @fun{gtk:window-resize} function is a more appropriate
  function. The @fun{gtk:window-resize} function changes the current size of
  the window, rather than the size to be used on initial display. The
  @fun{gtk:window-resize} function always affects the window itself, not the
  geometry widget.

  The default size of a window only affects the first time a window is shown.
  If a window is hidden and re-shown, it will remember the size it had prior
  to hiding, rather than using the default size.

  Windows cannot actually be 0 x 0 in size, they must be at least 1 x 1, but
  passing 0 for width and height is fine, resulting in a 1 x 1 default size.
  @begin[Examples]{dictionary}
   @begin{pre}
(let ((window (make-instance 'gtk:window)))
  (setf (gtk:window-default-size window) '(300 200))
  (gtk:window-default-size window))
=> 300, 200
    @end{pre}
    In the Lisp implementation, it is more convenient to set the default size
    when constructing the window
    @begin{pre}
(let ((window (make-instance 'gtk:window
                             :default-width 300
                             :default-height 200)))
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:window-resize}
  @see-function{gtk:window-default-width}
  @see-function{gtk:window-default-height}
  @see-function{gtk:widget-size-request}
  @see-function{gtk:window-set-geometry-hints}"
  (values (window-default-width window)
          (window-default-height window)))

(export 'window-default-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_geometry
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implemenation

(cffi:defcfun ("gtk_window_set_default_geometry" window-set-default-geometry)
    :void
 #+liber-documentation
 "@version{2024-03-20}
  @short{This function does nothing and is deprectated since 3.20.}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-set-default-geometry} function has been deprecated
    since version 3.20. This function does nothing. If you want to set a
    default size, use the @fun{gtk:window-default-size} function instead.
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:window-default-size}"
  (window (g:object window))
  (width :int)
  (height :int))

(export 'window-set-default-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_geometry_hints
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_geometry_hints" %window-set-geometry-hints) :void
  (window (g:object window))
  (widget (g:object widget))
  (geometry (:pointer (:struct gdk:geometry)))
  (mask gdk:window-hints))

(defun window-set-geometry-hints (window geometry mask)
 #+liber-documentation
 "@version{2024-06-23}
  @argument[window]{a @class{gtk:window} widget}
  @argument[geometry]{a @sym{gdk:geometry} instance containing geometry
    information}
  @argument[mask]{a @sym{gdk:window-hints} mask indicating which geometry
    structure fields should be paid attention to}
  @begin{short}
    This function sets up hints about how a window can be resized by the user.
  @end{short}
  You can set a minimum and maximum size. Allowed resize increments, for
  example for xterm, you can only resize by the size of a character, aspect
  ratios, and more. See the @sym{gdk:geometry} structure.
  @begin[Notes]{dictionary}
    In the Lisp implementation an unused widget argument is omitted.
  @end{dictionary}
  @see-class{gtk:window}
  @see-symbol{gdk:geometry}
  @see-symbol{gdk:window-hints}"
  (%window-set-geometry-hints window nil geometry mask))

(export 'window-set-geometry-hints)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_position
;;; ----------------------------------------------------------------------------

;; Implemented as (setf window-window-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_list_toplevels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_list_toplevels" window-list-toplevels)
    (g:list-t (g:object window))
 #+liber-documentation
 "@version{2024-03-16}
  @return{List of toplevel @class{gtk:widget} objects.}
  @short{Returns a list of all existing toplevel windows.}
  @see-class{gtk:window}
  @see-class{gtk:widget}")

(export 'window-list-toplevels)

;;; ----------------------------------------------------------------------------
;;; gtk_window_add_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_add_mnemonic" window-add-mnemonic) :void
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[keyval]{an unsigned integer for the mnemonic}
  @argument[target]{a @class{gtk:widget} object that gets activated by the
    mnemonic}
  @begin{short}
    Adds a mnemonic to the window.
  @end{short}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:window-remove-mnemonic}
  @see-function{gtk:window-mnemonic-activate}"
  (window (g:object window))
  (keyval :uint)
  (target (g:object widget)))

(export 'window-add-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_remove_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_remove_mnemonic" window-remove-mnemonic) :void
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[keyval]{an unsigned integer for the mnemonic}
  @argument[target]{a @class{gtk:widget} object that gets activated by the
    mnemonic}
  @begin{short}
    Removes a mnemonic from the window.
  @end{short}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:window-add-mnemonic}"
  (window (g:object window))
  (keyval :uint)
  (target (g:object widget)))

(export 'window-remove-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_window_mnemonic_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_mnemonic_activate" window-mnemonic-activate) :boolean
 #+liber-documentation
 "@version{#2025-06-23}
  @argument[window]{a @class{gtk:window} widget}
  @argument[keyval]{an unsigned integer for the mnemonic}
  @argument[modifier]{a @sym{gdk:modifier-type} value with the modifiers}
  @return{@em{True} if the activation is done.}
  @begin{short}
    Activates the targets associated with the mnemonic.
  @end{short}
  @see-class{gtk:window}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:window-add-mnemonic}"
  (window (g:object window))
  (keyval :uint)
  (modifier gdk:modifier-type))

(export 'window-mnemonic-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_window_activate_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_activate_key" window-activate-key) :boolean
 #+liber-documentation
 "@version{#2025-06-23}
  @argument[window]{a @class{gtk:window} widget}
  @argument[event]{a @class{gdk:event-key} event}
  @return{@em{True} if a mnemonic or accelerator was found and activated.}
  @begin{short}
    Activates mnemonics and accelerators for the window.
  @end{short}
  This is normally called by the default @sig[gtk:widget]{key-press-event}
  signal handler for toplevel windows, however in some cases it may be useful
  to call this directly when overriding the standard key handling for a toplevel
  window.
  @see-class{gtk:window}
  @see-class{gdk:event-key}"
  (window (g:object window))
  (event (g:boxed gdk:event)))

(export 'window-activate-key)

;;; ----------------------------------------------------------------------------
;;; gtk_window_propagate_key_event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_propagate_key_event" window-propagate-key-event)
    :boolean
 #+liber-documentation
 "@version{#2025-06-23}
  @argument[window]{a @class{gtk:window} widget}
  @argument[event]{a @class{gdk:event-key} event}
  @return{@em{True} if a widget in the focus chain handled the event.}
  @begin{short}
    Propagate a key press or release event to the focus widget and up the focus
    container chain until a widget handles the event.
  @end{short}
  This is normally called by the default @sig[gtk:widget]{key-press-event} and
  @sig[gtk:widget]{key-release-event} signal handlers for toplevel windows,
  however in some cases it may be useful to call this directly when overriding
  the standard key handling for a toplevel window.
  @see-class{gtk:window}
  @see-class{gdk:event-key}"
  (window (g:object window))
  (event (g:boxed gdk:event)))

(export 'window-propagate-key-event)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_focus
;;; gtk_window_set_focus
;;; ----------------------------------------------------------------------------

(defun (setf window-focus) (focus window)
  (cffi:foreign-funcall "gtk_window_set_focus"
                        (g:object window) window
                        (g:object widget) focus
                        :void)
  focus)

(cffi:defcfun ("gtk_window_get_focus" window-focus) (g:object widget)
 #+liber-documentation
 "@version{#2024-03-20}
  @syntax{(gtk:window-focus window) => focus}
  @syntax{(setf (gtk:window-focus window) focus)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[focus]{a @class{gtk:widget} object to be the focus widget, or
    @code{nil} to unset any focus widget for the toplevel window}
  @begin{short}
    The @fun{gtk:window-focus} function retrieves the current focused widget
    within the window.
  @end{short}
  If the @arg{focus} argument is not the current focus widget, and is focusable,
  the @setf{gtk:window-focus} function sets it as the focus widget for the
  window.

  If the @arg{focus} argument is @code{nil}, unsets the focus widget for the
  window. To set the focus to a particular widget in the toplevel, it is usually
  more convenient to use the @fun{gtk:widget-grab-focus} function instead of
  this function.

  Note that this is the widget that would have the focus if the toplevel window
  focused. If the toplevel window is not focused then the
  @fun{gtk:widget-has-focus} function will not return @em{true} for the widget.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-focus}
  @see-function{gtk:widget-has-focus}"
  (window (g:object window)))

(export 'window-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_widget
;;; gtk_window_set_default
;;; ----------------------------------------------------------------------------

(defun (setf window-default-widget) (widget window)
  (cffi:foreign-funcall "gtk_window_set_default"
                        (g:object window) window
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_window_get_default_widget" window-default-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-03}
  @syntax{(gtk:window-default-widget window) => widget}
  @syntax{(setf (gtk:window-default-widget window) widget)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[widget]{a @class{gtk:widget} object to be the default, or
    @code{nil} to unset the default widget for the toplevel}
  @begin{short}
    The @fun{gtk:window-default-widget} function returns the default widget for
    the window.
  @end{short}
  The @setf{gtk:window-default-widget} function sets or unsets the default
  widget.

  The default widget is the widget that is activated when the user presses the
  @kbd{Enter} key in a dialog for example. When setting, rather than unsetting,
  the default widget it is generally easier to call the
  @fun{gtk:widget-grab-focus} function on the widget. Before making a widget the
  default widget, you must call the @fun{gtk:widget-can-default} function on the
  widget you would like to make the default.
  @begin[Notes]{dictionary}
    The C library has the @code{gtk_window_set_default()} function, which is
    implemented as the @setf{gtk:window-default-widget} function in the Lisp
    API.
  @end{dictionary}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:widget-can-default}
  @see-function{gtk:widget-grab-focus}"
  (window (g:object window)))

(export 'window-default-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_present" window-present) :void
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Presents a window to the user.
  @end{short}
  This may mean raising the window in the stacking order, deiconifying it,
  moving it to the current desktop, and/or giving it the keyboard focus,
  possibly dependent on the platform, window manager, and preferences of the
  user.

  If the window is hidden, this function calls the @fun{gtk:widget-show}
  function as well.

  This function should be used when the user tries to open a window that is
  already open. Say for example the preferences dialog is currently open, and
  the user chooses Preferences from the menu a second time. Use this function
  to move the already open dialog where the user can see it.

  If you are calling this function in response to a user interaction, it is
  preferable to use the @fun{gtk:window-present-with-time} function.
  @see-class{gtk:window}
  @see-function{gtk:widget-show}
  @see-function{gtk:window-present-with-time}"
  (window (g:object window)))

(export 'window-present)

;;; ----------------------------------------------------------------------------
;;; gtk_window_present_with_time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_present_with_time" window-present-with-time) :void
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[timestamp]{an unsigned integer for the timestamp of the user
    interaction, typically a button or key press event, which triggered this
    call}
  @begin{short}
    Presents a window to the user in response to a user interaction.
  @end{short}
  If you need to present a window without a timestamp, use the
  @fun{gtk:window-present} function.
  @see-class{gtk:window}
  @see-function{gtk:window-present}"
  (window (g:object window))
  (timestamp :uint32))

(export 'window-present-with-time)

;;; ----------------------------------------------------------------------------
;;; gtk_window_close
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_close" window-close) :void
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Requests that the window is closed, similar to what happens when a window
    manager Close button is clicked.
  @end{short}
  This function can be used with Close buttons in custom titlebars.
  @see-class{gtk:window}"
  (window (g:object window)))

(export 'window-close)

;;; ----------------------------------------------------------------------------
;;; gtk_window_iconify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_iconify" window-iconify) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to iconify, for example, minimize, the specified window.
  @end{short}
  Note that you should not assume the window is definitely iconified afterward,
  because other entities, for example, the user or window manager, could
  deiconify it again, or there may not be a window manager in which case
  iconification is not possible, etc. But normally the window will end up
  iconified. Just do not write code that crashes if not.

  It is permitted to call this function before showing a window, in which case
  the window will be iconified before it ever appears onscreen.

  You can track iconification via the @sig[gtk:widget]{window-state-event}
  signal on the @class{gtk:widget} object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-iconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_deiconify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_deiconify" window-deiconify) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to deiconify, for example, unminimize, the specified window.
  @end{short}
  Note that you should not assume the window is definitely deiconified
  afterward, because other entities, for example, the user or window manager,
  could iconify it again before your code which assumes deiconification gets to
  run.

  You can track iconification via the @sig[gtk:widget]{window-state-event}
  signal on the @class{gtk:widget} object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gtk_window_stick
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_stick" window-stick) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to stick the window, which means that it will appear on all user
    desktops.
  @end{short}
  Note that you should not assume the window is definitely stuck afterward,
  because other entities, for example, the user or window manager, could unstick
  it again, and some window managers do not support sticking windows. But
  normally the window will end up stuck. Just do not write code that crashes if
  not.

  It is permitted to call this function before showing a window.

  You can track stickiness via the @sig[gtk:widget]{window-state-event} signal
  on the @class{gtk:widget} object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-stick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unstick
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_unstick" window-unstick) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to unstick the window, which means that it will appear on only one of
    the desktops of the user.
  @end{short}
  Note that you should not assume the window is definitely unstuck afterward,
  because other entities, for example, the user or window manager, could stick
  it again. But normally the window will end up stuck. Just do not write code
  that crashes if not.

  You can track stickiness via the @sig[gtk:widget]{window-state-event} signal
  on the @class{gtk:widget} object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-unstick)

;;; ----------------------------------------------------------------------------
;;; gtk_window_maximize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_maximize" window-maximize) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to maximize the window, so that it becomes full screen.
  @end{short}
  Note that you should not assume the window is definitely maximized afterward,
  because other entities, for example, the user or window manager, could
  unmaximize it again, and not all window managers support maximization. But
  normally the window will end up maximized. Just do not write code that
  crashes if not.

  It is permitted to call this function before showing a window, in which case
  the window will be maximized when it appears onscreen initially.

  You can track maximization via the @sig[gtk:widget]{window-state-event}
  signal on the @class{gtk:widget} object, or by listening to notifications on
  the @slot[gtk:window]{is-maximized} property.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:window-is-maximized}"
  (window (g:object window)))

(export 'window-maximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unmaximize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_unmaximize" window-unmaximize) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to unmaximize the window.
  @end{short}
  Note that you should not assume the window is definitely unmaximized
  afterward, because other entities, for example, the user or window manager,
  could maximize it again, and not all window managers honor requests to
  unmaximize. But normally the window will end up unmaximized. Just do not
  write code that crashes if not.

  You can track maximization via the @sig[gtk:widget]{window-state-event}
  signal on the @class{gtk:widget} object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_fullscreen" window-fullscreen) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to place the window in the fullscreen state.
  @end{short}
  Note that you should not assume the window is definitely full screen
  afterward, because other entities, for example, the user or window manager,
  could unfullscreen it again, and not all window managers honor requests to
  fullscreen windows. But normally the window will end up fullscreen. Just do
  not write code that crashes if not.

  You can track the fullscreen state via the
  @sig[gtk:widget]{window-state-event} signal on the @class{gtk:widget} object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_fullscreen_on_monitor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_fullscreen_on_monitor" window-fullscreen-on-monitor)
    :void
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @argument[screen]{a @class{gdk:screen} object to draw on}
  @argument[monitor]{an integer which monitor to go fullscreen on}
  @begin{short}
    Asks to place the window in the fullscreen state.
  @end{short}
  Note that you should not assume the window is definitely full screen
  afterward.

  You can track the fullscreen state via the
  @sig[gtk:widget]{window-state-event} signal on the @class{gtk:widget} widget.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-class{gdk:screen}"
  (window (g:object window))
  (screen (g:object gdk:screen))
  (monitor :int))

(export 'window-fullscreen-on-monitor)

;;; ----------------------------------------------------------------------------
;;; gtk_window_unfullscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_unfullscreen" window-unfullscreen) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Asks to toggle off the fullscreen state for the window.
  @end{short}
  Note that you should not assume the window is definitely not full screen
  afterward, because other entities, for example, the user or window manager,
  could fullscreen it again, and not all window managers honor requests to
  unfullscreen windows. But normally the window will end up restored to its
  normal state. Just do not write code that crashes if not.

  You can track the fullscreen state via the
  @sig[gtk:widget]{\"window-state-event\"} signal on the @class{gtk:widget}
  object.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window)))

(export 'window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_keep_above
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_keep_above" window-set-keep-above) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether to keep @arg{window} above other windows}
  @begin{short}
    Asks to keep the window above, so that it stays on top.
  @end{short}
  Note that you should not assume the window is definitely above afterward,
  because other entities, for example, the user or window manager, could not
  keep it above, and not all window managers support keeping windows above. But
  normally the window will end kept above. Just do not write code that crashes
  if not.

  It is permitted to call this function before showing a window, in which case
  the window will be kept above when it appears onscreen initially.

  You can track the above state via the @sig[gtk:widget]{window-state-event}
  signal on the @class{gtk:widget} object.

  Note that, according to the Extended Window Manager Hints specification, the
  above state is mainly meant for user preferences and should not be used by
  applications, for example, for drawing attention to their dialogs.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window))
  (setting :boolean))

(export 'window-set-keep-above)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_keep_below
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_keep_below" window-set-keep-below) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @argument[setting]{a boolean whether to keep @arg{window} below other windows}
  @begin{short}
    Asks to keep the window below, so that it stays in bottom.
  @end{short}
  Note that you should not assume the window is definitely below afterward,
  because other entities, for example, the user or window manager, could not
  keep it below, and not all window managers support putting windows below. But
  normally the window will be kept below. Just do not write code that crashes
  if not.

  It is permitted to call this function before showing a window, in which case
  the window will be kept below when it appears onscreen initially.

  You can track the below state via the @sig[gtk:widget]{window-state-event}
  signal on the @class{gtk:widget} object.

  Note that, according to the Extended Window Manager Hints specification,
  the above state is mainly meant for user preferences and should not be used
  by applications, for example, for drawing attention to their dialogs.
  @see-class{gtk:window}
  @see-class{gtk:widget}"
  (window (g:object window))
  (setting :boolean))

(export 'window-set-keep-below)

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_resize_drag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_begin_resize_drag" window-begin-resize-drag) :void
 #+liber-documentation
 "@version{#2025-06-23}
  @argument[window]{a @class{gtk:window} widget}
  @argument[edge]{a @sym{gdk:window-edge} value for the position of the
    resize control}
  @argument[button]{an integer for the mouse button that initiated the drag}
  @argument[x]{an integer for the x position where the user clicked to
    initiate the drag, in root window coordinates}
  @argument[y]{an integer for the y position where the user clicked to
    initiate the drag}
  @argument[timestamp]{an unsigned integer for the timestamp from the click
    event that initiated the drag}
  @begin{short}
    Starts resizing a window.
  @end{short}
  This function is used if an application has window resizing controls. When
  GDK can support it, the resize will be done using the standard mechanism for
  the window manager or windowing system. Otherwise, GDK will try to emulate
  window resizing, potentially not all that well, depending on the windowing
  system.
  @see-class{gtk:window}
  @see-symbol{gdk:window-edge}"
  (window (g:object window))
  (edge gdk:window-edge)
  (button :int)
  (x :int)
  (y :int)
  (timestamp :uint32))

(export 'window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_begin_move_drag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_begin_move_drag" window-begin-move-drag) :void
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[button]{an integer for the mouse button that initiated the drag}
  @argument[x]{an integer for the x position where the user clicked to
    initiate the drag, in root window coordinates}
  @argument[y]{an integer for the y position where the user clicked to
    initiate the drag}
  @argument[timestamp]{an unsigned integer for the timestamp from the click
    event that initiated the drag}
  @begin{short}
    Starts moving a window.
  @end{short}
  This function is used if an application has window movement grips. When GDK
  can support it, the window movement will be done using the standard mechanism
  for the window manager or windowing system. Otherwise, GDK will try to emulate
  window movement, potentially not all that well, depending on the windowing
  system.
  @see-class{gtk:window}"
  (window (g:object window))
  (button :int)
  (x :int)
  (y :int)
  (timestamp :uint32))

(export 'window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_mnemonic_modifier
;;; gtk_window_get_mnemonic_modifier
;;; ----------------------------------------------------------------------------

(defun (setf window-mnemonic-modifier) (modifier window)
  (cffi:foreign-funcall "gtk_window_mnemonic_modifier"
                        (g:object window) window
                        gdk:modifier-type modifier
                        :void)
  modifier)

(cffi:defcfun ("gtk_window_get_mnemonic_modifier" window-mnemonic-modifier)
    gdk:modifier-type
 #+liber-documentation
 "@version{#2025-06-23}
  @syntax{(gtk:window-mnemonic-modifier window) => modifier}
  @syntax{(setf (gtk:window-mnemonic-modifier window) modifier)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[modifier]{a @sym{gdk:modifier-type} value for the modifier mask
    used to activate mnemonics on @arg{window}}
  @begin{short}
    Accessor of the modifier mask used to activate mnemonics on the window.
  @end{short}
  The @fun{gtk:window-mnemonic-modifier} function returns the mnemonic modifier
  for the window. The @setf{gtk:window-mnemonic-modifier} function sets the
  mnemonic modifier.
  @see-class{gtk:window}
  @see-symbol{gdk:modifier-type}"
  (window (g:object window)))

(export 'window-mnemonic-modifier)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_list
;;; gtk_window_set_default_icon_list
;;; ----------------------------------------------------------------------------

;; TODO: Check again the memory management of the g:list-t type. The C
;; documentation says: The list is a copy and should be freed
;; with g_list_free(), but the pixbufs in the list have not had their reference
;; count incremented.

(cffi:defcfun ("gtk_window_set_default_icon_list" %window-set-default-icon-list)
    :void
  (icon-list (g:list-t (g:object gdk-pixbuf:pixbuf))))

(defun (setf window-default-icon-list) (icon-list)
  (%window-set-default-icon-list (mapcar #'g:object-pointer icon-list)))

(cffi:defcfun ("gtk_window_get_default_icon_list" window-default-icon-list)
    (g:list-t (g:object gdk-pixbuf:pixbuf))
 #+liber-documentation
 "@version{#2024-03-16}
  @syntax{(gtk:window-default-icon-list) => icons}
  @syntax{(setf (gtk:window-default-icon-list) icons)}
  @argument[icons]{a list of @class{gdk-pixbuf:pixbuf} objects}
  @begin{short}
    The @fun{gtk:window-icon-list} function returns the default icon list.
  @end{short}
  The @setf{gtk:window-icon-list} function sets an icon list to be used as
  fallback for windows that have not had the @fun{gtk:window-icon-list} function
  called on them to set up a window specific icon list. This function allows you
  to set up the icon for all windows in your application at once.
  @see-class{gtk:window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:window-icon-list}")

(export 'window-default-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_default_icon_name
;;; gtk_window_set_default_icon_name
;;; ----------------------------------------------------------------------------

(defun (setf window-default-icon-name) (name)
  (cffi:foreign-funcall "gtk_window_set_defaul_icon_name"
                        :string name
                        :void)
  name)

(cffi:defcfun ("gtk_window_get_default_icon_name" window-default-icon-name)
    (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{#2025-06-02}
  @syntax{(gtk:window-default-icon-name) => name}
  @syntax{(setf (gtk:window-default-icon-name) name)}
  @argument[name]{a string for the name of the themed icon}
  @begin{short}
    The @fun{gtk:window-default-icon-name} function returns the fallback icon
    name for windows.
  @end{short}
  The @setf{gtk:window-default-icon-name} function sets an icon to be used as
  fallback for windows that have not had the @fun{gtk:window-icon-list} function
  called on them from a named themed icon.
  @see-class{gtk:window}
  @see-function{gtk:window-icon-list}")

(export 'window-default-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_icon_list
;;; gtk_window_set_icon_list
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_icon_list" %window-set-icon-list) :void
  (window (g:object window))
  (icon-list (g:list-t (g:object gdk-pixbuf:pixbuf))))

(defun (setf window-icon-list) (icon-list window)
  (%window-set-icon-list window (mapcar #'g:object-pointer icon-list)))

(cffi:defcfun ("gtk_window_get_icon_list" window-icon-list)
    (g:list-t (g:object gdk-pixbuf:pixbuf))
 #+liber-documentation
 "@version{#2024-03-16}
  @syntax{(gtk:window-icon-list window) => icons}
  @syntax{(setf (gtk:window-icon-list window) icons)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[icons]{a list of @class{gdk-pixbuf:pixbuf} objects}
  @begin{short}
    The @fun{gtk:window-icon-list} function retrieves the list of icons.
  @end{short}
  The @setf{gtk:window-icon-list} function sets up the icon representing the
  window.

  The icon is used when the window is minimized, also known as iconified. Some
  window managers or desktop environments may also place it in the window
  frame, or display it in other contexts.

  The @setf{gtk:window-icon-list} function allows you to pass in the same icon
  in several hand drawn sizes. The list should contain the natural sizes your
  icon is available in. That is, do not scale the image before passing it to
  GTK. Scaling is postponed until the last minute, when the desired final size
  is known, to allow best quality.

  By passing several sizes, you may improve the final image quality of the
  icon, by reducing or eliminating automatic image scaling. Recommended sizes
  to provide: 16 x 16, 32 x 32, 48 x 48 at minimum, and larger images 64 x 64,
  128 x 128, if you have them.

  See also the @fun{gtk:window-default-icon-list} function to set the icon for
  all windows in your application in one go.

  Note that transient windows, those who have been set transient for another
  window using the @fun{gtk:window-transient-for} function, will inherit their
  icon from their transient parent. So there is no need to explicitly set the
  icon on transient windows.
  @see-class{gtk:window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:window-default-icon-list}
  @see-function{gtk:window-transient-for}"
  (window (g:object window)))

(export 'window-icon-list)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_get_position" %window-get-position) :void
  (window (g:object window))
  (xroot (:pointer :int))
  (yroot (:pointer :int)))

;; The Lisp implementation returns the position as a value list.

(defun window-position (window)
 #+liber-documentation
 "@version{#2025-06-23}
  @syntax{(gtk:window-position window) => x, y}
  @argument[window]{a @class{gtk:window} widget}
  @argument[x]{an integer for the x coordinate of gravity determined reference
    point}
  @argument[y]{an integer for the y coordinate of gravity determined reference
    point}
  @begin{short}
    This function returns the position you need to pass to the
    @fun{gtk:window-move} function to keep the window in its current position.
  @end{short}
  This means that the meaning of the returned value varies with window gravity.
  See the @fun{gtk:window-move} function for more details.

  If you have not changed the window gravity, its gravity will be the
  @val[gdk:gravity]{:north-west} value of the @sym{gdk:gravity} enumeration.
  This means that the @fun{gtk:window-position} function gets the position of
  the top-left corner of the window manager frame for the window. The
  @fun{gtk:window-move} function sets the position of this same top-left corner.

  The @fun{gtk:window-position} function is not 100 % reliable because the
  X Window System does not specify a way to obtain the geometry of the
  decorations placed on a window by the window manager. Thus GTK is using a
  \"best guess\" that works with most window managers.

  Moreover, nearly all window managers are historically broken with respect to
  their handling of window gravity. So moving a window to its current position
  as returned by the @fun{gtk:window-position} function tends to result in
  moving the window slightly. Window managers are slowly getting better over
  time.

  If a window has @val[gdk:gravity]{:static} gravity the window manager frame
  is not relevant, and thus the @fun{gtk:window-position} function will always
  produce accurate results. However you can not use static gravity to do things
  like place a window in a corner of the screen, because static gravity ignores
  the window manager decorations.

  If you are saving and restoring your application's window positions, you
  should know that it is impossible for applications to do this without getting
  it somewhat wrong because applications do not have sufficient knowledge of
  window manager state. The Correct Mechanism is to support the session
  management protocol, see the \"GnomeClient\" object in the GNOME libraries for
  example, and allow the window manager to save your window sizes and positions.
  @see-class{gtk:window}
  @see-symbol{gdk:gravity}
  @see-function{gtk:window-move}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%window-get-position window x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'window-position)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_get_size" %window-get-size) :void
  (window (g:object window))
  (width (:pointer :int))
  (height (:pointer :int)))

;; The Lisp implementation returns the size as a value list.

(defun window-size (window)
 #+liber-documentation
 "@version{#2025-06-03}
  @syntax{(gtk:window-size window) => width, height}
  @argument[window]{a @class{gtk:window} widget}
  @argument[width]{an integer for the width}
  @argument[height]{an integer for the height}
  @begin{short}
    Obtains the current size of the window.
  @end{short}
  If the window is not onscreen, it returns the size GTK will suggest to the
  window manager for the initial window size, but this is not reliably the same
  as the size the window manager will actually select. The size obtained by the
  @fun{gtk:window-size} function is the last size received in a
  @class{gdk:event-configure} event, that is, GTK uses its locally stored size,
  rather than querying the X server for the size. As a result, if you call the
  @fun{gtk:window-resize} function then immediately call the
  @fun{gtk:window-size} function, the size will not have taken effect yet.
  After the window manager processes the resize request, GTK receives
  notification that the size has changed via a configure event, and the size of
  the window gets updated.
  @begin[Notes]{dictionary}
    @begin{enumerate}
      @begin{item}
        Nearly any use of this function creates a race condition, because the
        size of the window may change between the time that you get the size and
        the time that you perform some action assuming that size is the current
        size. To avoid race conditions, connect to the
        @sig[gtk:widget]{configure-event} signal on the window and adjust your
        size dependent state to match the size delivered in the
        @class{gdk:event-configure} event.
      @end{item}
      @begin{item}
        The returned size does not include the size of the window manager
        decorations, aka the window frame or border. Those are not drawn by GTK
        and GTK has no reliable method of determining their size.
      @end{item}
      @begin{item}
        If you are getting a window size in order to position the window
        onscreen, there may be a better way. The preferred way is to simply set
        the semantic type of the window with the @fun{gtk:window-type-hint}
        function, which allows the window manager to, for example, center
        dialogs. Also, if you set the transient parent of dialogs with the
        @fun{gtk:window-transient-for} function window managers will often
        center the dialog over its parent window. It is much preferred to let
        the window manager handle these things rather than doing it yourself,
        because all applications will behave consistently and according to user
        prefs if the window manager handles it. Also, the window manager can
        take the size of the window decorations/border into account, while your
        application cannot.
      @end{item}
    @end{enumerate}
    In any case, if you insist on application-specified window positioning,
    there is still a better way than doing it yourself - the
    @fun{gtk:window-window-position} function will frequently handle the
    details for you.
  @end{dictionary}
  @see-class{gtk:window}
  @see-class{gdk:event-configure}
  @see-function{gtk:window-resize}
  @see-function{gtk:window-type-hint}
  @see-function{gtk:window-window-position}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%window-get-size window width height)
    (values (cffi:mem-ref width :int) (cffi:mem-ref height :int))))

(export 'window-size)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_get_group" window-group)
    (g:object window-group)
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget, or @code{nil}}
  @begin{return}
    The @class{gtk:window-group} object for the @arg{window} argument or
    the default window group.
  @end{return}
  @begin{short}
    Returns the window group for the window or the default window group, if
    the @arg{window} argument is @code{nil} or if the @arg{window} argument
    does not have an explicit window group.
  @end{short}
  @see-class{gtk:window}
  @see-class{gtk:window-group}"
  (window (g:object window)))

(export 'window-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_has_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_has_group" window-has-group) :boolean
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @return{@em{True} if the @arg{window} argument has an explicit window group.}
  @short{Returns whether the window has an explicit window group.}
  @see-class{gtk:window}
  @see-class{gtk:window-group}"
  (window (g:object window)))

(export 'window-has-group)

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_window_type
;;; ----------------------------------------------------------------------------

;; Implemented as the slot acces function window-type

;;; ----------------------------------------------------------------------------
;;; gtk_window_move
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_move" window-move) :void
 #+liber-documentation
 "@version{#2025-06-23}
  @argument[window]{a @class{gtk:window} widget}
  @argument[x]{an integer for the x coordinate to move the window to}
  @argument[y]{an integer for the y coordinate to move the window to}
  @begin{short}
    Asks the window manager to move the window to the given position.
  @end{short}
  Window managers are free to ignore this. Most window managers ignore requests
  for initial window positions, instead using a user-defined placement
  algorithm, and honor requests after the window has already been shown.

  Note: The position is the position of the gravity-determined reference point
  for the window. The gravity determines two things: first, the location of
  the reference point in root window coordinates, and second, which point on
  the window is positioned at the reference point.

  By default the gravity is the @val[gdk:gravity]{:north-west} value of the
  @sym{gdk:gravity} enumeration, so the reference point is simply the x, y
  supplied to the @fun{gtk:window-move} function. The top-left corner of the
  window decorations, aka window frame or border, will be placed at x, y.
  Therefore, to position a window at the top left of the screen, you want to
  use the default gravity, which is @val[gdk:gravity]{:north-west}, and move
  the window to 0,0.

  To position a window at the bottom right corner of the screen, you would set
  the @val[gdk:gravity]{:south-east} value, which means that the reference point
  is at x + the window width and y + the window height, and the bottom-right
  corner of the window border will be placed at that reference point. So, to
  place a window in the bottom right corner you would first set gravity to
  south east, then write:
  @begin{pre}
(gtk:window-move window
                 (- (gdk:screen-width) window-width)
                 (- (gdk:screen-height) window-height))
  @end{pre}
  Note that this example does not take multi-head scenarios into account.

  The @url[http://www.freedesktop.org/Standards/wm-spec]{Extended Window
  Manager Hints} specification has a table of gravities in the \"Implementation
  Notes\" section. The @fun{gtk:window-position} documentation may also be
  relevant.
  @see-class{gtk:window}
  @see-symbol{gdk:gravity}
  @see-function{gtk:window-position}"
  (window (g:object window))
  (x :int)
  (y :int))

(export 'window-move)

;;; ----------------------------------------------------------------------------
;;; gtk_window_parse_geometry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_parse_geometry" window-parse-geometry) :boolean
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[window]{a @class{gtk:window} widget}
  @argument[geometry]{a geometry string}
  @return{@em{True} if the @arg{geometry} string was parsed successfully.}
  @begin{short}
    Parses a standard X Window System geometry string.
  @end{short}
  The @fun{gtk:window-parse-geometry} function does work on all GTK ports
  including Win32 but is primarily intended for an X environment.

  If either a size or a position can be extracted from the geometry string,
  the @fun{gtk:window-parse-geometry} function returns @em{true} and calls
  the @fun{gtk:window-default-size} and/or @fun{gtk:window-move} functions to
  resize/move the window.

  If the @fun{gtk:window-parse-geometry} function returns @em{true}, it will
  also set the @val[gdk:window-hints]{:user-pos} and/or
  @val[gdk:window-hints]{:user-size} hints of the @sym{gdk:window-hints} flags
  indicating to the window manager that the size/position of the window was user
  specified. This causes most window managers to honor the geometry.

  Note that for the @fun{gtk:window-parse-geometry} function to work as
  expected, it has to be called when the window has its \"final\" size, for
  example, after calling the @fun{gtk:widget-show-all} function on the contents
  and the @fun{gtk:window-set-geometry-hints} function on the window.
  @begin[Warning]{dictionary}
    The @fun{gtk:window-parse-geometry} function has been deprecated since
    version 3.20. Geometry handling in GTK is deprecated.
  @end{dictionary}
  @see-class{gtk:window}
  @see-symbol{gdk:window-hints}
  @see-function{gtk:window-default-size}
  @see-function{gtk:window-move}
  @see-function{gtk:widget-show-all}
  @see-function{gtk:window-set-geometry-hints}"
  (window (g:object window))
  (geometry :string))

(export 'window-parse-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_reshow_with_initial_size                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_reshow_with_initial_size"
               window-reshow-with-initial-size) :void
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Hides the window, then reshows it, resetting the default size and position
    of the window.
  @end{short}
  Used by GUI builders only.
  @begin[Warning]{dictionary}
    The @fun{gtk:window-reshow-with-initial-size} function has been deprecated
    since version 3.10 and should not be used in newly written code. GUI
    builders can call the @fun{gtk:widget-hide}, @fun{gtk:widget-unrealize}
    functions and then the @fun{gtk:widget-show} function on the window
    themselves, if they still need this functionality.
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:widget-hide}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-undrealize}"
  (window (g:object window)))

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_resize" window-resize) :void
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[width]{an integer for the width in pixels to resize the window to}
  @argument[height]{an integer for the height in pixels to resize the window to}
  @begin{short}
    Resizes the window as if the user had done so, obeying geometry constraints.
  @end{short}
  The default geometry constraint is that windows may not be smaller than their
  size request. To override this constraint, call the
  @fun{gtk:widget-size-request} function to set the request of the window to a
  smaller value.

  If the @fun{gtk:window-resize} function is called before showing a window for
  the first time, it overrides any default size set with the
  @fun{gtk:window-default-size} function.

  Windows may not be resized smaller than 1 by 1 pixels.
  @see-class{gtk:window}
  @see-function{gtk:widget-size-request}
  @see-function{gtk:window-default-size}"
  (window (g:object window))
  (width :int)
  (height :int))

(export 'window-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_to_geometry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_resize_to_geometry" window-resize-to-geometry) :void
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[width]{an integer for the width in resize increments to resize
    the window to}
  @argument[height]{an integer for the height in resize increments to resize
    the window to}
  @begin{short}
    Like the @fun{gtk:window-resize} function, but @arg{width} and @arg{height}
    are interpreted in terms of the base size and increment set with the
    @fun{gtk:window-set-geometry-hints} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-resize-to-geometry} function has been deprecated since
    version 3.20. This function does nothing. Use the @fun{gtk:window-resize}
    function and compute the geometry yourself.
  @end{dictionary}
  @see-class{gtk:window}
  @see-function{gtk:window-resize}
  @see-function{gtk:window-set-geometry-hints}"
  (window (g:object window))
  (width :int)
  (height :int))

(export 'window-resize-to-geometry)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_default_icon" window-set-default-icon) :void
 #+liber-documentation
 "@version{#2025-06-03}
  @argument[icon]{a @class{gdk-pixbuf:pixbuf} object for the icon}
  @begin{short}
    Sets an icon to be used as fallback for windows that have not had the
    @fun{gtk:window-icon} function called on them from a pixbuf.
  @end{short}
  @see-class{gtk:window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:window-icon}"
  (icon (g:object gdk-pixbuf:pixbuf)))

(export 'window-set-default-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_default_icon_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_default_icon_from_file"
               %window-set-default-icon-from-file) :boolean
  (filename :string)
  (err :pointer))

(defun window-set-default-icon-from-file (path)
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[path]{a pathname or namestring for the location of the icon file}
  @return{@em{True} if setting the icon succeeded.}
  @begin{short}
    Sets an icon to be used as fallback for windows that have not had the
    @fun{gtk:window-icon-list} function called on them from a file on disk.
  @end{short}
  @see-class{gtk:window}
  @see-function{gtk:window-icon-list}"
  (glib:with-error (err)
    (%window-set-default-icon-from-file (namestring path) err)))

(export 'window-set-default-icon-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_icon_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_icon_from_file" %window-set-icon-from-file)
    :boolean
  (window (g:object window))
  (filename :string)
  (err :pointer))

(defun window-set-icon-from-file (window path)
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[window]{a @class{gtk:window} widget}
  @argument[path]{a pathname or namestring for the location of the icon file}
  @return{@em{True} if setting the icon succeeded.}
  @begin{short}
    Sets the icon for the window.
  @end{short}
  This function is equivalent to calling the @fun{gtk:window-icon} function
  with a pixbuf created by loading the image from @arg{path}.
  @see-class{gtk:window}
  @see-function{gtk:window-icon}"
  (glib:with-error (err)
    (%window-set-icon-from-file window (namestring path) err)))

(export 'window-set-icon-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_auto_startup_notification
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_auto_startup_notification"
               window-set-auto-startup-notification) :void
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[setting]{@em{true} to automatically do startup notification}
  @begin{short}
    Call this function to disable the automatic startup notification.
  @end{short}
  By default, after showing the first window, GTK calls the
  @fun{gdk:notify-startup-complete} function.

  You might do this if your first window is a splash screen, and you want to
  delay notification until after your real main window has been shown. In that
  example, you would disable startup notification temporarily, show your splash
  screen, then re-enable it so that showing the main window would automatically
  result in notification.
  @see-class{gtk:window}
  @see-function{gdk:notify-startup-complete}"
  (setting :boolean))

(export 'window-set-auto-startup-notification)

;;; ----------------------------------------------------------------------------
;;; gtk_window_resize_grip_is_visible                      not exported
;;; ----------------------------------------------------------------------------

(defun window-resize-grip-is-visible (window)
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @return{@em{True} if a resize grip exists and is visible.}
  @begin{short}
    Determines whether a resize grip is visible for the specified window.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-resize-grip-is-visible} function has been deprecated
    since version 3.14 and should not be used in newly written code. Resize
    grips have been removed.
  @end{dictionary}
  @see-class{gtk:window}"
  (and (window-has-resize-grip window)
       (window-resize-grip-visible window)))

;;; ----------------------------------------------------------------------------
;;; gtk_window_get_resize_grip_area                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_get_resize_grip_area" %window-get-resize-grip-area)
    :boolean
  (window (g:object window))
  (rect (g:boxed gdk:rectangle)))

(defun window-resize-grip-area (window)
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[window]{a @class{gtk:window} widget}
  @begin{return}
    A @class{gdk:rectangle} rectangle with the resize grip area.
  @end{return}
  @begin{short}
    If the window has a resize grip, this function will retrieve the grip
    position, width and height.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:window-resize-grip-area} function has been deprecated since
    version 3.14 and should not be used in newly written code. Resize grips
    have been removed.
  @end{dictionary}
  @see-class{gtk:window}
  @see-class{gdk:rectangle}"
  (let ((rect (gdk:rectangle-new)))
    (when (%window-get-resize-grip-area window rect)
      rect)))

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_has_user_ref_count ()
;;;
;;; void
;;; gtk_window_set_has_user_ref_count (GtkWindow *window, gboolean setting)
;;;
;;; Tells GTK whether to drop its extra reference to the window when
;;; gtk_window_destroy() is called.
;;;
;;; This function is only exported for the benefit of language bindings which
;;; may need to keep the window alive until their wrapper object is garbage
;;; collected. There is no justification for ever calling this function in an
;;; application.
;;;
;;; window :
;;;     a GtkWindow
;;;
;;; setting :
;;;     the new value
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_titlebar
;;; gtk_window_get_titlebar
;;; ----------------------------------------------------------------------------

(defun (setf window-titlebar) (widget window)
  (cffi:foreign-funcall "gtk_window_set_titlebar"
                        (g:object window) window
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_window_get_titlebar" window-titlebar) (g:object widget)
 #+liber-documentation
 "@version{#2024-03-20}
  @syntax{(gtk:window-titlebar window) => widget}
  @syntax{(setf (gtk:window-titlebar window) widget)}
  @argument[window]{a @class{gtk:window} widget}
  @argument[widget]{a @class{gtk:widget} object to use as titlebar}
  @begin{short}
    The @fun{gtk:window-titlebar} function returns the custom titlebar for the
    window.
  @end{short}
  The @setf{gtk:window-titlebar} function sets a custom titlebar.

  If you set a custom titlebar, GTK will do its best to convince the window
  manager not to put its own titlebar on the window. Depending on the system,
  this function may not work for a window that is already visible, so you set
  the titlebar before calling the @fun{gtk:widget-show} function.
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}"
  (window (g:object window)))

(export 'window-titlebar)

;;; ----------------------------------------------------------------------------
;;; gtk_window_set_interactive_debugging
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_set_interactive_debugging"
               window-interactive-debugging) :void
 #+liber-documentation
 "@version{2024-03-16}
  @argument[enable]{@em{true} to enable interactice debugging}
  @begin{short}
    Opens or closes the interactive debugger, which offers access to the widget
    hierarchy of the application and to useful debugging tools.
  @end{short}
  @see-class{gtk:window}"
  (enable :boolean))

(export 'window-interactive-debugging)

;;; --- End of file gtk3.window.lisp -------------------------------------------
