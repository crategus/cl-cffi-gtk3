;;; ----------------------------------------------------------------------------
;;; gtk3.application.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GtkApplication
;;;
;;;     Application class
;;;
;;; Types and Values
;;;
;;;     GtkApplication
;;;     GtkApplicationInhibitFlags
;;;
;;; Accessors
;;;
;;;     gtk_application_get_active_window
;;;     gtk_application_get_app_menu
;;;     gtk_application_set_app_menu
;;;     gtk_application_get_menubar
;;;     gtk_application_set_menubar
;;;
;;; Functions
;;;
;;;     gtk_application_new
;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;;     gtk_application_get_window_by_id
;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit
;;;     gtk_application_is_inhibited
;;;     gtk_application_prefers_app_menu
;;;     gtk_application_get_menu_by_id
;;;     gtk_application_add_accelerator                     not exported
;;;     gtk_application_remove_accelerator                  not exported
;;;     gtk_application_list_action_descriptions
;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action
;;;     gtk_application_get_actions_for_accel
;;;
;;; Properties
;;;
;;;     active-window
;;;     app-menu
;;;     menubar
;;;     register-session
;;;     screensaver-active
;;;
;;; Signals
;;;
;;;     query-end
;;;     window-added
;;;     window-removed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GApplication
;;;         ╰── GtkApplication
;;;
;;; Implemented Interfaces
;;;
;;;     GtkApplication implements GActionGroup and GActionMap
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkApplicationInhibitFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkApplicationInhibitFlags" application-inhibit-flags
  (:export t
   :type-initializer "gtk_application_inhibit_flags_get_type")
  (:logout  #.(ash 1 0))
  (:switch  #.(ash 1 1))
  (:suspend #.(ash 1 2))
  (:idle    #.(ash 1 3)))

#+liber-documentation
(setf (liber:alias-for-symbol 'application-inhibit-flags)
      "GFlags"
      (liber:symbol-documentation 'application-inhibit-flags)
 "@version{2024-03-15}
  @begin{declaration}
(gobject:define-gflags \"GtkApplicationInhibitFlags\" application-inhibit-flags
  (:export t
   :type-initializer \"gtk_application_inhibit_flags_get_type\")
  (:logout  #.(ash 1 0))
  (:switch  #.(ash 1 1))
  (:suspend #.(ash 1 2))
  (:idle    #.(ash 1 3)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:logout]{Inhibit ending the user session by logging out or by
        shutting down the computer.}
      @entry[:switch]{Inhibit user switching.}
      @entry[:suspend]{Inhibit suspending the session or computer.}
      @entry[:idle]{Inhibit the session being marked as idle and possibly
        locked.}
    @end{table}
  @end{values}
  @begin{short}
    Types of user actions that may be blocked by the
    @fun{gtk:application-inhibit} function.
  @end{short}
  @see-class{gtk:application}
  @see-function{gtk:application-inhibit}")

;;; ----------------------------------------------------------------------------
;;; GtkApplication
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkApplication" application
  (:superclass g:application
   :export t
   :interfaces ("GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_get_type")
   ((active-window
     application-active-window
     "active-window" "GtkWindow" t nil)
    (app-menu
     application-app-menu
     "app-menu" "GMenuModel" t t)
    (menubar
     application-menubar
     "menubar" "GMenuModel" t t)
    (register-session
     application-register-session
     "register-session" "gboolean" t t)
   (screensaver-active
    application-screensaver-active
    "screensaver-active" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'application 'type)
 "@version{2025-06-02}
  @begin{short}
    The @class{gtk:application} class handles many important aspects of a GTK
    application in a convenient fashion, without enforcing a one-size-fits-all
    application model.
  @end{short}
  Currently, the @class{gtk:application} class handles GTK initialization,
  application uniqueness, session management, provides some basic scriptability
  and desktop shell integration by exporting actions and menus and manages a
  list of toplevel windows whose life cycle is automatically tied to the
  life cycle of the application. While the @class{gtk:application} class works
  fine with plain @class{gtk:window} widgets, it is recommended to use it
  together with @class{gtk:application-window} widgets.

  When GDK threads are enabled, the @class{gtk:application} instance will
  acquire the GDK lock when invoking actions that arrive from other processes.
  The GDK lock is not touched for local action invocations. In order to have
  actions invoked in a predictable context it is therefore recommended that the
  GDK lock be held while invoking actions locally with the
  @fun{g:action-group-activate-action} function. The same applies to actions
  associated with @class{gtk:application-window} widgets and to the
  @code{\"activate\"} and @code{\"open\"} signals of the @class{g:application}
  class.

  The @class{gtk:application} instance will automatically load menus from the
  @class{gtk:builder} resource located at @file{\"gtk/menus.ui\"}, relative to
  the resource base path of the application, see the
  @fun{g:application-resource-base-path} function. The menu with the ID
  @code{\"app-menu\"} is taken as the application menu of the application and
  the menu with the ID @code{\"menubar\"} is taken as the menubar of the
  application. Additional menus, most interesting submenus, can be named and
  accessed via the @fun{gtk:application-menu-by-id} function which allows for
  dynamic population of a part of the menu structure.

  If the resources @file{\"gtk/menus-appmenu.ui\"} or
  @file{\"gtk/menus-traditional.ui\"} are present then these files will be used
  in preference, depending on the value of the
  @fun{gtk:application-prefers-app-menu} function. If the resource
  @file{\"gtk/menus-common.ui\"} is present it will be loaded as well. This is
  useful for storing items that are referenced from both
  @file{\"gtk/menus-appmenu.ui\"} and @file{\"gtk/menus-traditional.ui\"}.

  It is also possible to provide the menus manually using the
  @fun{gtk:application-app-menu} and @fun{gtk:application-menubar} functions.

  The @class{gtk:application} instance will also automatically setup an icon
  search path for the default icon theme by appending @file{\"icons\"} to the
  resource base path. This allows your application to easily store its icons as
  resources. See the @fun{gtk:icon-theme-add-resource-path} function for more
  information.

  If there is a resource located at @file{\"gtk/help-overlay.ui\"} which
  defines a @class{gtk:shortcuts-window} widget with ID @code{\"help_overlay\"}
  then the @class{gtk:application} instance associates an instance of this
  shortcuts window with each @class{gtk:application-window} widget and sets up
  keyboard accelerators, @kbd{Control-F1} and @kbd{Control-?}, to open it. To
  create a menu item that displays the shortcuts window, associate the item
  with the @code{\"win.show-help-overlay\"} action.

  The @class{gtk:application} instance optionally registers with a session
  manager of the users session, if you set the
  @slot[gtk:application]{register-session} property, and offers various
  functionality related to the session life cycle.

  An application can block various ways to end the session with the
  @fun{gtk:application-inhibit} function. Typical use cases for this kind of
  inhibiting are long-running, uninterruptible operations, such as burning a CD
  or performing a disk backup. The session manager may not honor the inhibitor,
  but it can be expected to inform the user about the negative consequences of
  ending the session while inhibitors are present.
  @begin[Examples]{dictionary}
    A simple application.
    @begin{pre}
(defun application-simple (&rest argv)
  (let (;; Create an application
        (app (make-instance 'gtk:application
                            :application-id
                            \"com.crategus.application-simple\"
                            :flags :flags-none)))
    ;; Connect signal \"activate\" to the application
    (g:signal-connect app \"activate\"
        (lambda (application)
          ;; Create an application window
          (let ((window (make-instance 'gtk:application-window
                                       :application application
                                       :title \"Simple Application\"
                                       :default-width 480
                                       :default-height 300)))
            ;; Show the application window
            (gtk:widget-show-all window))))
    ;; Run the application
    (g:application-run app argv)))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"query-end\" signal}
      @begin{pre}
lambda (application)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[application]{The @class{gtk:application} instance which emitted
          the signal.}
      @end{table}
      Emitted when the session manager is about to end the session, only if the
      @slot[gtk:application]{register-session} property is @em{true}.
      Applications can connect to this signal and call the
      @fun{gtk:application-inhibit} function with the @code{:logout} value of
      the @symbol{gtk:application-inhibit-flags} flags to delay the end of the
      session until the state has been saved.
    @subheading{The \"window-added\" signal}
      @begin{pre}
lambda (application window)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[application]{The @class{gtk:application} instance which emitted
          the signal.}
        @entry[window]{The newly added @class{gtk:window} widget.}
      @end{table}
      Emitted when a @class{gtk:window} widget is added to the application
      through the @fun{gtk:application-add-window} function.
    @subheading{The \"window-removed\" signal}
      @begin{pre}
lambda (application window)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[application]{The @class{gtk:application} instance which emitted
          the signal.}
        @entry[window]{The @class{gtk:window} widget that is being removed.}
      @end{table}
      Emitted when a @class{gtk:window} widget is removed from the application,
      either as a side-effect of being destroyed or explicitly through the
      @fun{gtk:application-remove-window} function.
  @end{dictionary}
  @see-constructor{gtk:application-new}
  @see-slot{gtk:application-active-window}
  @see-slot{gtk:application-app-menu}
  @see-slot{gtk:application-menubar}
  @see-slot{gtk:application-register-session}
  @see-slot{gtk:application-screensaver-active}
  @see-class{gtk:application-window}
  @see-class{gtk:builder}
  @see-class{gtk:shortcuts-window}
  @see-class{gtk:window}
  @see-class{g:application}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:application-active-window ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active-window" 'application) t)
 "The @code{active-window} property of type @class{gtk:window} (Read) @br{}
  The window which most recently had focus.")

#+liber-documentation
(setf (liber:alias-for-function 'application-active-window)
      "Accessor"
      (documentation 'application-active-window 'function)
 "@version{2024-03-15}
  @syntax{(gtk:application-active-window object) => window}
  @argument[object]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Accessor of the @slot[gtk:application]{active-window} slot of the
    @class{gtk:application} class.
  @end{short}
  The @fun{gtk:application-active-window} function gets the active window for
  the application.

  The active window is the one that was most recently focused within the
  application. This window may not have the focus at the moment if another
  application has it - this is just the most recently focused window within
  this application.
  @see-class{gtk:application}
  @see-class{gtk:window}")

;;; --- gtk:application-app-menu -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "app-menu" 'application) t)
 "The @code{app-menu} property of type @class{g:menu-model} (Read / Write) @br{}
  The menu model for the application menu.")

#+liber-documentation
(setf (liber:alias-for-function 'application-app-menu)
      "Accessor"
      (documentation 'application-app-menu 'function)
 "@version{2024-03-15}
  @syntax{(gtk:application-app-menu object) => menu}
  @syntax{(setf (gtk:application-app-menu object) menu)}
  @argument[object]{a @class{gtk:application} instance}
  @argument[menu]{a @class{g:menu-model} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:application]{app-menu} slot of the
    @class{gtk:application} class.
  @end{short}
  The @fun{gtk:application-app-menu} function returns the application menu that
  has been set. The @setf{gtk:application-app-menu} function sets the
  application menu. This can only be done in the primary instance of the
  application, after it has been registered. The handler for the
  @code{\"startup\"} signal is a good place to call this.

  The application menu is a single menu containing items that typically impact
  the application as a whole, rather than acting on a specific window or
  document. For example, you would expect to see \"Preferences\" or \"Quit\" in
  an application menu, but not \"Save\" or \"Print\". If supported, the
  application menu will be rendered by the desktop environment.

  Use the @class{g:action-map} interface to add actions, to respond to the user
  selecting these menu items.
  @see-class{gtk:application}
  @see-class{g:menu-model}
  @see-class{g:action-map}")

;;; --- gtk:application-menubar ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menubar" 'application) t)
 "The @code{menubar} property of type @class{g:menu-model} (Read / Write) @br{}
  The menu model for the menubar.")

#+liber-documentation
(setf (liber:alias-for-function 'application-menubar)
      "Accessor"
      (documentation 'application-menubar 'function)
 "@version{2024-03-15}
  @syntax{(gtk:application-menubar object) => menubar}
  @syntax{(setf (gtk:application-menubar object) menubar)}
  @argument[object]{a @class{gtk:application} instance}
  @argument[menubar]{a @class{g:menu-model} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:application]{menubar} slot of the
    @class{gtk:application} class.
  @end{short}
  The @fun{gtk:application-menubar} function returns the menubar for windows of
  the application. The @setf{gtk:application-menubar} function sets or unsets
  the menubar.

  This is a menubar in the traditional sense. This can only be done in the
  primary instance of the application, after it has been registered. The
  handler for the @code{\"startup\"} signal is a good place to call this.

  Depending on the desktop environment, this may appear at the top of each
  window, or at the top of the screen. In some environments, if both the
  application menu and the menubar are set, the application menu will be
  presented as if it were the first item of the menubar. Other environments
  treat the two as completely separate - for example, the application menu
  may be rendered by the desktop shell while the menubar, if set, remains in
  each individual window.

  Use the @class{g:action-map} interface to add actions, to respond to the user
  selecting these menu items.
  @see-class{gtk:application}
  @see-class{g:menu-model}
  @see-class{g:action-map}")

;;; --- gtk:application-register-session ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "register-session"
                                               'application) t)
 "The @code{register-session} property of type @code{:boolean} (Read / Write)
  @br{}
  Set this property to @em{true} to register with the session manager. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'application-register-session)
      "Accessor"
      (documentation 'application-register-session 'function)
 "@version{2024-03-15}
  @syntax{(gtk:application-register-session object) => setting}
  @syntax{(setf (gtk:application-register-session object) setting)}
  @argument[object]{a @class{gtk:application} instance}
  @argument[setting]{a boolean whether to register with the session manager}
  @begin{short}
    Accessor of the @slot[gtk:application]{register-session} slot of the
    @class{gtk:application} class.
  @end{short}
  Set the @slot[gtk:application]{register-session} property to @em{true} to
  register with the session mananger.
  @see-class{gtk:application}")

;;; --- gtk:application-screensaver-active -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "screensaver-active"
                                               'application) t)
 "The @code{screensaver-active} property of type @code{:boolean} (Read) @br{}
  This property is @em{true} if GTK believes that the screensaver is currently
  active. GTK only tracks session state, including this, when the
  @slot[gtk:application]{register-session} property is set to @em{true}.
  Tracking the screensaver state is supported on Linux. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'application-screensaver-active)
      "Accessor"
      (documentation 'application-screensaver-active 'function)
 "@version{2024-03-15}
  @syntax{(gtk:application-screensaver-active object) => active}
  @argument[object]{a @class{gtk:application} instance}
  @argument[active]{a boolean whether the screensaver is active}
  @begin{short}
    Accessor of the @slot[gtk:application]{screensaver-active} slot of the
    @class{gtk:application} class.
  @end{short}
  The @slot[gtk:application]{screensaver-active} property is @em{true} if GTK
  believes that the screensaver is currently active. GTK only tracks session
  state, including this, when the @slot[gtk:application]{register-session}
  property is set to @em{true}. Tracking the screensaver state is supported on
  Linux.
  @see-class{gtk:application}
  @see-function{gtk:application-register-session}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_new
;;; ----------------------------------------------------------------------------

(declaim (inline application-new))

(defun application-new (id flags)
 "@version{2025-06-02}
  @argument[id]{a string for the application ID, or @code{nil} for no
    application ID}
  @argument[flags]{a @symbol{g:application-flags} value for the application
    flags}
  @return{The new @class{gtk:application} instance.}
  @begin{short}
    Creates a new application.
  @end{short}
  The @code{gtk_init()} function is called as soon as the application gets
  registered as the primary instance. Concretely, the @code{gtk_init()} function
  is called in the default handler for the @code{\"startup\"} signal. Therefore,
  @class{gtk:application} subclasses should chain up in their @code{\"startup\"}
  handler before using any GTK API.

  Note that command line arguments are not passed to the @code{gtk_init()}
  function. All GTK functionality that is available via command line
  arguments can also be achieved by setting suitable environment variables such
  as @code{G_DEBUG}, so this should not be a big problem. If you absolutely
  must support GTK command line arguments, you can explicitly call the
  @code{gtk_init()} function before creating the application instance.

  The application ID must be valid. See the @fun{g:application-id-is-valid}
  function. If no application ID is given then some features, most notably
  application uniqueness, will be disabled.
  @see-class{gtk:application}
  @see-symbol{g:application-flags}
  @see-function{g:application-id-is-valid}"
  (make-instance 'application
                 :application-id (or id (cffi:null-pointer))
                 :flags flags))

(export 'application-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_add_window" application-add-window) :void
 #+liber-documentation
 "@version{2024-03-15}
  @argument[application]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Adds a window to the application.
  @end{short}
  This call is equivalent to setting the @slot[gtk:window]{application}
  property of @arg{window} to @arg{application}.

  Normally, the connection between the application and the window will remain
  until the window is destroyed, but you can explicitly remove it with the
  @fun{gtk:application-remove-window} function.

  GTK will keep the application running as long as it has any windows.
  @begin[Examples]{dictionary}
    This is a code fragment from the GTK Demo. The function creates a window
    and adds the window to the list of application windows. The application is
    passed in as an argument.
    @begin{pre}
(defun example-window-simple-demo (&optional application)
  (gtk:within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk:window-new :toplevel)))
      ;; Add the window to the list of windows of the application
      (when application
        (gtk:application-add-window application window))
      ... )
    @end{pre}
    This is equivalent to the following code
    @begin{pre}
      ...
      ;; Add the window to the list of windows of the application
      (when application
        (setf (gtk:window-application window) application))
      ... )
    @end{pre}
    Finally, we can add the window to the list of application windows in one
    step when we create the window. This is the preferred way to do this in a
    Lisp program.
    @begin{pre}
(defun example-window-simple-demo (&optional application)
  (gtk:within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk:window
                                 :type :toplevel
                                 :application application)))
      ... )
    @end{pre}
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{gtk:window}
  @see-function{gtk:application-remove-window}
  @see-function{gtk:window-application}"
  (application (g:object application))
  (window (g:object window)))

(export 'application-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_remove_window" application-remove-window) :void
 #+liber-documentation
 "@version{2024-03-15}
  @argument[application]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget}
  @begin{short}
    Remove a window from the application.
  @end{short}
  If @arg{window} belongs to @arg{application} then this call is equivalent to
  setting the @slot[gtk:window]{application} property of @arg{window} to
  @code{nil}. The application may stop running as a result of a call to this
  function.
  @see-class{gtk:application}
  @see-class{gtk:window}
  @see-function{gtk:application-add-window}
  @see-function{gtk:window-application}"
  (application (g:object application))
  (window (g:object window)))

(export 'application-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_windows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_windows" application-windows)
    (g:list-t (g:object window) :free-from-foreign nil)
 #+liber-documentation
 "@version{2024-03-15}
  @argument[application]{a @class{gtk:application} instance}
  @return{The list of @class{gtk:window} widgets.}
  @begin{short}
    Gets a list of the windows associated with the application.
  @end{short}
  The list is sorted by most recently focused windows, such that the first
  element is the currently focused window. This is useful for choosing a parent
  for a transient window.
  @begin[Examples]{dictionary}
    This code from the GTK Demo defines a @code{\"quit\"} action. The signal
    handler gets a list of all windows present in the application and destroys
    the windows. After the last window has been destroyed, the application is
    quit.
    @begin{pre}
;; Add action \"quit\" to the application
(let ((action (g:simple-action-new \"quit\" nil)))
  (g:signal-connect action \"activate\"
      (lambda (action parameter)
        (declare (ignore action parameter))
        ;; Destroy all windows of the application
        (dolist (window (gtk:application-windows application))
          (gtk:widget-destroy window))))
  ;; Add the action to the action map of the application
  (g:action-map-add-action application action))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{gtk:window}"
  (application (g:object application)))

(export 'application-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_window_by_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_window_by_id" application-window-by-id)
    (g:object window)
 #+liber-documentation
 "@version{2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[id]{an unsigned integer for the identifier number}
  @begin{return}
    The @class{gtk:application-window} widget with ID @arg{id}, or @code{nil}
    if there is no window with this ID.
  @end{return}
  @begin{short}
    Returns the application window with the given ID.
  @end{short}
  The ID of an application window can be retrieved with the
  @fun{gtk:application-window-id} function.
  @begin[Notes]{dictionary}
    Both a @class{gtk:window} and a @class{gtk:application-window} widget can be
    added to an application, but only a @class{gtk:application-window} widget
    has an ID.
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{gtk:application-window}
  @see-class{gtk:window}
  @see-function{gtk:application-window-id}"
  (application (g:object application))
  (id :uint))

(export 'application-window-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_inhibit
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_inhibit" application-inhibit) :uint
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[window]{a @class{gtk:window} widget, or @code{nil}}
  @argument[flags]{a @symbol{gtk:application-inhibit-flags} value for the
    types of user actions that should be inhibited}
  @argument[reason]{a short, human readable string that explains why these
    operations are inhibited}
  @begin{return}
    The non-zero unsigned integer cookie that is used to uniquely identify this
    request. It should be used as an argument to the
    @fun{gtk:application-uninhibit} function in order to remove the request. If
    the platform does not support inhibiting or the request failed for some
    reason, 0 is returned.
  @end{return}
  @begin{short}
    Inform the session manager that certain types of actions should be
    inhibited.
  @end{short}
  This is not guaranteed to work on all platforms and for all types of actions.

  Applications should invoke this method when they begin an operation that
  should not be interrupted, such as creating a CD or DVD. The types of
  actions that may be blocked are specified by the @arg{flags} argument. When
  the application completes the operation it should call the
  @fun{gtk:application-uninhibit} function to remove the inhibitor. Note that an
  application can have multiple inhibitors, and all of them must be individually
  removed. Inhibitors are also cleared when the application exits.

  Applications should not expect that they will always be able to block the
  action. In most cases, users will be given the option to force the action to
  take place.

  Reasons should be short and to the point.

  If the @arg{window} argument is given, the session manager may point the user
  to this window to find out more about why the action is inhibited.
  @see-class{gtk:application}
  @see-class{gtk:window}
  @see-symbol{gtk:application-inhibit-flags}
  @see-function{gtk:application-uninhibit}
  @see-function{gtk:application-is-inhibited}"
  (application (g:object application))
  (window (g:object window))
  (flags application-inhibit-flags)
  (reason :string))

(export 'application-inhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_uninhibit
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_uninhibit" application-uninhibit) :void
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[application]{a @class{gtk:application} instance}
  @argument[cookie]{an unsigned integer cookie that was returned by the
    @fun{gtk:application-inhibit} function}
  @begin{short}
    Removes an inhibitor that has been established with the
    @fun{gtk:application-inhibit} function.
  @end{short}
  Inhibitors are also cleared when the application exits.
  @see-class{gtk:application}
  @see-function{gtk:application-inhibit}
  @see-function{gtk:application-is-inhibited}"
  (application (g:object application))
  (cookie :uint))

(export 'application-uninhibit)

;;; ----------------------------------------------------------------------------
;;; gtk_application_is_inhibited
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_is_inhibited" application-is-inhibited) :boolean
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[flags]{a @symbol{gtk:application-inhibit-flags} value for the
    types of actions that should be queried}
  @begin{return}
    @em{True} if any of the actions specified in the @arg{flags} argument are
    inhibited.
  @end{return}
  @begin{short}
    Determines if any of the actions specified in the @arg{flags} argument are
    currently inhibited, possibly by another application.
  @end{short}
  @see-class{gtk:application}
  @see-symbol{gtk:application-inhibit-flags}
  @see-function{gtk:application-inhibit}
  @see-function{gtk:application-uninhibit}"
  (application (g:object application))
  (flags application-inhibit-flags))

(export 'application-is-inhibited)

;;; ----------------------------------------------------------------------------
;;; gtk_application_prefers_app_menu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_prefers_app_menu" application-prefers-app-menu)
    :boolean
 #+liber-documentation
 "@version{#2023-03-15}
  @argument[application]{a @class{gtk:application} instance}
  @begin{return}
    @em{True} if the desktop enviroment would prefer an application menu be
    shown.
  @end{return}
  @begin{short}
    Determines if the desktop environment in which the application is running
    would prefer an application menu be shown.
  @end{short}

  If this function returns @em{true} then the application should call the
  @fun{gtk:application-app-menu} function with the contents of an application
  menu, which will be shown by the desktop environment. If it returns @em{false}
  then you should consider using an alternate approach, such as a menubar.

  The value returned by this function is purely advisory and you are free to
  ignore it. If you call the @fun{gtk:application-app-menu} function even if
  the desktop environment does not support application menus, then a fallback
  will be provided.

  Applications are similarly free not to set an application menu even if the
  desktop environment wants to show one. In that case, a fallback will also be
  created by the desktop environment. GNOME, for example, uses a menu with only
  a \"Quit\" item in it.

  The value returned by this function never changes. Once it returns a
  particular value, it is guaranteed to always return the same value.

  You may only call this function after the application has been registered
  and after the @code{\"startup\"} handler has run. You are most likely to want
  to use this from your own @code{\"startup\"} handler. It may also make sense
  to consult this function in an @code{\"activate\"}, @code{\"open\"} or an
  action activation handler, while constructing the UI in order to determine if
  you should show a gear menu or not.

  This function will return @em{false} on Mac OS and a default application menu
  will be created automatically with the contents of that menu typical to most
  Mac OS applications. If you call the @fun{gtk:application-app-menu} function
  anyway, then this menu will be replaced with your own.
  @see-class{gtk:application}
  @see-function{gtk:application-app-menu}"
  (application (g:object application)))

(export 'application-prefers-app-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_menu_by_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_menu_by_id" application-menu-by-id)
    (g:object menu)
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[id]{a string for the ID of the menu to look up}
  @begin{return}
    Gets the @class{g:menu} object with the given @arg{id} argument from the
    automatically loaded resources.
  @end{return}
  @begin{short}
    Gets a menu from automatically loaded resources.
  @end{short}
  @see-class{gtk:application}
  @see-class{g:menu}"
  (application (g:object application))
  (id :string))

(export 'application-menu-by-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_add_accelerator                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_add_accelerator" %application-add-accelerator)
    :void
  (application (g:object application))
  (accel :string)
  (name :string)
  (parameter (:pointer (:struct g:variant))))

(defun application-add-accelerator (application accel name
                                    &optional (parameter nil))
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[accel]{a string representing an accelerator}
  @argument[name]{a string for the name of the action to activate}
  @argument[parameter]{an optional @symbol{g:variant} parameter to pass when
    activating the action, or @code{nil}, the default, if the action does not
    accept an activation parameter}
  @begin{short}
    Installs an accelerator that will cause the named action to be activated
    when the key combination specificed by the accelerator is pressed.
  @end{short}

  The @arg{accel} argument must be a string that can be parsed by the
  @fun{gtk:accelerator-parse} function, for example, @code{\"<Primary>q\"} or
  @code{\"<Control><Alt>p\"}.

  The @arg{name} argument must be the name of an action as it would be used in
  the application menu, this means that actions that have been added to the
  application are referred to with an @code{\"app.\"} prefix, and window
  specific actions with a @code{\"win.\"} prefix.

  The @class{gtk:application} instance also extracts accelerators out of
  'accel' attributes in the @class{g:menu-model} objects passed to the
  @fun{gtk:application-app-menu} and @fun{gtk:application-menubar} functions,
  which is usually more convenient than calling this function for each
  accelerator.
  @begin[Warning]{dictionary}
    The @fun{gtk:application-add-accelerator} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:application-accels-for-action} function instead.
  @end{dictionary}
  @see-class{gtk:application}
  @see-class{g:menu-model}
  @see-symbol{g:variant}
  @see-function{gtk:accelerator-parse}
  @see-function{gtk:application-app-menu}
  @see-function{gtk:application-menubar}
  @see-function{gtk:application-remove-accelerator}
  @see-function{gtk:application-accels-for-action}"
  (%application-add-accelerator application
                                accel
                                name
                                (or parameter (cffi:null-pointer))))

;;; ----------------------------------------------------------------------------
;;; gtk_application_remove_accelerator                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_remove_accelerator"
                %application-remove-accelerator) :void
  (application (g:object application))
  (name :string)
  (parameter (:pointer (:struct g:variant))))

(defun application-remove-accelerator (application name
                                       &optional (parameter nil))
 #+liber-documentation
 "@version{#2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[name]{a string for the name of the action to activate}
  @argument[parameter]{an optional @symbol{g:variant} parameter to pass when
    activating the action, or @code{nil}, the default, if the action does not
    accept an activation parameter}
  @begin{short}
    Removes an accelerator that has been previously added with the
    @fun{gtk:application-add-accelerator} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:application-remove-accelerator} function has been deprecated
    since version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:application-accels-for-action} function instead.
  @end{dictionary}
  @see-class{gtk:application}
  @see-symbol{g:variant}
  @see-function{gtk:application-add-accelerator}
  @see-function{gtk:application-accels-for-action}"
  (%application-remove-accelerator application
                                   name
                                   (or parameter (cffi:null-pointer))))

;;; ----------------------------------------------------------------------------
;;; gtk_application_list_action_descriptions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_list_action_descriptions"
                application-list-action-descriptions) g:strv-t
 #+liber-documentation
 "@version{2023-12-24}
  @argument[application]{a @class{gtk:application} instance}
  @return{The list of strings with the detailed action names.}
  @begin{short}
    Lists the detailed action names which have associated accelerators.
  @end{short}
  See the @fun{gtk:application-accels-for-action} function.
  @see-class{gtk:application}
  @see-function{gtk:application-accels-for-action}"
  (application (g:object application)))

(export 'application-list-action-descriptions)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_accels_for_action
;;; gtk_application_set_accels_for_action
;;; ----------------------------------------------------------------------------

(defun (setf application-accels-for-action) (accels application name)
  (cffi:foreign-funcall "gtk_application_set_accels_for_action"
                        (g:object application) application
                        :string name
                        g:strv-t (if (listp accels) accels (list accels))
                        :void)
  accels)

(cffi:defcfun ("gtk_application_get_accels_for_action"
                application-accels-for-action) g:strv-t
 #+liber-documentation
 "@version{2025-06-02}
  @syntax{(gtk:application-accels-for-action application name) => accels}
  @syntax{(setf (gtk:application-accels-for-action application name) accels)}
  @argument[application]{a @class{gtk:application} instance}
  @argument[name]{a string for a detailed action name, specifying an action
    and target}
  @argument[accels]{a string or a list of strings of accelerators in the format
    understood by the @fun{gtk:accelerator-parse} function}
  @begin{short}
    The @fun{gtk:application-accels-for-action} function gets the keyboard
    accelerators that will trigger the given action.
  @end{short}
  The @setf{gtk:application-accels-for-action} function sets zero or more
  keyboard accelerators.

  The first item in the list of accelerators will be the primary accelerator,
  which may be displayed in the UI. To remove all accelerators for an action,
  use an empty list.

  For the detailed action name, see the @fun{g:action-parse-detailed-name} and
  @fun{g:action-print-detailed-name} functions.
  @see-class{gtk:application}
  @see-function{gtk:accelerator-parse}
  @see-function{g:action-parse-detailed-name}
  @see-function{g:action-print-detailed-name}"
  (application (g:object application))
  (name :string))

(export 'application-accels-for-action)

;;; ----------------------------------------------------------------------------
;;; gtk_application_get_actions_for_accel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_get_actions_for_accel"
                application-actions-for-accel) g:strv-t
 "@version{2025-06-02}
  @argument[application]{a @class{gtk:application} instance}
  @argument[accel]{a string for an accelerator that can be parsed by the
    @fun{gtk:accelerator-parse} function}
  @return{The list of strings of actions for the @arg{accel} argument.}
  @begin{short}
    Returns the list of actions, possibly empty, that the given accelerator
    maps to.
  @end{short}
  Each item in the list is a detailed action name in the usual form.

  This might be useful to discover if an accelerator already exists in order
  to prevent installation of a conflicting accelerator, from an accelerator
  editor or a plugin system, for example. Note that having more than one
  action per accelerator may not be a bad thing and might make sense in cases
  where the actions never appear in the same context.

  In case there are no actions for a given accelerator, an empty list is
  returned.

  It is a programmer error to pass an invalid accelerator string. If you are
  unsure, check it with the @fun{gtk:accelerator-parse} function first.
  @see-class{gtk:application}
  @see-function{gtk:accelerator-parse}"
  (application (g:object application))
  (accel :string))

(export 'application-actions-for-accel)

;;; --- End of file gtk3.application.lisp --------------------------------------
