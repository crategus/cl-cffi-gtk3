;;; ----------------------------------------------------------------------------
;;; gtk3.places-sidebar.lisp
;;;
;;; The documentation of this file is taken from the GTK Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkPlacesSidebar
;;;
;;;     Sidebar that displays frequently-used places in the file system
;;;
;;; Types and Values
;;;
;;;     GtkPlacesSidebar
;;;     GtkPlacesOpenFlags
;;;
;;; Functions
;;;
;;;     gtk_places_sidebar_new
;;;     gtk_places_sidebar_set_open_flags                  Accessor
;;;     gtk_places_sidebar_get_open_flags                  Accessor
;;;     gtk_places_sidebar_set_location                    Accessor
;;;     gtk_places_sidebar_get_location                    Accessor
;;;     gtk_places_sidebar_set_show_recent                 Accessor
;;;     gtk_places_sidebar_get_show_recent                 Accessor
;;;     gtk_places_sidebar_set_show_desktop                Accessor
;;;     gtk_places_sidebar_get_show_desktop                Accessor
;;;     gtk_places_sidebar_add_shortcut
;;;     gtk_places_sidebar_remove_shortcut
;;;     gtk_places_sidebar_list_shortcuts
;;;     gtk_places_sidebar_get_nth_bookmark
;;;     gtk_places_sidebar_get_show_connect_to_server      Accessor
;;;     gtk_places_sidebar_set_show_connect_to_server      Accessor
;;;     gtk_places_sidebar_get_local_only                  Accessor
;;;     gtk_places_sidebar_set_local_only                  Accessor
;;;     gtk_places_sidebar_get_show_enter_location         Accessor
;;;     gtk_places_sidebar_set_show_enter_location         Accessor
;;;     gtk_places_sidebar_get_show_trash                  Accessor
;;;     gtk_places_sidebar_set_show_trash                  Accessor
;;;     gtk_places_sidebar_get_show_other_locations        Accessor
;;;     gtk_places_sidebar_set_show_other_locations        Accessor
;;;     gtk_places_sidebar_set_drop_targets_visible
;;;
;;; Properties
;;;
;;;     local-only
;;;     location
;;;     open-flags
;;;     populate-all
;;;     show-connect-to-server
;;;     show-desktop
;;;     show-enter-location
;;;     show-other-locations
;;;     show-recent
;;;     show-starred-location
;;;     show-trash
;;;
;;; Signals
;;;
;;;     drag-action-ask
;;;     drag-action-requested
;;;     drag-perform-drop
;;;     mount
;;;     open-location
;;;     populate-popup
;;;     show-connect-to-server
;;;     show-enter-location
;;;     show-error-message
;;;     show-other-locations
;;;     show-other-locations-with-flags
;;;     show-starred-location
;;;     unmount
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkScrolledWindow
;;;                         ╰── GtkPlacesSidebar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPlacesSidebar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkPlacesOpenFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkPlacesOpenFlags" places-open-flags
  (:export t
   :type-initializer "gtk_places_open_flags_get_type")
  (:normal     #.(ash 1 0))
  (:new-tab    #.(ash 1 1))
  (:new-window #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'places-open-flags)
      "GFlags"
      (liber:symbol-documentation 'places-open-flags)
 "@version{#2023-3-6}
  @begin{short}
    These flags serve two purposes.
  @end{short}
  First, the application can call the @fun{gtk:places-sidebar-open-flags}
  function using these flags as a bitmask. This tells the sidebar that the
  application is able to open folders selected from the sidebar in various ways,
  for example, in new tabs or in new windows in addition to the normal mode.

  Second, when one of these values gets passed back to the application in the
  \"open-location\" signal, it means that the application should open the
  selected location in the normal way, in a new tab, or in a new window. The
  sidebar takes care of determining the desired way to open the location, based
  on the modifier keys that the user is pressing at the time the selection is
  made.

  If the application never calls the @fun{gtk:places-sidebar-open-flags}
  function, then the sidebar will only use @code{:normal} in the
  \"open-location\" signal. This is the default mode of operation.
  @begin{pre}
(define-g-flags \"GtkPlacesOpenFlags\" gtk:places-open-flags
  (:export t
   :type-initializer \"gtk_places_open_flags_get_type\")
  (:normal     #.(ash 1 0))
  (:new-tab    #.(ash 1 1))
  (:new-window #.(ash 1 2)))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{This is the default mode that the @class{gtk:places-sidebar}
      widget uses if no other flags are specified. It indicates that the calling
      application should open the selected location in the normal way, for
      example, in the folder view beside the sidebar.}
    @entry[:new-tab]{When passed to the @fun{gtk:places-sidebar-open-flags}
      function, this indicates that the application can open folders selected
      from the sidebar in new tabs. This value will be passed to the
      \"open-location\" signal when the user selects that a location be opened
      in a new tab instead of in the standard fashion.}
    @entry[:new-window]{Similar to @code{:new-tab}, but indicates that the
      application can open folders in new windows.}
  @end{table}
  @see-class{gtk:places-sidebar}
  @see-function{gtk:places-sidebar-open-flags}")

;;; ----------------------------------------------------------------------------
;;; struct GtkPlacesSidebar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPlacesSidebar" places-sidebar
  (:superclass scrolled-window
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_places_sidebar_get_type")
  ((local-only
    places-sidebar-local-only
    "local-only" "gboolean" t t)
   (location
    places-sidebar-location
    "location" "GFile" t t)
   (open-flags
    places-sidebar-open-flags
    "open-flags" "GtkPlacesOpenFlags" t t)
   (populate-all
    places-sidebar-populate-all
    "populate-all" "gboolean" t t)
   (show-connect-to-server
    places-sidebar-show-connect-to-server
    "show-connect-to-server" "gboolean" t t)
   (show-desktop
    places-sidebar-show-desktop
    "show-desktop" "gboolean" t t)
   (show-enter-location
    places-sidebar-show-enter-location
    "show-enter-location" "gboolean" t t)
   (show-other-locations
    places-sidebar-show-other-locations
    "show-other-locations" "gboolean" t t)
   (show-recent
    places-sidebar-show-recent
    "show-recent" "gboolean" t t)
   (show-starred-location
    places-sidebar-show-starred-location
    "show-starred-location" "gboolean" t t)
   (show-trash
    places-sidebar-show-trash
    "show-trash" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'places-sidebar 'type)
 "@version{#2023-3-6}
  @begin{short}
    The @sym{gtk:places-sidebar} widget is a widget that displays a list of
    frequently-used places in the file system: the user’s home directory, the
    user’s bookmarks, and volumes and drives.
  @end{short}

  @image[places-sidebar]{Figure: GtkPlacesSidebar}

  This widget is used as a sidebar in the @class{gtk:file-chooser} interface
  and may be used by file managers and similar programs. The places sidebar
  displays drives and volumes, and will automatically mount or unmount them
  when the user selects them. Applications can hook to various signals in the
  places sidebar to customize its behavior. For example, they can add extra
  commands to the context menu of the sidebar.

  While bookmarks are completely in control of the user, the places sidebar
  also allows individual applications to provide extra shortcut folders that
  are unique to each application. For example, a Paint program may want to add
  a shortcut for a Clipart folder. You can do this with the
  @fun{gtk:places-sidebar-add-shortcut} function.

  To make use of the places sidebar, an application at least needs to connect
  to the \"open-location\" signal. This is emitted when the user selects in the
  sidebar a location to open. The application should also call the
  @fun{gtk:places-sidebar-location} function when it changes the currently
  viewed location.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:places-sidebar} implementation uses a single CSS node with name
    @code{placessidebar} and @code{.sidebar} style class.

    Among the children of the places sidebar, the following style classes can
    be used:
    @begin{itemize}
      @item{@code{.sidebar-new-bookmark-row} for the 'Add new bookmark' row}
      @item{@code{.sidebar-placeholder-row} for a row that is a placeholder}
      @item{@code{.has-open-popup} when a popup is open for a row}
    @end{itemize}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"drag-action-ask\" signal}
      @begin{pre}
lambda (sidebar actions)    :run-last
      @end{pre}
      The places sidebar emits this signal when it needs to ask the application
      to pop up a menu to ask the user for which drag action to perform.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received
          the signal.}
        @entry[actions]{An integer with the possible drag actions that need to
          be asked for.}
        @entry[Returns]{An integer with the final drag action that the sidebar
          should pass to the drag side of the drag and drop operation.}
      @end{table}
    @subheading{The \"drag-action-requested\" signal}
      @begin{pre}
lambda (sidebar context dest source)    :run-last
      @end{pre}
      When the user starts a drag and drop operation and the sidebar needs to
      ask the application for which drag action to perform, then the sidebar
      will emit this signal. The application can evaluate the context for
      customary actions, or it can check the type of the files indicated by
      @arg{source} against the possible actions for the destination @arg{dest}.
      The drag action to use must be the return value of the signal handler.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[context]{The @class{gdk:drag-context} object with information
          about the drag operation.}
        @entry[dest]{A @class{g:file} object with the tentative location that
          is being hovered for a drop.}
        @entry[source]{List of @class{g:file} objects that are being dragged.}
        @entry[Returns]{An integer with the drag action to use, for example,
        @code{GDK_ACTION_COPY} or @code{GDK_ACTION_MOVE}, or 0 if no action is
        allowed here, i.e. drops are not allowed in the specified @arg{dest}.}
      @end{table}
    @subheading{The \"drag-perform-drop\" signal}
      @begin{pre}
lambda (sidebar dest source action)    :run-first
      @end{pre}
      The places sidebar emits this signal when the user completes a drag and
      drop operation and one of the sidebar's items is the destination. This
      item is in the @arg{dest}, and the @arg{source} has the list of files
      that are dropped into it and which should be copied/moved/ etc. based on
      the specified action.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[dest]{Destination @class{g:file} object.}
        @entry[source]{List of @class{g:file} objects that got dropped.}
        @entry[action]{An integer with the drop action to perform.}
      @end{table}
    @subheading{The \"mount\" signal}
      @begin{pre}
lambda (sidebar mount)    :run-first
      @end{pre}
      The places sidebar emits this signal when it starts a new operation
      because the user clicked on some location that needs mounting. In this way
      the application using the @sym{gtk:places-sidebar} widget can track the
      progress of the operation and, for example, show a notification.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[mount]{The @class{g:mount-operation} object that is going to
          start.}
      @end{table}
    @subheading{The \"open-location\" signal}
      @begin{pre}
lambda (sidebar location flags)    :run-first
      @end{pre}
      The places sidebar emits this signal when the user selects a location in
      it. The calling application should display the contents of that location;
      for example, a file manager should show a list of files in the specified
      location.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[location]{The @class{g:file} object to which the caller should
          switch.}
        @entry[flags]{A single value from the @symbol{gtk:places-open-flags}
          flags specifying how the location should be opened.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
lambda (sidebar container item volume)    :run-first
      @end{pre}
      The places sidebar emits this signal when the user invokes a contextual
      popup on one of its items. In the signal handler, the application may add
      extra items to the menu as appropriate. For example, a file manager may
      want to add a \"Properties\" command to the menu.

      It is not necessary to store the @arg{item} for each menu item; during
      their callbacks, the application can use the
      @fun{gtk:places-sidebar-location} function to get the file to which the
      item refers.

      The @arg{item} argument may be @code{nil} in case the selection refers to
      a volume. In this case, @arg{volume} will be non-@code{nil}. In this case,
      the calling application will have to the @fun{g:object-ref} function the
      @arg{volume} and keep it around to use it in the callback.

      The container and all its contents are destroyed after the user dismisses
      the popup. The popup is re-created, and thus, this signal is emitted,
      every time the user activates the contextual menu.

      Before 3.18, the container always was a @class{gtk:menu} widget, and you
      were expected to add your items as @class{gtk:menu-item} objects. Since
      3.18, the popup may be implemented as a @class{gtk:popover} widget, in
      which case container will be something else, e.g. a @class{gtk:box}
      widget, to which you may add @class{gtk:model-button} widgets or other
      widgets, such as @class{gtk:entry}, @class{gtk:spin-button} widgets, etc.
      If your application can deal with this situation, you can set
      @code{populate-all} to @em{true} to request that this signal is emitted
      for populating popovers as well.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[container]{A @class{gtk:menu} widget or another
          @class{gtk:container} widget.}
        @entry[item]{A @class{g:file} object with the item to which the popup
          should refer, or @code{nil} in the case of a @arg{volume}.}
        @entry[volume]{A @class{g:volume} object if the selected item is a
          volume, or @code{nil} if it is a file.}
      @end{table}
    @subheading{The \"show-connect-to-server\" signal}
      @begin{pre}
lambda (sidebar)    :run-first
      @end{pre}
      The places sidebar emits this signal when it needs the calling application
      to present an way to connect directly to a network server. For example,
      the application may bring up a dialog box asking for a URL like
      @file{sftp://ftp.example.com}. It is up to the application to create the
      corresponding mount by using, for example,
      @code{g_file_mount_enclosing_volume()}.

      @em{Warning:} The \"show-connect-to-server\" signal has been deprecated
      since version 3.18 and should not be used in newly written code. Use the
      \"show-other-locations\" signal to connect to network servers.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"show-enter-location\" signal}
      @begin{pre}
lambda (sidebar)    :run-first
      @end{pre}
      The places sidebar emits this signal when it needs the calling application
      to present an way to directly enter a location. For example, the
      application may bring up a dialog box asking for a URL like
      @file{http://http.example.com}.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"show-error-message\" signal}
      @begin{pre}
lambda (sidebar primary secondary)    :run-first
      @end{pre}
      The places sidebar emits this signal when it needs the calling application
      to present an error message. Most of these messages refer to mounting or
      unmounting media, for example, when a drive cannot be started for some
      reason.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[primary]{A string with the primary message with a summary of the
          error to show.}
        @entry[secondary]{A string with the secondary message with details of
          the error to show.}
      @end{table}
    @subheading{The \"show-other-locations\" signal}
      @begin{pre}
lambda (sidebar)    :run-first
      @end{pre}
      The places sidebar emits this signal when it needs the calling application
      to present a way to show other locations e.g. drives and network access
      points. For example, the application may bring up a page showing
      persistent volumes and discovered network addresses.

      @em{Warning:} The \"show-other-locations\" signal has been deprecated
      since version 3.20 and should not be used in newly written code. Use the
      \"show-other-locations-with-flags\" signal which includes the open flags
      in order to allow the user to specify to open in a new tab or window, in
      a similar way than the \"open-location\" signal.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
      @end{table}
    @subheading{The \"show-other-locations-with-flags\" signal}
      @begin{pre}
lambda (sidebar flags)    :run-first
      @end{pre}
      The places sidebar emits this signal when it needs the calling application
      to present a way to show other locations e.g. drives and network access
      points. For example, the application may bring up a page showing
      persistent volumes and discovered network addresses.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[flags]{A single @symbol{gtk:places-open-flags} value specifying
          how it should be opened.}
      @end{table}
    @subheading{The \"show-starred-location\" signal}
      @begin{pre}
lambda (sidebar flags)    :run-first
      @end{pre}
      The places sidebar emits this signal when it needs the calling application
      to present a way to show the starred files. In GNOME, starred files are
      implemented by setting the @code{nao:predefined-tag-favorite} tag in the
      tracker database.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[flags]{A single @symbol{gtk:places-open-flags} value specifying
          how the starred file should be opened.}
      @end{table}
    @subheading{The \"unmount\" signal}
      @begin{pre}
lambda (sidebar mount)    :run-first
      @end{pre}
      The places sidebar emits this signal when it starts a new operation
      because the user for example ejected some drive or unmounted a mount. In
      this way the application using the @sym{gtk:places-sidebar} widget can
      track the progress of the operation and, for example, show a notification.
      @begin[code]{table}
        @entry[sidebar]{The @sym{gtk:places-sidebar} widget which received the
          signal.}
        @entry[mount]{The @code{GMountOperation} that is going to
          start.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:places-sidebar-new}
  @see-slot{gtk:places-sidebar-local-only}
  @see-slot{gtk:places-sidebar-location}
  @see-slot{gtk:places-sidebar-open-flags}
  @see-slot{gtk:places-sidebar-populate-all}
  @see-slot{gtk:places-sidebar-show-connect-to-server}
  @see-slot{gtk:places-sidebar-show-desktop}
  @see-slot{gtk:places-sidebar-show-enter-location}
  @see-slot{gtk:places-sidebar-show-other-locations}
  @see-slot{gtk:places-sidebar-show-recent}
  @see-slot{gtk:places-sidebar-show-starred-location}
  @see-slot{gtk:places-sidebar-show-trash}
  @see-class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- places-sidebar-local-only ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "local-only" 'places-sidebar) t)
 "The @code{local-only} property of type @code{:boolean} (Read / Write) @br{}
  Whether the sidebar only includes local files. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-local-only)
      "Accessor"
      (documentation 'places-sidebar-local-only 'function)
 "@version{#2023-3-6}
  @syntax[]{(gtk:places-sidebar-local-only object) => local-only}
  @syntax[]{(setf (gtk:places-sidebar-local-only object) local-only)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[local-only]{a boolean whether to show only local files}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{local-only} slot of the
    @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should only show local files.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-location ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "location" 'places-sidebar) t)
 "The @code{location} property of type @class{g:file} (Read / Write) @br{}
  The location to highlight in the sidebar.")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-location)
      "Accessor"
      (documentation 'places-sidebar-location 'function)
 "@version{#2023-3-6}
  @syntax[]{(gtk:places-sidebar-location object) => location}
  @syntax[]{(setf (gtk:places-sidebar-location object) location)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[location]{a @class{g:file} object with a location to select, or
    @code{nil} for no current path}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{location} slot of the
    @class{gtk:places-sidebar} class.
  @end{short}
  The @sym{gtk:places-sidebar-location} function gets the currently selected
  location in the sidebar. This can be @code{nil} when nothing is selected, for
  example, when the @sym{gtk:places-sidebar-location} function has been called
  with a location that is not among the sidebar’s list of places to show.

  You can use this function to get the selection in the sidebar. Also, if you
  connect to the \"populate-popup\" signal, you can use this function to get the
  location that is being referred to during the callbacks for your menu items.

  The @sym{(setf gtk:places-sidebar-location)} function sets the location that
  is being shown in the widgets surrounding the sidebar, for example, in a
  folder view in a file manager. In turn, the sidebar will highlight that
  location if it is being shown in the list of places, or it will unhighlight
  everything if the location is not among the places in the list.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-open-flags ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "open-flags" 'places-sidebar) t)
 "The @code{open-flags} property of type @symbol{gtk:places-open-flags}
  (Read / Write) @br{}
  Modes in which the calling application can open locations selected in the
  sidebar. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-open-flags)
      "Accessor"
      (documentation 'places-sidebar-open-flags 'function)
 "@version{#2023-3-6}
  @syntax[]{(gtk:places-sidebar-open-flags object) => flags}
  @syntax[]{(setf (gtk:places-sidebar-open-flags object) flags)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[flags]{a @symbol{gtk:places-open-flags} bitmask of modes in which
    the calling application can open locations}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{open-flags} slot of the
    @class{gtk:places-sidebar} class.
  @end{short}
  The @sym{gtk:places-sidebar-open-flags} function gets the open flags. The
  @sym{(setf gtk:places-sidebar-open-flags)} function sets the way in which the
  calling application can open new locations from the places sidebar. For
  example, some applications only open locations \"directly\" into their main
  view, while others may support opening locations in a new notebook tab or a
  new window.

  This function is used to tell the places sidebar about the ways in which the
  application can open new locations, so that the sidebar can display (or not)
  the \"Open in new tab\" and \"Open in new window\" menu items as appropriate.

  When the \"open-location\" signal is emitted, its flags argument will be set
  to one of the flags that was passed in the @sym{gtk:places-sidebar-open-flags}
  function.

  Passing 0 for flags will cause @code{:normal} to always be sent to callbacks
  for the \"open-location\" signal.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-populate-all --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "populate-all"
                                               'places-sidebar) t)
 "The @code{populate-all} property of type @code{:boolean} (Read / Write) @br{}
  If @code{populate-all} is @em{true}, the \"populate-popup\" signal is also
  emitted for popovers. @br{}
  Default value: @em{false}")

#+ liber-documentation
(setf (liber:alias-for-function 'places-sidebar-populate-all)
      "Accessor"
      (documentation 'places-sidebar-populate-all 'function)
 "@version{#2023-3-13}
  @syntax[]{(gtk:places-sidebar-populate-all object) => populate-all}
  @syntax[]{(setf (gtk:places-sidebar-populate-all object) populate-all)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[populate-all]{a boolean whether the \"populate-all\" signal is also
    emitted for popovers}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{populate-all} slot of the
    @class{gtk:places-sidebar} class.
  @end{short}
  If @code{populate-all} is @em{true}, the \"populate-popup\" signal is also
  emitted for popovers.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-connect-to-server ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-connect-to-server"
                                               'places-sidebar) t)
 "The @code{show-connect-to-server} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the sidebar includes a builtin shortcut to a 'Connect to server'
  dialog. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-connect-to-server)
      "Accessor"
      (documentation 'places-sidebar-show-connect-to-server 'function)
 "@version{#2023-3-6}
  @syntax[]{(gtk:places-sidebar-show-connect-to-server object) => setting}
  @syntax[]{(setf (gtk:places-sidebar-show-connect-to-server object) setting)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[setting]{a boolean whether to show an item for the Connect to
    Server command}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-connect-to-server} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should show an item for connecting to a network
  server. This is off by default. An application may want to turn this on if
  it implements a way for the user to connect to network servers directly.

  If you enable this, you should connect to the \"show-connect-to-server\"
  signal.
  @begin[Warning]{dictionary}
    The @sym{gtk:places-sidebar-show-connect-to-server} function has been
    deprecated since version 3.18 and should not be used in newly written code.
    It is recommended to group this functionality with the drives and network
    location under the new 'Other Location' item.
  @end{dictionary}
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-desktop --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-desktop"
                                               'places-sidebar) t)
 "The @code{show-desktop} property of type @code{:boolean} (Read / Write) @br{}
  Whether the sidebar includes a builtin shortcut to the Desktop folder. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-desktop)
      "Accessor"
      (documentation 'places-sidebar-show-desktop 'function)
 "@version{#2023-3-6}
  @syntax[]{(gtk:places-sidebar-show-desktop) => show-desktop}
  @syntax[]{(setf (gtk:places-sidebar-show-desktop object) show-desktop)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[show-desktop]{a boolean whether to show an item for the Desktop
    folder}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-desktop} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should show an item for the Desktop folder. The
  default value for this option is determined by the desktop environment and
  the user’s configuration, but this function can be used to override it on a
  per-application basis.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-enter-location ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-enter-location"
                                               'places-sidebar) t)
 "The @code{show-enter-location} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the sidebar includes a builtin shortcut to manually enter a location.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-enter-location)
      "Accessor"
      (documentation 'places-sidebar-show-enter-location 'function)
 "@version{#2023-3-6}
  @syntax[]{(gtk:places-sidebar-show-enter-location) => setting}
  @syntax[]{(setf (gtk:places-sidebar-show-enter-location object) setting)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument{setting]{a boolean whether to show an item to enter a location}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-enter-location} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should show an item for entering a location. This
  is off by default. An application may want to turn this on if manually
  entering URLs is an expected user action.

  If you enable this, you should connect to the \"show-enter-location\" signal.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-other-locations ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-other-locations"
                                               'places-sidebar) t)
 "The @code{show-other-locations} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the sidebar includes an item to show external locations. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-other-locations)
      "Accessor"
      (documentation 'places-sidebar-show-other-locations 'function)
 "@version{#2023-3-13}
  @syntax[]{(gtk:places-sidebar-show-other-locations) => setting}
  @syntax[]{(setf (gtk:places-sidebar-show-other-locations object) setting)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[setting]{a boolean whether to show an item for the Other Locations
    view}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-other-locations} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should show an item for the application to show an
  Other Locations view. This is off by default. When set to @em{true},
  persistent devices such as hard drives are hidden, otherwise they are shown in
  the sidebar. An application may want to turn this on if it implements a way
  for the user to see and interact with drives and network servers directly.

  If you enable this, you should connect to the \"show-other-locations\" signal.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-recent ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-recent" 'places-sidebar) t)
 "The @code{show-recent} property of type @code{:boolean} (Read / Write) @br{}
  Whether the sidebar includes a builtin shortcut for recent files. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-recent)
      "Accessor"
      (documentation 'places-sidebar-show-recent 'function)
 "@version{#2023-3-13}
  @syntax[]{(gtk:places-sidebar-show-recent) => show-recent}
  @syntax[]{(setf (gtk:places-sidebar-show-recent object) show-recent)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[show-recent]{a boolean whether to show an item for recent files}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-recent} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should show an item for recent files. The default
  value for this option is determined by the desktop environment, but this
  function can be used to override it on a per-application basis.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-starred-location -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-starred-location"
                                               'places-sidebar) t)
 "The @code{show-starred-location} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the sidebar includes an item to show starred files. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-starred-location)
      "Accessor"
      (documentation 'places-sidebar-show-starred-location 'function)
 "@version{#2023-3-13}
  @syntax[]{(gtk:places-sidebar-show-starred-location object) => setting}
  @syntax[]{(setf (gtk:places-sidebar-show-starred-location object) setting)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[setting]{a boolean whether the sidebar includes an item to show
    starred files}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-starred-location} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Whether the sidebar includes an item to show starred files.
  @see-class{gtk:places-sidebar}")

;;; --- places-sidebar-show-trash ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-trash" 'places-sidebar) t)
 "The @code{show-trash} property of type @code{:boolean} (Read / Write) @br{}
  Whether the sidebar includes a builtin shortcut to the Trash location. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'places-sidebar-show-trash)
      "Accessor"
      (documentation 'places-sidebar-show-trash 'function)
 "@version{#2023-3-13}
  @syntax[]{(gtk:places-sidebar-show-trash) => show-trash}
  @syntax[]{(setf (gtk:places-sidebar-show-trash object) show-trash)}
  @argument[object]{a @class{gtk:places-sidebar} widget}
  @argument[show-trash]{a boolean whether to show an item for the Trash
    location}
  @begin{short}
    Accessor of the @slot[gtk:places-sidebar]{show-trash} slot of
    the @class{gtk:places-sidebar} class.
  @end{short}
  Sets whether the sidebar should show an item for the Trash location.
  @see-class{gtk:places-sidebar}")

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline places-sidebar-new))

(defun places-sidebar-new ()
 #+liber-documentation
 "@version{2023-3-6}
  @return{A newly created @class{gtk:places-sidebar} widget.}
  @begin{short}
    Creates a new @class{gtk:places-sidebar} widget.
  @end{short}
  The application should connect to at least the \"open-location\" signal to be
  notified when the user makes a selection in the sidebar.
  @see-class{gtk:places-sidebar}"
  (make-instance 'places-sidebar))

(export 'places-sidebar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_add_shortcut ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_places_sidebar_add_shortcut" places-sidebar-add-shortcut) :void
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[sidebar]{a @class{gtk:places-sidebar} widget}
  @argument[location]{a pathname or namestring with the location to add as an
    application specific shortcut}
  @begin{short}
    Applications may want to present some folders in the places sidebar if they
    could be immediately useful to users.
  @end{short}
  For example, a drawing program could add a @file{/usr/share/clipart} location
  when the sidebar is being used in an \"Insert Clipart\" dialog box.

  This function adds the specified location to a special place for immutable
  shortcuts. The shortcuts are application specific. They are not shared
  across applications, and they are not persistent. If this function is called
  multiple times with different locations, then they are added to the sidebar’s
  list in the same order as the function is called.
  @see-class{gtk:places-sidebar}"
  (sidebar (g:object places-sidebar))
  (location g:file-as-namestring))

(export 'places-sidebar-add-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_remove_shortcut ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_places_sidebar_remove_shortcut" places-sidebar-remove-shortcut)
    :void
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[sidebar]{a @class{gtk:places-sidebar} widget}
  @argument[location]{a pathname or namestring with the location to remove}
  @begin{short}
    Removes an application specific shortcut that has been previously been
    inserted with the @fun{gtk:places-sidebar-add-shortcut} function.
  @end{short}
  If the location is not a shortcut in the sidebar, then nothing is done.
  @see-class{gtk:places-sidebar}
  @see-function{gtk:places-sidebar-add-shortcut}"
  (sidebar (g:object places-sidebar))
  (location g:file-as-namestring))

(export 'places-sidebar-remove-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_list_shortcuts ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_places_sidebar_list_shortcuts" places-sidebar-list-shortcuts)
    (g:slist-t g:file-as-namestring)
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[sidebar]{a @class{gtk:places-sidebar} widget}
  @return{A list of namestrings with the locations that have been added as
    application specific shortcuts.}
  @begin{short}
    Gets the list of shortcuts that have been added as application specific
    shortcuts with the @fun{gtk:places-sidebar-add-shortcut} function.
  @end{short}
  @see-class{gtk:places-sidebar}
  @see-function{gtk:places-sidebar-add-shortcut}"
  (sidebar (g:object places-sidebar)))

(export 'places-sidebar-list-shortcuts)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_get_nth_bookmark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_places_sidebar_get_nth_bookmark" places-sidebar-nth-bookmark)
    g:file-as-namestring
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[sidebar]{a @class{gtk:places-sidebar} widget}
  @argument[n]{an integer with the index of bookmark to query}
  @return{An namestring with the bookmark specified by the index @arg{n}, or
    @code{nil} if no such index exist. Note that the indices start at 0, even
    though the file chooser starts them with the keyboard shortcut @kbd{Alt-1}.}
  @begin{short}
    This function queries the bookmarks added by the user to the places sidebar,
    and returns one of them.
  @end{short}
  This function is used by the @class{gtk:file-chooser} widget to implement the
  @kbd{Alt-1}, @kbd{Alt-2}, etc. shortcuts, which activate the cooresponding
  bookmark.
  @see-class{gtk:places-sidebar}"
  (sidebar (g:object places-sidebar))
  (n :int))

(export 'places-sidebar-nth-bookmark)

;;; ----------------------------------------------------------------------------
;;; gtk_places_sidebar_set_drop_targets_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_places_sidebar_set_drop_targets_visible"
           places-sidebar-set-drop-targets-visible) :void
 #+liber-documentation
 "@version{#2023-3-6}
  @argument[sidebar]{a @class{gtk:places-sidebar} widget}
  @argument[visible]{a boolean whether to show the valid targets or not}
  @argument[context]{a @class{gdk:drag-context} drag context used to ask the
    source about the action that wants to perform, so hints are more accurate}
  @begin{short}
    Make the @class{gtk:places-sidebar} object show drop targets, so it can show
    the available drop targets and a \"new bookmark\" row.
  @end{short}
  This improves the Drag and Drop experience of the user and allows applications
  to show all available drop targets at once.

  This needs to be called when the application is aware of an ongoing drag that
  might target the sidebar. The drop-targets-visible state will be unset
  automatically if the drag finishes in the @class{gtk:places-sidebar} object.
  You only need to unset the state when the drag ends on some other widget on
  your application.
  @see-class{gtk:places-sidebar}
  @see-class{gdk:drag-context}"
  (sidebar (g:object places-sidebar))
  (visible :boolean)
  (context (g:object gdk:drag-context)))

(export 'places-sidebar-set-drop-targets-visible)

;;; --- End of file gtk3.places-sidebar.lisp -----------------------------------
