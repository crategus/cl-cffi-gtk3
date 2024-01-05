;;; ----------------------------------------------------------------------------
;;; gtk3.application-window.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; GtkApplicationWindow
;;;
;;;     GtkWindow subclass with GtkApplication support
;;;
;;; Types and Values
;;;
;;;     GtkApplicationWindow
;;;
;;; Accessors
;;;
;;;     gtk_application_window_set_show_menubar
;;;     gtk_application_window_get_show_menubar
;;;
;;; Functions
;;;
;;;     gtk_application_window_new
;;;     gtk_application_window_get_id
;;;     gtk_application_window_set_help_overlay
;;;     gtk_application_window_get_help_overlay
;;;
;;; Properties
;;;
;;;     show-menubar
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkApplicationWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkApplicationWindow implements AtkImplementorIface, GtkBuildable,
;;;     GActionGroup and GActionMap.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkApplicationWindow
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkApplicationWindow" application-window
  (:superclass window
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GActionGroup"
                "GActionMap")
   :type-initializer "gtk_application_window_get_type")
   ((show-menubar
     application-window-show-menubar
     "show-menubar" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'application-window 'type)
 "@version{2023-12-24}
  @begin{short}
    The @class{gtk:application-window} class is a @class{gtk:window} subclass
    that offers some extra functionality for better integration with
    @class{gtk:application} features.
  @end{short}
  Notably, it can handle both the application menu as well as the menubar.
  See the @fun{gtk:application-app-menu} and @fun{gtk:application-menubar}
  functions.

  This class implements the @class{g:action-group} and @class{g:action-map}
  interfaces, to let you add window specific actions that will be exported by
  the associated @class{gtk:application} instance, together with its
  application-wide actions. Window specific actions are prefixed with the
  \"win.\" prefix and application-wide actions are prefixed with the \"app.\"
  prefix. Actions must be addressed with the prefixed name when referring to
  them from a @class{g:menu-model} object.

  Note that widgets that are placed inside an application window can also
  activate these actions, if they implement the @class{gtk:actionable}
  interface.

  As with the @class{gtk:application} class, the GDK lock will be acquired when
  processing actions arriving from other processes and should therefore be held
  when activating actions locally if GDK threads are enabled.

  The @slot[gtk:settings]{gtk-shell-shows-app-menu} and
  @slot[gtk:settings]{gtk-shell-shows-menubar} settings tell GTK whether the
  desktop environment is showing the application menu and menubar models outside
  the application as part of the desktop shell. For instance, on OS X, both
  menus will be displayed remotely. On Windows neither will be. GNOME Shell,
  starting with version 3.4, will display the application menu, but not the
  menubar.

  If the desktop environment does not display the menubar, then the application
  window will automatically show a @class{gtk:menu-bar} widget for it. This
  behaviour can be overridden with the @code{show-menubar} property. If the
  desktop environment does not display the application menu, then it will
  automatically be included in the menubar.

  @b{Example:} An application window with a menubar
  @begin{pre}
;; Intitialize the menubar
(let ((builder (make-instance 'gtk:builder)))
  ;; Read the menus from a string
  (gtk:builder-add-from-string
      builder
      (format nil
              \"<interface> ~
                  <menu id='menubar'> ~
                    <submenu label='_Edit'> ~
                      <item label='_Copy' action='win.copy'/> ~
                      <item label='_Paste' action='win.paste'/> ~
                    </submenu> ~
                  </menu> ~
                </interface>\"))
  ;; Set the menubar
  (setf (gtk:application-menubar application)
        (gtk:builder-object builder \"menubar\"))
  ... )
  @end{pre}

  @subheading{Handling fallback yourself}
  The XML format understood by the @class{gtk:builder} class for a
  @class{g:menu-model} object consists of a toplevel @code{<menu>} element,
  which contains one or more @code{<item>} elements. Each @code{<item>} element
  contains @code{<attribute>} and @code{<link>} elements with a mandatory name
  attribute. @code{<link>} elements have the same content model as
  @code{<menu>}. Instead of @code{<link name=\"submenu\">} or
  @code{<link name=\"section\">}, you can use @code{<submenu>} or
  @code{<section>} elements.

  Attribute values can be translated using GNU gettext, like other
  @class{gtk:builder} content. @code{<attribute>} elements can be marked for
  translation with a translatable = \"yes\" attribute. It is also possible to
  specify message context and translator comments, using the context and
  comments attributes. To make use of this, the @class{gtk:builder} object must
  have been given the GNU gettext domain to use.

  The following attributes are used when constructing menu items:
  @begin{itemize}
    @item{@code{label}: a user visible string to display}
    @item{@code{action}: the prefixed name of the action to trigger}
    @item{@code{target}: the parameter to use when activating the action}
    @item{@code{icon} and @code{verb-icon}: names of icons that may be
      displayed}
    @item{@code{submenu-action}: name of an action that may be used to
      determine if a submenu can be opened}
    @item{@code{hidden-when}: a string used to determine when the item will be
      hidden. Possible values include \"action-disabled\", \"action-missing\",
      \"macos-menubar\".}
  @end{itemize}
  The following attributes are used when constructing sections:
  @begin{itemize}
    @item{@code{label}: a user visible string to use as section heading}
    @item{@code{display-hint}: a string used to determine special formatting
      for the section. Possible values include \"horizontal-buttons\".}
    @item{@code{text-direction}: a string used to determine the
      @symbol{gtk:text-direction} value to use when \"display-hint\" is set to
      \"horizontal-buttons\". Possible values include \"rtl\", \"ltr\", and
      \"none\".}
  @end{itemize}
  The following attributes are used when constructing submenus:
  @begin{itemize}
    @item{@code{label}: a user visible string to display}
    @item{@code{icon}: icon name to display}
  @end{itemize}
  @see-constructor{gtk:application-window-new}
  @see-slot{gtk:application-window-show-menubar}
  @see-class{gtk:window}
  @see-class{gtk:application}
  @see-class{g:action-group}
  @see-class{g:action-map}
  @see-class{g:menu-model}
  @see-class{gtk:actionable}
  @see-class{gtk:menu-bar}
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:application-window-show-menubar ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-menubar"
                                               'application-window) t)
 "The @code{show-menubar} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If this property is @em{true}, the application window will display a menubar
  that includes the application menu and menubar, unless these are shown by the
  desktop shell. If @em{false}, the applicaton window will not display a
  menubar, regardless of whether the desktop shell is showing the menus or not.
  @br{}
  Default value: @code{true}")

#+liber-documentation
(setf (liber:alias-for-function 'application-window-show-menubar)
      "Accessor"
      (documentation 'application-window-show-menubar 'function)
 "@version{2023-12-24}
  @syntax[]{(gtk:application-window-show-menubar object) => show}
  @syntax[]{(setf (gtk:application-window-show-menubar object) show)}
  @argument[window]{a @class{gtk:application-window} widget}
  @argument[show]{a boolean whether to show a menubar when needed}
  @begin{short}
    Accessor of the @slot[gtk:application-window]{show-menubar} slot of the
    @class{gtk:application-window} class.
  @end{short}
  The @fun{gtk:application-window-show-menubar} function returns whether the
  window will display a menubar for the application menu and menubar as needed.
  The @setf{gtk:application-window-show-menubar} function sets whether the
  window will display a menubar.
  @see-class{gtk:application-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_window_new" application-window-new)
    (g:object widget)
 #+liber-documentation
 "@version{2023-3-11}
  @argument[application]{a @class{gtk:application} instance}
  @return{The newly created @class{gtk:application-window} widget.}
  @short{Creates a new application window.}
  @see-class{gtk:application}
  @see-class{gtk:application-window}"
  (application (g:object application)))

(export 'application-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_get_id ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_application_window_get_id" application-window-id) :uint
 #+liber-documentation
 "@version{2023-3-11}
  @argument[window]{a @class{gtk:application-window} widget}
  @begin{return}
    The unique ID for @arg{window}, or 0 if @arg{window} has not yet been
    added to a @class{gtk:application} instance.
  @end{return}
  @begin{short}
    Returns the unique ID of the application window. If the application window
    has not yet been added to a @class{gtk:application} instance, returns 0.
  @end{short}
  @see-class{gtk:application}
  @see-class{gtk:application-window}"
  (window (g:object application-window)))

(export 'application-window-id)

;;; ----------------------------------------------------------------------------
;;; gtk_application_window_get_help_overlay ()
;;; gtk_application_window_set_help_overlay ()
;;; ----------------------------------------------------------------------------

(defun (setf application-window-help-overlay) (help-overlay window)
  (cffi:foreign-funcall "gtk_application_window_set_help_overlay"
                        (g:object application-window) window
                        (g:object shortcuts-window) help-overlay
                        :void)
  help-overlay)

(cffi:defcfun ("gtk_application_window_get_help_overlay"
               application-window-help-overlay) (g:object shortcuts-window)
 #+liber-documentation
 "@version{2023-12-24}
  @syntax[]{(gtk:application-window-help-overlay window) => help-overlay}
  @syntax[]{(setf (gtk:application-window-help-overlay window) help-overlay)}
  @argument[window]{a @class{gtk:application-window} widget}
  @argument[help-overlay]{a @class{gtk:shortcuts-window} widget}
  @begin{short}
    Accessor of the shortcuts window associated with the application window.
  @end{short}
  The @fun{gtk:application-window-help-overlay} function gets the shortcuts
  window. The @setf{gtk:applicaton-window-help-overlay} function associates a
  shortcuts window with the application window, and sets up an action with the
  name \"win.show-help-overlay\" to present it.
  @see-class{gtk:application-window}
  @see-class{gtk:shortcuts-window}"
  (window (g:object application-window)))

(export 'application-window-help-overlay)

;;; --- End of file gtk3.application-window.lisp -------------------------------
