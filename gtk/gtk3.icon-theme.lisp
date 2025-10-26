;;; ----------------------------------------------------------------------------
;;; gtk3.icon-theme.lisp
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
;;; GtkIconTheme
;;;
;;;     Looking up icons by name
;;;
;;; Types and Values
;;;
;;;     GtkIconInfo
;;;     GtkIconTheme
;;;     GtkIconLookupFlags

;;;     GtkIconThemeError
;;;
;;;     GTK_ICON_THEME_ERROR
;;;
;;; Functions
;;;
;;;     gtk_icon_theme_new
;;;     gtk_icon_theme_get_default
;;;     gtk_icon_theme_get_for_screen
;;;     gtk_icon_theme_set_screen
;;;     gtk_icon_theme_set_search_path
;;;     gtk_icon_theme_get_search_path
;;;     gtk_icon_theme_append_search_path
;;;     gtk_icon_theme_prepend_search_path
;;;     gtk_icon_theme_add_resource_path
;;;     gtk_icon_theme_set_custom_theme
;;;     gtk_icon_theme_has_icon
;;;     gtk_icon_theme_lookup_icon
;;;     gtk_icon_theme_lookup_icon_for_scale
;;;     gtk_icon_theme_choose_icon
;;;     gtk_icon_theme_choose_icon_for_scale
;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_lookup_by_gicon_for_scale
;;;     gtk_icon_theme_load_icon
;;;     gtk_icon_theme_load_icon_for_scale
;;;     gtk_icon_theme_load_surface
;;;     gtk_icon_theme_list_contexts
;;;     gtk_icon_theme_list_icons
;;;     gtk_icon_theme_get_icon_sizes
;;;     gtk_icon_theme_get_example_icon_name
;;;     gtk_icon_theme_rescan_if_needed
;;;     gtk_icon_theme_add_builtin_icon                              deprecated
;;;
;;;     gtk_icon_info_copy                                 missing / deprecated
;;;     gtk_icon_info_free                                 missing / deprecated
;;;     gtk_icon_info_new_for_pixbuf
;;;     gtk_icon_info_get_base_size
;;;     gtk_icon_info_get_base_scale
;;;     gtk_icon_info_get_filename
;;;     gtk_icon_info_get_builtin_pixbuf                             deprecated
;;;     gtk_icon_info_load_icon
;;;     gtk_icon_info_load_surface
;;;     gtk_icon_info_load_icon_async                      missing
;;;     gtk_icon_info_load_icon_finish                     missing
;;;     gtk_icon_info_load_symbolic
;;;     gtk_icon_info_load_symbolic_async                  missing
;;;     gtk_icon-info_load_symbolic_finish                 missing
;;;     gtk_icon_info_load_symbolic_for_style              missing / deprecated
;;;     gtk_icon_info_load_symbolic_for_context
;;;     gtk_icon_info_load_symbolic_for_context_async      missing
;;;     gtk_icon_info_load_symbolic_for_context_finish     missing
;;;     gtk_icon_info_set_raw_coordinates                  missing / deprecated
;;;     gtk_icon_info_get_embedded_rect                    missing / deprecated
;;;     gtk_icon_info_get_attach_points                    missing / deprecated
;;;     gtk_icon_info_get_display_name                     missing / deprecated
;;;     gtk_icon_info_is_symbolic
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIconTheme
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkIconLookupFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkIconLookupFlags" icon-lookup-flags
  (:export t
   :type-initializer "gtk_icon_lookup_flags_get_type")
  (:no-svg           #.(ash 1 0))
  (:force-svg        #.(ash 1 1))
  (:use-builtin      #.(ash 1 2))
  (:generic-fallback #.(ash 1 3))
  (:force-size       #.(ash 1 4))
  (:force-regular    #.(ash 1 5))
  (:force-symbolic   #.(ash 1 6))
  (:dir-ltr          #.(ash 1 7))
  (:dir-rtl          #.(ash 1 8)))

#+liber-documentation
(setf (liber:alias-for-symbol 'icon-lookup-flags)
      "GFlags"
      (liber:symbol-documentation 'icon-lookup-flags)
 "@version{2025-07-05}
  @begin{declaration}
(gobject:define-gflags \"GtkIconLookupFlags\" icon-lookup-flags
  (:export t
   :type-initializer \"gtk_icon_lookup_flags_get_type\")
  (:no-svg           #.(ash 1 0))
  (:force-svg        #.(ash 1 1))
  (:use-builtin      #.(ash 1 2))
  (:generic-fallback #.(ash 1 3))
  (:force-size       #.(ash 1 4))
  (:force-regular    #.(ash 1 5))
  (:force-symbolic   #.(ash 1 6))
  (:dir-ltr          #.(ash 1 7))
  (:dir-rtl          #.(ash 1 8)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:no-svg]{Never get SVG icons, even if the @class{gdk-pixbuf:pixbuf}
        object supports them. Cannot be used together with the
        @val[gtk:icon-lookup-flags]{:force-svg} value.}
      @entry[:force-svg]{Get SVG icons, even if the @class{gdk-pixbuf:pixbuf}
        object does not support them. Cannot be used together with the
        @val[gtk:icon-lookup-flags]{:no-svg} value.}
      @entry[:use-builtin]{When passed to the @fun{gtk:icon-theme-lookup-icon}
        function includes built-in icons as well as files. For a built-in icon,
        the @fun{gtk:icon-info-filename} function returns @code{nil} and you
        need to call the @fun{gtk:icon-info-builtin-pixbuf} function.}
      @entry[:generic-fallback]{Try to shorten icon name at '-' characters
        before looking at inherited themes. This flag is only supported in
        functions that take a single icon name. For more general fallback, see
        the @fun{gtk:icon-theme-choose-icon} function.}
      @entry[:force-size]{Always get the icon scaled to the requested size.}
      @entry[:force-regular]{Try to always load regular icons, even when
        symbolic icon names are given.}
      @entry[:force-symbolic]{Try to always load symbolic icons, even when
        regular icon names are given.}
      @entry[:dir-ltr]{Try to load a variant of the icon for left-to-right
        text direction.}
      @entry[:dir-rtl]{Try to load a variant of the icon for right-to-left text
        direction.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used to specify options for the @fun{gtk:icon-theme-lookup-icon} function.
  @end{short}
  @see-class{gtk:icon-theme}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:icon-theme-lookup-icon}
  @see-function{gtk:icon-theme-choose-icon}
  @see-function{gtk:icon-info-filename}
  @see-function{gtk:icon-info-builtin-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; GtkIconInfo
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIconInfo" icon-info
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_icon_info_get_type")
  nil)

#+liber-documentation
(setf (documentation 'icon-info 'type)
 "@version{2025-06-20}
  @begin{short}
    Contains information found when looking up an icon in an icon theme.
  @end{short}
  @see-constructor{gtk:icon-info-new-for-pixbuf}
  @see-class{gtk:icon-theme}")

(export 'icon-info)

;;; ----------------------------------------------------------------------------
;;; GtkIconTheme
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIconTheme" icon-theme
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_icon_theme_get_type")
  nil)

#+liber-documentation
(setf (documentation 'icon-theme 'type)
 "@version{2025-07-05}
  @begin{short}
    The @class{gtk:icon-theme} class provides a facility for looking up icons by
    name and size.
  @end{short}
  The main reason for using a name rather than simply providing a filename is
  to allow different icons to be used depending on what icon theme is selected
  by the user. The operation of icon themes on Linux and Unix follows the Icon
  Theme Specification. There is a default icon theme, named Hicolor where
  applications should install their icons, but more additional application
  themes can be installed as operating system vendors and users choose.

  Named icons are similar to the Themeable Stock Images facility, and the
  distinction between the two may be a bit confusing. A few things to keep in
  mind:
  @begin{itemize}
    @begin{item}
      Stock images usually are used in conjunction with Stock Items, such as
      \"gtk-ok\" or \"gtk-open\". Named icons are easier to set up and therefore
      are more useful for new icons that an application wants to add, such as
      application icons or window icons.
    @end{item}
    @begin{item}
      Stock images can only be loaded at the symbolic sizes defined by the
      @sym{gtk:icon-size} enumeration, or by custom sizes defined by the
      @fun{gtk:icon-size-register} function, while named icons are more
      flexible and any pixel size can be specified.
    @end{item}
    @begin{item}
      Because stock images are closely tied to stock items, and thus to
      actions in the user interface, stock images may come in multiple
      variants for different widget states or writing directions.
    @end{item}
  @end{itemize}
  A good rule of thumb is that if there is a stock image for what you want to
  use, use it, otherwise use a named icon. It turns out that internally stock
  images are generally defined in terms of one or more named icons. An
  example of the more than one case is icons that depend on writing direction;
  \"gtk-go-forward\" uses the two themed icons \"gtk-stock-go-forward-ltr\" and
  \"gtk-stock-go-forward-rtl\".

  In many cases, named icon themes are used indirectly, via the
  @class{gtk:image} widget or stock items, rather than directly, but looking up
  icons directly is also simple. The icon theme acts as a database of all the
  icons in the current icon theme. You can create new icon themes, but its much
  more efficient to use the standard icon theme for the @class{gdk:screen}
  object so that the icon information is shared with other people looking up
  icons.
  @begin[Examples]{dictionary}
    In the case where the default screen is being used, looking up an icon can
    be as simple as:
    @begin{pre}
(let* ((theme (gtk:icon-theme-default))
       (pixbuf (gtk:icon-theme-load-icon theme
                                         \"gtk-ok\"    ; icon name
                                         48          ; size
                                         0)))        ; no flags
   ... )
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[icon-theme::chaned]{signal}
      @begin{pre}
lambda (theme)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[theme]{The @class{gtk:icon-theme} object.}
      @end{simple-table}
      Emitted when the current icon theme is switched or GTK detects that a
      change has occurred in the contents of the current icon theme.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:icon-theme-new}
  @see-class{gtk:icon-info}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_new
;;; ----------------------------------------------------------------------------

(declaim (inline icon-theme-new))

(defun icon-theme-new ()
 #+liber-documentation
 "@version{2025-06-20}
  @return{The newly created @class{gtk:icon-theme} object.}
  @begin{short}
    Creates a new icon theme.
  @end{short}
  Icon themes are used to lookup up an icon by name in a particular icon theme.
  Usually, you will want to use the @fun{gtk:icon-theme-default} or
  @fun{gtk:icon-theme-for-screen} functions rather than creating a new icon
  theme for scratch.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-default}
  @see-function{gtk:icon-theme-for-screen}"
  (make-instance 'icon-theme))

(export 'icon-theme-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_get_default" icon-theme-default)
    (g:object icon-theme)
 #+liber-documentation
 "@version{2025-06-20}
  @begin{return}
    The unique @class{gtk:icon-theme} object associated with the default screen.
  @end{return}
  @begin{short}
    Gets the icon theme for the default screen.
  @end{short}
  This icon theme is associated with the screen and can be used as long as the
  screen is open. See the @fun{gtk:icon-theme-for-screen} function.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-for-screen}")

(export 'icon-theme-default)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_for_screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_get_for_screen" icon-theme-for-screen)
    (g:object icon-theme)
 #+liber-documentation
 "@version{2025-06-20}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{return}
    The unique @class{gtk:icon-theme} object associated with the given
    @arg{screen}.
  @end{return}
  @begin{short}
    Gets the icon theme associated with the screen.
  @end{short}
  This icon theme is associated with the screen and can be used as long as the
  screen is open.

  If this function has not previously been called for the given screen, a new
  icon theme will be created and associated with the screen. Icon themes are
  fairly expensive to create, so using this function is usually a better choice
  than calling the @fun{gtk:icon-theme-new} function and setting the screen
  yourself. By using this function a single icon theme will be shared between
  users.
  @see-class{gtk:icon-theme}
  @see-class{gdk:screen}
  @see-function{gtk:icon-theme-new}"
  (screen (g:object gdk:screen)))

(export 'icon-theme-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_set_screen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_set_screen" icon-theme-set-screen) :void
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Sets the screen for an icon theme.
  @end{short}
  The screen is used to track the currently configured icon theme, which might
  be different for different screens.
  @see-class{gtk:icon-theme}
  @see-class{gdk:screen}"
  (theme (g:object icon-theme))
  (screen (g:object gdk:screen)))

(export 'icon-theme-set-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_search_path
;;; gtk_icon_theme_set_search_path
;;; ----------------------------------------------------------------------------

(defun (setf icon-theme-search-path) (path theme)
  (cffi:foreign-funcall "gtk_icon_theme_set_search_path"
                        (g:object icon-theme) theme
                        g:strv-t path
                        :int (length path)
                        :void)
  path)

(cffi:defcfun ("gtk_icon_theme_get_search_path" %icon-theme-search-path) :void
  (theme (g:object icon-theme))
  (path (:pointer g:strv-t))
  (n-elements (:pointer :int)))

(defun icon-theme-search-path (theme)
 #+liber-documentation
 "@version{2025-06-20}
  @syntax{(gtk:icon-theme-search-path theme) => path}
  @syntax{(setf (gtk:icon-theme-search-path theme) path)}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[path]{a list of strings for the directories that are searched for
    icon themes}
  @begin{short}
    The @fun{gtk:icon-theme-search-path} function gets the current list of
    strings with the search path that are searched for icon themes.
  @end{short}
  The @setf{gtk:icon-theme-search-path} function sets the search path.

  When looking for an icon theme, GTK will search for a subdirectory of one or
  more of the directories in @arg{path} with the same name as the icon theme.
  Icon themes from multiple of the path elements are combined to allow themes
  to be extended by adding icons in the home directory of the user.

  In addition if an icon found is not found either in the current icon theme or
  the default icon theme, and an image file with the right name is found
  directly in one of the elements of @arg{path}, then that image will be used
  for the icon name. This is legacy feature, and new icons should be put into
  the fallback icon theme, which is called Hicolor, rather than directly on the
  icon path.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:icon-theme-default) => #<GTK-ICON-THEME {1001A1A1A3@}>
(gtk:icon-theme-search-path *)
=> (\"/home/dieter/.local/share/icons\" \"/home/dieter/.icons\"
    \"/usr/share/ubuntu/icons\" \"/usr/local/share/icons\"
    \"/usr/share/icons\" \"/var/lib/snapd/desktop/icons\"
    \"/var/lib/snapd/desktop/icons\" \"/usr/share/ubuntu/pixmaps\"
    \"/usr/local/share/pixmaps\" \"/usr/share/pixmaps\"
    \"/var/lib/snapd/desktop/pixmaps\" \"/var/lib/snapd/desktop/pixmaps\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk:icon-theme}"
  (cffi:with-foreign-objects ((path 'g:strv-t) (n-elements :int))
    (%icon-theme-search-path theme path n-elements)
    (cffi:mem-ref path 'g:strv-t)))

(export 'icon-theme-search-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_append_search_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_append_search_path"
               icon-theme-append-search-path) :void
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[path]{a string for the directory name to append to the icon path}
  @begin{short}
    Appends a directory to the search path.
  @end{short}
  See the @fun{gtk:icon-theme-search-path} function.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-search-path}"
  (theme (g:object icon-theme))
  (path :string))

(export 'icon-theme-append-search-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_prepend_search_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_prepend_search_path"
               icon-theme-prepend-search-path) :void
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[path]{a string for the directory name to prepend to the icon path}
  @begin{short}
    Prepends a directory to the search path.
  @end{short}
  See the @fun{gtk:icon-theme-search-path} function.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-search-path}"
  (theme (g:object icon-theme))
  (path :string))

(export 'icon-theme-prepend-search-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_add_resource_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_add_resource_path" icon-theme-add-resource-path)
    :void
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[path]{a string for a resource path}
  @begin{short}
    Adds a resource path that will be looked at when looking for icons, similar
    to search paths.
  @end{short}
  This function should be used to make application specific icons available as
  part of the icon theme.

  The resources are considered as part of the Hicolor Icon Theme and must be
  located in subdirectories that are defined in the Hicolor Icon Theme, such
  as @file{@@path/16x16/actions/run.png}. Icons that are directly placed in the
  resource path instead of a subdirectory are also considered as ultimate
  fallback.
  @see-class{gtk:icon-theme}"
  (theme (g:object icon-theme))
  (path :string))

(export 'icon-theme-add-resource-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_set_custom_theme
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_set_custom_theme" icon-theme-set-custom-theme)
    :void
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the icon theme name to use instead of the
    configured icon theme, or @code{nil} to unset a previously set custom
    icon theme}
  @begin{short}
    Sets the name of the icon theme that the icon theme uses overriding system
    configuration.
  @end{short}
  This function cannot be called on the icon themes returned from the
  @fun{gtk:icon-theme-default} and @fun{gtk:icon-theme-for-screen} functions.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-default}
  @see-function{gtk:icon-theme-for-screen}"
  (theme (g:object icon-theme))
  (name :string))

(export 'icon-theme-set-custom-theme)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_has_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_has_icon" icon-theme-has-icon) :boolean
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of an icon}
  @return{@em{True} if @arg{theme} includes an icon for @arg{name}.}
  @begin{short}
    Checks whether an icon theme includes an icon for a particular name.
  @end{short}
  @see-class{gtk:icon-theme}"
  (theme (g:object icon-theme))
  (name :string))

(export 'icon-theme-has-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_lookup_icon" icon-theme-lookup-icon)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of the icon to lookup}
  @argument[size]{an integer for the desired icon size}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @begin{return}
    The @class{gtk:icon-info} instance containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up a named icon and returns a @class{gtk:icon-info} instance
    containing information such as the filename of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the
  @fun{gtk:icon-info-load-icon} function. The @fun{gtk:icon-theme-load-icon}
  function combines these two steps if all you need is the pixbuf.
  @see-class{gtk:icon-theme}
  @see-class{gtk:icon-info}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-theme-load-icon}"
  (theme (gobject:object icon-theme))
  (name :string)
  (size :int)
  (flags icon-lookup-flags))

(export 'icon-theme-lookup-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_icon_for_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_lookup_icon_for_scale"
               icon-theme-lookup-icon-for-scale)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of the icon to lookup}
  @argument[size]{an integer for the desired icon size}
  @argument[scale]{an integer for the desired scale}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @begin{return}
    The @class{gtk:icon-info} instance containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up a named icon for a particular window scale and returns a
    @sym{gtk:icon-info} instance containing information such as the filename
    of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the
  @fun{gtk:icon-info-load-icon} function. The @fun{gtk:icon-theme-load-icon}
  function combines these two steps if all you need is the pixbuf.
  @see-class{gtk:icon-theme}
  @see-symbol{gtk:icon-info}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-theme-load-icon}"
  (theme (gobject:object icon-theme))
  (name :string)
  (size :int)
  (scale :int)
  (flags icon-lookup-flags))

(export 'icon-theme-lookup-icon-for-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_choose_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_choose_icon" icon-theme-choose-icon)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[names]{list of strings for the icon names to lookup}
  @argument[size]{an integer for the desired icon size}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @begin{return}
    The @class{gtk:icon-info} instance containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up a named icon and returns a @sym{gtk:icon-info} instance containing
    information such as the filename of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the
  @fun{gtk:icon-info-load-icon} function. The @fun{gtk:icon-theme-load-icon}
  function combines these two steps if all you need is the pixbuf.

  If the @arg{names} argument contains more than one name, this function tries
  them all in the given order before falling back to inherited icon themes.
  @see-class{gtk:icon-theme}
  @see-symbol{gtk:icon-info}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-theme-load-icon}"
  (theme (gobject:object icon-theme))
  (names glib:strv-t)
  (size :int)
  (flags icon-lookup-flags))

(export 'icon-theme-choose-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_choose_icon_for_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_choose_icon_for_scale"
               icon-theme-choose-icon-for-scale)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[names]{list of strings for icon names to lookup}
  @argument[size]{an integer for the desired icon size}
  @argument[scale]{an integer for the desired scale}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @begin{return}
    The @sym{gtk:icon-info} instance containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up a named icon for a particular window scale and returns a
    @sym{gtk:icon-info} instance containing information such as the filename
    of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the
  @fun{gtk:icon-info-load-icon} function. The @fun{gtk:icon-theme-load-icon}
  function combines these two steps if all you need is the pixbuf.

  If the argument @arg{names} contains more than one name, this function tries
  them all in the given order before falling back to inherited icon themes.
  @see-class{gtk:icon-theme}
  @see-symbol{gtk:icon-info}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-theme-load-icon}"
  (theme (gobject:object icon-theme))
  (names glib:strv-t)
  (size :int)
  (scale :int)
  (flags icon-lookup-flags))

(export 'icon-theme-choose-icon-for-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_by_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_lookup_by_gicon" icon-theme-lookup-by-gicon)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[icon]{a @class{g:icon} object to look up}
  @argument[size]{an integer for the desired icon size}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @begin{return}
    The @class{gtk:icon-info} instance containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up an icon and returns a @sym{gtk:icon-info} instance containing
    information such as the filename of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the
  @fun{gtk:icon-info-load-icon} function.
  @see-class{gtk:icon-theme}
  @see-class{g:icon}
  @see-class{gtk:icon-info}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}"
  (theme (gobject:object icon-theme))
  (icon (gobject:object gio:icon))
  (size :int)
  (flags icon-lookup-flags))

(export 'icon-theme-lookup-by-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_by_gicon_for_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_lookup_by_gicon_for_scale"
               icon-theme-lookup-by-gicon-for-scale)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[icon]{a @class{g:icon} object to look up}
  @argument[size]{an integer for the desired icon size}
  @argument[scale]{an integer for the desired scale}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @begin{return}
    The @sym{gtk:icon-info} instance containing information about the icon,
    or @code{nil} if the icon was not found.
  @end{return}
  @begin{short}
    Looks up an icon and returns a @sym{gtk:icon-info} instance containing
    information such as the filename of the icon.
  @end{short}
  The icon can then be rendered into a pixbuf using the
  @fun{gtk:icon-info-load-icon} function.
  @see-class{gtk:icon-theme}
  @see-class{g:icon}
  @see-symbol{gtk:icon-info}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}"
  (theme (gobject:object icon-theme))
  (icon (gobject:object gio:icon))
  (size :int)
  (scale :int)
  (flags icon-lookup-flags))

(export 'icon-theme-lookup-by-gicon-for-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_load_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_load_icon" %icon-theme-load-icon)
    (gobject:object gdk-pixbuf:pixbuf :return)
  (theme (gobject:object icon-theme))
  (name :string)
  (size :int)
  (flags icon-lookup-flags)
  (err :pointer))

(defun icon-theme-load-icon (theme name size flags)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of the icon to lookup}
  @argument[size]{an integer for the desired icon size, the resulting icon may
    not be exactly this size}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @return{The rendered icon as a @class{gdk-pixbuf:pixbuf} object.}
  @begin{short}
    Looks up an icon in an icon theme, scales it to the given size and renders
    it into a pixbuf.
  @end{short}
  This is a convenience function. If more details about the icon are needed,
  use the @fun{gtk:icon-theme-lookup-icon} function followed by the
  @fun{gtk:icon-info-load-icon} function.

  Note that you probably want to listen for icon theme changes and update the
  icon. This is usually done by connecting to the
  @sig[gtk:widget]{style-updated} signal. If for some reason you do not want to
  update the icon when the icon theme changes, you should consider using the
  @fun{gdk-pixbuf:pixbuf-copy} function to make a private copy of the pixbuf
  returned by this function. Otherwise GTK may need to keep the old icon theme
  loaded, which would be a waste of memory.
  @see-class{gtk:icon-theme}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-theme-lookup-icon}
  @see-function{gdk-pixbuf:pixbuf-copy}"
  (glib:with-ignore-error (err)
    (%icon-theme-load-icon theme name size flags err)))

(export 'icon-theme-load-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_load_icon_for_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_load_icon_for_scale"
               %icon-theme-load-icon-for-scale)
    (gobject:object gdk-pixbuf:pixbuf :return)
  (theme (g:object icon-theme))
  (name :string)
  (size :int)
  (scale :int)
  (flags icon-lookup-flags)
  (err :pointer))

(defun icon-theme-load-icon-for-scale (theme name size scale flags)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of the icon to lookup}
  @argument[size]{an integer for the desired icon size}
  @argument[scale]{an integer for the desired scale}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} flags modifying the behavior
    of the icon lookup}
  @return{The rendered icon as a @class{gdk-pixbuf:pixbuf} object.}
  @begin{short}
    Looks up an icon in an icon theme for a particular window scale, scales it
    to the given size and renders it into a pixbuf.
  @end{short}
  The resulting icon may not be exactly this size, see the
  @fun{gtk:icon-info-load-icon} function. This is a convenience function. If
  more details about the icon are needed, use the
  @fun{gtk:icon-theme-lookup-icon} function followed by the
  @fun{gtk:icon-info-load-icon} function.

  Note that you probably want to listen for icon theme changes and update the
  icon. This is usually done by connecting to the
  @sig[gtk:widget]{style-updated} signal. If for some reason you do not want to
  update the icon when the icon theme changes, you should consider using the
  @fun{gdk-pixbuf:pixbuf-copy} function to make a private copy of the pixbuf
  returned by this function. Otherwise GTK may need to keep the old icon theme
  loaded, which would be a waste of memory.
  @see-class{gtk:icon-theme}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-theme-lookup-icon}
  @see-function{gdk-pixbuf:pixbuf-copy}"
  (glib:with-error (err)
    (%icon-theme-load-icon-for-scale theme name size scale flags err)))

(export 'icon-theme-load-icon-for-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_load_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_load_surface" %icon-theme-load-surface)
    (:pointer (:struct cairo:surface-t))
  (theme (g:object icon-theme))
  (name :string)
  (size :int)
  (scale :int)
  (window (g:object gdk:window))
  (flags icon-lookup-flags)
  (err :pointer))

(defun icon-theme-load-surface (theme name size scale window flags)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of the icon to lookup}
  @argument[size]{an integer for the desired icon size}
  @argument[scale]{an integer for the desired scale}
  @argument[window]{a @class{gdk:window} object to optimize drawing for, or
    @code{nil}}
  @argument[flags]{a @sym{gtk:icon-lookup-flags} value modifying the behavior
    of the icon lookup}
  @return{The rendered icon as a @sym{cairo:surface-t} instance.}
  @begin{short}
    Looks up an icon in an icon theme for a particular window scale, scales it
    to the given size and renders it into a Cairo surface.
  @end{short}
  This is a convenience function. If more details about the icon are needed,
  use the @fun{gtk:icon-theme-lookup-icon} function followed by the
  @fun{gtk:icon-info-load-surface} function.

  Note that you probably want to listen for icon theme changes and update the
  icon. This is usually done by connecting to the
  @sig[gtk:widget]{style-updated} signal.
  @see-class{gtk:icon-theme}
  @see-symbol{cairo:surface-t}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-theme-lookup-icon}
  @see-function{gtk:icon-info-load-surface}"
  (glib:with-error (err)
    (%icon-theme-load-surface theme name size scale window flags err)))

(export 'icon-theme-load-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_list_contexts
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_list_contexts" icon-theme-list-contexts)
    (g:list-t :string)
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @begin{return}
    The list of strings holding the names of all the contexts in the icon theme.
  @end{return}
  @begin{short}
    Gets the list of contexts available within the current hierarchy of icon
    themes.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:icon-theme-list-contexts (gtk:icon-theme-default))
=> (\"International\" \"Emotes\" \"Places\" \"stock\" \"FileSystems\"
    \"Devices\" \"Applications\" \"Actions\" \"Categories\" \"Animations\"
    \"MimeTypes\" \"Stock\" \"Status\" \"Emblems\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-list-icons}"
  (theme (g:object icon-theme)))

(export 'icon-theme-list-contexts)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_list_icons
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_list_icons" %icon-theme-list-icons)
    (g:list-t :string)
  (theme (g:object icon-theme))
  (context :string))

(defun icon-theme-list-icons (theme context)
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[context]{a string identifying a particular type of icon, or
    @code{nil} to list all icons}
  @return{The list of strings holding the names of all the icons in the theme.}
  @begin{short}
    Lists the icons in the current icon theme.
  @end{short}
  Only a subset of the icons can be listed by providing a context string. The
  set of values for the context string is system dependent, but will typically
  include such values as \"Applications\" and \"MimeTypes\".
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-list-contexts}"
  (let ((context (or context (cffi:null-pointer))))
    (%icon-theme-list-icons theme context)))

(export 'icon-theme-list-icons)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_icon_sizes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_get_icon_sizes" %icon-theme-icon-sizes) :pointer
  (theme (gobject:object icon-theme))
  (name :string))

(defun icon-theme-icon-sizes (theme name)
 #+liber-documentation
 "@version{2025-09-26}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string for the name of an icon}
  @begin{return}
    The Lisp array of integer for the sizes at which the icon is available.
  @end{return}
  @begin{short}
    Returns an array of integers describing the sizes at which the icon is
    available without scaling.
  @end{short}
  A size of -1 means that the icon is available in a scalable format.
  @see-class{gtk:icon-theme}"
  (let ((ptr (%icon-theme-icon-sizes theme name)))
    (cffi:foreign-array-to-lisp ptr '(:array :int 1 ) :adjustable t)))

(export 'icon-theme-icon-sizes)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_example_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_get_example_icon_name"
               icon-theme-example-icon-name) :string
 #+liber-documentation
 "@version{2025-10-09}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @return{The string for the name of an example icon or @code{nil}.}
  @begin{short}
    Gets the name of an icon that is representative of the current icon theme,
    for instance, to use when presenting a list of icon themes to the user.
  @end{short}
  @see-class{gtk:icon-theme}"
  (theme (g:object icon-theme)))

(export 'icon-theme-example-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_rescan_if_needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_rescan_if_needed" icon-theme-rescan-if-needed)
    :boolean
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @return{@em{True} if the icon theme has changed and needed to be reloaded.}
  @begin{short}
    Checks to see if the icon theme has changed.
  @end{short}
  If it has, any currently cached information is discarded and will be reloaded
  next time the icon theme is accessed.
  @see-class{gtk:icon-theme}"
  (theme (gobject:object icon-theme)))

(export 'icon-theme-rescan-if-needed)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_add_builtin_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_add_builtin_icon" icon-theme-add-builtin-icon)
    :void
 #+liber-documentation
 "@version{#2025-06-20}
  @argument[name]{a string for the name of the icon to register}
  @argument[size]{an integer for the size at which to register the icon}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object that contains the image
    to use for the icon}
  @begin{short}
    Registers a built-in icon for icon theme lookups.
  @end{short}
  Different images can be registered for the same icon name at different sizes.

  The idea of built-in icons is to allow an application or library that uses
  themed icons to function requiring files to be present in the file system.
  For instance, the default images for all stock icons of GTK are registered
  as built-in icons.

  In general, if you use the @fun{gtk:icon-theme-add-builtin-icon} function you
  should also install the icon in the icon theme, so that the icon is generally
  available.
  @begin[Warning]{dictionary}
    The @fun{gtk:icon-theme-add-builtin-icon} function has been deprecated
    since version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:icon-theme-add-resource-path} function to add application specific
    icons to the icon theme.
  @end{dictionary}
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-add-resource-path}"
  (name :string)
  (size :int)
  (pixbuf (gobject:object gdk-pixbuf:pixbuf)))

(export 'icon-theme-add-builtin-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_copy                                      deprecated
;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_free                                      deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_new_for_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_new_for_pixbuf" icon-info-new-for-pixbuf)
    (gobject:object icon-info :return)
 #+liber-documentation
 "@version{2025-06-20}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object to wrap in a
    @class{gtk:icon-info} instance}
  @return{The @class{gtk:icon-info} instance.}
  @begin{short}
    Creates a @class{gtk:icon-info} instance for a @class{gdk-pixbuf:pixbuf}
    object.
  @end{short}
  @see-class{gtk:icon-theme}
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gtk:icon-info}"
  (theme (gobject:object icon-theme))
  (pixbuf (gobject:object gdk-pixbuf:pixbuf)))

(export 'icon-info-new-for-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_base_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_get_base_size" icon-info-base-size) :int
 #+liber-documentation
 "@version{2025-07-15}
  @argument[info]{a @class{gtk:icon-info} instance}
  @begin{return}
    The integer for the base size, or 0, if no base size is known for the icon.
  @end{return}
  @begin{short}
    Gets the base size for the icon.
  @end{short}
  The base size is a size for the icon that was specified by the icon theme
  creator. This may be different than the actual size of the image. An example
  of this is small emblem icons that can be attached to a larger icon. These
  icons will be given the same base size as the larger icons to which they are
  attached.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:icon-theme-lookup-icon (gtk:icon-theme-default) \"battery\" 0 0)
=> #.(SB-SYS:INT-SAP #X01D3F840)
(gtk:icon-info-base-size *)
=> 24
    @end{pre}
  @end{dictionary}
  @see-class{gtk:icon-info}
  @see-function{gtk:icon-info-base-scale}"
  (info (gobject:object icon-info)))

(export 'icon-info-base-size)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_base_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_get_base_scale" icon-info-base-scale) :int
 #+liber-documentation
 "@version{2025-07-15}
  @argument[info]{a @sym{gtk:icon-info} instance}
  @return{The integer for the base scale.}
  @begin{short}
    Gets the base scale for the icon.
  @end{short}
  The base scale is a scale for the icon that was specified by the icon theme
  creator. For instance an icon drawn for a high DPI screen with window scale 2
  for a base size of 32 will be 64 pixels tall and have a base scale of 2.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:icon-theme-lookup-icon (gtk:icon-theme-default) \"battery\" 0 0)
=> #.(SB-SYS:INT-SAP #X01D3F840)
(gtk:icon-info-base-scale *)
=> 1
    @end{pre}
  @end{dictionary}
  @see-class{gtk:icon-info}
  @see-function{gtk:icon-info-base-size}"
  (info (gobject:object icon-info)))

(export 'icon-info-base-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_get_filename" icon-info-filename) :string
 #+liber-documentation
 "@version{2025-10-09}
  @argument[info]{a @class{gtk:icon-info} instance}
  @return{The string for the filename for the icon.}
  @begin{short}
    Gets the filename for the icon.
  @end{short}
  If the @val[gtk:icon-lookup-flags]{:use-builtin} flag was passed to the
  @fun{gtk:icon-theme-lookup-icon} function, there may be no filename if a
  built-in icon is returned. In this case, you should use the
  @fun{gtk:icon-info-builtin-pixbuf} function.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:icon-theme-lookup-icon (gtk:icon-theme-default) \"battery\" 0 0)
=> #.(SB-SYS:INT-SAP #X01D3F840)
(gtk:icon-info-filename *)
=> \"/usr/share/icons/Humanity/devices/24/battery.svg\"
    @end{pre}
  @end{dictionary}
  @see-symbol{gtk:icon-info}
  @see-function{gtk:icon-info-builtin-pixbuf}
  @see-function{gtk:icon-theme-lookup-icon}"
  (info (gobject:object icon-info)))

(export 'icon-info-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_builtin_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_get_builtin_pixbuf" icon-info-builtin-pixbuf)
    (gobject:object gdk-pixbuf:pixbuf)
 #+liber-documentation
 "@version{2025-06-20}
  @argument[info]{a @class{gtk:icon-info} instance}
  @begin{return}
    The built-in image as a @class{gdk-pixbuf:pixbuf} object, or @code{nil}.
  @end{return}
  @begin{short}
    Gets the built-in image for this icon, if any.
  @end{short}
  To allow GTK to use built-in icon images, you must pass the
  @val[gtk:icon-lookup-flags]{:use-builtin} flag to the
  @fun{gtk:icon-theme-lookup-icon} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:icon-info-builtin-pixbuf} function has been deprecated
    since version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:icon-theme-add-resource-path} function instead of built-in icons.
  @end{dictionary}
  @see-symbol{gtk:icon-info}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:icon-theme-lookup-icon}
  @see-function{gtk:icon-theme-add-resource-path}"
  (info (gobject:object icon-info)))

(export 'icon-info-builtin-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_load_icon" %icon-info-load-icon)
    (gobject:object gdk-pixbuf:pixbuf)
  (info (gobject:object icon-info))
  (err :pointer))

(defun icon-info-load-icon (info)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[info]{a @sym{gtk:icon-info} instance}
  @return{The @class{gdk-pixbuf:pixbuf} object for the rendered icon.}
  @begin{short}
    Renders an icon previously looked up in an icon theme using the
    @fun{gtk:icon-theme-lookup-icon} function.
  @end{short}
  The size will be based on the size passed to the
  @fun{gtk:icon-theme-lookup-icon} function.

  Note that the resulting pixbuf may not be exactly this size. An icon theme
  may have icons that differ slightly from their nominal sizes, and in addition
  GTK will avoid scaling icons that it considers sufficiently close to the
  requested size or for which the source image would have to be scaled up too
  far. This maintains sharpness. This behaviour can be changed by passing the
  @val[gtk:icon-lookup-flags]{:force-size} flag when obtaining the
  @sym{gtk:icon-info} instance. If this flag has been specified, the pixbuf
  returned by this function will be scaled to the exact size.
  @see-class{gtk:icon-info}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-theme-lookup-icon}"
  (glib:with-error (err)
    (%icon-info-load-icon info err)))

(export 'icon-info-load-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_load_surface" %icon-info-load-surface)
    (:pointer (:struct cairo:surface-t))
  (info (gobject:object icon-info))
  (window (gobject:object gdk:window))
  (err :pointer))

(defun icon-info-load-surface (info window)
 #+liber-documentation
 "@version{2025-07-05}
  @argument[info]{a @class{gtk:icon-info} instance}
  @argument[window]{a @class{gdk:window} object to optimize drawing for}
  @return{The @sym{cairo:surface-t} instance with the rendered icon.}
  @begin{short}
    Renders an icon previously looked up in an icon theme using the
    @fun{gtk:icon-theme-lookup-icon} function.
  @end{short}
  The size will be based on the size passed to the
  @fun{gtk:icon-theme-lookup-icon} function. Note that the resulting Cairo
  surface may  not be exactly this size. An icon theme may have icons that
  differ slightly from their nominal sizes, and in addition GTK will avoid
  scaling icons that it considers sufficiently close to the requested size or
  for which the source image would have to be scaled up too far. This maintains
  sharpness. This behaviour can be changed by passing the
  @val[gtk:icon-lookup-flags]{:force-size} flag when obtaining the
  @class{gtk:icon-info} instance. If this flag has been specified, the pixbuf
  returned by this function will be scaled to the exact size.
  @see-class{gtk:icon-info}
  @see-class{gdk:window}
  @see-symbol{cairo:surface-t}
  @see-symbol{gtk:icon-lookup-flags}
  @see-function{gtk:icon-theme-lookup-icon}"
  (glib:with-error (err)
    (%icon-info-load-surface info window err)))

(export 'icon-info-load-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_icon_async ()
;;;
;;; void
;;; gtk_icon_info_load_icon_async (GtkIconInfo *icon_info,
;;;                                GCancellable *cancellable,
;;;                                GAsyncReadyCallback callback,
;;;                                gpointer user_data);
;;;
;;; Asynchronously load, render and scale an icon previously looked up from the
;;; icon theme using gtk_icon_theme_lookup_icon().
;;;
;;; For more details, see gtk_icon_info_load_icon() which is the synchronous
;;; version of this call.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_icon_finish ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_info_load_icon_finish (GtkIconInfo *icon_info,
;;;                                 GAsyncResult *res,
;;;                                 GError **error);
;;;
;;; Finishes an async icon load, see gtk_icon_info_load_icon_async().
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_load_symbolic" %icon-info-load-symbolic)
    (gobject:object gdk-pixbuf:pixbuf)
  (info (gobject:object icon-info))
  (fg (glib:boxed gdk:rgba))
  (success (glib:boxed gdk:rgba))
  (warning (glib:boxed gdk:rgba))
  (error (glib:boxed gdk:rgba))
  (symbolic? (:pointer :boolean))
  (err :pointer))

(defun icon-info-load-symbolic (info fg success warning error)
 #+liber-documentation
 "@version{2025-06-20}
  @argument[info]{a @class{gtk:icon-info} instance}
  @argument[fg]{a @class{gdk:rgba} instance for the foreground color}
  @argument[success]{a @class{gdk:rgba} instance for the success color}
  @argument[warning]{a @class{gdk:rgba} instance for the warning color}
  @argument[error]{a @class{gdk:rgba} instance for the error color}
  @return{The @class{gdk-pixbuf:pixbuf} object representing the loaded icon.}
  @begin{short}
    Loads an icon, modifying it to match the system colours for the foreground,
    success, warning and error colors provided.
  @end{short}
  If the icon is not a symbolic one, the function will return the result from
  the @fun{gtk:icon-info-load-icon} function.

  This allows loading symbolic icons that will match the system theme. Unless
  you are implementing a widget, you will want to use the
  @fun{g-themed-icon-new-with-default-fallbacks} function to load the icon.

  As implementation details, the icon loaded needs to be of SVG type, contain
  the \"symbolic\" term as the last component of the icon name, and use the
  'fg', 'success', 'warning' and 'error' CSS styles in the SVG file itself.
  @see-class{gtk:icon-info}
  @see-class{gdk:rgba}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:icon-info-load-icon}
  @see-function{g:themed-icon-new-with-default-fallbacks}"
  (glib:with-error (err)
    (cffi:with-foreign-object (symbolic? :boolean)
      (%icon-info-load-symbolic info
                                fg
                                success
                                warning
                                error
                                symbolic?
                                err))))

(export 'icon-info-load-symbolic)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_async ()
;;;
;;; void
;;; gtk_icon_info_load_symbolic_async (GtkIconInfo *icon_info,
;;;                                    const GdkRGBA *fg,
;;;                                    const GdkRGBA *success_color,
;;;                                    const GdkRGBA *warning_color,
;;;                                    const GdkRGBA *error_color,
;;;                                    GCancellable *cancellable,
;;;                                    GAsyncReadyCallback callback,
;;;                                    gpointer user_data);
;;;
;;; Asynchronously load, render and scale a symbolic icon previously looked up
;;; from the icon theme using gtk_icon_theme_lookup_icon().
;;;
;;; For more details, see gtk_icon_info_load_symbolic() which is the synchronous
;;; version of this call.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; fg :
;;;     a GdkRGBA representing the foreground color of the icon
;;;
;;; success_color :
;;;     a GdkRGBA representing the warning color of the icon or NULL to use the
;;;     default color.
;;;
;;; warning_color :
;;;     a GdkRGBA representing the warning color of the icon or NULL to use the
;;;     default color.
;;;
;;; error_color :
;;;     a GdkRGBA representing the error color of the icon or NULL to use the
;;;     default color (allow-none).
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_finish ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_info_load_symbolic_finish (GtkIconInfo *icon_info,
;;;                                     GAsyncResult *res,
;;;                                     gboolean *was_symbolic,
;;;                                     GError **error);
;;;
;;; Finishes an async icon load, see gtk_icon_info_load_symbolic_async().
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it.
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_style ()
;;;
;;; GdkPixbuf * gtk_icon_info_load_symbolic_for_style (GtkIconInfo *icon_info,
;;;                                                    GtkStyle *style,
;;;                                                    GtkStateType state,
;;;                                                    gboolean *was_symbolic,
;;;                                                    GError **error);
;;;
;;; Warning
;;;
;;; gtk_icon_info_load_symbolic_for_style has been deprecated since version 3.0
;;; and should not be used in newly written code. Use
;;; gtk_icon_info_load_symbolic_for_context() instead
;;;
;;; Loads an icon, modifying it to match the system colours for the foreground,
;;; success, warning and error colors provided. If the icon is not a symbolic
;;; one, the function will return the result from gtk_icon_info_load_icon().
;;;
;;; This allows loading symbolic icons that will match the system theme.
;;;
;;; See gtk_icon_info_load_symbolic() for more details.
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; style :
;;;     a GtkStyle to take the colors from
;;;
;;; state :
;;;     the widget state to use for colors
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it
;;;
;;; error :
;;;     location to store error information on failure, or NULL
;;;
;;; Returns :
;;;     a GdkPixbuf representing the loaded icon
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_load_symbolic_for_context"
               %icon-info-load-symbolic-for-context)
    (gobject:object gdk-pixbuf:pixbuf)
  (info (gobject:object icon-info))
  (context (gobject:object style-context))
  (was-symbolic (:pointer :boolean))
  (err :pointer))

(defun icon-info-load-symbolic-for-context (info context)
 #+liber-documentation
 "@version{#2025-06-20}
  @argument[info]{a @class{gtk:icon-info} instance}
  @argument[context]{a @class{gtk:style-context} object}
  @return{The @class{gdk-pixbuf:pixbuf} object representing the loaded icon.}
  @begin{short}
    Loads an icon, modifying it to match the system colors for the foreground,
    success, warning and error colors provided.
  @end{short}
  If the icon is not a symbolic one, the function will return the result from
  the @fun{gtk:icon-info-load-icon} function.

  This function uses the regular foreground color and the symbolic colors with
  the names \"success_color\", \"warning_color\" and \"error_color\" from the
  context. This allows loading symbolic icons that will match the system theme.

  See the @fun{gtk:icon-info-load-symbolic} function for more details.
  @see-class{gtk:icon-info}
  @see-class{gtk:style-context}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:icon-info-load-icon}
  @see-function{gtk:icon-info-load-symbolic}"
  (glib:with-error (err)
    (cffi:with-foreign-object (symbolic? :boolean)
      (%icon-info-load-symbolic-for-context info context symbolic? err))))

(export 'icon-info-load-symbolic-for-context)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_context_async ()
;;;
;;; void
;;; gtk_icon_info_load_symbolic_for_context_async
;;;                                (GtkIconInfo *icon_info,
;;;                                 GtkStyleContext *context,
;;;                                 GCancellable *cancellable,
;;;                                 GAsyncReadyCallback callback,
;;;                                 gpointer user_data);
;;;
;;; Asynchronously load, render and scale a symbolic icon previously looked up
;;; from the icon theme using gtk_icon_theme_lookup_icon().
;;;
;;; For more details, see gtk_icon_info_load_symbolic_for_context() which is the
;;; synchronous version of this call.
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_load_symbolic_for_context_finish ()
;;;
;;; GdkPixbuf *
;;; gtk_icon_info_load_symbolic_for_context_finish
;;;                                (GtkIconInfo *icon_info,
;;;                                 GAsyncResult *res,
;;;                                 gboolean *was_symbolic,
;;;                                 GError **error);
;;;
;;; Finishes an async icon load, see
;;; gtk_icon_info_load_symbolic_for_context_async().
;;;
;;; icon_info :
;;;     a GtkIconInfo from gtk_icon_theme_lookup_icon()
;;;
;;; res :
;;;     a GAsyncResult
;;;
;;; was_symbolic :
;;;     a gboolean, returns whether the loaded icon was a symbolic one and
;;;     whether the fg color was applied to it.
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the rendered icon; this may be a newly created icon or a new reference
;;;     to an internal icon, so you must not modify the icon. Use
;;;     g_object_unref() to release your reference to the icon.
;;;
;;; Since 3.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_set_raw_coordinates ()
;;;
;;; void gtk_icon_info_set_raw_coordinates (GtkIconInfo *icon_info,
;;;                                         gboolean raw_coordinates);
;;;
;;; Sets whether the coordinates returned by gtk_icon_info_get_embedded_rect()
;;; and gtk_icon_info_get_attach_points() should be returned in their original
;;; form as specified in the icon theme, instead of scaled appropriately for the
;;; pixbuf returned by gtk_icon_info_load_icon().
;;;
;;; Raw coordinates are somewhat strange; they are specified to be with respect
;;; to the unscaled pixmap for PNG and XPM icons, but for SVG icons, they are in
;;; a 1000x1000 coordinate space that is scaled to the final size of the icon.
;;; You can determine if the icon is an SVG icon by using
;;; gtk_icon_info_get_filename(), and seeing if it is non-NULL and ends in
;;; '.svg'.
;;;
;;; This function is provided primarily to allow compatibility wrappers for
;;; older API's, and is not expected to be useful for applications.
;;;
;;; Warning
;;;
;;; gtk_icon_info_set_raw_coordinates has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;;
;;; Embedded rectangles and attachment points are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; raw_coordinates :
;;;     whether the coordinates of embedded rectangles and attached points
;;;     should be returned in their original (unscaled) form.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_embedded_rect ()
;;;
;;; gboolean gtk_icon_info_get_embedded_rect (GtkIconInfo *icon_info,
;;;                                           GdkRectangle *rectangle);
;;;
;;; Gets the coordinates of a rectangle within the icon that can be used for
;;; display of information such as a preview of the contents of a text file. See
;;; gtk_icon_info_set_raw_coordinates() for further information about the
;;; coordinate system.
;;;
;;; Warning
;;;
;;; gtk_icon_info_get_embedded_rect has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;;
;;; Embedded rectangles are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; rectangle :
;;;     GdkRectangle in which to store embedded rectangle coordinates;
;;;     coordinates are only stored when this function returns TRUE
;;;
;;; Returns :
;;;     TRUE if the icon has an embedded rectangle
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_attach_points ()
;;;
;;; gboolean gtk_icon_info_get_attach_points (GtkIconInfo *icon_info,
;;;                                           GdkPoint **points,
;;;                                           gint *n_points);
;;;
;;; Fetches the set of attach points for an icon. An attach point is a location
;;; in the icon that can be used as anchor points for attaching emblems or
;;; overlays to the icon.
;;;
;;; Warning
;;;
;;; gtk_icon_info_get_attach_points has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;;
;;; Attachment points are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; points :
;;;     location to store pointer to an array of points, or NULL free the array
;;;     of points with g_free()
;;;
;;; n_points :
;;;     location to store the number of points in points, or NULL
;;;
;;; Returns :
;;;     TRUE if there are any attach points for the icon.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_get_display_name ()
;;;
;;; const gchar * gtk_icon_info_get_display_name (GtkIconInfo *icon_info);
;;;
;;; Gets the display name for an icon. A display name is a string to be used in
;;; place of the icon name in a user visible context like a list of icons.
;;;
;;; Warning
;;;
;;; gtk_icon_info_get_display_name has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;;
;;; Display names are deprecated
;;;
;;; icon_info :
;;;     a GtkIconInfo
;;;
;;; Returns :
;;;     the display name for the icon or NULL, if the icon does not have a
;;;     specified display name. This value is owned icon_info and must not be
;;;     modified or free.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_icon_info_is_symbolic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_info_is_symbolic" icon-info-is-symbolic) :boolean
 #+liber-documentation
 "@version{2025-06-20}
  @argument[info]{a @class{gtk:icon-info} instance}
  @return{@em{True} if the icon is symbolic, @em{false} otherwise.}
  @begin{short}
    Checks if the icon is symbolic or not.
  @end{short}
  This currently uses only the file name and not the file contents for
  determining this. This behaviour may change in the future.
  @see-class{gtk:icon-info}
  @see-function{gtk:icon-theme-lookup-icon}"
  (info (gobject:object icon-info)))

(export 'icon-info-is-symbolic)

;;; --- End of file gtk3.icon-theme.lisp ---------------------------------------
