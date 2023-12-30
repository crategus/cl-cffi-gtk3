;;; ----------------------------------------------------------------------------
;;; gdk3.screen.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GdkScreen
;;;
;;;     Object representing a physical screen
;;;
;;; Types and Values
;;;
;;;     GdkScreen
;;;
;;; Functions
;;;
;;;     gdk_screen_get_default
;;;     gdk_screen_get_system_visual
;;;     gdk_screen_get_rgba_visual
;;;     gdk_screen_is_composited
;;;     gdk_screen_get_root_window
;;;     gdk_screen_get_display
;;;     gdk_screen_get_number                              deprecated
;;;     gdk_screen_get_width                               deprecated
;;;     gdk_screen_get_height                              deprecated
;;;     gdk_screen_get_width_mm                            deprecated
;;;     gdk_screen_get_height_mm                           deprecated
;;;     gdk_screen_list_visuals
;;;     gdk_screen_get_toplevel_windows
;;;     gdk_screen_make_display_name                       deprecated
;;;     gdk_screen_get_n_monitors                          deprecated
;;;     gdk_screen_get_primary_monitor                     deprecated
;;;     gdk_screen_get_monitor_geometry                    deprecated
;;;     gdk_screen_get_monitor_workarea                    deprecated
;;;     gdk_screen_get_monitor_at_point                    deprecated
;;;     gdk_screen_get_monitor_at_window                   deprecated
;;;     gdk_screen_get_monitor_height_mm                   deprecated
;;;     gdk_screen_get_monitor_width_mm                    deprecated
;;;     gdk_screen_get_monitor_plug_name                   deprecated
;;;     gdk_screen_get_monitor_scale_factor                deprecated
;;;     gdk_screen_get_setting
;;;     gdk_screen_get_font_options                        Accessor
;;;     gdk_screen_set_font_options                        Accessor
;;;     gdk_screen_get_resolution                          Accessor
;;;     gdk_screen_set_resolution                          Accessor
;;;     gdk_screen_get_active_window                       deprecated
;;;     gdk_screen_get_window_stack
;;;
;;; Properties
;;;
;;;     font-options
;;;     resolution
;;;
;;; Signals
;;;
;;;     composited-changed
;;;     monitors-changed
;;;     size-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkScreen
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkScreen
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkScreen" screen
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_screen_get_type")
  ((font-options
    screen-font-options
    "font-options" "cairo_font_options_t" t t)
   (resolution
    screen-resolution
    "resolution" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'screen 'type)
 "@version{#2021-12-13}
  @begin{short}
    The @class{gdk:screen} object is the GDK representation of the screen on
    which windows can be displayed and on which the pointer moves.
  @end{short}
  X11 originally identified screens with physical screens, but nowadays it is
  more common to have a single @class{gdk:screen} object which combines several
  physical monitors. See the @fun{gdk:screen-n-monitors} function.

  The @class{gdk:screen} object is used throughout GDK and GTK to specify which
  screen the toplevel windows are to be displayed on. It is also used to query
  the screen specification and default settings such as the default visual with
  the @fun{gdk:screen-system-visual} function or the dimensions of the physical
  monitors with the @fun{gdk:screen-monitor-geometry} function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"composited-changed\" signal}
      @begin{pre}
lambda (screen)    :run-last
      @end{pre}
      The signal is emitted when the composited status of the screen changes.
      @begin[code]{table}
        @entry[screen]{The @class{gdk:screen} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"monitors-changed\" signal}
      @begin{pre}
lambda (screen)    :run-last
      @end{pre}
      The signal is emitted when the number, size or position of the monitors
      attached to the screen change. Only for X11 and OS X for now. A future
      implementation for Win32 may be a possibility.
      @begin[code]{table}
        @entry[screen]{The @class{gdk:screen} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"size-changed\" signal}
      @begin{pre}
lambda (screen)    :run-last
      @end{pre}
      The signal is emitted when the pixel width or height of a the screen
      changes.
      @begin[code]{table}
        @entry[screen]{The @class{gdk:screen} object on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk:screen-font-options}
  @see-slot{gdk:screen-resolution}
  @see-function{gdk:screen-n-monitors}
  @see-function{gdk:screen-system-visual}
  @see-function{gdk:screen-monitor-geometry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- screen-font-options ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-options" 'screen) t)
 "The @code{font-options} property of type @symbol{cairo:font-options-t}
  (Read / Write) @br{}
  The default font options for the screen.")

#+liber-documentation
(setf (liber:alias-for-function 'screen-font-options)
      "Accessor"
      (documentation 'screen-font-options 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:screen-font-options object) => options}
  @syntax[]{(setf (gdk:screen-font-options object) options)}
  @argument[object]{a @class{gdk:screen} object}
  @argument[options]{a @symbol{cairo:font-options-t} instance, or
    @code{null-pointer} to unset any previously set default font options}
  @begin{short}
    Accessor of the @slot[gdk:screen]{font-options} slot of the
    @class{gdk:screen} class.
  @end{short}
  The @fun{gdk:screen-font-options} function returns the current font options
  for the screen, or @code{null-pointer} if no default font options have been
  set. The @setf{gdk:screen-font-options} function sets the default font
  options.

  These font options will be set on any Pango context newly created with the
  @fun{gdk:pango-context-for-screen} function. Changing the default set
  of font options does not affect Pango contexts that have already been created.
  @see-class{gdk:screen}
  @see-symbol{cairo:font-options-t}
  @see-function{gdk:pango-context-for-screen}")

;;; --- screen-resolution --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resolution" 'screen) t)
 "The @code{resolution} property of type @code{:double} (Read / Write) @br{}
  The resolution for fonts on the screen. @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'screen-resolution)
      "Accessor"
      (documentation 'screen-resolution 'function)
 "@version{#2021-12-13}
  @syntax[]{(gdk:screen-resolution object) => dpi}
  @syntax[]{(setf (gdk:screen-resolution object) dpi)}
  @argument[object]{a @class{gdk:screen} object}
  @argument[dpi]{a double float with the resolution in \"dots per inch\"}
  @begin{short}
    Accessor of the @slot[gdk:screen]{resolution} slot of the @class{gdk:screen}
    class.
  @end{short}
  The @fun{gdk:screen-resolution} function gets the resolution for font
  handling on the screen, or -1 if no resolution has been set. The
  @setf{gdk:screen-resolution} function sets the resolution for font handling
  on the screen.

  This is a scale factor between points specified in a
  @class{pango:font-description} structure and Cairo units. The default value
  is 96, meaning that a 10 point font will be 13 units high
  (10 * 96 / 72 = 13.3).
  @see-class{gdk:screen}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_default () -> screen-default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_default" screen-default) (g:object screen)
 #+liber-documentation
 "@version{#2021-12-13}
  @begin{return}
    A @class{gdk:screen} object, or @code{nil} if there is no default display.
  @end{return}
  @begin{short}
    Gets the default screen for the default display.
  @end{short}
  See the @fun{gdk:display-default} function.
  @see-class{gdk:screen}
  @see-function{gdk:display-default}")

(export 'screen-default)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_system_visual () -> screen-system-visual
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_system_visual" screen-system-visual)
    (g:object visual)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{The system @class{gdk:visual} object.}
  @begin{short}
    Get the default visual of the system for the screen.
  @end{short}
  This is the visual for the root window of the display.
  @see-class{gdk:screen}
  @see-class{gdk:visual}"
  (screen (g:object screen)))

(export 'screen-system-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_rgba_visual () -> screen-rgba-visual
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_rgba_visual" screen-rgba-visual)
    (g:object visual)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{return}
    A @class{gdk:visual} object to use for windows with an alpha channel or
    @code{nil} if the capability is not available.
  @end{return}
  @begin{short}
    Gets a visual to use for creating windows with an alpha channel.
  @end{short}
  The windowing system on which GTK is running may not support this capability,
  in which case @code{nil} will be returned. Even if a non-@code{nil} value is
  returned, it is possible that the window's alpha channel will not be honored
  when displaying the window on the screen: in particular, for X11 an
  appropriate windowing manager and compositing manager must be running to
  provide appropriate display.

  This functionality is not implemented in the Windows backend.

  For setting an overall opacity for a toplevel window, see the
  @fun{gdk:window-set-opacity} function.
  @see-class{gdk:screen}
  @see-class{gdk:visual}
  @see-function{gdk:window-set-opacity}"
  (screen (g:object screen)))

(export 'screen-rgba-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_is_composited ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_is_composited" screen-is-composited) :boolean
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{return}
    A boolean whether windows with RGBA visuals can reasonably be expected to
    have their alpha channels drawn correctly on @arg{screen}.
  @end{return}
  @begin{short}
    Returns whether windows with an RGBA visual can reasonably be expected to
    have their alpha channel drawn correctly on the screen.
  @end{short}

  On X11 this function returns whether a compositing manager is compositing
  the screen.
  @see-class{gdk:screen}"
  (screen (g:object screen)))

(export 'screen-is-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_root_window () -> screen-root-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_root_window" screen-root-window)
    (g:object window)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{The root @class{gdk:window} object.}
  @short{Gets the root window of the screen.}
  @see-class{gdk:screen}
  @see-class{gdk:window}"
  (screen (g:object screen)))

(export 'screen-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_display () -> screen-display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_display" screen-display)
    (g:object display)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{The @class{gdk:display} object to which @arg{screen} belongs.}
  @short{Gets the display to which the screen belongs.}
  @see-class{gdk:screen}
  @see-class{gdk:display}"
  (screen (g:object screen)))

(export 'screen-display)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_number () -> screen-number
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_number" screen-number) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the index of @arg{screen}.}
  @begin{short}
    Gets the index of the screen among the screens in the display to which
    it belongs.
  @end{short}
  See the @fun{gdk:screen-display} function.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-number} function has been deprecated since version
    3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:screen-display}"
  (screen (g:object screen)))

(export 'screen-number)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width () -> screen-width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_width" %screen-width) :int
  (screen (g:object screen)))

(defun screen-width (&optional (screen (screen-default)))
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the width of @arg{screen} in pixels.}
  @short{Gets the width of the screen in pixels.}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-width} function has been deprecated since version
    3.22 and should not be used in newly written code. Use per monitor
    information instead.
  @end{dictionary}
  @see-class{gdk:screen}"
  (%screen-width screen))

(export 'screen-width)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height () -> screen-height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_height" %screen-height) :int
  (screen (g:object screen)))

(defun screen-height (&optional (screen (screen-default)))
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the height of @arg{screen} in pixels.}
  @short{Gets the height of the screen in pixels.}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-height} function has been deprecated since version
    3.22 and should not be used in newly written code. Use per monitor
    information instead.
  @end{dictionary}
  @see-class{gdk:screen}"
  (%screen-height screen))

(export 'screen-height)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_width_mm () -> screen-width-mm
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_width_mm" %screen-width-mm) :int
  (screen (g:object screen)))

(defun screen-width-mm (&optional (screen (screen-default)))
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the width of @arg{screen} in millimeters.}
  @begin{short}
    Gets the width of the screen in millimeters.
  @end{short}
  Note that on some X servers this value will not be correct.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-width-mm} function has been deprecated since version
    3.22 and should not be used in newly written code. Use per monitor
    information instead.
  @end{dictionary}
  @see-class{gdk:screen}"
  (%screen-width-mm screen))

(export 'screen-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_height_mm ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_height_mm" %screen-height-mm) :int
  (screen (g:object screen)))

(defun screen-height-mm (&optional (screen (screen-default)))
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the height of @arg{screen} in millimeters.}
  @begin{short}
    Returns the height of the screen in millimeters.
  @end{short}
  Note that on some X servers this value will not be correct.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-height-mm} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use per monitor
    information instead.
  @end{dictionary}
  @see-class{gdk:screen}"
  (%screen-height-mm screen))

(export 'screen-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_list_visuals ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_list_visuals" screen-list-visuals)
    (g:list-t (g:object visual) :free-from-foreign t)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a relevant @class{gdk:screen} object}
  @return{A list of @class{gdk:visual} objects for @arg{screen}.}
  @begin{short}
    Lists the available visuals for the specified screen.
  @end{short}
  A visual describes a hardware image data format. For example, a visual might
  support 24-bit color, or 8-bit color, and might expect pixels to be in a
  certain format.
  @see-class{gdk:screen}
  @see-class{gdk:visual}"
  (screen (g:object screen)))

(export 'screen-list-visuals)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_toplevel_windows () -> screen-toplevel-windows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_toplevel_windows" screen-toplevel-windows)
    (g:list-t (g:object window :free-from-foreign nil) :free-from-foreign t)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object where the toplevels are located}
  @return{List of toplevel @class{gdk:window} objects for @arg{screen}.}
  @begin{short}
    Obtains a list of all toplevel windows known to GDK on the screen.
  @end{short}
  A toplevel window is a child of the root window. See the
  @fun{gdk:default-root-window} function.
  @see-class{gdk:screen}
  @see-class{gdk:window}
  @see-function{gdk:default-root-window}"
  (screen (g:object screen)))

(export 'screen-toplevel-windows)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_make_display_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_make_display_name" screen-make-display-name)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{A string with the name of the default display.}
  @begin{short}
    Determines the name to pass to the @fun{gdk:display-open} function to get
    a @class{gdk:display} object with this screen as the default screen.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-make-display-name} function has been deprecated since
    version 3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gdk:display}
  @see-function{gdk:display-open}"
  (screen (g:object screen)))

(export 'screen-make-display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_n_monitors () -> screen-n-monitors
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_n_monitors" screen-n-monitors) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the number of monitors which @arg{screen} consists
    of.}
  @short{Returns the number of monitors which the screen consists of.}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-n-monitors} function has been deprecated since version
    3.22 and should not be used in newly written code. Use the
    @fun{gdk:display-n-monitors} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:display-n-monitors}"
  (screen (g:object screen)))

(export 'screen-n-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_primary_monitor () -> screen-primary-monitor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_primary_monitor" screen-primary-monitor) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{An integer with the index for the primary monitor, or 0 if none is
    configured.}
  @begin{short}
    Gets the primary monitor for the screen.
  @end{short}
  The primary monitor is considered the monitor where the \"main desktop\"
  lives. While normal application windows typically allow the window manager to
  place the windows, specialized desktop applications such as panels should
  place themselves on the primary monitor.

  If no primary monitor is configured by the user, the return value will be 0,
  defaulting to the first monitor.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-primary-monitor} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:display-primary-monitor} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:display-primary-monitor}"
  (screen (g:object screen)))

(export 'screen-primary-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_geometry () -> screen-monitor-geometry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_geometry" %screen-monitor-geometry) :void
  (screen (g:object screen))
  (monitor-num :int)
  (dest (g:boxed rectangle)))

(defun screen-monitor-geometry (screen num)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[num]{an integer with the monitor number}
  @return{A @class{gdk:rectangle} instance filled with the monitor geometry.}
  @begin{short}
    Retrieves the rectangle representing the size and position of the
    individual monitor within the entire screen area.
  @end{short}
  Monitor numbers start at 0. To obtain the number of monitors of the screen,
  use the @fun{gdk:screen-n-monitors} function.

  Note that the size of the entire screen area can be retrieved via the
  @fun{gdk:screen-width} and @fun{gdk:screen-height} functions.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-geometry} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:monitor-geometry} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gdk:rectangle}
  @see-function{gdk:screen-n-monitors}
  @see-function{gdk:screen-width}
  @see-function{gdk:screen-height}
  @see-function{gdk:monitor-geometry}"
  (let ((dest (rectangle-new)))
    (%screen-monitor-geometry screen num dest)
    dest))

(export 'screen-monitor-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_workarea () -> screen-monitor-workarea
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_workarea" %screen-monitor-workarea) :void
  (screen (g:object screen))
  (num :int)
  (dest (g:boxed rectangle)))

(defun screen-monitor-workarea (screen num)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[num]{an integer with the monitor number}
  @return{A @class{gdk:rectangle} instance filled with the monitor workarea.}
  @begin{short}
    Retrieves the rectangle representing the size and position of the
    \"work area\" on a monitor within the entire screen area.
  @end{short}

  The work area should be considered when positioning menus and similar
  popups, to avoid placing them below panels, docks or other desktop
  components.

  Monitor numbers start at 0. To obtain the number of monitors of the screen,
  use the @fun{gdk:screen-n-monitors} function.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-workarea} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:monitor-workarea} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gdk:rectangle}
  @see-function{gdk:screen-n-monitors}
  @see-function{gdk:monitor-workarea}"
  (let ((dest (rectangle-new)))
    (%screen-monitor-workarea screen num dest)
    dest))

(export 'screen-monitor-workarea)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_point () -> screen-monitor-at-point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_at_point" screen-monitor-at-point) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[x]{an integer with the x coordinate in the virtual @arg{screen}}
  @argument[y]{an integer with the y coordinate in the virtual @arg{screen}}
  @begin{return}
    The monitor number in which the point (@arg{x}, @arg{y}) lies, or a monitor
    close to the point if not in any monitor.
  @end{return}
  @begin{short}
    Returns the monitor number in which the point (@arg{x}, @arg{y}) is located.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-at-point} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:display-monitor-at-point} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:display-monitor-at-point}"
  (screen (g:object screen))
  (x :int)
  (y :int))

(export 'screen-monitor-at-point)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_at_window () -> screen-monitor-at-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_at_window" screen-monitor-at-window) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    An integer with the monitor number in which most of @arg{window} is
    located, or if @arg{window} does not intersect any monitors, a monitor,
    close to @arg{window}.
  @end{return}
  @begin{short}
    Returns the number of the monitor in which the largest area of the bounding
    rectangle of the window resides.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-at-window} function has been deprecated
    since version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:display-monitor-at-window} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gdk:window}
  @see-function{gdk:display-monitor-at-window}"
  (screen (g:object screen))
  (window (g:object window)))

(export 'screen-monitor-at-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_height_mm () -> screen-monitor-height-mm
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_height_mm" screen-monitor-height-mm) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[num]{an integer with the number of the monitor, between 0 and and
    the result of @code{(gdk:screen-n-monitors @arg{screen})}}
  @return{An integer with the height of the monitor, or -1 if not available.}
  @begin{short}
    Gets the height in millimeters of the specified monitor.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-height-mm} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:monitor-height-mm} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:monitor-height-mm}"
  (screen (g:object screen))
  (num :int))

(export 'screen-monitor-height-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_width_mm () -> screen-monitor-width-mm
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_width_mm" screen-monitor-width-mm) :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[monitor-num]{an integer with the number of the monitor, between
    0 and and the result of @code{(gdk:screen-n-monitors @arg{screen})}}
  @return{An integer with the width of the monitor, or -1 if not available.}
  @begin{short}
    Gets the width in millimeters of the specified monitor, if available.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-width-mm} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:monitor-width-mm} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:monitor-width-mm}"
  (screen (g:object screen))
  (monitor-num :int))

(export 'screen-monitor-width-mm)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_plug_name () -> screen-monitor-plug-name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_plug_name" screen-monitor-plug-name)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[monitor-num]{an integer with the number of the monitor, between
    0 and the result of @code{(gdk:screen-n-monitors @arg{screen})}}
  @begin{return}
    A string containing the name of the monitor, or @code{nil} if the name
    cannot be determined.
  @end{return}
  @begin{short}
    Returns the output name of the specified monitor. Usually something like
    VGA, DVI, or TV, not the actual product name of the display device.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-plug-name} function has been deprecated
    since version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:monitor-model} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:monitor-model}"
  (screen (g:object screen))
  (monitor-num :int))

(export 'screen-monitor-plug-name)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_monitor_scale_factor () -> screen-monitor-scale-factor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_monitor_scale_factor" screen-monitor-scale-factor)
    :int
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object to get scale factor for}
  @argument[num]{an integer with the number of the monitor, between
    0 and @code{(gdk:screen-n-monitors @arg{screen})}}
  @return{An integer with the scale factor.}
  @begin{short}
    Returns the internal scale factor that maps from monitor coordiantes to the
    actual device pixels.
  @end{short}
  On traditional systems this is 1, but on very high density outputs this can
  be a higher value (often 2).

  This can be used if you want to create pixel based data for a particula
  monitor, but most of the time you are drawing to a window where it is better
  to use the @fun{gdk:window-scale-factor} function instead.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-monitor-scale-factor} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:monitor-scale-factor} function instead.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-function{gdk:monitor-scale-factor}"
  (screen (g:object screen))
  (num :int))

(export 'screen-monitor-scale-factor)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_setting () -> screen-setting
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_setting" %screen-setting) :boolean
  (screen (g:object screen))
  (name :string)
  (value (:pointer (:struct g:value))))

(defun screen-setting (screen name gtype)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object where the setting is located}
  @argument[name]{a string with the name of the setting}
  @argument[gtype]{a string with the @class{g:type-t} type of the setting}
  @begin{return}
    The value of the setting, or @code{nil} if the setting does not exist.
  @end{return}
  @begin{short}
    Retrieves a desktop wide setting such as double-click time for the screen.
  @end{short}

  See the @class{gtk:settings} class for the available settings.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((screen (gdk:display-default-screen (gdk:display-default))))
  (gdk:screen-setting screen \"gtk-double-click-time\" \"gint\"))
=> 400
    @end{pre}
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gtk:settings}
  @see-class{g-type-t}"
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value gtype)
    (when (%screen-setting screen name value)
      (prog1
        (gobject:parse-g-value value)
        (g:value-unset value)))))

(export 'screen-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_active_window () -> screen-active-window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_active_window" screen-active-window)
    (g:object window)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @return{The currently active @class{gdk:window} object, or @code{nil}.}
  @begin{short}
    Returns the currently active window of the screen.
  @end{short}

  On X11, this is done by inspecting the @code{_NET_ACTIVE_WINDOW} property on
  the root window, as described in the Extended Window Manager Hints. If there
  is no currently currently active window, or the window manager does not
  support the @code{_NET_ACTIVE_WINDOW} hint, this function returns @code{nil}.

  On other platforms, this function may return @code{nil}, depending on whether
  it is implementable on that platform.
  @begin[Warning]{dictionary}
    The @fun{gdk:screen-active-window} function has been deprecated since
    version 3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:screen}
  @see-class{gdk:window}"
  (screen (g:object screen)))

(export 'screen-active-window)

;;; ----------------------------------------------------------------------------
;;; gdk_screen_get_window_stack () -> screen-window-stack
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_screen_get_window_stack" screen-window-stack)
    (g:list-t (g:object window :free-from-foreign t) :free-from-foreign t)
 #+liber-documentation
 "@version{#2021-12-13}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{return}
    A list of @class{gdk:window} objects for the current window stack, or
    @code{nil}.
  @end{return}
  @begin{short}
    Returns a list of windows representing the current window stack.
  @end{short}

  On X11, this is done by inspecting the @code{_NET_CLIENT_LIST_STACKING}
  property on the root window, as described in the Extended Window Manager
  Hints. If the window manager does not support the
  @code{_NET_CLIENT_LIST_STACKING} hint, this function returns @code{nil}.

  On other platforms, this function may return @code{nil}, depending on whether
  it is implementable on that platform.
  @see-class{gdk:screen}
  @see-class{gdk:window}"
  (screen (g:object screen)))

(export 'screen-window-stack)

;;; --- End of file gdk3.screen.lisp -------------------------------------------
