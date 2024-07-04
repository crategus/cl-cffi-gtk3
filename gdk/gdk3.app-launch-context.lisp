;;; ----------------------------------------------------------------------------
;;; gdk3.app-launch-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; Application launching
;;;
;;;     Startup notification for applications
;;;
;;; Types and Values
;;;
;;;     GdkAppLaunchContext
;;;
;;; Functions
;;;
;;;     gdk_app_launch_context_new                         deprecated
;;;     gdk_app_launch_context_set_display                 deprecated
;;;     gdk_app_launch_context_set_screen
;;;     gdk_app_launch_context_set_desktop
;;;     gdk_app_launch_context_set_timestamp
;;;     gdk_app_launch_context_set_icon
;;;     gdk_app_launch_context_set_icon_name
;;;
;;; Properties
;;;
;;;     display
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GAppLaunchContext
;;;         ╰── GdkAppLaunchContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAppLaunchContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkAppLaunchContext" app-launch-context
  (:superclass g:app-launch-context
   :export t
   :interfaces nil
   :type-initializer "gdk_app_launch_context_get_type")
  ((display
    app-launch-context-display
    "display" "GdkDisplay" t nil)))

#+liber-documentation
(setf (documentation 'app-launch-context 'type)
 "@version{2023-4-7}
  @begin{short}
    The @class{gdk:app-launch-context} object is an implementation of the
    @class{g:app-launch-context} object that handles launching an application
    in a graphical context.
  @end{short}
  It provides startup notification and allows to launch applications on a
  specific screen or workspace.
  @begin[Example]{dictionary}
    Launching an application.
    @begin{pre}
(let* ((display (gdk:display-default))
       (context (gdk:display-app-launch-context display)))
  (unless (g:app-info-launch-default-for-uri \"http://www.gtk.org\" context)
    (warn \"Launching failed\")))
    @end{pre}
  @end{dictionary}
  @see-constructor{gdk:app-launch-context-new}
  @see-slot{gdk:app-launch-context-display}
  @see-class{g:app-launch-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:app-launch-context-display -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'app-launch-context) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct Only) @br{}
  The display on which applications will be launched when using this context.")

#+liber-documentation
(setf (liber:alias-for-function 'app-launch-context-display)
      "Accessor"
      (documentation 'app-launch-context-display 'function)
 "@version{2022-12-15}
  @syntax{(gdk:app-launch-context-display object) => display}
  @argument[object]{a @class{gdk:app-launch-context} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @code{display} slot of the @class{gdk:app-launch-context}
    class.
  @end{short}
  The @fun{gdk:app-launch-context-display} function gets the display on which
  applications will be launched when using the launch context. See also the
  @fun{gdk:app-launch-context-set-screen} function.
  @begin[Warning]{dictionary}
    The @fun{gdk:app-launch-context-display} function has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @fun{gdk:display-app-launch-context} function instead.
  @end{dictionary}
  @see-class{gdk:app-launch-context}
  @see-class{gdk:display}
  @see-function{gdk:display-app-launch-context}
  @see-function{gdk:app-launch-context-set-screen}")

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_app_launch_context_new" app-launch-context-new)
         (g:object app-launch-context)
 #+liber-documentation
 "@version{2022-12-15}
  @return{A new @class{gdk:app-launch-context} object.}
  @begin{short}
    Creates a new launch context.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:app-launch-context-new} function has been deprecated since
    version 3.0 and should not be used in newly written code. Use the
    @fun{gdk:display-app-launch-context} function instead.
  @end{dictionary}
  @see-class{gdk:app-launch-context}
  @see-function{gdk:display-app-launch-context}")

(export 'app-launch-context-new)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_screen ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_app_launch_context_set_screen"
               app-launch-context-set-screen) :void
 #+liber-documentation
 "@version{2022-12-15}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Sets the screen on which applications will be launched when using the
    launch context.
  @end{short}
  See also the @fun{gdk:app-launch-context-display} function. If both screen
  and display are set, the screen takes priority. If neither screen or display
  are set, the default screen and display are used.
  @see-class{gdk:app-launch-context}
  @see-class{gdk:screen}
  @see-function{gdk:app-launch-context-display}"
  (context (g:object app-launch-context))
  (screen (g:object screen)))

(export 'app-launch-context-set-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_desktop ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_app_launch_context_set_desktop"
               app-launch-context-set-desktop) :void
 #+liber-documentation
 "@version{2022-12-15}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[desktop]{an integer with the number of a workspace, or -1}
  @begin{short}
    Sets the workspace on which applications will be launched when using the
    launch context when running under a window manager that supports multiple
    workspaces, as described in the Extended Window Manager Hints.
  @end{short}
  When the workspace is not specified or the @arg{desktop} argument is set to
  -1, it is up to the window manager to pick one, typically it will be the
  current workspace.
  @see-class{gdk:app-launch-context}"
  (context (g:object app-launch-context))
  (desktop :int))

(export 'app-launch-context-set-desktop)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_timestamp ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_app_launch_context_set_timestamp"
               app-launch-context-set-timestamp) :void
 #+liber-documentation
 "@version{2022-12-15}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[timestamp]{an unsigned integer with a timestamp}
  @begin{short}
    Sets the timestamp of the launch context.
  @end{short}
  The timestamp should ideally be taken from the event that triggered the
  launch. Window managers can use this information to avoid moving the focus to
  the newly launched application when the user is busy typing in another window.
  This is also known as 'focus stealing prevention'.
  @see-class{gdk:app-launch-context}"
  (context (g:object app-launch-context))
  (timestamp :uint32))

(export 'app-launch-context-set-timestamp)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_app_launch_context_set_icon" app-launch-context-set-icon)
    :void
 #+liber-documentation
 "@version{2022-12-15}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[icon]{a @class{g:icon} object, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with the launch context.
  @end{short}
  Window Managers can use this information when displaying startup
  notification. See also the @fun{gdk:app-launch-context-set-icon-name}
  function.
  @see-class{gdk:app-launch-context}
  @see-class{g:icon}
  @see-function{gdk:app-launch-context-set-icon-name}"
  (context (g:object app-launch-context))
  (icon (g:object g:icon)))

(export 'app-launch-context-set-icon)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_app_launch_context_set_icon_name"
               %app-launch-context-set-icon-name) :void
  (context (g:object app-launch-context))
  (name :string))

(defun app-launch-context-set-icon-name (context name)
 #+liber-documentation
 "@version{2022-12-15}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[name]{a string with an icon name, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with the launch context.
  @end{short}
  The icon name will be interpreted in the same way as the icon field in
  desktop files. See also the @fun{gdk:app-launch-context-set-icon} function.
  If both an icon and an icon name are set, the @arg{name} argument takes
  priority. If neither an icon or an icon name is set, the icon is taken from
  either the file that is passed to launched application or from the
  @class{g:app-info} object for the launched application itself.
  @see-class{gdk:app-launch-context}
  @see-class{g:app-info}
  @see-function{gdk:app-launch-context-set-icon}"
  (let ((name (if name name (cffi:null-pointer))))
    (%app-launch-context-set-icon-name context name)))

(export 'app-launch-context-set-icon-name)

;;; --- End of file gdk3.app-launch-context.lisp -------------------------------
