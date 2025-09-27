;;; ----------------------------------------------------------------------------
;;; gdk3.general.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GDK library,
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
;;; General
;;;
;;;     Library initialization and miscellaneous functions
;;;
;;; Types and Values
;;;
;;;     GdkGrabStatus                                       -> gdk.device.lisp
;;;
;;;     GDK_WINDOWING_X11
;;;     GDK_WINDOWING_WIN32
;;;     GDK_WINDOWING_QUARTZ
;;;     GDK_WINDOWING_WAYLAND
;;;
;;; Functions
;;;
;;;     gdk_init                                            not implemented
;;;     gdk_init_check                                      not implemented
;;;     gdk_parse_args                                      not implemented
;;;     gdk_get_display_arg_name
;;;     gdk_notify_startup_complete
;;;     gdk_notify_startup_complete_with_id
;;;     gdk_set_allowed_backends
;;;     gdk_get_program_class
;;;     gdk_set_program_class
;;;
;;;     gdk_get_display                                     not implemented
;;;     gdk_flush                                           not implemented
;;;     gdk_pointer_grab                                    not implemented
;;;     gdk_pointer_ungrab                                  not implemented
;;;     gdk_pointer_is_grabbed                              not implemented
;;;     gdk_set_double_click_time                           not implemented
;;;     gdk_keyboard_grab                                   not implemented
;;;     gdk_keyboard_ungrab                                 not implemented
;;;     gdk_beep                                            not implemented
;;;     gdk_error_trap_push                                 not implemented
;;;     gdk_error_trap_pop                                  not implemented
;;;     gdk_error_trap_pop_ignored                          not implemented
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_init ()
;;;
;;; void gdk_init (gint *argc, gchar ***argv);
;;;
;;; Initializes the GDK library and connects to the windowing system. If
;;; initialization fails, a warning message is output and the application
;;; terminates with a call to exit(1).
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; GTK initializes GDK in gtk_init() and so this function is not usually
;;; needed by GTK applications.
;;;
;;; argc :
;;;     the number of command line arguments
;;;
;;; argv :
;;;     the array of command line arguments
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_init_check ()
;;;
;;; gboolean gdk_init_check (gint *argc, gchar ***argv);
;;;
;;; Initializes the GDK library and connects to the windowing system, returning
;;; TRUE on success.
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; GTK initializes GDK in gtk_init() and so this function is not usually
;;; needed by GTK applications.
;;;
;;; argc :
;;;     the number of command line arguments
;;;
;;; argv :
;;;     the array of command line arguments
;;;
;;; Returns :
;;;     TRUE if initialization succeeded.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_parse_args ()
;;;
;;; void gdk_parse_args (gint *argc, gchar ***argv);
;;;
;;; Parse command line arguments, and store for future use by calls to
;;; gdk_display_open().
;;;
;;; Any arguments used by GDK are removed from the array and argc and argv are
;;; updated accordingly.
;;;
;;; You shouldn't call this function explicitely if you are using gtk_init(),
;;; gtk_init_check(), gdk_init(), or gdk_init_check().
;;;
;;; argc :
;;;     the number of command line arguments.
;;;
;;; argv :
;;;     the array of command line arguments
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_get_display_arg_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_get_display_arg_name" get-display-arg-name)
    (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{2024-06-27}
  @return{The string with the display name, if specified explicitely, otherwise
    @code{nil}.}
  @begin{short}
    Gets the display name specified in the command line arguments passed to
    the @code{gdk_init} or @code{gdk_parse_args} functions, if any.
  @end{short}")

(export 'get-display-arg-name)

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_notify_startup_complete" notify-startup-complete) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading.
  @end{short}
  If the application open windows, this function is normally called after
  opening the initial set of windows of the application.

  GTK will call this function automatically after opening the first
  @class{gtk:window} widget unless the
  @fun{gtk:window-set-auto-startup-notification} function is called to disable
  that feature.
  @see-class{gtk:window}
  @see-function{gdk:notify-startup-complete-with-id}
  @see-function{gtk:window-set-auto-startup-notification}")

(export 'notify-startup-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_notify_startup_complete_with_id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_notify_startup_complete_with_id"
               notify-startup-complete-with-id) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[startup]{a string with the startup notification identifier}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading,
    using a given startup notification identifier.
  @end{short}

  GTK will call this function automatically for a @class{gtk:window} widget
  with custom startup notification identifier unless the
  @fun{gtk:window-set-auto-startup-notification} function is called to disable
  that feature.
  @see-class{gtk:window}
  @see-function{gdk:notify-startup-complete}
  @see-function{gtk:window-set-auto-startup-notification}"
  (startup :string))

(export 'notify-startup-complete-with-id)

;;; ----------------------------------------------------------------------------
;;; gdk_set_allowed_backends
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_set_allowed_backends" set-allowed-backends) :void
 #+liber-documentation
 "@version{#2024-06-27}
  @argument[backends]{a string with a comma-separated list of backends}
  @begin{short}
    Sets a list of backends that GDK should try to use.
  @end{short}
  This can be be useful if your application does not work with certain GDK
  backends. By default, GDK tries all included backends.

  For example,
  @begin{pre}
(gdk:set-allowed-backends \"wayland,quartz,*\)
  @end{pre}
  instructs GDK to try the Wayland backend first, followed by the Quartz
  backend, and then all others.

  If the @code{GDK_BACKEND} environment variable is set, it determines what
  backends are tried in what order, while still respecting the set of allowed
  backends that are specified by this function.

  The possible backend names are x11, win32, quartz, broadway, wayland. You
  can also include a * in the list to try all remaining backends.

  This call must happen prior to the @fun{gdk:display-open}, @code{gtk_init()},
  @code{gtk_init_with_args()} or @code{gtk_init_check()} functions in order to
  take effect.
  @see-function{gdk:display-open}"
  (backends :string))

(export 'set-allowed-backends)

;;; ----------------------------------------------------------------------------
;;; gdk_get_program_class
;;; gdk_set_program_class
;;; ----------------------------------------------------------------------------

(defun (setf program-class) (program-class)
  (cffi:foreign-funcall "gdk_set_program_class"
                        :string program-class
                        :void)
  program-class)

(cffi:defcfun ("gdk_get_program_class" program-class)
    (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{2024-06-27}
  @syntax{(gdk:program-class) => program-class}
  @syntax{(setf (gdk:program-class) program-class)}
  @argument[program-class]{a string with the program class}
  @begin{short}
    The @fun{gdk:program-class} function gets the program class.
  @end{short}
  The @setf{gdk:program-class} function sets the program class.

  Unless the program class has explicitly been set with the
  @setf{gdk:program-class} function or with the @code{--class} command line
  option, the default value is the program name determined with the
  @fun{g:prgname} function and with the first character converted to uppercase.

  The X11 backend uses the program class to set the class name part of the
  @code{WM_CLASS} property on toplevel windows. See the Inter-Client
  Communication Conventions Manual (ICCCM).
  @see-function{g:prgname}")

(export 'program-class)

;;; --- End of file gdk3.general.lisp ------------------------------------------
