;;; ----------------------------------------------------------------------------
;;; gdk3.threads.lisp
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
;;; Threads
;;;
;;;     Functions for using GDK in multi-threaded programs
;;;
;;; Functions
;;;
;;;     gdk_threads_init
;;;     gdk_threads_enter
;;;     gdk_threads_leave
;;;     gdk_threads_set_lock_functions                      not implemented
;;;     gdk_threads_add_idle
;;;     gdk_threads_add_idle_full                           not exported
;;;     gdk_threads_add_timeout
;;;     gdk_threads_add_timeout_full                        not exported
;;;     gdk_threads_add_timeout_seconds
;;;     gdk_threads_add_timeout_seconds_full                not exported
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove this macro.

(defmacro with-threads-lock (&body body)
 #+liber-documentation
 "@version{2024-6-29}
  @begin{short}
    A macro to execute @arg{body} between the @fun{gdk:threads-enter} and
    @fun{gdk:threads-leave} functions.
  @end{short}
  @see-function{gdk:threads-enter}
  @see-function{gdk:threads-leave}"
  `(progn
     (threads-enter)
     (unwind-protect
       (progn ,@body)
       (threads-leave))))

(export 'with-threads-lock)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_init
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_threads_init" threads-init) :void
 #+liber-documentation
 "@version{2024-6-29}
  @begin{short}
    Initializes GDK so that it can be used from multiple threads in conjunction
    with the @fun{gdk:threads-enter} and @fun{gdk:threads-leave} functions.
  @end{short}
  This call must be made before any use of the main loop from GTK. To be
  safe, call it before the @code{gtk_init()} function.
  @begin[Warning]{dictionary}
    The @fun{gdk:threads-init} function has been deprecated since version 3.6
    and should not be used in newly written code. All GDK and GTK calls should
    be made from the main thread.
  @end{dictionary}
  @see-function{gdk:threads-enter}
  @see-function{gdk:threads-leave}")

(export 'threads-init)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_enter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_threads_enter" threads-enter) :void
 #+liber-documentation
 "@version{2024-6-29}
  @begin{short}
    This function marks the beginning of a critical section in which GDK and
    GTK functions can be called safely and without causing race conditions.
    Only one thread at a time can be in such a critial section.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:threads-enter} function has been deprecated since version 3.6
    and should not be used in newly written code. All GDK and GTK calls should
    be made from the main thread.
  @end{dictionary}
  @see-function{gdk:threads-init}
  @see-function{gdk:threads-leave}")

(export 'threads-enter)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_leave
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_threads_leave" threads-leave) :void
 #+liber-documentation
 "@version{2026-6-29}
  @begin{short}
    Leaves a critical region begun with the @fun{gdk:threads-enter} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:threads-leave} function has been deprecated since version 3.6
    and should not be used in newly written code. All GDK and GTK calls should
    be made from the main thread.
  @end{dictionary}
  @see-function{gdk:threads-init}
  @see-function{gdk:threads-enter}")

(export 'threads-leave)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_set_lock_functions ()
;;;
;;; void gdk_threads_set_lock_functions (GCallback enter_fn, GCallback leave_fn)
;;;
;;; Warning
;;;
;;; gdk_threads_set_lock_functions has been deprecated since version 3.6 and
;;; should not be used in newly written code. All GDK and GTK calls should be
;;; made from the main thread
;;;
;;; Allows the application to replace the standard method that GDK uses to
;;; protect its data structures. Normally, GDK creates a single GMutex that is
;;; locked by gdk_threads_enter(), and released by gdk_threads_leave(); using
;;; this function an application provides, instead, a function enter_fn that is
;;; called by gdk_threads_enter() and a function leave_fn that is called by
;;; gdk_threads_leave().
;;;
;;; The functions must provide at least same locking functionality as the
;;; default implementation, but can also do extra application specific
;;; processing.
;;;
;;; As an example, consider an application that has its own recursive lock that
;;; when held, holds the GTK lock as well. When GTK unlocks the GTK lock when
;;; entering a recursive main loop, the application must temporarily release its
;;; lock as well.
;;;
;;; Most threaded GTK apps won't need to use this method.
;;;
;;; This method must be called before gdk_threads_init(), and cannot be called
;;; multiple times.
;;;
;;; enter_fn :
;;;     function called to guard GDK
;;;
;;; leave_fn :
;;;     function called to release the guard
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_idle
;;; gdk_threads_add_idle_full                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_threads_add_idle_full" %threads-add-idle-full) :uint
  (priority :int)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun threads-add-idle (func &key (priority glib:+priority-default-idle+))
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[func]{a @sym{g:source-func} callback function to call}
  @argument[priority]{an integer for the priority of the idle source,
    typically this will be in the range between
    @var{glib:+priority-default-idle+} and @var{glib:+priority-high-idle+}}
  @return{The unsigned integer ID, greater than 0, for the event source.}
  @begin{short}
    Adds a function to be called whenever there are no higher priority events
    pending.
  @end{short}
  If the function returns @em{false} it is automatically removed from the list
  of event sources and will not be called again. The default for @arg{priority}
  is @var{glib:+priority-default-idle+}.

  This variant of the @fun{g:idle-add} function calls the function with
  the GDK lock held. It can be thought of a MT-safe version for GTK widgets.
  @see-symbol{g:source-func}
  @see-function{g:idle-add}"
  (%threads-add-idle-full priority
                          (cffi:callback g:source-func)
                          (glib:allocate-stable-pointer func)
                          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'threads-add-idle)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout
;;; gdk_threads_add_timeout_full                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_threads_add_timeout_full" %threads-add-timeout-full) :uint
  (priority :int)
  (interval :uint)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun threads-add-timeout (interval func
                            &key (priority glib:+priority-default+))
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[interval]{an unsigned integer for the time between calls to the
    function, in milliseconds}
  @argument[func]{a @sym{g:source-func} callback function to call}
  @argument[priority]{an integer for the priority of the timeout source,
    typically this will be in the range between
    @var{glib:+priority-default-idle+} and @var{glib:+priority-high-idle+}}
  @return{The unsigned integer ID, greater than 0, for the event source.}
  @begin{short}
    Sets a function to be called at regular intervals holding the GDK lock,
    with the given priority.
  @end{short}
  The default priority is @var{glib:+priority-default+}. The function is
  called repeatedly until it returns @em{false}, at which point the timeout is
  automatically destroyed and the function will not be called again.

  Note that timeout functions may be delayed, due to the processing of other
  event sources. Thus they should not be relied on for precise timing. After
  each call to the timeout function, the time of the next timeout is
  recalculated based on the current time and the given interval It does not
  try to 'catch up' time lost in delays.

  This variant of the @fun{g:timeout-add} function can be thought of a MT-safe
  version for GTK widgets.
  @see-symbol{g:source-func}
  @see-function{g:timeout-add}"
  (%threads-add-timeout-full
              priority
              interval
              (cffi:callback g:source-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'threads-add-timeout)

;;; ----------------------------------------------------------------------------
;;; gdk_threads_add_timeout_seconds
;;; gdk_threads_add_timeout_seconds_full                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_threads_add_timeout_seconds_full"
          %threads-add-timeout-seconds-full) :uint
  (priority :int)
  (interval :uint)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun threads-add-timeout-seconds (interval func
                                    &key (priority glib:+priority-default+))
 #+liber-documentation
 "@version{#2025-08-31}
  @argument[interval]{an unsigned integer for the time between calls to the
    function, in seconds}
  @argument[func]{a @sym{g:source-func} callback function to call}
  @argument[priority]{an integer for the priority of the timeout source,
    typically this will be in the range between
    @var{glib:+priority-default-idle+} and @var{glib:+priority-high-idle+}}
  @return{The unsigned integer ID, greater than 0, for the event source.}
  @begin{short}
    A variant of the @fun{gdk:threads-add-timeout} function with second
    granularity.
  @end{short}
  The default priority is @var{glib:+priority-default+}. See the
  @fun{g:timeout-add-seconds} function for a discussion of why it is a good
  idea to use this function if you do not need finer granularity.
  @see-symbol{g:source-func}
  @see-function{g:timeout-add-seconds}"
  (%threads-add-timeout-seconds-full
              priority
              interval
              (cffi:callback g:source-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'threads-add-timeout-seconds)

;;; --- End of file gdk3.threads.lisp ------------------------------------------
