;;; ----------------------------------------------------------------------------
;;; gtk3.init.lisp
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

(in-package :gtk)

(defun finalize-subclasses (class)
  (c2mop:ensure-finalized class)
  (iter (for subclass in (c2mop:class-direct-subclasses class))
        (finalize-subclasses subclass)))

(defun finalize-gtk-classes ()
  (finalize-subclasses (find-class 'g:object)))

(finalize-gtk-classes)

;;; ----------------------------------------------------------------------------

#+thread-support
(progn
  (defvar *main-thread* nil)
  (defvar *main-thread-level* 0)
  (defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

  (glib-init:at-finalize ()
    (when (and *main-thread* (bt:thread-alive-p *main-thread*))
      (bt:destroy-thread *main-thread*)
      (setf *main-thread* nil)))

  (defun ensure-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
        (setf *main-thread* nil))
      (unless *main-thread*
        (setf *main-thread*
              (bt:make-thread (lambda ()
                                ;; On Windows it is necessary to use the
                                ;; version gtk-main which puts the C function
                                ;; %main between gdk:thread-enter und
                                ;; gdk:thread-leave
                                (unless (find :win32 *features*)
                                  ;; Calling on win32 will deadlock
                                  (gdk::threads-init)
                                  (gdk::threads-enter))
                                (unwind-protect
                                  (progn
;                                   (%gtk-init)
                                    (%main))
                                  (unless (find :win32 *features*)
                                    ;; Calling on win32 will deadlock
                                    (gdk::threads-leave))
                                  ))
                              :name "cl-cffi-gtk main thread")
              *main-thread-level* 0))
      (incf *main-thread-level*))
    (values *main-thread* *main-thread-level*))

  (defun join-gtk-main ()
   #+liber-documentation
   "@version{#2023-3-5}
    @begin{short}
      Wait until the GTK program terminates.
    @end{short}
    @see-function{gtk:within-main-loop}
    @see-function{gtk:leave-gtk-main}"
    (when *main-thread*
      (bt:join-thread *main-thread*)))

  (defun leave-gtk-main ()
   #+liber-documentation
   "@version{2023-12-26}
    @begin{short}
      Makes the innermost invocation of the main loop return when it regains
      control.
    @end{short}
    In the Lisp binding to GTK the @fun{gtk:main-quit} function is not called,
    but the @fun{gtk:leave-gtk-main} function. This function does some
    additional bookkeeping, which is necessary to stop a Lisp program safely.
    See the @fun{gtk:within-main-loop} documentation for an example.
    @see-function{gtk:within-main-loop}
    @see-function{gtk:main-quit}"
    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (<= *main-thread-level* 0)
        (main-quit)
        (setf *main-thread-level* 0)))
    (values *main-thread* *main-thread-level*)))

#-thread-support
(progn
  (defun ensure-gtk-main ()
    (gtk-main)
    (values))

  (defun leave-gtk-main ()
    (main-quit))

  (defun join-gtk-main ()))

(export 'ensure-gtk-main)
(export 'leave-gtk-main)
(export 'join-gtk-main)

;;; ----------------------------------------------------------------------------

(cffi:defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (glib:get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))

(defun call-from-gtk-main-loop (func
                                &key (priority glib:+g-priority-default-idle+))
  (glib::%idle-add-full priority
                        (cffi:callback call-from-main-loop-callback)
                        (glib:allocate-stable-pointer func)
                        (cffi:callback glib:stable-pointer-destroy-notify))
  (ensure-gtk-main))

(defmacro within-main-loop (&body body)
 #+liber-documentation
 "@version{2023-12-26}
  @begin{short}
    The @fun{gtk:within-main-loop} macro is a wrapper around a GTK program.
  @end{short}
  The functionality of the macro corresponds to the @code{gtk_init()} and
  @code{gtk_main()} C functions which initialize and start a GTK program. Both
  functions have corresponding Lisp functions. The @code{gtk_main()} function is
  exported as the @fun{gtk:main} Lisp function. The corresponding Lisp function
  to the @code{gtk_init()} function is called internally when loading the
  library, but is not exported.

  To stop the execution of the main loop the @fun{gtk:leave-gtk-main} function
  is called.
  @begin{examples}
    An example with a simple window from the
    @url[http://www.crategus.com/books/cl-gtk/gtk-tutorial.html#example-simple-window]{GTK Lisp tutorial}
    which shows the usage of the @fun{gtk:within-main-loop} macro:
    @begin{pre}
(defun example-simple-window ()
  (gtk:within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk:window-new :toplevel)))
      ;; Signal handler for the window to handle the \"destroy\" signal.
      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window))))
    @end{pre}
  @end{examples}
  @see-function{gtk:main}
  @see-function{gtk:leave-gtk-main}"
  `(call-from-gtk-main-loop (lambda () ,@body)))

(export 'within-main-loop)

#+thread-support
(defmacro with-main-loop (&body body)
  `(progn
     (ensure-gtk-main)
     (within-main-loop ,@body)))

#-thread-support
(defmacro with-main-loop (&body body)
  `(progn
     ,@body
     (ensure-gtk-main)))

;;; ----------------------------------------------------------------------------

(glib-init:at-init () (%gtk-init))

;;; --- End of file gtk3.init.lisp ---------------------------------------------
