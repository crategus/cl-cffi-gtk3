;;; ----------------------------------------------------------------------------
;;; gtk3.window-group.lisp
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
;;; GtkWindowGroup
;;;
;;;     Limit the effect of grabs
;;;
;;; Types and Values
;;;
;;;     GtkWindowGroup
;;;
;;; Functions
;;;
;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkWindowGroup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindowGroup
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkWindowGroup" window-group
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_window_group_get_type")
  nil)

#+liber-documentation
(setf (documentation 'window-group 'type)
 "@version{2024-04-09}
  @begin{short}
    The @class{gtk:window-group} object restricts the effect of grabs to windows
    in the same group, thereby making window groups almost behave like separate
    applications.
  @end{short}

  A window can be a member in at most one window group at a time. Windows that
  have not been explicitly assigned to a window group are implicitly treated
  like windows of the default window group.

  Window groups are referenced by each window in the window group. If the
  windows in the window group are subsequently destroyed, then they will be
  removed from the window group and drop their references on the window group.
  When all window have been removed, the window group will be freed.
  @see-constructor{gtk:window-group-new}
  @see-class{gtk:window}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_new
;;; ----------------------------------------------------------------------------

(declaim (inline window-group-new))

(defun window-group-new ()
 #+liber-documentation
 "@version{2024-03-17}
  @return{The new @class{gtk:window-group} object.}
  @begin{short}
    Creates a new window group.
  @end{short}
  Grabs added with the @fun{gtk:grab-add} function only affect windows within
  the same window group.
  @see-class{gtk:window-group}
  @see-function{gtk:grab-add}"
  (make-instance 'window-group))

(export 'window-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_add_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun (window-group-add-window "gtk_window_group_add_window") :void
 #+liber-documentation
 "@version{2024-03-17}
  @argument[group]{a @class{gtk:window-group} object}
  @argument[window]{a @class{gtk:window} widget to add}
  @begin{short}
    Adds a window to a window group.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gtk:window}"
  (group (g:object window-group))
  (window (g:object window)))

(export 'window-group-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_remove_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_group_remove_window" window-group-remove-window)
    :void
 #+liber-documentation
 "@version{2024-03-17}
  @argument[group]{a @class{gtk:window-group} object}
  @argument[window]{a @class{gtk:window} widget to remove}
  @begin{short}
    Removes a window from a window group.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gtk:window}"
  (group (g:object window-group))
  (window (g:object window)))

(export 'window-group-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_list_windows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_group_list_windows" window-group-list-windows)
    (g:list-t (g:object window))
 #+liber-documentation
 "@version{2024-03-17}
  @argument[group]{a @class{gtk:window-group} object}
  @return{The list of @class{gtk:window} widgets inside the window group.}
  @begin{short}
    Returns a list of windows that belong to the window group.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gtk:window}"
  (group (g:object window-group)))

(export 'window-group-list-windows)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_grab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_group_get_current_grab" window-group-current-grab)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[group]{a @class{gtk:window-group} object}
  @return{The current @class{gtk:widget} grab widget of the window group.}
  @begin{short}
    Gets the current grab widget of the given window group.
  @end{short}
  See the @fun{gtk:grab-add} function.
  @see-class{gtk:window-group}
  @see-class{gtk:widget}
  @see-function{gtk:grab-add}"
  (group (g:object window-group)))

(export 'window-group-current-grab)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_get_current_device_grab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_window_group_get_current_device_grab"
               window-group-current-device-grab) (g:object widget)
 #+liber-documentation
 "@version{#2023-03-30}
  @argument[group]{a @class{gtk:window-group} object}
  @argument[device]{a @class{gdk:device} object}
  @return{The @class{gtk:widget} grab widget, or @code{nil}.}
  @begin{short}
    Returns the current grab widget for @arg{device}, or @code{nil} if none.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gdk:device}
  @see-class{gtk:widget}"
  (group (g:object window-group))
  (device (g:object gdk:device)))

(export 'window-group-current-device-grab)

;;; --- End of file gtk3.window-group.lisp -------------------------------------
