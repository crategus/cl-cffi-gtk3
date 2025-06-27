;;; ----------------------------------------------------------------------------
;;; gtk3.offscreen-window.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkOffscreenWindow
;;;
;;;     A toplevel to manage offscreen rendering of child widgets
;;;
;;; Types and Values
;;;
;;;     GtkOffscreenWindow
;;;
;;; Functions
;;;
;;;     gtk_offscreen_window_new
;;;     gtk_offscreen_window_get_surface
;;;     gtk_offscreen_window_get_pixbuf
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkWindow
;;;                         ╰── GtkOffscreenWindow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkOffscreenWindow implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOffscreenWindow
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkOffscreenWindow" offscreen-window
  (:superclass window
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_offscreen_window_get_type")
  nil)

#+liber-documentation
(setf (documentation 'offscreen-window 'type)
 "@version{2024-03-17}
  @begin{short}
    The @class{gtk:offscreen-window} widget is strictly intended to be used for
    obtaining snapshots of widgets that are not part of a normal widget
    hierarchy.
  @end{short}
  Since the offscreen window is a toplevel widget you cannot obtain snapshots
  of a full window with it since you cannot pack a toplevel widget in another
  toplevel.

  The idea is to take a widget and manually set the state of it, add it to a
  offscreen window and then retrieve the snapshot as a @sym{cairo:surface-t}
  instance or @class{gdk-pixbuf:pixbuf} object.

  The @class{gtk:offscreen-window} widget derives from the @class{gtk:window}
  class only as an implementation detail. Applications should not use any API
  specific to the @class{gtk:window} class to operate on this object. It should
  be treated as a @class{gtk:bin} widget that has no parent widget.

  When contained offscreen widgets are redrawn, the @class{gtk:offscreen-window}
  widget will emit a @sig[gtk:widget]{damage-event} signal.
  @see-constructor{gtk:offscreen-window-new}
  @see-class{gtk:bin}
  @see-class{gtk:window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{cairo:surface-t}")

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_new
;;; ----------------------------------------------------------------------------

(declaim (inline offscreen-window-new))

(defun offscreen-window-new ()
 #+liber-documentation
 "@version{2024-03-17}
  @return{The @class{gtk:offscreen-window} widget.}
  @begin{short}
    Creates a toplevel container widget that is used to retrieve snapshots of
    widgets without showing them on the screen.
  @end{short}
  @see-class{gtk:offscreen-window}"
  (make-instance 'offscreen-window))

(export 'offscreen-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_get_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_offscreen_window_get_surface" %offscreen-window-surface)
    (:pointer (:struct cairo:surface-t))
  (offscreen (g:object offscreen-window)))

(defun offscreen-window-surface (offscreen)
 #+liber-documentation
 "@version{2024-03-17}
  @argument[offscreen]{a @class{gtk:offscreen-window} widget}
  @begin{return}
    The @sym{cairo:surface-t} instance to the @arg{offscreen} surface,
    or @code{nil}.
  @end{return}
  @begin{short}
    Retrieves a snapshot of the contained widget in the form of a
    @sym{cairo:surface-t} instance.
  @end{short}
  If you need to keep this around over window resizes then you should add a
  reference to it.
  @see-class{gtk:offscreen-window}
  @see-symbol{cairo:surface-t}
  @see-function{gtk:offscreen-window-pixbuf}"
  (let ((surface (%offscreen-window-surface offscreen)))
    (unless (cffi:null-pointer-p surface)
      surface)))

(export 'offscreen-window-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_get_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_offscreen_window_get_pixbuf" offscreen-window-pixbuf)
    (g:object gdk-pixbuf:pixbuf)
 #+liber-documentation
 "@version{2024-03-17}
  @argument[offscreen]{a @class{gtk:offscreen-window} widget}
  @return{The @class{gdk-pixbuf:pixbuf} object, or @code{nil}.}
  @begin{short}
    Retrieves a snapshot of the contained widget in the form of a
    @class{gdk-pixbuf:pixbuf} object.
  @end{short}
  @see-class{gtk:offscreen-window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:offscreen-window-surface}"
  (offscreen (g:object offscreen-window)))

(export 'offscreen-window-pixbuf)

;;; --- End of file gtk3.offscreen-window.lisp ---------------------------------
