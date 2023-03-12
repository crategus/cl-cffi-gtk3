;;; ----------------------------------------------------------------------------
;;; gtk3.offscreen-window.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;; struct GtkOffscreenWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkOffscreenWindow" offscreen-window
  (:superclass window
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_offscreen_window_get_type")
  nil)

#+liber-documentation
(setf (documentation 'offscreen-window 'type)
 "@version{#2023-3-12}
  @begin{short}
    The @sym{gtk:offscreen-window} widget is strictly intended to be used for
    obtaining snapshots of widgets that are not part of a normal widget
    hierarchy.
  @end{short}
  Since the offscreen window is a toplevel widget you cannot obtain snapshots
  of a full window with it since you cannot pack a toplevel widget in another
  toplevel.

  The idea is to take a widget and manually set the state of it, add it to a
  offscreen window and then retrieve the snapshot as a @symbol{cairo:surface-t}
  instance or @class{gdk-pixbuf:pixbuf} object.

  The @sym{gtk:offscreen-window} widget derives from the @class{gtk:window}
  class only as an implementation detail. Applications should not use any API
  specific to the @class{gtk:window} class to operate on this object. It should
  be treated as a @class{gtk:bin} widget that has no parent widget.

  When contained offscreen widgets are redrawn, the @sym{gtk:offscreen-window}
  widget will emit a \"damage-event\" signal.
  @see-constructor{gtk:offscreen-window-new}
  @see-class{gtk:bin}
  @see-class{gtk:window}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{cairo:surface-t}")

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline offscreen-window-new))

(defun offscreen-window-new ()
 #+liber-documentation
 "@version{#2021-10-26}
  @return{A @class{gtk:offscreen-window} widget.}
  @begin{short}
    Creates a toplevel container widget that is used to retrieve snapshots of
    widgets without showing them on the screen.
  @end{short}
  @see-class{gtk:offscreen-window}"
  (make-instance 'offscreen-window))

(export 'offscreen-window-new)

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_get_surface () -> offscreen-window-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_offscreen_window_get_surface" offscreen-window-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2021-12-10}
  @argument[offscreen]{the @class{gtk:offscreen-window} contained widget}
  @return{A @symbol{cairo:surface-t} instance to the @arg{offscreen} surface,
    or a @code{null-pointer}.}
  @begin{short}
    Retrieves a snapshot of the contained widget in the form of a
    @symbol{cairo:surface-t} instance.
  @end{short}
  If you need to keep this around over window resizes then you should add a
  reference to it.
  @see-class{gtk:offscreen-window}
  @see-symbol{cairo:surface-t}
  @see-function{gtk:offscreen-window-pixbuf}"
  (offscreen (g:object offscreen-window)))

(export 'offscreen-window-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_offscreen_window_get_pixbuf () -> offscreen-window-pixbuf
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_offscreen_window_get_pixbuf" offscreen-window-pixbuf)
    (g:object gdk-pixbuf:pixbuf)
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[offscreen]{the @class{gtk:offscreen-window} contained widget}
  @return{A @class{gdk-pixbuf:pixbuf} object, or @code{nil}.}
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
