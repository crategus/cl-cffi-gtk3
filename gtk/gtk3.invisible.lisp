;;; ----------------------------------------------------------------------------
;;; gtk3.invisible.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkInvisible
;;;
;;;     A widget which is not displayed
;;;
;;; Synopsis
;;;
;;;     GtkInvisible
;;;
;;;     gtk_invisible_new
;;;     gtk_invisible_new_for_screen
;;;     gtk_invisible_set_screen                           Accessor
;;;     gtk_invisible_get_screen                           Accessor
;;;
;;; Properties
;;;
;;;     screen
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkInvisible
;;;
;;; Implemented Interfaces
;;;
;;;     GtkInvisible implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkInvisible
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkInvisible" invisible
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_invisible_get_type")
  ((screen
    invisible-screen
    "screen" "GdkScreen" t t)))

#+liber-documentation
(setf (documentation 'invisible 'type)
 "@version{#2023-3-20}
  @begin{short}
    The @sym{gtk:invisible} widget is used internally in GTK, and is probably
    not very useful for application developers.
  @end{short}
  It is used for reliable pointer grabs and selection handling in the code for
  drag and drop.
  @see-constructor{gtk:invisible-new}
  @see-constructor{gtk:invisible-new-for-screen}
  @see-slot{gtk:invisible-screen}
  @see-class{gdk:screen}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "screen" 'invisible) t)
 "The @code{screen} property of type @class{gdk:screen} (Read / Write) @br{}
  The screen where this window will be displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'invisible-screen)
      "Accessor"
      (documentation 'invisible-screen 'function)
 "@version{#2023-3-20}
  @syntax[]{(gtk:invisible-screen object) => screen}
  @syntax[]{(setf (gtk:invisible-screen object) screen)}
  @argument[object]{a @class{gtk:invisible} widget}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Accessor of the @slot[gtk:invisible]{screen} slot of the
    @class{gtk:invisible} class.
  @end{short}
  The @sym{gtk:invisible-screen} function returns the screen associated with
  the invisible widget. The @sym{(setf gtk:invisible-screen)} function sets the
  screen where the invisible widget will be displayed.
  @see-class{gtk:invisible}
  @see-class{gdk:screen}")

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline invisible-new))

(defun invisible-new ()
 #+liber-documentation
 "@version{#2023-3-20}
  @return{A new @class{gtk:invisible} widget.}
  @begin{short}
    Creates a new invisible widget.
  @end{short}
  The @slot[gtk:invisible]{screen} slot is initialized to the default screen.
  See the @fun{gtk:invisible-new-for-screen} function to create a new invisible
  widget for a specified screen.
  @see-class{gtk:invisible}
  @see-class{gdk:screen}
  @see-function{gtk:invisible-new-for-screen}
  @see-function{gtk:invisible-screen}"
  (make-instance 'invisible))

(export 'invisible-new)

;;; ----------------------------------------------------------------------------
;;; gtk_invisible_new_for_screen ()
;;; ----------------------------------------------------------------------------

(declaim (inline invisible-new-for-screen))

(defun invisible-new-for-screen (screen)
 #+liber-documentation
 "@version{#2023-3-20}
  @argument[screen]{a @class{gdk:screen} object which identifies on which the
    @class{gtk:invisible} widget will be created}
  @return{A newly created @class{gtk:invisible} widget.}
  @begin{short}
    Creates a new invisible widget for a specified screen.
  @end{short}
  See also the @fun{gtk:invisible-new} function to create a new invisible widget
  for the default screen.
  @see-class{gtk:invisible}
  @see-class{gdk:screen}
  @see-function{gtk:invisible-new}"
  (make-instance 'invisible
                 :screen screen))

(export 'invisible-new-for-screen)

;;; --- End of file gtk3.invisible.lisp ----------------------------------------
