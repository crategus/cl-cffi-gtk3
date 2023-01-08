;;; ----------------------------------------------------------------------------
;;; gtk.spinner.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2021 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkSpinner
;;;
;;; Functions
;;;
;;;     gtk_spinner_new
;;;     gtk_spinner_start
;;;     gtk_spinner_stop
;;;
;;; Properties
;;;
;;;     gboolean  active    Read / Write
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSpinner
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSpinner implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSpinner
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSpinner" spinner
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_spinner_get_type")
  ((active
    spinner-active
    "active" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'spinner 'type)
 "@version{#2021-12-22}
  @begin{short}
    A @sym{gtk:spinner} widget displays an icon size spinning animation.
  @end{short}
  It is often used as an alternative to a @class{gtk:progress-bar} widget for
  displaying indefinite activity, instead of actual progress.

  To start the animation, use the @fun{gtk:spinner-start} function, to stop it
  use the @fun{gtk:spinner-stop} function.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:spinner} implementation has a single CSS node with the name
    @code{spinner}. When the animation is active, the @code{:checked}
    pseudoclass is added to this node.
  @end{dictionary}
  @see-slot{gtk:spinner-active}
  @see-class{gtk:cell-renderer-spinner}
  @see-class{gtk:progress-bar}
  @see-function{gtk:spinner-start}
  @see-function{gtk:spinner-stop}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'spinner) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the spinner is active. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'spinner-active)
      "Accessor"
      (documentation 'spinner-active 'function)
 "@version{#2021-12-22}
  @syntax[]{(gtk:spinner-active object) => active}
  @syntax[]{(setf (gtk:spinner-active object) active)}
  @argument[object]{a @class{gtk:spinner} widget}
  @argument[active]{a boolean whether the spinner is active}
  @begin{short}
    Accessor of the @slot[gtk:spinner]{active} slot of the @class{gtk:spinner}
    class.
  @end{short}
  @see-class{gtk:spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline spinner-new))

(defun spinner-new ()
 #+liber-documentation
 "@version{#2021-12-22}
  @return{A new @class{gtk:spinner} widget.}
  @short{Returns a new spinner. Not yet started.}
  @see-class{gtk:spinner}
  @see-class{gtk:spinner-start}"
  (make-instance 'spinner))

(export 'spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_start ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_start" spinner-start) :void
 #+liber-documentation
 "@version{#2021-12-22}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Starts the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-stop}"
  (spinner (g:object spinner)))

(export 'spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_stop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_stop" spinner-stop) :void
 #+liber-documentation
 "@version{#2021-12-22}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Stops the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-start}"
  (spinner (g:object spinner)))

(export 'spinner-stop)

;;; --- End of file gtk.spinner.lisp -------------------------------------------
