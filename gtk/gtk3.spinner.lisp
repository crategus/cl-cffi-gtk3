;;; ----------------------------------------------------------------------------
;;; gtk3.spinner.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation in the Lisp binding is
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
;;;     active
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

(gobject:define-gobject "GtkSpinner" spinner
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
 "@version{#2023-03-26}
  @begin{short}
    The @class{gtk:spinner} widget displays an icon size spinning animation.
  @end{short}
  It is often used as an alternative to a @class{gtk:progress-bar} widget for
  displaying indefinite activity, instead of actual progress.

  To start the animation, use the @fun{gtk:spinner-start} function, to stop it
  use the @fun{gtk:spinner-stop} function.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:spinner} implementation has a single CSS node with the name
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
 "@version{#2023-03-26}
  @syntax{(gtk:spinner-active object) => active}
  @syntax{(setf (gtk:spinner-active object) active)}
  @argument[object]{a @class{gtk:spinner} widget}
  @argument[active]{a boolean whether the spinner is active}
  @begin{short}
    Accessor of the @slot[gtk:spinner]{active} slot of the @class{gtk:spinner}
    class.
  @end{short}
  @see-class{gtk:spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_new
;;; ----------------------------------------------------------------------------

(declaim (inline spinner-new))

(defun spinner-new ()
 #+liber-documentation
 "@version{#2025-06-17}
  @return{The new @class{gtk:spinner} widget.}
  @short{Returns a new spinner. Not yet started.}
  @see-class{gtk:spinner}
  @see-class{gtk:spinner-start}"
  (make-instance 'spinner))

(export 'spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_spinner_start" spinner-start) :void
 #+liber-documentation
 "@version{#2023-03-26}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Starts the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-stop}"
  (spinner (g:object spinner)))

(export 'spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_stop
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_spinner_stop" spinner-stop) :void
 #+liber-documentation
 "@version{#2023-03-26}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Stops the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-start}"
  (spinner (g:object spinner)))

(export 'spinner-stop)

;;; --- End of file gtk3.spinner.lisp ------------------------------------------
