;;; ----------------------------------------------------------------------------
;;; gtk3.stack-switcher.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkStackSwitcher
;;;
;;;     A controller for GtkStack
;;;
;;; Types and Values
;;;
;;;     GtkStackSwitcher
;;;
;;; Functions
;;;
;;;     gtk_stack_switcher_new
;;;     gtk_stack_switcher_set_stack                       Accessor
;;;     gtk_stack_switcher_get_stack                       Accessor
;;;
;;; Properties
;;;
;;;     icon-size
;;;     stack
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkStackSwitcher
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStackSwitcher implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkStackSwitcher
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStackSwitcher" stack-switcher
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_stack_switcher_get_type")
  ((icon-size
    stack-switcher-icon-size
    "icon-size" "gint" t t)
   (stack
    stack-switcher-stack
    "stack" "GtkStack" t t)))

#+liber-documentation
(setf (documentation 'stack-switcher 'type)
 "@version{#2023-3-27}
  @begin{short}
    The @sym{gtk:stack-switcher} widget acts as a controller for a
    @class{gtk:stack} widget.
  @end{short}
  It shows a row of buttons to switch between the various pages of the
  associated stack widget.

  @image[stackswitcher]{Figure: GtkStackSwitcher}

  All the content for the buttons comes from the child properties of the
  @class{gtk:stack} widget. The button visibility in a @sym{gtk:stack-switcher}
  widget is controlled by the visibility of the child widget in the
  @class{gtk:stack} widget.

  It is possible to associate multiple @sym{gtk:stack-switcher} widgets with
  the same @class{gtk:stack} widget.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:stack-switcher} implementation has a single CSS node named
    @code{stackswitcher} and @code{.stack-switcher} style class. When
    circumstances require it, the @sym{gtk:stack-switcher} widget adds the
    @code{.needs-attention} style class to the widgets representing the stack
    pages.
  @end{dictionary}
  @see-constructor{gtk:stack-switch-new}
  @see-slot{gtk:stack-switcher-icon-size}
  @see-slot{gtk:stack-switcher-stack}
  @see-class{gtk:stack}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- stack-switcher-icon-size -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-size" 'stack-switcher) t)
 "The @code{icon-size} property of type @code{:int} (Read / Write) @br{}
  Use this property to change the size of the image displayed when a stack
  switcher is displaying icons. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'stack-switcher-icon-size)
      "Accessor"
      (documentation 'stack-switcher-icon-size 'function)
 "@version{#2023-3-27}
  @syntax[]{(gtk:stack-switcher-icon-size object) => size}
  @syntax[]{(setf (gtk:stack-switcher-icon-size object) size)}
  @argument[object]{a @class{gtk:stack-switcher} widget}
  @argument[size]{an integer with size of the image}
  @begin{short}
    Accessor of the @slot[gtk:stack-switcher]{icon-size} slot of the
    @class{gtk:stack-switcher} class.
  @end{short}
  Use the @slot[gtk:stack-switcher]{icon-size} property to change the size of
  the image displayed when a stack switcher is displaying icons.
  @see-class{gtk:stack-switcher}")

;;; --- stack-switcher-stack ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stack" 'stack-switcher) t)
 "The @code{stack} property of type @class{stack} (Read / Write) @br{}
  The stack to control. @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'stack-switcher-stack)
      "Accessor"
      (documentation 'stack-switcher-stack 'function)
 "@version{#2023-3-27}
  @syntax[]{(gtk:stack-switcher-stack object) => stack}
  @syntax[]{(setf (gtk:stack-switcher-stack object) stack)}
  @argument[object]{a @class{gtk:stack-switcher} widget}
  @argument[stack]{a @class{gtk:stack} widget}
  @begin{short}
    Accessor of the @slot[gtk:stack-switcher]{stack} slot of the
    @class{gtk:stack-switcher} class.
  @end{short}
  The @sym{gtk:stack-switcher-stack} function retrieves the stack. The 
  @sym{(setf gtk:stack-switcher-stack)} function sets the stack to control.
  @see-class{gtk:stack-switcher}
  @see-class{gtk:stack}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_switcher_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline stack-switcher-new))

(defun stack-switcher-new ()
 #+liber-documentation
 "@version{#2023-3-27}
  @return{The new @class{gtk:stack-switcher} widget.}
  @short{Creates a new stack switcher.}
  @see-class{gtk:stack-switcher}"
  (make-instance 'stack-switcher))

(export 'stack-switcher-new)

;;; --- End of file gtk3.stack-switcher.lisp -----------------------------------
