;;; ----------------------------------------------------------------------------
;;; gtk3.fixed.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
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
;;;
;;; GtkFixed
;;;
;;;     A container which allows you to position widgets at fixed coordinates
;;;
;;; Types and Values
;;;
;;;     GtkFixed
;;;
;;; Functions
;;;
;;;     gtk_fixed_new
;;;     gtk_fixed_put
;;;     gtk_fixed_move
;;;
;;; Child Properties
;;;
;;;     x
;;;     y
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkFixed
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFixed implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFixed
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFixed" fixed
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_fixed_get_type")
  nil)

#+liber-documentation
(setf (documentation 'fixed 'type)
 "@version{2023-3-17}
  @begin{short}
    The @class{gtk:fixed} widget is a container which can place child widgets at
    fixed positions and with fixed sizes, given in pixels.
  @end{short}
  The fixed widget performs no automatic layout management.

  For most applications, you should not use this container. It keeps you from
  having to learn about the other GTK containers, but it results in broken
  applications. With the @class{gtk:fixed} widget, the following things will
  result in truncated text, overlapping widgets, and other display bugs:
  @begin{itemize}
    @begin{item}
      Themes, which may change widget sizes.
    @end{item}
    @begin{item}
      Fonts other than the one you used to write the application will of course
      change the size of widgets containing text. Keep in mind that users may
      use a larger font because of difficulty reading the default, or they
      may be using Windows or the framebuffer port of GTK, where different
      fonts are available.
    @end{item}
    @begin{item}
      Translation of text into other languages changes its size. Also,
      display of non-English text will use a different font in many cases.
    @end{item}
  @end{itemize}
  In addition, the fixed widget cannot properly be mirrored in right-to-left
  languages such as Hebrew and Arabic. i.e. normally GTK will flip the
  interface to put labels to the right of the thing they label, but it cannot
  do that with the fixed widget. So your application will not be usable in
  right-to-left languages.

  Finally, fixed positioning makes it kind of annoying to add/remove GUI
  elements, since you have to reposition all the other elements. This is a
  long-term maintenance problem for your application.

  If you know none of these things are an issue for your application, and
  prefer the simplicity of the @class{gtk:fixed} widget, by all means use the
  fixed widget. But you should be aware of the tradeoffs.

  See also the @class{gtk:layout} widget, which shares the ability to perform
  fixed positioning of child widgets and additionally adds custom drawing and
  scrollability.
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[x]{entry}
        The @code{x} child property of type @code{:int} (Read / Write) @br{}
        The x position of the child widget. @br{}
        Default value: 0
      @end{entry}
      @begin[y]{entry}
        The @code{y} child property of type @code{:int} (Read / Write) @br{}
        The y position of the child widget. @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:fixed-new}
  @see-function{gtk:fixed-child-x}
  @see-function{gtk:fixed-child-y}
  @see-class{gtk:layout}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:fixed-child-x ------------------------------------------------------

(define-child-property fixed-child-x "x" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'fixed-child-x)
      "Accessor"
      (documentation 'fixed-child-x 'function)
 "@version{2023-3-17}
  @syntax{(gtk:fixed-child-x container child) => x}
  @syntax{(setf (gtk:fixed-child-x container child) x)}
  @argument[container]{a @class{gtk:fixed} widget}
  @argument[child]{a @class{gtk:widget} object}
  @argument[x]{an integer with the x position of the child}
  @begin{short}
    Accessor of the @code{x} child property  of the @class{gtk:fixed} class.
  @end{short}
  The x position of the child widget in the fixed widget.
  @see-class{gtk:fixed}
  @see-class{gtk:widget}
  @see-function{gtk:fixed-child-y}")

;;; --- gtk:fixed-child-y ------------------------------------------------------

(define-child-property fixed-child-y "y" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'fixed-child-y)
      "Accessor"
      (documentation 'fixed-child-y 'function)
 "@version{2023-3-17}
  @syntax{(gtk:fixed-child-y container child) => y}
  @syntax{(setf (gtk:fixed-child-y container child) y)}
  @argument[container]{a @class{gtk:fixed} widget}
  @argument[child]{a @class{gtk:widget} object}
  @argument[y]{an integer with the y position of the child}
  @begin{short}
    Accessor of the @code{y} child property of the @class{gtk:fixed} class.
  @end{short}
  The y position of the child widget in the fixed widget.
  @see-class{gtk:fixed}
  @see-class{gtk:widget}
  @see-function{gtk:fixed-child-x}")

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline fixed-new))

(defun fixed-new ()
 #+liber-documentation
 "@version{2023-3-17}
  @return{A new @class{gtk:fixed} widget.}
  @short{Creates a new fixed widget.}
  @see-class{gtk:fixed}"
  (make-instance 'fixed))

(export 'fixed-new)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_put ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_fixed_put" fixed-put) :void
 #+liber-documentation
 "@version{2023-3-17}
  @argument[fixed]{a @class{gtk:fixed} widget}
  @argument[widget]{a @class{gtk:widget} child widget to add}
  @argument[x]{an integer with the horizontal position to place the child
    widget at}
  @argument[y]{an integer with the vertical position to place the child widget
    at}
  @begin{short}
    Adds a child widget to a fixed widget at the given position.
  @end{short}
  @see-class{gtk:fixed}
  @see-class{gtk:widget}
  @see-function{gtk:fixed-move}"
  (fixed (g:object fixed))
  (widget (g:object widget))
  (x :int)
  (y :int))

(export 'fixed-put)

;;; ----------------------------------------------------------------------------
;;; gtk_fixed_move ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_fixed_move" fixed-move) :void
 #+liber-documentation
 "@version{2023-3-17}
  @argument[fixed]{a @class{gtk:fixed} widget}
  @argument[widget]{a @class{gtk:widget} child widget}
  @argument[x]{an integer with the horizontal position to move the child widget
    to}
  @argument[y]{an integer with the vertical position to move the child widget
    to}
  @begin{short}
    Moves a child widget of a fixed widget to the given position.
  @end{short}
  @see-class{gtk:fixed}
  @see-class{gtk:widget}
  @see-function{gtk:fixed-put}"
  (fixed (g:object fixed))
  (widget (g:object widget))
  (x :int)
  (y :int))

(export 'fixed-move)

;;; --- End of file gtk3.fixed.lisp --------------------------------------------
