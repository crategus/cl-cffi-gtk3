;;; ----------------------------------------------------------------------------
;;; gtk3.action-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GtkActionBar
;;;
;;;     A full width bar for presenting contextual actions
;;;
;;; Types and Values
;;;
;;;     GtkActionBar
;;;
;;; Functions
;;;
;;;     gtk_action_bar_new
;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end
;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget
;;;
;;; Child Properties
;;;
;;;     pack-type
;;;     position
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkActionBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkActionBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActionBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkActionBar" action-bar
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_action_bar_get_type")
  nil)

#+liber-documentation
(setf (documentation 'action-bar 'type)
 "@version{2023-12-25}
  @begin{short}
    The @class{gtk:action-bar} widget is designed to present contextual actions.
  @end{short}
  It is expected to be displayed below the content and expand horizontally to
  fill the area.

  @image[action-bar]{Figure: GtkActionBar}

  It allows placing children at the start or the end. In addition, it contains
  an internal centered box which is centered with respect to the full width of
  the box, even if the children at either side take up different amounts of
  space.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:action-bar} implementation has a single CSS node with name
    @code{actionbar}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[pack-type]{entry}
        The @code{pack-type} child property of type @symbol{gtk:pack-type}
        (Read / Write) @br{}
        Whether the child widget is packed with reference to the start or end
        of the parent.
        @br{}
        Default value: @code{:start}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child widget in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:action-bar-new}
  @see-class{gtk:stack}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:action-bar-child-pack-type -----------------------------------------

(define-child-property action-bar-child-pack-type
                       "pack-type" "GtkPackType" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'action-bar-child-pack-type)
      "Accessor"
      (documentation 'action-bar-child-pack-type 'function)
 "@version{2024-04-09}
  @syntax{(gtk:action-bar-child-pack-type container child) => packtype)}
  @syntax{(setf (gtk:action-bar-child-pack-type container child) packtype)}
  @argument[container]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[packtype]{a value of the @symbol{gtk:pack-type} enumeration for
    the child}
  @begin{short}
    Accessor of the @code{pack-type} child property of the
    @class{gtk:action-bar} class.
  @end{short}
  A value of the @symbol{gtk:pack-type} enumeration indicating whether the
  child widget is packed with reference to the start or end of the parent.
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}")

;;; --- gtk:action-bar-child-position ------------------------------------------

(define-child-property action-bar-child-position
                       "position" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'action-bar-child-position)
      "Accessor"
      (documentation 'action-bar-child-position 'function)
 "@version{2025-06-13}
  @syntax{(gtk:action-bar-child-position container child) => position)}
  @syntax{(setf (gtk:action-bar-child-position container child) position)}
  @argument[container]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[position]{an integer for the index of the child widget in the
    parent}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk:action-bar} class.
  @end{short}
  The index of the child widget in the parent.
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline action-bar-new))

(defun action-bar-new ()
 #+liber-documentation
 "@version{2023-12-29}
  @return{The new @class{gtk:action-bar} widget.}
  @short{Creates a new action bar.}
  @see-class{gtk:action-bar}"
  (make-instance 'action-bar))

(export 'action-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_bar_pack_start" action-bar-pack-start) :void
 #+liber-documentation
 "@version{2023-12-25}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to
  @arg{actionbar}}
  @begin{short}
    Adds the child widget to the action bar, packed with reference to the start
    of the action bar.
  @end{short}
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar))
  (child (g:object widget)))

(export 'action-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_bar_pack_end" action-bar-pack-end) :void
 #+liber-documentation
 "@version{2023-12-29}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to
    @arg{actionbar}}
  @begin{short}
    Adds the child widget to the action bar, packed with reference to the end
    of the action bar.
  @end{short}
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar))
  (child (g:object widget)))

(export 'action-bar-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_get_center_widget
;;; gtk_action_bar_set_center_widget
;;; ----------------------------------------------------------------------------

(defun (setf action-bar-center-widget) (widget actionbar)
  (cffi:foreign-funcall "gtk_action_bar_set_center_widget"
                        (g:object action-bar) actionbar
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_action_bar_get_center_widget" action-bar-center-widget)
    (g:object widget)
 #+liber-documentation
 "@version{2024-04-09}
  @syntax{(gtk:action-bar-center-widget actionbar) => widget}
  @syntax{(setf (gtk:action-bar-center-widget actionbar) widget)}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[widget]{a @class{gtk:widget} object to use for the center widget}
  @begin{short}
    The @fun{gtk:action-bar-center-widget} function retrieves the center widget
    of the action bar.
  @end{short}
  The @setf{gtk:action-bar-center-widget} function sets the center widget for
  the action bar.
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar)))

(export 'action-bar-center-widget)

;;; --- End of file gtk3.action-bar.lisp ---------------------------------------
