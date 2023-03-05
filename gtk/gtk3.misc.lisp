;;; ----------------------------------------------------------------------------
;;; gtk3.misc.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkMisc
;;;
;;;     Base class for widgets with alignments and padding
;;;
;;; Types and Values
;;;
;;;     GtkMisc
;;;
;;; Functions
;;;
;;;     gtk_misc_set_alignment
;;;     gtk_misc_set_padding
;;;     gtk_misc_get_alignment
;;;     gtk_misc_get_padding
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMisc
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMisc" misc
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_misc_get_type")
  ((xalign
    misc-xalign
    "xalign" "gfloat" t t)
   (xpad
    misc-xpad
    "xpad" "gint" t t)
   (yalign
    misc-yalign
    "yalign" "gfloat" t t)
   (ypad
    misc-ypad "ypad" "gint" t t)))

#+liber-documentation
(setf (documentation 'misc 'type)
 "@version{2023-2-27}
  @short{Base class for widgets with alignments and padding.}
  The @sym{gtk:misc} widget is an abstract widget which is not useful itself,
  but is used to derive subclasses which have alignment and padding attributes.

  The horizontal and vertical padding attributes allows extra space to be
  added around the widget.

  The horizontal and vertical alignment attributes enable the widget to be
  positioned within its allocated area. Note that if the widget is added to a
  container in such a way that it expands automatically to fill its allocated
  area, the alignment settings will not alter the widgets position.
  @begin[Warning]{dictionary}
    Note that the desired effect can in most cases be achieved by using the
    @slot[gtk:widget]{halign}, @slot[gtk:widget]{valign} and
    @slot[gtk:widget]{margin} properties on the child widget, so the
    @sym{gtk:misc} widget should not be used in new code. To reflect this fact,
    all @sym{gtk:misc} API has been deprecated.
  @end{dictionary}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}
  @see-function{gtk:widget-margin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- misc-xalign --------------------------------------------------------

;; not exported

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'misc) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed
  for RTL layouts. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'misc-xalign)
      "Accessor"
      (documentation 'misc-xalign 'function)
 "@version{#2021-7-21}
  @syntax[]{(gtk:misc-xalign object) => xalign}
  @syntax[]{(setf (gtk:misc-xalign object) xalign)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[xalign]{a float with the horizontal alignment}
  @begin{short}
    Accessor of the @slot[gtk:misc]{xalign} slot of the @class{gtk:misc} class.
  @end{short}

  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed
  for RTL layouts.
  @see-class{gtk:misc}")

;;; --- misc-xpad ----------------------------------------------------------

;; not exported

#+liber-documentation
(setf (documentation (liber:slot-documentation "xpad" 'misc) t)
 "The @code{xpad} property of type @code{:int} (Read / Write) @br{}
  The amount of space to add on the left and right of the widget, in pixels.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'misc-xpad)
      "Accessor"
      (documentation 'misc-xpad 'function)
 "@version{#2021-7-21}
  @syntax[]{(gtk:misc-xpad object) => xpad}
  @syntax[]{(setf (gtk:misc-xpad object) xpad)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[xpad]{an integer with the amount of space to add}
  @begin{short}
    Accessor of the @slot[gtk:misc]{xpad} slot of the @class{gtk:misc} class.
  @end{short}

  The amount of space to add on the left and right of the widget, in
  pixels.
  @see-class{gtk:misc}")

;;; --- misc-yalign --------------------------------------------------------

;; not exported

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'misc) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The vertical alignment, from 0.0 (top) to 1.0 (bottom). @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'misc-yalign)
      "Accessor"
      (documentation 'misc-yalign 'function)
 "@version{#2021-7-21}
  @syntax[]{(gtk:misc-yalign object) => yalign}
  @syntax[]{(setf (gtk:misc-yalign object) yalign)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[yalign]{a float with the vertical alignment}
  @begin{short}
    Accessor of the @slot[gtk:misc]{yalign} slot of the @class{gtk:misc} class.
  @end{short}

  The vertical alignment, from 0.0 (top) to 1.0 (bottom).
  @see-class{gtk:misc}")

;;; --- misc-ypad ----------------------------------------------------------

;; not exported

#+liber-documentation
(setf (documentation (liber:slot-documentation "ypad" 'misc) t)
 "The @code{ypad} property of type @code{:int} (Read / Write) @br{}
  The amount of space to add on the top and bottom of the widget, in
  pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'misc-ypad)
      "Accessor"
      (documentation 'misc-ypad 'function)
 "@version{#2021-7-21}
  @syntax[]{(gtk:misc-ypad object) => ypad}
  @syntax[]{(setf (gtk:misc-ypad object) ypad)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[ypad]{an integer with the amount of space to add}
  @begin{short}
    Accessor of the @slot[gtk:misc]{ypad} slot of the @class{gtk:misc} class.
  @end{short}

  The amount of space to add on the top and bottom of the widget, in
  pixels.
  @see-class{gtk:misc}")

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline misc-set-alignment))

(defun misc-set-alignment (misc xalign yalign)
 #+liber-documentation
 "@version{2023-2-27}
  @argument[misc]{a @class{gtk:misc} widget}
  @argument[xalign]{a float with the horizontal alignment, from 0.0 (left)
    to 1.0 (right)}
  @argument[yalign]{a float with the vertical alignment, from 0.0 (top)
    to 1.0 (bottom)}
  @short{Sets the alignment of the widget.}
  @begin[Warning]{dictionary}
    The @sym{gtk:misc-set-alignment} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (setf (misc-xalign misc) xalign
        (misc-yalign misc) yalign))

(export 'misc-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline misc-set-padding))

(defun misc-set-padding (misc xpad ypad)
 #+liber-documentation
 "@version{2023-2-27}
  @argument[misc]{a @class{gtk:misc} widget}
  @argument[xpad]{an integer with the amount of space to add on the left
    and right of the widget, in pixels}
  @argument[ypad]{an integer with the amount of space to add on the top
    and bottom of the widget, in pixels}
  @short{Sets the amount of space to add around the widget.}
  @begin[Warning]{dictionary}
    The @sym{gtk:misc-set-padding} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (setf (misc-xpad misc) xpad
        (misc-ypad misc) ypad))

(export 'misc-set-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_alignment ()
;;; ----------------------------------------------------------------------------

(declaim (inline misc-get-alignment))

(defun misc-get-alignment (misc)
 #+liber-documentation
 "@version{2023-2-27}
  @argument[misc]{a @class{gtk:misc} widget}
  @begin{return}
    @arg{xalign} -- a float with the x alignment @br{}
    @arg{yalign} -- a float with the y alignment
  @end{return}
  @short{Gets the x and y alignment of the widget within its allocation.}
  @begin[Warning]{dictionary}
    The @sym{gtk:misc-get-alignment} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (values (misc-xalign misc)
          (misc-yalign misc)))

(export 'misc-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline misc-get-padding))

(defun misc-get-padding (misc)
 #+liber-documentation
 "@version{2023-2-27}
  @argument[misc]{a @class{gtk:misc} widget}
  @begin{return}
    @arg{xpad} -- an integer with the padding in the x direction @br{}
    @arg{ypad} -- an integer with the padding in the y direction
  @end{return}
  @short{Gets the padding in the x and y directions of the widget.}
  @begin[Warning]{dictionary}
    The @sym{gtk:misc-get-padding} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (values (misc-xpad misc)
          (misc-ypad misc)))

(export 'misc-get-padding)

;;; --- End of file gtk3.misc.lisp ---------------------------------------------
