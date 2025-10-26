;;; ----------------------------------------------------------------------------
;;; gtk3.misc.lisp
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
;;; GtkMisc
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMisc" misc
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
 "@version{2025-07-17}
  @short{Base class for widgets with alignments and padding.}
  The @class{gtk:misc} widget is an abstract widget that is not useful itself,
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
    @class{gtk:misc} widget should not be used in new code. To reflect this fact,
    all @class{gtk:misc} API has been deprecated.
  @end{dictionary}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}
  @see-function{gtk:widget-margin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:misc-xalign --------------------------------------------------------

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
 "@version{#2025-07-06}
  @syntax{(gtk:misc-xalign object) => xalign}
  @syntax{(setf (gtk:misc-xalign object) xalign)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[xalign]{a number coerced to single float for the horizontal
    alignment}
  @begin{short}
    Accessor of the @slot[gtk:misc]{xalign} slot of the @class{gtk:misc} class.
  @end{short}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed
  for RTL layouts.
  @see-class{gtk:misc}")

;;; --- gtk:misc-xpad ----------------------------------------------------------

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
 "@version{#2025-07-06}
  @syntax{(gtk:misc-xpad object) => xpad}
  @syntax{(setf (gtk:misc-xpad object) xpad)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[xpad]{an integer for the amount of space to add}
  @begin{short}
    Accessor of the @slot[gtk:misc]{xpad} slot of the @class{gtk:misc} class.
  @end{short}
  The amount of space to add on the left and right of the widget, in
  pixels.
  @see-class{gtk:misc}")

;;; --- gtk:misc-yalign --------------------------------------------------------

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
 "@version{#2025-07-06}
  @syntax{(gtk:misc-yalign object) => yalign}
  @syntax{(setf (gtk:misc-yalign object) yalign)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[yalign]{a number coerced to a single float for the vertical
    alignment}
  @begin{short}
    Accessor of the @slot[gtk:misc]{yalign} slot of the @class{gtk:misc} class.
  @end{short}
  The vertical alignment, from 0.0 (top) to 1.0 (bottom).
  @see-class{gtk:misc}")

;;; --- gtk:misc-ypad ----------------------------------------------------------

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
 "@version{#2025-07-06}
  @syntax{(gtk:misc-ypad object) => ypad}
  @syntax{(setf (gtk:misc-ypad object) ypad)}
  @argument[object]{a @class{gtk:misc} widget}
  @argument[ypad]{an integer for the amount of space to add}
  @begin{short}
    Accessor of the @slot[gtk:misc]{ypad} slot of the @class{gtk:misc} class.
  @end{short}
  The amount of space to add on the top and bottom of the widget, in
  pixels.
  @see-class{gtk:misc}")

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_alignment
;;; ----------------------------------------------------------------------------

(declaim (inline misc-set-alignment))

(defun misc-set-alignment (misc xalign yalign)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[misc]{a @class{gtk:misc} widget}
  @argument[xalign]{a number coerced to a single float for the horizontal
    alignment, from 0.0 (left) to 1.0 (right)}
  @argument[yalign]{a number coerced to a single float for the vertical
    alignment, from 0.0 (top) to 1.0 (bottom)}
  @short{Sets the alignment of the widget.}
  @begin[Warning]{dictionary}
    The @fun{gtk:misc-set-alignment} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (setf (misc-xalign misc) xalign
        (misc-yalign misc) yalign))

(export 'misc-set-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_set_padding
;;; ----------------------------------------------------------------------------

(declaim (inline misc-set-padding))

(defun misc-set-padding (misc xpad ypad)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[misc]{a @class{gtk:misc} widget}
  @argument[xpad]{an integer for the amount of space to add on the left and
    right of the widget, in pixels}
  @argument[ypad]{an integer for the amount of space to add on the top and
    bottom of the widget, in pixels}
  @short{Sets the amount of space to add around the widget.}
  @begin[Warning]{dictionary}
    The @fun{gtk:misc-set-padding} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (setf (misc-xpad misc) xpad
        (misc-ypad misc) ypad))

(export 'misc-set-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_alignment
;;; ----------------------------------------------------------------------------

(declaim (inline misc-get-alignment))

(defun misc-get-alignment (misc)
 #+liber-documentation
 "@version{2025-10-09}
  @argument[misc]{a @class{gtk:misc} widget}
  @begin{return}
    @arg{xalign} -- a float for the x alignment @br{}
    @arg{yalign} -- a float for the y alignment
  @end{return}
  @short{Gets the x and y alignment of the widget within its allocation.}
  @begin[Warning]{dictionary}
    The @fun{gtk:misc-get-alignment} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (values (misc-xalign misc)
          (misc-yalign misc)))

(export 'misc-get-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_misc_get_padding
;;; ----------------------------------------------------------------------------

(declaim (inline misc-get-padding))

(defun misc-get-padding (misc)
 #+liber-documentation
 "@version{2025-09-26}
  @argument[misc]{a @class{gtk:misc} widget}
  @begin{return}
    @arg{xpad} -- an integer for the padding in the x direction @br{}
    @arg{ypad} -- an integer for the padding in the y direction
  @end{return}
  @short{Gets the padding in the x and y directions of the widget.}
  @begin[Warning]{dictionary}
    The @fun{gtk:misc-get-padding} function has been deprecated since version
    3.14 and should not be used in newly written code. Use @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:misc}
  @see-class{gtk:widget}"
  (values (misc-xpad misc)
          (misc-ypad misc)))

(export 'misc-get-padding)

;;; --- End of file gtk3.misc.lisp ---------------------------------------------
