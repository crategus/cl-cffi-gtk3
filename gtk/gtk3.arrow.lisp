;;; ----------------------------------------------------------------------------
;;; gtk3.arrow.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkArrow
;;;
;;;     Displays an arrow
;;;
;;; Types and Values
;;;
;;;     GtkArrow
;;;
;;; Functions
;;;
;;;     gtk_arrow_new
;;;     gtk_arrow_set
;;;
;;; Properties
;;;
;;;     arrow-type
;;;     shadow-type
;;;
;;; Style Properties
;;;
;;;     arrow-scaling
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkArrow
;;;
;;; Implemented Interfaces
;;;
;;;     GtkArrow implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkArrow
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkArrow" arrow
  (:superclass misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_arrow_get_type")
  ((arrow-type
    arrow-arrow-type
    "arrow-type" "GtkArrowType" t t)
   (shadow-type
    arrow-shadow-type
    "shadow-type" "GtkShadowType" t t)))

#+liber-documentation
(setf (documentation 'arrow 'type)
 "@version{2023-12-30}
  @begin{short}
    The @class{gtk:arrow} widget should be used to draw simple arrows that need
    to point in one of the four cardinal directions: up, down, left, or right.
  @end{short}
  The style of the arrow can be one of shadow in, shadow out, etched in, or
  etched out. Note that these directions and style types may be ammended in
  versions of GTK to come.

  The @class{gtk:arrow} widget will fill any space alloted to it, but since it
  is inherited from the @class{gtk:misc} class, it can be padded and/or aligned,
  to fill exactly the space the programmer desires.

  Arrows are created with a call to the @fun{gtk:arrow-new} function. The
  direction or style of an arrow can be changed after creation by using the
  @fun{gtk:arrow-set} function.
  @begin[Warning]{dictionary}
    The @class{gtk:arrow} widget has been deprecated. You can simply use a
    @class{gtk:image} widget with a suitable icon name, such as
    \"pan-down-symbolic\". When replacing the @class{gtk:arrow} widget by an
    image, pay attention to the fact that the @class{gtk:arrow} widget is doing
    automatic flipping between @code{:left} and @code{:right}, depending on the
    text direction. To get the same effect with an image, use the icon names
    \"pan-start-symbolic\" and \"pan-end-symbolic\", which react to the text
    direction.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of type @code{:float}
        (Read) @br{}
        Amount of space used up by the arrow. @br{}
        Allowed values: [0,1] @br{}
        Default value: 0.7
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:arrow-new}
  @see-slot{gtk:arrow-arrow-type}
  @see-slot{gtk:arrow-shadow-type}
  @see-class{gtk:misc}
  @see-class{gtk:image}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:arrow-arrow-type ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "arrow-type" 'arrow) t)
 "The @code{arrow-type} property of type @symbol{gtk:arrow-type}
  (Read / Write) @br{}
  The direction the arrow should point. @br{}
  Default value: @code{:right}")

#+liber-documentation
(setf (liber:alias-for-function 'arrow-arrow-type)
      "Accessor"
      (documentation 'arrow-arrow-type 'function)
 "@version{2023-12-30}
  @syntax{(gtk:arrow-arrow-type object) => arrow-type}
  @syntax{(setf (gtk:arrow-arrow-type object) arrow-type)}
  @argument[object]{a @class{gtk:arrow} widget}
  @argument[arrow-type]{a value of the @symbol{gtk:arrow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:arrow]{arrow-type} slot of the @class{gtk:arrow}
    class.
  @end{short}
  The direction the arrow should point.
  @begin[Warning]{dictionary}
    The @fun{gtk:arrow-arrow-type} function has been deprecated since version
    3.14 and should not be used in newly written code. Use a @class{gtk:image}
    widget with a suitable icon.
  @end{dictionary}
  @see-class{gtk:arrow}
  @see-class{gtk:image}
  @see-symbol{gtk:arrow-type}")

;;; --- gtk:arrow-shadow-type --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shadow-type" 'arrow) t)
 "The @code{shadow-type} property of type @symbol{gtk:shadow-type}
  (Read / Write) @br{}
  Appearance of the shadow surrounding the arrow. @br{}
  Default value: @code{:out}")

#+liber-documentation
(setf (liber:alias-for-function 'arrow-shadow-type)
      "Accessor"
      (documentation 'arrow-shadow-type 'function)
 "@version{2023-12-30}
  @syntax{(gtk:arrow-shadow-type object) => shadow-type}
  @syntax{(setf (gtk:arrow-shadow-type object) shadow-type)}
  @argument[object]{a @class{gtk:arrow} widget}
  @argument[shadow-type]{a value of the @symbol{gtk:shadow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:arrow]{shadow-type} slot of the @class{gtk:arrow}
    class.
  @end{short}
  Appearance of the shadow surrounding the arrow.
  @begin[Warning]{dictionary}
    The @fun{gtk:arrow-shadow-type} function has been deprecated since version
    3.14 and should not be used in newly written code. Use a @class{gtk:image}
    widget with a suitable icon.
  @end{dictionary}
  @see-class{gtk:arrow}
  @see-class{gtk:image}
  @see-symbol{gtk:shadow-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline arrow-new))

(defun arrow-new (arrow-type shadow-type)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[arrow-type]{a @symbol{gtk:arrow-type} value}
  @argument[shadow-type]{a @symbol{gtk:shadow-type} value}
  @return{The new @class{gtk:arrow} widget.}
  @short{Creates a new arrow.}
  @begin[Warning]{dictionary}
    The @fun{gtk:arrow-new} function has been deprecated since version 3.14 and
    should not be used in newly written code. Use a @class{gtk:image} widget
    with a suitable icon.
  @end{dictionary}
  @see-class{gtk:arrow}
  @see-class{gtk:image}
  @see-symbol{gtk:arrow-type}
  @see-symbol{gtk:shadow-type}"
  (make-instance 'arrow
                 :arrow-type arrow-type
                 :shadow-type shadow-type))

(export 'arrow-new)

;;; ----------------------------------------------------------------------------
;;; gtk_arrow_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline arrow-set))

(defun arrow-set (arrow arrow-type shadow-type)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[arrow]{a @class{gtk:arrow} widget}
  @argument[arrow-type]{a value of the @symbol{gtk:arrow-type} enumeration}
  @argument[shadow-type]{a value of the @symbol{gtk:shadow-type} enumeration}
  @short{Sets the direction and style of the arrow.}
  @begin[Warning]{dictionary}
    The @fun{gtk:arrow-set} function has been deprecated since version 3.14 and
    should not be used in newly written code. Use a @class{gtk:image} widget
    with a suitable icon.
  @end{dictionary}
  @see-class{gtk:arrow}
  @see-class{gtk:image}
  @see-symbol{gtk:arrow-type}
  @see-symbol{gtk:shadow-type}"
  (setf (arrow-arrow-type arrow) arrow-type
        (arrow-shadow-type arrow) shadow-type)
  nil)

(export 'arrow-set)

;;; --- End of file gtk3.arrow.lisp --------------------------------------------
