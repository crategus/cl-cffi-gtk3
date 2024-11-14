;;; ----------------------------------------------------------------------------
;;; gtk3.alignment.lisp
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
;;; GtkAlignment
;;;
;;;     A widget which controls the alignment and size of its child
;;;
;;; Types and Values
;;;
;;;     GtkAlignment
;;;
;;; Functions
;;;
;;;     gtk_alignment_new
;;;     gtk_alignment_set
;;;     gtk_alignment_get_padding
;;;     gtk_alignment_set_padding
;;;
;;; Properties
;;;
;;;     bottom-padding
;;;     left-padding
;;;     right-padding
;;;     top-padding
;;;     xalign
;;;     xscale
;;;     yalign
;;;     yscale
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkAlignment
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAlignment implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkAlignment
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAlignment" alignment
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_alignment_get_type")
  ((bottom-padding
    alignment-bottom-padding
    "bottom-padding" "guint" t t)
   (left-padding
    alignment-left-padding
    "left-padding" "guint" t t)
   (right-padding
    alignment-right-padding
    "right-padding" "guint" t t)
   (top-padding
    alignment-top-padding
    "top-padding" "guint" t t)
   (xalign
    alignment-xalign
    "xalign" "gfloat" t t)
   (xscale
    alignment-xscale
    "xscale" "gfloat" t t)
   (yalign
    alignment-yalign
    "yalign" "gfloat" t t)
   (yscale
    alignment-yscale
    "yscale" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'alignment 'type)
 "@version{2023-12-28}
  @begin{short}
    The @class{gtk:alignment} widget controls the alignment and size of its
    child widget.
  @end{short}
  It has four settings: @code{xscale}, @code{yscale}, @code{xalign},
  and @code{yalign}.

  The scale settings are used to specify how much the child widget should
  expand to fill the space allocated to the alignment. The values can range
  from 0.0, meaning the child does not expand at all, to 1.0, meaning the
  child expands to fill all of the available space.

  The align settings are used to place the child widget within the available
  area. The values range from 0.0, top or left, to 1.0, bottom or right. Of
  course, if the scale settings are both set to 1.0, the alignment settings
  have no effect.
  @begin[Warning]{dictionary}
    The @class{gtk:alignment} widget has been deprecated in 3.14 and should not
    be used in newly written code. The desired effect can be achieved by using
    the @slot[gtk:widget]{halign}, @slot[gtk:widget]{valign} and
    @slot[gtk:widget]{margin} properties on the child widget.
  @end{dictionary}
  @see-constructor{gtk:alignment-new}
  @see-slot{gtk:alignment-bottom-padding}
  @see-slot{gtk:alignment-left-padding}
  @see-slot{gtk:alignment-right-padding}
  @see-slot{gtk:alignment-top-padding}
  @see-slot{gtk:alignment-xalign}
  @see-slot{gtk:alignment-xscale}
  @see-slot{gtk:alignment-yalign}
  @see-slot{gtk:alignment-yscale}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:alignment-bottom-padding -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "bottom-padding" 'alignment) t)
 "The @code{bottom-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the bottom of the child widget. @br{}
  Allowed values: < +32.767 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-bottom-padding)
      "Accessor"
      (documentation 'alignment-bottom-padding 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{bottom-padding} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-bottom-padding} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-bottom} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-margin-bottom}")

;;; --- gtk:alignment-left-padding ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "left-padding" 'alignment) t)
 "The @code{left-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the left of the child widget. @br{}
  Allowed values: < +32.767 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-left-padding)
      "Accessor"
      (documentation 'alignment-left-padding 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{left-padding} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-left-padding} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-start} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-margin-start}")

;;; --- gtk:alignment-right-padding --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "right-padding" 'alignment) t)
 "The @code{right-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the right of the child widget. @br{}
  Allowed values: < +32.767 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-right-padding)
      "Accessor"
      (documentation 'alignment-right-padding 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{right-padding} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-right-padding} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-end} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-margin-end}")

;;; --- gtk:alignment-top-padding ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "top-padding" 'alignment) t)
 "The @code{top-padding} property of type @code{:uint} (Read / Write) @br{}
  The padding to insert at the top of the child widget. @br{}
  Allowed values: < +32.767 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-top-padding)
      "Accessor"
      (documentation 'alignment-top-padding 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{top-padding} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-top-padding} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-margin-top} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-margin-top}")

;;; --- gtk:alignment-xalign ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'alignment) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  Horizontal position of the child widget in available space. 0.0 is left
  aligned, 1.0 is right aligned. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-xalign)
      "Accessor"
      (documentation 'alignment-xalign 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{xalign} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-xalign} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-halign} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-halign}")

;;; --- gtk:alignment-xscale ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xscale" 'alignment) t)
 "The @code{xscale} property @code{:float} (Read / Write) @br{}
  If available horizontal space is bigger than needed for the child widget, how
  much of it to use for the child. 0.0 means none, 1.0 means all. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-xscale)
      "Accessor"
      (documentation 'alignment-xscale 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{xscale} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-xscale} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-hexpand} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-hexpand}")

;;; --- gtk:alignment-yalign ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'alignment) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  Vertical position of the child widget in available space. 0.0 is top aligned,
  1.0 is bottom aligned. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-yalign)
      "Accessor"
      (documentation 'alignment-yalign 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{yalign} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-yalign} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-valign} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-valign}")

;;; --- gtk:alignment-yscale ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yscale" 'alignment) t)
 "The @code{yscale} property of type @code{:float} (Read / Write) @br{}
  If available vertical space is bigger than needed for the child widget, how
  much of it to use for the child. 0.0 means none, 1.0 means all. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'alignment-yscale)
      "Accessor"
      (documentation 'alignment-yscale 'function)
 "@version{2023-12-28}
  @begin{short}
    Accessor of the @slot[gtk:alignment]{yscale} slot of the
    @class{gtk:alignment} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-yscale} function has been deprecated since version
    3.14 and should not be used in newly written code. Use the
    @fun{gtk:widget-vexpand} function instead.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-vexpand}")

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline alignment-new))

(defun alignment-new (xalign yalign xscale yscale)
 #+liber-documentation
 "@version{2023-12-28}
  @argument[xalign]{a float with the horizontal alignment of the child widget,
    from 0.0 (left) to 1.0 (right)}
  @argument[yalign]{a float with the vertical alignment of the child widget,
    from 0.0 (top) to 1.0 (bottom)}
  @argument[xscale]{a float with the amount that the child widget expands
    horizontally to fill up unused space, from 0.0 to 1.0, a value of 0.0
    indicates that the child widget should never expand, a value of 1.0
    indicates that the child widget will expand to fill all of the space
    allocated for the alignment}
  @argument[yscale]{a float with the amount that the child widget expands
    vertically to fill up unused space, from 0.0 to 1.0, he values are similar
    to @arg{xscale}}
  @return{The new @class{gtk:alignment} widget.}
  @short{Creates a new alignment.}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-new} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-class{gtk:widget}"
  (make-instance 'alignment
                 :xalign xalign
                 :yalign yalign
                 :xscale xscale
                 :yscale yscale))

(export 'alignment-new)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_set ()
;;; ----------------------------------------------------------------------------

(declaim (inline alignment-set))

(defun alignment-set (alignment xalign yalign xscale yscale)
 #+liber-documentation
 "@version{2023-12-28}
  @argument[alignment]{a @class{gtk:alignment} widget}
  @argument[xalign]{a float with the horizontal alignment of the child widget,
    from 0.0 (left) to 1.0 (right)}
  @argument[yalign]{a float with the vertical alignment of the child widget,
    from 0.0 (top) to 1.0 (bottom)}
  @argument[xscale]{a float with the amount that the child widget expands
    horizontally to fill up unused space, from 0.0 to 1.0, a value of 0.0
    indicates that the child widget should never expand, a value of 1.0
    indicates that the child widget will expand to fill all of the space
    allocated for the alignment}
  @argument[yscale]{a float with the amount that the child widget expands
    vertically to fill up unused space, from 0.0 to 1.0, the values are similar
    to @arg{xscale}}
  @short{Sets the alignment values.}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-set} function has been deprecated since version 3.14
    and should not be used in newly written code. Use the @class{gtk:widget}
    alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-class{gtk:widget}"
  (setf (alignment-xalign alignment) xalign
        (alignment-yalign alignment) yalign
        (alignment-xscale alignment) xscale
        (alignment-yscale alignment) yscale)
  nil)

(export 'alignment-set)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_get_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline alignment-get-padding))

(defun alignment-get-padding (alignment)
 #+liber-documentation
 "@version{2023-12-28}
  @argument[alignment]{a @class{gtk:alignment} widget}
  @begin{return}
    @arg{top} -- an unsigned integer with the padding for the top of the child
    widget @br{}
    @arg{bottom} -- an unsigned integer with the padding for the bottom of the
    child widget @br{}
    @arg{left} -- an unsigned integer with the padding for the left of the
    child widget @br{}
    @arg{right} -- an unsigned integer with the padding for the right of the
    child widget
  @end{return}
  @begin{short}
    Gets the padding on the different sides of the child widget.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-get-padding} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @class{gtk:widget} alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-class{gtk:widget}
  @see-function{gtk:alignment-set-padding}"
  (values (alignment-top-padding alignment)
          (alignment-bottom-padding alignment)
          (alignment-left-padding alignment)
          (alignment-right-padding alignment)))

(export 'alignment-get-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_alignment_set_padding ()
;;; ----------------------------------------------------------------------------

(declaim (inline alignment-set-padding))

(defun alignment-set-padding (alignment top bottom left right)
 #+liber-documentation
 "@version{2023-12-28}
  @argument[alignment]{a @class{gtk:alignment} widget}
  @argument[top]{an unsigned integer with the padding at the top of the child
    widget}
  @argument[bottom]{an unsigned integer with the padding at the bottom of the
    child widget}
  @argument[left]{an unsigned integer with the padding at the left of the
    child widget}
  @argument[right]{an unsigned integer with the padding at the right of the
    child widget}
  @begin{short}
    Sets the padding on the different sides of the widget.
  @end{short}
  The padding adds blank space to the sides of the child widget. For instance,
  this can be used to indent the child widget towards the right by adding
  padding on the left.
  @begin[Warning]{dictionary}
    The @fun{gtk:alignment-set-padding} function has been deprecated since
    version 3.14 and should not be used in newly written code. Use the
    @class{gtk:widget} alignment and margin properties.
  @end{dictionary}
  @see-class{gtk:alignment}
  @see-class{gtk:widget}
  @see-function{gtk:alignment-get-padding}"
  (setf (alignment-top-padding alignment) top
        (alignment-bottom-padding alignment) bottom
        (alignment-left-padding alignment) left
        (alignment-right-padding alignment) right)
  nil)

(export 'alignment-set-padding)

;;; --- End of file gtk3.alignment.lisp ----------------------------------------
