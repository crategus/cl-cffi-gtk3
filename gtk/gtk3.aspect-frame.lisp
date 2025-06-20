;;; ----------------------------------------------------------------------------
;;; gtk3.aspect-frame.lisp
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
;;; GtkAspectFrame
;;;
;;;     A frame that constrains its child to a particular aspect ratio
;;;
;;; Types and Values
;;;
;;;     GtkAspectFrame
;;;
;;; Functions
;;;
;;;     gtk_aspect_frame_new
;;;     gtk_aspect_frame_set
;;;
;;; Properties
;;;
;;;     obey-child
;;;     ratio
;;;     xalign
;;;     yalign
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;        ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkFrame
;;;                        ╰── GtkAspectFrame
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAspectFrame implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAspectFrame
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAspectFrame" aspect-frame
  (:superclass frame
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_aspect_frame_get_type")
  ((obey-child
    aspect-frame-obey-child
    "obey-child" "gboolean" t t)
   (ratio
    aspect-frame-ratio
    "ratio" "gfloat" t t)
   (xalign
    aspect-frame-xalign
    "xalign" "gfloat" t t)
   (yalign
    aspect-frame-yalign
    "yalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'aspect-frame 'type)
 "@version{2025-06-16}
  @begin{short}
    The @class{gtk:aspect-frame} widget is useful when you want pack a widget
    so that it can resize but always retains the same aspect ratio.
  @end{short}
  For instance, one might be drawing a small preview of a larger image.
  The @class{gtk:aspect-frame} class derives from the @class{gtk:frame} class,
  so it can draw a label and a frame around the child. The frame will be
  \"shrink-wrapped\" to the size of the child.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:aspect-frame} implementation uses a CSS node with name
    @code{frame}.
  @end{dictionary}
  @see-constructor{gtk:aspect-frame-new}
  @see-slot{gtk:aspect-frame-obey-child}
  @see-slot{gtk:aspect-frame-ratio}
  @see-slot{gtk:aspect-frame-xalign}
  @see-slot{gtk:aspect-frame-yalign}
  @see-class{gtk:frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:aspect-frame-obey-child --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "obey-child" 'aspect-frame) t)
 "The @code{obey-child} property of type @code{:boolean} (Read / Write) @br{}
  Force aspect ratio to match that of the child widget of the aspect frame.@br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-obey-child)
      "Accessor"
      (documentation 'aspect-frame-obey-child 'function)
 "@version{2023-12-30}
  @syntax{(gtk:aspect-frame-obey-child object) => obey-child}
  @syntax{(setf (gtk:aspect-frame-obey-child object) obey-child)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[obey-child]{a boolean whether to force the aspect ratio}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{obey-child} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}
  Whether to force the aspect ratio to match that of the child widget of the
  aspect frame.
  @see-class{gtk:aspect-frame}")

;;; --- gtk:aspect-frame-ratio -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ratio" 'aspect-frame) t)
 "The @code{ratio} property of type @code{:float} (Read / Write) @br{}
  The aspect ratio if the @code{obey-child} property is @em{false}. @br{}
  Allowed values: [0.0001, 10000.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-ratio)
      "Accessor"
      (documentation 'aspect-frame-ratio 'function)
 "@version{2025-06-16}
  @syntax{(gtk:aspect-frame-ratio object) => ratio}
  @syntax{(setf (gtk:aspect-frame-ratio object) ratio)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[ratio]{a number coerced to a single float for an aspect ratio}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{ratio} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}
  The aspect ratio if the @slot[gtk:aspect-frame]{obey-child} property is
  @em{false}. Allowed values are in [0.0001, 10000.0]. The default value is 1.0.
  @see-class{gtk:aspect-frame}")

;;; --- gtk:aspect-frame-xalign ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'aspect-frame) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The x alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-xalign)
      "Accessor"
      (documentation 'aspect-frame-xalign 'function)
 "@version{2025-06-16}
  @syntax{(gtk:aspect-frame-xalign object) => xalign}
  @syntax{(setf (gtk:aspect-frame-xalign object) xalign)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[xalign]{a number coerced to a single float for the x alignment of
    the child widget}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{xalign} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}
  The x alignment of the child widget in the aspect frame container.
  @see-class{gtk:aspect-frame}")

;;; --- gtk:aspect-frame-yalign ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'aspect-frame) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The y alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-yalign)
      "Accessor"
      (documentation 'aspect-frame-yalign 'function)
 "@version{2025-06-16}
  @syntax{(gtk:aspect-frame-yalign object) => yalign}
  @syntax{(setf (gtk:aspect-frame-yalign object) yalign)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[yalign]{a number coerced to a single float for the y alignment of
    the child widget}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{yalign} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}
  The y alignment of the child widget in the aspect frame container.
  @see-class{gtk:aspect-frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_new
;;; ----------------------------------------------------------------------------

(declaim (inline aspect-frame-new))

(defun aspect-frame-new (label xalign yalign ratio obey-child)
 #+liber-documentation
 "@version{2025-06-16}
  @argument[label]{a string for the label text}
  @argument[xalign]{a number coerced to a single float for the horizontal
    alignment of the child within the allocation of the aspect frame, this
    ranges from 0.0 (left aligned) to 1.0 (right aligned)}
  @argument[yalign]{a number coerced to a single float for the vertical
    alignment of the child within the allocation of the aspect frame, this
    ranges from 0.0 (left aligned) to 1.0 (right aligned)}
  @argument[ratio]{a number coerced to a single float for the desired aspect
    ratio}
  @argument[obey-child]{if @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  @return{The new @class{gtk:aspect-frame} widget.}
  @begin{short}
    Create a new aspect frame container.
  @end{short}
  @see-class{gtk:aspect-frame}"
  (make-instance 'aspect-frame
                 :label label
                 :xalign xalign
                 :yalign yalign
                 :ratio ratio
                 :obey-child obey-child))

(export 'aspect-frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_set
;;; ----------------------------------------------------------------------------

(defun aspect-frame-set (frame xalign yalign ratio obey-child)
 #+liber-documentation
 "@version{2025-06-16}
  @argument[frame]{a @class{gtk:aspect-frame} widget}
  @argument[xalign]{a number coerced to a single float for the horizontal
    alignment of the child within the allocation of the aspect frame container,
    this ranges from 0.0 (left aligned) to 1.0 (right aligned)}
  @argument[yalign]{a number coerced to a single float for the vertical
    alignment of the child within the allocation of the aspect frame container,
    this ranges from 0.0 (left aligned) to 1.0 (right aligned)}
  @argument[ratio]{a number coerced to a single float for the desired aspect
    ratio}
  @argument[obey-child]{if @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  @begin{short}
    Set parameters for an existing aspect frame.
  @end{short}
  @see-class{gtk:aspect-frame}"
  (setf (aspect-frame-xalign frame) xalign
        (aspect-frame-yalign frame) yalign
        (aspect-frame-ratio frame) ratio
        (aspect-frame-obey-child frame) obey-child))

(export 'aspect-frame-set)

;;; --- End of file gtk3.aspect-frame.lisp -------------------------------------
