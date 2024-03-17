;;; ----------------------------------------------------------------------------
;;; gtk3.frame.lisp
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
;;; GtkFrame
;;;
;;;     A bin with a decorative frame and optional label
;;;
;;; Types and Values
;;;
;;;     GtkFrame
;;;
;;; Functions
;;;
;;;     gtk_frame_new
;;;     gtk_frame_set_label                                Accessor
;;;     gtk_frame_set_label_widget                         Accessor
;;;     gtk_frame_set_label_align
;;;     gtk_frame_set_shadow_type                          Accessor
;;;     gtk_frame_get_label                                Accessor
;;;     gtk_frame_get_label_align
;;;     gtk_frame_get_label_widget                         Accessor
;;;     gtk_frame_get_shadow_type                          Accessor
;;;
;;; Properties
;;;
;;;     label
;;;     label-widget
;;;     label-xalign
;;;     label-yalign
;;;     shadow-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkFrame
;;;                         ╰── GtkAspectFrame
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFrame implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFrame
;;; ----------------------------------------------------------------------------

;; TODO: This is a workaround to initialize the symbol for GtkFrame. This is
;; a bug. For GtkFrame the method initialize-instance for gobject-class is not
;; called. Why?
(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (glib:symbol-for-gtype "GtkFrame") 'frame)))

(gobject:define-g-object-class "GtkFrame" frame
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_frame_get_type")
  ((label
    frame-label
    "label" "gchararray" t t)
   (label-widget
    frame-label-widget
    "label-widget" "GtkWidget" t t)
   (label-xalign
    frame-label-xalign
    "label-xalign" "gfloat" t t)
   (label-yalign
    frame-label-yalign
    "label-yalign" "gfloat" t t)
   (shadow-type
    frame-shadow-type
    "shadow-type" "GtkShadowType" t t)))

#+liber-documentation
(setf (documentation 'frame 'type)
 "@version{2023-2-18}
  @begin{short}
    The frame widget is a @class{gtk:bin} widget that surrounds its child with
    a decorative frame and an optional label.
  @end{short}

  @image[frame]{Figure: GtkFrame}

  If present, the label is drawn in a gap in the top side of the frame. The
  position of the label can be controlled with the @fun{gtk:frame-label-align}
  function.
  @begin[GtkFrame as GtkBuildable]{dictionary}
    The @class{gtk:frame} implementation of the @class{gtk:buildable} interface
    supports placing a child in the label position by specifying
    @code{\"label\"} as the @code{type} attribute of a @code{<child>} element.
    A normal content child can be specified without specifying a @code{<child>}
    type attribute.

    @b{Example:} A UI definition fragment with a @class{gtk:frame} widget
    @begin{pre}
<object class=\"GtkFrame\">
 <child type=\"label\">
   <object class=\"GtkLabel\" id=\"frame-label\"/>
 </child>
 <child>
   <object class=\"GtkEntry\" id=\"frame-content\"/>
 </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
frame
├── border[.flat]
├── <label widget>
╰── <child>
    @end{pre}
    The @class{gtk:frame} implementation has a main CSS node named @code{frame}
    and a subnode named @code{border}. The @code{border} node is used to draw
    the visible border. You can set the appearance of the border using CSS
    properties like @code{border-style} on the @code{border} node.

    The @code{border} node can be given the @code{.flat} style class, which is
    used by themes to disable drawing of the border. To do this from code, call
    the @fun{gtk:frame-shadow-type} function with the @code{:none} value to add
    the @code{.flat} style class or any other shadow type to remove it.
  @end{dictionary}
  @see-constructor{gtk:frame-new}
  @see-slot{gtk:frame-label}
  @see-slot{gtk:frame-label-widget}
  @see-slot{gtk:frame-label-xalign}
  @see-slot{gtk:frame-label-yalign}
  @see-slot{gtk:frame-shadow-type}
  @see-class{gtk:bin}
  @see-class{gtk:widget}
  @see-class{gtk:buildable}
  @see-symbol{gtk:shadow-type}
  @see-function{gtk:frame-label-align}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:frame-label --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'frame) t)
 "The @code{label} property of type  @code{:string} (Read / Write) @br{}
  Text of the frame's label. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label)
      "Accessor"
      (documentation 'frame-label 'function)
 "@version{2023-2-18}
  @syntax{(gtk:frame-label object) => label}
  @syntax{(setf (gtk:frame-label object) label)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[label]{a string with the text to use as the label of the frame}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label} slot of the @class{gtk:frame} class.
  @end{short}
  The @fun{gtk:frame-label} function returns the text in the label, or
  @code{nil} if there was no label widget or the label widget was not a
  @class{gtk:label} widget. The @setf{gtk:frame-label} function sets the text
  of the label. If the @arg{label} argument is @code{nil}, the current label is
  removed.

  The frame will have a @class{gtk:label} widget for the label widget if a
  non-@code{nil} argument was passed to the @fun{gtk:frame-new} function.
  @see-class{gtk:frame}
  @see-class{gtk:label}
  @see-function{gtk:frame-new}
  @see-function{gtk:frame-label-widget}")

;;; --- gtk:frame-label-widget -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget" 'frame) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  A widget to display in place of the usual frame label.")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label-widget)
      "Accessor"
      (documentation 'frame-label-widget 'function)
 "@version{2023-2-18}
  @syntax{(gtk:frame-label-widget object) => widget}
  @syntax{(setf (gtk:frame-label-widget object) widget)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[widget]{a @class{gtk:widget} label widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-widget} slot of the @class{gtk:frame}
    class.
  @end{short}
  The @fun{gtk:frame-label-widget} function retrieves the label widget for the
  frame. The @setf{gtk:frame-label-widget} function sets the label widget. This
  is the widget that will appear embedded in the top edge of the frame as a
  title.
  @see-class{gtk:frame}
  @see-class{gtk:widget}
  @see-function{gtk:frame-label}")

;;; --- gtk:frame-label-xalign -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-xalign" 'frame) t)
 "The @code{label-xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment of the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label-xalign)
      "Accessor"
      (documentation 'frame-label-xalign 'function)
 "@version{2023-2-18}
  @syntax{(gtk:frame-label-xalign object) => xalign}
  @syntax{(setf (gtk:frame-label-xalign object) xalign)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[xalign]{a float with the horizontal alignment of the label}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-xalign} slot of the @class{gtk:frame}
    class.
  @end{short}
  @see-class{gtk:frame}
  @see-function{gtk:frame-label-yalign}")

;;; --- gtk:frame-label-yalign -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-yalign" 'frame) t)
 "The @code{label-yalign} property of type @code{:float} (Read / Write) @br{}
  The vertical alignment of the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label-yalign)
      "Accessor"
      (documentation 'frame-label-yalign 'function)
 "@version{2023-2-18}
  @syntax{(gtk:frame-label-yalign object) => yalign}
  @syntax{(setf (gtk:frame-label-yalign object) yalign)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[yalign]{a float with the vertical alignment of the label}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-yalign} slot of the @class{gtk:frame}
    class.
  @end{short}
  @see-class{gtk:frame}
  @see-function{gtk:frame-label-xalign}")

;;; --- gtk:frame-shadow-type --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shadow-type" 'frame) t)
 "The @code{shadow-type} property of type @symbol{gtk:shadow-type}
  (Read / Write) @br{}
  Appearance of the frame border. @br{}
  Default value: @code{:etched-in}")

#+liber-documentation
(setf (liber:alias-for-function 'frame-shadow-type)
      "Accessor"
      (documentation 'frame-shadow-type 'function)
 "@version{2023-2-18}
  @syntax{(gtk:frame-shadow-type object) => type}
  @syntax{(setf (gtk:frame-shadow-type object) type)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[type]{a value of the @symbol{gtk:shadow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:frame]{shadow-type} slot of the @class{gtk:frame}
    class.
  @end{short}
  The @fun{gtk:frame-shadow-type} function retrieves the shadow type of the
  frame. The @setf{gtk:frame-shadow-type} function sets the shadow type.
  @see-class{gtk:frame}
  @see-symbol{gtk:shadow-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_frame_new ()
;;; ----------------------------------------------------------------------------

(defun frame-new (&optional label)
 #+liber-documentation
 "@version{2023-2-18}
  @argument[label]{an optional string with the text to use as the label of the
    frame}
  @return{The new @class{gtk:frame} widget.}
  @begin{short}
    Creates a new frame widget, with an optional label.
  @end{short}
  If the @arg{label} argument is @code{nil}, the label is omitted.
  @see-class{gtk:frame}"
  (make-instance 'frame
                 :label (if label label (cffi:null-pointer))))

(export 'frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_align ()
;;; gtk_frame_set_label_align () -> frame-label-align
;;; ----------------------------------------------------------------------------

(defun (setf frame-label-align) (align frame)
  (destructuring-bind (xalign yalign) align
     (values (setf (frame-label-xalign frame) xalign)
             (setf (frame-label-yalign frame) yalign))))

(defun frame-label-align (frame)
 #+liber-documentation
 "@version{2023-2-18}
  @syntax{(gtk:frame-label-align frame) => xalign, yalign}
  @syntax{(setf (gtk:frame-label-align frame) (list xalign yalign))}
  @argument[frame]{a @class{gtk:frame} widget}
  @argument[xalign]{a float with the position of the label along the top edge
    of the widget, a value of 0.0 represents left alignment, 1.0 represents
    right alignment}
  @argument[yalign]{a float with the y alignment of the label, a value of 0.0
    aligns under the frame, 1.0 aligns above the frame, if the values are
    exactly 0.0 or 1.0 the gap in the frame will not be painted because the
    label will be completely above or below the frame}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-xalign} and
    @slot[gtk:frame]{label-yalign} properties of a frame widget.
  @end{short}
  The @fun{gtk:frame-label-align} function retrieves the x and y alignment of
  the frame's label. The @setf{gtk:frame-label-align} function sets the
  alignment of the frame container's label. The default values for a newly
  created frame are 0.0 and 0.5.
  @see-class{gtk:frame}
  @see-function{gtk:frame-label-xalign}
  @see-function{gtk:frame-label-yalign}"
  (values (frame-label-xalign frame)
          (frame-label-yalign frame)))

(export 'frame-label-align)

;;; --- End of file gtk3.frame.lisp --------------------------------------------
