;;; ----------------------------------------------------------------------------
;;; gtk3.overlay.lisp
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
;;; GtkOverlay
;;;
;;;     A container which overlays widgets on top of each other
;;;
;;; Types and Values
;;;
;;;     GtkOverlay
;;;
;;; Functions
;;;
;;;     gtk_overlay_new
;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_reorder_overlay
;;;     gtk_overlay_get_overlay_pass_through   -> gtk:overlay-child-pass-through
;;;     gtk_overlay_set_overlay_pass_through   -> gtk:overlay-child-pass-through
;;;
;;; Child Properties
;;;
;;;     index
;;;     pass-through
;;;
;;; Signals
;;;
;;;     get-child-position
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkOverlay
;;;
;;; Implemented Interfaces
;;;
;;;     GtkOverlay implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOverlay
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkOverlay" overlay
  (:superclass bin
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_overlay_get_type")
  nil)

#+liber-documentation
(setf (documentation 'overlay 'type)
 "@version{2025-06-27}
  @begin{short}
    The @class{gtk:overlay} widget is a container which contains a single main
    widget, on top of which it can place overlay widgets.
  @end{short}
  The position of each overlay widget is determined by its
  @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties. For
  example, a overlay widget with both alignments set to @val[gtk:align]{:start}
  will be placed at the top left corner of the main widget, whereas an overlay
  with the @slot[gtk:widget]{halign} property set to @val[gtk:align]{:center}
  and the @slot[gtk:widget]{valign} property set to @val[gtk:align]{:end} will
  be placed a the bottom edge of the main widget, horizontally centered. The
  position can be adjusted by setting the margin properties of the overlay
  widget to non-zero values.

  More complicated placement of overlays is possible by connecting to the
  @sig[gtk:overlay]{get-child-position} signal.
  @begin[GtkOverlay as GtkBuildable]{dictionary}
    The @class{gtk:overlay} implementation of the @class{gtk:buildable}
    interface supports placing a child widget as an overlay by specifying
    @code{\"overlay\"} as the @code{\"type\"} attribute of a @code{<child>}
    element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:overlay} implementation has a single CSS node with the name
    @code{overlay}. Overlay children whose alignments cause them to be
    positioned at an edge get the @code{.left}, @code{.right}, @code{.top},
    and/or @code{.bottom} style classes according to their position.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[overlay:index]{property}
      The @code{index} child property of type @code{:int} (Read / Write) @br{}
      The index of the overlay in the parent, -1 for the main child. @br{}
      Allowed values: >= -1 @br{}
      Default value: 0
    @end{property}
    @begin[overlay:pass-through]{property}
      The @code{pass-through} child property of type @code{:boolean}
      (Read / Write) @br{}
      Whether pass through input does not affect main child. @br{}
      Default value: @em{false}
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[overlay::get-child-position]{signal}
      @begin{pre}
lambda (overlay widget allocation)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[overlay]{The @class{gtk:overlay} widget that emitted the
          signal.}
        @entry[widget]{The @class{gtk:widget} child widget to position.}
        @entry[allocation]{The return location of type @class{gdk:rectangle}
          for the allocation.}
        @entry[Returns]{@em{True} if the allocation has been filled.}
      @end{simple-table}
      The signal is emitted to determine the position and size of any overlay
      child widgets. A handler for this signal should fill allocation with the
      desired position and size for @arg{widget}, relative to the 'main' child
      of the overlay. The default handler for this signal uses the
      @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties of the
      widget to determine the position and gives the widget its natural size,
      except that an alignment of @val[gtk:align]{:fill} will cause the overlay
      to be full-width/height. If the main child is a
      @class{gtk:scrolled-window} widget, the overlays are placed relative to
      its contents.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:buildable}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:overlay-child-index ------------------------------------------------

(define-child-property overlay-child-index "index" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'overlay-child-index)
      "Accessor"
      (documentation 'overlay-child-index 'function)
 "@version{2025-06-27}
  @syntax{(gtk:overlay-child-index container child) => index)}
  @syntax{(setf (gtk:overlay-child-index container child) index)}
  @argument[container]{a @class{gtk:overlay} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[index]{an integer for the index of the child widget in the parent}
  @begin{short}
    Accessor of the @prop[gtk:overlay]{index} child property of the
    @class{gtk:overlay} class.
  @end{short}
  The index of the child widget in the parent, -1 for the main child.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}")

;;; --- gtk:overlay-child-pass-through -----------------------------------------

(define-child-property overlay-child-pass-through
                       "pass-through" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'overlay-child-pass-through)
      "Accessor"
      (documentation 'overlay-child-pass-through 'function)
 "@version{2025-06-27}
  @syntax{(gtk:overlay-child-pass-through container child) => setting)}
  @syntax{(setf (gtk:overlay-child-pass-through container child) setting)}
  @argument[container]{a @class{gtk:overlay} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[setting]{a boolean whether to pass through input}
  @begin{short}
    Accessor of the @prop[gtk:overlay]{pass-through} child property of the
    @class{gtk:overlay} class.
  @end{short}
  Pass through input, does not affect main child.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_new
;;; ----------------------------------------------------------------------------

(declaim (inline overlay-new))

(defun overlay-new ()
 #+liber-documentation
 "@version{2024-01-01}
  @return{The new @class{gtk:overlay} widget.}
  @short{Creates a new overlay container.}
  @see-class{gtk:overlay}"
  (make-instance 'overlay))

(export 'overlay-new)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_add_overlay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_overlay_add_overlay" overlay-add-overlay) :void
 #+liber-documentation
 "@version{2024-01-01}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be added to the
    container}
  @begin{short}
    Adds a child widget to the overlay container.
  @end{short}
  The child widget will be stacked on top of the main widget added with the
  @fun{gtk:container-add} function.

  The position at which the child widget is placed is determined from its
  @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}
  @see-function{gtk:container-add}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-add-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_reorder_overlay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_overlay_reorder_overlay" overlay-reorder-overlay) :void
 #+liber-documentation
 "@version{2025-06-13}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[child]{a overlaid @class{gtk:widget} child widget to move}
  @argument[position]{an integer for the new index for the child widget in the
    list of overlay children of the overlay container, starting from 0, if
    negative, indicates the end of the list}
  @begin{short}
    Moves a child widget to a new index in the list of overlay children.
  @end{short}
  The list contains overlays in the order that these were added to the overlay
  container.

  An index of the widget in the overlay children list determines which order
  the children are drawn if they overlap. The first child is drawn at the
  bottom. It also affects the default focus chain order.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}"
  (overlay (g:object overlay))
  (child (g:object widget))
  (position :int))

(export 'overlay-reorder-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_get_overlay_pass_through
;;; gtk_overlay_set_overlay_pass_through
;;; ----------------------------------------------------------------------------

;; Implemented as the child accessor gtk:overlay-child-pass-through

;;; --- End of file gtk3.overlay.lisp ------------------------------------------
