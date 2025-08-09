;;; ----------------------------------------------------------------------------
;;; gtk3.scrollbar.lisp
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
;;; GtkScrollbar
;;;
;;;     A Scrollbar
;;;
;;; Types and Values
;;;
;;;     GtkScrollbar
;;;
;;; Functions
;;;
;;;     gtk_scrollbar_new
;;;
;;; Style Properties
;;;
;;;     fixed-slider-length
;;;     has-backward-stepper
;;;     has-forward-stepper
;;;     has-secondary-backward-stepper
;;;     has-secondary-forward-stepper
;;;     min-slider-length
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRange
;;;                 ╰── GtkScrollbar
;;;                     ├── GtkHScrollbar
;;;                     ╰── GtkVScrollbar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScrollbar implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScrollbar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkScrollbar" scrollbar
  (:superclass range
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_scrollbar_get_type")
  nil)

#+liber-documentation
(setf (documentation 'scrollbar 'type)
 "@version{#2023-3-24}
  @begin{short}
    The @class{gtk:scrollbar} widget is a horizontal or vertical scrollbar,
    depending on the value of the @slot[gtk:orientable]{orientation} property.
  @end{short}

  @image[scrollbar]{Figure: GtkScrollbar}

  The position of the thumb in a scrollbar is controlled by the scroll
  adjustments. See the @class{gtk:adjustment} object for the properties in an
  adjustment - for the @class{gtk:scrollbar} widget, the
  @slot[gtk:adjustment]{value} property represents the position of the
  scrollbar, which must be between the @slot[gtk:adjustment]{lower} value and
  the @code{(@slot[gtk:adjustment]{upper} - @slot[gtk:adjustment]{page-size})}
  difference. The @slot[gtk:adjustment]{page-size} property represents the size
  of the visible scrollable area. The @slot[gtk:adjustment]{step-increment} and
  @slot[gtk:adjustment]{page-increment} properties are used when the user asks
  to step down, using the small stepper arrows, or page down, using for example
  the PageDown key.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 scrollbar[.fine-tune]
 ╰── contents
     ├── [button.up]
     ├── [button.down]
     ├── trough
     │   ╰── slider
     ├── [button.up]
     ╰── [button.down]
    @end{pre}
    The @class{gtk:scrollbar} implementation has a main CSS node with name
    @code{scrollbar} and a subnode for its contents, with subnodes named
    @code{trough} and @code{slider}.

    The main node gets the @code{.fine-tune} style class added when the
    scrollbar is in \"fine-tuning\" mode.

    If steppers are enabled, they are represented by up to four additional
    subnodes with name @code{button}. These get the @code{.up} and @code{.down}
    style classes to indicate in which direction they are moving.

    Other style classes that may be added to scrollbars inside the
    @class{gtk:scrolled-window} implmentation include the @code{.left},
    @code{.right}, @code{.top}, @code{.bottom} positional classes and style
    classes related to overlay scrolling @code{.overlay-indicator},
    @code{.dragging}, @code{.hovering}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[scrollbar:fixed-slider-length]{property}
      The @code{fixed-slider-length} style property of type @code{:boolean}
      (Read) @br{}
      Do not change slider size, just lock it to the minimum length. @br{}
      Default value: @em{false}
    @end{property}
    @begin[scrollbar:has-backward-stepper]{property}
      The @code{has-backward-stepper} style property of type @code{:boolean}
      (Read)@br{}
      Display the standard backward arrow button. @br{}
      Default value: @em{true}
    @end{property}
    @begin[scrollbar:has-forward-stepper]{property}
      The @code{has-forward-stepper} style property of type @code{:boolean}
      (Read) @br{}
      Display the standard forward arrow button. @br{}
      Default value: @em{true}
    @end{property}
    @begin[scrollbar:has-secondary-backward-stepper]{property}
      The @code{has-secondary-backward-stepper} style property of type
      @code{:boolean} (Read) @br{}
      Display a second backward arrow button on the opposite end of the
      scrollbar. @br{}
      Default value: @em{false}
    @end{property}
    @begin[scrollbar:has-secondary-forward-stepper]{property}
      The @code{has-secondary-forward-stepper} style property of type
      @code{:boolean} (Read) @br{}
      Display a second forward arrow button on the opposite end of the
      scrollbar. @br{}
      Default value: @em{false}
    @end{property}
    @begin[scrollbar:min-slider-length]{property}
      The @code{min-slider-length} style property of type @code{:int} (Read)
      @br{}
      Minimum length of scrollbar slider. @br{}
      @em{Warning:} The @code{min-slider-length} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. Use min-height/min-width CSS properties on the slider element
      instead. The value of this style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 21
    @end{property}
  @end{dictionary}
  @see-constructor{gtk:scrollbar-new}
  @see-class{gtk:adjustment}
  @see-class{gtk:scrolled-window}")

;;; ----------------------------------------------------------------------------
;;; gtk_scrollbar_new
;;; ----------------------------------------------------------------------------

(declaim (inline scrollbar-new))

(defun scrollbar-new (orientation &optional (adjustment nil))
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[orientation]{a @sym{gtk:orientation} value for the orientation of
    the scrollbar}
  @argument[adjustment]{an optional @class{gtk:adjustment} object to use,
    the default is to create a new adjustment}
  @return{The new @class{gtk:scrollbar} widget.}
  @short{Creates a new scrollbar with the given @arg{orientation}.}
  @see-class{gtk:scrollbar}
  @see-class{gtk:adjustment}
  @see-symbol{gtk:orientation}"
  (make-instance 'scrollbar
                 :orientation orientation
                 :adjustment adjustment))

(export 'scrollbar-new)

;;; --- End of file gtk3.scrollbar.lisp ----------------------------------------
