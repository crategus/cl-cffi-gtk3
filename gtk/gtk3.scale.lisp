;;; ----------------------------------------------------------------------------
;;; gtk3.scale.lisp
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
;;; GtkScale
;;;
;;;     A slider widget for selecting a value from a range
;;;
;;; Types and Values
;;;
;;;     GtkScale
;;;
;;; Functions
;;;
;;;     gtk_scale_new
;;;     gtk_scale_new_with_range
;;;     gtk_scale_set_digits
;;;     gtk_scale_set_draw_value
;;;     gtk_scale_set_has_origin
;;;     gtk_scale_set_value_pos
;;;     gtk_scale_get_digits
;;;     gtk_scale_get_draw_value
;;;     gtk_scale_get_has_origin
;;;     gtk_scale_get_value_pos
;;;     gtk_scale_get_layout
;;;     gtk_scale_get_layout_offsets
;;;     gtk_scale_add_mark
;;;     gtk_scale_clear_marks
;;;
;;; Properties
;;;
;;;     digits
;;;     draw-value
;;;     has-origin
;;;     value-pos
;;;
;;; Style Properties
;;;
;;;     slider-length
;;;     value-spacing
;;;
;;; Signals
;;;
;;;     format-value
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRange
;;;                 ╰── GtkScale
;;;                     ├── GtkHScale
;;;                     ╰── GtkVScale
;;;
;;; Implemented Interfaces
;;;
;;;     GtkScale implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkScale
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkScale" scale
  (:superclass range
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_scale_get_type")
  ((digits
    scale-digits
    "digits" "gint" t t)
   (draw-value
    scale-draw-value
    "draw-value" "gboolean" t t)
   (has-origin
    scale-has-origin
    "has-origin" "gboolean" t t)
   (value-pos
    scale-value-pos
    "value-pos" "GtkPositionType" t t)))

#+liber-documentation
(setf (documentation 'scale 'type)
 "@version{2025-07-02}
  @begin{short}
    The @class{gtk:scale} widget is a slider control used to select a numeric
    value.
  @end{short}
  To use it, you will probably want to investigate the methods on its base
  @class{gtk:range} class, in addition to the methods for the @class{gtk:scale}
  class itself. To set the value of a scale, you would normally use the
  @fun{gtk:range-value} function. To detect changes to the value, you would
  normally use the @sig[gtk:range]{value-changed} signal.

  @image[scales]{Figure: GtkScale}

  Note that using the same upper and lower bounds for the @class{gtk:scale}
  widget, through the @class{gtk:range} methods, will hide the slider itself.
  This is useful for applications that want to show an undeterminate value on
  the scale, without changing the layout of the application, such as movie or
  music players.
  @begin[GtkScale as GtkBuildable]{dictionary}
    The @class{gtk:scale} widget supports a custom @code{<marks>} element, which
    can contain multiple @code{<mark>} elements. The \"value\" and \"position\"
    attributes have the same meaning as the parameters of the
    @fun{gtk:scale-add-mark} function of the same name. If the element is not
    empty, its content is taken as the markup to show at the mark. It can be
    translated with the usual \"translatable\" and \"context\" attributes.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 scale[.fine-tune][.marks-before][.marks-after]
 ├── marks.top
 │   ├── mark
 │   │    ├── [label]
 │   │    ╰── indicator
 │   │
 │   ╰── mark
 ├── [value]
 ├── contents
 │   ╰── trough
 │       ├── slider
 │       ├── [highlight]
 │       ╰── [fill]
 ╰── marks.bottom
     ├── mark
     ┊    ├── indicator
     ┊    ╰── [label]
     ╰── mark
    @end{pre}
    The @class{gtk:scale} implementation has a main CSS node with name
    @code{scale} and a subnode for its contents, with subnodes named
    @code{trough} and @code{slider}. The main node gets the @code{.fine-tune}
    style class added when the scale is in 'fine-tuning' mode.

    If the scale has an origin, see the @fun{gtk:scale-has-origin} function,
    there is a subnode with name @code{highlight} below the trough node that
    is used for rendering the highlighted part of the trough.

    If the scale is showing a fill level, see the
    @fun{gtk:range-show-fill-level} function, there is a subnode with name
    @code{fill} below the trough node that is used for rendering the filled in
    part of the trough.

    If marks are present, there is a marks subnode before or after the contents
    node, below which each mark gets a node with name @code{mark}. The marks
    nodes get either the @code{.top} or @code{.bottom} style class.

    The mark node has a subnode named @code{indicator}. If the mark has text,
    it also has a subnode named @code{label}. When the mark is either above or
    left of the scale, the label subnode is the first when present. Otherwise,
    the indicator subnode is the first.

    The main CSS node gets the @code{marks-before} and/or @code{marks-after}
    style classes added depending on what marks are present.

    If the scale is displaying the value, see the @code{draw-value} property,
    there is subnode with name @code{value}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[scale:slider-length]{property}
      The @code{slider-length} style property of type @code{:int} (Read) @br{}
      Length of the slider of the scale. @br{}
      @em{Warning:} The @code{slider-length} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. Use min-height/min-width CSS properties on the slider element
      instead. The value of this style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 31
    @end{property}
    @begin[scale:value-spacing]{property}
      The @code{value-spacing} style property of type @code{:int} (Read) @br{}
      Space between value text and the slider/trough area. @br{}
      @em{Warning:} The @code{value-spacing} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. Use min-height/min-width CSS properties on the value element
      instead. The value of this style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 2
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[scale::format-value]{signal}
      @begin{pre}
lambda (scale value)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[scale]{The @class{gtk:scale} widget which received the signal.}
        @entry[value]{A double float with the value to format.}
        @entry[Returns]{String representing the value.}
      @end{simple-table}
      Signal which allows you to change how the scale value is displayed.
      Connect a signal handler which returns an allocated string representing
      @arg{value}. That string will then be used to display the scale's value.
      If no user provided handlers are installed, the value will be displayed
      on its own, rounded according to the value of the @slot[gtk:scale]{digits}
      property. Here is an example signal handler which displays a value 1.0 as
      with \"-->1.0<--\".
      @begin{pre}
static gchar*
format_value_callback (GtkScale *scale,
                       gdouble   value)
{
  return g_strdup_printf (\"-->%0.*g<--\",
                          gtk_scale_get_digits (scale), value);
@}
      @end{pre}
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:scale-new}
  @see-constructor{gtk:scale-new-with-range}
  @see-slot{gtk:scale-digits}
  @see-slot{gtk:scale-draw-value}
  @see-slot{gtk:scale-has-origin}
  @see-slot{gtk:scale-value-pos}
  @see-class{gtk:range}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:scale-digits -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "digits" 'scale) t)
 "The @code{digits} property of type @code{:int} (Read / Write) @br{}
  The number of decimal places that are displayed in the value. @br{}
  Allowed values: [-1, 64] @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'scale-digits)
      "Accessor"
      (documentation 'scale-digits 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:scale-digits object) => digits}
  @syntax{(setf (gtk:scale-digits object) digits)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[digits]{an integer for the number of decimal places to display,
    for example, use 1 to display 1.0, 2 to display 1.00, and so on}
  @begin{short}
    Accessor of the @slot[gtk:scale]{digits} slot of the @class{gtk:scale}
    class.
  @end{short}
  The @fun{gtk:scale-digits} function returns the number of decimal places that
  are displayed. The @setf{gtk:scale-digits} function sets the number of
  decimal places that are displayed in the value. Also causes the value of the
  adjustment to be rounded off to this number of @arg{digits}, so the retrieved
  value matches the value the user saw.
  @see-class{gtk:scale}")

;;; --- gtk:scale-draw-value ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw-value" 'scale) t)
 "The @code{draw-value} property of type @code{:boolean} (Read / Write) @br{}
  Whether the current value is displayed as a string next to the slider. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-draw-value)
      "Accessor"
      (documentation 'scale-draw-value 'function)
 "@version{#2023-03-24}
  @syntax{(gtk:scale-draw-value object) => draw-value}
  @syntax{(setf (gtk:scale-digits object) draw-value)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[draw-value]{@em{true} to draw the value}
  @begin{short}
    Accessor of the @slot[gtk:scale]{draw-value} slot of the @class{gtk:scale}
    class.
  @end{short}
  The @fun{gtk:scale-draw-value} function returns whether the current value is
  displayed as a string next to the slider. The
  @setf{gtk:scale-draw-value} function specifies whether the current value is
  displayed as a string next to the slider.
  @see-class{gtk:scale}")

;;; --- gtk:scale-has-origin ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-origin" 'scale) t)
 "The @code{has-origin} property of type @code{:boolean} (Read / Write) @br{}
  Whether the scale has an origin. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-has-origin)
      "Accessor"
      (documentation 'scale-has-origin 'function)
 "@version{#2023-03-24}
  @syntax{(gtk:scale-has-origin object) => has-origin}
  @syntax{(setf (gtk:scale-digits object) has-origin)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[has-origin]{@em{true} if the scale has an origin}
  @begin{short}
    Accessor of the @slot[gtk:scale]{has-origin} slot of the @class{gtk:scale}
    class.
  @end{short}
  The @fun{gtk:scale-has-origin} function returns whether the scale has an
  origin.

  If @arg{has-origin} is set to @em{true}, the default, the scale will
  highlight the part of the scale between the origin, bottom or left
  side, of the scale and the current value.
  @see-class{gtk:scale}")

;;; --- gtk:scale-value-pos ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value-pos" 'scale) t)
 "The @code{value-pos} property of type @sym{gtk:position-type} (Read / Write)
  @br{}
  The position in which the current value is displayed. @br{}
  Default value: @val[gtk:position-type]{:top}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-value-pos)
      "Accessor"
      (documentation 'scale-value-pos 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:scale-value-pos object) => pos}
  @syntax{(setf (gtk:scale-digits object) pos)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[pos]{a @sym{gtk:position-type} value for the position in which
    the current value is displayed}
  @begin{short}
    Accessor of the @slot[gtk:scale]{value-pos} slot of the @class{gtk:scale}
    class.
  @end{short}
  The @fun{gtk:scale-value-pos} function gets the position in which the current
  value is displayed. The @fun{gtk:scale-value-pos} function sets the position
  in which the current value is displayed.
  @see-class{gtk:scale}")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new
;;; ----------------------------------------------------------------------------

(declaim (inline scale-new))

(defun scale-new (orientation adjustment)
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[orientation]{a value of the @sym{gtk:orientation} enumeration}
  @argument[adjustment]{a @class{gtk:adjustment} object which sets the range
    of the scale, or @code{nil} to create a new adjustment}
  @return{The new @class{gtk:scale} widget.}
  @short{Creates a new scale widget.}
  @see-class{gtk:scale}"
  (make-instance 'scale
                 :orientation orientation
                 :adjustment adjustment))

(export 'scale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new_with_range
;;; ----------------------------------------------------------------------------

(declaim (inline scale-new-with-range))

(defun scale-new-with-range (orientation min max step)
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[orientation]{a value of the @sym{gtk:orientation} enumeration}
  @argument[min]{a number coerced to a double float for the minimum value}
  @argument[max]{a number coerced to a double float for the maximum value}
  @argument[step]{a number coerced to a double float for the step increment,
    tick size, used with keyboard shortcuts}
  @return{The new @class{gtk:scale} widget.}
  @begin{short}
    Creates a new scale widget with the given @arg{orientation} that lets the
    user input a number between @arg{min} and @arg{max}, including @arg{min}
    and @arg{max}, with the increment @arg{step}.
  @end{short}
  The @arg{step} argument must be nonzero. It is the distance the slider moves
  when using the arrow keys to adjust the scale value.

  Note that the way in which the precision is derived works best if the
  @arg{step} argument is a power of ten. If the resulting precision is not
  suitable for your needs, use the @fun{gtk:scale-digits} function to correct
  it.
  @see-class{gtk:scale}
  @see-function{gtk:scale-digits}"
  (make-instance 'scale
                 :orientation orientation
                 :adjustment (make-instance 'adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'scale-new-with-range)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_get_layout" scale-layout) (g:object pango-layout)
 #+liber-documentation
 "@version{#2023-03-24}
  @argument[scale]{a @class{gtk:scale} widget}
  @begin{return}
    The @class{pango-layout} instance for this scale, or @code{nil} if the
    @slot[gtk:scale]{draw-value} property is @code{nil}.
  @end{return}
  @short{Gets the Pango layout used to display the scale.}
  @see-class{gtk:scale}"
  (scale (g:object scale)))

(export 'scale-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout_offsets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_get_layout_offsets" %scale-get-layout-offsets) :void
  (scale (g:object scale))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun scale-layout-offsets (scale)
 #+liber-documentation
 "@version{#2023-03-24}
  @argument[scale]{a @class{gtk:scale} widget}
  @begin{return}
    @arg{x} -- an integer with the x offset of layout, or @code{nil} @br{}
    @arg{y} -- an integer with the y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the coordinates where the scale will draw the Pango layout
    representing the text in the scale.
  @end{short}
  Remember when using the @class{pango-layout} functions you need to convert
  to and from pixels using the @fun{pango-pixels} function or the
  @var{pango:+scale+} constant. If the @slot[gtk:scale]{draw-value} property
  is @code{nil}, the return values are undefined.
  @see-class{gtk:scale}
  @see-function{pango-pixels}
  @see-variable{pango:+scale+}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%scale-get-layout-offsets scale x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'scale-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_add_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_add_mark" %scale-add-mark) :void
  (scale (g:object scale))
  (value :double)
  (pos position-type)
  (markup :string))

(defun scale-add-mark (scale value pos markup)
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[scale]{a @class{gtk:scale} widget}
  @argument[value]{a number coerced to a double float for the value at which
    the mark is placed, must be between the lower and upper limits of the
    adjustment of the scale}
  @argument[position]{a value of the @sym{gtk:position-type} enumeration}
  @argument[markup]{a string for the text to be shown at the mark, using Pango
    markup, or @code{nil}}
  @begin{short}
    Adds a mark at value.
  @end{short}
  A mark is indicated visually by drawing a tick mark next to the scale, and
  GTK makes it easy for the user to position the scale exactly at the marks
  value. For a horizontal scale, @val[gtk:position-type]{:top} and
  @val[gtk:position-type]{:left} are drawn above the scale, anything else below.
  For a vertical scale, @val[gtk:position-type]{:left} and
  @val[gtk:position-type]{:top} are drawn to the left of the scale, anything
  else to the right.

  If markup is not @code{nil}, text is shown next to the tick mark.

  To remove marks from a scale, use the @fun{gtk:scale-clear-marks} function.
  @see-class{gtk:scale}
  @see-function{gtk:scale-clear-marks}"
  (%scale-add-mark scale (coerce value 'double-float) pos markup))

(export 'scale-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_clear_marks
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_clear_marks" scale-clear-marks) :void
 #+liber-documentation
 "@version{#2023-03-24}
  @argument[scale]{a @class{gtk:scale} widget}
  @begin{short}
    Removes any marks that have been added with the @fun{gtk:scale-add-mark}
    function.
  @end{short}
  @see-class{gtk:scale}
  @see-function{gtk:scale-add-mark}"
  (scale (g:object scale)))

(export 'scale-clear-marks)

;;; --- End of file gtk3.scale.lisp --------------------------------------------
