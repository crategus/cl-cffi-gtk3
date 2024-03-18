;;; ----------------------------------------------------------------------------
;;; gtk3.tooltip.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkTooltip
;;;
;;;     Add tips to your widgets
;;;
;;; Types and Values
;;;
;;;     GtkTooltip
;;;
;;; Functions
;;;
;;;     gtk_tooltip_set_markup
;;;     gtk_tooltip_set_text
;;;     gtk_tooltip_set_icon
;;;     gtk_tooltip_set_icon_from_stock
;;;     gtk_tooltip_set_icon_from_icon_name
;;;     gtk_tooltip_set_icon_from_gicon
;;;     gtk_tooltip_set_custom
;;;     gtk_tooltip_trigger_tooltip_query
;;;     gtk_tooltip_set_tip_area
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTooltip
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTooltip
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTooltip" tooltip
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_tooltip_get_type")
  nil)

#+liber-documentation
(setf (documentation 'tooltip 'type)
 "@version{#2023-2-23}
  @short{Add tips to your widgets.}
  Basic tooltips can be realized simply by using the
  @fun{gtk:widget-tooltip-text} or @fun{gtk:widget-tooltip-markup} functions
  without any explicit tooltip object.

  When you need a tooltip with a little more fancy contents, like adding an
  image, or you want the tooltip to have different contents per
  @class{gtk:tree-view} row or cell, you will have to do a little more work:
  @begin{itemize}
    @begin{item}
      Set the @slot[gtk:widget]{has-tooltip} property to @em{true}, this will
      make GTK monitor the widget for motion and related events which are
      needed to determine when and where to show a tooltip.
    @end{item}
    @begin{item}
      Connect to the \"query-tooltip\" signal. The signal will be emitted when
      a tooltip is supposed to be shown. One of the arguments passed to the
      signal handler is a @class{gtk:tooltip} object. This is the object that we
      are about to display as a tooltip, and can be manipulated in your callback
      function using functions like the @fun{gtk:tooltip-set-icon} function.
      There are functions for setting the markup of the tooltip, setting an
      image from a stock icon, or even putting in a custom widget.
    @end{item}
    @begin{item}
      Return @em{true} from your query-tooltip handler. This causes the tooltip
      to be show. If you return @em{false}, it will not be shown.
    @end{item}
  @end{itemize}
  In the probably rare case where you want to have even more control over the
  tooltip that is about to be shown, you can set your own @class{gtk:window}
  widget which will be used as tooltip window. This works as follows:
  @begin{itemize}
    @begin{item}
      Set the @slot[gtk:widget]{has-tooltip} property and connect to the
      \"query-tooltip\" signal as before.
    @end{item}
    @begin{item}
      Use the @fun{gtk:widget-tooltip-window} function to set a
      @class{gtk:window} widget created by you as tooltip window.
    @end{item}
    @begin{item}
      In the \"query-tooltip\" callback you can access your window using the
       @fun{gtk:widget-tooltip-window} function and manipulate as you wish.
      The semantics of the return value are exactly as before, return @em{true}
      to show the window, @em{false} to not show it.
    @end{item}
  @end{itemize}
  @see-class{gtk:tree-view}
  @see-class{gtk:window}
  @see-function{gtk:widget-tooltip-text}
  @see-function{gtk:widget-tooltip-markup}
  @see-function{gtk:widget-tooltip-window}
  @see-function{gtk:tooltip-set-icon}")

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_markup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_markup" tooltip-set-markup) :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[markup]{a markup string, see Pango markup format, or @code{nil}}
  @begin{short}
    Sets the text of the tooltip to be @arg{markup}, which is marked up
    with the Pango text markup language.
  @end{short}
  If @arg{markup} is @code{nil}, the label will be hidden.
  @see-class{gtk:tooltip}
  @see-function{gtk:tooltip-set-text}"
  (tooltip (g:object tooltip))
  (markup :string))

(export 'tooltip-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_text ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_text" tooltip-set-text) :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[text]{a text string or @code{nil}}
  @begin{short}
    Sets the text of the tooltip to be @arg{text}.
  @end{short}
  If @arg{text} is @code{nil}, the label will be hidden. See also the
  @fun{gtk:tooltip-set-markup} function.
  @see-class{gtk:tooltip}
  @see-function{gtk:tooltip-set-markup}"
  (tooltip (g:object tooltip))
  (text :string))

(export 'tooltip-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon" tooltip-set-icon) :void
 #+liber-documentation
 "@version{#2023-2-12}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object, or @code{nil}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be
    @arg{pixbuf}.
  @end{short}
  If the @arg{pixbuf} argument is @code{nil}, the image will be hidden.
  @see-class{gtk:tooltip}
  @see-class{gdk-pixbuf:pixbuf}"
  (tooltip (g:object tooltip))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'tooltip-set-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_stock ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon_from_stock" tooltip-set-icon-from-stock)
    :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[stockid]{a string with the stock ID, or @code{nil}}
  @argument[size]{a @symbol{gtk:icon-size} value for the icon size}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the
    stock item indicated by @arg{stockid} with the size indicated by
    @arg{size}.
  @end{short}
  If @arg{stockid} is @code{nil}, the image will be hidden.
  @begin[Warning]{dictionary}
    The @fun{gtk:tooltip-set-icon-from-stock} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:tooltip-set-icon-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:tooltip}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:tooltip-set-icon-from-icon-name}"
  (tooltip (g:object tooltip))
  (stockid :string)
  (size icon-size))

(export 'tooltip-set-icon-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_icon_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon_from_icon_name"
               tooltip-set-icon-from-icon-name) :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[name]{a string with the icon name, or @code{nil}}
  @argument[size]{a @symbol{gtk:icon-size} value for the icon size}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the icon
    indicated by @arg{name} with the size indicated by @arg{size}.
  @end{short}
  If @arg{name} is @code{nil}, the image will be hidden.
  @see-class{gtk:tooltip}
  @see-symbol{gtk:icon-size}"
  (tooltip (g:object tooltip))
  (name :string)
  (size icon-size))

(export 'tooltip-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon_from_gicon" tooltip-set-icon-from-gicon)
    :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} widget}
  @argument[gicon]{a @class{g:icon} object representing the icon, or @code{nil}}
  @argument[size]{a @symbol{gtk:icon-size} value with the icon size}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the
    icon indicated by @arg{gicon} with the size indicated by @arg{size}.
  @end{short}
  If @arg{gicon} is @code{nil}, the image will be hidden.
  @see-class{gtk:tooltip}
  @see-class{g:icon}
  @see-symbol{gtk:icon-size}"
  (tooltip (g:object tooltip))
  (gicon (g:object g:icon))
  (size icon-size))

(export 'tooltip-set-icon-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_custom ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_custom" tooltip-set-custom) :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[widget]{a @class{gtk:widget} widget, or @code{nil} to unset the old
    custom widget}
  @begin{short}
    Replaces the widget packed into the tooltip with a custom widget.
  @end{short}
  @arg{widget} does not get destroyed when the @arg{tooltip} goes away. By
  default a box with a @class{gtk:image} and @class{gtk:label} widget is
  embedded in the tooltip, which can be configured using the
  @fun{gtk:tooltip-set-markup} and @fun{gtk:tooltip-set-icon} functions.
  @see-class{gtk:tooltip}
  @see-class{gtk:widget}
  @see-class{gtk:image}
  @see-class{gtk:label}
  @see-function{gtk:tooltip-set-markup}
  @see-function{gtk:tooltip-set-icon}"
  (tooltip (g:object tooltip))
  (widget (g:object widget)))

(export 'tooltip-set-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_trigger_tooltip_query ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_trigger_tooltip_query"
               tooltip-trigger-tooltip-query) :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Triggers a new tooltip query on @arg{display}, in order to update the
    current visible tooltip, or to show/hide the current tooltip.
  @end{short}
  This function is useful to call when, for example, the state of the widget
  changed by a key press.
  @see-class{gtk:tooltip}
  @see-class{gdk:display}
  @see-function{gtk:widget-trigger-tooltip-query}"
  (display (g:object gdk:display)))

(export 'tooltip-trigger-tooltip-query)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_tip_area ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_tip_area" tooltip-set-tip-area) :void
 #+liber-documentation
 "@version{#2023-2-23}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[rectangle]{a @class{gdk:rectangle} instance}
  @begin{short}
    Sets the area of the widget, where the contents of the tooltip apply, to be
    @arg{rectangle} in widget coordinates.
  @end{short}
  This is especially useful for properly setting tooltips on
  @class{gtk:tree-view} rows and cells, @class{gtk:icon-view} widgets, etc.

  For setting tooltips on the @class{gtk:tree-view} widget, please refer to the
  convenience @fun{gtk:tree-view-set-tooltip-row} and
  @fun{gtk:tree-view-set-tooltip-cell} functions for this.
  @see-class{gtk:tooltip}
  @see-class{gdk:rectangle}
  @see-class{gtk:tree-view}
  @see-class{gtk:icon-view}
  @see-function{gtk:tree-view-set-tooltip-row}
  @see-function{gtk:tree-view-set-tooltip-cell}"
  (tooltip (g:object tooltip))
  (rectangle (g:boxed gdk:rectangle)))

(export 'tooltip-set-tip-area)

;;; ---- End of file gtk3.tooltip.lisp -----------------------------------------
