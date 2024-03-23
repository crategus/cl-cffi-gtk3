;;; ----------------------------------------------------------------------------
;;; gtk3.style-context.lisp
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
;;; GtkStyleContext
;;;
;;;     Rendering UI elements
;;;
;;; Types and Values
;;;
;;;     GtkStyleContext
;;;
;;;     GtkJunctionSides
;;;     GtkRegionFlags                                  to gtk3.widget-path.lisp
;;;     GtkStyleContextPrintFlags
;;;     GtkBorder
;;;     GtkBorderStyle
;;;
;;; Functions
;;;
;;;     gtk_style_context_new
;;;     gtk_style_context_add_provider
;;;     gtk_style_context_add_provider_for_screen
;;;     gtk_style_context_get                              not needed
;;;     gtk_style_context_get_direction                    Accessor
;;;     gtk_style_context_get_junction_sides
;;;     gtk_style_context_get_parent                       Accessor
;;;     gtk_style_context_get_path
;;;     gtk_style_context_get_property
;;;     gtk_style_context_get_screen                       Accessor
;;;     gtk_style_context_get_frame_clock
;;;     gtk_style_context_get_state
;;;     gtk_style_context_get_style                        not needed
;;;     gtk_style_context_get_style_property
;;;     gtk_style_context_get_style_valist                 not needed
;;;     gtk_style_context_get_valist                       not needed
;;;     gtk_style_context_get_section
;;;     gtk_style_context_get_color
;;;     gtk_style_context_get_background_color
;;;     gtk_style_context_get_border_color
;;;     gtk_style_context_get_border
;;;     gtk_style_context_get_padding
;;;     gtk_style_context_get_margin
;;;     gtk_style_context_get_font
;;;     gtk_style_context_invalidate
;;;     gtk_style_context_state_is_running                 not implemented
;;;     gtk_style_context_lookup_color
;;;     gtk_style_context_lookup_icon_set
;;;     gtk_style_context_notify_state_change              not exported
;;;     gtk_style_context_pop_animatable_region            not exported
;;;     gtk_style_context_push_animatable_region           not exported
;;;     gtk_style_context_cancel_animations                not implemented
;;;     gtk_style_context_scroll_animations                not implemented
;;;     gtk_style_context_remove_provider
;;;     gtk_style_context_remove_provider_for_screen
;;;     gtk_style_context_reset_widgets
;;;     gtk_style_context_set_background
;;;     gtk_style_context_restore
;;;     gtk_style_context_save
;;;     gtk_style_context_set_direction                    Accessor
;;;     gtk_style_context_set_junction_sides
;;;     gtk_style_context_set_parent                       Accessor
;;;     gtk_style_context_set_path
;;;     gtk_style_context_add_class
;;;     gtk_style_context_remove_class
;;;     gtk_style_context_has_class
;;;     gtk_style_context_list_classes
;;;     gtk_style_context_add_region
;;;     gtk_style_context_remove_region
;;;     gtk_style_context_has_region
;;;     gtk_style_context_list_regions
;;;     gtk_style_context_set_screen                       Accessor
;;;     gtk_style_context_set_frame_clock
;;;     gtk_style_context_set_state
;;;     gtk_style_context_set_scale
;;;     gtk_style_context_get_scale
;;;     gtk_style_context_to_string
;;;
;;;     GtkBorder
;;;
;;;     gtk_border_new
;;;     gtk_border_copy
;;;     gtk_border_free
;;;
;;;     gtk_render_arrow
;;;     gtk_render_background
;;;     gtk_render_background_get_clip
;;;     gtk_render_check
;;;     gtk_render_expander
;;;     gtk_render_extension
;;;     gtk_render_focus
;;;     gtk_render_frame
;;;     gtk_render_frame_gap
;;;     gtk_render_handle
;;;     gtk_render_layout
;;;     gtk_render_line
;;;     gtk_render_option
;;;     gtk_render_slider
;;;     gtk_render_activity
;;;     gtk_render_icon_pixbuf
;;;     gtk_render_icon_surface
;;;     gtk_render_icon
;;;     gtk_render_insertion_cursor
;;;
;;; Properties
;;;
;;;     direction
;;;     paint-clock
;;;     parent
;;;     screen
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkBorder
;;;
;;;     GObject
;;;     ╰── GtkStyleContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; Search a better place
(glib-init:at-init ()
  (cffi:foreign-funcall "gtk_ui_manager_get_type" :size))

;;; ----------------------------------------------------------------------------
;;; enum GtkJunctionSides
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GtkJunctionSides" junction-sides
  (:export t
   :type-initializer "gtk_junction_sides_get_type")
  (:none 0)
  (:corner-topleft #.(ash 1 0))
  (:corner-topright #.(ash 1 1))
  (:corner-bottomleft #.(ash 1 2))
  (:corner-bottomright #.(ash 1 3))
  (:top 3)
  (:left 5)
  (:bottom 6)
  (:right 10))

#+liber-documentation
(setf (liber:alias-for-symbol 'junction-sides)
      "GFlags"
      (liber:symbol-documentation 'junction-sides)
 "@version{#2024-3-21}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-flags \"GtkJunctionSides\" junction-sides
  (:export t
   :type-initializer \"gtk_junction_sides_get_type\")
  (:none 0)
  (:corner-topleft #.(ash 1 0))
  (:corner-topright #.(ash 1 1))
  (:corner-bottomleft #.(ash 1 2))
  (:corner-bottomright #.(ash 1 3))
  (:top 3)
  (:left 5)
  (:bottom 6)
  (:right 10))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No junctions.}
      @entry[:corner-topleft]{Element connects on the top-left corner.}
      @entry[:corner-topright]{Element connects on the top-right corner.}
      @entry[:corner-bottomleft]{Element connects on the bottom-left corner.}
      @entry[:corner-bottomright]{Element connects on the bottom-right corner.}
      @entry[:top]{Element connects on the top side.}
      @entry[:bottom]{Element connects on the bottom side.}
      @entry[:left]{Element connects on the left side.}
      @entry[:right]{Element connects on the right side.}
    @end{table}
  @end{values}
  @begin{short}
    Describes how a rendered element connects to adjacent elements.
  @end{short}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; enum GtkStyleContextPrintFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GtkStyleContextPrintFlags" style-context-print-flags
  (:export t
   :type-initializer "gtk_style_context_print_flags_get_type")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1)))

#+liber-documentation
(setf (liber:alias-for-symbol 'style-context-print-flags)
      "GFlags"
      (liber:symbol-documentation 'style-context-print-flags)
 "@version{#2024-3-21}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-flags \"GtkStyleContextPrintFlags\" style-context-print-flags
  (:export t
   :type-initializer \"gtk_style_context_print_flags_get_type\")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1)))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{}
      @entry[:recurse]{Print the entire tree of CSS nodes starting at the node
        of the style context.}
      @entry[:show-style]{Show the values of the CSS properties for each node.}
    @end{table}
  @end{values}
  @begin{short}
    Flags that modify the behavior of the @fun{gtk:style-context-to-string}
    function.
  @end{short}
  New values may be added to this enumeration.
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-to-string}")

;;; ----------------------------------------------------------------------------
;;; enum GtkBorderStyle
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkBorderStyle" border-style
  (:export t
   :type-initializer "gtk_border_style_get_type")
  :none
  :solid
  :inset
  :outset
  :hidden
  :dotted
  :dashed
  :double
  :groove
  :ridge)

#+liber-documentation
(setf (liber:alias-for-symbol 'border-style)
      "GEnum"
      (liber:symbol-documentation 'border-style)
 "@version{#2024-3-22}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkBorderStyle\" border-style
  (:export t
   :type-initializer \"gtk_border_style_get_type\")
  :none
  :solid
  :inset
  :outset
  :hidden
  :dotted
  :dashed
  :double
  :groove
  :ridge)
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No visible border.}
      @entry[:solid]{A single line segment.}
      @entry[:inset]{Looks as if the content is sunken into the canvas.}
      @entry[:outset]{Looks as if the content is coming out of the canvas.}
      @entry[:hidden]{Same as the @code{:none} value.}
      @entry[:dotted]{A series of round dots.}
      @entry[:dashed]{A series of square-ended dashes.}
      @entry[:double]{Two parrallel lines with some space between them.}
      @entry[:groove]{Looks as if it were carved in the canvas.}
      @entry[:ridge]{Looks as if it were coming out of the canvas.}
    @end{table}
  @end{values}
  @begin{short}
    Describes how the border of a UI element should be rendered.
  @end{short}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; struct GtkBorder
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-cstruct border "GtkBorder"
  (:export t
   :type-initializer "gtk_border_get_type")
  (left   :int16 :initform 0)
  (right  :int16 :initform 0)
  (top    :int16 :initform 0)
  (bottom :int16 :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'border)
      "GBoxed"
      (documentation 'border 'type)
 "@version{#2023-3-27}
  @begin{short}
    A structure that specifies a border around a rectangular area that can be
    of different width on each side.
  @end{short}
  @begin{pre}
(gobject:define-g-boxed-cstruct border \"GtkBorder\"
  (:export t
   :type-initializer \"gtk_border_get_type\")
  (left   :int16 :initform 0)
  (right  :int16 :initform 0)
  (top    :int16 :initform 0)
  (bottom :int16 :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[left]{The width of the left border.}
    @entry[right]{The width of the right border.}
    @entry[top]{The width of the top border.}
    @entry[bottom]{The width of the bottom border.}
  @end{table}
  @see-constructor{gtk:border-new}
  @see-slot{gtk:border-left}
  @see-slot{gtk:border-right}
  @see-slot{gtk:border-top}
  @see-slot{gtk:border-bottom}")

;;; ----------------------------------------------------------------------------
;;; gtk_border_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline border-new))

(defun border-new (&key (left 0) (right 0) (top 0) (bottom 0))
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[left]{an integer with the width of the left border}
  @argument[right]{an integer with the width of the right border}
  @argument[top]{an integer with the width of the top border}
  @argument[bottom]{an integer with the width of the bottom border}
  @return{A newly allocated @class{gtk:border} instance.}
  @begin{short}
    Allocates a new @class{gtk:border} instance and initializes its elements.
  @end{short}
  @see-class{gtk:border}"
  (make-border :left left :right right :top top :bottom bottom))

(export 'border-new)

;;; ----------------------------------------------------------------------------
;;; gtk_border_copy ()
;;; ----------------------------------------------------------------------------

(declaim (inline border-copy))

(defun border-copy (border)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[border]{a @class{gtk:border} instance}
  @return{A copy of @arg{border}.}
  @short{Copies a @class{gtk:border} instance.}
  @see-class{gtk:border}"
  (copy-border border))

(export 'border-copy)

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkBorder
;;; ----------------------------------------------------------------------------

;;; --- gtk:border-left --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-left)
      "Accessor"
      (documentation 'border-left 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:border-left instance) => left}
  @syntax{(setf gtk:border-left instance) left)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[left]{an integer with the width of the left border}
  @begin{short}
    Accessor of the @code{left} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-left)

;;; --- gtk:border-right -------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-right)
      "Accessor"
      (documentation 'border-right 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:border-right instance) => right}
  @syntax{(setf gtk:border-right instance) right)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[right]{an integer with the width of the right border}
  @begin{short}
    Accessor of the @code{right} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-right)

;;; --- gtk:border-top ---------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-top)
      "Accessor"
      (documentation 'border-top 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:border-top instance) => top}
  @syntax{(setf gtk:border-top instance) top)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[top]{an integer with the width of the top border}
  @begin{short}
    Accessor of the @code{top} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-top)

;;; --- gtk:border-bottom ------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-bottom)
      "Accessor"
      (documentation 'border-bottom 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:border-top instance) => bottom}
  @syntax{(setf gtk:border-top instance) bottom)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[bottom]{an integer with the width of the bottom border}
  @begin{short}
    Accessor of the @code{bottom} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-bottom)

;;; ----------------------------------------------------------------------------
;;; GtkStyleContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkStyleContext" style-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_style_context_get_type")
  ((direction
    style-context-direction
    "direction" "GtkTextDirection" t t)
   (paint-clock
    style-context-paint-clock
    "paint-clock" "GdkFrameClock" t t)
   (parent
    style-context-parent
    "parent" "GtkStyleContext" t t)
   (screen
    style-context-screen
    "screen" "GdkScreen" t t)))

#+liber-documentation
(setf (documentation 'style-context 'type)
 "@version{#2023-3-27}
  @begin{short}
    The @class{gtk:style-context} object stores styling information affecting a
    widget defined by a @class{gtk:widget-path} instance.
  @end{short}

  In order to construct the final style information, the
  @class{gtk:style-context} object queries information from all attached
  @class{gtk:style-provider} objects. Style providers can be either attached
  explicitly to the style context through the
  @fun{gtk:style-context-add-provider} function, or to the screen through the
  @fun{gtk:style-context-add-provider-for-screen} function. The resulting style
  is a combination of all information of the style provider in priority order.

  For GTK widgets, any @class{gtk:style-context} object returned by the
  @fun{gtk:widget-style-context} function will already have a
  @class{gtk:widget-path} instance, a @class{gdk:screen} object and a text
  direction information set. The style context will be also updated
  automatically if any of these settings change on the widget.

  If you are using the theming layer standalone, you will need to set a widget
  path and a screen yourself to the created style context through the
  @fun{gtk:style-context-path} and @fun{gtk:style-context-screen} functions.
  See the \"Custom Drawing\" example in the GTK Lisp Demo.

  @subheading{Style Classes}
  Widgets can add style classes to their style context, which can be used to
  associate different styles by class. The documentation for individual widgets
  lists which style classes it uses itself, and which style classes may be added
  by applications to affect their appearance.

  @subheading{Style Regions}
  Widgets can also add regions with flags to their style context. This feature
  is deprecated and will be removed in a future GTK update. Please use style
  classes instead.

  @subheading{Custom styling in UI libraries and applications}
  If you are developing a library with custom widgets that render differently
  than standard components, you may need to add a @class{gtk:style-provider}
  object yourself with the @var{+gtk-priority-fallback+} priority, either a
  @class{gtk:css-provider} object or a custom object implementing the
  @class{gtk:style-provider} interface. This way themes may still attempt to
  style your UI elements in a different way if needed so.

  If you are using custom styling on an application, you probably want then to
  make your style information prevail to the style information of the theme, so
  you must use a @class{gtk:style-provider} object with the
  @var{+gtk-priority-application+} priority. Keep in mind that the user settings
  in @file{XDG_CONFIG_HOME/gtk-3.0/gtk.css} will still take precedence over your
  changes, as it uses the @var{+gtk-priority-user+} priority.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (context)    :run-first
      @end{pre}
      The signal is emitted when there is a change in the style context. For a
      style context returned by the @fun{gtk:widget-style-context} function,
      the @code{\"style-updated\"} signal of the @class{gtk:widget} class might
      be more convenient to use. The signal is useful when using the theming
      layer standalone.
      @begin[code]{table}
        @entry[context]{The @class{gtk:style-context} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:style-context-new}
  @see-slot{gtk:style-context-direction}
  @see-slot{gtk:style-context-paint-clock}
  @see-slot{gtk:style-context-parent}
  @see-slot{gtk:style-context-screen}
  @see-class{gtk:style-provider}
  @see-class{gtk:css-provider}
  @see-class{gtk:widget-path}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:style-context-direction --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "direction" 'style-context) t)
 "The @code{direction} property of type @symbol{gtk:text-direction}
  (Read / Write) @br{}
  The text direction of the style context. @br{}
  Default value: @code{:ltr}")

#+liber-documentation
(setf (liber:alias-for-function 'style-context-direction)
      "Accessor"
      (documentation 'style-context-direction 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-direction object) => direction}
  @syntax{(setf (gtk:style-context-direction object) direction)}
  @argument[object]{a @class{gtk:style-context} object}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:style-context]{direction} slot of the
    @class{gtk:style-context} class.
  @end{short}
  The @fun{gtk:style-context-direction} function returns the widget direction
  used for rendering. The @setf{gtk:style-context-direction} function sets the
  reading direction.

  If you are using a style context returned from the
  @fun{gtk:widget-style-context} function, you do not need to call this
  yourself.
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-direction} function has been deprecated since
    version 3.8 and should not be used in newly written code. Use the
    @fun{gtk:style-context-state} function and check for the @code{:dir-ltr}
    and @code{:dir-rtl} values instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:style-context-state}
  @see-function{gtk:widget-style-context}")

;;; --- gtk:style-context-paint-clock ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paint-clock" 'style-context) t)
 "The @code{paint-clock} property of type @class{gdk:frame-clock} (Read / Write)
  @br{}
  The associated frame clock of the style context.")

#+liber-documentation
(setf (liber:alias-for-function 'style-context-paint-clock)
      "Accessor"
      (documentation 'style-context-paint-clock 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-paint-clock object) => clock}
  @syntax{(setf (gtk:style-context-paint-clock object) clock)}
  @argument[object]{a @class{gtk:style-context} object}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{short}
    Accessor of the @slot[gtk:style-context]{paint-clock} slot of the
    @class{gtk:style-context} class.
  @end{short}
  The associated frame clock of the style context.
  @see-class{gtk:style-context}
  @see-class{gdk:frame-clock}")

;;; --- gtk:style-context-parent -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "parent" 'style-context) t)
 "The @code{parent} property of type @class{gtk:style-context} (Read / Write)
  @br{}
  The parent of the style context.")

#+liber-documentation
(setf (liber:alias-for-function 'style-context-parent)
      "Accessor"
      (documentation 'style-context-parent 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-parent object) => parent}
  @syntax{(setf (gtk:style-context-parent object) parent)}
  @argument[object]{a @class{gtk:style-context} object}
  @argument[parent]{a @class{gtk:style-context} parent object or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:style-context]{parent} slot of the
    @class{gtk:style-context} class.
  @end{short}
  The @fun{gtk:style-context-parent} function gets the parent style context.
  The @setf{gtk:style-context-parent} function sets the parent style context.

  The parent style context is used to implement inheritance of properties. If
  you are using a style context returned from the @fun{gtk:widget-style-context}
  function, the parent will be set for you.
  @see-class{gtk:style-context}
  @see-function{gtk:widget-style-context}")

;;; --- gtk:style-context-screen -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "screen" 'style-context) t)
 "The @code{screen} property of type @class{gdk:screen} (Read / Write) @br{}
  The associated screen of the style context.")

#+liber-documentation
(setf (liber:alias-for-function 'style-context-screen)
      "Accessor"
      (documentation 'style-context-screen 'function)
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-screen object) => screen}
  @syntax{(setf (gtk:style-context-screen object) screen)}
  @argument[object]{a @class{gtk:style-context} object}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    Accessor of the @slot[gtk:style-context]{screen} slot of the
    @class{gtk:style-context} class.
  @end{short}
  The @fun{gtk:style-context-screen} function returns the screen to which the
  style context is attached. The @setf{gtk:style-context-screen} function
  attaches the style context to the given screen.

  The screen is used to add style information from 'global' style providers,
  such as the @class{gtk:settings} object of the screen.

  If you are using a style context returned from the
  @fun{gtk:widget-style-context} function, you do not need to call this
  yourself.
  @see-class{gtk:style-context}
  @see-class{gdk:screen}
  @see-class{gtk:settings}
  @see-function{gtk:widget-style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_new ()
;;; ----------------------------------------------------------------------------

(defun style-context-new ()
 #+liber-documentation
 "@version{#2023-3-27}
  @return{A newly created @class{gtk:style-context} object.}
  @begin{short}
    Creates a standalone style context object.
  @end{short}
  This style context will not be attached to any widget, so you may want to
  call the @fun{gtk:style-context-path} function yourself.
  @begin{notes}
    This function is only useful when using the theming layer separated from
    GTK, if you are using a style context to theme widgets, use the
    @fun{gtk:widget-style-context} function in order to get a style context
    ready to theme the widget.
  @end{notes}
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-path}
  @see-function{gtk:widget-style-context}"
  (make-instance 'style-context))

(export 'style-context-new)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_add_provider" style-context-add-provider)
    :void
 #+liber-documentation
 "@version{2023-12-30}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an unsigned integer with the priority of the style
    provider}
  @begin{short}
    Adds a style provider to the style context, to be used in style
    construction.
  @end{short}
  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{gtk:+gtk-priority-fallback+} and @var{gtk:+gtk-priority-user+}
  priorities.
  @begin{notes}
    If both priorities are the same, a style provider object added through this
    function takes precedence over another added through the
    @fun{gtk:style-context-add-provider-for-screen} function.
  @end{notes}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider-for-screen}"
  (context (g:object style-context))
  (provider (g:object style-provider))
  (priority :uint))

(export 'style-context-add-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider_for_screen ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_add_provider_for_screen"
               style-context-add-provider-for-screen) :void
 #+liber-documentation
 "@version{2024-1-2}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an unsigned integer with the priority of the style
    provider}
  @begin{short}
    Adds a global style provider to the screen, which will be used in style
    construction for all style contexts under the screen.
  @end{short}
  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{+gtk-priority-fallback+} and @var{+gtk-priority-user+} priorities.

  GTK uses this to make styling information from the @class{gtk:settings}
  object available.
  @begin{notes}
    If both priorities are the same, a style provider object added through the
    @fun{gtk:style-context-add-provider} function takes precedence over another
    added through this function.
  @end{notes}
  @see-class{gdk:screen}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-class{gtk:settings}
  @see-function{gtk:style-context-add-provider}"
  (screen (g:object gdk:screen))
  (provider (g:object style-provider))
  (priority :uint))

(export 'style-context-add-provider-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get ()
;;;
;;; void gtk_style_context_get (GtkStyleContext *context,
;;;                             GtkStateFlags state,
;;;                             ...);
;;;
;;; Retrieves several style property values from context for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the property values for
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_junction_sides ()
;;; gtk_style_context_set_junction_sides () -> style-context-junction-sides
;;; ----------------------------------------------------------------------------

(defun (setf style-context-junction-sides) (sides context)
  (cffi:foreign-funcall "gtk_style_context_set_junction_sides"
                        (g:object style-context) context
                        junction-sides sides
                        :void)
  sides)

(cffi:defcfun ("gtk_style_context_get_junction_sides"
               style-context-junction-sides) junction-sides
 #+liber-documentation
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-junction-sides context) => sides}
  @syntax{(setf (gtk:style-context-junction-sides context) sides)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[sides]{a value of the @symbol{gtk:junction-sides} flags}
  @begin{short}
    Accessor of the junction sides of a style context.
  @end{short}
  The @fun{gtk:style-context-junction-sides} function returns the sides where
  rendered elements, mostly through the @fun{gtk:render-frame} function, will
  visually connect with other visual elements. The
  @setf{gtk:style-context-junction-sides} function sets the sides where rendered
  elements will visually connect with other visual elements.

  This is merely a hint that may or may not be honored by theming engines.

  Container widgets are expected to set junction hints as appropriate for their
  children, so it should not normally be necessary to call this function
  manually.
  @see-class{gtk:style-context}
  @see-symbol{gtk:junction-sides}
  @see-function{gtk:render-frame}"
  (context (g:object style-context)))

(export 'style-context-junction-sides)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_path ()
;;; gtk_style_context_set_path ()
;;; ----------------------------------------------------------------------------

(defun (setf style-context-path) (path context)
  (cffi:foreign-funcall "gtk_style_context_set_path"
                        (g:object style-context) context
                        (g:boxed widget-path) path
                        :void)
  path)

(cffi:defcfun ("gtk_style_context_get_path" style-context-path)
    (g:boxed widget-path)
 #+liber-documentation
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-path context) => path}
  @syntax{(setf (gtk:style-context-path context) path)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[path]{a @class{gtk:widget-path} instance}
  @begin{short}
    Accessor of the widget path of the style context.
  @end{short}
  The @fun{gtk:style-context-path} function returns the widget path used for
  style matching. The @setf{gtk:style-context-path} function sets the widget
  path. As a consequence, the style will be regenerated to match the new given
  path.

  If you are using a style context returned from the
  @fun{gtk:widget-style-context} function, you do not need to call this
  yourself.
  @see-class{gtk:style-context}
  @see-class{gtk:widget-path}
  @see-function{gtk:widget-style-context}"
  (context (g:object style-context)))

(export 'style-context-path)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_property ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_property" %style-context-property) :void
  (context (g:object style-context))
  (property :string)
  (state state-flags)
  (value (:pointer (:struct g:value))))

(defun style-context-property (context property state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[property]{a string with a style property name}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the property value for}
  @return{The value for the style property.}
  @begin{short}
    Gets a style property from the style context for the given state.
  @end{short}
  @begin{examples}
    @begin{pre}
(setq context (gtk:style-context-new))
=> #<GTK-STYLE-CONTEXT {100687D223@}>
(gtk:style-context-property context \"color\" :normal)
=> #S(GDK-RGBA :RED 1.0d0 :GREEN 1.0d0 :BLUE 1.0d0 :ALPHA 1.0d0)
(gtk:style-context-property context \"opacity\" :normal)
=> 1.0d0
(gtk:style-context-property context \"font\" :normal)
=> #<PANGO-FONT-DESCRIPTION {100687E0B3@}>
(pango:font-description-to-string *)
=> \"Ubuntu 11\"
    @end{pre}
  @end{examples}
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}"
  (cffi:with-foreign-object (value '(:struct g:value))
    (g:value-init value)
    (prog2
      (%style-context-property context property state value)
      ;; TODO: Handle the case for an invalid property
      (when value
        (gobject:parse-g-value value))
      (g:value-unset value))))

(export 'style-context-property)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_frame_clock ()
;;; gtk_style_context_set_frame_clock ()
;;; ----------------------------------------------------------------------------

(defun (setf style-context-frame-clock) (frame-clock context)
  (cffi:foreign-funcall "gtk_style_context_set_frame_clock"
                        (g:object style-context) context
                        (g:object gdk:frame-clock) frame-clock
                        :void)
  frame-clock)

(cffi:defcfun ("gtk_style_context_get_frame_clock" style-context-frame-clock)
    (g:object gdk:frame-clock)
 #+liber-documentation
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-frame-clock context) => clock}
  @syntax{(setf (gtk:style-context-frame-clock context) clock)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{short}
    Accessor of the @class{gdk:frame-clock} object of the style context.
  @end{short}
  The @fun{gtk:style-context-frame-clock} function returns the frame clock to
  which the style context is attached. The
  @setf{gtk:style-context-frame-clock} function attaches the style context to
  the given frame clock.

  The frame clock is used for the timing of animations. If you are using a
  style context returned from the @fun{gtk:widget-style-context} function, you
  do not need to call this yourself.
  @begin{notes}
    This function is equivalent to the @fun{gtk:style-context-paint-clock}
    function.
  @end{notes}
  @see-class{gtk:style-context}
  @see-class{gdk:frame-clock}
  @see-function{gtk:widget-style-context}
  @see-function{gtk:style-context-paint-clock}"
  (context (g:object style-context)))

(export 'style-context-frame-clock)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_state ()
;;; gtk_style_context_set_state ()
;;; ----------------------------------------------------------------------------

(defun (setf style-context-state) (state context)
  (cffi:foreign-funcall "gtk_style_context_set_state"
                        (g:object style-context) context
                        state-flags state
                        :void)
  state)

(cffi:defcfun ("gtk_style_context_get_state" style-context-state) state-flags
 #+liber-documentation
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-state context) => state}
  @syntax{(setf (gtk:style-context-state context) state)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to represent}
  @begin{short}
    Accessor of the state used when rendering.
  @end{short}
  The @fun{gtk:style-context-state} function returns the state to be used when
  rendering with any of the @code{gtk:render-*} functions. The
  @setf{gtk:style-context-state} function sets the state.
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}"
  (context (g:object style-context)))

(export 'style-context-state)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style ()
;;;
;;; void gtk_style_context_get_style (GtkStyleContext *context, ...);
;;;
;;; Retrieves several widget style properties from context according to the
;;; current style.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; ... :
;;;     property name /return value pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style_property () -> style-context-style-property
;;; ----------------------------------------------------------------------------

;; TODO: The C implementation does not have the argument widget. The GtkWidget
;; class is looked up in the function gtk_style_context_get_style_property().
;; The Lisp implementation uses the widget to get the GType of the returned
;; value. In contrast to the function gtk:style-context-property the C
;; function must be called with the correct GType of the value.
;; Consider to change the implementation.

(cffi:defcfun ("gtk_style_context_get_style_property"
               %style-context-style-property) :void
  (context (g:object style-context))
  (property :string)
  (value (:pointer (:struct g:value))))

(defun style-context-style-property (context widget property)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[widget]{a @class{gtk:widget} object the style property is looked up
    for}
  @argument[property]{a string with the name of the widget style property}
  @return{Returns the value of the style property.}
  @begin{short}
    Gets the value for a widget style property.
  @end{short}
  @begin{examples}
    @begin{pre}
(setq message (make-instance 'gtk:message-dialog))
=> #<GTK-MESSAGE-DIALOG {100577F4A3@}>
(setq context (gtk:widget-style-context message))
=> #<GTK-STYLE-CONTEXT {10057DE323@}>
(gtk:style-context-style-property context message \"message-border\")
=> 12
    @end{pre}
  @end{examples}
  @see-class{gtk:style-context}
  @see-class{gtk:widget}"
  (let ((gtype (g:param-spec-value-type
                   (widget-class-find-style-property
                       (g:type-from-instance widget)
                       property))))
    (cffi:with-foreign-object (value '(:struct g:value))
      (g:value-init value gtype)
      (prog2
        (%style-context-style-property context property value)
        (gobject:parse-g-value value)
        (g:value-unset value)))))

(export 'style-context-style-property)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_style_valist ()
;;;
;;; void gtk_style_context_get_style_valist (GtkStyleContext *context,
;;;                                          va_list args);
;;;
;;; Retrieves several widget style properties from context according to the
;;; current style.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_valist ()
;;;
;;; void gtk_style_context_get_valist (GtkStyleContext *context,
;;;                                    GtkStateFlags state,
;;;                                    va_list args);
;;;
;;; Retrieves several style property values from context for a given state.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     state to retrieve the property values for
;;;
;;; args :
;;;     va_list of property name/return location pairs, followed by NULL
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_section ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_section" style-context-section)
    (g:boxed css-section)
 #+liber-documentation
 "@version{2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[property]{a string with the name of the style property}
  @return{Returns the @class{gtk:css-section} instance where the property was
    defined, or @code{nil}.}
  @begin{short}
    Queries the location in the CSS where @arg{property} was defined for the
    current style context.
  @end{short}
  Note that the state to be queried is taken from the
  @fun{gtk:style-context-state} function.

  If the location is not available, @code{nil} will be returned. The location
  might not be available for various reasons, such as the property being
  overridden, the @arg{property} argument not naming a supported CSS property or
  tracking of definitions being disabled for performance reasons.

  Shorthand CSS properties cannot be queried for a location and will always
  return @code{nil}.
  @see-class{gtk:style-context}
  @see-class{gtk:css-section}
  @see-function{gtk:style-context-state}"
  (context (g:object style-context))
  (property :string))

(export 'style-context-section)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_color" %style-context-color) :void
  (context (g:object style-context))
  (state state-flags)
  (color (g:boxed gdk:rgba)))

(defun style-context-color (context state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the color for}
  @return{The @class{gdk:rgba} foreground color.}
  @begin{short}
    Gets the foreground color for a given state.
  @end{short}
  See the @fun{gtk:style-context-property} function for details.
  @begin{examples}
    @begin{pre}
(setq context (gtk:style-context-new))
=> #<GTK-STYLE-CONTEXT {10058ED093@}>
(gtk:style-context-color context :normal)
=> #S(GDK-RGBA :RED 1.0d0 :GREEN 1.0d0 :BLUE 1.0d0 :ALPHA 1.0d0)
    @end{pre}
  @end{examples}
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}
  @see-class{gdk:rgba}
  @see-function{gtk:style-context-property}"
  (let ((color (gdk:rgba-new)))
    (%style-context-color context state color)
    color))

(export 'style-context-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_background_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_background_color"
               %style-context-background-color) :void
  (context (g:object style-context))
  (state state-flags)
  (color (g:boxed gdk:rgba)))

(defun style-context-background-color (context state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the color for}
  @return{Returns the @class{gdk:rgba} background color.}
  @begin{short}
    Gets the background color for a given state.
  @end{short}
  This function is far less useful than it seems, and it should not be used in
  newly written code. CSS has no concept of \"background color\", as a
  background can be an image, or a gradient, or any other pattern including
  solid colors. The only reason why you would call the
  @fun{gtk:style-context-background-color} function is to use the returned value
  to draw the background with it. The correct way to achieve this result is to
  use the @fun{gtk:render-background} function instead, along with CSS style
  classes to modify the color to be rendered.
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-background-color} function has been deprecated
    since version 3.16 and should not be used in newly written code. Use the
    @fun{gtk:render-background} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}
  @see-class{gdk:rgba}
  @see-function{gtk:render-background}"
  (let ((color (gdk:rgba-new)))
    (%style-context-background-color context state color)
    color))

(export 'style-context-background-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_border_color" %style-context-border-color)
    :void
  (context (g:object style-context))
  (state state-flags)
  (color (g:boxed gdk:rgba)))

(defun style-context-border-color (context state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the color for}
  @return{Returns the @class{gdk:rgba} border color.}
  @begin{short}
    Gets the border color for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-border-color} function has been deprecated since
    version 3.16 and should not be used in newly written code. Use the
    @fun{gtk:render-frame} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}
  @see-class{gdk:rgba}
  @see-function{gtk:render-frame}"
  (let ((color (gdk:rgba-new)))
    (%style-context-border-color context state color)
    color))

(export 'style-context-border-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_border" %style-context-border) :void
  (context (g:object style-context))
  (state state-flags)
  (border (g:boxed border)))

(defun style-context-border (context state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the border for}
  @return{Returns border settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the border settings for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  (let ((border (border-new)))
    (%style-context-border context state border)
    border))

(export 'style-context-border)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_padding ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_padding" %style-context-padding) :void
  (context (g:object style-context))
  (state state-flags)
  (padding (g:boxed border)))

(defun style-context-padding (context state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the padding for}
  @return{Returns padding settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the padding settings for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  (let ((padding (border-new)))
    (%style-context-padding context state padding)
    padding))

(export 'style-context-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_margin ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_margin" %style-context-margin) :void
  (context (g:object style-context))
  (state state-flags)
  (margin (g:boxed border)))

(defun style-context-margin (context state)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the margin for}
  @return{Returns margin settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the margin settings for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  (let ((margin (border-new)))
    (%style-context-margin context state margin)
    margin))

(export 'style-context-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_font ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_font" style-context-font)
    (g:boxed pango:font-description)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to retrieve
    the font for}
  @return{Returns a @class{pango:font-description} instance for the given
    state.}
  @begin{short}
    Returns the Pango font description for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-font} function has been deprecated since version
    3.8 and should not be used in newly written code. Use the
    @fun{gtk:style-context-property} function for \"font\" or subproperties
    instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:style-context-property}
  @see-class{pango:font-description}"
  (context (g:object style-context))
  (state state-flags))

(export 'style-context-font)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_invalidate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_invalidate" style-context-invalidate) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Invalidates style context information, so it will be reconstructed again.
  @end{short}
  If you are using a style context returned from the
  @fun{gtk:widget-style-context} function, you do not need to call this
  yourself.
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-invalidate} function has been deprecated since
    version 3.12 and should not be used in newly written code. Style contexts
    are invalidated automatically.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-style-context}"
  (context (g:object style-context)))

(export 'style-context-invalidate)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_state_is_running ()
;;;
;;; gboolean gtk_style_context_state_is_running (GtkStyleContext *context,
;;;                                              GtkStateType state,
;;;                                              gdouble *progress);
;;;
;;; Returns TRUE if there is a transition animation running for the current
;;; region (see gtk_style_context_push_animatable_region()).
;;;
;;; If progress is not NULL, the animation progress will be returned there, 0.0
;;; means the state is closest to being unset, while 1.0 means it's closest to
;;; being set. This means transition animation will run from 0 to 1 when state
;;; is being set and from 1 to 0 when it's being unset.
;;;
;;; Warning
;;;
;;; gtk_style_context_state_is_running has been deprecated since version 3.6 and
;;; should not be used in newly written code.
;;;
;;; This function always returns FALSE
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; state :
;;;     a widget state
;;;
;;; progress :
;;;     return location for the transition progress
;;;
;;; Returns :
;;;     TRUE if there is a running transition animation for state.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_lookup_color" %style-context-lookup-color)
    :boolean
  (context (g:object style-context))
  (name :string)
  (color (g:boxed gdk:rgba)))

(defun style-context-lookup-color (context name)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[name]{a string with a color name to lookup}
  @return{The looked up @class{gdk:rgba} color, or @code{nil}.}
  @begin{short}
    Looks up and resolves a color name in the style context color map.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gdk:rgba}"
  (let ((color (gdk:rgba-new)))
    (when (%style-context-lookup-color context name color)
      color)))

(export 'style-context-lookup-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_icon_set ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_lookup_icon_set"
               style-context-lookup-icon-set) (g:boxed icon-set)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[stock]{a string with an icon name}
  @return{The looked up @class{gtk:icon-set} instance, or @code{nil}.}
  @begin{short}
    Looks up a stock icon in the icon factories associated to the style
    context and the default icon factory, returning an icon set if found,
    otherwise @code{nil}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-lookup-icon-set} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:icon-theme-lookup-icon} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:icon-set}
  @see-function{gtk:icon-theme-lookup-icon}"
  (context (g:object style-context))
  (stock :string))

(export 'style-context-lookup-icon-set)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_notify_state_change ()               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_notify_state_change"
               style-context-notify-state-change) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[window]{a @class{gdk:window} object}
  @argument[region-id]{a pointer to the animatable region to notify on, or
    @code{NULL}}
  @argument[state]{a value of the @symbol{gtk:state-type} flags to trigger
    transition for}
  @argument[value]{@em{true} if @arg{state} is the state we are changing to,
    @em{false} if we are changing away from it}
  @begin{short}
    Notifies a state change on the style context.
  @end{short}
  So if the current style makes use of transition animations, one will be
  started so all rendered elements under @arg{region-id} are animated for the
  state being set to @arg{value}.

  The @arg{window} parameter is used in order to invalidate the rendered area
  as the animation runs, so make sure it is the same window that is being
  rendered on by the @code{gtk:render-*} functions.

  If @arg{region-id} is @code{NULL}, all rendered elements using the style
  context will be affected by this state transition.
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-notify-state-change} function has been deprecated
    since version 3.6 and should not be used in newly written code. This
    function does nothing.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gdk:window}
  @see-symbol{gtk:state-type}"
  (context (g:object style-context))
  (window (g:object gdk:window))
  (region-id :pointer)
  (state state-type)
  (value :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_pop_animatable_region ()             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_pop_animatable_region"
               style-context-pop-animatable-region) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Pops an animatable region from the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-pop-animatable-region} function has been
    deprecated since version 3.6 and should not be used in newly written code.
    This function does nothing.
  @end{dictionary}
  @see-class{gtk:style-context}"
  (context (g:object style-context)))

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_push_animatable_region ()            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_push_animatable_region"
               style-context-push-animatable-region) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[region-id]{a pointer which is an unique identifier for the
    animatable region}
  @begin{short}
    Pushes an animatable region, so all further @code{gtk:render-*} calls
    between this call and the following a
    @fun{gtk:style-context-pop-animatable-region} function call will
    potentially show transition animations for this region.
  @end{short}
  If the @fun{gtk:style-context-notify-state-change} function is called for a
  given state, and the current theme/style defines transition animations for
  state changes.

  The @arg{region-id} used must be unique in the style context so the theming
  engine can uniquely identify rendered elements subject to a state transition.
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-push-animatable-region} function has been
    deprecated since version 3.6 and should not be used in newly written code.
    This function does nothing.
  @end{dictionary}
  @see-class{gtk:style-context}"
  (context (g:object style-context))
  (region-id :pointer))

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_cancel_animations ()
;;;
;;; void gtk_style_context_cancel_animations (GtkStyleContext *context,
;;;                                           gpointer region_id);
;;;
;;; Stops all running animations for region_id and all animatable regions
;;; underneath.
;;;
;;; A NULL region_id will stop all ongoing animations in context, when dealing
;;; with a GtkStyleContext obtained through gtk_widget_get_style_context(), this
;;; is normally done for you in all circumstances you would expect all widget to
;;; be stopped, so this should be only used in complex widgets with different
;;; animatable regions.
;;;
;;; Warning
;;;
;;; gtk_style_context_cancel_animations has been deprecated since version 3.6
;;; and should not be used in newly written code.
;;;
;;; This function does nothing.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; region_id :
;;;     animatable region to stop, or NULL. See
;;;     gtk_style_context_push_animatable_region().
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_scroll_animations ()
;;;
;;; void gtk_style_context_scroll_animations (GtkStyleContext *context,
;;;                                           GdkWindow *window,
;;;                                           gint dx,
;;;                                           gint dy);
;;;
;;; This function is analogous to gdk_window_scroll(), and should be called
;;; together with it so the invalidation areas for any ongoing animation are
;;; scrolled together with it.
;;;
;;; Warning
;;;
;;; gtk_style_context_scroll_animations has been deprecated since version 3.6
;;; and should not be used in newly written code.
;;;
;;; This function does nothing.
;;;
;;; context :
;;;     a GtkStyleContext
;;;
;;; window :
;;;     a GdkWindow used previously in gtk_style_context_notify_state_change()
;;;
;;; dx :
;;;     Amount to scroll in the X axis
;;;
;;; dy :
;;;     Amount to scroll in the Y axis
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_remove_provider"
               style-context-remove-provider) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @begin{short}
    Removes the style provider from the style providers list in the style
    context.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider}"
  (context (g:object style-context))
  (provider (g:object style-provider)))

(export 'style-context-remove-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider_for_screen ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_remove_provider_for_screen"
               style-context-remove-provider-for-screen) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[screen]{a @class{gdk:screen} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @begin{short}
    Removes the style provider from the global style providers list in the
    screen.
  @end{short}
  @see-class{gdk:screen}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider-for-screen}"
  (screen (g:object gdk:screen))
  (provider (g:object style-provider)))

(export 'style-context-remove-provider-for-screen)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_reset_widgets ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_reset_widgets" style-context-reset-widgets)
    :void
 #+liber-documentation
 "@version{2024-1-3}
  @argument[screen]{a @class{gdk:screen} object}
  @begin{short}
    This function recomputes the styles for all widgets under a particular
    screen.
  @end{short}
  This is useful when some global parameter has changed that affects the
  appearance of all widgets, because when a widget gets a new style, it will
  both redraw and recompute any cached information about its appearance. As an
  example, it is used when the color scheme changes in the related
  @class{gtk:settings} object.
  @see-class{gtk:style-context}
  @see-class{gtk:settings}
  @see-class{gdk:screen}"
  (screen (g:object gdk:screen)))

(export 'style-context-reset-widgets)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_set_background ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_set_background" style-context-set-background)
    :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Sets the background of the GDK window to the background pattern or color
    specified in the style context for its current state.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-set-background} function has been deprecated
    since version 3.18 and should not be used in newly written code. Use the
    @fun{gtk:render-background} function instead. Note that clients still using
    this function are now responsible for calling this function again whenever
    the style context is invalidated.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gdk:window}
  @see-function{gtk:render-background}"
  (context (g:object style-context))
  (window (g:object gdk:window)))

(export 'style-context-set-background)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_restore ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_restore" style-context-restore) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Restores the style context state to a previous stage.
  @end{short}
  See the @fun{gtk:style-context-save} function.
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-save}"
  (context (g:object style-context)))

(export 'style-context-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_save ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_save" style-context-save) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Saves the style context state.
  @end{short}
  So all modifications done through the @fun{gtk:style-context-add-class},
  @fun{gtk:style-context-remove-class} or @fun{gtk:style-context-junction-sides}
  functions can be reverted in one go through the
  @fun{gtk:style-context-restore} function.
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-add-class}
  @see-function{gtk:style-context-remove-class}
  @see-function{gtk:style-context-junction-sides}
  @see-function{gtk:style-context-restore}"
  (context (g:object style-context)))

(export 'style-context-save)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_class ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_add_class" style-context-add-class) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string with a class name to use in styling}
  @begin{short}
    Adds a style class to the context, so posterior calls to the
    @fun{gtk:style-context-property} function or any of the @code{gtk:render-*}
    functions will make use of this new class for styling.
  @end{short}
  @begin{examples}
    In the CSS file format, a GtkEntry defining an \"entry\" class, would be
    matched by:
    @begin{pre}
GtkEntry.entry { ... @}
  @end{pre}
  While any widget defining an \"entry\" class would be matched by:
  @begin{pre}
.entry { ... @}
    @end{pre}
  @end{examples}
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-property}"
  (context (g:object style-context))
  (classname :string))

(export 'style-context-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_class ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_remove_class" style-context-remove-class)
    :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string with a class name to remove}
  @begin{short}
    Removes a class name from the style context.
  @end{short}
  @see-class{gtk:style-context}"
  (context (g:object style-context))
  (classname :string))

(export 'style-context-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_class ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_has_class" style-context-has-class) :boolean
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string with a class name}
  @return{@em{True} if the style context has @arg{classname} defined.}
  @begin{short}
    Returns @em{true} if the style context currently has defined the given
    class name.
  @end{short}
  @see-class{gtk:style-context}"
  (context (g:object style-context))
  (classname :string))

(export 'style-context-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_classes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_list_classes" style-context-list-classes)
    (g:list-t :string)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @return{The list of strings with the currently defined classes.}
  @begin{short}
    Returns the list of classes currently defined in the style context.
  @end{short}
  @see-class{gtk:style-context}"
  (context (g:object style-context)))

(export 'style-context-list-classes)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_add_region" style-context-add-region) :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[regionname]{a string with a region name to use in styling}
  @argument[flags]{a value of the @symbol{gtk:region-flags} flags that apply to
    the region}
  @begin{short}
    Adds a region to the style context, so posterior calls to the
    @fun{gtk:style-context-property} function or any of the @code{gtk:render-*}
    functions will make use of this new region for styling.
  @end{short}
  @begin{examples}
    In the CSS file format, a GtkTreeView defining a \"row\" region, would be
    matched by:
    @begin{pre}
 GtkTreeView row { ... @}
    @end{pre}
    Pseudo-classes are used for matching flags, so the two following rules would
    apply to even and odd rows, respectively.
    @begin{pre}
 GtkTreeView row:nth-child(even) { ... @}
 GtkTreeView row:nth-child(odd) { ... @}
    @end{pre}
  @end{examples}
  @begin{notes}
    Region names must only contain lowercase letters and '-', starting always
    with a lowercase letter.
  @end{notes}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-add-region} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:region-flags}
  @see-function{gtk:style-context-property}"
  (context (g:object style-context))
  (regionname :string)
  (flags region-flags))

(export 'style-context-add-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_remove_region" style-context-remove-region)
    :void
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[regionname]{a string with a region name to unset}
  @begin{short}
    Removes a region from the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-remove-region} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:style-context}"
  (context (g:object style-context))
  (regionname :string))

(export 'style-context-remove-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_has_region" %style-context-has-region)
    :boolean
  (context (g:object style-context))
  (regionname :string)
  (flags (:pointer region-flags)))

(defun style-context-has-region (context regionname)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[regionname]{a string with a region name}
  @return{Returns a @symbol{gtk:region-flags} value.}
  @begin{short}
    Returns the region flags if the style context has the region defined.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-has-region} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:region-flags}"
  (cffi:with-foreign-object (flags 'region-flags)
    (when (%style-context-has-region context regionname flags)
      (cffi:mem-ref flags 'region-flags))))

(export 'style-context-has-region)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_list_regions ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_list_regions" style-context-list-regions)
    (g:list-t :string)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @return{Returns a list of strings with the currently defined regions.}
  @begin{short}
    Returns the list of regions currently defined in the style context.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:style-context-list-regions} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:style-context}"
  (context (g:object style-context)))

(export 'style-context-list-regions)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_scale ()
;;; gtk_style_context_set_scale ()
;;; ----------------------------------------------------------------------------

(defun (setf style-context-scale) (scale context)
  (cffi:foreign-funcall "gtk_style_context_set_scale"
                        (g:object style-context) context
                        :int scale
                        :void)
  scale)

(cffi:defcfun ("gtk_style_context_get_scale" style-context-scale) :int
 #+liber-documentation
 "@version{#2023-3-27}
  @syntax{(gtk:style-context-scale context) => scale}
  @syntax{(setf (gtk:style-context-scale context) scale)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[scale]{an integer with a scale}
  @begin{short}
    Accessor of the scale used for image assets for the style context.
  @end{short}
  The @fun{gtk:style-context-scale} function returns the scale used for image
  assets for the style context. The @setf{gtk:style-context-scale} function sets
  the scale.
  @see-class{gtk:style-context}"
  (context (g:object style-context)))

(export 'style-context-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_to_string" style-context-to-string) :string
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[flags]{a value of the @symbol{gtk:style-context-print-flags} flags
    that determine what to print}
  @return{The string representing the style context.}
  @begin{short}
    Converts the style context into a string representation.
  @end{short}
  The string representation always includes information about the name, state,
  ID, visibility and style classes of the CSS node that is backing the style
  context. Depending on the flags, more information may be included.

  This function is intended for testing and debugging of the CSS implementation
  in GTK. There are no guarantees about the format of the returned string, it
  may change.
  @begin{examples}
    @begin{pre}
(setq context
      (gtk:widget-style-context (make-instance 'gtk:message-dialog)))
=> #<gtk:style-context {1001C70663@}>
(gtk:style-context-to-string context :recurse)
=>
\"[messagedialog.background.csd:dir(ltr)@]
  decoration:dir(ltr)
  box.vertical.dialog-vbox:dir(ltr)
    box.horizontal:dir(ltr)
      image:dir(ltr)
      box.vertical:dir(ltr)
        label:dir(ltr)
        [label:dir(ltr)@]
    box.horizontal.dialog-action-box:dir(ltr)
      buttonbox.linked.horizontal.dialog-action-area:dir(ltr)
  box.titlebar.horizontal:dir(ltr)
    [label.title:dir(ltr)@]
\"
    @end{pre}
  @end{examples}
  @see-class{gtk:style-context}
  @see-symbol{gtk:style-context-print-flags}"
  (context (g:object style-context))
  (flags style-context-print-flags))

(export 'style-context-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_border_free ()
;;;
;;; void gtk_border_free (GtkBorder *border_);
;;;
;;; Frees a GtkBorder structure.
;;;
;;; border_ :
;;;     a GtkBorder
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_render_arrow ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_arrow" %render-arrow) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (angle :double)
  (x :double)
  (y :double)
  (size :double))

(defun render-arrow (context cr angle x y size)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[angle]{a number, coerced to a double float, with an arrow angle
    from 0 to 2 * Pi, being 0 the arrow pointing to the north}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[size]{a number, coerced to a double float, with the square side for
    render area}
  @begin{short}
    Renders an arrow pointing to an angle.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-arrow context cr (coerce angle 'double-float)
                            (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce size 'double-float)))

(export 'render-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_background" %render-background) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-background (context cr x y width height)
 #+liber-documentation
 "@version{2024-1-2}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number coerced to a double float with the x origin of the
    render area}
  @argument[y]{a number coerced to a double float with the y origin of the
    render area}
  @argument[width]{a number coerced to a double float with the rectangle width}
  @argument[height]{a number coerced to a double float with the rectangle
    height}
  @short{Renders the background of an element.}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-background context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'render-background)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background_get_clip () -> render-background-clip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_background_get_clip" %render-background-clip) :void
  (context (g:object style-context))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (out-clip (g:boxed gdk:rectangle)))

(defun render-background-clip (context x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @return{The @class{gdk:rectangle} instance.}
  @begin{short}
    Returns the area that will be affected, i.e. drawn to, when calling the
    @fun{gtk:render-background} function for the given context and rectangle.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gdk:rectangle}
  @see-function{gtk:render-background}"
  (let ((out-clip (gdk:rectangle-new)))
    (%render-background-clip context (coerce x 'double-float)
                                     (coerce y 'double-float)
                                     (coerce width 'double-float)
                                     (coerce height 'double-float)
                                     out-clip)
    out-clip))

(export 'render-background-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_render_check ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_check" %render-check) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-check (context cr x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a checkmark as in a @class{gtk:check-button} widget.
  @end{short}
  The @code{:active} state of the @symbol{gtk:state-flags} flags determines
  whether the check is on or off, and the @code{:inconsistent} state determines
  whether it should be marked as undefined.
  @see-class{gtk:style-context}
  @see-class{gtk:check-button}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-check context cr (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'render-check)

;;; ----------------------------------------------------------------------------
;;; gtk_render_expander ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_expander" %render-expander) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-expander (context cr x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an expander as used in the @class{gtk:tree-view} widget and
    @class{gtk:expander} widget in the area defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  The @code{:active} state of the @symbol{gtk:state-flags} flags determines
  whether the expander is collapsed or expanded.
  @see-class{gtk:style-context}
  @see-class{gtk:expander}
  @see-class{gtk:tree-view}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-expander context cr (coerce x 'double-float)
                               (coerce y 'double-float)
                               (coerce width 'double-float)
                               (coerce height 'double-float)))

(export 'render-expander)

;;; ----------------------------------------------------------------------------
;;; gtk_render_extension ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_extension" %render-extension) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (gap-side position-type))

(defun render-extension (context cr x y width height side)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @argument[side]{a value of the @symbol{gtk:position-type} enumeration where
    the gap is}
  @begin{short}
    Renders a extension as in a @class{gtk:notebook} widget tab in the rectangle
    defined by @arg{x}, @arg{y}, @arg{width}, @arg{height}.
  @end{short}
  The side where the extension connects to is defined by @arg{side}.
  @see-class{gtk:style-context}
  @see-class{gtk:notebook}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:position-type}"
  (%render-extension context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)
                                side))

(export 'render-extension)

;;; ----------------------------------------------------------------------------
;;; gtk_render_focus ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_focus" %render-focus) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-focus (context cr x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a focus indicator on the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-focus context cr (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'render-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_frame" %render-frame) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-frame (context cr x y width height)
 #+liber-documentation
 "@version{2024-1-2}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number coerced to a double float with the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float with the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float with the rectangle width}
  @argument[height]{a number coerced to a double float with the rectangle
    height}
  @begin{short}
    Renders a frame around the rectangle defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-frame context cr (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'render-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame_gap ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_frame_gap" %render-frame-gap) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (side position-type)
  (xy0 :double)
  (xy1 :double))

(defun render-frame-gap (context cr x y width height side xy0 xy1)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @argument[side]{a value of the @symbol{gtk:position-type} enumeration where
    the gap is}
  @argument[xy0]{a number, coerced to a double float, with the initial
    coordinate for the gap}
  @argument[xy1]{a number, coerced to a double float, with the end coordinate
    for the gap}
  @begin{short}
    Renders a frame around the rectangle defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height} leaving a gap on one side.
  @end{short}
  The arguments @arg{xy0} and @arg{xy1} will mean x coordinates for @code{:top}
  and @code{:bottom} gap sides, and y coordinates for @code{:left} and
  @code{:right} gap sides.
  @begin[Warning]{dictionary}
    The @fun{gtk:render-frame-gap} function has been deprecated since version
    3.24 and should not be used in newly written code. Use the
    @fun{gtk:render-frame} function instead. Themes can create gaps by omitting
    borders via CSS.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:position-type}
  @see-function{gtk:render-frame}"
  (%render-frame-gap context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)
                                side
                                (coerce xy0 'double-float)
                                (coerce xy1 'double-float)))

(export 'render-frame-gap)

;;; ----------------------------------------------------------------------------
;;; gtk_render_handle ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_handle" %render-handle) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-handle (context cr x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a handle, as the resize grip of the @class{gtk:paned} widget and
    @class{gtk:window} widget, in the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:paned}
  @see-class{gtk:window}
  @see-symbol{cairo:context-t}"
  (%render-handle context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             (coerce width 'double-float)
                             (coerce height 'double-float)))

(export 'render-handle)

;;; ----------------------------------------------------------------------------
;;; gtk_render_layout ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_layout" %render-layout) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (layout (g:object pango:layout)))

(defun render-layout (context cr x y layout)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[layout]{a @symbol{pango:layout} object to render}
  @begin{short}
    Renders a Pango layout on the coordinates @arg{x}, @arg{y}.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-symbol{pango:layout}"
  (%render-layout context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             layout))

(export 'render-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_render_line ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_line" %render-line) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun render-line (context cr x0 y0 x1 y1)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x0]{a number, coerced to a double float, with the x coordinate for
    the origin of the line}
  @argument[y0]{a number, coerced to a double float, with the y coordinate for
    the origin of the line}
  @argument[x1]{a number, coerced to a double float, with the x coordinate for
    the end of the line}
  @argument[y1]{a number, coerced to a double float, with the y coordinate for
    the end of the line}
  @begin{short}
    Renders a line from (@arg{x0}, @arg{y0}) to (@arg{x1}, @arg{y1}).
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-line context cr (coerce x0 'double-float)
                           (coerce y0 'double-float)
                           (coerce x1 'double-float)
                           (coerce y1 'double-float)))

(export 'render-line)

;;; ----------------------------------------------------------------------------
;;; gtk_render_option ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_option" %render-option) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-option (context cr x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an option mark as in a @class{gtk:radio-button} widget.
  @end{short}
  The @code{:active} state of the @symbol{gtk:state-flags} flags will determine
  whether the option is on or off, and the @code{:inconsistent} state whether
  it should be marked as undefined.
  @see-class{gtk:style-context}
  @see-class{gtk:radio-button}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-option context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             (coerce width 'double-float)
                             (coerce height 'double-float)))

(export 'render-option)

;;; ----------------------------------------------------------------------------
;;; gtk_render_slider ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_slider" %render-slider) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (orientation orientation))

(defun render-slider (context cr x y width height orientation)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @begin{short}
    Renders a slider as in the @class{gtk:scale} widget in the rectangle
    defined by @arg{x}, @arg{y}, @arg{width}, @arg{height}.
  @end{short}
  The @arg{orientation} argument defines whether the slider is vertical or
  horizontal.
  @see-class{gtk:style-context}
  @see-class{gtk:scale}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:orientation}"
  (%render-slider context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             (coerce width 'double-float)
                             (coerce height 'double-float)
                             orientation))

(export 'render-slider)

;;; ----------------------------------------------------------------------------
;;; gtk_render_activity ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_activity" %render-activity) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-activity (context cr x y width height)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an activity area such as in the @class{gtk:spinner} widget or the
    fill line in the @class{gtk:range} widget.
  @end{short}
  The @code{:active} state of the @symbol{gtk:stage-flags} flags determines
  whether there is activity going on.
  @see-class{gtk:style-context}
  @see-class{gtk:spinner}
  @see-class{gtk:range}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-activity context cr (coerce x 'double-float)
                               (coerce y 'double-float)
                               (coerce width 'double-float)
                               (coerce height 'double-float)))

(export 'render-activity)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_pixbuf ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_icon_pixbuf" render-icon-pixbuf)
    (g:object gdk-pixbuf:pixbuf)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[source]{a @class{gtk:icon-source} instance specifying the icon to
    render}
  @argument[size]{a @symbol{gtk:icon-size} size to render the icon at}
  @return{The newly created @class{gdk-pixbuf:pixbuf} object containing the
    rendered icon.}
  @begin{short}
    Renders the icon specified by @arg{source} at the given @arg{size},
    returning the result in a pixbuf.
  @end{short}
  A size of -1 means render at the size of the source and do not scale.
  @begin[Warning]{dictionary}
    The @fun{gtk:render-icon-pixbuf} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{gtk:icon-theme-load-icon} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:icon-source}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:icon-theme-load-icon}"
  (context (g:object style-context))
  (source (g:boxed icon-source))
  (size icon-size))

(export 'render-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_icon_surface" %render-icon-surface) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (surface (:pointer (:struct cairo:surface-t)))
  (x :double)
  (y :double))

(defun render-icon-surface (context cr surface x y)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[surface]{a @symbol{cairo:surface-t} instance containing the icon
    to draw}
  @argument[x]{a number, coerced to a double float, with a x position for the
    icon}
  @argument[y]{a number, coerced to a double float, with a y position for the
    icon}
  @begin{short}
    Renders the icon in the Cairo surface at the specified @arg{x} and @arg{y}
    coordinates.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-symbol{cairo:surface-t}"
  (%render-icon-surface context cr surface (coerce x 'double-float)
                                           (coerce y 'double-float)))

(export 'render-icon-surface)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_icon" %render-icon) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (pixbuf (g:object gdk-pixbuf:pixbuf))
  (x :double)
  (y :double))

(defun render-icon (context cr pixbuf x y)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object containing the icon to
    draw}
  @argument[x]{a number, coerced to a double float, with the x position for the
    pixbuf}
  @argument[y]{a number, coerced to a double float, with the y position for the
    pixbuf}
  @begin{short}
    Renders the icon in a pixbuf at the specified @arg{x} and @arg{y}
    coordinates.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-class{gdk-pixbuf:pixbuf}"
  (%render-icon context cr pixbuf (coerce x 'double-float)
                                  (coerce y 'double-float)))

(export 'render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_render_insertion_cursor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_insertion_cursor" %render-insertion-cursor) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (layout (g:object pango:layout))
  (index :int)
  (direction pango:direction))

(defun render-insertion-cursor (context cr x y layout index direction)
 #+liber-documentation
 "@version{#2023-3-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin}
  @argument[y]{a number, coerced to a double float, with the y origin}
  @argument[layout]{the @class{pango:layout} object of the text}
  @argument[index]{an integer with the index in the Pango layout}
  @argument[direction]{a value of the @symbol{pango:direction} enumeration}
  @begin{short}
    Draws a text caret on the Cairo context at the specified index of the
    Pango layout.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-class{pango:layout}
  @see-symbol{pango:direction}"
  (%render-insertion-cursor context cr (coerce x 'double-float)
                                       (coerce y 'double-float)
                                       layout
                                       index
                                       direction))

(export 'render-insertion-cursor)

;;; --- End of file gtk3.style-context.lisp ------------------------------------
