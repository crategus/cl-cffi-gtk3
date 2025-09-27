;;; ----------------------------------------------------------------------------
;;; gtk3.color-selection.lisp
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
;;; GtkColorSelection
;;;
;;;     Deprecated widget used to select a color
;;;
;;; Types and Values
;;;
;;;     GtkColorSelection
;;;
;;; Accessors
;;;
;;;     gtk_color_selection_get_current_alpha
;;;     gtk_color_selection_set_current_alpha
;;;     gtk_color_selection_get_current_color
;;;     gtk_color_selection_set_current_color
;;;     gtk_color_selection_get_current_rgba
;;;     gtk_color_selection_set_current_rgba
;;;     gtk_color_selection_set_has_opacity_control
;;;     gtk_color_selection_get_has_opacity_control
;;;     gtk_color_selection_set_has_palette
;;;     gtk_color_selection_get_has_palette
;;;
;;; Functions
;;;
;;;     gtk_color_selection_new
;;;     gtk_color_selection_get_previous_alpha
;;;     gtk_color_selection_set_previous_alpha
;;;     gtk_color_selection_get_previous_color
;;;     gtk_color_selection_set_previous_color
;;;     gtk_color_selection_get_previous_rgba
;;;     gtk_color_selection_set_previous_rgba
;;;     gtk_color_selection_is_adjusting
;;;     gtk_color_selection_palette_from_string
;;;     gtk_color_selection_palette_to_string
;;;     gtk_color_selection_set_change_palette_with_screen_hook
;;;
;;; Properties
;;;
;;;     current-alpha
;;;     current-color
;;;     current-rgba
;;;     has-opacity-control
;;;     has-palette
;;;
;;; Signals
;;;
;;;     color-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkColorSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GtkColorSelection implements AtkImplementorIface, GtkBuildable and
;;;     GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorSelection
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColorSelection" color-selection
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
   :type-initializer "gtk_color_selection_get_type")
  ((current-alpha
    color-selection-current-alpha
    "current-alpha" "guint" t t)
   (current-color
    color-selection-current-color
    "current-color" "GdkColor" t t)
   (current-rgba
    color-selection-current-rgba
    "current-rgba" "GdkRGBA" t t)
   (has-opacity-control
    color-selection-has-opacity-control
    "has-opacity-control" "gboolean" t t)
   (has-palette
    color-selection-has-palette
    "has-palette" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'color-selection 'type)
 "@version{2025-07-14}
  @begin{short}
    The @class{gtk:color-selection} widget is used to select a color.
  @end{short}
  It consists of a color wheel and a number of sliders and entry boxes for
  color parameters such as hue, saturation, value, red, green, blue, and
  opacity. It is found on the @class{gtk:color-selection-dialog} widget.
  @begin[Warning]{dictionary}
    The @class{gtk:color-selection} widget is deprecated since GTK 3.4 and
    should not be used in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[color-selection::color-changed]{signal}
      @begin{pre}
lambda (selection)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[selection]{The @class{gtk:color-selection} widget that received
        the signal.}
      @end{simple-table}
      The signal is emitted when the color changes in the color selector
      according to its update policy.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:color-selection-new}
  @see-slot{gtk:color-selection-current-alpha}
  @see-slot{gtk:color-selection-current-color}
  @see-slot{gtk:color-selection-current-rgba}
  @see-slot{gtk:color-selection-has-opacity-control}
  @see-slot{gtk:color-selection-has-palette}
  @see-class{gtk:color-selection-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:color-selection-current-alpha --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-alpha"
                                               'color-selection) t)
 "The @code{current-alpha} property of type @code{:uint} (Read / Write) @br{}
  The current opacity value, 0 fully transparent, 65535 fully opaque. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 65535")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-current-alpha)
      "Accessor"
      (documentation 'color-selection-current-alpha 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-current-alpha object) => alpha}
  @syntax{(setf (gtk:color-selection-current-alpha object) alpha)}
  @argument[object]{a @class{gtk:color-selection} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    Accessor of the @slot[gtk:color-selection]{current-alpha} slot of the
    @class{gtk:color-selection} class.
  @end{short}
  The @fun{gtk:color-selection-current-alpha} function returns the current
  alpha value. The @setf{gtk:color-selection-current-alpha} function sets the
  current opacity. The first time this is called, it will also set the original
  opacity to be @arg{alpha} too.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-current-alpha} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}")

;;; --- gtk:color-selection-current-color --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-color"
                                               'color-selection) t)
 "The @code{current-color} property of type @class{gdk:color} (Read / Write)
  @br{}
  The current color.")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-current-color)
      "Accessor"
      (documentation 'color-selection-current-color 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-current-color object) => color}
  @syntax{(setf (gtk:color-selection-current-color object) color)}
  @argument[object]{a @class{gtk:color-selection} widget}
  @argument[color]{a @class{gdk:color} color}
  @begin{short}
    Accessor of the @slot[gtk:color-selection]{current-color} slot of the
    @class{gtk:color-selection} class.
  @end{short}
  The @fun{gtk:color-selection-current-alpha} function gets the current color
  in the color selector. The @setf{gtk:color-selection-current-alpha} function
  sets the current color. The first time this is called, it will also set the
  original color to be @arg{color} too.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-current-color} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}
  @see-class{gdk:color}")

;;; --- gtk:color-selection-current-rgba ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-rgba"
                                               'color-selection) t)
 "The @code{current-rgba} property of type @class{gdk:rgba} (Read / Write) @br{}
  The current RGBA color.")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-current-rgba)
      "Accessor"
      (documentation 'color-selection-current-rgba 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-current-rgba object) => rgba}
  @syntax{(setf (gtk:color-selection-current-rgba object) rgba)}
  @argument[object]{a @class{gtk:color-selection} widget}
  @argument[rgba]{a @class{gdk:rgba} color to set the current color with}
  @begin{short}
    Accessor of the @slot[gtk:color-selection]{current-rgba} slot of the
    @class{gtk:color-selection} class.
  @end{short}
  The @fun{gtk:color-selection-current-rgba} function gets the current color in
  the color selector. The @setf{gtk:color-selection-current-rgba} function sets
  the current color. The first time this is called, it will also set the
  original color to be @arg{rgba} too.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-current-rgba} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}
  @see-class{gdk:rgba}")

;;; --- gtk:color-selection-has-opacity-control --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-opacity-control"
                                               'color-selection) t)
 "The @code{has-opacity-control} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the color selector should allow setting the opacity. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-has-opacity-control)
      "Accessor"
      (documentation 'color-selection-has-opacity-control 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-has-opacity-control object) => has-opacity}
  @syntax{(setf (gtk:color-selection-has-opacity-control object) has-opacity)}
  @argument[object]{a @class{gtk:color-selection} widget}
  @argument[has-opacity]{@em{true} if the color selector can set the opacity,
    @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk:color-selection]{has-opacity-control} slot of the
    @class{gtk:color-selection} class.
  @end{short}
  The @fun{gtk:color-selection-has-opacity-control} function determines whether
  the color selector has an opacity control. The
  @setf{gtk:color-selection-has-opacity-control} function sets the color
  selector to use or not use opacity.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-has-opacity-control} function is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}")

;;; --- gtk:color-selection-has-palette ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-palette"
                                               'color-selection) t)
 "The @code{has-palette} property of type @code{:boolean} (Read / Write) @br{}
  Whether a palette should be used. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-selection-has-palette)
      "Accessor"
      (documentation 'color-selection-has-palette 'function)
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-has-palette object) => has-palette}
  @syntax{(setf (gtk:color-selection-has-palette object) has-palette)}
  @argument[object]{a @class{gtk:color-selection} widget}
  @argument[has-palette]{@em{true} if the color palette is to be visible,
    @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk:color-selection]{has-palette} slot of the
    @class{gtk:color-selection} class.
  @end{short}
  The @fun{gtk:color-selection-has-palette} function determines whether the
  color selector has a color palette. The
  @setf{gtk:color-selection-has-palette} function shows and hides the color
  palette.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-has-palette} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_new
;;; ----------------------------------------------------------------------------

(defun color-selection-new ()
 #+liber-documentation
 "@version{2025-07-07}
  @return{The new @class{gtk:color-selection} widget.}
  @short{Creates a new color selector.}
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-new} function is deprecated since version 3.4
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}"
  (make-instance 'color-selection))

(export 'color-selection-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_alpha
;;; gtk_color_selection_set_previous_alpha
;;; ----------------------------------------------------------------------------

(defun (setf color-selection-previous-alpha) (alpha selection)
  (cffi:foreign-funcall "gtk_color_selection_set_previous_alpha"
                        (g:object color-selection) selection
                        :uint16 alpha
                        :void)
  alpha)

(cffi:defcfun ("gtk_color_selection_get_previous_alpha"
                color-selection-previous-alpha) :uint16
 #+liber-documentation
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-previous-alpha selection) => alpha}
  @syntax{(setf (gtk:color-selection-previous-alpha selection) alpha)}
  @argument[selection]{a @class{gtk:color-selection} widget}
  @argument[alpha]{an integer between 0 and 65535}
  @begin{short}
    The @fun{gtk:color-selection-previous-alpha} function returns the previous
    alpha value.
  @end{short}
  The @setf{gtk:color-selection-previous-alpha} function sets the previous
  alpha value.

  This function should be called with some hesitations, as it might seem
  confusing to have that alpha change.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-previous-alpha} function is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}"
  (selection (g:object color-selection)))

(export 'color-selection-previous-alpha)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_color
;;; gtk_color_selection_set_previous_color
;;; ----------------------------------------------------------------------------

(defun (setf color-selection-previous-color) (color selection)
  (cffi:foreign-funcall "gtk_color_selection_set_previous_color"
                        (g:object color-selection) selection
                        (g:boxed gdk:color) color
                        :void)
  color)

(cffi:defcfun ("gtk_color_selection_get_previous_color"
                %color-selection-get-previous-color) :void
  (selection (g:object color-selection))
  (color (g:boxed gdk:color)))

(defun color-selection-previous-color (selection)
 #+liber-documentation
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-previous-color selection) => color}
  @syntax{(setf (gtk:color-selection-previous-color selection) color)}
  @argument[selection]{a @class{gtk:color-selection} widget}
  @argument[color]{a @class{gdk:color} color}
  @begin{short}
    The @fun{gtk:color-selection-previous-color} function gets the previous
    color value.
  @end{short}
  The @setf{gtk:color-selection-previous-color} function sets the previous
  color.

  This function should be called with some hesitations, as it might seem
  confusing to have that color change. Calling the
  @fun{gtk:color-selection-current-color} function will also set this color the
  first time it is called.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-previous-color} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}
  @see-class{gdk:color}
  @see-function{gtk:color-selection-current-rgba}"
  (let ((color (gdk:color-new)))
    (%color-selection-get-previous-color selection color)
    color))

(export 'color-selection-previous-color)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_get_previous_rgba
;;; gtk_color_selection_set_previous_rgba
;;; ----------------------------------------------------------------------------

(defun (setf color-selection-previous-rgba) (rgba selection)
  (cffi:foreign-funcall "gtk_color_selection_set_previous_rgba"
                        (g:object color-selection) selection
                        (g:boxed gdk:rgba) rgba
                        :void)
  rgba)

(cffi:defcfun ("gtk_color_selection_get_previous_rgba"
               %color-selection-get-previous-rgba) :void
  (selection (g:object color-selection))
  (rgba (g:boxed gdk:rgba)))

(defun color-selection-previous-rgba (selection)
 #+liber-documentation
 "@version{2023-06-14}
  @syntax{(gtk:color-selection-previous-rgba selection) => rgba}
  @syntax{(setf (gtk:color-selection-previous-rgba selection) rgba)}
  @argument[selection]{a @class{gtk:color-selection} widget}
  @argument[rgba]{a @class{gdk:rgba} color to set the previous color with}
  @begin{short}
    The @fun{gtk:color-selection-previous-rgba} function gets the previous
    color.
  @end{short}
  The @setf{gtk:color-selection-previous-rgba} function sets the previous color.

  This function should be called with some hesitations, as it might seem
  confusing to have that color change. Calling the
  @fun{gtk:color-selection-current-rgba} function will also set this color the
  first time it is called.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-previous-rgba} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}
  @see-class{gdk:rgba}
  @see-function{gtk:color-selection-current-color}"
  (let ((rgba (gdk:rgba-new)))
    (%color-selection-get-previous-rgba selection rgba)
    rgba))

(export 'color-selection-previous-rgba)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_is_adjusting
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_color_selection_is_adjusting" color-selection-is-adjusting)
    :boolean
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[selection]{a @class{gtk:color-selection} widget}
  @begin{return}
    @em{True} if the user is currently dragging a color around, and @em{false}
    if the selection has stopped.
  @end{return}
  @begin{short}
    Gets the current state of the color selector.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-is-adjusting} function is deprecated since
    version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}"
  (selection g:object))

(export 'color-selection-is-adjusting)

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_from_string                 not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_color_selection_palette_from_string"
                %color-selection-palette-from-string) :boolean
  (str :string)
  (colors :pointer)
  (n-colors :pointer))

(defun color-selection-palette-from-string (str)
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[str]{a string encoding a color palette}
  @return{@em{True} if a color palette was successfully parsed.}
  @begin{short}
    Parses a color palette from a string.
  @end{short}
  The string is a colon-separated list of color names readable by the
  @fun{gdk:color-parse} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-palette-from-string} function is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}
  @see-function{gdk:color-parse}"
  (cffi:with-foreign-objects ((colors :pointer) (n-colors :int))
    (when (%color-selection-palette-from-string str colors n-colors)
      (iter (with colors-ar = (cffi:mem-ref colors :pointer))
            (for i from 0 below (cffi:mem-ref n-colors :int))
            (for color-ptr =
                 (cffi:inc-pointer
                         colors-ar
                         ;; TODO: We have direct access to the structure.
                         ;; Check to replace this access.
                         (* i (cffi:foreign-type-size 'gdk::color-cstruct))))
            (for color = (cffi:convert-from-foreign color-ptr
                                                    '(g:boxed gdk:color)))
            (collect color)
            (finally (g:free colors-ar))))))

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_palette_to_string                   not exported
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation, is this correct?

(cffi:defcfun ("gtk_color_selection_palette_to_string"
                color-selection-palette-to-string) :string
 #+liber-documentation
 "@version{#2023-03-16}
  @argument[colors]{an array of colors}
  @argument[n-colors]{length of the array}
  @return{Allocated string encoding the palette.}
  @begin{short}
    Encodes a palette as a string, useful for persistent storage.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:color-selection-palette-to-string} function is deprecated
    since version 3.4 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:color-selection}"
  (colors :pointer)
  (n-colors :int))

;;; ----------------------------------------------------------------------------
;;; GtkColorSelectionChangePaletteFunc ()
;;;
;;; void (*GtkColorSelectionChangePaletteFunc) (const GdkColor *colors,
;;;                                             gint n_colors);
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_color_selection_set_change_palette_with_screen_hook ()
;;;
;;; GtkColorSelectionChangePaletteWithScreenFunc
;;; gtk_color_selection_set_change_palette_with_screen_hook
;;;                          (GtkColorSelectionChangePaletteWithScreenFunc func)
;;;
;;; Installs a global function to be called whenever the user tries to modify
;;; the palette in a color selection.
;;;
;;; This function should save the new palette contents, and update the
;;; "color-palette" GtkSettings property so all GtkColorSelection widgets
;;; will be modified.
;;;
;;; func :
;;;     a function to call when the custom palette needs saving
;;;
;;; Returns :
;;;     the previous change palette hook (that was replaced)
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkColorSelectionChangePaletteWithScreenFunc ()
;;;
;;; void (*GtkColorSelectionChangePaletteWithScreenFunc)
;;;                                                     (GdkScreen *screen,
;;;                                                      const GdkColor *colors,
;;;                                                      gint n_colors);
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.color-selection.lisp ----------------------------------
