;;; ----------------------------------------------------------------------------
;;; gtk3.numerable-icon.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2014 - 2024 Dieter Kaiser
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
;;; GtkNumerableIcon
;;;
;;;     A GIcon that allows numbered emblems
;;;
;;; Types and Values
;;;
;;;     GtkNumerableIcon
;;;
;;; Functions
;;;
;;;     gtk_numerable_icon_new
;;;     gtk_numerable_icon_new_with_style_context
;;;     gtk_numerable_icon_get_background_gicon            Accessor
;;;     gtk_numerable_icon_set_background_gicon            Accessor
;;;     gtk_numerable_icon_get_background_icon_name        Accessor
;;;     gtk_numerable_icon_set_background_icon_name        Accessor
;;;     gtk_numerable_icon_get_count                       Accessor
;;;     gtk_numerable_icon_set_count                       Accessor
;;;     gtk_numerable_icon_get_label                       Accessor
;;;     gtk_numerable_icon_set_label                       Accessor
;;;     gtk_numerable_icon_get_style_context               Accessor
;;;     gtk_numerable_icon_set_style_context               Accessor
;;;
;;; Properties
;;;
;;;     background-icon
;;;     background-icon-name
;;;     count
;;;     label
;;;     style-context
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GEmblemedIcon
;;;         ╰── GtkNumerableIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GtkNumerableIcon implements GIcon.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkNumerableIcon
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkNumerableIcon" numerable-icon
  (:superclass g:emblemed-icon
   :export t
   :interfaces ("GIcon")
   :type-initializer "gtk_numerable_icon_get_type")
  ((background-icon
    numerable-icon-background-icon
    "background-icon" "GIcon" t t)
   (background-icon-name
    numerable-icon-background-icon-name
    "background-icon-name" "gchararray" t t)
   (count
    numerable-icon-count
    "count" "gint" t t)
   (label
    numerable-icon-label
    "label" "gchararray" t t)
   (style-context
    numerable-icon-style-context
    "style-context" "GtkStyleContext" t t)))

#+liber-documentation
(setf (documentation 'numerable-icon 'type)
 "@version{2022-12-21}
  @begin{short}
    The @class{gtk:numerable-icon} class is a subclass of the
    @class{g:emblemed-icon} class that can show a number or short string as an
    emblem.
  @end{short}
  The number can be overlayed on top of another emblem, if desired. It supports
  theming by taking font and color information from a provided
  @class{gtk:style-context} object.
  @begin[Examples]{dictionary}
    Typical numerable icons:

    @image[numerableicon]{} @image[numerableicon2]{}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:numerable-icon} class has been deprecated since version 3.14
    and should not be used in newly written code.
  @end{dictionary}
  @see-slot{gtk:numerable-icon-background-icon}
  @see-slot{gtk:numerable-icon-background-icon-name}
  @see-slot{gtk:numerable-icon-count}
  @see-slot{gtk:numerable-icon-label}
  @see-slot{gtk:numerable-icon-style-context}
  @see-constructor{gtk:numerable-icon-new}
  @see-constructor{gtk:numerable-icon-new-with-style-context}
  @see-class{g:emblemed-icon}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- numerable-icon-background-icon -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-icon"
                                               'numerable-icon) t)
 "The @code{background-icon} property of type @class{g:icon} (Read / Write)
  @br{}
  The icon for the number emblem background.")

#+liber-documentation
(setf (liber:alias-for-function 'numerable-icon-background-icon)
      "Accessor"
      (documentation 'numerable-icon-background-icon 'function)
 "@version{2022-12-21}
  @syntax{(gtk:numerable-icon-background-icon object) => icon}
  @syntax{(setf (gtk:numerable-icon-background-icon object) icon)}
  @argument[object]{a @class{gtk:numerable-icon} object}
  @argument[icon]{a @class{g:icon} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:numerable-icon]{background-icon} slot of the
    @class{gtk:numerable-icon} class.
  @end{short}
  The @fun{gtk:numerable-icon-background-icon} function returns the icon that
  was set as the base background image, or @code{nil} if there is none. The
  @setf{gtk:numerable-icon-background-icon} function updates the background
  icon.

  If @arg{icon} is @code{nil}, the numerable icon will go back using style
  information or default theming for its background image.

  If this method is called and an icon name was already set as background for
  the numerable icon, @arg{icon} will be used, i.e. the last method called
  between the @fun{gtk:numerable-icon-background-icon} and
  @fun{gtk:numerable-icon-background-icon-name} functions has always priority.
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-background-icon} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-function{gtk:numerable-icon-background-icon-name}")

;;; --- numerable-icon-background-icon-name ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-icon-name"
                                               'numerable-icon) t)
 "The @code{background-icon-name} property of type @code{:string} (Read / Write)
  @br{}
  The icon name for the number emblem background. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'numerable-icon-background-icon-name)
      "Accessor"
      (documentation 'numerable-icon-background-icon-name 'function)
 "@version{2022-12-21}
  @syntax{(gtk:numerable-icon-background-icon-name object) => name}
  @syntax{(setf (gtk:numerable-icon-background-icon-name object) name)}
  @argument[object]{a @class{gtk:numerable-icon} object}
  @argument[name]{a string with an icon name, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:numerable-icon]{background-icon-name} slot of the
    @class{gtk:numerable-icon} class.
  @end{short}
  The @fun{gtk:numerable-icon-background-icon-name} function returns the icon
  name used as the base background image, or @code{nil} if there is none. The
  @setf{gtk:numerable-icon-background-icon-name} function updates the background
  icon.

  If @arg{name} is @code{nil}, the numerable icon will go back using style
  information or default theming for its background image.

  If this method is called and a @class{g:icon} object was already set as
  background for the numerable icon, @arg{name} will be used, i.e. the last
  method called between the @fun{gtk:numerable-icon-background-icon-name} and
  @fun{gtk:numerable-icon-background-icon} functions has always priority.
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-backgroun-icon-name} function has been
    deprecated since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-function{gtk:numerable-icon-background-icon}")

;;; --- numerable-icon-count ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "count" 'numerable-icon) t)
 "The @code{count} property of type @code{:int} (Read / Write) @br{}
  The count of the emblem currently displayed. @br{}
  Allowed values: [-99,99] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'numerable-icon-count)
      "Accessor"
      (documentation 'numerable-icon-count 'function)
 "@version{2022-12-21}
  @syntax{(gtk:numerable-icon-count object) => count}
  @syntax{(setf (gtk:numerable-icon-count object) count)}
  @argument[object]{a @class{gtk:numerable-icon} object}
  @argument[count]{an integer between -99 and 99}
  @begin{short}
    Accessor of the @slot[gtk:numerable-icon]{count} slot of the
    @class{gtk:numerable-icon} class.
  @end{short}
  The @fun{gtk:numerable-icon-count} function returns the value currently
  displayed by the numerable icon. The @setf{gtk:numerable-icon-count} function
  sets the currently displayed value.

  The numeric value is always clamped to make it two digits, i.e. between -99
  and 99. Setting a count of zero removes the emblem. If this method is called,
  and a label was already set on the numerable icon, it will automatically be
  reset to @code{nil} before rendering the number, i.e. the last method called
  between the @fun{gtk:numerable-icon-count} and @fun{gtk:numerable-icon-label}
  functions has always priority.
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-count} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-function{gtk:numerable-icon-label}")

;;; --- numerable-icon-label ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'numerable-icon) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label to be displayed over the icon. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'numerable-icon-label)
      "Accessor"
      (documentation 'numerable-icon-label 'function)
 "@version{2022-12-21}
  @syntax{(gtk:numerable-icon-label object) => label}
  @syntax{(setf (gtk:numerable-icon-label object) label)}
  @argument[object]{a @class{gtk:numerable-icon} object}
  @argument[label]{a string with a short label, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:numerable-icon]{label} slot of the
    @class{gtk:numerable-icon} class.
  @end{short}
  The @fun{gtk:numerable-icon-label} function returns the currently displayed
  label of the numerable icon, or @code{nil}. The
  @setf{gtk:numerable-icon-label} function sets the currently displayed label.
  Setting an empty label removes the emblem.

  Note that this is meant for displaying short labels, such as roman numbers,
  or single letters. For roman numbers, consider using the Unicode characters
  U+2160 - U+217F. Strings longer than two characters will likely not be
  rendered very well.

  If this method is called, and a number was already set on the icon, it will
  automatically be reset to zero before rendering the label, i.e. the last
  method called between the @fun{gtk:numerable-icon-label} and
  @fun{gtk:numerable-icon-count} functions has always priority.
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-label} function has been deprecated since
    version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-function{gtk:numerable-icon-count}")

;;; --- numerable-icon-style-context -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "style-context"
                                               'numerable-icon) t)
 "The @code{style-context} property of type @class{gtk:style-context}
  (Read / Write) @br{}
  The style context to theme the icon appearance.")

#+liber-documentation
(setf (liber:alias-for-function 'numerable-icon-style-context)
      "Accessor"
      (documentation 'numerable-icon-style-context 'function)
 "@version{2022-12-21}
  @syntax{(gtk:numerable-icon-style-context object) => style}
  @syntax{(setf (gtk:numerable-icon-style-context object) style)}
  @argument[object]{a @class{gtk:numerable-icon} object}
  @argument[style]{a @class{gtk:style-context} object}
  @begin{short}
    Accessor of the @slot[gtk:numerable-icon]{style-context} slot of the
    @class{gtk:numerable-icon} class.
  @end{short}
  The @fun{gtk:numerable-icon-style-context} function returns the style context
  used by the numerable icon for theming, or @code{nil} if there is none. The
  @setf{gtk:numerable-icon-style-context} function updates the numerable icon
  to fetch theme information.
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-style-context} function has been deprecated
    since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline numerable-icon-new))

(defun numerable-icon-new (icon)
 #+liber-documentation
 "@version{2022-12-21}
  @argument[icon]{a @class{g:icon} object to overlay on}
  @return{The new @class{gtk:numerable-icon} object.}
  @short{Creates a new unthemed numerable icon.}
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-new} function has been deprecated since version
    3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-class{g:icon}"
  (make-instance 'numerable-icon
                 :background-icon icon))

(export 'numerable-icon-new)

;;; ----------------------------------------------------------------------------
;;; gtk_numerable_icon_new_with_style_context ()
;;; ----------------------------------------------------------------------------

(declaim (inline numerable-icon-new-with-style-context))

(defun numerable-icon-new-with-style-context (icon context)
 #+liber-documentation
 "@version{2022-12-21}
  @argument[icon]{a @class{g:icon} object to overlay on}
  @argument[context]{a @class{gtk:style-context} object}
  @return{The new @class{gtk:numerable-icon} object.}
  @begin{short}
    Creates a new numerable icon which will themed according to the passed
    style context.
  @end{short}
  This is a convenience constructor that calls the function
  @fun{gtk:numerable-icon-style-context} internally.
  @begin[Warning]{dictionary}
    The @fun{gtk:numerable-icon-new-with-style-context} function has been
    deprecated since version 3.14 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:numerable-icon}
  @see-class{gtk:style-context}
  @see-function{gtk:numerable-icon-style-context}"
  (make-instance 'numerable-icon
                 :background-icon icon
                 :style-context context))

(export 'numerable-icon-new-with-style-context)

;;; --- End of file gtk3.numerable-icon.lisp -----------------------------------
