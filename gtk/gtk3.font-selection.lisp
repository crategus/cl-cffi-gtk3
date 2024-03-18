;;; ----------------------------------------------------------------------------
;;; gtk3.font-selection.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.6.4 and modified to document the Lisp binding to the GTK library.
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
;;; GtkFontSelection
;;;
;;;     Deprecated widget for selecting fonts
;;;
;;; Synopsis
;;;
;;;     GtkFontSelection
;;;
;;;     gtk_font_selection_new
;;;     gtk_font_selection_get_font_name
;;;     gtk_font_selection_set_font_name
;;;     gtk_font_selection_get_preview_text
;;;     gtk_font_selection_set_preview_text
;;;     gtk_font_selection_get_face
;;;     gtk_font_selection_get_face_list
;;;     gtk_font_selection_get_family
;;;     gtk_font_selection_get_size
;;;     gtk_font_selection_get_family_list
;;;     gtk_font_selection_get_preview_entry
;;;     gtk_font_selection_get_size_entry
;;;     gtk_font_selection_get_size_list
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontSelection
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFontSelection" font-selection
  (:superclass box
   :export nil
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_font_selection_get_type")
  ((font-name
    font-selection-font-name
    "font-name" "gchararray" t t)
   (preview-text
    font-selection-preview-text
    "preview-text" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'font-selection 'type)
 "@version{#2013-6-18}
  @subheading{Warning}
    The @class{gtk:font-selection} widget is deprecated and should not be used
    in newly written code. Use the @class{gtk:font-chooser} widget.

  @begin{short}
    The @class{gtk:font-selection} widget lists the available fonts, styles and
    sizes, allowing the user to select a font. It is used in the
    @class{gtk:font-selection-dialog} widget to provide a dialog box for
    selecting fonts.
  @end{short}

  To set the font which is initially selected, use the
  @fun{gtk:font-selection-set-font-name} function.

  To get the selected font use the @fun{gtk:font-selection-get-font-name}
  function.

  To change the text which is shown in the preview area, use the
  @fun{gtk:font-selection-set-preview-text} function.

  In GTK 3.2, the @class{gtk:font-selection} widget has been deprecated in
  favor of the @class{gtk:font-chooser} widget.
  @see-slot{gtk:font-selection-font-name}
  @see-slot{gtk:font-selection-preview-text}
  @see-function{gtk:font-selection-set-font-name}
  @see-function{gtk:font-selection-get-font-name}
  @see-function{gtk:font-selection-set-preview-text}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-selection-font-name -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-name" 'font-selection) t)
 "The @code{font-name} property of type @code{:string} (Read / Write) @br{}
  The string that represents this font. @br{}
  Default value: \"Sans 10\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-font-name)
      "Accessor"
      (documentation 'font-selection-font-name 'function)
 "@version{#2013-6-18}
  Accessor of the @slot[gtk:font-selection]{font-name} slot of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; --- gtk:font-selection-preview-text ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "preview-text"
                                               'font-selection) t)
 "The @code{preview-text} property of type @code{:string}
  (Read / Write) @br{}
  The text to display in order to demonstrate the selected font. @br{}
  Default value: \"abcdefghijk ABCDEFGHIJK\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-preview-text)
      "Accessor"
      (documentation 'font-selection-preview-text 'function)
 "@version{#2013-6-19}
  Accessor of the @slot[gtk:font-selection]{preview-text} slot of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-selection-child-expand ----------------------------------------

(define-child-property font-selection-child-expand
                       "expand" "gboolean" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-child-expand)
      "Accessor"
      (documentation 'font-selection-child-expand 'function)
 "@version{#2013-8-28}
  Accessor of the @code{expand} child property of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; --- gtk:font-selection-child-fill ------------------------------------------

(define-child-property font-selection-child-fill
                       "fill" "gboolean" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-child-fill)
      "Accessor"
      (documentation 'font-selection-child-fill 'function)
 "@version{#2013-8-28}
  Accessor of the @code{fill} child property of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; --- gtk:font-selection-child-padding ---------------------------------------

(define-child-property font-selection-child-padding
                       "padding" "guint" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-child-padding)
      "Accessor"
      (documentation 'font-selection-child-padding 'function)
 "@version{#2013-8-28}
  Accessor of the @code{padding} child property of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; --- gtk:font-selection-child-pack-type -------------------------------------

(define-child-property font-selection-child-pack-type
                       "pack-type" "GtkPackType" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-child-pack-type)
      "Accessor"
      (documentation 'font-selection-child-pack-type 'function)
 "@version{#2013-8-28}
  Accessor of the @code{pack-type} child property of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; --- gtk:font-selection-child-position --------------------------------------

(define-child-property font-selection-child-position
                       "position" "gint" t t nil)

#+liber-documentation
(setf (liber:alias-for-function 'font-selection-child-position)
      "Accessor"
      (documentation 'font-selection-child-position 'function)
 "@version{#2013-8-28}
  Accessor of the @code{position} child property of the
  @class{gtk:font-selection} class.
  @see-class{gtk:font-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-selection-new))

(defun font-selection-new ()
 #+liber-documentation
 "@version{#2013-6-24}
  @return{A new @class{gtk:font-selection} widget.}
  @begin{short}
    Creates a new @class{gtk:font-selection} widget.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:font-selection-new} function is deprecated and should not be
    used in newly written code. Use @class{gtk:font-chooser}.
  @end{dictionary}
  @see-class{gtk:font-selection}"
  (make-instance 'font-selection-new))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_font_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-selection-get-font-name))

(defun font-selection-get-font-name (fontsel)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{A string with the name of the current font, or @code{nil} if no font
    is selected.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-font-name} function has been deprecated
    since version 3.2 and should not be used in newly written code. Use
    @class{gtk:font-chooser}.

  @begin{short}
    Gets the currently selected font name.
  @end{short}

  Note that this can be a different string than what you set with the
  @fun{gtk:font-selection-set-font-name} function, as the font selection widget
  may normalize font names and thus return a string with a different structure.
  For example, \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\". Use the @fun{pango:font-description-equal}
  function if you want to compare two font descriptions.
  @see-function{gtk:font-selection-set-font-name}
  @see-function{pango:font-description-equal}"
  (font-selection-font-name fontsel))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_font_name ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-selection-set-font-name))

(defun font-selection-set-font-name (fontsel fontname)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @argument[fontname]{a font name like \"Helvetica 12\" or \"Times Bold 18\"}
  @begin{return}
    @em{True} if the font could be set successfully; @code{nil} if no such font
    exists or if the @arg{fontsel} does not belong to a particular screen yet.
  @end{return}
  @subheading{Warning}
    The @fun{gtk:font-selection-set-font-name} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    Sets the currently selected font.
  @end{short}

  Note that the @arg{fontsel} needs to know the screen in which it will appear
  for this to work; this can be guaranteed by simply making sure that the
  @arg{fontsel} is inserted in a toplevel window before you call this function.
  @see-function{gtk:font-selection-get-font-name}"
  (setf (font-selection-font-name fontsel) fontname))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-selection-get-preview-text))

(defun font-selection-get-preview-text (fontsel)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{The text displayed in the preview area.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-preview-text} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    Gets the text displayed in the preview area.
  @end{short}
  @see-function{gtk:font-selection-set-preview-text}"
  (font-selection-preview-text fontsel))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_set_preview_text ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-selection-set-preview-text))

(defun font-selection-set-preview-text (fontsel text)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @argument[text]{the text to display in the preview area}
  @subheading{Warning}
    The @fun{gtk:font-selection-set-preview-text} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    Sets the text displayed in the preview area. The text is used to show how
    the selected font looks.
  @end{short}
  @see-function{gtk:font-selection-get-preview-text}"
  (setf (font-selection-preview-text fontsel) text))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_face" font-selection-get-face)
    (g:object pango-font-face)
 #+liber-documentation
 "@version{#2013-7-1}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @begin{return}
    A @class{pango-font-face} representing the selected font group details.
  @end{return}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-face} function has been deprecated since
    version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    Gets the @class{pango-font-face} representing the selected font group
    details (i.e. family, slant, weight, width, etc).
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_face_list ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_face_list" font-selection-get-face-list)
    (g:object widget)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{A @class{gtk:widget} that is part of fontsel.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-face-list} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    This returns the @class{gtk:tree-view} which lists all styles available for
    the selected font. For example, 'Regular', 'Bold', etc.
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_family" font-selection-get-family)
    (g:object pango-font-family)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @begin{return}
    A @class{pango-font-family} representing the selected font family. Font
    families are a collection of font faces. The returned object is owned by
    @arg{fontsel} and must not be modified or freed.
  @end{return}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-family} function has been deprecated since
    version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    Gets the @class{pango-font-family} representing the selected font family.
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_size" font-selection-get-size) :int
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @begin{return}
    An integer representing the selected font size, or -1 if no font size
    is selected.
  @end{return}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-size} function has been deprecated since
    version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @short{The selected font size.}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_family_list ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_family_list"
               font-selection-get-family-list) (g:object widget)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{A @class{gtk:widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-family-list} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    This returns the @class{gtk:tree-view} that lists font families, for
    example, 'Sans', 'Serif', etc.
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_preview_entry ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_preview_entry"
               font-selection-get-preview-entry) (g:object widget)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{A @class{gtk:widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-preview-entry} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    This returns the @class{gtk:entry} used to display the font as a preview.
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_entry ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_size_entry"
               font-selection-get-size-entry) (g:object widget)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{A @class{gtk:widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-size-entry} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    This returns the @class{gtk:entry} used to allow the user to edit the font
    number manually instead of selecting it from the list of font sizes.
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; ----------------------------------------------------------------------------
;;; gtk_font_selection_get_size_list ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_selection_get_size_list" font-selection-get-size-list)
    (g:object widget)
 #+liber-documentation
 "@version{#2013-6-24}
  @argument[fontsel]{a @class{gtk:font-selection} widget}
  @return{A @class{gtk:widget} that is part of @arg{fontsel}.}
  @subheading{Warning}
    The @fun{gtk:font-selection-get-size-list} function has been deprecated
    since version 3.2 and should not be used in newly written code.
    Use @class{gtk:font-chooser}.

  @begin{short}
    This returns the @class{gtk:tree-view} used to list font sizes.
  @end{short}
  @see-class{gtk:font-selection}"
  (fontsel (g:object font-selection)))

;;; --- End of file gtk3.font-selection.lisp -----------------------------------
