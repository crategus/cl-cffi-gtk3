;;; ----------------------------------------------------------------------------
;;; gtk3.text-attributes.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2020 - 2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkTextAppearance                               <--- gtk.text-tag.lisp
;;;     GtkTextAttributes                               <--- gtk.text-tag.lisp
;;;-----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkTextAppearance
;;;
;;; struct GtkTextAppearance {
;;;   GdkColor bg_color; /* pixel is taken for underline color */
;;;   GdkColor fg_color; /* pixel is taken for strikethrough color */
;;;
;;;   /* super/subscript rise, can be negative */
;;;   gint rise;
;;;
;;;   guint underline : 4;          /* PangoUnderline */
;;;   guint strikethrough : 1;
;;;
;;;   /* Whether to use background-related values; this is irrelevant for
;;;    * the values struct when in a tag, but is used for the composite
;;;    * values struct; it's true if any of the tags being composited
;;;    * had background stuff set.
;;;    */
;;;   guint draw_bg : 1;
;;;
;;;   /* These are only used when we are actually laying out and rendering
;;;    * a paragraph; not when a GtkTextAppearance is part of a
;;;    * GtkTextAttributes.
;;;    */
;;;   guint inside_selection : 1;
;;;   guint is_text : 1;
;;;
;;;   /* For the sad story of this bit of code, see
;;;    * https://bugzilla.gnome.org/show_bug.cgi?id=711158
;;;    */
;;;   #ifdef __GI_SCANNER__
;;;   /* The scanner should only see the transparent union, so that its
;;;    * content does not vary across architectures.
;;;    */
;;;   union {
;;;     GdkRGBA *rgba[2];
;;; };
;;;
;;; Members
;;;
;;; GdkColor bg_color;
;;;     Background GdkColor.
;;;
;;; GdkColor fg_color;
;;;     Foreground GdkColor.
;;;
;;; gint rise;
;;;     Super/subscript rise, can be negative.
;;;
;;; guint underline : 4;
;;;     PangoUnderline
;;;
;;; guint strikethrough : 1;
;;;     Strikethrough style
;;;
;;; guint draw_bg : 1;
;;;     Whether to use background-related values; this is irrelevant for the
;;;     values struct when in a tag, but is used for the composite values
;;;     struct; it’s true if any of the tags being composited had background
;;      stuff set.
;;;
;;; guint inside_selection : 1;
;;;     This are only used when we are actually laying out and rendering a
;;;     paragraph; not when a GtkTextAppearance is part of a GtkTextAttributes.
;;;
;;; guint is_text : 1;
;;;     This are only used when we are actually laying out and rendering a
;;;     paragraph; not when a GtkTextAppearance is part of a GtkTextAttributes.
;;;
;;; GdkRGBA *rgba[2];
;;;     GdkRGBA
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkTextAttributes
;;; ----------------------------------------------------------------------------

;; This implementation crashes the testsuite, when copying the structure.
;; We do not implement this structure and the corresponding functions.

(glib:define-g-boxed-cstruct text-attributes "GtkTextAttributes"
  (:export t
   :type-initializer "gtk_text_attributes_get_type")
  (refcount :uint :initform 0) ; private field
  (appearance :pointer :initform (cffi:null-pointer))  ; type is text-appearance
  (justification justification :initform :left)
  (direction text-direction :initform :none)
  (font (g:boxed pango:font-description))
  (font-scale :double :initform 0.0d0)
  (left-margin :int :initform 0)
  (right-margin :int :initform 0)
  (indent :int :initform 0)
  (pixels-above-lines :int :initform 0)
  (pixels-below-lines :int :initform 0)
  (pixels-inside-wrap :int :initform 0)
  (tabs :pointer :initform (cffi:null-pointer))        ; type is pango-tab-array
  (wrap-mode wrap-mode :initform :none)
  (language (g:boxed pango:language))
  (bg-color (g:boxed gdk:color)) ; private field
  (invisible :uint :initform 1)
  (bg-full-height :uint :initform 1)
  (editable :uint :initform 1)
  (no-fallback :uint :initform 1)
  (bg-rgba (g:boxed gdk:rgba)) ; private field
  (letter-spacing :int :initform 0)
  (font-features :pointer :initform (cffi:null-pointer)) ; type is char*
  (dummy1 :pointer)
  (dummy2 :pointer))

#+liber-documentation
(setf (liber:alias-for-class 'text-attributes)
      "GBoxed"
      (documentation 'text-attributes 'type)
 "@version{#2021-8-19}
  @begin{short}
    Using the @sym{gtk:text-attributes} structure directly should rarely be
    necessary.
  @end{short}
  It is primarily useful with the @fun{gtk:text-iter-attributes} function. As
  with most GTK structures, the fields in this structure should only be read,
  never modified directly.
  @begin{pre}
(glib:define-g-boxed-cstruct gtk:text-attributes \"GtkTextAttributes\"
  (:export t
   :type-initializer \"gtk_text_attributes_get_type\")
  (appearance :pointer :initform (cffi:null-pointer))
  (justification justification)
  (direction text-direction)
  (font (g:boxed pango:font-description))
  (font-scale :double)
  (left-margin :int)
  (right-margin :int)
  (indent :int)
  (pixels-above-lines :int)
  (pixels-below-lines :int)
  (pixels-inside-wrap :int)
  (tabs :pointer)             ; type is pango-tab-array
  (wrap-mode wrap-mode)
  (language (g:boxed pango:language))
  (invisible :uint)
  (bg-full-height :uint)
  (editable :uint)
  (no-fallback :uint)
  (letter-spacing :int)
  (font-features :string))
  @end{pre}
  @begin[code]{table}
    @entry[appearance]{Pointer to a @code{GtkTextAppearance} structure for
      text.}
    @entry[justification]{A value of the @symbol{gtk:justification} enumeration
      for text.}
    @entry[direction]{A value of the @symbol{gtk:text-direction} enumeration
      for text.}
    @entry[font]{The @class{pango:font-description} structure for text.}
    @entry[font-scale]{A double float with the font scale factor.}
    @entry[left-margin]{An integer with the width of the left margin,
      in pixels.}
    @entry[right-margin]{An integer with the width of the right margin,
      in pixels.}
    @entry[indent]{An integer with the amount to indent the paragraph,
      in pixels.}
    @entry[pixels-above-lines]{An integer with the pixels of blank space above
      paragraphs.}
    @entry[pixels-below-lines]{An integer with the pixels of blank space below
      paragraphs.}
    @entry[pixels-inside-wrap]{An integer with the pixels of blank space
      between wrapped lines in a paragraph.}
    @entry[tabs]{Pointer to a custom @class{pango-tab-array} structure for
      this text.}
    @entry[wrap-mode]{A value of the @symbol{gtk:wrap-mode} enumeration for
      text.}
    @entry[language]{Pointer to a @class{pango:language} structure for text.}
    @entry[invisible]{An unsigned integer whether to hide the text.}
    @entry[bg-full-height]{Whether background is fit to full line height rather
      than baseline +/- ascent/descent (font height).}
    @entry[editable]{Whether the text is editable.}
    @entry[no-fallback]{Whether to disable font fallback.}
    @entry[letter-spacing]{An integer with the extra space to insert between
      graphemes, in Pango units.}
    @entry[font-features]{A string with font features.}
  @end{table}
  @see-slot{gtk:text-attributes-appearance}
  @see-slot{gtk:text-attributes-justification}
  @see-slot{gtk:text-attributes-direction}
  @see-slot{gtk:text-attributes-font}
  @see-slot{gtk:text-attributes-font-scale}
  @see-slot{gtk:text-attributes-left-margin}
  @see-slot{gtk:text-attributes-right-margin}
  @see-slot{gtk:text-attributes-indent}
  @see-slot{gtk:text-attributes-pixels-above-lines}
  @see-slot{gtk:text-attributes-pixels-below-lines}
  @see-slot{gtk:text-attributes-pixels-inside-wrap}
  @see-slot{gtk:text-attributes-tabs}
  @see-slot{gtk:text-attributes-wrap-mode}
  @see-slot{gtk:text-attributes-language}
  @see-slot{gtk:text-attributes-invisible}
  @see-slot{gtk:text-attributes-bg-full-height}
  @see-slot{gtk:text-attributes-editable}
  @see-slot{gtk:text-attributes-no-fallback}
  @see-slot{gtk:text-attributes-letter-spacing}
  @see-slot{gtk:text-attributes-font-features}
  @see-function{gtk:text-iter-attributes}")

;; Unexport the private field of the GtkTextAttributes structure
(unexport 'text-attributes-refcount)
(unexport 'text-attributes-bg-color)
(unexport 'text-attributes-bg-rgba)

;;; ----------------------------------------------------------------------------
;;; Constructors for GtkTextAttributes                     not exported
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation 'make-text-attributes 'function)
 "@version{#2021-8-19}
  @begin{short}
    Creates and returns a structure of type @class{gtk:text-attributes}.
  @end{short}
  @see-class{gtk:text-attributes}
  @see-function{copy-gtk:text-attributes}")

#+liber-documentation
(setf (documentation 'copy-text-attributes 'function)
 "@version{#2021-8-19}
  @begin{short}
    Copies and returns a structure of type @class{gtk:text-attributes}.
  @end{short}
  @see-class{gtk:text-attributes}
  @see-function{make-gtk:text-attributes}")

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkTextAttributes                        not exported
;;; ----------------------------------------------------------------------------

;;; --- text-attributes-appearance ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-appearance)
      "Accessor"
      (documentation 'text-attributes-appearance 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{appearance} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  Pointer to a @code{GtkTextAppearance} structure for text.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-justification ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-justification)
      "Accessor"
      (documentation 'text-attributes-justification 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{justification} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  A value of the @symbol{gtk:justification} enumeration for text.
  @see-class{gtk:text-attributes}
  @see-symbol{gtk:justification}")

;;; --- text-attributes-direction ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-direction)
      "Accessor"
      (documentation 'text-attributes-direction 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{direction} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  A value of the @symbol{gtk:text-direction} enumeration for text.
  @see-class{gtk:text-attributes}
  @see-symbol{gtk:text-direction}")

;;; --- text-attributes-font ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-font)
      "Accessor"
      (documentation 'text-attributes-font 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{font} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  The @class{pango:font-description} structure for text.
  @see-class{gtk:text-attributes}
  @see-class{pango:font-description}")

;;; --- text-attributes-font-scale ---------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-font-scale)
      "Accessor"
      (documentation 'text-attributes-font-scale 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{font-scale} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  A double float with the font scale factor.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-left-margin --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-left-margin)
      "Accessor"
      (documentation 'text-attributes-left-margin 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{left-margin} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  An integer with the width of the left margin, in pixels.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-right-margin -------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-right-margin)
      "Accessor"
      (documentation 'text-attributes-right-margin 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{right-margin} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  An integer with the width of the right margin, in pixels.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-indent -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-indent)
      "Accessor"
      (documentation 'text-attributes-indent 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{indent} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  An integer with the amount to indent the paragraph, in pixels.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-pixels-above-lines -------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-pixels-above-lines)
      "Accessor"
      (documentation 'text-attributes-pixels-above-lines 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{pixels-above-lines} slot of the
    @class{gtk:text-attributes} structure.
  @end{short}
  An integer with the pixels of blank space above paragraphs.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-pixels-below-lines -------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-pixels-below-lines)
      "Accessor"
      (documentation 'text-attributes-pixels-below-lines 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{pixels-below-lines} slot of the
    @class{gtk:text-attributes} structure.
  @end{short}
  An integer with the pixels of blank space below paragraphs.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-pixels-inside-wrap -------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-pixels-inside-wrap)
      "Accessor"
      (documentation 'text-attributes-pixels-inside-wrap 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{pixels-inside-wrap} slot of the
    @class{gtk:text-attributes} structure.
  @end{short}
  An integer with the pixels of blank space between wrapped lines in a
  paragraph.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-tabs ---------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-tabs)
      "Accessor"
      (documentation 'text-attributes-tabs 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{tabs} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  Pointer to a custom @class{pango-tab-array} instance for this text.
  @see-class{gtk:text-attributes}
  @see-class{pango-tab-array}")

;;; --- text-attributes-wrap-mode ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-wrap-mode)
      "Accessor"
      (documentation 'text-attributes-wrap-mode 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{wrap-mode} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  A value of the @symbol{gtk:wrap-mode} enumeration for text.
  @see-class{gtk:text-attributes}
  @see-symbol{gtk:wrap-mode}")

;;; --- text-attributes-language -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-language)
      "Accessor"
      (documentation 'text-attributes-language 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{language} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  Pointer to a @class{pango:language} instance for text.
  @see-class{gtk:text-attributes}
  @see-class{pango:language}")

;;; --- text-attributes-invisible ----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-invisible)
      "Accessor"
      (documentation 'text-attributes-invisible 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{invisible} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  An unsigned integer whether to hide the text.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-bg-full-height -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-bg-full-height)
      "Accessor"
      (documentation 'text-attributes-bg-full-height 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{bg-full-height} slot of the
    @class{gtk:text-attributes} structure.
  @end{short}
  Whether background is fit to full line height rather than baseline +/-
  ascent/descent (font height).
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-editable -----------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-editable)
      "Accessor"
      (documentation 'text-attributes-editable 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{editable} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  Whether the text is editable.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-no-fallback --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-no-fallback)
      "Accessor"
      (documentation 'text-attributes-no-fallback 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{no-fallback} slot of the @class{gtk:text-attributes}
    structure.
  @end{short}
  Whether to disable font fallback.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-letter-spacing -----------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-letter-spacing)
      "Accessor"
      (documentation 'text-attributes-letter-spacing 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{letter-spacing} slot of the
    @class{gtk:text-attributes} structure.
  @end{short}
  An integer with the extra space to insert between graphemes, in Pango units.
  @see-class{gtk:text-attributes}")

;;; --- text-attributes-font-features ------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes-font-features)
      "Accessor"
      (documentation 'text-attributes-font-features 'function)
 "@version{#2021-8-19}
  @begin{short}
    Accessor of the @code{font-features} slot of the
    @class{gtk:text-attributes} structure.
  @end{short}
  A string with font features.
  @see-class{gtk:text-attributes}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_new ()
;;; ----------------------------------------------------------------------------

(defun text-attributes-new ()
 #+liber-documentation
 "@version{#2021-8-19}
  @return{A new @class{gtk:text-attributes} instance.}
  @begin{short}
    Creates a @class{gtk:text-attributes} instance, which describes a set of
    properties on some text.
  @end{short}
  @see-class{gtk:text-attributes}"
  (make-text-attributes))

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy ()
;;; ----------------------------------------------------------------------------

(defun text-attributes-copy (src)
 #+liber-documentation
 "@version{#2021-8-19}
  @argument[src]{a @class{gtk:text-attributes} instance to be copied}
  @return{A copy of @arg{src}.}
  @begin{short}
    Copies @arg{src} and returns a new @class{gtk:text-attributes} instance.
  @end{short}
  @see-class{gtk:text-attributes}"
  (copy-text-attributes src))

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_copy_values ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_attributes_copy_values" text-attributes-copy-values)
    :void
 #+liber-documentation
 "@version{#2021-8-19}
  @argument[src]{a @class{gtk:text-attributes} instance}
  @argument[dest]{another @class{gtk:text-attributes} instance}
  @begin{short}
    Copies the values from @arg{src} to @arg{dest} so that @arg{dest} has the
    same values as @arg{src}.
  @end{short}
  @see-class{gtk:text-attributes}"
  (src (g:boxed text-attributes))
  (dest (g:boxed text-attributes)))

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_unref ()
;;;
;;; void gtk_text_attributes_unref (GtkTextAttributes *values);
;;;
;;; Decrements the reference count on values, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; values :
;;;     a GtkTextAttributes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_attributes_ref ()
;;;
;;; GtkTextAttributes * gtk_text_attributes_ref (GtkTextAttributes *values);
;;;
;;; Increments the reference count on values.
;;;
;;; values :
;;;     a GtkTextAttributes
;;;
;;; Returns :
;;;     the GtkTextAttributes that were passed in
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.text-attributes.lisp ----------------------------------
