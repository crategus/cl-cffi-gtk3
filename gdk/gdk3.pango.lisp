;;; ----------------------------------------------------------------------------
;;; gdk3.pango.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
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
;;; Pango Interaction
;;;
;;;     Using Pango in GDK
;;;
;;; Functions
;;;
;;;     gdk_pango_layout_get_clip_region
;;;     gdk_pango_layout_line_get_clip_region
;;;     gdk_pango_context_get
;;;     gdk_pango_context_get_for_screen
;;;     gdk_pango_context_get_for_display
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_get_clip_region () -> pango-layout-clip-region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pango_layout_get_clip_region" %pango-layout-clip-region)
    (:pointer (:struct cairo:region-t))
  (layout (g:object pango-layout))
  (x-origin :int)
  (y-origin :int)
  (ranges (:pointer :int))
  (n-ranges :int))

(defun pango-layout-clip-region (layout x-origin y-origin ranges)
 #+liber-documentation
 "@version{#2021-4-5}
  @argument[layout]{a @class{pango:layout} object}
  @argument[x-origin]{an integer with the x pixel where you intend to draw the
    layout with this clip}
  @argument[y-origin]{an integer with the y pixel where you intend to draw the
    layout with this clip}
  @argument[ranges]{a list of byte indexes into the layout, where even members
    of the list are start indexes and odd elements are end indexes}
  @return{A @symbol{cairo:region-t} clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  The argumentes @arg{x-origin} and @arg{y-origin} are the top left point to
  center the layout. The @arg{ranges} list should contain ranges of bytes in
  the layout's text.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn layout may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango:layout}
  @see-symbol{cairo:region-t}"
  (let ((n (length ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (cffi:with-foreign-object (ranges-ar :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (cffi:mem-aref ranges-ar :int i) x)
                 (incf i))
               ranges))
        (%pango-layout-clip-region layout
                                   x-origin
                                   y-origin
                                   ranges-ar
                                   n-ranges)))))

(export 'pango-layout-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_layout_line_get_clip_region () -> pango-layout-line-clip-region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pango_layout_line_get_clip_region"
          %pango-layout-line-clip-region)
    (:pointer (:struct cairo:region-t))
  (layout-line (g:boxed pango:layout-line))
  (x-origin :int)
  (y-origin :int)
  (ranges (:pointer :int))
  (n-ranges :int))

(defun pango-layout-line-clip-region (line x-origin y-origin ranges)
 #+liber-documentation
 "@version{#2021-4-5}
  @argument[line]{a @class{pango:layout-line} instance}
  @argument[x-origin]{an integer with the x pixel where you intend to draw the
    layout line with this clip}
  @argument[y-origin]{an integer with the baseline pixel where you intend to
    draw the layout line with this clip}
  @argument[ranges]{list of byte indexes into the layout, where even members of
    list are start indexes and odd elements are end indexes}
  @return{A @symbol{cairo:region-t} clip region containing the given ranges.}
  @begin{short}
    Obtains a clip region which contains the areas where the given ranges of
    text would be drawn.
  @end{short}
  The arguments @arg{x-origin} and @arg{y-origin} are the top left position of
  the layout. The @arg{ranges} list should contain ranges of bytes in the
  layout's text. The clip region will include space to the left or right of the
  line, to the layout bounding box, if you have indexes above or below the
  indexes contained inside the line. This is to draw the selection all the way
  to the side of the layout. However, the clip region is in line coordinates,
  not layout coordinates.

  Note that the regions returned correspond to logical extents of the text
  ranges, not ink extents. So the drawn line may in fact touch areas out of
  the clip region. The clip region is mainly useful for highlightling parts of
  text, such as when text is selected.
  @see-class{pango:layout-line}
  @see-symbol{cairo:region-t}"
  (let ((n (length ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (cffi:with-foreign-object (ranges-ar :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (cffi:mem-aref ranges-ar :int i) x)
                 (incf i))
               ranges))
        (%pango-layout-line-clip-region line
                                        x-origin
                                        y-origin
                                        ranges-ar
                                        n-ranges)))))

(export 'pango-layout-line-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pango_context_get" pango-context-get)
    (g:object pango:context :already-referenced)
 #+liber-documentation
 "@version{#2021-10-28}
  @return{A new @class{pango-context} instance for the default GDK screen.}
  @begin{short}
    Creates a @class{pango-context} instance for the default GDK screen.
  @end{short}

  When using GTK, normally you should use the @fun{gtk:widget-pango-context}
  function instead of this function, to get the appropriate Pango context for
  the widget you intend to render text onto.

  The newly created Pango context will have the default font options, see the
  @symbol{cairo:font-options-t} API, for the default screen. If these options
  change it will not be updated. Using the @fun{gtk:widget-pango-context}
  function is more convenient if you want to keep a Pango context around and
  track changes to the font rendering settings of the screen.
  @see-class{pango:context}
  @see-symbol{cairo:font-options-t}
  @see-function{gtk:widget-pango-context}")

(export 'pango-context-get)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_screen () -> pango-context-for-screen
;;; ----------------------------------------------------------------------------

;; TODO: Is this the correct usage of the :already-referenced keyword
;; The documentation for the return value is: The caller of the function takes
;; ownership of the data, and is responsible for freeing it.

(cffi:defcfun ("gdk_pango_context_get_for_screen" pango-context-for-screen)
    (g:object pango-context :already-referenced)
 #+liber-documentation
 "@version{2023-12-26}
  @argument[screen]{a @class{gdk:screen} object for which the Pango context
    is to be created}
  @return{The new @class{pango-context} instance for @arg{screen}.}
  @begin{short}
    Creates a Pango context for the screen.
  @end{short}
  When using GTK, normally you should use the @fun{gtk:widget-pango-context}
  function instead of this function, to get the appropriate Pango context for
  the widget you intend to render text onto.

  The newly created Pango context will have the default font options, see the
  @symbol{cairo:font-options-t} API, for the screen. If these options change
  it will not be updated. Using the @fun{gtk:widget-pango-context} function is
  more convenient if you want to keep a Pango context around and track changes
  to the screens font rendering settings.
  @see-class{pango:context}
  @see-class{gdk:screen}
  @see-symbol{cairo:font-options-t}
  @see-function{gtk:widget-pango-context}"
  (screen (g:object screen)))

(export 'pango-context-for-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_pango_context_get_for_display () -> pango-context-for-display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pango_context_get_for_display" pango-context-for-display)
    (g:object pango-context :already-referenced)
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[display]{a @class{gdk:display} object for which the Pango context
    is to be created}
  @return{A new @class{pango:context} instance for @arg{display}.}
  @begin{short}
    Creates a Pango context for the display.
  @end{short}
  When using GTK, normally you should use the @fun{gtk:widget-pango-context}
  function instead of this function, to get the appropriate Pango context for
  the widget you intend to render text onto.

  The newly created Pango context will have the default font options, see the
  @symbol{cairo:font-options-t} API, for the display. If these options change
  it will not be updated. Using the @fun{gtk:widget-pango-context} function is
  more convenient if you want to keep a Pango context around and track changes
  to the font rendering settings.
  @see-class{gdk:display}
  @see-class{pango:context}
  @see-symbol{cairo:font-options-t}
  @see-function{gtk:widget-pango-context}"
  (display (g:object display)))

(export 'pango-context-for-display)

;;; --- End of file gdk3.pango.lisp --------------------------------------------
