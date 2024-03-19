;;; ----------------------------------------------------------------------------
;;; gtk3.style-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; GtkStyleProvider
;;;
;;;     Interface to provide style information to GtkStyleContext
;;;
;;; Types and Values
;;;
;;;     GtkStyleProvider
;;;
;;;     GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;;     GTK_STYLE_PROVIDER_PRIORITY_THEME
;;;     GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;;     GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;;     GTK_STYLE_PROVIDER_PRIORITY_USER
;;;
;;; Functions
;;;
;;;     gtk_style_provider_get_icon_factory                not implemented
;;;     gtk_style_provider_get_style                       not implemented
;;;     gtk_style_provider_get_style_property
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkStyleProvider
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_FALLBACK
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-fallback+ 1)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-fallback+)
      "Constant"
      (documentation '+gtk-priority-fallback+ 'variable)
 "@version{2023-3-27}
  @variable-value{1}
  @begin{short}
    The priority used for default style information that is used in the absence
    of themes.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+gtk-priority-fallback+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_THEME
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-theme+ 200)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-theme+)
      "Constant"
      (documentation '+gtk-priority-theme+ 'variable)
 "@version{2023-3-27}
  @variable-value{200}
  @begin{short}
    The priority used for style information provided by themes.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+gtk-priority-theme+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_SETTINGS
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-settings+ 400)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-settings+)
      "Constant"
      (documentation '+gtk-priority-settings+ 'variable)
 "@version{2023-3-27}
  @variable-value{400}
  @begin{short}
    The priority used for style information provided via a @class{gtk:settings}
    object.
  @end{short}
  This priority is higher than the @var{+gtk-priority-theme+} value to let
  settings override themes.
  @see-class{gtk:style-provider}
  @see-class{gtk:settings}
  @see-variable{+gtk-priority-theme+}")

(export '+gtk-priority-settings+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-application+ 600)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-application+)
      "Constant"
      (documentation '+gtk-priority-application+ 'variable)
 "@version{2023-3-27}
  @variable-value{600}
  @begin{short}
    A priority that can be used when adding a @class{gtk:style-provider} object
    for application specific style information.
  @end{short}
  @see-class{gtk:style-provider}")

(export '+gtk-priority-application+)

;;; ----------------------------------------------------------------------------
;;; GTK_STYLE_PROVIDER_PRIORITY_USER
;;; ----------------------------------------------------------------------------

(defconstant +gtk-priority-user+ 800)

#+liber-documentation
(setf (liber:alias-for-variable '+gtk-priority-user+)
      "Constant"
      (documentation '+gtk-priority-user+ 'variable)
 "@version{2023-3-27}
  @variable-value{800}
  @begin{short}
    The priority used for the style information from the @file{~/.gtk-3.0.css}
    file.
  @end{short}
  You should not use priorities higher than this, to give the user the last
  word.
  @see-class{gtk:style-provider}")

(export '+gtk-priority-user+)

;;; ----------------------------------------------------------------------------
;;; GtkStyleProvider
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GtkStyleProvider" style-provider
  (:export t
   :type-initializer "gtk_style_provider_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'style-provider)
      "Interface"
      (documentation 'style-provider 'type)
 "@version{2023-3-27}
  @begin{short}
    The @class{gtk:style-provider} interface is an interface used to provide
    style information to a @class{gtk:style-context} object.
  @end{short}
  See the @fun{gtk:style-context-add-provider} and
  @fun{gtk:style-context-add-provider-for-screen} functions.
  @see-class{gtk:style-context}
  @see-class{gtk:css-provider}
  @see-function{gtk:style-context-add-provider}
  @see-function{gtk:style-context-add-provider-for-screen}")

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_icon_factory ()
;;;
;;; Deprecated since version 3.8 and not implemented.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_style ()
;;;
;;; Deprecated since version 3.8 and not implemented.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_style_provider_get_style_property -> style-provider-style-property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_provider_get_style_property"
               %style-provider-style-property) :void
  (provider (g:object style-provider))
  (path (g:boxed widget-path))
  (state state-flags)
  (pspec (:pointer (:struct g:param-spec)))
  (value (:pointer (:struct g:value))))

(defun style-provider-style-property (provider path state pspec)
 #+liber-documentation
 "@version{2023-3-27}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[path]{a @symbol{gtk:widget-path} instance to query}
  @argument[state]{a @symbol{gtk:state-flags} value to query the style property
    for}
  @argument[pspec]{a @symbol{g:param-spec} instance to query}
  @return{Returns the value of the style property.}
  @begin{short}
    Looks up the value of a widget style property as defined by the provider
    for the widget represented by @arg{path}.
  @end{short}
  @see-class{gtk:style-provider}
  @see-class{gtk:widget-path}
  @see-symbol{gtk:state-flags}
  @see-symbol{g:param-spec}"
  (let ((gtype (g:param-spec-value-type pspec)))
    (cffi:with-foreign-object (value '(:struct g:value))
      (unwind-protect
        (progn
          (g:value-init value gtype)
          (%style-provider-style-property provider path state pspec value)
          (gobject:parse-g-value value))
        (g:value-unset value)))))

(export 'style-provider-style-property)

;;; --- End of file gtk3.style-provider.lisp -----------------------------------
