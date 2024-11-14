;;; ----------------------------------------------------------------------------
;;; gtk3.css-provider.lisp
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
;;; GtkCssProvider
;;;
;;;     CSS-like styling for widgets
;;;
;;; Types and Values
;;;
;;;     GtkCssProvider
;;;     GtkCssProviderError
;;;     GtkCssSection
;;;     GtkCssSectionType
;;;
;;;     GTK_CSS_PROVIDER_ERROR
;;;
;;; Functions
;;;
;;;     gtk_css_provider_get_default
;;;     gtk_css_provider_get_named
;;;     gtk_css_provider_load_from_data
;;;     gtk_css_provider_load_from_file
;;;     gtk_css_provider_load_from_path
;;;     gtk_css_provider_load_from_resource
;;;     gtk_css_provider_new
;;;     gtk_css_provider_to_string
;;;
;;;     gtk_css_section_get_end_line
;;;     gtk_css_section_get_end_position
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_section_type
;;;     gtk_css_section_get_start_line
;;;     gtk_css_section_get_start_position
;;;
;;;     gtk_css_section_ref                                not needed
;;;     gtk_css_section_unref                              not needed
;;;
;;; Signals
;;;
;;;     parsing-error
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkCssSection
;;;
;;;     GObject
;;;     ╰── GtkCssProvider
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCssProvider implements GtkStyleProvider and GtkStyleProviderPrivate.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCssProvider
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCssProvider" css-provider
  (:superclass g:object
   :export t
   :interfaces ("GtkStyleProvider")
   :type-initializer "gtk_css_provider_get_type")
  nil)

#+liber-documentation
(setf (documentation 'css-provider 'type)
 "@version{2023-12-30}
  @begin{short}
    The @class{gtk:css-provider} object is an object implementing the
    @class{gtk:style-provider} interface.
  @end{short}
  It is able to parse CSS-like input in order to style widgets.

  An application can make GTK parse a specific CSS style sheet by calling
  the @fun{gtk:css-provider-load-from-file} or
  @fun{gtk:css-provider-load-from-resource} functions and adding the provider
  with the @fun{gtk:style-context-add-provider} or
  @fun{gtk:style-context-add-provider-for-screen} functions.

  In addition, certain files will be read when GTK is initialized. First, the
  file @file{$XDG_CONFIG_HOME/gtk-3.0/gtk.css} is loaded if it exists. Then,
  GTK loads the first existing file among
  @file{XDG_DATA_HOME/themes/THEME/gtk-VERSION/gtk.css},
  @file{$HOME/.themes/THEME/gtk-VERSION/gtk.css},
  @file{$XDG_DATA_DIRS/themes/THEME/gtk-VERSION/gtk.css} and
  @file{DATADIR/share/themes/THEME/gtk-VERSION/gtk.css}, where  @file{THEME} is
  the name of the current theme, see the @slot[gtk:settings]{gtk-theme-name}
  setting, @file{DATADIR} is the prefix configured when GTK was compiled
  (unless overridden by the @code{GTK_DATA_PREFIX} environment variable), and
  @file{VERSION} is the GTK version number. If no file is found for the
  current version, GTK tries older versions all the way back to 3.0.

  In the same way, GTK tries to load a @file{gtk-keys.css} file for the
  current key theme, as defined by the @slot[gtk:settings]{gtk-key-theme-name}
  setting.
  @begin[Signal Details]{dictionary}
    @subheading{The \"parsing-error\" signal}
      @begin{pre}
lambda (provider section error)    :run-last
      @end{pre}
      Signals that a parsing error occured. The path, line and position of the
      @symbol{gtk:css-section} instance describe the actual location of the
      error as accurately as possible.

      Parsing errors are never fatal, so the parsing will resume after the
      error. Errors may however cause parts of the given data or even all of it
      to not be parsed at all. So it is a useful idea to check that the parsing
      succeeds by connecting to this signal.

      Note that this signal may be emitted at any time as the CSS provider may
      opt to defer parsing parts or all of the input to a later time than when
      a loading function was called.
      @begin[code]{table}
        @entry[provider]{The @class{gtk:css-provider} object that had a parsing
          error.}
        @entry[section]{The @class{gtk:css-section} instance the error happened
          in.}
        @entry[error]{The parsing error of type @code{GError}.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:css-provider-new}
  @see-class{gtk:style-provider}
  @see-class{gtk:css-section}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_default () -> css-provider-default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_get_default" css-provider-default)
    (g:object css-provider)
 #+liber-documentation
 "@version{2024-1-2}
  @return{The @class{gtk:css-provider} object used for fallback styling.}
  @begin{short}
    Returns the provider containing the style settings used as a fallback for
    all widgets.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:css-provider-default} function has been deprecated since
    version 3.24 and should not be used in newly written code. Use the
    @fun{gtk:css-provider-new} function instead.
  @end{dictionary}
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-new}")

(export 'css-provider-default)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_get_named () -> css-provider-named
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_get_named" %css-provider-named)
    (g:object css-provider)
  (name :string)
  (variant :string))

(defun css-provider-named (name variant)
 #+liber-documentation
 "@version{2024-1-2}
  @argument[name]{a string with the theme name}
  @argument[variant]{a string with a variant to load}
  @return{The @class{gtk:css-provider} object with the theme loaded.}
  @begin{short}
    Loads a theme from the usual theme paths, for example, \"dark\", or
    @code{nil} for the default theme.
  @end{short}
  @see-class{gtk:css-provider}"
  (%css-provider-named name
                       (if variant variant (cffi:null-pointer))))

(export 'css-provider-named)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_data ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_data" %css-provider-load-from-data)
    :boolean
  (provider (g:object css-provider))
  (data :string)
  (length :long)
  (err :pointer))

(defun css-provider-load-from-data (provider data)
 #+liber-documentation
 "@version{2023-12-30}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[data]{a string with the CSS data}
  @begin{return}
    @em{True}. The return value is deprecated and @em{false} will only be
    returned for backwards compatibility reasons if an error occured.
  @end{return}
  @begin{short}
    Loads data into the CSS provider, making it clear any previously loaded
    information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}"
  (glib:with-ignore-g-error (err)
    (%css-provider-load-from-data provider data -1 err)))

(export 'css-provider-load-from-data)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_file" %css-provider-load-from-file)
    :boolean
  (provider (g:object css-provider))
  (file (g:object g:file))
  (err :pointer))

(defun css-provider-load-from-file (provider file)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[file]{a @class{g:file} object pointing to a file to load}
  @return{@em{True}. The return value is deprecated and @em{false} will only
    be returned for backwards compatibility reasons if an error occured.}
  @begin{short}
    Loads the data contained in @arg{file} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}
  @see-class{g:file}"
  (glib:with-g-error (err)
    (%css-provider-load-from-file provider file err)))

(export 'css-provider-load-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_path" %css-provider-load-from-path)
    :boolean
  (provider (g:object css-provider))
  (path :string)
  (err :pointer))

(defun css-provider-load-from-path (provider path)
 #+liber-documentation
 "@version{2023-3-17}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[path]{a pathname or namestring with the path of a filename to load,
    in the GLib filename encoding}
  @begin{return}
    @em{True}. The return value is deprecated and @em{false} will only be
    returned for backwards compatibility reasons if an error occured.
  @end{return}
  @begin{short}
    Loads the data contained in @arg{path} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}"
  (glib:with-g-error (err)
    (%css-provider-load-from-path provider (namestring path) err)))

(export 'css-provider-load-from-path)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_resource ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_resource"
               css-provider-load-from-resource) :void
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[path]{a string with the resource path}
  @begin{short}
    Loads the data contained in the resource at @arg{path} into the CSS
    provider, clearing any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}
  @see-class{g:resource}"
  (provider (g:object css-provider))
  (path :string))

(export 'css-provider-load-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline css-provider-new))

(defun css-provider-new ()
 #+liber-documentation
 "@version{2023-12-30}
  @return{The new @class{gtk:css-provider} object.}
  @short{Returns a newly created CSS provider object.}
  @see-class{gtk:css-provider}"
  (make-instance 'css-provider))

(export 'css-provider-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_to_string" css-provider-to-string) :string
 #+liber-documentation
 "@version{2024-1-2}
  @argument[provider]{a @class{gtk:css-provider} object to write to a string}
  @return{The string representing the provider.}
  @begin{short}
    Convertes the provider into a string representation in CSS format.
  @end{short}
  Using the @fun{gtk:css-provider-load-from-data} function with the return
  value from this function on a new provider created with the
  @fun{gtk:css-provider-new} function will basically create a duplicate of this
  provider.
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-new}
  @see-function{gtk:css-provider-load-from-data}"
  (provider (g:object css-provider)))

(export 'css-provider-to-string)

;;; ----------------------------------------------------------------------------
;;; GTK_CSS_PROVIDER_ERROR
;;;
;;; #define GTK_CSS_PROVIDER_ERROR (gtk_css_provider_error_quark ())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCssProviderError
;;;
;;; typedef enum {
;;;   GTK_CSS_PROVIDER_ERROR_FAILED,
;;;   GTK_CSS_PROVIDER_ERROR_SYNTAX,
;;;   GTK_CSS_PROVIDER_ERROR_IMPORT,
;;;   GTK_CSS_PROVIDER_ERROR_NAME,
;;;   GTK_CSS_PROVIDER_ERROR_DEPRECATED,
;;;   GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE
;;; } GtkCssProviderError;
;;;
;;; GTK_CSS_PROVIDER_ERROR_FAILED
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_SYNTAX
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_IMPORT
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_NAME
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_DEPRECATED
;;;
;;;
;;; GTK_CSS_PROVIDER_ERROR_UNKNOWN_VALUE
;;;-----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCssSectionType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkCssSectionType" css-section-type
  (:export t
   :type-initializer "gtk_css_section_type_get_type")
  (:document 0)
  (:import 1)
  (:color-definition 2)
  (:binding-set 3)
  (:ruleset 4)
  (:selector 5)
  (:declaration 6)
  (:value 7)
  (:keyframes 8))

#+liber-documentation
(setf (liber:alias-for-symbol 'css-section-type)
      "GEnum"
      (liber:symbol-documentation 'css-section-type)
 "@version{2024-3-21}
  @begin{declaration}
(gobject:define-genum \"GtkCssSectionType\" css-section-type
  (:export t
   :type-initializer \"gtk_css_section_type_get_type\")
  (:document 0)
  (:import 1)
  (:color-definition 2)
  (:binding-set 3)
  (:ruleset 4)
  (:selector 5)
  (:declaration 6)
  (:value 7)
  (:keyframes 8))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:document]{The section describes a complete document. This section
        is the only one where the @fun{gtk:css-section-parent} function might
        return @code{nil}.}
      @entry[:import]{The section defines an import rule.}
      @entry[:color-definition]{The section defines a color. This is a GTK
        extension to CSS.}
      @entry[:binding-set]{The section defines a binding set. This is a GTK
        extension to CSS.}
      @entry[:ruleset]{The section defines a CSS ruleset.}
      @entry[:selector]{The section defines a CSS selector.}
      @entry[:declaration]{The section defines the declaration of a CSS
        variable.}
      @entry[:value]{The section defines the value of a CSS declaration.}
      @entry[:keyframes]{The section defines keyframes. See CSS animations for
        details.}
    @end{table}
  @end{values}
  @begin{short}
    The different types of sections indicate parts of a CSS document as parsed
    by the CSS parser of GTK.
  @end{short}
  They are oriented towards the CSS grammar, but may contain extensions.

  More types might be added in the future as the parser incorporates more
  features.
  @see-class{gtk:css-provider}")

;;; ----------------------------------------------------------------------------
;;; GtkCssSection
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque css-section "GtkCssSection"
  :export t
  :type-initializer "gtk_css_section_get_type"
  :alloc (error "GtkCssSection cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'css-section)
      "GBoxed"
      (documentation 'css-section 'type)
 "@version{#2023-3-17}
  @begin{short}
    Defines a part of a CSS document.
  @end{short}
  The @class{css-section} structure is opaque, and has no user visible fields.
  An instance cannot be created from the Lisp side. Because sections are nested
  into one another, you can use the @fun{gtk:css-section-parent} function to
  get the containing region.
  @see-class{gtk:css-provider}
  @see-function{gtk:css-section-parent}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_line () -> css-section-end-line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_end_line" css-section-end-line) :uint
 #+liber-documentation
 "@version{2024-1-2}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The unsigned integer with the line number.}
  @begin{short}
    Returns the line in the CSS document where this section end.
  @end{short}
  The line number is zero-indexed, so the first line of the document will
  return 0. This value may change in future invocations of this function if the
  section is not yet parsed completely. This will for example happen in the
  @code{\"parsing-error\"} signal. The end position and line may be identical
  to the start position and line for sections which failed to parse anything
  successfully.
  @see-class{gtk:css-section}
  @see-class{gtk:css-provider}"
  (section (g:boxed css-section)))

(export 'css-section-end-line)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_position () -> css-section-end-position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_end_position" css-section-end-position)
    :uint
 #+liber-documentation
 "@version{2024-1-2}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The unsigned integer with the offset in bytes from the start of the
    line.}
  @begin{short}
    Returns the offset in bytes from the start of the current line returned via
    the @fun{gtk:css-section-end-line} function.
  @end{short}
  This value may change in future invocations of this function if the section
  is not yet parsed completely. This will for example happen in the
  @code{\"parsing-error\"} signal. The end position and line may be identical
  to the start position and line for sections which failed to parse anything
  successfully.
  @see-class{gtk:css-section}
  @see-class{gtk:css-provider}
  @see-function{gtk:css-section-end-line}"
  (section (g:boxed css-section)))

(export 'css-section-end-position)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_file () -> css-section-file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_get_file" css-section-file) (g:object g:file)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The @class{g:file} object that section was parsed from or @code{nil}
    if section was parsed from other data.}
  @begin{short}
    Gets the file that the section was parsed from.
  @end{short}
  If no such file exists, for example because the CSS was loaded via the
  @fun{gtk:css-provider-load-from-data} function, then @code{nil} is returned.
  @see-class{gtk:css-section}
  @see-class{gtk:css-provider}
  @see-class{g:file}
  @see-function{gtk:css-provider-load-from-data}"
  (section (g:boxed css-section)))

(export 'css-section-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_parent () -> css-section-parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_parent" css-section-parent)
    (g:boxed css-section)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The @symbol{gtk:css-section} parent section, or @code{nil} if none.}
  @begin{short}
    Gets the parent section for the given section.
  @end{short}
  The parent section is the section that contains this section. A special case
  are sections of @code{:document} type. Their parent will either be @code{nil}
  if they are the original CSS document that was loaded by the
  @fun{gtk:css-provider-load-from-file} function or a section of @code{:section}
  type if it was loaded with an import rule from a different file.
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-section-type}
  @see-function{gtk:css-provider-load-from-file}"
  (section (g:boxed css-section)))

(export 'css-section-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_section_type () -> css-section-section-type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_section_type" css-section-section-type)
    css-section-type
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The @symbol{gtk:css-section-type} value for the section.}
  @begin{short}
    Gets the type of information that the section describes.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-section-type}"
  (section (g:boxed css-section)))

(export 'css-section-section-type)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_line () -> css-section-start-line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_start_line" css-section-start-line) :uint
 #+liber-documentation
 "@version{2024-1-2}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The unsigned integer with the line number.}
  @begin{short}
    Returns the line in the CSS document where this section starts.
  @end{short}
  The line number is zero-indexed, so the first line of the document will
  return 0.
  @see-class{gtk:css-section}"
  (section (g:boxed css-section)))

(export 'css-section-start-line)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_position () -> css-section-start-position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_start_position" css-section-start-position)
    :uint
 #+liber-documentation
 "@version{2024-1-2}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The unsigned integer with the offset in bytes from the start of the
    line.}
  @begin{short}
    Returns the offset in bytes from the start of the current line returned
    via the @fun{gtk:css-section-start-line} function.
  @end{short}
  @see-class{gtk:css-section}
  @see-function{gtk:css-section-start-line}"
  (section (g:boxed css-section)))

(export 'css-section-start-position)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_ref ()
;;;
;;; GtkCssSection * gtk_css_section_ref (GtkCssSection *section);
;;;
;;; Increments the reference count on section.
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Returns :
;;;     section itself.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_unref ()
;;;
;;; void gtk_css_section_unref (GtkCssSection *section);
;;;
;;; Decrements the reference count on section, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.css-provider.lisp -------------------------------------
