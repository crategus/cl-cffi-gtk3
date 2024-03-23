;;; ----------------------------------------------------------------------------
;;; gtk3.file-filter.lisp
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkFileFilter
;;;
;;;     A filter for selecting a file subset
;;;
;;; Types and Values
;;;
;;;     GtkFileFilter
;;;     GtkFileFilterFlags
;;;     GtkFileFilterInfo
;;;
;;; Functions
;;;
;;;     gtk_file_filter_new
;;;     gtk_file_filter_set_name
;;;     gtk_file_filter_get_name
;;;     gtk_file_filter_add_mime_type
;;;     gtk_file_filter_add_pattern
;;;     gtk_file_filter_add_pixbuf_formats
;;;     gtk_file_filter_add_custom
;;;     gtk_file_filter_get_needed
;;;     gtk_file_filter_filter
;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkFileFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileFilter implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkFileFilterFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GtkFileFilterFlags" file-filter-flags
  (:export t
   :type-initializer "gtk_file_filter_flags_get_type")
  (:filename 1)
  (:uri 2)
  (:display-name 4)
  (:mime-type 8))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-filter-flags)
      "GFlags"
      (liber:symbol-documentation 'file-filter-flags)
 "@version{2024-3-21}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-flags \"GtkFileFilterFlags\" file-filter-flags
  (:export t
   :type-initializer \"gtk_file_filter_flags_get_type\")
  (:filename 1)
  (:uri 2)
  (:display-name 4)
  (:mime-type 8))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:filename]{The filename of the file being tested.}
      @entry[:uri]{The URI for the file being tested.}
      @entry[:display-name]{The string that will be used to display the file in
        the file chooser.}
      @entry[:mime-type]{The MIME type of the file.}
  @end{table}
  @end{values}
  @begin{short}
    These flags indicate what parts of a @symbol{gtk:file-filter-info}
    instance are filled or need to be filled.
  @end{short}
  @see-class{gtk:file-filter}
  @see-symbol{gtk:file-filter-info}")

;;; ----------------------------------------------------------------------------
;;; struct GtkFileFilterInfo
;;; ----------------------------------------------------------------------------

;; TODO: Consider to improve the implementation of GtkFileFilterInfo.

(cffi:defcstruct file-filter-info
  (contains file-filter-flags)
  (filename :string)
  (uri :string)
  (display-name :string)
  (mime-type :string))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-filter-info)
      "CStruct"
      (liber:symbol-documentation 'file-filter-info)
 "@version{#2024-3-23}
  @begin{declaration}
    @begin{pre}
(cffi:defcstruct gtk:file-filter
  (contains gtk:file-filter-flags)
  (filename :string)
  (uri :string)
  (display-name :string)
  (mime-type :string))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[contains]{Flags indicating which of the following fields need are
        filled.}
      @entry[filename]{The filename of the file being tested.}
      @entry[uri]{The URI for the file being tested.}
      @entry[display-name]{The string that will be used to display the file in
        the file chooser.}
      @entry[mime-type]{The MIME type of the file.}
    @end{table}
  @end{values}
  @begin{short}
    The @class{gtk:file-filter-info} structure is used to pass information
    about the tested file to the @fun{gtk:file-filter-filter} function.
  @end{short}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-filter-filter}")

(export 'file-filter-info)

;;; --- Accessors for the file-filter-info structure ---------------------------

(defun file-filter-info-contains (info)
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-filter-info-contains info) => contains}
  @syntax{(setf (gtk:file-filter-info-contains info) contains)}
  @argument[info]{a @symbol{gtk:file-filter-info} instance}
  @argument[contains]{a @symbol{gtk:file-filter-flags} value}
  @begin{short}
    Accessor of the @code{contains} slot of the @symbol{gtk:file-filter-info}
    structure.
  @end{short}
  Flags indicating which of the fields in @arg{info} are filled.
  @see-symbol{gtk:file-filter-info}
  @see-symbol{gtk:file-filter-flags}"
  (cffi:foreign-slot-value info '(:struct file-filter-info) 'contains))

(defun file-filter-info-filename (info)
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-filter-info-filename info) => filename}
  @syntax{(setf (gtk:file-filter-info-filename info) filename)}
  @argument[info]{a @symbol{gtk:file-filter-info} instance}
  @argument[filename]{a string with the filename being tested}
  @begin{short}
    Accessor of the @code{filename} slot of the @symbol{gtk:file-filter-info}
    structure.
  @end{short}
  The filename of the file being tested.
  @see-symbol{gtk:file-filter-info}"
  (cffi:foreign-slot-value info '(:struct file-filter-info) 'filename))

(defun file-filter-info-uri (info)
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-filter-info-uri info) => uri}
  @syntax{(setf (gtk:file-filter-info-uri info) uri)}
  @argument[info]{a @symbol{gtk:file-filter-info} instance}
  @argument[uri]{a string with the URI being tested}
  @begin{short}
    Accessor of the @code{uri} slot of the @symbol{gtk:file-filter-info}
    structure.
  @end{short}
  The URI for the file being tested.
  @see-symbol{gtk:file-filter-info}"
  (cffi:foreign-slot-value info '(:struct file-filter-info) 'uri))

(defun file-filter-info-display-name (info)
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-filter-info-display-name info) => display-name}
  @syntax{(setf (gtk:file-filter-info-display-name info) display-name)}
  @argument[info]{a @symbol{gtk:file-filter-info} instance}
  @argument[display-name]{a string that will be used to display in the
    file chooser}
  @begin{short}
    Accessor of the @code{display-name} slot of the
    @symbol{gtk:file-filter-info} structure.
  @end{short}
  The string that will be used to display the file in the file chooser.
  @see-symbol{gtk:file-filter-info}"
  (cffi:foreign-slot-value info '(:struct file-filter-info) 'display-name))

(defun file-filter-info-mime-type (info)
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-filter-info-mime-type info) => mime-type}
  @syntax{(setf (gtk:file-filter-info-mime-type info) mime-type)}
  @argument[info]{a @symbol{gtk:file-filter-info} instance}
  @argument[mime-type]{a string with the MIME type of the file}
  @begin{short}
    Accessor of the @code{mime-type} slot of the @symbol{gtk:file-filter-info}
    structure.
  @end{short}
  The MIME type of the file.
  @see-symbol{gtk:file-filter-info}"
  (cffi:foreign-slot-value info '(:struct file-filter-info) 'mime-type))

(export 'file-filter-info-contains)
(export 'file-filter-info-filename)
(export 'file-filter-info-uri)
(export 'file-filter-info-display-name)
(export 'file-filter-info-mime-type)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFileFilter" file-filter
  (:superclass g:initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_file_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'file-filter 'type)
 "@version{2023-6-11}
  @begin{short}
    The @class{gtk:file-filter} object can be used to restrict the files being
    shown in a @class{gtk:file-chooser} widget.
  @end{short}
  Files can be filtered based on their name with the
  @fun{gtk:file-filter-add-pattern} function, on their mime type with the
  @fun{gtk:file-filter-add-mime-type} function, or by a custom filter function
  with the @fun{gtk:file-filter-add-custom} function.

  Filtering by MIME types handles aliasing and subclassing of mime types. E.g.
  a filter for @code{text/plain} also matches a file with MIME type
  @code{application/rtf}, since @code{application/rtf} is a subclass of
  @code{text/plain}. Note that the @class{gtk:file-filter} object allows
  wildcards for the subtype of a MIME type, so you can e.g. filter for
  @code{image/*}.

  Normally, filters are used by adding them to a @class{gtk:file-chooser}
  widget, see the @fun{gtk:file-chooser-add-filter} function, but it is also
  possible to manually use a filter on a file with the
  @fun{gtk:file-filter-filter} function.
  @begin[GtkFileFilter as GtkBuildable]{dictionary}
    The @class{gtk:file-filter} implementation of the @class{gtk:buildable}
    interface supports adding rules using the @code{<mime-types>},
    @code{<patterns>} and @code{<applications>} elements and listing the rules
    within. Specifying a @code{<mime-type>} or @code{<pattern>} is the same as
    calling the @fun{gtk:file-filter-add-mime-type} or
    @fun{gtk:file-filter-add-pattern} functions.

    @b{Example:} A UI definition fragment specifying @class{gtk:file-filter}
    rules
    @begin{pre}
<object class=\"GtkFileFilter\">
  <mime-types>
    <mime-type>text/plain</mime-type>
    <mime-type>image/&ast;</mime-type>
  </mime-types>
  <patterns>
    <pattern>*.txt</pattern>
    <pattern>*.png</pattern>
  </patterns>
</object>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:file-filter-new}
  @see-constructor{gtk:file-filter-new-from-gvariant}
  @see-class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline file-filter-new))

(defun file-filter-new ()
 #+liber-documentation
 "@version{2023-6-11}
  @return{A new @class{gtk:file-filter} object.}
  @begin{short}
    Creates a new file filter with no rules added to it.
  @end{short}
  Such a filter does not accept any files, so is not particularly useful until
  you add rules with the @fun{gtk:file-filter-add-mime-type},
  @fun{gtk:file-filter-add-pattern}, or @fun{gtk:file-filter-add-custom}
  functions.
  @begin{examples}
    To create a filter that accepts any file, use:
    @begin{pre}
(let ((filter (gtk:file-filter-new)))
  (gtk:file-filter-add-pattern filter \"*\")
  ... )
    @end{pre}
  @end{examples}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-filter-add-mime-type}
  @see-function{gtk:file-filter-add-pattern}
  @see-function{gtk:file-filter-add-custom}"
  (make-instance 'file-filter))

(export 'file-filter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_name ()
;;; gtk_file_filter_set_name ()
;;; ----------------------------------------------------------------------------

(defun (setf file-filter-name) (name filter)
  (cffi:foreign-funcall "gtk_file_filter_set_name"
                        (g:object file-filter) filter
                        :string (if name name (cffi:null-pointer))
                        :void)
  name)

(cffi:defcfun ("gtk_file_filter_get_name" file-filter-name) :string
 #+liber-documentation
 "@version{2023-6-11}
  @syntax{(gtk:file-filter-name filter) => name}
  @syntax{(setf (gtk:file-filter-name filter) name)}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[name]{a string with the human readable name for the filter,
    or @code{nil} to remove any existing name}
  @begin{short}
    Accessor of the human readable name of the file filter.
  @end{short}
  The @fun{gtk:file-filter-name} function gets the human readable name for the
  file filter. The @setf{gtk:file-filter-name} function sets the human readable
  name. This is the string that will be displayed in the file selector user
  interface if there is a selectable list of filters.
  @see-class{gtk:file-filter}"
  (filter (g:object file-filter)))

(export 'file-filter-name)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_mime_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_mime_type" file-filter-add-mime-type) :void
 #+liber-documentation
 "@version{2023-6-11}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[mime-type]{a string with the name of a MIME type}
  @begin{short}
    Adds a rule allowing a given MIME type to the file filter.
  @end{short}
  @see-class{gtk:file-filter}"
  (filter (g:object file-filter))
  (mime-type :string))

(export 'file-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pattern ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_pattern" file-filter-add-pattern) :void
 #+liber-documentation
 "@version{2023-6-11}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[pattern]{a string with a shell style glob pattern}
  @begin{short}
    Adds a rule allowing a shell style glob pattern to a file filter.
  @end{short}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-filter-add-mime-type}"
  (filter g:object)
  (pattern :string))

(export 'file-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pixbuf_formats ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_pixbuf_formats"
                file-filter-add-pixbuf-formats) :void
 #+liber-documentation
 "@version{2023-6-11}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Adds a rule allowing image files in the formats supported by a
    @class{gdk-pixbuf:pixbuf} object.
  @end{short}
  @see-class{gtk:file-filter}
  @see-class{gdk-pixbuf:pixbuf}"
  (filter g:object))

(export 'file-filter-add-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilterFunc ()
;;; ----------------------------------------------------------------------------

(cffi:defcallback file-filter-func :boolean
    ((info (:pointer (:struct file-filter-info)))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func info)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-filter-func)
      "Callback"
      (liber:symbol-documentation 'file-filter-func)
 "@version{#2024-3-23}
  @syntax{lambda (info) => result}
  @argument[info]{a @symbol{gtk:file-filter-info} instance that is filled
    according to the needed flags passed to the @fun{gtk:file-filter-add-custom}
    function}
  @argument[result]{@em{true} if the file should be displayed}
  @begin{short}
    The type of the callback function that is used with custom filters.
  @end{short}
  @see-class{gtk:file-filter}
  @see-symbol{gtk:file-filter-info}
  @see-function{gtk:file-filter-add-custom}")

(export 'file-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_custom ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_custom" %file-filter-add-custom) :void
  (filter (g:object file-filter))
  (needed file-filter-flags)
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun file-filter-add-custom (filter needed func)
 #+liber-documentation
 "@version{#2024-3-23}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[needed]{a @symbol{gtk:file-filter-flags} value with the flags
    indicating the information that the custom filter function needs}
  @argument[func]{a @symbol{gtk:file-filter-func} callback function, if the
    function returns @em{true}, then the file will be displayed}
  @begin{short}
    Adds rule to a filter that allows files based on a custom callback function.
  @end{short}
  The bitfield needed which is passed in provides information about what sorts
  of information that the filter function needs. This allows GTK to avoid
  retrieving expensive information when it is not needed by the filter.
  @begin{examples}
    @begin{pre}
(defun custom-file-filter (filter-info)
  ;; Select files with upcase characters in the display name
  (let ((display-name (gtk:file-filter-info-display-name filter-info)))
    (string= display-name
             (string-upcase display-name))))
...
(let ((filter-custom (gtk:file-filter-new)))
  ;; Add a custom file filter
  (setf (gtk:file-filter-name filter-custom) \"Custom Filter\")
  (gtk:file-filter-add-custom filter-custom
                              :display-name
                              #'custom-file-filter)
  (gtk:file-chooser-add-filter chooser filter-custom)
  ... )
    @end{pre}
  @end{examples}
  @see-class{gtk:file-filter}
  @see-symbol{gtk:file-filter-flags}"
  (%file-filter-add-custom filter
                           needed
                           (cffi:callback file-filter-func)
                           (glib:allocate-stable-pointer func)
                           (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'file-filter-add-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_needed () -> file-filter-needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_get_needed" file-filter-needed)
    file-filter-flags
 #+liber-documentation
 "@version{2023-6-11}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{return}
    A @symbol{gtk:file-filter-flags} value with the flags indicating needed
    fields when calling the @fun{gtk:file-filter-filter} function.
  @end{return}
  @begin{short}
    Gets the fields that need to be filled in for the structure passed to
    the @fun{gtk:file-filter-filter} function.
  @end{short}
  This function will not typically be used by applications. It is intended
  principally for use in the implementation of a @class{gtk:file-chooser}
  widget.
  @see-class{gtk:file-filter}
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-filter-flags}
  @see-function{gtk:file-filter-filter}"
  (filter (g:object file-filter)))

(export 'file-filter-needed)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_filter ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_filter" file-filter-filter) :boolean
 #+liber-documentation
 "@version{#2023-6-11}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[info]{a @symbol{gtk:file-filter-info} instance containing
    information about a file}
  @return{@em{True} if the file should be displayed.}
  @begin{short}
    Tests whether a file should be displayed according to @arg{filter}.
  @end{short}
  The @arg{info} argument should include the fields returned from the
  @fun{gtk:file-filter-needed} function.

  This function will not typically be used by applications. It is intended
  principally for use in the implementation of a @class{gtk:file-chooser}
  widget.
  @see-class{gtk:file-filter}
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-filter-info}
  @see-function{gtk:file-filter-needed}"
  (filter (g:object file-filter))
  (info (:pointer (:struct file-filter-info))))

(export 'file-filter-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_new_from_gvariant"
                file-filter-new-from-gvariant) (g:object file-filter)
 #+liber-documentation
 "@version{2024-3-17}
  @argument[variant]{a @type{g:variant} instance of type @code{a{sv@}}}
  @return{The new @class{gtk:file-filter} object.}
  @begin{short}
    Deserialize a file filter from an @code{a{sv@}} variant in the format
    produced by the @fun{gtk:file-filter-to-gvariant} function.
  @end{short}
  @see-class{gtk:file-filter}
  @see-type{g:variant}
  @see-function{gtk:file-filter-to-gvariant}"
  (variant (:pointer (:struct g:variant))))

(export 'file-filter-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_to_gvariant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_to_gvariant" file-filter-to-gvariant)
    (:pointer (:struct g:variant))
 #+liber-documentation
 "@version{2024-3-17}
  @argument[filter]{a @class{gtk:file-filter} object}
  @return{The new @type{g:variant} instance.}
  @begin{short}
    Serialize a file filter to a @code{a{sv@}} variant.
  @end{short}
  @see-class{gtk:file-filter}
  @see-type{g:variant}"
  (filter (g:object file-filter)))

(export 'file-filter-to-gvariant)

;;; ---- End of file gtk3.file-filter.lisp -------------------------------------
