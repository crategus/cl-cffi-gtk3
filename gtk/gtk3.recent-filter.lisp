;;; ----------------------------------------------------------------------------
;;; gtk3.recent-filter.lisp
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
;;; GtkRecentFilter
;;;
;;;     A filter for selecting a subset of recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentFilter
;;;     GtkRecentFilterInfo
;;;     GtkRecentFilterFlags
;;;
;;; Functions
;;;
;;;     gtk_recent_filter_new
;;;     gtk_recent_filter_get_name
;;;     gtk_recent_filter_set_name
;;;     gtk_recent_filter_add_mime_type
;;;     gtk_recent_filter_add_pattern
;;;     gtk_recent_filter_add_pixbuf_formats
;;;     gtk_recent_filter_add_application
;;;     gtk_recent_filter_add_group
;;;     gtk_recent_filter_add_age
;;;     gtk_recent_filter_add_custom
;;;     gtk_recent_filter_get_needed
;;;     gtk_recent_filter_filter
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkRecentFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentFilter implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilterFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkRecentFilterFlags" recent-filter-flags
  (:export t
   :type-initializer "gtk_recent_filter_flags_get_type")
  (:uri 1)
  (:display-name 2)
  (:mime-type 4)
  (:application 8)
  (:group 16)
  (:age 32))

#+liber-documentation
(setf (liber:alias-for-symbol 'recent-filter-flags)
      "GFlags"
      (liber:symbol-documentation 'recent-filter-flags)
 "@version{#2025-07-01}
  @begin{declaration}
(gobject:define-gflags \"GtkRecentFilterFlags\" recent-filter-flags
  (:export t
   :type-initializer \"gtk_recent_filter_flags_get_type\")
  (:uri 1)
  (:display-name 2)
  (:mime-type 4)
  (:application 8)
  (:group 16)
  (:age 32))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:uri]{The URI of the file being tested.}
      @entry[:display-name]{The string that will be used to display the file in
        the recent chooser.}
      @entry[:mime-type]{The MIME type of the file.}
      @entry[:application]{The list of applications that have registered the
        file.}
      @entry[:group]{The groups to which the file belongs to.}
      @entry[:age]{The number of days elapsed since the file has been
        registered,}
    @end{simple-table}
  @end{values}
  @begin{short}
    These flags indicate what parts of a @sym{gtk:recent-filter-info} structure
    are filled or need to be filled.
  @end{short}
  @see-symbol{gtk:recent-filter-info}")

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilterInfo
;;; ----------------------------------------------------------------------------

(cffi:defcstruct recent-filter-info
  (contains recent-filter-flags)
  (uri :string)
  (display-name :string)
  (mime-type :string)
  (applications g:strv-t)
  (groups g:strv-t))

#+liber-documentation
(setf (liber:alias-for-symbol 'recent-filter-info)
      "CStruct"
      (liber:symbol-documentation 'recent-filter-info)
 "@version{#2023-3-24}
  @begin{short}
    The @class{gtk:recent-filter-info} structure is used to pass information
    about the tested file to the @fun{gtk:recent-filter-filter} function.
  @end{short}
  @begin{pre}
(cffi:defcstruct gtk:recent-filter-info
  (contains gtk:recent-filter-flags)
  (uri :string)
  (display-name :string)
  (mime-type :string)
  (applications g:strv-t)
  (groups g:strv-t))
  @end{pre}
  @see-class{gtk:recent-filter}
  @see-function{gtk:recent-filter-filter}")

(export 'recent-filter-info)

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkRecentFilter" recent-filter
  (:superclass g:initially-unowned
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_recent_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'recent-filter 'type)
 "@version{#2025-07-11}
  @begin{short}
    The @class{gtk:recent-filter} object can be used to restrict the files being
    shown in a @class{gtk:recent-chooser} widget.
  @end{short}
  Files can be filtered based on their name with the
  @fun{gtk:recent-filter-add-pattern} function, on their MIME type with the
  @fun{gtk:file-filter-add-mime-type} function, on the application that has
  registered them with the @fun{gtk:recent-filter-add-application} function, or
  by a custom filter function with the @fun{gtk:recent-filter-add-custom}
  function.

  Filtering by MIME type handles aliasing and subclassing of mime types. For
  example, a filter for text/plain also matches a file with MIME type
  application/rtf, since application/rtf is a subclass of text/plain. Note that
  the @class{gtk:recent-filter} object allows wildcards for the subtype of a
  MIME type, so you can, for example, filter for image/*.

  Normally, filters are used by adding them to a @class{gtk:recent-chooser}
  widget, see the @fun{gtk:recent-chooser-add-filter} function, but it is also
  possible to manually use a filter on a file with the
  @fun{gtk:recent-filter-filter} function.

  @begin[GtkRecentFilter as GtkBuildable]{dictionary}
  The @class{gtk:recent-filter} implementation of the @class{gtk:buildable}
  interface supports adding rules using the @code{<mime-types>},
  @code{<patterns>} and @code{<applications>} elements and listing the rules
  within. Specifying a @code{<mime-type>}, @code{<pattern>} or
  @code{<application>} is the same as calling the
  @fun{gtk:recent-filter-add-mime-type}, @fun{gtk:recent-filter-add-pattern}
  or @fun{gtk:recent-filter-add-application} functions.

  @b{Example:} A UI definition fragment specifying @class{gtk:recent-filter}
  rules
  @begin{pre}
<object class=\"GtkRecentFilter\">
  <mime-types>
    <mime-type>text/plain</mime-type>
    <mime-type>image/png</mime-type>
  </mime-types>
  <patterns>
    <pattern>*.txt</pattern>
    <pattern>*.png</pattern>
  </patterns>
  <applications>
    <application>gimp</application>
    <application>gedit</application>
    <application>glade</application>
  </applications>
</object>
  @end{pre}
  @end{dictionary}
  @see-constructor{gtk:recent-filter}
  @see-class{gtk:recent-chooser}
  @see-function{gtk:recent-filter-add-pattern}
  @see-function{gtk:file-filter-add-mime-type}
  @see-function{gtk:recent-filter-add-application}
  @see-function{gtk:recent-filter-add-custom}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_new" recent-filter-new)
    (g:object recent-filter)
 #+liber-documentation
 "@version{#2025-07-01}
  @return{The new @class{gtk:recent-filter} object.}
  @begin{short}
    Creates a new @class{gtk:recentFilter} object with no rules added to it.
  @end{short}
  Such filter does not accept any recently used resources, so is not
  particularly useful until you add rules with the
  @fun{gtk:recent-filter-add-pattern}, @fun{gtk:recent-filter-add-mime-type},
  @fun{gtk:recent-filter-add-application}, @fun{gtk:recent-filter-add-age}
  functions. To create a filter that accepts any recently used resource, use:
  @begin{pre}
(defvar filter (gtk:recent-filter-new))
=> FILTER
(gtk:recent-filter-add-pattern filter \"*\")
  @end{pre}
  @see-class{gtk:recent-filter}
  @see-function{gtk:recent-filter-add-pattern}
  @see-function{gtk:recent-filter-add-mime-type}
  @see-function{gtk:recent-filter-add-application}
  @see-function{gtk:recent-filter-add-age}")

(export 'recent-filter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_get_name
;;; gtk_recent_filter_set_name
;;; ----------------------------------------------------------------------------

(defun (setf recent-filter-name) (name filter)
  (cffi:foreign-funcall "gtk_recent_filter_set_name"
                        (g:object recent-filter) filter
                        :string name
                        :void)
  name)

(cffi:defcfun ("gtk_recent_filter_get_name" recent-filter-name) :string
 #+liber-documentation
 "@version{#2025-07-07}
  @syntax{(gtk:recent-filter-name filter) => name}
  @syntax{(setf (gtk:recent-filter-name filter) name)}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[name]{a string for the human readable name of @arg{filter}}
  @begin{short}
    The @fun{gtk:recent-filter-name} function gets the human readable name for
    the filter.
  @end{short}
  The @setf{gtk:recent-filter-name} function sets the human readable name of
  the filter. This is the string that will be displayed in the recently used
  resources selector user interface if there is a selectable list of filters.
  @see-class{gtk:recent-filter}"
  (filter (g:object recent-filter)))

(export 'recent-filter-name)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_mime_type" recent-filter-add-mime-type)
    :void
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[mime-type]{a string for the MIME type}
  @begin{short}
    Adds a rule that allows resources based on their registered MIME type.
  @end{short}
  @see-class{gtk:recent-filter}"
  (filter (g:object recent-filter))
  (mime-type :string))

(export 'recent-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_pattern
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_pattern" recent-filter-add-pattern) :void
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[pattern]{a string for the file pattern}
  @begin{short}
    Adds a rule that allows resources based on a pattern matching their display
    name.
  @end{short}
  @see-class{gtk:recent-filter}"
  (filter (g:object recent-filter))
  (pattern :string))

(export 'recent-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_pixbuf_formats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_pixbuf_formats"
               recent-filter-add-pixbuf-formats) :void
 #+liber-documentation
 "@version{#2023-3-24}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @begin{short}
    Adds a rule allowing image files in the formats supported by the
    @class{gdk-pixbuf:pixbuf} object.
  @end{short}
  @see-class{gtk:recent-filter}
  @see-class{gdk-pixbuf:pixbuf}"
  (filter (g:object recent-filter)))

(export 'recent-filter-add-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_application
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_application"
               recent-filter-add-application) :void
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[application]{a string for an application name}
  @begin{short}
    Adds a rule that allows resources based on the name of the application that
    has registered them.
  @end{short}
  @see-class{gtk:recent-filter}"
  (filter (g:object recent-filter))
  (application :string))

(export 'recent-filter-add-application)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_group" recent-filter-add-group) :void
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[group]{a string for the group name}
  @begin{short}
    Adds a rule that allows resources based on the name of the group to which
    they belong.
  @end{short}
  @see-class{gtk:recent-filter}"
  (filter (g:object recent-filter))
  (group :string))

(export 'recent-filter-add-group)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_age
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_age" recent-filter-add-age) :void
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[days]{an integer for the number of days}
  @begin{short}
    Adds a rule that allows resources based on their age - that is, the number
    of days elapsed since they were last modified.
  @end{short}
  @see-class{gtk:recent-filter}"
  (filter (g:object recent-filter))
  (days :int))

(export 'recent-filter-add-age)

;;; ----------------------------------------------------------------------------
;;; GtkRecentFilterFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback recent-filter-func :boolean
    ((info (:pointer (:struct recent-filter-info)))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func info)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'recent-filter-func)
      "Callback"
      (liber:symbol-documentation 'recent-filter-func)
 "@version{#2025-07-01}
  @syntax{lambda (info) => result}
  @argument[info]{a @sym{gtk:recent-filter-info} instance that is filled
    according to the needed flags passed to the
    @fun{gtk:recent-filter-add-custom} function}
  @argument[result]{@em{true} if the file should be displayed}
  @begin{short}
    The type of function that is used with custom filters, see the
    @fun{gtk:recent-filter-add-custom} function.
  @end{short}
  @see-class{gtk:recent-filter}
  @see-function{gtk:recent-filter-add-custom}")

(export 'recent-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_add_custom
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_add_custom" %recent-filter-add-custom) :void
  (filter (g:object recent-filter))
  (needed recent-filter-flags)
  (func :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun recent-filter-add-custom (filter needed func)
 #+liber-documentation
 "@version{#2025-07-01}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[needed]{bitfield of @sym{gtk:recent-filter-flags} flags
    indicating the information that the custom filter function needs}
  @argument[func]{a @sym{gtk:recent-filter-func} callback function, if the
    function returns @em{true}, then the file will be displayed}
  @begin{short}
    Adds a rule to a @arg{filter} that allows resources based on a custom
    callback function.
  @end{short}
  The bitfield @arg{needed} which is passed in provides information about
  what sorts of information that the filter function needs. This allows GTK
  to avoid retrieving expensive information when it is not needed by the
  filter.
  @see-class{gtk:recent-filter}
  @see-symbol{gtk:recent-filter-flags}
  @see-symbol{gtk:recent-filter-func}"
  (%recent-filter-add-custom
          filter
          needed
          (cffi:callback recent-filter-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'recent-filter-add-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_get_needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_get_needed" recent-filter-needed)
    recent-filter-flags
 #+liber-documentation
 "@version{#2025-07-01}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @begin{return}
    Bitfield of @sym{gtk:recent-filter-flags} flags of indicating needed fields
    when calling the @fun{gtk:recent-filter-filter} function.
  @end{return}
  @begin{short}
    Gets the fields that need to be filled in for the structure passed to the
    @fun{gtk:recent-filter-filter} function.
  @end{short}
  This function will not typically be used by applications. It is intended
  principally for use in the implementation of the @class{gtk:recent-chooser}
  class.
  @see-class{gtk:recent-filter}
  @see-class{gtk:recent-chooser}
  @see-symbol{gtk:recent-filter-flags}"
  (filter (g:object recent-filter)))

(export 'recent-filter-needed)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_filter_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_filter_filter" recent-filter-filter) :boolean
 #+liber-documentation
 "@version{#2025-07-01}
  @argument[filter]{a @class{gtk:recent-filter} object}
  @argument[info]{a @sym{gtk:recent-filter-info} instance containing information
    about a recently used resource}
  @return{@em{True} if the file should be displayed.}
  @begin{short}
    Tests whether a file should be displayed according to filter.
  @end{short}
  The @arg{info} argument should include the fields returned from the
  @fun{gtk:recent-filter-needed} function.

  This function will not typically be used by applications. It is intended
  principally for use in the implementation of the @class{gtk:recent-chooser}
  class.
  @see-class{gtk:recent-filter}
  @see-class{gtk:recent-chooser}
  @see-symbol{gtk:recent-filter-info}
  @see-function{gtk:recent-filter-needed}"
  (filter (g:object recent-filter))
  (filter-info (:pointer (:struct recent-filter-info))))

(export 'recent-filter-filter)

;;; --- End of file gtk3.recent-filter.lisp ------------------------------------
