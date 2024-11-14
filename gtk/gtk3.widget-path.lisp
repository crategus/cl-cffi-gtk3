;;; ----------------------------------------------------------------------------
;;; gtk3.widget-path.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
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
;;; GtkWidgetPath
;;;
;;;     Widget path abstraction
;;;
;;; Types and Values
;;;
;;;     GtkWidgetPath
;;;     GtkRegionFlags                               from gtk-style-context.lisp
;;;
;;; Functions
;;;
;;;     gtk_widget_path_append_type
;;;     gtk_widget_path_append_with_siblings
;;;     gtk_widget_path_append_for_widget
;;;     gtk_widget_path_copy                               missing
;;;     gtk_widget_path_ref                                missing
;;;     gtk_widget_path_unref                              missing
;;;     gtk_widget_path_free                               missing
;;;     gtk_widget_path_get_object_type
;;;     gtk_widget_path_has_parent
;;;     gtk_widget_path_is_type
;;;     gtk_widget_path_iter_add_class
;;;     gtk_widget_path_iter_add_region
;;;     gtk_widget_path_iter_clear_classes
;;;     gtk_widget_path_iter_clear_regions
;;;     gtk_widget_path_iter_get_name
;;;     gtk_widget_path_iter_get_object_name
;;;     gtk_widget_path_iter_get_object_type
;;;     gtk_widget_path_iter_get_siblings
;;;     gtk_widget_path_iter_get_sibling_index
;;;     gtk_widget_path_iter_get_state
;;;     gtk_widget_path_iter_has_class
;;;     gtk_widget_path_iter_has_name
;;;     gtk_widget_path_iter_has_qclass                    missing
;;;     gtk_widget_path_iter_has_qname                     missing
;;;     gtk_widget_path_iter_has_qregion                   missing
;;;     gtk_widget_path_iter_has_region
;;;     gtk_widget_path_iter_list_classes
;;;     gtk_widget_path_iter_list_regions
;;;     gtk_widget_path_iter_remove_class
;;;     gtk_widget_path_iter_remove_region
;;;     gtk_widget_path_iter_set_name
;;;     gtk_widget_path_iter_set_object_name
;;;     gtk_widget_path_iter_set_object_type
;;;     gtk_widget_path_iter_set_state
;;;     gtk_widget_path_length
;;;     gtk_widget_path_new
;;;     gtk_widget_path_prepend_type
;;;     gtk_widget_path_to_string
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkRegionFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkRegionFlags" region-flags
  (:export t
   :type-initializer "gtk_region_flags_get_type")
  (:even #.(ash 1 0))
  (:odd #.(ash 1 1))
  (:first #.(ash 1 2))
  (:last #.(ash 1 3))
  (:only #.(ash 1 4))
  (:sorted #.(ash 1 5)))

#+liber-documentation
(setf (liber:alias-for-symbol 'region-flags)
      "GFlags"
      (liber:symbol-documentation 'region-flags)
 "@version{#2024-3-21}
  @begin{declaration}
(gobject:define-gflags \"GtkRegionFlags\" region-flags
  (:export t
   :type-initializer \"gtk_region_flags_get_type\")
  (:even #.(ash 1 0))
  (:odd #.(ash 1 1))
  (:first #.(ash 1 2))
  (:last #.(ash 1 3))
  (:only #.(ash 1 4))
  (:sorted #.(ash 1 5)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:even]{Region has an even number within a set.}
      @entry[:odd]{Region has an odd number within a set.}
      @entry[:first]{Region is the first one within a set.}
      @entry[:last]{Region is the last one within a set.}
      @entry[:only]{Region is the only one within a set.}
      @entry[:sorted]{Region is part of a sorted area.}
    @end{table}
  @end{values}
  @begin{short}
    Describes a region within a widget.
  @end{short}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; GtkWidgetPath
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_new" %widget-path-new) :pointer)

(glib:define-gboxed-opaque widget-path "GtkWidgetPath"
  :export t
  :type-initializer "gtk_widget_path_get_type"
  :alloc (%widget-path-new))

#+liber-documentation
(setf (liber:alias-for-class 'widget-path)
      "GBoxed"
      (documentation 'widget-path 'type)
 "@version{#2023-3-30}
  @begin{short}
    The @class{gtk:widget-path} structure is a boxed type that represents a
    widget hierarchy from the topmost widget, typically a toplevel, to any
    child.
  @end{short}
  The @class{gtk:widget-path} structure is opaque, and has no user visible
  fields. This widget path abstraction is used in the @class{gtk:style-context}
  implementation on behalf of the real widget in order to query style
  information.

  If you are using GTK widgets, you probably will not need to use this API
  directly, as there is the @fun{gtk:widget-path} function, and the style
  context returned by the @fun{gtk:widget-style-context} function will be
  automatically updated on widget hierarchy changes.
  @begin[Examples]{dictionary}
    Defining a button within a window:
    @begin{pre}
(let ((path (gtk:widget-path-new)))
  (gtk:widget-path-append-type path \"GtkWindow\")
  (gtk:widget-path-append-type path \"GtkButton\")
  ... )
    @end{pre}
    Although more complex information, such as widget names, or different
    classes (property that may be used by other widget types) and intermediate
    regions may be included:

    Defining the first tab widget in a notebook:
    @begin{pre}
(let ((path (gtk:widget-path-new)))
  (gtk:widget-path-iter-add-region
      path
      (gtk:widget-path-append-type path \"GtkNotebook\")
      \"tab\"
      '(:even :first))
  (setf (gtk:widget-path-iter-name
            path
            (gtk:widget-path-append-type path \"GtkLabel\"))
        \"first tab label\")
  ... )
    @end{pre}
    All this information will be used to match the style information that
    applies to the described widget.
  @end{dictionary}
  @see-class{gtk:style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_append_type" widget-path-append-type) :int
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[gtype]{a @class{g:type-t} type ID of the widget to append}
  @return{An integer with the position where the element was inserted.}
  @begin{short}
    Appends a widget type to the widget hierarchy represented by @arg{path}.
  @end{short}
  @see-class{gtk:widget-path}
  @see-class{g:type-t}"
  (path (g:boxed widget-path))
  (gtype g:type-t))

(export 'widget-path-append-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_with_siblings ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_append_with_siblings"
               widget-path-append-with-siblings) :int
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[siblings]{a @class{gtk:widget-path} instance describing a list of
    siblings, this path may not contain any siblings itself and it must not be
    modified afterwards}
  @argument[index]{an unsigned integer with the index into @arg{siblings} for
    where the added element is positioned}
  @return{An integer with the position where the element was inserted.}
  @begin{short}
    Appends a widget type with all its siblings to the widget hierarchy
    represented by @arg{path}.
  @end{short}
  Using this function instead of the @fun{gtk:widget-path-append-type} function
  will allow the CSS theming to use sibling matches in selectors and apply
  @code{:nth-child()} pseudo classes. In turn, it requires a lot more care in
  widget implementations as widgets need to make sure to call the
  @fun{gtk:widget-reset-style} function on all involved widgets when the
  siblings path changes.
  @see-class{gtk:widget-path}
  @see-function{gtk:widget-reset-style}
  @see-function{gtk:widget-append-type}"
  (path (g:boxed widget-path))
  (siblings (g:boxed widget-path))
  (index :uint))

(export 'widget-path-append-with-siblings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_append_for_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_append_for_widget"
               widget-path-append-for-widget) :int
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[widget]{a @class{gtk:widget} object to append to the widget path}
  @return{An integer with the position where the data was inserted.}
  @begin{short}
    Appends the data from @arg{widget} to the widget hierarchy represented by
    @arg{path}.
  @end{short}
  This function is a shortcut for adding information from a widget to the given
  path. This includes setting the name or adding the style classes from a
  widget.
  @see-class{gtk:widget-path}
  @see-class{gtk:widget}"
  (path (g:boxed widget-path))
  (widget (g:object widget)))

(export 'widget-path-append-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_copy" widget-path-copy)
    (g:boxed widget-path :return)
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @return{Returns a copy of @arg{path}.}
  @short{Returns a copy of a widget path.}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path)))

(export 'widget-path-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_ref ()
;;;
;;; GtkWidgetPath * gtk_widget_path_ref (GtkWidgetPath *path);
;;;
;;; Increments the reference count on path.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Returns :
;;;     path itself.
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_unref ()
;;;
;;; void gtk_widget_path_unref (GtkWidgetPath *path);
;;;
;;; Decrements the reference count on path, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Since 3.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_free ()
;;;
;;; void gtk_widget_path_free (GtkWidgetPath *path);
;;;
;;; Decrements the reference count on path, freeing the structure if the
;;; reference count reaches 0.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_get_object_type () -> widget-path-object-type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_get_object_type" widget-path-object-type)
    g:type-t
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @return{The @class{g:type-t} type ID of the object.}
  @begin{short}
    Returns the topmost object type.
  @end{short}
  That is, the object type this path is representing.
  @see-class{gtk:widget-path}
  @see-class{g:type-t}"
  (path (g:boxed widget-path)))

(export 'widget-path-object-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_has_parent ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_has_parent" widget-path-has-parent) :boolean
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[gtype]{a @class{g:type-t} type ID of the widget to check in parents}
  @return{@em{True} if any parent is of @arg{gtype} type.}
  @begin{short}
    Returns @em{true} if any of the parents of the widget represented in
    @arg{path} is of @arg{gtype} type, or any subtype of it.
  @end{short}
  @see-class{gtk:widget-path}
  @see-class{g:type-t}"
  (path (g:boxed widget-path))
  (gtype g:type-t))

(export 'widget-path-has-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_is_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_is_type" widget-path-is-type) :boolean
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[gtype]{a @class{g:type-t} type ID of the widget to match}
  @return{@em{True} if the widget represented by @arg{path} is of @arg{gtype}
    type.}
  @begin{short}
    Returns @em{true} if the widget type represented by this path is
    @arg{gtype}, or a subtype of it.
  @end{short}
  @see-class{gtk:widget-path}
  @see-class{g:type-t}"
  (path (g:boxed widget-path))
  (gtype g:type-t))

(export 'widget-path-is-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_add_class ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_add_class" widget-path-iter-add-class)
    :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to modify, -1 for the path head}
  @argument[name]{a string with a class name}
  @begin{short}
    Adds the class name to the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  See the @fun{gtk:style-context-add-class} function.
  @see-class{gtk:widget-path}
  @see-function{gtk:style-context-add-class}"
  (path (g:boxed widget-path))
  (pos :int)
  (name :string))

(export 'widget-path-iter-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_add_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_add_region" widget-path-iter-add-region)
    :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to modify, -1 for the path head}
  @argument[name]{a string with the region name}
  @argument[flags]{a @symbol{gtk:region-flags} value affecting the region}
  @begin{short}
    Adds the region name to the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  See the @fun{gtk:style-context-add-region} function.
  @begin[Notes]{dictionary}
    Region names must only contain lowercase letters and '-', starting always
    with a lowercase letter.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-path-iter-add-region} function has been deprecated since
    version 3.14 and should not be used in newly written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk:widget-path}
  @see-symbol{gtk:region-flags}
  @see-function{gtk:style-context-add-region}"
  (path (g:boxed widget-path))
  (pos :int)
  (name :string)
  (flags region-flags))

(export 'widget-path-iter-add-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_clear_classes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_clear_classes"
               widget-path-iter-clear-classes) :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to modify, -1 for the path head}
  @begin{short}
    Removes all classes from the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-clear-classes)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_clear_regions ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_clear_regions"
               widget-path-iter-clear-regions) :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to modify, -1 for the path head}
  @begin{short}
    Removes all regions from the widget at position @arg{pos} in the hierarchy
    defined in @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-path-iter-clear-regions} function has been deprecated
    since version 3.14 and should not be used in newly written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-clear-regions)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_name ()
;;; gtk_widget_path_iter_set_name ()
;;; ----------------------------------------------------------------------------

(defun (setf widget-path-iter-name) (value path pos)
  (cffi:foreign-funcall "gtk_widget_path_iter_set_name"
                        (g:boxed widget-path) path
                        :int pos
                        :string value)
  value)

(cffi:defcfun ("gtk_widget_path_iter_get_name" widget-path-iter-name) :string
 #+liber-documentation
 "@version{#2023-3-30}
  @syntax{(gtk:widget-path-iter-name path pos) => name}
  @syntax{(setf (gtk:widget-path-iter-name path pos) name)}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position, -1 for the path head}
  @argument[name]{a string with the widget name}
  @begin{short}
    Accessor of the widget name.
  @end{short}
  The @fun{gtk:widget-path-iter-name} function returns the name corresponding
  to the widget found at the position @arg{pos} in the widget hierarchy defined
  by @arg{path}. The @setf{gtk:widget-path-iter-name} function sets the widget
  name.
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_object_name ()
;;; gtk_widget_path_iter_set_object_name ()
;;; ----------------------------------------------------------------------------

(defun (setf widget-path-iter-object-name) (value path pos)
  (cffi:foreign-funcall "gtk_widget_path_iter_set_object_name"
                        (g:boxed widget-path) path
                        :int pos
                        :string value)
  value)

(cffi:defcfun ("gtk_widget_path_iter_get_object_name"
               widget-path-iter-object-name) :string
 #+liber-documentation
 "@version{#2023-3-30}
  @syntax{(gtk:widget-path-iter-object-name path pos) => name}
  @syntax{(setf (gtk:widget-path-iter-object-name path pos) name)}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position, -1 for the path head}
  @argument[name]{a string with the object name to set or @code{nil} to unset}
  @begin{short}
    Accessor of the object name.
  @end{short}
  The @fun{gtk:widget-path-iter-object-name} function returns the object name
  that is at position @arg{pos} in the widget hierarchy defined in @arg{path}.
  The @setf{gtk:widget-path-iter-object-name} function sets the object name.
  When set, the object name overrides the object type when matching CSS.
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-object-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_object_type ()
;;; gtk_widget_path_iter_set_object_type ()
;;; ----------------------------------------------------------------------------

(defun (setf widget-path-iter-object-type) (value path pos)
  (cffi:foreign-funcall "gtk_widget_path_iter_set_object_type"
                        (g:boxed widget-path) path
                        :int pos
                        g:type-t value)
  value)

(cffi:defcfun ("gtk_widget_path_iter_get_object_type"
               widget-path-iter-object-type) g:type-t
 #+liber-documentation
 "@version{#2023-3-30}
  @syntax{(gtk:widget-path-iter-object-type path pos) => gtype}
  @syntax{(setf (gtk:widget-path-iter-object-type path pos) gtype)}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position, -1 for the path head}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the object type.
  @end{short}
  The @fun{gtk:widget-path-iter-object-type} function returns the
  @class{g:type-t} type ID of the object that is at position @arg{pos} in the
  widget hierarchy defined in @arg{path}. The
  @setf{gtk:widget-path-iter-object-type} function sets the object type.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq widget (make-instance 'gtk:button))
=> #<GTK-BUTTON {10027EB373@}>
(gtk:widget-path-iter-object-type (gtk:widget-path *) -1)
=> #<GTYPE :name \"GtkButton\" :id 23267040>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget-path}
  @see-class{g:type-t}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-object-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_siblings ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_get_siblings" widget-path-iter-siblings)
    (g:boxed widget-path)
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to get the siblings for, -1 for
    the path head}
  @return{A @class{gtk:widget-path} instance with the list of siblings for the
    element at @arg{pos}.}
  @begin{short}
    Returns the list of siblings for the element at @arg{pos}.
  @end{short}
  If the element was not added with siblings, @code{nil} is returned.
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-siblings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_sibling_index ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_get_sibling_index"
               widget-path-iter-sibling-index) :uint
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to get the sibling index for,
    -1 for the path head}
  @return{An unsigned integer with the index into the list of siblings for the
    element at @arg{pos}.}
  @begin{short}
    Returns the index into the list of siblings for the element at @arg{pos} as
    returned by the @fun{gtk:widget-path-iter-siblings} function.
  @end{short}
  If that function would return @code{nil} because the element at @arg{pos} has
  no siblings, this function will return 0.
  @see-class{gtk:widget-path}
  @see-function{gtk:widget-path-iter-siblings}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-sibling-index)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_get_state ()
;;; gtk_widget_path_iter_set_state ()
;;; ----------------------------------------------------------------------------

(defun (setf widget-path-iter-state) (value path pos)
  (cffi:foreign-funcall "gtk_widget_path_iter_set_state"
                        (g:boxed widget-path) path
                        :int pos
                        state-flags value)
  value)

(cffi:defcfun ("gtk_widget_path_iter_get_state"
               widget-path-iter-state) state-flags
 #+liber-documentation
 "@version{#2023-3-30}
  @syntax{(gtk:widget-path-iter-state path pos) => state}
  @syntax{(setf (gtk:widget-path-iter-state path pos) state)}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position , -1 for the path head}
  @argument[state]{a @symbol{gtk:state-flags} value to set or unset}
  @begin{short}
    Accessor of the state flags.
  @end{short}
  The @fun{gtk:widget-path-iter-state} function returns the state flags
  corresponding to the widget found at the position @arg{pos} in the widget
  hierarchy defined by @arg{path}. The @setf{gtk:widget-path-iter-state}
  function sets the state flags.

  If you want to update just a single state flag, you need to do this manually,
  as this function updates all state flags.
  @begin[Examples]{dictionary}
    Setting more flags
    @begin{pre}
(let ((flags (gtk:widget-path-iter-state path pos)))
  (setf (gtk:widget-path-iter-state path pos)
        (union flags '(:dir-ltr :selected))))
    @end{pre}
    Unsetting a flag
    @begin{pre}
(let ((flags (gtk:widget-path-iter-state path pos)))
  (setf (gtk:widget-path-iter-state path pos)
        (set-difference flags '(:active))))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget-path}
  @see-symbol{gtk:state-flags}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-state)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_class ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_has_class" widget-path-iter-has-class)
    :boolean
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to query, -1 for the path head}
  @argument[name]{a string with a class name}
  @return{@em{True} if the class name is defined for the widget at @arg{pos}.}
  @begin{short}
    Returns @em{true} if the widget at position @arg{pos} has the class name
    defined, @em{false} otherwise.
  @end{short}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int)
  (name :string))

(export 'widget-path-iter-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_has_name" widget-path-iter-has-name)
    :boolean
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to query, -1 for the path head}
  @argument[name]{a string with a widget name}
  @return{@em{True} if the widget at @arg{pos} has this name.}
  @begin{short}
    Returns @em{true} if the widget at position @arg{pos} has the name
    @arg{name}, @em{false} otherwise.
  @end{short}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int)
  (name :string))

(export 'widget-path-iter-has-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_qclass ()
;;;
;;; gboolean gtk_widget_path_iter_has_qclass (const GtkWidgetPath *path,
;;;                                           gint pos,
;;;                                           GQuark qname);
;;;
;;; See gtk_widget_path_iter_has_class(). This is a version that operates with
;;; GQuarks.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; qname :
;;;     class name as a GQuark
;;;
;;; Returns :
;;;     TRUE if the widget at pos has the class defined.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_qname ()
;;;
;;; gboolean gtk_widget_path_iter_has_qname (const GtkWidgetPath *path,
;;;                                          gint pos,
;;;                                          GQuark qname);
;;;
;;; See gtk_widget_path_iter_has_name(). This is a version that operates on
;;; GQuarks.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; qname :
;;;     widget name as a GQuark
;;;
;;; Returns :
;;;     TRUE if the widget at pos has this name
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_qregion ()
;;;
;;; gboolean gtk_widget_path_iter_has_qregion (const GtkWidgetPath *path,
;;;                                            gint pos,
;;;                                            GQuark qname,
;;;                                            GtkRegionFlags *flags);
;;;
;;; See gtk_widget_path_iter_has_region(). This is a version that operates with
;;; GQuarks.
;;;
;;; Warning
;;;
;;; gtk_widget_path_iter_has_qregion has been deprecated since version 3.14 and
;;; should not be used in newly written code.
;;;
;;; The use of regions is deprecated.
;;;
;;; path :
;;;     a GtkWidgetPath
;;;
;;; pos :
;;;     position to query, -1 for the path head
;;;
;;; qname :
;;;     region name as a GQuark
;;;
;;; flags :
;;;     return location for the region flags. [out]
;;;
;;; Returns :
;;;     TRUE if the widget at pos has the region defined.
;;;
;;; Since 3.0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_has_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_has_region" %widget-path-iter-has-region)
    :boolean
  (path (g:boxed widget-path))
  (pos :int)
  (name :string)
  (flags :pointer))

(defun widget-path-iter-has-region (path pos name)
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to query, -1 for the path head}
  @argument[name]{a string with a region name}
  @return{Returns the @symbol{gtk:region-flags} region flags.}
  @begin{short}
    Returns the region flags corresponding to the widget found at the position
    @arg{pos} in the widget hierarchy defined by @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-path-iter-has-region} function has been deprecated since
    version 3.14 and should not be used in newly written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk:widget-path}
  @see-symbol{gtk:region-flags}"
  (cffi:with-foreign-object (flags 'region-flags)
    (%widget-path-iter-has-region path pos name flags)
    (cffi:mem-ref flags 'region-flags)))

(export 'widget-path-iter-has-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_list_classes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_list_classes"
               widget-path-iter-list-classes) g:strv-t
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to query, -1 for the path head}
  @return{Returns a list of strings with the class names.}
  @begin{short}
    Returns a list of strings with all the class names defined for the widget
    at position @arg{pos} in the hierarchy defined in @arg{path}.
  @end{short}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-list-classes)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_list_regions ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_list_regions"
               widget-path-iter-list-regions) g:strv-t
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to query, -1 for the path head}
  @return{Returns a list of strings with the region names.}
  @begin{short}
    Returns a list with all the region names defined for the widget at position
    @arg{pos} in the hierarchy defined in @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-path-iter-list-regions} function has been deprecated
    since version 3.14 and should not be used in newly written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int))

(export 'widget-path-iter-list-regions)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_remove_class ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_remove_class"
               widget-path-iter-remove-class) :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to modify, -1 for the path head}
  @argument[name]{a string with a class name}
  @begin{short}
    Removes the class name from the widget at position @arg{pos} in the
    hierarchy defined in @arg{path}.
  @end{short}
  @see-class{gtk:widget-path}"
 (path (g:boxed widget-path))
 (pos :int)
 (name :string))

(export 'widget-path-iter-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_iter_remove_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_iter_remove_region"
               widget-path-iter-remove-region) :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[pos]{an integer with the position to modify, -1 for the path head}
  @argument[name]{a string with a region name}
  @begin{short}
    Removes the region name from the widget at position @arg{pos} in the
    hierarchy defined in @arg{path}.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:widget-path-iter-remove-region} function has been deprecated
    since version 3.14 and should not be used in newly written code. The use of
    regions is deprecated.
  @end{dictionary}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path))
  (pos :int)
  (name :string))

(export 'widget-path-iter-remove-region)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_length ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_length" widget-path-length) :int
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @return{An integer with the number of elements in the path.}
  @begin{short}
    Returns the number of widget types between the represented widget and its
    topmost container.
  @end{short}
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path)))

(export 'widget-path-length)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_new" widget-path-new) (g:boxed widget-path)
 #+liber-documentation
 "@version{#2023-3-30}
  @return{A newly created, empty, @class{gtk:widget-path} instance.}
  @short{Returns an empty widget path.}
  @see-class{gtk:widget-path}")

(export 'widget-path-new)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_prepend_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_prepend_type" widget-path-prepend-type) :void
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @argument[gtype]{a @class{g:type-t} type ID to prepend}
  @begin{short}
    Prepends a widget type to the widget hierachy represented by @arg{path}.
  @end{short}
  @see-class{gtk:widget-path}
  @see-class{g:type-t}"
  (path (g:boxed widget-path))
  (gtype g:type-t))

(export 'widget-path-prepend-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_path_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_path_to_string" widget-path-to-string) :string
 #+liber-documentation
 "@version{#2023-3-30}
  @argument[path]{a @class{gtk:widget-path} instance}
  @return{A new string describing @arg{path}.}
  @begin{short}
    Dumps the widget path into a string representation.
  @end{short}
  It tries to match the CSS style as closely as possible. Note that there might
  be paths that cannot be represented in CSS.

  The main use of this code is for debugging purposes.
  @see-class{gtk:widget-path}"
  (path (g:boxed widget-path)))

(export 'widget-path-to-string)

;;; --- End of file gtk3.widget-path.lisp  -------------------------------------
