;;; ----------------------------------------------------------------------------
;;; gtk3.buildable.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
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
;;; GtkBuildable
;;;
;;;     Interface for objects that can be built by GtkBuilder
;;;
;;; Types and Values
;;;
;;;     GtkBuildable
;;;
;;; Functions
;;;
;;;     gtk_buildable_set_name
;;;     gtk_buildable_get_name
;;;     gtk_buildable_add_child
;;;     gtk_buildable_set_buildable_property
;;;     gtk_buildable_construct_child
;;;     gtk_buildable_custom_tag_start
;;;     gtk_buildable_custom_tag_end
;;;     gtk_buildable_custom_finished
;;;     gtk_buildable_parser_finished
;;;     gtk_buildable_get_internal_child
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuildable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkBuildable" buildable
  (:export t
   :type-initializer "gtk_buildable_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'buildable)
      "Interface"
      (documentation 'buildable 'type)
 "@version{#2023-3-15}
  @begin{short}
    Interface for objects that can be built by a @class{gtk:builder} UI
    description.
  @end{short}

  The @sym{gtk:buildable} interface allows objects to extend and customize
  their deserialization from @class{gtk:builder} UI descriptions. The interface
  includes methods for setting names and properties of objects, parsing custom
  tags and constructing child objects.

  The @sym{gtk:buildable} interface is implemented by all widgets and many of
  the non-widget objects that are provided by GTK. The main user of this
  interface is the @class{gtk:builder} class. There should be very little need
  for applications to call any functions from the @sym{gtk:buildable} interface.
  @begin[Note]{dictionary}
    An object only needs to implement this interface if it needs to extend the
    @class{gtk:builder} format or run any extra routines at deserialization
    time.
  @end{dictionary}
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_set_name ()
;;; gtk_buildable_get_name () -> buildable-name
;;; ----------------------------------------------------------------------------

(defun (setf buildable-name) (name buildable)
  (cffi:foreign-funcall "gtk_buildable_set_name"
                        (g:object buildable) buildable
                        :string name
                        :void)
  name)

(defcfun ("gtk_buildable_get_name" buildable-name) :string
 #+liber-documentation
 "@version{#2023-3-15}
  @syntax[]{(gtk:buildable-name buildable) => name}
  @syntax[]{(setf (gtk:buildable-name buildable) name)}
  @argument[buildable]{a @class{gtk:buildable} widget}
  @argument[name]{a string with the name}
  @begin{short}
    Accessor of the name of the buildable widget.
  @end{short}
  The @sym{gtk:buildable-name} function gets the name of the buildable widget.
  The @sym{(setf gtk:buildable-name)} function sets the name.

  The @class{gtk:builder} object sets the name based on the the
  @class{gtk:builder} UI definition used to construct the buildable widget.
  @see-class{gtk:buildable}
  @see-class{gtk:builder}"
  (buildable (g:object buildable)))

(export 'buildable-name)

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_add_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_buildable_add_child" %buildable-add-child) :void
  (buildable (g:object buildable))
  (builder (g:object builder))
  (child g:object)
  (child-type :string))

(defun buildable-add-child (buildable builder child &optional type)
 #+liber-documentation
 "@version{#2023-3-15}
  @argument[buildable]{a @class{gtk:buildable} widget}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[child]{a @class{g:object} child widget to add}
  @argument[type]{an optional string with the kind of the child widget}
  @begin{short}
    Adds a child widget to the buildable widget.
  @end{short}
  The @arg{type} argument is an optional string describing how the child widget
  should be added, the default value is @code{nil}.
  @see-class{gtk:buildable}
  @see-class{gtk:builder}
  @see-class{g:object}"
  (if type
      (%buildable-add-child buildable builder child type)
      (%buildable-add-child buildable builder Child (cffi:null-pointer))))

(export 'buildable-add-child)

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_set_buildable_property ()
;;;
;;; void gtk_buildable_set_buildable_property (GtkBuildable *buildable,
;;;                                            GtkBuilder *builder,
;;;                                            const gchar *name,
;;;                                            const GValue *value);
;;;
;;; Sets the property name name to value on the buildable object.
;;;
;;; buildable :
;;;     a GtkBuildable
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; name :
;;;     name of property
;;;
;;; value :
;;;     value of property
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_construct_child ()
;;;
;;; GObject * gtk_buildable_construct_child (GtkBuildable *buildable,
;;;                                          GtkBuilder *builder,
;;;                                          const gchar *name);
;;;
;;; Constructs a child of buildable with the name name.
;;;
;;; GtkBuilder calls this function if a "constructor" has been specified in the
;;; UI definition.
;;;
;;; buildable :
;;;     A GtkBuildable
;;;
;;; builder :
;;;     GtkBuilder used to construct this object
;;;
;;; name :
;;;     name of child to construct
;;;
;;; Returns :
;;;     the constructed child
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_custom_tag_start ()
;;;
;;; gboolean gtk_buildable_custom_tag_start (GtkBuildable *buildable,
;;;                                          GtkBuilder *builder,
;;;                                          GObject *child,
;;;                                          const gchar *tagname,
;;;                                          GMarkupParser *parser,
;;;                                          gpointer *data);
;;;
;;; This is called for each unknown element under <child>.
;;;
;;; buildable :
;;;     a GtkBuildable
;;;
;;; builder :
;;;     a GtkBuilder used to construct this object
;;;
;;; child :
;;;     child object or NULL for non-child tags
;;;
;;; tagname :
;;;     name of tag
;;;
;;; parser :
;;;     a GMarkupParser structure to fill in
;;;
;;; data :
;;;     return location for user data that will be passed in to parser
;;;     functions
;;;
;;; Returns :
;;;     TRUE if a object has a custom implementation, FALSE if it does not.
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_custom_tag_end ()
;;;
;;; void gtk_buildable_custom_tag_end (GtkBuildable *buildable,
;;;                                    GtkBuilder *builder,
;;;                                    GObject *child,
;;;                                    const gchar *tagname,
;;;                                    gpointer *data);
;;;
;;; This is called at the end of each custom element handled by the buildable.
;;;
;;; buildable :
;;;     A GtkBuildable
;;;
;;; builder :
;;;     GtkBuilder used to construct this object
;;;
;;; child :
;;;     child object or NULL for non-child tags
;;;
;;; tagname :
;;;     name of tag
;;;
;;; data :
;;;     user data that will be passed in to parser functions
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_custom_finished ()
;;;
;;; void gtk_buildable_custom_finished (GtkBuildable *buildable,
;;;                                     GtkBuilder *builder,
;;;                                     GObject *child,
;;;                                     const gchar *tagname,
;;;                                     gpointer data);
;;;
;;; This is similar to gtk_buildable_parser_finished() but is called once for
;;; each custom tag handled by the buildable.
;;;
;;; buildable :
;;;     a GtkBuildable
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; child :
;;;     child object or NULL for non-child tags
;;;
;;; tagname :
;;;     the name of the tag
;;;
;;; data :
;;;     user data created in custom_tag_start
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parser_finished ()
;;;
;;; void gtk_buildable_parser_finished (GtkBuildable *buildable,
;;;                                     GtkBuilder *builder);
;;;
;;; Called when the builder finishes the parsing of a GtkBuilder UI definition.
;;; Note that this will be called once for each time gtk_builder_add_from_file()
;;; or gtk_builder_add_from_string() is called on a builder.
;;;
;;; buildable :
;;;     a GtkBuildable
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_get_internal_child () -> buildable-internal-child
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_buildable_get_internal_child" buildable-internal-child) g:object
 #+liber-documentation
 "@version{#2023-3-15}
  @argument[buildable]{a @class{gtk:buildable} widget}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string with the name of the child widget}
  @return{The internal child widget of the buildable widget.}
  @begin{short}
    Gets the internal child widget called @arg{name} of the buildable widget.
  @end{short}
  @see-class{gtk:buildable}
  @see-class{gtk:builder}"
  (buildable (g:object buildable))
  (builder (g:object builder))
  (name :string))

(export 'buildable-internal-child)

;;; --- End of file gtk3.buildable.lisp ----------------------------------------
