;;; ----------------------------------------------------------------------------
;;; gtk3.builder.lisp
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkBuilder
;;;
;;;     Build an interface from an XML UI definition
;;;
;;; Types and Values
;;;
;;;     GtkBuilder
;;;     GtkBuilderError
;;;
;;; Accessors
;;;
;;;     gtk_builder_set_translation_domain
;;;     gtk_builder_get_translation_domain
;;;
;;; Functions
;;;
;;;     gtk_builder_new
;;;     gtk_builder_new_from_file
;;;     gtk_builder_new_from_resource
;;;     gtk_builder_new_from_string
;;;     gtk_builder_add_callback_symbol                     not implemented
;;;     gtk_builder_add_callback_symbols                    not implemented
;;;     gtk_builder_lookup_callback_symbol                  not implemented
;;;     gtk_builder_add_from_file
;;;     gtk_builder_add_from_resource
;;;     gtk_builder_add_from_string
;;;     gtk_builder_add_objects_from_file
;;;     gtk_builder_add_objects_from_string
;;;     gtk_builder_add_objects_from_resource
;;;     gtk_builder_extend_with_template                    not implemened
;;;     gtk_builder_get_object
;;;     gtk_builder_get_objects
;;;     gtk_builder_expose_object
;;;     GtkBuilderConnectFunc                               not exported
;;;     gtk_builder_connect_signals_full                    not exported
;;;     gtk_builder_connect_signals
;;;     gtk_builder_set_application
;;;     gtk_builder_get_application
;;;     gtk_builder_get_type_from_name
;;;     gtk_builder_value_from_string                       not implemented
;;;     gtk_builder_value_from_string_type                  not implemented
;;;
;;; Properties
;;;
;;;     translation-domain
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkBuilder
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkBuilderError                                   not exported
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkBuilderError" builder-error
  (:export nil
   :type-initializer "gtk_builder_error_get_type")
  (:invalid-type-function 0)
  (:unhandled-tag 1)
  (:missing-attribute 2)
  (:invalid-attribute 3)
  (:invalid-tag 4)
  (:missing-property-value 5)
  (:invalid-value 6)
  (:version-mismatch 7)
  (:duplicate-id 8)
  (:type-refused 9)
  (:template-mismatch 10)
  (:invalid-property 11)
  (:invalid-signal 12)
  (:invalid-id 13))

#+liber-documentation
(setf (liber:alias-for-symbol 'builder-error)
      "GEnum"
      (liber:symbol-documentation 'builder-error)
 "@version{#2024-3-21}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkBuilderError\" builder-error
  (:export t
   :type-initializer \"gtk_builder_error_get_type\")
  (:invalid-type-function 0)
  (:unhandled-tag 1)
  (:missing-attribute 2)
  (:invalid-attribute 3)
  (:invalid-tag 4)
  (:missing-property-value 5)
  (:invalid-value 6)
  (:version-mismatch 7)
  (:duplicate-id 8)
  (:type-refused 9)
  (:template-mismatch 10)
  (:invalid-property 11)
  (:invalid-signal 12)
  (:invalid-id 13))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:invalid-type-function]{A @code{type-func} attribute did not name
        a function that returns a @class{g:type-t} type ID.}
      @entry[:unhandled-tag]{The input contained a tag that a
        @class{gtk:builder} object cannot handle.}
      @entry[:missing-attribute]{An attribute that is required by a
        @class{gtk:builder} object was missing.}
      @entry[:invalid-attribute]{A @class{gtk:builder} object found an attribute
        that it does not understand.}
      @entry[:invalid-tag]{A @class{gtk:builder} object found a tag that it does
        not understand.}
      @entry[:missing-property-value]{A required property value was missing.}
      @entry[:invalid-value]{A @class{gtk:builder} object could not parse some
        attribute value.}
      @entry[:version-mismatch]{The input file requires a newer version of GTK.}
      @entry[:duplicate-id]{An object ID occurred twice.}
      @entry[:type-refused]{A specified object type is of the same type or
        derived from the type of the composite class being extended with builder
        XML.}
      @entry[:template-mismatch]{The wrong type was specified in a composite
        class’s template XML.}
      @entry[:invalid-property]{The specified property is unknown for the object
        class.}
      @entry[:invalid-signal]{The specified signal is unknown for the object
        class.}
      @entry[:invalid-id]{An object ID is unknown.}
    @end{table}
  @end{values}
  @begin{short}
    Error codes that identify various errors that can occur while parsing the
    @class{gtk:builder} UI definition.
  @end{short}
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; struct GtkBuilder
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkBuilder" builder
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_builder_get_type")
  ((translation-domain
    builder-translation-domain
    "translation-domain" "gchararray" t t)))

;;; This Lisp extension is not documented
(defmethod initialize-instance :after ((builder builder)
                                       &key from-file from-string)
  (when from-file
    (builder-add-from-file builder (namestring from-file)))
  (when from-string
    (builder-add-from-string builder from-string)))

#+liber-documentation
(setf (documentation 'builder 'type)
 "@version{2024-3-20}
  @begin{short}
    The @class{gtk:builder} object is an auxiliary object that reads textual
    descriptions of a user interface and instantiates the described objects.
  @end{short}
  To create a @class{gtk:builder} object from a user interface description,
  call the @fun{gtk:builder-new-from-file}, @fun{gtk:builder-new-from-resource}
  or @fun{gtk:builder-new-from-string} functions.

  In the (unusual) case that you want to add user interface descriptions from
  multiple sources to the same @class{gtk:builder} object you can call the
  @fun{gtk:builder-new} function to get an empty builder and populate it by
  (multiple) calls to the @fun{gtk:builder-add-from-file},
  @fun{gtk:builder-add-from-resource} or @fun{gtk:builder-add-from-string}
  functions.

  A @class{gtk:builder} object holds a reference to all objects that it has
  constructed and drops these references when it is finalized. This finalization
  can cause the destruction of non-widget objects or widgets which are not
  contained in a toplevel window. For toplevel windows constructed by a builder,
  it is the responsibility of the user to call the @fun{gtk:widget-destroy}
  function to get rid of them and all the widgets they contain.

  The @fun{gtk:builder-object} and @fun{gtk:builder-objects} functions can be
  used to access the widgets in the interface by the names assigned to them
  inside the UI description. Toplevel windows returned by these functions will
  stay around until the user explicitly destroys them with the
  @fun{gtk:widget-destroy} function. Other widgets will either be part of a
  larger hierarchy constructed by the builder, in which case you should not have
  to worry about their life cycle, or without a parent, in which case they have
  to be added to some container to make use of them.

  The @fun{gtk:builder-connect-signals} function can be used to connect
  handlers to the named signals in the UI description.
  @begin[GtkBuilder UI Definitions]{dictionary}
    The @class{gtk:builder} implementation parses textual descriptions of user
    interfaces which are specified in an XML format which can be roughly
    described by the
    @url[https://gitlab.gnome.org/GNOME/gtk/-/blob/gtk-3-24/gtk/gtkbuilder.rnc]{RELAX NG Compact Syntax}.
    We refer to these descriptions as @class{gtk:builder} UI definitions or just
    UI definitions if the context is clear. Do not confuse @class{gtk:builder}
    UI Definitions with the deprecated @class{gtk:ui-manager} UI Definitions,
    which are more limited in scope. It is common to use @code{.ui} as the
    filename extension for files containing @class{gtk:builder} UI definitions.

    The toplevel element is @code{<interface>}. It optionally takes a
    @code{\"domain\"} attribute, which will make the builder look for translated
    strings using GNU gettext in the domain specified. This can also be done by
    calling the @fun{gtk:builder-translation-domain} function on the builder.
    Objects are described by @code{<object>} elements, which can contain
    @code{<property>} elements to set properties, @code{<signal>} elements which
    connect signals to handlers, and @code{<child>} elements, which describe
    child objects, most often widgets inside a container, but also e.g. actions
    in an action group, or columns in a tree model. A @code{<child>} element
    contains an @code{<object>} element which describes the child object. The
    target toolkit version(s) are described by @code{<requires>} elements, the
    @code{\"lib\"} attribute specifies the widget library in question,
    (currently the only supported value is @code{\"gtk+\"} and the
    @code{\"version\"} attribute specifies the target version in the form
    @code{\"<major>.<minor>\"}. The builder will error out if the version
    requirements are not met.

    Typically, the specific kind of object represented by an @code{<object>}
    element is specified by the @code{\"class\"} attribute. If the type has not
    been loaded yet, GTK tries to find the @code{_get_type()} from the class
    name by applying heuristics. This works in most cases, but if necessary, it
    is possible to specify the name of the @code{_get_type()} explictly with the
    @code{\"type-func\"} attribute. As a special case, the @class{gtk:builder}
    implementation allows to use an object that has been constructed by a
    @class{gtk:ui-manager} object in another part of the UI definition by
    specifying the ID of the @class{gtk:ui-manager} object in the
    @code{\"constructor\"} attribute and the name of the object in the
    @code{\"id\"} attribute.

    Objects must be given a name with the @code{\"ID\"} attribute, which allows
    the application to retrieve them from the builder with the
    @fun{gtk:builder-object} function. An ID is also necessary to use the object
    as property value in other parts of the UI definition. GTK reserves IDs
    starting and ending with @code{___} (3 underscores) for its own purposes.

    Setting properties of objects is pretty straightforward with the
    @code{<property>} element: the @code{\"name\"} attribute specifies the name
    of the property, and the content of the element specifies the value. If the
    @code{\"translatable\"} attribute is set to a true value, GTK uses GNU
    gettext to find a translation for the value. This happens before the value
    is parsed, so it can be used for properties of any type, but it is probably
    most useful for string properties. It is also possible to specify a context
    to disambiguate short strings, and comments which may help the translators.

    The @class{gtk:builder} implementation can parse textual representations
    for the most common property types: characters, strings, integers, floating
    point numbers, booleans, strings like @code{\"TRUE\"}, @code{\"t\"},
    @code{\"yes\"}, @code{\"y\"}, @code{\"1\"} are interpreted as @em{true},
    strings like @code{\"FALSE\"}, @code{\"f\"}, @code{\"no\"}, @code{\"n\"},
    @code{\"0\"} are interpreted as @em{false}, enumerations, can be specified
    by their name, nick or integer value, flags, can be specified by their name,
    nick, integer value, optionally combined with @code{\"|\"}, e.g.
    @code{\"GTK_VISIBLE | GTK_REALIZED\"}, and colors, in a format understood
    by the @fun{gdk:rgba-parse} function. A @type{g:variant} instance can be
    specified in the format understood by the @fun{g:variant-parse} function,
    and pixbufs can be specified as a filename of an image file to load.

    Objects can be referred to by their name and by default refer to objects
    declared in the local XML fragment and objects exposed via the
    @fun{gtk:builder-expose-object} function. In general, the
    @class{gtk:builder} implementation allows forward references to objects -
    declared in the local XML. An object does not have to be constructed before
    it can be referred to. The exception to this rule is that an object has to
    be constructed before it can be used as the value of a construct-only
    property.

    It is also possible to bind a property value to another object’s property
    value using the attributes @code{\"bind-source\"} to specify the source
    object of the binding, @code{\"bind-property\"} to specify the source
    property and optionally @code{\"bind-flags\"} to specify the binding flags.
    Internally the builder implements this using @class{g:binding} objects. For
    more information see the @fun{g:object-bind-property} function.

    Signal handlers are set up with the @code{<signal>} element. The
    @code{\"name\"} attribute specifies the name of the signal, and the
    @code{\"handler\"} attribute specifies the function to connect to the
    signal. Use the @fun{gtk:builder-connect-signals} function to connect Lisp
    callback functions to the signal handlers. The remaining attributes,
    @code{\"after\"}, @code{\"swapped\"} and @code{\"object\"}, have the same
    meaning as the corresponding parameters of the @fun{g:signal-connect}
    function. A @code{\"last_modification_time\"} attribute is also allowed,
    but it does not have a meaning to the builder.

    Sometimes it is necessary to refer to widgets which have implicitly been
    constructed by GTK as part of a composite widget, to set properties on them
    or to add further children, e.g. the @code{vbox} of a @class{gtk:dialog}
    widget. This can be achieved by setting the @code{\"internal-child\"}
    propery of the @code{<child>} element to a @em{true} value. Note that a
    @class{gtk:builder} object still requires an @code{<object>} element for
    the internal child, even if it has already been constructed.

    A number of widgets have different places where a child can be added, e.g.
    tabs versus page content in notebooks. This can be reflected in a UI
    definition by specifying the @code{\"type\"} attribute on a @code{<child>}.
    The possible values for the @code{\"type\"} attribute are described in the
    sections describing the widget specific portions of UI definitions.

    Beyond this general structure, several object classes define their own XML
    DTD fragments for filling in the ANY placeholders in the DTD above. Note
    that a custom element in a @code{<child>} element gets parsed by the custom
    tag handler of the parent object, while a custom element in an
    @code{<object>} element gets parsed by the custom tag handler of the object.

    These XML fragments are explained in the documentation of the respective
    objects.

    Additionally, since 3.10 a special @code{<template>} tag has been added to
    the format allowing one to define a widget class's components.
  @end{dictionary}
  @begin{examples}
    A @class{gtk:builder} UI Definition
    @begin{pre}
<interface>
  <object class=\"GtkDialog\" id=\"dialog1\">
    <child internal-child=\"vbox\">
      <object class=\"GtkVBox\" id=\"vbox1\">
        <property name=\"border-width\">10</property>
        <child internal-child=\"action_area\">
          <object class=\"GtkHButtonBox\" id=\"hbuttonbox1\">
            <property name=\"border-width\">20</property>
            <child>
              <object class=\"GtkButton\" id=\"ok_button\">
                <property name=\"label\">gtk-ok</property>
                <property name=\"use-stock\">TRUE</property>
                <signal name=\"clicked\" handler=\"ok_button_clicked\"/>
              </object>
            </child>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>
    @end{pre}
  @end{examples}
  @see-constructor{gtk:builder-new}
  @see-constructor{gtk:builder-new-from-file}
  @see-constructor{gtk:builder-new-from-resource}
  @see-constructor{gtk:builder-new-from-string}
  @see-slot{gtk:builder-translation-domain}
  @see-class{gtk:buildable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "translation-domain" 'builder) t)
 "The @code{translation-domain} property of type @code{:string} (Read / Write)
  @br{}
  The translation domain used when translating property values that have been
  marked as translatable in interface descriptions. If the translation domain
  is @code{nil}, the @class{gtk:builder} object uses GNU gettext, otherwise
  GLIB gettext. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'builder-translation-domain)
      "Accessor"
      (documentation 'builder-translation-domain 'function)
 "@version{2024-3-16}
  @syntax{(gtk:builder-translation-domain object) => domain}
  @syntax{(setf (gtk:builder-translation-domain object) domain)}
  @argument[object]{a @class{gtk:builder} object}
  @argument[domain]{a string with the translation domain or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:builder]{translation-domain} slot of the
    @class{gtk:builder} class.
  @end{short}
  The @fun{gtk:builder-translation-domain} function gets the translation domain
  of @arg{object}. The @setf{gtk:builder-translation-domain} function sets the
  translation domain.
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline builder-new))

(defun builder-new ()
 #+liber-documentation
 "@version{2024-3-16}
  @return{The new @class{gtk:builder} object.}
  @short{Creates a new builder object.}
  @see-class{gtk:builder}
  @see-function{gtk:builder-new-from-file}
  @see-function{gtk:builder-new-from-resource}
  @see-function{gtk:builder-new-from-string}"
  (make-instance 'builder))

(export 'builder-new)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_new_from_file" %builder-new-from-file)
    (g:object builder)
  (filename :string))

(defun builder-new-from-file (path)
 #+liber-documentation
 "@version{2024-3-16}
  @argument[path]{a path or namestring with the file to load}
  @return{The @class{gtk:builder} object containing the described interface.}
  @begin{short}
    Builds the @class{gtk:builder} UI definition from a user interface
    description file.
  @end{short}
  If there is an error opening the file or parsing the description then the
  program will be aborted. You should only ever attempt to parse user interface
  descriptions that are shipped as part of your program.
  @see-class{gtk:builder}"
  (%builder-new-from-file (namestring path)))

(export 'builder-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_resource ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_new_from_resource" builder-new-from-resource)
    (g:object builder)
 #+liber-documentation
 "@version{2023-3-2}
  @argument[path]{a string with the @class{g:resource} path}
  @return{The @class{gtk:builder} object containing the described interface.}
  @begin{short}
    Builds the @class{gtk:builder} UI definition from a resource path.
  @end{short}
  If there is an error locating the resource or parsing the description then
  the program will be aborted.
  @see-class{gtk:builder}
  @see-class{g:resource}
  @see-function{gtk:builder-new}
  @see-function{gtk:builder-new-from-file}
  @see-function{gtk:builder-new-from-string}"
  (path :string))

(export 'builder-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_new_from_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_new_from_string" %builder-new-from-string)
    (g:object builder)
  (string :string)
  (length :int))

(defun builder-new-from-string (string)
 #+liber-documentation
 "@version{2023-3-2}
  @argument[string]{a string with the user interface description}
  @return{The @class{gtk:builder} object containing the interface described by
    @arg{string}.}
  @begin{short}
    Builds the user interface described by @arg{string} in the
    @class{gtk:builder} UI definition format.
  @end{short}
  If there is an error parsing the string then the program will be aborted. You
  should not attempt to parse user interface description from untrusted
  sources.
  @see-class{gtk:builder}
  @see-function{gtk:builder-new}
  @see-function{gtk:builder-new-from-file}
  @see-function{gtk:builder-new-from-resource}"
  (%builder-new-from-string string -1))

(export 'builder-new-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_callback_symbol ()
;;;
;;; void
;;; gtk_builder_add_callback_symbol (GtkBuilder *builder,
;;;                                  const gchar *callback_name,
;;;                                  GCallback callback_symbol)
;;;
;;; Adds the callback_symbol to the scope of builder under the given
;;; callback_name .
;;;
;;; Using this function overrides the behavior of gtk_builder_connect_signals()
;;; for any callback symbols that are added. Using this method allows for better
;;; encapsulation as it does not require that callback symbols be declared in
;;; the global namespace.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; callback_name :
;;;     The name of the callback, as expected in the XML
;;;
;;; callback_symbol :
;;;     The callback pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_callback_symbols ()
;;;
;;; void
;;; gtk_builder_add_callback_symbols (GtkBuilder *builder,
;;;                                   const gchar *first_callback_name,
;;;                                   GCallback first_callback_symbol,
;;;                                   ...)
;;;
;;; A convenience function to add many callbacks instead of calling
;;; gtk_builder_add_callback_symbol() for each symbol.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; first_callback_name :
;;;     The name of the callback, as expected in the XML
;;;
;;; first_callback_symbol :
;;;     The callback pointer.
;;;
;;; ... :
;;;     A list of callback name and callback symbol pairs terminated with NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_lookup_callback_symbol ()
;;;
;;; GCallback
;;; gtk_builder_lookup_callback_symbol (GtkBuilder *builder,
;;;                                     const gchar *callback_name)
;;;
;;; Fetches a symbol previously added to builder with
;;; gtk_builder_add_callback_symbols()
;;;
;;; This function is intended for possible use in language bindings or for any
;;; case that one might be cusomizing signal connections using
;;; gtk_builder_connect_signals_full()
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; callback_name :
;;;     The name of the callback
;;;
;;; Returns :
;;;     The callback symbol in builder for callback_name , or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_from_file" %builder-add-from-file) :uint
  (builder (g:object builder))
  (filename :string)
  (err :pointer))

(defun builder-add-from-file (builder path)
 #+liber-documentation
 "@version{2024-3-20}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a path or namestring with the name of the file to parse}
  @return{The unsigned integer with a positive value on success, 0 if an
    error occurred.}
  @begin{short}
    Parses a file containing a @class{gtk:builder} UI definition and merges it
    with the current contents of the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-from-resource}
  @see-function{gtk:builder-add-from-string}"
  (glib:with-g-error (err)
    (%builder-add-from-file builder (namestring path) err)))

(export 'builder-add-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_resource ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_from_resource" %builder-add-from-resource) :uint
  (builder (g:object builder))
  (path :string)
  (err :pointer))

(defun builder-add-from-resource (builder path)
 #+liber-documentation
 "@version{2023-3-20}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a string with the path of the resouce file to parse}
  @return{The unsigned integer with a positive value on success, 0 if an error
    occured.}
  @begin{short}
    Parses a resource file containing a @class{gtk:builder} UI definition and
    merges it with the current contents of the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:resource}
  @see-function{gtk:builder-add-from-file}
  @see-function{gtk:builder-add-from-string}"
  (glib:with-g-error (err)
    (%builder-add-from-resource builder path err)))

(export 'builder-add-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_from_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_from_string" %builder-add-from-string) :uint
  (builder (g:object builder))
  (string :string)
  (length :int)
  (err :pointer))

(defun builder-add-from-string (builder string)
 #+liber-documentation
 "@version{2023-3-2}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[string]{a string to parse}
  @return{The unsigned integer with a positive value on success, 0 if an error
    occurred.}
  @begin{short}
    Parses a string containing a @class{gtk:builder} UI definition and merges
    it with the current contents of the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-from-file}
  @see-function{gtk:builder-add-from-resource}"
  (glib:with-g-error (err)
    (%builder-add-from-string builder string -1 err)))

(export 'builder-add-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_objects_from_file"
               %builder-add-objects-from-file) :uint
  (builder (g:object builder))
  (filename :string)
  (object-ids :pointer)
  (err :pointer))

(defun builder-add-objects-from-file (builder path &rest ids)
 #+liber-documentation
 "@version{2024-3-20}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a path or namestring with the name of the file to parse}
  @argument[ids]{strings with the object IDs to build}
  @return{The unsigned integer with a positive value on success, 0 if an error
    occurred.}
  @begin{short}
    Parses a file containing a @class{gtk:builder} UI definition building only
    the requested objects and merges them with the current contents of
    @arg{builder}.
  @end{short}
  Upon errors 0 will be returned.
  @begin{notes}
    If you are adding an object that depends on an object that is not its
    child, for instance a @class{gtk:tree-view} widget that depends on its
    @class{gtk:tree-model} implementation, you have to explicitely list all of
    them in @arg{ids}.
  @end{notes}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-from-file}
  @see-function{gtk:builder-add-objects-from-string}
  @see-function{gtk:builder-add-objects-from-resource}"
  (let ((ids-ptr (cffi:foreign-alloc :pointer :count (1+ (length ids)))))
    (iter (for i from 0)
          (for object-id in ids)
          (setf (cffi:mem-aref ids-ptr :pointer i)
                (cffi:foreign-string-alloc object-id)))
    (setf (cffi:mem-aref ids-ptr :pointer (length ids)) (cffi:null-pointer))
    (unwind-protect
      (glib:with-g-error (err)
        (%builder-add-objects-from-file builder (namestring path) ids-ptr err))
      (progn
        (iter (for i from 0)
              (repeat (1- (length ids)))
              (cffi:foreign-string-free (cffi:mem-aref ids-ptr :pointer i)))
        (cffi:foreign-free ids-ptr)))))

(export 'builder-add-objects-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_objects_from_string"
               %builder-add-objects-from-string) :uint
  (builder (g:object builder))
  (string :string)
  (length :int)
  (ids :pointer)
  (err :pointer))

(defun builder-add-objects-from-string (builder string &rest ids)
 #+liber-documentation
 "@version{2024-3-20}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[string]{a string to parse}
  @argument[ids]{strings with the object IDs to build}
  @return{The unsigned integer with a positive value on success, 0 if an error
    occurred.}
  @begin{short}
    Parses a string containing a @class{gtk:builder} UI definition building only
    the requested objects and merges them with the current contents of builder.
  @end{short}
  @begin{notes}
    If you are adding an object that depends on an object that is not its child,
    for instance a @class{gtk:tree-view} widget that depends on its
    @class{gtk:tree-model} implementation, you have to explicitely list all of
    them in @arg{ids}.
  @end{notes}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-from-string}
  @see-function{gtk:builder-add-objects-from-file}
  @see-function{gtk:builder-add-objects-from-resource}"
  (let ((ids-ptr (cffi:foreign-alloc :pointer :count (1+ (length ids)))))
    (iter (for i from 0)
          (for object-id in ids)
          (setf (cffi:mem-aref ids-ptr :pointer i)
                (cffi:foreign-string-alloc object-id)))
    (setf (cffi:mem-aref ids-ptr :pointer (length ids)) (cffi:null-pointer))
    (unwind-protect
      (glib:with-g-error (err)
        (%builder-add-objects-from-string builder string -1 ids-ptr err))
      (progn
        (iter (for i from 0)
              (repeat (1- (length ids)))
              (cffi:foreign-string-free (cffi:mem-aref ids-ptr :pointer i)))
        (cffi:foreign-free ids-ptr)))))

(export 'builder-add-objects-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_add_objects_from_resource ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_add_objects_from_resource"
               %builder-add-objects-from-resource) :uint
  (builder (g:object builder))
  (path :string)
  (ids :pointer)
  (err :pointer))

(defun builder-add-objects-from-resource (builder path &rest ids)
 #+liber-documentation
 "@version{#2024-3-20}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[path]{a string with the path of the resource file to parse}
  @argument[ids]{strings with the object IDs to build}
  @return{The unsigned integer with a positive value on success, 0 if an error
    occurred.}
  @begin{short}
    Parses a resource file containing a @class{gtk:builder} UI definition
    building only the requested objects and merges them with the current
    contents of builder.
  @end{short}
  @begin{notes}
    If you are adding an object that depends on an object that is not its
    child, for instance a @class{gtk:tree-view} widget that depends on its
    @class{gtk:tree-model} implementation, you have to explicitely list all of
    them in @arg{ids}.
  @end{notes}
  @see-class{gtk:builder}
  @see-function{gtk:builder-add-from-resource}
  @see-function{gtk:builder-add-objects-from-file}
  @see-function{gtk:builder-add-objects-from-string}"
  (let ((ids-ptr (cffi:foreign-alloc :pointer :count (1+ (length ids)))))
    (iter (for i from 0)
          (for object-id in ids)
          (setf (cffi:mem-aref ids-ptr :pointer i)
                (cffi:foreign-string-alloc object-id)))
    (unwind-protect
      (glib:with-g-error (err)
        (%builder-add-objects-from-resource builder path ids-ptr err))
      (progn
        (iter (for i from 0)
              (repeat (1- (length ids)))
              (cffi:foreign-string-free (cffi:mem-aref ids-ptr :pointer i)))
        (cffi:foreign-free ids-ptr)))))

(export 'builder-add-objects-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_extend_with_template ()
;;;
;;; guint
;;; gtk_builder_extend_with_template (GtkBuilder *builder,
;;;                                   GtkWidget *widget,
;;;                                   GType template_type,
;;;                                   const gchar *buffer,
;;;                                   gsize length,
;;;                                   GError **error)
;;;
;;; Main private entry point for building composite container components from
;;; template XML.
;;;
;;; This is exported purely to let gtk:builder-tool validate templates,
;;; applications have no need to call this function.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; widget :
;;;     the widget that is being extended
;;;
;;; template_type :
;;; the type that the template is for
;;;
;;; buffer :
;;;     the string to parse
;;;
;;; length :
;;;     the length of buffer (may be -1 if buffer is nul-terminated)
;;;
;;; error :
;;;     return location for an error, or NULL.
;;;
;;; Returns :
;;;     A positive value on success, 0 if an error occurred
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_object ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_get_object" builder-object) g:object
 #+liber-documentation
 "@version{2023-3-2}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string with the name of the object to get}
  @return{The @class{g:object} instance named @arg{name} or @code{nil} if it
    could not be found in the object tree.}
  @begin{short}
    Gets the object named @arg{name} from the @class{gtk:builder} UI definition.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:object}
  @see-function{gtk:builder-objects}"
  (builder (g:object builder))
  (name :string))

(export 'builder-object)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_objects ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_get_objects" builder-objects) (g:slist-t g:object)
 #+liber-documentation
 "@version{2023-3-2}
  @argument[builder]{a @class{gtk:builder} object}
  @begin{return}
    A list containing all the @class{g:object} instances constructed by the
    @class{gtk:builder} object.
  @end{return}
  @begin{short}
    Gets all objects that have been constructed by the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:object}
  @see-function{gtk:builder-object}"
  (builder (g:object builder)))

(export 'builder-objects)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_expose_object ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_expose_object" builder-expose-object) :void
 #+liber-documentation
 "@version{#2023-3-2}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string with the name of the object exposed to the builder}
  @argument[object]{a @class{g:object} instance to expose}
  @begin{short}
    Adds an object to the builder object pool so it can be referenced just like
    any other object built by the builder.
  @end{short}
  @see-class{gtk:builder}
  @see-class{g:object}"
  (builder (g:object builder))
  (name :string)
  (object g:object))

(export 'builder-expose-object)

;;; ----------------------------------------------------------------------------
;;; GtkBuilderConnectFunc ()                                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcallback builder-connect-func :void
    ((builder (g:object builder))
     (object g:object)
     (signal (:string :free-from-foreign nil))
     (handler (:string :free-from-foreign nil))
     (connect g:object)
     (flags g:connect-flags)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func builder object signal handler connect flags)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'builder-connect-func)
      "Callback"
      (liber:symbol-documentation 'builder-connect-func)
 "@version{#2024-3-20}
  @syntax{lambda (builder object signal handler connect flags)}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[object]{a @class{g:object} instance to connect a signal to}
  @argument[signal]{a string with the name of the signal}
  @argument[handler]{a string with the name of the handler}
  @argument[connect]{a @class{g:object} instance, if non-@code{nil}, use the
    @fun{g:signal-connect-object} function}
  @argument[flags]{a @symbol{g:connect-flags} value to use}
  @begin{short}
    This is the signature of a callback function used to connect signals.
  @end{short}
  It is used by the @fun{gtk:builder-connect-signals} and
  @fun{gtk:builder-connect-signals-full} functions. It is mainly intended for
  interpreted language bindings, but could be useful where the programmer wants
  more control over the signal connection process. Note that this function can
  only be called once, subsequent calls will do nothing.
  @see-class{gtk:builder}
  @see-function{gtk:builder-connect-signals}
  @see-function{gtk:builder-connect-signals-full}")

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals_full ()                     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_connect_signals_full" %builder-connect-signals-full)
    :void
  (builder (g:object builder))
  (func :pointer)
  (data :pointer))

(defun builder-connect-signals-full (builder func)
 #+liber-documentation
 "@version{#2023-3-2}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[func]{a @symbol{gtk:builder-connect-func} callback function used to
    connect the signals}
  @begin{short}
    This function can be thought of the interpreted language binding version of
    the @fun{gtk:builder-connect-signals} function.
  @end{short}
  @see-class{gtk:builder}
  @see-symbol{gtk:builder-connect-func}
  @see-function{gtk:builder-connect-signals}"
  (glib:with-stable-pointer (ptr func)
    (%builder-connect-signals-full builder
                                   (cffi:callback builder-connect-func)
                                   ptr)))

;;; ----------------------------------------------------------------------------
;;; gtk_builder_connect_signals ()
;;; ----------------------------------------------------------------------------

(defun builder-connect-signals (builder &rest handlers)
 #+liber-documentation
 "@version{2024-3-16}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[handlers]{pairs with the signal ID from the UI definition and the
    symbol for the corresponding signal handler}
  @begin{short}
    This function connects signal handlers to the signal IDs in the UI
    definition.
  @end{short}
  @begin{examples}
    Code fragment from the GTK examples.
    @begin{pre}
(let ((builder (make-instance 'gtk:builder)))
  (gtk:builder-add-from-file builder (sys-path \"resource/dialog.ui\"))
  (gtk:builder-connect-signals builder
                               \"ok-clicked\" 'ok-button-clicked
                               \"cancel-clicked\" 'cancel-button-clicked)
  ... )
    @end{pre}
  @end{examples}
  @see-class{gtk:builder}"
  (flet ((connect-func (builder
                        object
                        signalname
                        handlername
                        connect
                        flags)
           (declare (ignore builder connect))
           (let ((handler (member handlername handlers :test 'string=)))
             (when handler
               (g:signal-connect object
                                 signalname
                                 (second handler)
                                 :after (member :after flags))))))
    (builder-connect-signals-full builder #'connect-func)))

(export 'builder-connect-signals)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_application ()
;;; gtk_builder_set_application ()
;;; ----------------------------------------------------------------------------

(defun (setf builder-application) (application builder)
  (cffi:foreign-funcall "gtk_builder_set_application"
                        (g:object builder) builder
                        (g:object application) application
                        :void)
  application)

(cffi:defcfun ("gtk_builder_get_application" builder-application)
    (g:object application)
 #+liber-documentation
 "@version{#2024-3-16}
  @syntax{(gtk:builder-application builder) => application}
  @syntax{(setf (gtk:builder-application builder) application)}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[application]{a @class{gtk:application} instance}
  @begin{short}
    The @fun{gtk:builder-application} function gets the application associated
    with the builder.
  @end{short}
  The @setf{gtk:builder-application} function sets the application.

  The application is used for creating action proxies as requested from XML
  that the builder is loading. By default, the builder uses the default
  application: the one from the @fun{g:application-default} function. If you
  want to use another application for constructing proxies, use the
  @setf{gtk:builder-application} function.

  You only need this function if there is more than one
  @class{g:application} instance in your process. The @arg{application}
  argument cannot be @code{nil}.
  @see-class{gtk:builder}
  @see-class{gtk:application}
  @see-class{g:application}
  @see-function{g:application-default}"
  (builder (g:object builder)))

(export 'builder-application)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_get_type_from_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_builder_get_type_from_name" builder-type-from-name) g:type-t
 #+liber-documentation
 "@version{2024-3-16}
  @argument[builder]{a @class{gtk:builder} object}
  @argument[name]{a string with the type name to lookup}
  @return{The @class{g:type-t} type ID found for @arg{name}.}
  @begin{short}
    Looks up a type by name, using the virtual function that the
    @class{gtk:builder} class has for that purpose.
  @end{short}
  This is mainly used when implementing the @class{gtk:buildable} interface on
  a type.
  @see-class{gtk:builder}
  @see-class{gtk:buildable}
  @see-class{g:type-t}"
  (builder (g:object builder))
  (name :string))

(export 'builder-type-from-name)

;;; ----------------------------------------------------------------------------
;;; gtk_builder_value_from_string ()
;;;
;;; gboolean
;;; gtk_builder_value_from_string (GtkBuilder *builder,
;;;                                GParamSpec *pspec,
;;;                                const gchar *string,
;;;                                GValue *value,
;;;                                GError **error)
;;;
;;; This function demarshals a value from a string. This function calls
;;; g_value_init() on the value argument, so it need not be initialised
;;; beforehand.
;;;
;;; This function can handle char, uchar, boolean, int, uint, long, ulong, enum,
;;; flags, float, double, string, GdkColor, GdkRGBA and GtkAdjustment type
;;; values. Support for GtkWidget type values is still to come.
;;;
;;; Upon errors FALSE will be returned and error will be assigned a GError from
;;; the GTK_BUILDER_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;;
;;; string :
;;;     the string representation of the value
;;;
;;; value :
;;;     the GValue to store the result in
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_builder_value_from_string_type ()
;;;
;;; gboolean
;;; gtk_builder_value_from_string_type (GtkBuilder *builder,
;;;                                     GType type,
;;;                                     const gchar *string,
;;;                                     GValue *value,
;;;                                     GError **error)
;;;
;;; Like gtk_builder_value_from_string(), this function demarshals a value from
;;; a string, but takes a GType instead of GParamSpec. This function calls
;;; g_value_init() on the value argument, so it need not be initialised
;;; beforehand.
;;;
;;; Upon errors FALSE will be returned and error will be assigned a GError from
;;; the GTK_BUILDER_ERROR domain.
;;;
;;; builder :
;;;     a GtkBuilder
;;;
;;; type :
;;;     the GType of the value
;;;
;;; string :
;;;     the string representation of the value
;;;
;;; value :
;;;     the GValue to store the result in
;;;
;;; error :
;;;     return location for an error, or NULL
;;;
;;; Returns :
;;;     TRUE on success
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.builder.lisp ------------------------------------------
