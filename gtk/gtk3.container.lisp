;;; ----------------------------------------------------------------------------
;;; gtk3.container.lisp
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
;;; GtkContainer
;;;
;;;     Base class for widgets which contain other widgets
;;;
;;; Types and Values
;;;
;;;     GtkResizeMode
;;;     GtkContainer
;;;
;;; Accessors
;;;
;;;     gtk_container_get_border_width
;;;     gtk_container_set_border_width
;;;     gtk_container_get_resize_mode
;;;     gtk_container_set_resize_mode
;;;
;;; Functions
;;;
;;;     gtk_container_add
;;;     gtk_container_remove
;;;     gtk_container_add_with_properties
;;;     gtk_container_check_resize
;;;     gtk_container_foreach
;;;     gtk_container_get_children
;;;     gtk_container_get_path_for_child
;;;     gtk_container_set_reallocate_redraws               deprecated
;;;     gtk_container_get_focus_child
;;;     gtk_container_set_focus_child
;;;     gtk_container_get_focus_vadjustment
;;;     gtk_container_set_focus_vadjustment
;;;     gtk_container_get_focus_hadjustment
;;;     gtk_container_set_focus_hadjustment
;;;     gtk_container_resize_children                      deprecated
;;;     gtk_container_child_type
;;;     gtk_container_child_get
;;;     gtk_container_child_set
;;;     gtk_container_child_get_property
;;;     gtk_container_child_set_property
;;;     gtk_container_child_get_valist
;;;     gtk_container_child_set_valist
;;;     gtk_container_child_notify
;;;     gtk_container_child_notify_by_pspec
;;;     gtk_container_forall
;;;     gtk_container_propagate_draw
;;;     gtk_container_get_focus_chain                      deprecated
;;;     gtk_container_set_focus_chain                      deprecated
;;;     gtk_container_unset_focus_chain                    deprecated
;;;     gtk_container_class_find_child_property
;;;     gtk_container_class_install_child_property
;;;     gtk_container_class_install_child_properties
;;;     gtk_container_class_list_child_properties
;;;     gtk_container_class_handle_border_width
;;;
;;; Properties
;;;
;;;     border-width
;;;     child
;;;     resize-mode
;;;
;;; Signals
;;;
;;;     add
;;;     check-resize
;;;     remove
;;;     set-focus-child
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 ├── GtkBox
;;;                 ├── GtkFixed
;;;                 ├── GtkFlowBox
;;;                 ├── GtkGrid
;;;                 ├── GtkHeaderBar
;;;                 ├── GtkPaned
;;;                 ├── GtkIconView
;;;                 ├── GtkLayout
;;;                 ├── GtkListBox
;;;                 ├── GtkMenuShell
;;;                 ├── GtkNotebook
;;;                 ├── GtkSocket
;;;                 ├── GtkStack
;;;                 ├── GtkTable
;;;                 ├── GtkTextView
;;;                 ├── GtkToolbar
;;;                 ├── GtkToolItemGroup
;;;                 ├── GtkToolPalette
;;;                 ╰── GtkTreeView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkContainer implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------

;; Macro for defining accessors for the child properties of a container.

(defmacro define-child-property (name                       ; fixed-child-x
                                 gname                      ; "x"
                                 gtype                      ; "gint"
                                 readable writable export)
  `(progn
     ,@(when readable
         (list `(defun ,name (container child)
                  (container-child-property container
                                            child
                                            ,gname
                                            ,gtype))))
     ,@(when writable
         (list `(defun (setf ,name) (value container child)
                  (setf (container-child-property container
                                                  child
                                                  ,gname
                                                  ,gtype)
                        value)
                  value)))
     ,@(when export
         (list `(export ',name)))))

;;; ----------------------------------------------------------------------------
;;; GtkResizeMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkResizeMode" resize-mode
  (:export t
   :type-initializer "gtk_resize_mode_get_type")
  (:parent 0)
  (:queue 1)
  (:immediate 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'resize-mode)
      "GEnum"
      (liber:symbol-documentation 'resize-mode)
 "@version{2024-03-21}
  @begin{declaration}
(gobject:define-genum \"GtkResizeMode\" resize-mode
  (:export t
   :type-initializer \"gtk_resize_mode_get_type\")
  (:parent 0)
  (:queue 1)
  (:immediate 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:parent]{Pass resize request to the parent.}
      @entry[:queue]{Queue resizes on this widget.}
      @entry[:immediate]{Resize immediately.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration representing the values of the
    @slot[gtk:container]{resize-mode} property.
  @end{short}
  @begin[Warning]{dictionary}
    Resize modes are deprecated since version 3.12 and should not be used in
    newly written code. They are not necessary anymore since frame clocks and
    might introduce obscure bugs if used.
  @end{dictionary}
  @see-class{gtk:container}
  @see-function{gtk:container-resize-mode}")

;;; ----------------------------------------------------------------------------
;;; GtkContainer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkContainer" container
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_container_get_type")
  ((border-width
    container-border-width
    "border-width" "guint" t t)
   (child
    container-child
    "child" "GtkWidget" nil t)
   (resize-mode
    container-resize-mode
    "resize-mode" "GtkResizeMode" t t)))

#+liber-documentation
(setf (documentation 'container 'type)
 "@version{2023-03-03}
  @begin{short}
    Base class for widgets which contain other widgets.
  @end{short}
  A GTK user interface is constructed by nesting widgets inside widgets.
  Container widgets are the inner nodes in the resulting tree of widgets: they
  contain other widgets. So, for example, you might have a @class{gtk:window}
  widget containing a @class{gtk:frame} widget containing a @class{gtk:label}
  widget. If you wanted an image instead of a textual label inside the frame,
  you might replace the @class{gtk:label} widget with a @class{gtk:image}
  widget.

  There are two major kinds of container widgets in GTK. Both are subclasses
  of the abstract @class{gtk:container} base class. The first type of container
  widget has a single child widget and derives from the @class{gtk:bin} class.
  These containers are decorators, which add some kind of functionality to the
  child. For example, a @class{gtk:button} widget makes its child into a
  clickable button. A @class{gtk:frame} widget draws a frame around its child
  and a @class{gtk:window} widget places its child widget inside a toplevel
  window.

  The second type of container can have more than one child. Its purpose is to
  manage layout. This means that these containers assign sizes and positions
  to their children. For example, a @class{gtk:grid} widget arranges the widgets
  it contains in a two-dimensional grid.

  @subheading{Height for width geometry management}
  GTK uses a height-for-width and width-for-height geometry management system.
  Height-for-width means that a widget can change how much vertical space it
  needs, depending on the amount of horizontal space that it is given and
  similar for width-for-height.

  There are some things to keep in mind when implementing container widgets
  that make use of the height for width geometry management system. First,
  it is important to note that a container must prioritize one of its
  dimensions, that is to say that a widget or container can only have a
  @sym{gtk:size-request-mode} mode that is
  @val[gtk:size-request-mode]{:height-for-width} or
  @val[gtk:size-request-mode]{:width-for-height}. However, every widget and
  container must be able to respond to the APIs for both dimensions, that is,
  even if a widget has a request mode that is height-for-width, it is possible
  that its parent will request its sizes using the width-for-height APIs.

  @subheading{Child properties}
  The @class{gtk:container} widget introduces child properties. These are object
  properties that are not specific to either the container or the contained
  widget, but rather to their relation. Typical examples of child properties
  are the position or pack-type of a widget which is contained in a
  @class{gtk:box} widget. Use the @fun{gtk:container-class-find-child-property}
  or @fun{gtk:container-class-list-child-properties} functions to get
  information about existing child properties.

  To obtain or to set the value of a child property, use the
  @fun{gtk:container-child-property}, @fun{gtk:container-child-get},
  or @fun{gtk:container-child-set} functions. To emit notification about child
  property changes, use the @fun{gtk:widget-child-notify} function.
  @begin[GtkContainer as GtkBuildable]{dictionary}
    The @class{gtk:container} implementation of the @class{gtk:buildable}
    interface supports a @code{<packing>} element for children, which can
    contain multiple @code{<property>} elements that specify child properties
    for the child. Child properties can also be marked as translatable using
    the same \"translatable\", \"comments\" and \"context\" attributes that are
    used for regular properties.

    Containers can have a @code{<focus-chain>} element containing multiple
    @code{<widget>} elements, one for each child that should be added to the
    focus chain. The \"name\" attribute gives the ID of the widget.

    @b{Example:} Child properties in UI definitions
    @begin{pre}
<object class=\"GtkBox\">
  <child>
    <object class=\"GtkEntry\" id=\"entry1\"/>
    <packing>
      <property name=\"pack-type\">start</property>
    </packing>
  </child>
  <child>
    <object class=\"GtkEntry\" id=\"entry2\"/>
  </child>
  <focus-chain>
    <widget name=\"entry1\"/>
    <widget name=\"entry2\"/>
  </focus-chain>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[container::add]{signal}
      @begin{pre}
lambda (container widget)    :run-first
      @end{pre}
    @end{signal}
    @begin[container::check-resize]{signal}
      @begin{pre}
lambda (container)    :run-last
      @end{pre}
    @end{signal}
    @begin[container::remove]{signal}
      @begin{pre}
lambda (container widget)    :run-first
      @end{pre}
    @end{signal}
    @begin[container::set-focus-child]{signal}
      @begin{pre}
lambda (container widget)    :run-first
      @end{pre}
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:container-border-width}
  @see-slot{gtk:container-child}
  @see-slot{gtk:container-resize-mode}
  @see-class{gtk:bin}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:container-border-width ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "border-width" 'container) t)
 "The @code{border-width} property of type @code{:uint} (Read / Write) @br{}
  The width of the empty border outside the containers children. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'container-border-width)
      "Accessor"
      (documentation 'container-border-width 'function)
 "@version{2025-07-06}
  @syntax{(gtk:container-border-width object) => width}
  @syntax{(setf gtk:container-border-width object) width)}
  @argument[object]{a @class{gtk:container} widget}
  @argument[width]{an unsigned integer for the border width}
  @begin{short}
    Accessor of the @slot[gtk:container]{border-width} slot of the
    @class{gtk:container} class.
  @end{short}
  The @fun{gtk:container-border-width} function retrieves the border width of
  the container. The @setf{gtk:container-border-width} function sets the border
  width.

  The border width of a container is the amount of space to leave around the
  outside of the container. Valid values are in the range [0, 65535] pixels.
  The only exception to this is the @class{gtk:window} widget. Because toplevel
  windows cannot leave space outside, they leave the space inside. The border
  is added on all sides of the container. To add space to only one side, one
  approach is to create a @class{gtk:alignment} widget, call the
  @fun{gtk:widget-size-request} function to give it a size, and place it on the
  side of the container as a spacer.
  @see-class{gtk:container}
  @see-class{gtk:window}
  @see-class{gtk:alignment}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:container-child ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'container) t)
 "The @code{child} property of type @class{gtk:widget} (Write) @br{}
  Can be used to add a new child to the container.")

#+liber-documentation
(setf (liber:alias-for-function 'container-child)
      "Accessor"
      (documentation 'container-child 'function)
 "@version{2023-03-03}
  @syntax{(gtk:container-child object) => child}
  @syntax{(setf gtk:container-child object) child)}
  @argument[object]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:container]{child} slot of the
    @class{gtk:container} class.
  @end{short}
  Can be used to add a new child to the container.
  @see-class{gtk:container}
  @see-class{gtk:widget}")

;;; --- gtk:container-resize-mode ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resize-mode" 'container) t)
 "The @code{resize-mode} property of type @sym{gtk:resize-mode}
  (Read / Write) @br{}
  Specify how resize events are handled. @br{}
  @em{Warning:} Resize modes are deprecated since version 3.12 and should not
  be used in newly written code. They are not necessary anymore since frame
  clocks and might introduce obscure bugs if used. @br{}
  Default value: @val[gtk:resize-mode]{:parent}")

#+liber-documentation
(setf (liber:alias-for-function 'container-resize-mode)
      "Accessor"
      (documentation 'container-resize-mode 'function)
 "@version{2025-07-11}
  @syntax{(gtk:container-resize-mode object) => mode}
  @syntax{(setf gtk:container-resize-mode object) mode)}
  @argument[object]{a @class{gtk:container} widget}
  @argument[mode]{a value of the @sym{gtk:resize-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:container]{resize-mode} slot of the
    @class{gtk:container} class.
  @end{short}
  The @fun{gtk:container-resize-mode} function returns the current resize mode
  of the container. The @setf{gtk:container-resize-mode} function sets the
  resize mode.

  The resize mode of a container determines whether a resize request will be
  passed to the parent of the container, queued for later execution or executed
  immediately.
  @begin[Warning]{dictionary}
    The @fun{gtk:container-resize-mode} function has been deprecated since
    version 3.12 and should not be used in newly written code. Resize modes are
    deprecated. They are not necessary anymore since frame clocks and might
    introduce obscure bugs if used.
  @end{dictionary}
  @see-class{gtk:container}
  @see-symbol{gtk:resize-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_container_add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_add" container-add) :void
 #+liber-documentation
 "@version{2023-06-17}
  @argument[container]{a @class{gtk:container} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be placed inside
    @arg{container}}
  @begin{short}
    Adds a child widget to the container.
  @end{short}
  Typically used for simple containers such as @class{gtk:window},
  @class{gtk:frame}, or @class{gtk:button} widgets.

  For more complicated layout containers such as @class{gtk:box} or
  @class{gtk:grid} widgets, this function will pick default packing parameters
  that may not be correct. So consider functions such as the
  @fun{gtk:box-pack-start} and @fun{gtk:grid-attach} functions as an
  alternative to the @fun{gtk:container-add} function in those cases.

  A widget may be added to only one container at a time. You cannot place the
  same widget inside two different containers.
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-class{gtk:frame}
  @see-class{gtk:button}
  @see-function{gtk:box-pack-start}
  @see-function{gtk:grid-attach}"
  (container (g:object container))
  (widget (g:object widget)))

(export 'container-add)

;;; ----------------------------------------------------------------------------
;;; gtk_container_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_remove" container-remove) :void
 #+liber-documentation
 "@version{2023-07-17}
  @argument[container]{a @class{gtk:container} widget}
  @argument[widget]{a current @class{gtk:widget} child widget of
    @arg{container}}
  @begin{short}
    Removes a child widget from the container.
  @end{short}
  The child widget must be inside the container.
  @see-class{gtk:container}
  @see-class{gtk:widget}"
  (container (g:object container))
  (widget (g:object widget)))

(export 'container-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_container_add_with_properties
;;; ----------------------------------------------------------------------------

(defun container-add-with-properties (container widget &rest args)
 "@version{2023-06-17}
  @argument[container]{a @class{gtk:container} widget}
  @argument[widget]{a @class{gtk:widget} child widget}
  @argument[args]{a list of property names and values}
  @begin{short}
    Adds @arg{widget} to the container, setting child properties at the same
    time.
  @end{short}
  See the @fun{gtk:container-add} and @fun{gtk:container-child-set} functions
  for more details.
  @see-class{gtk:container}
  @see-function{gtk:container-add}
  @see-function{gtk:container-add-with-properties}"
  (container-add container widget)
  (apply #'container-child-set container widget args))

(export 'container-add-with-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_container_check_resize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_check_resize" container-check-resize) :void
 #+liber-documentation
 "@version{#2025-07-16}
  @argument[container]{a @class{gtk:container} widget}
  @begin{short}
    Emits the @sig[gtk:container]{check-resize} signal on the container.
  @end{short}
  @see-class{gtk:container}"
  (container (g:object container)))

(export 'container-check-resize)

;;; ----------------------------------------------------------------------------
;;; GtkCallback
;;; ----------------------------------------------------------------------------

(cffi:defcallback gtk-callback :void
    ((widget (g:object widget))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func widget)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'gtk-callback)
      "Callback"
      (liber:symbol-documentation 'gtk-callback)
 "@version{2025-07-11}
  @syntax{lambda (widget)}
  @argument[widget]{a @class{gtk:widget} widget to operate on}
  @begin{short}
    The type of the callback functions used, for example, for iterating over the
    children of a container, see the @fun{gtk:container-foreach} function.
  @end{short}
  @see-class{gtk:container}
  @see-function{gtk:container-foreach}
  @see-function{gtk:container-forall}")

(export 'gtk-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_container_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_foreach" %container-foreach) :void
  (container (g:object container))
  (func :pointer)
  (data :pointer))

(defun container-foreach (container func)
 #+liber-documentation
 "@version{2025-07-11}
  @argument[container]{a @class{gtk:container} widget}
  @argument[func]{a @sym{gtk:gtk-callback} callback function}
  @begin{short}
    Invokes a function on each non-internal child of the container.
  @end{short}
  See the @fun{gtk:container-forall} function for details on what constitutes
  an \"internal\" child. Most applications should use the
  @fun{gtk:container-foreach} function, rather than the
  @fun{gtk:container-forall} function.
  @see-class{gtk:container}
  @see-symbol{gtk:gtk-callback}
  @see-function{gtk:container-forall}"
  (glib:with-stable-pointer (ptr func)
    (%container-foreach container
                        (cffi:callback gtk-callback)
                        ptr)))

(export 'container-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_get_children" container-children)
    (g:list-t g:object)
 #+liber-documentation
 "@version{2023-06-17}
  @argument[container]{a @class{gtk:container} widget}
  @return{The list of the containers non-internal children.}
  @begin{short}
    Returns a list with the non-internal children of the container.
  @end{short}
  See the @fun{gtk:container-forall} function for details on what constitutes
  an \"internal\" child.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq box (make-instance 'gtk:box :orientation :vertical))
=> #<GTK:BOX {1001E2A183@}>
(gtk:container-add box (make-instance 'gtk:button))
(gtk:container-add box (make-instance 'gtk:label))
(gtk:container-children box)
=> (#<GTK:BUTTON {1001E2B0A3@}> #<GTK:LABEL {1001E2BFD3@}>)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:container}
  @see-function{gtk:container-forall}"
  (container (g:object container)))

(export 'container-children)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_path_for_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_get_path_for_child" container-path-for-child)
    (g:boxed widget-path)
 #+liber-documentation
 "@version{2023-03-03}
  @argument[container]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} child widget of @arg{container}}
  @return{The newly created @class{gtk:widget-path} instance.}
  @begin{short}
    Returns a newly created widget path representing all the widget hierarchy
    from the toplevel down to and including child.
  @end{short}
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-class{gtk:widget-path}"
  (container (g:object container))
  (child (g:object widget)))

(export 'container-path-for-child)

;;; ----------------------------------------------------------------------------
;;; gtk_container_set_reallocate_redraws                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_set_reallocate_redraws"
                container-set-reallocate-redraws) :void
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[container]{a @class{gtk:container} widget}
  @argument[redraw]{a boolean for the value for the @code{reallocate-redraws}
    flag of the container}
  @begin{short}
    Sets the @code{reallocate_redraws} flag of the container to the given value.
  @end{short}
  Containers requesting reallocation redraws get automatically redrawn if any
  of their children changed allocation.
  @begin[Warning]{dictionary}
    The @fun{gtk:container-reallocate-redraws} function has been deprecated
    since version 3.14 and should not be used in newly written code. Call
    the @fun{gtk:widget-queue-draw} function.
  @end{dictionary}
  @see-class{gtk:container}
  @see-function{gtk:widget-queue-draw}"
  (container (g:object container))
  (redraw :boolean))

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_child
;;; gtk_container_set_focus_child
;;; ----------------------------------------------------------------------------

(defun (setf container-focus-child) (child container)
  (cffi:foreign-funcall "gtk_container_set_focus_child"
                        (g:object container) container
                        (g:object widget) child
                        :void)
  child)

(cffi:defcfun ("gtk_container_get_focus_child" container-focus-child)
    (g:object widget)
 #+liber-documentation
 "@version{2025-07-16}
  @syntax{(gtk:container-focus-child container) => child}
  @syntax{(setf (gtk:container-focus-child container) child)}
  @argument[container]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the current focused child widget in the container.
  @end{short}
  The @fun{gtk:container-focus-child} function returns the current focus child
  widget which will receive the focus inside the container when the container
  is focussed. This is not the currently focused widget. That can be obtained
  by calling the @fun{gtk:window-focus} function. The
  @setf{gtk:container-focus-child} function sets, or unsets, if the @arg{child}
  argument is @code{nil}, the focused child of the container.

  This function emits the @sig[gtk:container]{set-focus-child} signal of the
  container. Implementations of the @class{gtk:container} class can override
  the default behaviour by overriding the handler of this signal.

  This function is mostly meant to be used by widgets. Applications can use the
  @fun{gtk:widget-grab-focus} function to manualy set the focus to a specific
  widget.
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-function{gtk:window-focus}
  @see-function{gtk:widget-grab-focus}"
  (container (g:object container)))

(export 'container-focus-child)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_vadjustment
;;; gtk_container_set_focus_vadjustment
;;; ----------------------------------------------------------------------------

(defun (setf container-focus-vadjustment) (adjustment container)
  (cffi:foreign-funcall "gtk_container_set_focus_vadjustment"
                        (g:object container) container
                        (g:object adjustment) adjustment
                        :void)
  adjustment)

(cffi:defcfun ("gtk_container_get_focus_vadjustment"
                container-focus-vadjustment)
    (g:object adjustment)
 #+liber-documentation
 "@version{#2025-07-17}
  @syntax{(gtk:container-focus-vadjustment container) => adjustment}
  @syntax{(setf (gtk:container-focus-vadjustment container) adjustment)}
  @argument[container]{a @class{gtk:container} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object that should be
    adjusted when the focus is moved among the descendents of the container}
  @begin{short}
    Accessor of the vertical focus adjustment of the container.
  @end{short}
  The @fun{gtk:container-focus-vadjustment} function retrieves the vertical
  focus adjustment for the container. The
  @setf{gtk:container-focus-vadjustment} function sets the vertical adjustment.

  Hooks up an adjustment to focus handling in a container, so when a child of
  the container is focused, the adjustment is scrolled to show that widget. The
  adjustments have to be in pixel units and in the same coordinate system as
  the allocation for immediate children of the container.
  @see-class{gtk:container}
  @see-class{gtk:adjustment}
  @see-function{gtk:container-focus-hadjustment}"
  (container (g:object container)))

(export 'container-focus-vadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_hadjustment
;;; gtk_container_set_focus_hadjustment
;;; ----------------------------------------------------------------------------

(defun (setf container-focus-hadjustment) (adjustment container)
  (cffi:foreign-funcall "gtk_container_set_focus_hadjustment"
                        (g:object container) container
                        (g:object adjustment) adjustment
                        :void)
  adjustment)

(cffi:defcfun ("gtk_container_get_focus_hadjustment"
                container-focus-hadjustment)
    (g:object adjustment)
 #+liber-documentation
 "@version{#2025-07-17}
  @syntax{(gtk:container-focus-hadjustment container) => adjustment}
  @syntax{(setf (gtk:container-focus-hadjustment container) adjustment)}
  @argument[container]{a @class{gtk:container} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object that should be
    adjusted when the focus is moved among the descendents of the container}
  @begin{short}
    Accessor of the horizontal focus adjustment of the container.
  @end{short}
  The @fun{gtk:container-focus-hadjustment} function retrieves the horizontal
  focus adjustment for the container. The @setf{gtk:container-focus-hadjustment}
  function sets the horizontal adjustment.

  Hooks up an adjustment to focus handling in a container, so when a child of
  the container is focused, the adjustment is scrolled to show that widget. The
  adjustments have to be in pixel units and in the same coordinate system as
  the allocation for immediate children of the container.
  @see-class{gtk:container}
  @see-class{gtk:adjustment}
  @see-function{gtk:container-focus-vadjustment}"
  (container (g:object container)))

(export 'container-focus-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_container_resize_children                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_resize_children" container-resize-children) :void
 #+liber-documentation
 "@version{#2021-09-12}
  @short{undocumented}
  @begin[Warning]{dictionary}
    The @fun{gtk:container-resize-children} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:container}"
  (container (g:object container)))

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_child_type" container-child-type) g:type-t
 #+liber-documentation
 "@version{2025-07-11}
  @argument[container]{a @class{gtk:container} widget}
  @return{The @class{g:type-t} type ID.}
  @begin{short}
    Returns the type of the children supported by the container.
  @end{short}

  Note that this may return @var{g:+type-none+} to indicate that no more
  children can be added, for example, for a @class{gtk:paned} widget which
  already has two children.
  @see-class{gtk:container}
  @see-class{g:type-t}
  @see-variable{g:+type-none+}"
  (container (g:object container)))

(export 'container-child-type)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get
;;; ----------------------------------------------------------------------------

(defun container-child-get (container child &rest args)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[container]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} child widget which is a child of
    @arg{container}}
  @argument[args]{a list of strings for the child property names to get the
    values for}
  @return{The list with the values of the properties.}
  @begin{short}
    Gets the values of one or more child properties for a child widget of the
    container.
  @end{short}
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-function{gtk:container-child-set}
  @see-function{gtk:container-child-property}"
  (iter (for arg in args)
        (collect (container-child-property container child arg))))

(export 'container-child-get)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_set
;;; ----------------------------------------------------------------------------

(defun container-child-set (container child &rest args)
 #+liber-documentation
 "@version{2023-06-17}
  @argument[container]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} child widget which is a child of
    @arg{container}}
  @argument[args]{a list of property names and values}
  @begin{short}
    Sets one or more child properties for a child widget of the container.
  @end{short}
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-function{gtk:container-child-get}
  @see-function{gtk:container-child-property}"
  (iter (for (name value) on args by #'cddr)
        (setf (container-child-property container child name) value)))

(export 'container-child-set)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get_property
;;; gtk_container_child_set_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_child_set_property" %container-child-set-property)
    :void
  (container (g:object container))
  (child (g:object widget))
  (property :string)
  (gvalue (:pointer (:struct g:value))))

(defun (setf container-child-property) (value container child property
                                              &optional gtype)
  (let ((gtype (if gtype
                   (g:gtype gtype)
                   (g:param-spec-value-type
                     (container-class-find-child-property
                       (g:type-from-instance container) property)))))
    (cffi:with-foreign-object (gvalue '(:struct g:value))
      (gobject:set-gvalue gvalue value gtype)
      (%container-child-set-property container child property gvalue)
      (g:value-unset gvalue)
      (values value))))

(cffi:defcfun ("gtk_container_child_get_property"
                %container-child-property) :void
  (container (g:object container))
  (child (g:object widget))
  (property :string)
  (gvalue (:pointer (:struct g:value))))

(defun container-child-property (container child property &optional gtype)
 #+liber-documentation
 "@version{2025-07-17}
  @syntax{(gtk:container-child-property container child property) => value}
  @syntax{(setf (gtk:container-child-property container child property) value)}
  @argument[container]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} object that is a child of
    @arg{container}}
  @argument[property]{a string for the name of the property to get}
  @argument[gtype]{an optional @class{g:type-t} type ID for @arg{value}}
  @argument[value]{value for the property}
  @begin{short}
    Gets or sets the value of a child property for the child widget of the
    container.
  @end{short}
  If the @arg{gtype} type ID is not given, it is determined from the specified
  @arg{property} argument.
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-function{gtk:container-child-get}
  @see-function{gtk:container-child-set}"
  (let ((gtype (if gtype
                   (g:gtype gtype)
                   (g:param-spec-value-type
                     (container-class-find-child-property
                       (g:type-from-instance container)
                       property)))))
    (cffi:with-foreign-object (gvalue '(:struct g:value))
      (g:value-init gvalue gtype)
      (%container-child-property container child property gvalue)
      (prog1
        (g:value-get gvalue)
        (g:value-unset gvalue)))))

(export 'container-child-property)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_get_valist ()
;;;
;;; void
;;; gtk_container_child_get_valist (GtkContainer *container,
;;;                                 GtkWidget *child,
;;;                                 const gchar *first_property_name,
;;;                                 va_list var_args);
;;;
;;; Gets the values of one or more child properties for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; first_property_name :
;;;     the name of the first property to get
;;;
;;; var_args :
;;;     return location for the first property, followed optionally by more
;;;     name/return location pairs, followed by NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_set_valist ()
;;;
;;; void
;;; gtk_container_child_set_valist (GtkContainer *container,
;;;                                 GtkWidget *child,
;;;                                 const gchar *first_property_name,
;;;                                 va_list var_args);
;;;
;;; Sets one or more child properties for child and container.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a widget which is a child of container
;;;
;;; first_property_name :
;;;     the name of the first property to set
;;;
;;; var_args :
;;;     a NULL-terminated list of property names and values, starting with
;;;     first_prop_name
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_notify
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_child_notify" container-child-notify) :void
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[container]{a @class{gtk:container} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[property]{a string for the name of a child property installed on
    the class of @arg{container}}
  @begin{short}
    Emits a @sig[gtk:widget]{child-notify} signal for the @arg{property} child
    property on @arg{widget}.
  @end{short}
  This is an analogue of the @fun{g:object-notify} function for child
  properties. Also see the @fun{gtk:widget-child-notify} function.
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-function{g:object-notify}
  @see-function{gtk:widget-child-notify}"
  (container (g:object container))
  (child (g:object widget))
  (property :string))

(export 'container-child-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_container_child_notify_by_pspec ()
;;;
;;; void
;;; gtk_container_child_notify_by_pspec (GtkContainer *container,
;;;                                      GtkWidget *child,
;;;                                      GParamSpec *pspec);
;;;
;;; Emits a "child-notify" signal for the child property specified by pspec on
;;; the child.
;;;
;;; This is an analogue of g_object_notify_by_pspec() for child properties.
;;;
;;; container :
;;;     the GtkContainer
;;;
;;; child :
;;;     the child widget
;;;
;;; pspec :
;;;     the GParamSpec of a child property instealled on the class of container
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_forall
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_forall" %container-forall) :void
  (container (g:object container))
  (callback :pointer)
  (data :pointer))

(defun container-forall (container func)
 #+liber-documentation
 "@version{2025-07-11}
  @argument[container]{a @class{gtk:container} widget}
  @argument[func]{a @sym{gtk:gtk-callback} callback function which is passed
    as a callback}
  @begin{short}
    Invokes a function on each child of the container, including children that
    are considered \"internal\", implementation details of the container.
  @end{short}
  \"Internal\" children generally were not added by the user of the container,
  but were added by the container implementation itself. Most applications
  should use the @fun{gtk:container-foreach} function, rather than the
  @fun{gtk:container-forall} function.
  @see-class{gtk:container}
  @see-symbol{gtk:gtk-callback}
  @see-function{gtk:container-foreach}"
  (glib:with-stable-pointer (ptr func)
    (%container-forall container
                       (cffi:callback gtk-callback)
                       ptr)))

(export 'container-forall)

;;; ----------------------------------------------------------------------------
;;; gtk_container_propagate_draw ()
;;;
;;; void
;;; gtk_container_propagate_draw (GtkContainer *container,
;;;                               GtkWidget *child,
;;;                               cairo_t *cr);
;;;
;;; When a container receives a call to the draw function, it must send
;;; synthetic "draw" calls to all children that don't have their own GdkWindows.
;;; This function provides a convenient way of doing this. A container, when it
;;; receives a call to its "draw" function, calls gtk_container_propagate_draw()
;;; once for each child, passing in the cr the container received.
;;;
;;; gtk_container_propagate_draw() takes care of translating the origin of cr,
;;; and deciding whether the draw needs to be sent to the child. It is a
;;; convenient and optimized way of getting the same effect as calling
;;; gtk_widget_draw() on the child directly.
;;;
;;; In most cases, a container can simply either inherit the "draw"
;;; implementation from GtkContainer, or do some drawing and then chain to the
;;; ::draw implementation from GtkContainer.
;;;
;;; container :
;;;     a GtkContainer
;;;
;;; child :
;;;     a child of container
;;;
;;; cr :
;;;     Cairo context as passed to the container. If you want to use cr in
;;;     container's draw function, consider using cairo_save() and
;;;     cairo_restore() before calling this function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_get_focus_chain                           deprecated
;;; gtk_container_set_focus_chain
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_set_focus_chain" %container-set-focus-chain) :void
  (container (g:object container))
  (focusable (g:list-t (g:object widget))))

(defun (setf container-focus-chain) (focusable container)
  (%container-set-focus-chain container
                              (mapcar #'g:object-pointer focusable))
  focusable)

(cffi:defcfun ("gtk_container_get_focus_chain"
                %container-get-focus-chain) :boolean
  (container (g:object container))
  (focusable (:pointer (g:list-t (g:object widget)))))

(defun container-focus-chain (container)
 #+liber-documentation
 "@version{2025-07-16}
  @syntax{(gtk:container-focus-chain container) => focusable}
  @syntax{(setf (gtk:container-focus-chain container) focusable)}
  @argument[container]{a @class{gtk:container} widget}
  @argument[focusable]{a list of @class{gtk:widget} widgets representing the
    focus chain}
  @begin{short}
    Accessor of the focus chain widgets of the container.
  @end{short}
  The @fun{gtk:container-focus-chain} function retrieves the focus chain of the
  container, if one has been set explicitly. If no focus chain has been
  explicitly set, GTK computes the focus chain based on the positions of the
  children. In that case, GTK returns @em{false}. The
  @setf{gtk:container-focus-chain} function sets a focus chain, overriding the
  one computed automatically by GTK.

  In principle each widget in the chain should be a descendant of the container,
  but this is not enforced by this method, since it is allowed to set the focus
  chain before you pack the widgets, or have a widget in the chain that is not
  always packed. The necessary checks are done when the focus chain is actually
  traversed.
  @begin[Warning]{dictionary}
    The @fun{gtk:container-focus-chain} function has been deprecated since
    version 3.24 and should not be used in newly written code. For overriding
    focus behavior, use the @sig[gtk:widget]{focus} signal.
  @end{dictionary}
  @see-class{gtk:container}
  @see-class{gtk:widget}
  @see-function{gtk:container-unset-focus-chain}"
  (cffi:with-foreign-object (focusable '(g:list-t (g:object widget)))
    (when (%container-get-focus-chain container focusable)
      (cffi:mem-ref focusable '(g:list-t (g:object widget))))))

(export 'container-focus-chain)

;;; ----------------------------------------------------------------------------
;;; gtk_container_unset_focus_chain                         deprecated
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_unset_focus_chain"
                container-unset-focus-chain) :void
 #+liber-documentation
 "@version{#2025-07-16}
  @argument[container]{a @class{gtk:container} widget}
  @begin{short}
    Removes a focus chain explicitly set with the
    @fun{gtk:container-focus-chain} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:container-unset-focus-chain} function has been deprecated since
    version 3.24 and should not be used in newly written code. For overriding
    focus behavior, use the @sig[gtk:widget]{focus} signal.
  @end{dictionary}
  @see-class{gtk:container}
  @see-function{gtk:container-focus-chain}"
  (container (g:object container)))

(export 'container-unset-focus-chain)

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_find_child_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_container_class_find_child_property"
                %container-class-find-child-property)
    (:pointer (:struct g:param-spec))
  (class (:pointer (:struct g:type-class)))
  (property :string))

(defun container-class-find-child-property (gtype property)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[gtype]{a @class{g:type-t} type ID}
  @argument[property]{a string for the name of the child property to find}
  @begin{return}
    The @sym{g:param-spec} instance of the child property or a
    @code{cffi:null-pointer} if the @arg{gtype} type ID has no child property
    with that name.
  @end{return}
  @begin{short}
    Finds a child property of a container type by name.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:container-class-find-child-property \"GtkBox\" \"expand\")
=> #.(SB-SYS:INT-SAP #X00A7DA60)
(g:param-spec-type *)
=> #<GTYPE :name \"GParamBoolean\" :id 24606448>
(g:param-spec-value-type **)
=> #<GTYPE :name \"gboolean\" :id 20>
(gtk:container-class-find-child-property \"GtkBox\" \"unknown\")
=> #.(SB-SYS:INT-SAP #X00000000)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:container}
  @see-symbol{g:type-t}
  @see-symbol{g:param-spec}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%container-class-find-child-property class property)))
        (unless (cffi:null-pointer-p pspec) pspec))
      (g:type-class-unref class))))

(export 'container-class-find-child-property)

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_install_child_property ()
;;;
;;; void
;;; gtk_container_class_install_child_property (GtkContainerClass *cclass,
;;;                                             guint property_id,
;;;                                             GParamSpec *pspec);
;;;
;;; Installs a child property on a container class.
;;;
;;; cclass :
;;;     a GtkContainerClass
;;;
;;; property_id :
;;;     the id for the property
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_install_child_properties ()
;;;
;;; void
;;; gtk_container_class_install_child_properties (GtkContainerClass *cclass,
;;;                                               guint n_pspecs,
;;;                                               GParamSpec **pspecs);
;;;
;;; Installs child properties on a container class.
;;;
;;; cclass :
;;;     a GtkContainerClass

;;; n_pspecs :
;;;     the length of the GParamSpec array
;;;
;;; pspecs :
;;;     the GParamSpec array defining the new child properties.
;;;
;;; Since 3.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_list_child_properties
;;; ----------------------------------------------------------------------------

;; gobject::object-class is not exported

(cffi:defcfun ("gtk_container_class_list_child_properties"
                %container-class-list-child-properties)
    (:pointer (:pointer (:struct g:param-spec)))
  (class (:pointer (:struct gobject::object-class)))
  (n-props (:pointer :uint)))

(defun container-class-list-child-properties (gtype)
 #+liber-documentation
 "@version{2025-07-11}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{The list of @sym{g:param-spec} instances.}
  @short{Returns the child properties for a container type.}
  @begin[Notes]{dictionary}
    In the Lisp binding we pass the type of a container class and not
    a pointer to the container class as argument to the function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Get the child properties of GtkBox.
    @begin{pre}
(gtk:container-class-list-child-properties \"GtkBox\")
=> (#.(SB-SYS:INT-SAP #X55878748BE60) #.(SB-SYS:INT-SAP #X7FC3F800C2F0)
    #.(SB-SYS:INT-SAP #X558787508850) #.(SB-SYS:INT-SAP #X5587875088D0)
    #.(SB-SYS:INT-SAP #X558787508950))
(mapcar #'g:param-spec-name *)
=> (\"expand\" \"fill\" \"padding\" \"pack-type\" \"position\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk:container}
  @see-class{g:type-t}
  @see-class{g:param-spec}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%container-class-list-child-properties class n-props)))
          (unwind-protect
            (iter (for count from 0 below (cffi:mem-ref n-props :uint))
                  (for pspec = (cffi:mem-aref pspecs :pointer count))
                  (collect pspec))
            (g:free pspecs))))
      (g:type-class-unref class))))

(export 'container-class-list-child-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_container_class_handle_border_width ()
;;;
;;; void gtk_container_class_handle_border_width (GtkContainerClass *klass);
;;;
;;; Modifies a subclass of GtkContainerClass to automatically add and remove the
;;; border-width setting on GtkContainer. This allows the subclass to ignore the
;;; border width in its size request and allocate methods. The intent is for a
;;; subclass to invoke this in its class_init function.
;;;
;;; gtk_container_class_handle_border_width() is necessary because it would
;;; break API too badly to make this behavior the default. So subclasses must
;;; "opt in" to the parent class handling border_width for them.
;;;
;;; klass :
;;;     the class struct of a GtkContainer subclass
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.container.lisp ----------------------------------------
