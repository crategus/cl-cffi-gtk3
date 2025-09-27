;;; ----------------------------------------------------------------------------
;;; gtk3.size-group.lisp
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
;;; GtkSizeGroup
;;;
;;;     Grouping widgets so they request the same size
;;;
;;; Types and Values
;;;
;;;     GtkSizeGroup
;;;     GtkSizeGroupMode
;;;
;;; Functions
;;;
;;;     gtk_size_group_new
;;;     gtk_size_group_set_mode
;;;     gtk_size_group_get_mode
;;;     gtk_size_group_set_ignore_hidden
;;;     gtk_size_group_get_ignore_hidden
;;;     gtk_size_group_add_widget
;;;     gtk_size_group_remove_widget
;;;     gtk_size_group_get_widgets
;;;
;;; Properties
;;;
;;;     ignore-hidden
;;;     mode
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSizeGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSizeGroup implements GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSizeGroupMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkSizeGroupMode" size-group-mode
  (:export t
   :type-initializer "gtk_size_group_mode_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'size-group-mode)
      "GEnum"
      (liber:symbol-documentation 'size-group-mode)
 "@version{#2025-07-06}
  @begin{declaration}
(gobject:define-genum \"GtkSizeGroupMode\" size-group-mode
  (:export t
   :type-initializer \"gtk_size_group_mode_get_type\")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))
 @end{declaration}
 @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Group has no effect.}
      @entry[:horizontal]{Group affects horizontal requisition.}
      @entry[:vertical]{Group affects vertical requisition.}
      @entry[:both]{Group affects both horizontal and vertical requisition.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The mode of the size group determines the directions in which the size
    group affects the requested sizes of its component widgets.
  @end{short}
  @see-class{gtk:size-group}")

;;; ----------------------------------------------------------------------------
;;; GtkSizeGroup
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSizeGroup" size-group
  (:superclass g:object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_size_group_get_type")
  ((ignore-hidden
    size-group-ignore-hidden
    "ignore-hidden" "gboolean" t t)
   (mode
    size-group-mode
    "mode" "GtkSizeGroupMode" t t)))

#+liber-documentation
(setf (documentation 'size-group 'type)
 "@version{#2025-07-06}
  @begin{short}
    The @class{gtk:size-group} object provides a mechanism for grouping a number
    of widgets together so they all request the same amount of space. This is
    typically useful when you want a column of widgets to have the same size,
    but you cannot use a @class{gtk:grid} widget.
  @end{short}

  In detail, the size requested for each widget in a @class{gtk:size-group}
  object is the maximum of the sizes that would have been requested for each
  widget in the size group if they were not in the size group. The mode of the
  size group, see the @fun{gtk:size-group-mode} function, determines whether
  this applies to the horizontal size, the vertical size, or both sizes.

  Note that size groups only affect the amount of space requested, not the
  size that the widgets finally receive. If you want the widgets in a
  @class{gtk:size-group} object to actually be the same size, you need to pack
  them in such a way that they get the size they request and not more. For
  example, if you are packing your widgets into a table, you would not include
  the @val[gtk:align]{:fill} flag.

  The @class{gtk:size-group} objects are referenced by each widget in the size
  group, so once you have added all widgets to a @class{gtk:size-group} object,
  you can drop the initial reference to the size group with the
  @fun{g:object-unref} function. If the widgets in the size group are
  subsequently destroyed, then they will be removed from the size group and drop
  their references on the size group. When all widgets have been removed, the
  size group will be freed.

  Widgets can be part of multiple size groups. GTK will compute the horizontal
  size of a widget from the horizontal requisition of all widgets that can be
  reached from the widget by a chain of size groups of type
  @val[gtk:size-group-mode]{:horizontal} or @val[gtk:size-group-mode]{:both},
  and the vertical size from the vertical requisition of all widgets that can
  be reached from the widget by a chain of size groups of type
  @val[gtk:size-group-mode]{:vertical} or @val[gtk:size-group-mode]{:both}.

  Note that only non-contextual sizes of every widget are ever consulted by
  size groups, since size groups have no knowledge of what size a widget will
  be allocated in one dimension, it cannot derive how much height a widget
  will receive for a given width. When grouping widgets that trade height for
  width in mode @val[gtk:size-group-mode]{:vertical} or
  @val[gtk:size-group-mode]{:both}, the height for the minimum width will be the
  requested height for all widgets in the group. The same is of course true when
  horizontally grouping width for height widgets.

  Widgets that trade height-for-width should set a reasonably large minimum
  width by way of \"width-chars\" for instance. Widgets with static sizes as
  well as widgets that grow (such as ellipsizing text) need no such
  considerations.
  @begin[GtkSizeGroup as GtkBuildable]{dictionary}
    Size groups can be specified in a UI definition by placing an
    @code{<object>} element with class \"GtkSizeGroup\" somewhere in the UI
    definition. The widgets that belong to the size group are specified by a
    @code{<widgets>} element that may contain multiple @code{<widget>} elements,
    one for each member of the size group. The name attribute gives the ID of
    the widget.

    @b{Example:} A UI definition fragment with the @class{gtk:size-group}
    object.
    @begin{pre}
<object class=\"GtkSizeGroup\">
  <property name=\"mode\">GTK_SIZE_GROUP_HORIZONTAL</property>
  <widgets>
    <widget name=\"radio1\"/>
    <widget name=\"radio2\"/>
  </widgets>
</object>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:size-group-new}
  @see-slot{gtk:size-group-ignore-hidden}
  @see-slot{gtk:size-group-mode}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:size-group-ignore-hidden -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ignore-hidden" 'size-group) t)
 "The @code{ignore-hidden} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, unmapped widgets are ignored when determining the size of the
  group. @br{}
  @em{Warning:} The @code{ignore-hidden} property has been deprecated since
  version 3.22 and should not be used in newly written code. Measuring the size
  of hidden widgets has not worked reliably for a long time. In most cases, they
  will report a size of 0 nowadays, and thus, their size will not affect the
  other size group members. In effect, size groups will always operate as if
  this property was @em{true}. Use a @class{gtk:stack} widget instead to hide
  widgets while still having their size taken into account. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'size-group-ignore-hidden)
      "Accessor"
      (documentation 'size-group-ignore-hidden 'function)
 "@version{#2023-02-23}
  @syntax{(gtk:size-group-ignore-hidden object) => ignore-hidden}
  @syntax{(setf (gtk:size-group-ignore-hidden object) ignore-hidden)}
  @argument[size-group]{a @class{gtk:size-group} object}
  @argument[ignore-hidden]{a boolean whether unmapped widgets should be ignored
    when calculating the size}
  @begin{short}
    Accessor of the @slot[gtk:size-group]{ignore-hidden} slot of the
    @class{gtk:size-group} class.
  @end{short}
  The @fun{gtk:size-group-ignore-hidden} function returns if invisible widgets
  are ignored when calculating the size. The @setf{gtk:size-group-ignore-hidden}
  function sets whether unmapped widgets should be ignored when calculating the
  size.
  @begin[Warning]{dictionary}
    The @fun{gtk:size-group-ignore-hidden} function has been deprecated since
    version 3.22 and should not be used in newly written code. Measuring the
    size of hidden widgets has not worked reliably for a long time. In most
    cases, they will report a size of 0 nowadays, and thus, their size will not
    affect the other size group members. In effect, size groups will always
    operate as if this property was @em{true}. Use a @class{gtk:stack} widget
    instead to hide widgets while still having their size taken into account.
  @end{dictionary}
  @see-class{gtk:size-group}")

;;; --- gtk:size-group-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'size-group) t)
 "The @code{mode} property of type @sym{gtk:size-group-mode} (Read / Write)@br{}
  The directions in which the size group affects the requested sizes of its
  component widgets. @br{}
  Default value: @val[gtk:size-group-mode]{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'size-group-mode)
      "Accessor"
      (documentation 'size-group-mode 'function)
 "@version{#2025-07-06}
  @syntax{(gtk:size-group-mode object) => mode}
  @syntax{(setf (gtk:size-group-mode object) mode)}
  @argument[size-group]{a @class{gtk:size-group} object}
  @argument[mode]{a @sym{gtk:size-group-mode} value to set for the size group}
  @begin{short}
    Accessor of the @slot[gtk:size-group]{mode} slot of the
    @class{gtk:size-group} class.
  @end{short}
  The @fun{gtk:size-group-mode} function gets the current mode of the size
  group. The @setf{gtk:size-group-mode} function sets the mode of the size
  group.

  The mode of the size group determines whether the widgets in the size group
  should all have the same horizontal requisition,
  @val[gtk:size-group-mode]{:horizontal}, all have the same vertical
  requisition, @val[gtk:size-group-mode]{:vertical}, or should all have the
  same requisition in both directions, @val[gtk:size-groupmode]{:both}.
  @see-class{gtk:size-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_new
;;; ----------------------------------------------------------------------------

(declaim (inline size-group-new))

(defun size-group-new (mode)
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[mode]{a @sym{gtk:size-group-mode} value for the new size group}
  @return{The newly created @class{gtk:size-group} object.}
  @short{Create a new size group.}
  @see-class{gtk:size-group}
  @see-symbol{gtk:size-group-mode}"
  (make-instance 'size-group
                 :mode mode))

(export 'size-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_add_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_size_group_add_widget" size-group-add-widget) :void
 #+liber-documentation
 "@version{#2023-02-23}
  @argument[group]{a @class{gtk:size-group} object}
  @argument[widget]{a @class{gtk:widget} widget to add}
  @begin{short}
    Adds a widget to a @class{gtk:size-group} object.
  @end{short}
  In the future, the requisition of the widget will be determined as the maximum
  of its requisition and the requisition of the other widgets in the size group.
  Whether this applies horizontally, vertically, or in both directions depends
  on the mode of the size group. See the @fun{gtk:size-group-mode} function.

  When the widget is destroyed or no longer referenced elsewhere, it will be
  removed from the size group.
  @see-class{gtk:size-group}
  @see-class{gtk:widget}
  @see-function{gtk:size-group-mode}"
  (group (g:object size-group))
  (widget (g:object widget)))

(export 'size-group-add-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_remove_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_size_group_remove_widget" size-group-remove-widget) :void
 #+liber-documentation
 "@version{#2023-02-23}
  @argument[group]{a @class{gtk:size-group} object}
  @argument[widget]{a @class{gtk:widget} widget to remove}
  @begin{short}
    Removes a widget from a size group.
  @end{short}
  @see-class{gtk:size-group}
  @see-class{gtk:widget}"
  (group (g:object size-group))
  (widget (g:object widget)))

(export 'size-group-remove-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_widgets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_size_group_get_widgets" size-group-widgets)
    (g:slist-t g:object :free-from-foreign nil)
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[group]{a @class{gtk:size-group} object}
  @return{The list of widgets.}
  @begin{short}
    Returns the list of widgets associated with the size group.
  @end{short}
  @see-class{gtk:size-group}"
  (group (g:object size-group)))

(export 'size-group-widgets)

;;; --- End of file gtk3.size-group.lisp ---------------------------------------
