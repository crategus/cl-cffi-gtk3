;;; ----------------------------------------------------------------------------
;;; gtk3.box.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation in the Lisp binding is
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
;;; GtkBox
;;;
;;;     A container for packing widgets in a single row or column
;;;
;;; Types and Values
;;;
;;;     GtkBox
;;;
;;; Accessors
;;;
;;;     gtk_box_get_homogeneous
;;;     gtk_box_set_homogeneous
;;;     gtk_box_get_spacing
;;;     gtk_box_set_spacing
;;;     gtk_box_get_baseline_position
;;;     gtk_box_set_baseline_position
;;;
;;; Functions
;;;
;;;     gtk_box_new
;;;     gtk_box_pack_start
;;;     gtk_box_pack_end
;;;     gtk_box_reorder_child
;;;     gtk_box_query_child_packing
;;;     gtk_box_set_child_packing
;;;     gtk_box_get_center_widget
;;;     gtk_box_set_center_widget
;;;
;;; Properties
;;;
;;;     baseline-position
;;;     homogeneous
;;;     spacing
;;;
;;; Child Properties
;;;
;;;     expand
;;;     fill
;;;     pack-type
;;;     padding
;;;     position
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ├── GtkAppChooserWidget
;;;                     ├── GtkButtonBox
;;;                     ├── GtkColorChooserWidget
;;;                     ├── GtkColorSelection
;;;                     ├── GtkFileChooserButton
;;;                     ├── GtkFileChooserWidget
;;;                     ├── GtkFontChooserWidget
;;;                     ├── GtkFontSelection
;;;                     ├── GtkHBox
;;;                     ├── GtkInfoBar
;;;                     ├── GtkRecentChooserWidget
;;;                     ├── GtkShortcutsSection
;;;                     ├── GtkShortcutsGroup
;;;                     ├── GtkShortcutsShortcut
;;;                     ├── GtkStackSwitcher
;;;                     ├── GtkStatusbar
;;;                     ╰── GtkVBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBox
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBox" box
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_box_get_type")
  ((baseline-position
    box-baseline-position
    "baseline-position" "GtkBaselinePosition" t t)
   (homogeneous
    box-homogeneous
    "homogeneous" "gboolean" t t)
   (spacing
    box-spacing
    "spacing" "gint" t t)))

#+liber-documentation
(setf (documentation 'box 'type)
 "@version{2025-06-24}
  @begin{short}
    The @class{gtk:box} widget arranges child widgets into a single row or
    column, depending upon the @val[gtk:orientation]{:horizontal} or
    @val[gtk:orientation]{:vertical} value of its inherited
    @slot[gtk:orientable]{orientation} property.
  @end{short}
  Within the other dimension, all children are allocated the same size. Of
  course, the @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign}
  properties can be used on the children to influence their allocation.

  The @class{gtk:box} widget uses a notion of packing. Packing refers to adding
  widgets with reference to a particular position in a @class{gtk:container}
  widget. For a box, there are two reference positions: the start and the end
  of the box. For a vertical box, the start is defined as the top of the box
  and the end is defined as the bottom. For a horizontal box the start is
  defined as the left side and the end is defined as the right side.

  Use repeated calls to the @fun{gtk:box-pack-start} function to pack widgets
  into a box from start to end. Use the @fun{gtk:box-pack-end} function to add
  widgets from end to start. You may intersperse these calls and add widgets
  from both ends of the same box.

  Because the @class{gtk:box} widget is a @class{gtk:container} widget, you may
  also use the @fun{gtk:container-add} function to insert widgets into the box,
  and they will be packed with the default values for the @code{expand} and
  @code{fill} child properties. Use the @fun{gtk:container-remove} function to
  remove widgets from the box.

  Use the @fun{gtk:box-homogeneous} function to specify whether or not all
  children of the box are forced to get the same amount of space.

  Use the @fun{gtk:box-spacing} function to determine how much space will be
  minimally placed between all children in the box. Note that spacing is added
  between the children, while padding added by the @fun{gtk:box-pack-start} or
  @fun{gtk:box-pack-end} functions is added on either side of the widget it
  belongs to.

  Use the @fun{gtk:box-reorder-child} function to move a @class{gtk:box} child
  widget to a different place in the box.

  Use the @fun{gtk:box-child-packing} function to reset the
  @prop[gtk:box]{expand}, @prop[gtk:box]{fill} and @prop[gtk:box]{padding} child
  properties and use the @fun{gtk:box-query-child-packing} function to query
  these properties.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:box} implementation uses a single CSS node with name
    @code{box}. In horizontal orientation, the nodes of the children are always
    arranged from left to right. So @code{:first-child} will always select the
    leftmost child widget, regardless of text direction.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[box:expand]{property}
      The @code{expand} child property of type @code{:boolean} (Read / Write)
      @br{}
      Whether the child widget should receive extra space when the parent
      widget grows. The @slot[gtk:widget]{hexpand} or
      @slot[gtk:widget]{vexpand} properties are the preferred way to influence
      whether the child widget receives extra space, by setting the expand
      property of the child widget corresponding to the orientation of the
      box. In contrast to the @slot[gtk:widget]{hexpand} property, the
      @prop[gtk:box]{expand} child property does not cause the box to expand
      itself. @br{}
      Default value: @em{false}
    @end{property}
    @begin[box:fill]{property}
      The @code{fill} child property of type @code{:boolean} (Read / Write)
      @br{}
      Whether the child widget should fill extra space or use it as padding.
      The @slot[gtk:widget]{halign} or @slot[gtk:widget]{valign} properties
      are the preferred way to influence whether the child widget fills
      available space, by setting the align property of the child widget
      corresponding to the orientation of the box to the
      @val[gtk:align]{:fill} value of the @sym{gtk:align} enumeration to fill,
      or to something else to refrain from filling. @br{}
      Default value: @em{true}
    @end{property}
    @begin[box:pack-type]{property}
      The @code{pack-type} child property of type @sym{gtk:pack-type}
      (Read / Write) @br{}
      The pack type indicating whether the child widget is packed with
      reference to the start or end of the parent widget. @br{}
      Default value: @val[gtk:pack-type]{:start}
    @end{property}
    @begin[box:padding]{property}
      The @code{padding} child property of type @code{:uint} (Read / Write)
      @br{}
      The extra space to put between the child widget and its neighbors, in
      pixels. The CSS padding properties are the preferred way to add space
      among widgets, by setting the paddings corresponding to the orientation
      of the box. @br{}
      Allowed values: <= @code{G_MAXINT} @br{}
      Default value: 0
    @end{property}
    @begin[box:position]{property}
      The @code{position} child property of type @code{:int} (Read / Write)
      @br{}
      The index of the child widget in the parent widget. @br{}
      Allowed values: >= -1 @br{}
      Default value: 0
    @end{property}
  @end{dictionary}
  @see-constructor{gtk:box-new}
  @see-slot{gtk:box-baseline-position}
  @see-slot{gtk:box-homogeneous}
  @see-slot{gtk:box-spacing}
  @see-class{gtk:orientable}
  @see-class{gtk:container}
  @see-symbol{gtk:orientation}
  @see-symbol{gtk:pack-type}
  @see-symbol{gtk:align}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:box-baseline-position ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "baseline-position" 'box) t)
 "The @code{baseline-position} property of type @sym{gtk:baseline-position}
  (Read / Write) @br{}
  The position of the baseline aligned widgets if extra space is available.@br{}
  Default value: @val[gtk:baseline-position]{:center}")

#+liber-documentation
(setf (liber:alias-for-function 'box-baseline-position)
      "Accessor"
      (documentation 'box-baseline-position 'function)
 "@version{2025-06-27}
  @syntax{(gtk:box-baseline-position object) => position}
  @syntax{(setf (gtk:box-baseline-position object) position)}
  @argument[object]{a @class{gtk:box} container widget}
  @argument[position]{a @sym{gtk:baseline-position} value for the baseline
    position}
  @begin{short}
    Accessor of the @slot[gtk:box]{baseline-position} slot of the
    @class{gtk:box} class.
  @end{short}
  The @fun{gtk:box-baseline-position} function gets the baseline position of a
  box. The @setf{gtk:box-baseline-position} functions sets the baseline
  position.

  This affects only horizontal boxes with at least one baseline aligned child
  widget. If there is more vertical space available than requested, and the
  baseline is not allocated by the parent widget then @arg{position} is used
  to allocate the baseline wrt the extra space available.
  @see-class{gtk:box}
  @see-symbol{gtk:baseline-position}")

;;; --- gtk:box-homogeneous ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "homogeneous" 'box) t)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Whether the children should all be the same size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'box-homogeneous)
      "Accessor"
      (documentation 'box-homogeneous 'function)
 "@version{2024-03-15}
  @syntax{(gtk:box-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:box} container widget}
  @argument[homogeneous]{@em{true} to create equal allotments, @em{false}
    for variable allotments}
  @begin{short}
    Accessor of the @slot[gtk:box]{homogeneous} slot of the @class{gtk:box}
    class.
  @end{short}
  The @fun{gtk:box-homogeneous} function returns whether the box is homogeneous.
  The @setf{gtk:box-homogeneous} function sets the @slot[gtk:box]{homogeneous}
  property of the box, controlling whether or not all children of the box are
  given equal space in the box.
  @see-class{gtk:box}")

;;; --- gtk:box-spacing --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spacing" 'box) t)
 "The @code{spacing} property of type @code{:int} (Read / Write) @br{}
  The amount of space between children. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'box-spacing)
      "Accessor"
      (documentation 'box-spacing 'function)
 "@version{2025-06-06}
  @syntax{(gtk:box-spacing object) => spacing}
  @syntax{(setf (gtk:box-spacing object) spacing)}
  @argument[object]{a @class{gtk:box} container widget}
  @argument[spacing]{an integer for the number of pixels to put between
    children}
  @begin{short}
    Accessor of the @slot[gtk:box]{spacing} slot of the @class{gtk:box} class.
  @end{short}
  The @fun{gtk:box-spacing} function returns the spacing between children. The
  @setf{gtk:box-spacing} function sets the @slot[gtk:box]{spacing} property of
  the box, which is the number of pixels to place between children of the box.
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Child Property Implementation
;;; ----------------------------------------------------------------------------

;;; --- gtk:box-child-expand ---------------------------------------------------

(define-child-property box-child-expand "expand" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'box-child-expand)
      "Accessor"
      (documentation 'box-child-expand 'function)
 "@version{2025-06-24}
  @syntax{(gtk:box-child-expand container child) => expand}
  @syntax{(setf (gtk:box-child-expand container child) expand)}
  @argument[container]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[expand]{@em{true}, if @arg{child} is to be given extra space}
  @begin{short}
    Accessor of the @prop[gtk:box]{expand} child property of the @class{gtk:box}
    class.
  @end{short}
  Whether the child widget should receive extra space when the parent widget
  grows. The @slot[gtk:widget]{hexpand} or @slot[gtk:widget]{vexpand} properties
  are the preferred way to influence whether the child widget receives extra
  space, by setting the expand property of the child widget corresponding to
  the orientation of the box. In contrast to the
  @slot[gtk:widget]{hexpand} property, the @prop[gtk:box]{expand} child property
  does not cause the box to expand itself.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-function{gtk:widget-hexpand}
  @see-function{gtk:widget-vexpand}")

;;; --- gtk:box-child-fill -----------------------------------------------------

(define-child-property box-child-fill "fill" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'box-child-fill)
      "Accessor"
      (documentation 'box-child-fill 'function)
 "@version{2025-06-24}
  @syntax{(gtk:box-child-expand container child) => fill}
  @syntax{(setf (gtk:box-child-expand container child) fill)}
  @argument[container]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[fill]{@em{true}, if space given to @arg{child} by the expand option}
  @begin{short}
    Accessor of the @prop[gtk:box]{fill} child property of the @class{gtk:box}
    class.
  @end{short}
  Whether the child widget should fill extra space or use it as padding. The
  @slot[gtk:widget]{halign} or @slot[gtk:widget]{valign} properties are the
  preferred way to influence whether the child widget fills available space, by
  setting the align property of the child widget corresponding to the
  orientation of the box to the @val[gtk:align]{:fill} value of the
  @sym{gtk:align} enumeration to fill, or to something else to refrain from
  filling.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-symbol{gtk:align}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}")

;;; --- gtk:box-child-pack-type ------------------------------------------------

(define-child-property box-child-pack-type "pack-type" "GtkPackType" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'box-child-pack-type)
      "Accessor"
      (documentation 'box-child-pack-type 'function)
 "@version{2025-06-24}
  @syntax{(gtk:box-child-pack-type container child) => packtype}
  @syntax{(setf (gtk:box-child-pack-type container child) packtype)}
  @argument[container]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[packtype]{a value of the @sym{gtk:pack-type} enumeration}
  @begin{short}
    Accessor of the @prop[gtk:box]{pack-type} child property of the
    @class{gtk:box} class.
  @end{short}
  A pack type indicating whether the child widget is packed with reference to
  the start or end of the parent widget.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}")

;;; --- gtk:box-child-padding --------------------------------------------------

(define-child-property box-child-padding "padding" "guint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'box-child-padding)
      "Accessor"
      (documentation 'box-child-padding 'function)
 "@version{2025-06-24}
  @syntax{(gtk:box-child-padding container child) => padding}
  @syntax{(setf (gtk:box-child-padding container child) padding)}
  @argument[container]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[padding]{an unsigned integer for extra space in pixels to put
    between childs}
  @begin{short}
    Accessor of the @prop[gtk:box]{padding} child property of the
    @class{gtk:box} class.
  @end{short}
  Extra space to put between the child widget and its neighbors, in pixels. The
  CSS padding properties are the preferred way to add space among widgets, by
  setting the paddings corresponding to the orientation of the box.
  @see-class{gtk:box}
  @see-class{gtk:widget}")

;;; --- gtk:box-child-position -------------------------------------------------

(define-child-property box-child-position "position" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'box-child-position)
      "Accessor"
      (documentation 'box-child-position 'function)
 "@version{2025-06-24}
  @syntax{(gtk:box-child-position container child) => position}
  @syntax{(setf (gtk:box-child-position container child) position)}
  @argument[container]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[position]{an integer for the position of @arg{child} in a box}
  @begin{short}
    Accessor of the @prop[gtk:box]{position} child property of the
    @class{gtk:box} class.
  @end{short}
  The index of the child widget in the parent widget.
  @see-class{gtk:box}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline box-new))

(defun box-new (orientation &optional (spacing 0))
 #+liber-documentation
 "@version{2025-07-07}
  @argument[orientation]{a @sym{gtk:orientation} value for the orientation}
  @argument[spacing]{an optional integer for the number of pixels to place by
    default between children}
  @return{The new @class{gtk:box} widget.}
  @begin{short}
    Creates a new box with the given @arg{orientation} and an optional value
    for the @slot[gtk:box]{spacing} property.
  @end{short}
  The default value for @arg{spacing} is 0.
  @see-class{gtk:box}
  @see-symbol{gtk:orientation}
  @see-function{gtk:box-spacing}"
  (make-instance 'box
                 :orientation orientation
                 :spacing spacing))

(export 'box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_box_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_pack_start" %box-pack-start) :void
  (box (g:object box))
  (child (g:object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-start (box child &key (expand t) (fill t) (padding 0))
 #+liber-documentation
 "@version{2025-06-06}
  @argument[box]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to @arg{box}}
  @argument[expand]{@em{true} if @arg{child} is to be given extra space
    allocated to @arg{box}}
  @argument[fill]{@em{true} if space given to @arg{child} by the expand option
    is actually allocated to @arg{child}, rather than just padding it}
  @argument[padding]{an unsigned integer for extra space in pixels}
  @begin{short}
    Adds a child widget to the box, packed with reference to the start of the
    box.
  @end{short}
  The child widget is packed after any other child widget packed with reference
  to the start of the box.

  The extra space of the @arg{expand} parameter will be divided evenly between
  all children that use this option. The default value is @em{true}.

  The @arg{fill} parameter has no effect if @arg{expand} is set to @em{false}.
  A child widget is always allocated the full height of a horizontal box and
  the full width of a vertical box. This option affects the other dimension.
  The default value is @em{true}.

  The @arg{padding} parameter is the extra space in pixels to put between this
  child widget and its neighbors, over and above the global amount specified by
  the @slot[gtk:box]{spacing} property. If the child widget is a widget at one
  of the reference ends of the box, then padding pixels are also put between
  the child widget and the reference edge of the box. The default value is 0.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-function{gtk:box-pack-end}
  @see-function{gtk:box-spacing}
  @see-function{gtk:box-child-expand}
  @see-function{gtk:box-child-fill}
  @see-function{gtk:box-child-padding}"
  (%box-pack-start box child expand fill padding))

(export 'box-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_box_pack_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_pack_end" %box-pack-end) :void
  (box (g:object box))
  (child (g:object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-end (box child &key (expand t) (fill t) (padding 0))
 #+liber-documentation
 "@version{2025-06-06}
  @argument[box]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to @arg{box}}
  @argument[expand]{@em{true} if @arg{child} is to be given extra space
    allocated to @arg{box}}
  @argument[fill]{@em{true} if space given to @arg{child} by the @arg{expand}
    option is actually allocated to @arg{child}, rather than just padding it}
  @argument[padding]{an unsigned integer for extra space in pixels}
  @begin{short}
    Adds a child widget to the box, packed with reference to the end of the box.
  @end{short}
  The child widget is packed after (away from end of) any other child widget
  packed with reference to the end of the box.

  The extra space of the @arg{expand} parameter will be divided evenly between
  all children that use this option. The default value is @em{true}.

  The @arg{fill} parameter has no effect if @arg{expand} is set to @em{false}.
  A child widget is always allocated the full height of a horizontal box and
  the full width of a vertical box. This option affects the other dimension.
  The default value is @em{true}.

  The @arg{padding} parameter is the extra space in pixels to put between this
  child widget and its neighbors, over and above the global amount specified by
  the @slot[gtk:box]{spacing} property. If the child widget is a widget at one
  of the reference ends of the box, then padding pixels are also put between
  the child widget and the reference edge of the box. The default value is 0.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-function{gtk:box-pack-start}
  @see-function{gtk:box-spacing}
  @see-function{gtk:box-child-expand}
  @see-function{gtk:box-child-fill}
  @see-function{gtk:box-child-padding}"
  (%box-pack-end box child expand fill padding))

(export 'box-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_box_reorder_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_box_reorder_child" box-reorder-child) :void
 #+liber-documentation
 "@version{2025-06-24}
  @argument[box]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget to move}
  @argument[position]{an integer for the position for @arg{child} in the
    list of children of @arg{box}, starting from 0, if negative, indicates the
    end of the list}
  @begin{short}
    Moves the child widget to a new position in the list of the box children.
  @end{short}
  The list is the children field of the box, and contains both widgets
  packed @val[gtk:pack-type]{:start} as well as widgets packed
  @val[gtk:pack-type]{:end}, in the order that these widgets were added to the
  box.

  A position of the child widget in the box children list determines where the
  widget is packed into the box. A child widget at some position in the list
  will be packed just after all other widgets of the same packing type that
  appear earlier in the list.
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}"
  (box (g:object box))
  (child (g:object widget))
  (position :int))

(export 'box-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_box_query_child_packing
;;; ----------------------------------------------------------------------------

(declaim (inline box-query-child-packing))

(defun box-query-child-packing (box child)
 #+liber-documentation
 "@version{2025-06-06}
  @syntax{(gtk:box-query-child-packing box child) => expand, fill, padding,
    packtype}
  @argument[box]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget to query}
  @argument[expand]{a boolean for the @prop[gtk:box]{expand} child property}
  @argument[fill]{a boolean for the @prop[gtk:box]{fill} child property}
  @argument[padding]{an integer for the @prop[gtk:box]{padding} child property}
  @argument[packtype]{a @sym{gtk:pack-type} value for the
    @prop[gtk:box]{pack-type} child property}
  @begin{short}
    Obtains information about how the child widget is packed into the box.
  @end{short}
  @begin[Lisp implementation]{dictionary}
    In the Lisp binding you can use the @fun{gtk:box-child-expand},
    @fun{gtk:box-child-fill}, @fun{gkt-box-child-padding}, and
    @fun{gtk:box-child-pack-type} functions to retrieve the child properties.
    This function is implemented with these child accessor functions.
  @end{dictionary}
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}
  @see-function{gtk:box-child-packing}
  @see-function{gtk:box-child-expand}
  @see-function{gtk:box-child-fill}
  @see-function{gtk:box-child-padding}
  @see-function{gtk:box-child-pack-type}"
  (values (box-child-expand box child)
          (box-child-fill box child)
          (box-child-padding box child)
          (box-child-pack-type box child)))

(export 'box-query-child-packing)

;;; ----------------------------------------------------------------------------
;;; gtk_box_set_child_packing
;;; ----------------------------------------------------------------------------

(defun box-child-packing (box child expand fill padding packtype)
 #+liber-documentation
 "@version{2024-03-15}
  @argument[box]{a @class{gtk:box} container widget}
  @argument[child]{a @class{gtk:widget} child widget to set}
  @argument[expand]{a boolean for the @prop[gtk:box]{expand} child property}
  @argument[fill]{a boolean for the @prop[gtk:box]{fill} child property}
  @argument[padding]{an integer for the @prop[gtk:box]{padding} child property}
  @argument[packtype]{a @sym{gtk:pack-type} value for the
    @prop[gtk:box]{pack-type} child property}
  @begin{short}
    Sets the way the child widget is packed into the box.
  @end{short}
  @begin[Lisp implementation]{dictionary}
    In the Lisp binding you can use the @fun{gtk:box-child-expand},
    @fun{gtk:box-child-fill}, @fun{gkt-box-child-padding}, and
    @fun{gtk:box-child-pack-type} functions to set the child properties. This
    function is implemented with these child accessor functions.
  @end{dictionary}
  @see-class{gtk:box}
  @see-class{gtk:widget}
  @see-symbol{gtk:pack-type}
  @see-function{gtk:box-query-child-packing}
  @see-function{gtk:box-child-expand}
  @see-function{gtk:box-child-fill}
  @see-function{gtk:box-child-padding}
  @see-function{gtk:box-child-pack-type}"
  (setf (box-child-expand box child) expand
        (box-child-fill box child) fill
        (box-child-padding box child) padding
        (box-child-pack-type box child) packtype))

(export 'box-child-packing)

;;; ----------------------------------------------------------------------------
;;; gtk_box_get_center_widget
;;; gtk_box_set_center_widget
;;; ----------------------------------------------------------------------------

(defun (setf box-center-widget) (widget box)
  (cffi:foreign-funcall "gtk_box_set_center_widget"
                        (g:object box) box
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_box_get_center_widget" box-center-widget) (g:object widget)
 #+liber-documentation
 "@version{2024-03-15}
  @syntax{(gtk:box-center-widget box) => widget}
  @syntax{(setf (gtk:box-center-widget box) widget)}
  @argument[box]{a @class{gtk:box} container widget}
  @argument[widget]{a @class{gtk:widget} child widget to center}
  @begin{short}
    The @fun{gtk:box-center-widget} function retrieves the center widget of the
    box.
  @end{short}
  The @setf{gtk:box-center-widget} function sets a center widget. That is a
  child widget that will be centered with respect to the full width of the box,
  even if the children at either side take up different amounts of space.
  @see-class{gtk:box}
  @see-class{gtk:widget}"
  (box (g:object box)))

(export 'box-center-widget)

;;; --- End of file gtk3.box.lisp ----------------------------------------------
