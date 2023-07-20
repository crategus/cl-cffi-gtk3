;;; ----------------------------------------------------------------------------
;;; gtk3.tool-item-group.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; GtkToolItemGroup
;;;
;;;     A sub container used in a tool palette
;;;
;;; Types and Values
;;;
;;;     GtkToolItemGroup
;;;
;;; Functions
;;;
;;;     gtk_tool_item_group_get_collapsed                  Accessor
;;;     gtk_tool_item_group_get_drop_item
;;;     gtk_tool_item_group_get_ellipsize                  Accessor
;;;     gtk_tool_item_group_get_item_position
;;;     gtk_tool_item_group_get_n_items
;;;     gtk_tool_item_group_get_label                      Accessor
;;;     gtk_tool_item_group_get_label_widget               Accessor
;;;     gtk_tool_item_group_get_nth_item
;;;     gtk_tool_item_group_get_header_relief              Accessor
;;;     gtk_tool_item_group_insert
;;;     gtk_tool_item_group_new
;;;     gtk_tool_item_group_set_collapsed                  Accessor
;;;     gtk_tool_item_group_set_ellipsize                  Accessor
;;;     gtk_tool_item_group_set_item_position
;;;     gtk_tool_item_group_set_label                      Accessor
;;;     gtk_tool_item_group_set_label_widget               Accessor
;;;     gtk_tool_item_group_set_header_relief              Accessor
;;;
;;; Properties
;;;
;;;     collapsed
;;;     ellipsize
;;;     header-relief
;;;     label
;;;     label-widget
;;;
;;; Child Properties
;;;
;;;     expand
;;;     fill
;;;     homogeneous
;;;     new-row
;;;     position
;;;
;;; Style Properties
;;;
;;;     expander-size
;;;     header-spacing
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkToolItemGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolItemGroup implements AtkImplementorIface, GtkBuildable and
;;;     GtkToolShell.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolItemGroup
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkToolItemGroup" tool-item-group
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkToolShell")
   :type-initializer "gtk_tool_item_group_get_type")
  ((collapsed
    tool-item-group-collapsed
    "collapsed" "gboolean" t t)
   (ellipsize
    tool-item-group-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (header-relief
    tool-item-group-header-relief
    "header-relief" "GtkReliefStyle" t t)
   (label
    tool-item-group-label
    "label" "gchararray" t t)
   (label-widget
    tool-item-group-label-widget
    "label-widget" "GtkWidget" t t)))

#+liber-documentation
(setf (documentation 'tool-item-group 'type)
 "@version{#2023-2-27}
  @begin{short}
    A @sym{gtk:tool-item-group} widget is used together with a
    @class{gtk:tool-palette} widget to add @class{gtk:tool-item} widgets to a
    palette like container with different categories and drag and drop support.
  @end{short}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:tool-item-group} implementation has a single CSS node named
    @code{toolitemgroup}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should receive extra space when the group grows. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[fill]{entry}
        The @code{fill} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should fill the available space. @br{}
        Default value: @em{true}
      @end{entry}
      @begin[homogenous]{entry}
        The @code{homogeneous} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item should be the same size as other homogeneous items.
        @br{}
        Default value: @em{true}
      @end{entry}
      @begin[new-row]{entry}
        The @code{new-row} child property of type @code{:boolean} (Read / Write)
        @br{}
        Whether the item should start a new row. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        Position of the item within this group. @br{}
        Allowed values: >= 0 @br{}
        Default value: 0
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[expander-size]{entry}
        The @code{expander-size} style property of type @code{:int} (Read) @br{}
        Size of the expander arrow. @br{}
        Allowed values: >= 0 @br{}
        Default value: 16
      @end{entry}
      @begin[header-spacing]{entry}
        The @code{header-spacing} style property of type @code{:int} (Read)
        @br{}
        Spacing between expander arrow and caption. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:tool-item-group-new}
  @see-slot{gtk:tool-item-group-collapsed}
  @see-slot{gtk:tool-item-group-ellipsize}
  @see-slot{gtk:tool-item-group-header-relief}
  @see-slot{gtk:tool-item-group-label}
  @see-slot{gtk:tool-item-group-label-widget}
  @see-class{gtk:tool-item}
  @see-class{gtk:tool-palette}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tool-item-group-collapsed ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "collapsed" 'tool-item-group) t)
 "The @code{collapsed} property of type @code{:boolean} (Read / Write) @br{}
  Whether the group has been collapsed and items are hidden. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-collapsed)
      "Accessor"
      (documentation 'tool-item-group-collapsed 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-collapsed object) => collapsed}
  @syntax[]{(setf (gtk:tool-item-group-collapsed object) collapsed)}
  @argument[object]{a @class{gtk:tool-item-group} widget}
  @argument[collapsed]{a boolean whether the group should be collapsed
    or expanded}
  @begin{short}
    Accessor of the @slot[gtk:tool-item-group]{collapsed} slot of the
    @class{gtk:tool-item-group} class.
  @end{short}
  The @sym{gtk:tool-item-group-collapsed} function gets whether the tool item
  group is collapsed or expanded. The @sym{(setf gtk:tool-item-group-collapsed)}
  function sets whether the tool item group should be collapsed or expanded.
  @see-class{gtk:tool-item-group}")

;;; --- tool-item-group-ellipsize ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize" 'tool-item-group) t)
 "The @code{ellipsize} property of type @symbol{pango:ellipsize-mode}
  (Read / Write) @br{}
  Ellipsize for item group headers. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-ellipsize)
      "Accessor"
      (documentation 'tool-item-group-ellipsize 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-ellipsize object) => ellipsize}
  @syntax[]{(setf (gtk:tool-item-group-ellipsize object) ellipsize)}
  @argument[object]{a @class{gtk:tool-item-group} widget}
  @argument[ellipsize]{a @symbol{pango:ellipsize-mode} value for labels in
    @arg{group}}
  @begin{short}
    Accessor of the @slot[gtk:tool-item-group]{ellipsize} slot of the
    @class{gtk:tool-item-group} class.
  @end{short}
  The @sym{gtk:tool-item-group-ellipsize} function gets the ellipsization mode
  of the tool item group. The @sym{(setf gtk:tool-item-group-ellipsize)}
  function sets the ellipsization mode which should be used by labels in the
  tool item group.
  @see-class{gtk:tool-item-group}
  @see-symbol{pango:ellipsize-mode}")

;;; --- tool-item-group-header-relief ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "header-relief"
                                               'tool-item-group) t)
 "The @code{header-relief} property of type @symbol{gtk:relief-style}
  (Read / Write) @br{}
  Relief of the group header button. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-header-relief)
      "Accessor"
      (documentation 'tool-item-group-header-relief 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-header-relief object) => style}
  @syntax[]{(setf (gtk:tool-item-group-header-relief object) style)}
  @argument[group]{a @class{gtk:tool-item-group} widget}
  @argument[object]{a value of the @symbol{gtk:relief-style} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:tool-item-group]{header-relief} slot of the
    @class{gtk:tool-item-group} class.
  @end{short}
  The @sym{gtk:tool-item-group-header-relief} function gets the relief mode of
  the header button of the tool item group. The
  @sym{(setf gtk:tool-item-group-header-relief)} function sets the button
  relief of the group header.
  @see-class{gtk:tool-item-group}
  @see-symbol{gtk:relief-style}")

;;; --- tool-item-group-label --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'tool-item-group) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The human readable title of this item group. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-label)
      "Accessor"
      (documentation 'tool-item-group-label 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-label object) => label}
  @syntax[]{(setf (gtk:tool-item-group-label object) label)}
  @argument[object]{a @class{gtk:tool-item-group} widget}
  @argument[label]{a string with the new human readable label of of the group}
  @begin{short}
    Accessor of the @slot[gtk:tool-item-group]{label} slot of the
    @class{gtk:tool-item-group} class.
  @end{short}
  The @sym{gtk:tool-item-group-label} function gets the label of the tool item
  group. The @sym{(setf gtk:tool-item-group-label)} function sets the label of
  the tool item group.

  The label is displayed in the header of the group. Note that @code{nil} is
  returned if a custom label has been set with the
  @fun{gtk:tool-item-group-label-widget} function.
  @see-class{gtk:tool-item-group}
  @see-function{gtk:tool-item-group-label-widget}")

;;; --- tool-item-group-label-widget -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget"
                                               'tool-item-group) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  A widget to display in place of the usual label.")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-label-widget)
      "Accessor"
      (documentation 'tool-item-group-label-widget 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-label-widget object) => label-widget}
  @syntax[]{(setf (gtk:tool-item-group-label-widget object) label-widget)}
  @argument[object]{a @class{gtk:tool-item-group} widget}
  @argument[label-widget]{a @class{gtk:widget} widget to be displayed in place
    of the usual label}
  @begin{short}
    Accessor of the @slot[gtk:tool-item-group]{label-widget} slot of the
    @class{gtk:tool-item-group} class.
  @end{short}
  The @sym{gtk:tool-item-group-label-widget} function gets the label widget of
  the tool item group. The @sym{(setf gtk:tool-item-group-label-widget)}
  function sets the label of the tool item group.

  The label widget is displayed in the header of the group, in place of the
  usual label.
  @see-class{gtk:tool-item-group}
  @see-class{gtk:widget}
  @see-function{gtk:tool-item-group-label}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- tool-item-group-child-expand -------------------------------------------

(define-child-property tool-item-group-child-expand
                       "expand" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-child-expand)
      "Accessor"
      (documentation 'tool-item-group-child-expand 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-child-expand container child) => expand}
  @syntax[]{(setf (gtk:tool-item-group-child-expand container child) expand)}
  @argument[container]{a @class{gtk:tool-item-group} widget}
  @argument[child]{a @class{gtk:widget} child object}
  @argument[expand]{a boolean whether the item should receive extra space when
    the group grows}
  @begin{short}
    Accessor of the @code{expand} child property of the
    @class{gtk:tool-item-group} class.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:widget}")

;;; --- tool-item-group-child-fill ---------------------------------------------

(define-child-property tool-item-group-child-fill
                       "fill" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-child-fill)
      "Accessor"
      (documentation 'tool-item-group-child-fill 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-child-fill container child) => fill}
  @syntax[]{(setf (gtk:tool-item-group-child-fill container child) fill)}
  @argument[container]{a @class{gtk:tool-item-group} widget}
  @argument[child]{a @class{gtk:widget} child object}
  @argument[fill]{a boolean whether the item should fill the available
    space}
  @begin{short}
    Accessor of the @code{fill} child property of the
    @class{gtk:tool-item-group} class.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:widget}")

;;; --- tool-item-group-child-homogeneous --------------------------------------

(define-child-property tool-item-group-child-homogeneous
                       "homogeneous" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-child-homogeneous)
      "Accessor"
      (documentation 'tool-item-group-child-homogeneous 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-child-homogeneous container child) => homogeneous}
  @syntax[]{(setf (gtk:tool-item-group-child-homogeneous container child) homogeneous)}
  @argument[container]{a @class{gtk:tool-item-group} widget}
  @argument[child]{a @class{gtk:widget} child object}
  @argument[fill]{a boolean whether the item should be the same size
    as other homogeneous items}
  @begin{short}
    Accessor of the @code{homogeneous} child property of the
    @class{gtk:tool-item-group} class.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:widget}")

;;; --- tool-item-group-child-new-row ------------------------------------------

(define-child-property tool-item-group-child-new-row
                       "new-row" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-child-new-row)
      "Accessor"
      (documentation 'tool-item-group-child-new-row 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-child-new-row container child) => new-row}
  @syntax[]{(setf (gtk:tool-item-group-child-new-row container child) new-row)}
  @argument[container]{a @class{gtk:tool-item-group} widget}
  @argument[child]{a @class{gtk:widget} child object}
  @argument[new-row]{a boolean whether the item should start a new row}
  @begin{short}
    Accessor of the @code{new-row} child property of the
    @class{gtk:tool-item-group} class.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:widget}")

;;; --- tool-item-group-child-position -----------------------------------------

(define-child-property tool-item-group-child-position
                       "position" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-group-child-position)
      "Accessor"
      (documentation 'tool-item-group-child-position 'function)
 "@version{#2023-2-27}
  @syntax[]{(gtk:tool-item-group-child-position container child) => position}
  @syntax[]{(setf (gtk:tool-item-group-child-position container child) position)}
  @argument[container]{a @class{gtk:tool-item-group} widget}
  @argument[child]{a @class{gtk:widget} child object}
  @argument[position]{an integer with the position of the item within the group}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk:tool-item-group} class.
  @end{short}
  The @sym{gtk:tool-item-group-child-position} function gets the position of
  @arg{item} in the list of children of @arg{container}, or -1 if @arg{item}
  is no child of @arg{container}. The @sym{gtk:tool-item-group-child-position}
  function sets the position, starting with 0, the position -1 means end of
  list.
  @see-class{gtk:tool-item-group}
  @see-class{gtk:tool-item}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_drop_item () -> tool-item-group-drop-item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_group_get_drop_item" tool-item-group-drop-item)
    (g:object tool-item)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[group]{a @class{gtk:tool-item-group} widget}
  @argument[x]{an integer with the x position}
  @argument[y]{an integer with the y position}
  @return{The @class{gtk:tool-item} widget at position (@arg{x}, @arg{y}).}
  @short{Gets the tool item at position (@arg{x}, @arg{y}).}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:tool-item}"
  (group (g:object tool-item-group))
  (x :int)
  (y :int))

(export 'tool-item-group-drop-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_item_position ()
;;; ----------------------------------------------------------------------------

;; Implemented as tool-item-group-child-position

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_n_items () -> tool-item-n-items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_group_get_n_items" tool-item-group-n-items) :uint
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[group]{a @class{gtk:tool-item-group} widget}
  @return{A unsigned integer with the number of tool items in @arg{group}.}
  @begin{short}
    Gets the number of tool items in the tool item group.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-function{gtk:tool-item-group-nth-item}"
  (group (g:object tool-item-group)))

(export 'tool-item-group-n-items)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_get_nth_item () -> tool-item-group-nth-item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_group_get_nth_item" tool-item-group-nth-item)
    (g:object tool-item)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[group]{a @class{gtk:tool-item-group} widget}
  @argument[index]{an unsigned integer with the index}
  @return{The @class{gtk:tool-item} widget at @arg{index}.}
  @begin{short}
    Gets the tool item at @arg{index} in the tool item group.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:tool-item}"
  (group (g:object tool-item-group))
  (index :uint))

(export 'tool-item-group-nth-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_insert ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_group_insert" tool-item-group-insert) :void
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[group]{a @class{gtk:tool-item-group} widget}
  @argument[item]{a @class{gtk:tool-item} widget to insert into group}
  @argument[position]{an integer with the position of @arg{item} in @arg{group},
    starting with 0, the position -1 means end of list}
  @begin{short}
    Inserts @arg{item} at @arg{position} in the list of children of the
    tool item group.
  @end{short}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:tool-item}"
  (group (g:object tool-item-group))
  (item (g:object tool-item))
  (position :int))

(export 'tool-item-group-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline tool-item-group-new))

(defun tool-item-group-new (label)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[label]{a string with the label of the new group}
  @return{A new @class{gtk:tool-item-group} widget.}
  @begin{short}
    Creates a new tool item group with label @arg{label}.
  @end{short}
  @see-class{gtk:tool-item-group}"
  (make-instance 'tool-item-group
                 :label label))

(export 'tool-item-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_group_set_item_position ()
;;; ----------------------------------------------------------------------------

;; Implemented as (setf tool-item-group-child-position

;;; --- End of file gtk3.tool-item-group.lisp ----------------------------------
