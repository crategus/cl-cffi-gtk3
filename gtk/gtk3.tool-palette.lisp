;;; ----------------------------------------------------------------------------
;;; gtk3.tool-palette.lisp
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
;;; GtkToolPalette
;;;
;;;     A tool palette with categories
;;;
;;; Types and Values
;;;
;;;     GtkToolPalette
;;;     GtkToolPaletteDragTargets
;;;
;;; Functions
;;;
;;;     gtk_tool_palette_new
;;;     gtk_tool_palette_get_exclusive
;;;     gtk_tool_palette_set_exclusive
;;;     gtk_tool_palette_get_expand
;;;     gtk_tool_palette_set_expand
;;;     gtk_tool_palette_get_group_position
;;;     gtk_tool_palette_set_group_position
;;;     gtk_tool_palette_get_icon_size                     Accessor
;;;     gtk_tool_palette_set_icon_size                     Accessor
;;;     gtk_tool_palette_unset_icon_size
;;;     gtk_tool_palette_get_style
;;;     gtk_tool_palette_set_style
;;;     gtk_tool_palette_unset_style
;;;
;;;     gtk_tool_palette_add_drag_dest
;;;     gtk_tool_palette_get_drag_item
;;;     gtk_tool_palette_get_drag_target_group
;;;     gtk_tool_palette_get_drag_target_item
;;;     gtk_tool_palette_get_drop_group
;;;     gtk_tool_palette_get_drop_item
;;;     gtk_tool_palette_set_drag_source
;;;
;;;     gtk_tool_palette_get_hadjustment                   deprecated
;;;     gtk_tool_palette_get_vadjustment                   deprecated
;;;
;;; Properties
;;;
;;;     icon-size
;;;     icon-size-set
;;;     toolbar-style
;;;
;;; Child Properties
;;;
;;;     exclusive
;;;     expand
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkToolPalette
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolPalette implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkScrollable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkToolPaletteDragTargets
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkToolPaletteDragTargets" tool-palette-drag-targets
  (:export t
   :type-initializer "gtk_tool_palette_drag_targets_get_type")
  (:items 1)
  (:groups 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'tool-palette-drag-targets)
      "GFlags"
      (liber:symbol-documentation 'tool-palette-drag-targets)
 "@version{#2021-3-14}
  @begin{short}
    Flags used to specify the supported drag targets.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkToolPaletteDragTargets\" tool-palette-drag-targets
  (:export t
   :type-initializer \"gtk_tool_palette_drag_targets_get_type\")
  (:items 1)
  (:groups 2))
  @end{pre}
  @begin[code]{table}
    @entry[:items]{Support drag of items.}
    @entry[:groups]{Support drag of groups.}
  @end{table}
  @see-class{gtk:tool-palette}")

;;; ----------------------------------------------------------------------------
;;; struct GtkToolPalette
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToolPalette" tool-palette
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkScrollable")
   :type-initializer "gtk_tool_palette_get_type")
  ((icon-size
    tool-palette-icon-size
    "icon-size" "GtkIconSize" t t)
   (icon-size-set
    tool-palette-icon-size-set
    "icon-size-set" "gboolean" t t)
   (toolbar-style
    tool-palette-toolbar-style
    "toolbar-style" "GtkToolbarStyle" t t)))

#+liber-documentation
(setf (documentation 'tool-palette 'type)
 "@version{#2023-3-28}
  @begin{short}
    A @sym{gtk:tool-palette} widget allows you to add @class{gtk:tool-item}
    widgets to a palette-like container with different categories and drag and
    drop support.
  @end{short}

  @image[toolpalette]{GtkToolPalette}

  A @sym{gtk:tool-palette} widget is created with a call to the
  @fun{gtk:tool-palette-new} function.

  @class{gtk:tool-item} widgets cannot be added directly to a
  @sym{gtk:tool-palette} widget - instead they are added to a
  @class{gtk:tool-item-group} widget which can than be added to a
  @sym{gtk:tool-palette} widget. To add a @class{gtk:tool-item-group} widget to
  a @sym{gtk:tool-palette} widget, use the @fun{gtk:container-add} function.
  @begin{pre}
(let ((palette (gtk:tool-palette-new))
      (group (gtk:tool-item-group-new \"Test Category\"))
      (item (make-instance 'gtk:tool-button
                           :icon-name \"gtk-ok\")))
  (gtk:container-add palette group)
  (gtk:tool-item-group-insert group item -1)
  ... )
  @end{pre}
  The easiest way to use drag and drop with the @sym{gtk:tool-palette} widget
  is to call the @fun{gtk:tool-palette-add-drag-dest} function with the desired
  drag source palette and the desired drag target widget. Then the
  @fun{gtk:tool-palette-drag-item} function can be used to get the dragged item
  in the \"drag-data-received\" signal handler of the drag target.
  @begin{pre}
static void
passive_canvas_drag_data_received (GtkWidget        *widget,
                                   GdkDragContext   *context,
                                   gint              x,
                                   gint              y,
                                   GtkSelectionData *selection,
                                   guint             info,
                                   guint             time,
                                   gpointer          data)
{
  GtkWidget *palette;
  GtkWidget *item;

  /* Get the dragged item */
  palette = gtk_widget_get_ancestor (gtk_drag_get_source_widget (context),
                                     GTK_TYPE_TOOL_PALETTE);
  if (palette != NULL)
    item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (palette),
                                           selection);

  /* Do something with item */
@}

GtkWidget *target, palette;

palette = gtk_tool_palette_new ();
target = gtk_drawing_area_new ();

g_signal_connect (G_OBJECT (target), \"drag-data-received\",
                  G_CALLBACK (passive_canvas_drag_data_received), NULL);
gtk_tool_palette_add_drag_dest (GTK_TOOL_PALETTE (palette), target,
                                GTK_DEST_DEFAULT_ALL,
                                GTK_TOOL_PALETTE_DRAG_ITEMS,
                                GDK_ACTION_COPY);
  @end{pre}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:tool-palette} widget has a single CSS node named
    @code{toolpalette}.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[exclusive]{entry}
        The @code{exclusive} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item group should be the only one that is expanded at a
        given time. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[expand]{entry}
        The @code{expand} child property of type @code{:boolean}
        (Read / Write) @br{}
        Whether the item group should receive extra space when the palette
        grows at a given time. @br{}
        Default value: @em{false}
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:tool-palette-new}
  @see-slot{gtk:tool-palette-icon-size}
  @see-slot{gtk:tool-palette-icon-size-set}
  @see-slot{gtk:tool-palette-toolbar-style}
  @see-class{gtk:tool-item}
  @see-class{gtk:tool-item-group}
  @see-function{gtk:tool-palette-new}
  @see-function{gtk:container-add}
  @see-function{gtk:tool-palette-add-drag-dest}
  @see-function{gtk:tool-palette-drag-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tool-palette-icon-size -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-size" 'tool-palette) t)
 "The @code{icon-size} property of type @symbol{gtk:icon-size} (Read / Write)
  @br{}
  The size of the icons in a tool palette is normally determined by the
  @slot[gtk:settings]{gtk:toolbar-icon-size} setting. When this property is set,
  it overrides the setting. This should only be used for special purpose tool
  palettes, normal application tool palettes should respect the user preferences
  for the size of icons. @br{}
  Default value: @code{:small-toolbar}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-palette-icon-size)
      "Accessor"
      (documentation 'tool-palette-icon-size 'function)
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-palette-icon-size object) => icon-size}
  @syntax[]{(setf (gtk:tool-palette-icon-size object) icon-size)}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[icon-size]{a value of the @symbol{gtk:icon-size} enumeration that
    icons in the tool palette shall have}
  @begin{short}
    Accessor of the @slot[gtk:tool-palette]{icon-size} slot of the
    @class{gtk:tool-palette} class.
  @end{short}
  The @sym{gtk:tool-palette-icon-size} function gets the size of icons in the
  tool palette. The @sym{(setf gtk:tool-palette-icon-size)} function sets the
  size of icons.
  @see-class{gtk:tool-palette}
  @see-symbol{gtk:icon-size}")

;;; --- tool-palette-icon-size-set ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-size-set" 'tool-palette) t)
 "The @code{icon-size-set} property of type @code{:boolean} (Read / Write) @br{}
  Is @em{true} if the @code{icon-size} property has been set. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-palette-icon-size-set)
      "Accessor"
      (documentation 'tool-palette-icon-size-set 'function)
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-palette-icon-size.set object) => setting}
  @syntax[]{(setf (gtk:tool-palette-icon-size-set object) setting)}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[setting]{a boolean whether the @slot[gtk:tool-palette]{icon-size}
    property has been set}
  @begin{short}
    Accessor of the @slot[gtk:tool-palette]{icon-size-set} slot of the
    @class{gtk:tool-palette} class.
  @end{short}
  Is @em{true} if the @slot[gtk:tool-palette]{icon-size} property has been set.
  @see-class{gtk:tool-palette}
  @see-function{gtk:tool-palette-icon-set}
  @see-function{gtk:tool-palette-unset-icon-size}")

;;; --- tool-palette-toolbar-style ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "toolbar-style" 'tool-palette) t)
 "The @code{toolbar-style} property of type @symbol{gtk:toolbar-style}
  (Read / Write) @br{}
  The style of items in the tool palette. @br{}
  Default value: @code{:icons}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-palette-toolbar-style)
      "Accessor"
      (documentation 'tool-palette-toolbar-style 'function)
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-palette-toolbar-style object) => style}
  @syntax[]{(setf (gtk:tool-palette-toolbar-style object) style)}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[style]{a value of the @symbol{gtk:toolbar-style} enumeration that
    items in the tool palette shall have}
  @begin{short}
    Accessor of the @slot[gtk:tool-palette]{toolbar-style} slot of the
    @class{gtk:tool-palette} class.
  @end{short}
  The @sym{gtk:tool-palette-toolbar-style} function gets the style, icons, text
  or both, of items in the tool palette. The
  @sym{(setf gtk:tool-palette-toolbar-style)} function sets the style.
  @see-class{gtk:tool-palette}
  @see-symbol{gtk:toolbar-style}
  @see-function{gtk:tool-palette-unset-style}")

;;; ----------------------------------------------------------------------------
;;; Accessors of Child Properties
;;; ----------------------------------------------------------------------------

;;; --- tool-palette-child-exclusive -------------------------------------------

(define-child-property tool-palette-child-exclusive
                       "exclusive" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-palette-child-exclusive)
      "Accessor"
      (documentation 'tool-palette-child-exclusive 'function)
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-palette-child-exclusive container child) => exclusive}
  @syntax[]{(setf (gtk:tool-palette-child-exclusive container child) exclusive)}
  @argument[container]{a @class{gtk:tool-palette} widget}
  @argument[child]{a @class{gtk:tool-item-group} widget which is a child of
    @arg{container}}
  @argument[exclusive]{a boolean whether the tool item group should be
    exclusive or not}
  @begin{short}
    Accessor of the @code{exclusive} child property of the
    @class{gtk:tool-palette} class.
  @end{short}
  The @sym{gtk:tool-palette-child-exclusive} function gets whether the tool
  item group is exclusive or not. The
  @sym{(setf gtk:tool-palette-child-exclusive)} function sets the child
  property.

  If an exclusive tool item group is expanded all other groups are collapsed.
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item-group}")

;;; --- tool-palette-child-expand ----------------------------------------------

(define-child-property tool-palette-child-expand
                       "expand" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'tool-palette-child-expand)
      "Accessor"
      (documentation 'tool-palette-child-expand 'function)
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-palette-child-expand container child) => expand}
  @syntax[]{(setf (gtk:tool-palette-child-expand container child) expand)}
  @argument[container]{a @class{gtk:tool-palette} widget}
  @argument[child]{a @class{gtk:tool-item-group} widget which is a child of
    @arg{container}}
  @argument[expand]{a boolean whether the tool item group should be given
    extra space}
  @begin{short}
    Accessor of the @code{expand} child property of the @class{gtk:tool-palette}
    class.
  @end{short}
  The @sym{gtk:tool-palette-child-expand} function gets whether the tool item
  group should be given extra space. The
  @sym{(setf gtk:tool-palette-child-expand)} function sets the child property.
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline tool-palette-new))

(defun tool-palette-new ()
 #+liber-documentation
 "@version{#2023-3-28}
  @return{A new @class{gtk:tool-palette} widget.}
  @short{Creates a new tool palette.}
  @see-class{gtk:tool-palette}"
  (make-instance 'tool-palette))

(export 'tool-palette-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_exclusive ()
;;; ----------------------------------------------------------------------------

;; Implemented as tool-palette-child-exclusive

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_exclusive ()
;;; ----------------------------------------------------------------------------

;; Implemented as (setf tool-palette-child-exclusive

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_expand ()
;;; ----------------------------------------------------------------------------

;; Implemented as tool-palette-child-expand

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_expand ()
;;; ----------------------------------------------------------------------------

;; Implemented as (setf tool-palette-child-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_group_position ()
;;; gtk_tool_palette_set_group_position () -> tool-palette-group-position
;;; ----------------------------------------------------------------------------

(defun (setf tool-palette-group-position) (position palette group)
  (cffi:foreign-funcall "gtk_tool_palette_set_group_position"
                        (g:object tool-palette) palette
                        (g:object tool-item-group) group
                        :int position
                        :void)
  position)

(defcfun ("gtk_tool_palette_get_group_position" tool-palette-group-position)
    :int
 #+liber-documentation
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-palette-group-position palette group) => position}
  @syntax[]{(setf (gtk:tool-palette-group-position palette group) position)}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[group]{a @class{gtk:tool-item-group} widget which is a child of
    @arg{palette}}
  @argument[position]{an integer with an index for @arg{group}}
  @begin{short}
    Accessor of the index of the tool item group in the tool palette.
  @end{short}
  The @sym{gtk:tool-palette-group-position} function gets the position of
  @arg{group} in @arg{palette} as index. The
  @sym{(setf gtk:tool-palette-group-position)} function sets the position.

  If @arg{position} is 0 the group will become the first child, if
  @arg{position} is -1 it will become the last child.
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item-group}"
  (palette (g:object tool-palette))
  (group (g:object tool-item-group)))

(export 'tool-palette-group-position)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_unset_icon_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_unset_icon_size" tool-palette-unset-icon-size) :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @begin{short}
    Unsets the tool palette icon size set with the
    @fun{gtk:tool-palette-icon-size} function, so that user preferences will be
    used to determine the icon size.
  @end{short}
  @see-class{gtk:tool-palette}
  @see-function{gtk:tool-palette-icon-size}"
  (palette (g:object tool-palette)))

(export 'tool-palette-unset-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_style ()
;;; ----------------------------------------------------------------------------

;; Implemented as tool-palette-toolbar-style

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_style ()
;;; ----------------------------------------------------------------------------

;; Implemented as (setf tool-palette-toolbar-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_unset_style ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_unset_style" tool-palette-unset-style) :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @begin{short}
    Unsets a toolbar style set with the @fun{gtk:tool-palette-toolbar-style}
    function, so that user preferences will be used to determine the toolbar
    style.
  @end{short}
  @see-class{gtk:tool-palette}
  @see-function{gtk:tool-palette-toolbar-style}"
  (palette (g:object tool-palette)))

(export 'tool-palette-unset-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_add_drag_dest ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_add_drag_dest" tool-palette-add-drag-dest) :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[widget]{a @class{gtk:widget} object which should be a drag
    destination for @arg{palette}}
  @argument[flags]{the @symbol{gtk:dest-defaults} flags that specify what
    actions GTK+ should take for drops on that @arg{widget}}
  @argument[targets]{the @symbol{gtk:tool-palette-drag-targets} flags which the
    @arg{widget} should support}
  @argument[actions]{the @symbol{gdk:drag-action} flags which the @arg{widget}
    should suppport}
  @begin{short}
    Sets the tool palette as drag source and sets a widget as a drag
    destination for drags from the tool palette.
  @end{short}
  See the @fun{gtk:tool-palette-set-drag-source} and @fun{gtk:drag-dest-set}
  functions.
  @see-class{gtk:tool-palette}
  @see-symbol{gtk:dest-defaults}
  @see-symbol{gtk:tool-palette-drag-targets}
  @see-symbol{gdk:drag-action}
  @see-function{gtk:drag-dest-set}
  @see-function{gtk:tool-palette-set-drag-source}"
  (palette (g:object tool-palette))
  (widget (g:object widget))
  (flags dest-defaults)
  (targets tool-palette-drag-targets)
  (actions gdk:drag-action))

(export 'tool-palette-add-drag-dest)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_item () -> tool-palette-drag-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drag_item" tool-palette-drag-item)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[selection]{a @class{gtk:selection-data} object}
  @return{The @class{gtk:widget} dragged item in @arg{selection}.}
  @begin{short}
    Get the dragged item from @arg{selection}.
  @end{short}
  This could be a @class{gtk:tool-item} or a @class{gtk:tool-item-group} widget.
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item}
  @see-class{gtk:tool-item-group}
  @see-class{gtk:selection-data}"
  (palette (g:object tool-palette))
  (selection (g:boxed selection-data)))

(export 'tool-palette-drag-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_target_group ()
;;; -> tool-palette-drag-target-group
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drag_target_group"
          %tool-palette-drag-target-group) :pointer)

(defun tool-palette-drag-target-group ()
 #+liber-documentation
 "@version{#2023-3-28}
  @return{A list with the target entry for a dragged group.}
  @begin{short}
    Gets the target entry for a dragged @class{gtk:tool-item-group} widget.
  @end{short}
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item-group}"
  (let ((target-ptr (%tool-palette-drag-target-group)))
    (with-foreign-slots ((target flags info)
                         target-ptr
                         (:struct %target-entry))
      (list target flags info))))

(export 'tool-palette-drag-target-group)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drag_target_item ()
;;; -> tool-palette-drag-target-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drag_target_item"
          %tool-palette-drag-target-item) :pointer)

(defun tool-palette-drag-target-item ()
 #+liber-documentation
 "@version{#2023-3-28}
  @return{A list with the target entry for a dragged item.}
  @begin{short}
    Gets the target entry for a dragged @class{gtk:tool-item} widget.
  @end{short}
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item}"
  (let ((target-ptr (%tool-palette-drag-target-item)))
    (with-foreign-slots ((target flags info)
                         target-ptr
                         (:struct %target-entry))
      (list target flags info))))

(export 'tool-palette-drag-target-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drop_group () -> tool-palette-drop-group
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drop_group" tool-palette-drop-group)
    (g:object tool-item-group)
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[x]{an integer with the x position}
  @argument[y]{an integer with the y position}
  @return{The @class{gtk:tool-item-group} widget at position
    (@arg{x}, @arg{y}) or @code{nil} if there is no such group.}
  @begin{short}
    Gets the group at position (@arg{x}, @arg{y}).
  @end{short}
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item-group}"
  (palette (g:object tool-palette))
  (x :int)
  (y :int))

(export 'tool-palette-drop-group)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_drop_item () -> tool-palette-drop-item
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_get_drop_item" tool-palette-drop-item)
    (g:object tool-item)
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[x]{an integer with the x position}
  @argument[y]{an integer with the y position}
  @begin{return}
    The @class{gtk:tool-item} widget at position (@arg{x}, @arg{y}) or
    @code{nil} if there is no such item.
  @end{return}
  @begin{short}
    Gets the item at position (@arg{x}, @arg{y}).
  @end{short}
  See also the @fun{gtk:tool-palette-drop-group} function.
  @see-class{gtk:tool-palette}
  @see-class{gtk:tool-item}
  @see-function{gtk:tool-palette-drop-group}"
  (palette (g:object tool-palette))
  (x :int)
  (y :int))

(export 'tool-palette-drop-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_set_drag_source ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_tool_palette_set_drag_source" tool-palette-set-drag-source) :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[palette]{a @class{gtk:tool-palette} widget}
  @argument[targets]{the @symbol{gtk:tool-palette-drag-targets} flags which the
    widget should support}
  @begin{short}
    Sets the tool palette as a drag source.
  @end{short}
  Enables all groups and items in the tool palette as drag sources on button 1
  and button 3 press with copy and move actions. See the
  @fun{gtk:drag-source-set} function.
  @see-class{gtk:tool-palette}
  @see-function{gtk:drag-source-set}"
  (palette (g:object tool-palette))
  (targets tool-palette-drag-targets))

(export 'tool-palette-set-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_hadjustment ()
;;;
;;; GtkAdjustment * gtk_tool_palette_get_hadjustment (GtkToolPalette *palette);
;;;
;;; Warning
;;;
;;; gtk_tool_palette_get_hadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gtk_scrollable_get_hadjustment()
;;;
;;; Gets the horizontal adjustment of the tool palette.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Returns :
;;;     the horizontal adjustment of palette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_tool_palette_get_vadjustment ()
;;;
;;; GtkAdjustment * gtk_tool_palette_get_vadjustment (GtkToolPalette *palette);
;;;
;;; Warning
;;;
;;; gtk_tool_palette_get_vadjustment has been deprecated since version 3.0 and
;;; should not be used in newly written code. Use
;;; gtk_scrollable_get_vadjustment()
;;;
;;; Gets the vertical adjustment of the tool palette.
;;;
;;; palette :
;;;     a GtkToolPalette
;;;
;;; Returns :
;;;     the vertical adjustment of palette
;;;
;;; Since 2.20
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.tool-palette.lisp -------------------------------------
