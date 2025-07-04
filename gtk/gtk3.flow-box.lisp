;;; ----------------------------------------------------------------------------
;;; gtk3.flow-box.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GtkFlowBox
;;;
;;;     A container that allows reflowing its children
;;;
;;; Types and Values
;;;
;;;     GtkFlowBox
;;;     GtkFlowBoxChild
;;;
;;; Accessors
;;;
;;;     gtk_flow_box_set_activate_on_single_click
;;;     gtk_flow_box_get_activate_on_single_click
;;;     gtk_flow_box_set_column_spacing
;;;     gtk_flow_box_get_column_spacing
;;;     gtk_flow_box_set_homogeneous
;;;     gtk_flow_box_get_homogeneous
;;;     gtk_flow_box_set_max_children_per_line
;;;     gtk_flow_box_get_max_children_per_line
;;;     gtk_flow_box_set_min_children_per_line
;;;     gtk_flow_box_get_min_children_per_line
;;;     gtk_flow_box_set_row_spacing
;;;     gtk_flow_box_get_row_spacing
;;;     gtk_flow_box_set_selection_mode
;;;     gtk_flow_box_get_selection_mode
;;;
;;; Functions
;;;
;;;     gtk_flow_box_new
;;;     gtk_flow_box_insert
;;;     gtk_flow_box_get_child_at_index
;;;     gtk_flow_box_get_child_at_pos
;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment
;;;     GtkFlowBoxForeachFunc
;;;     gtk_flow_box_selected_foreach
;;;     gtk_flow_box_get_selected_children
;;;     gtk_flow_box_select_child
;;;     gtk_flow_box_unselect_child
;;;     gtk_flow_box_select_all
;;;     gtk_flow_box_unselect_all
;;;     GtkFlowBoxFilterFunc
;;;     gtk_flow_box_set_filter_func
;;;     gtk_flow_box_invalidate_filter
;;;     GtkFlowBoxSortFunc
;;;     gtk_flow_box_set_sort_func
;;;     gtk_flow_box_invalidate_sort
;;;     GtkFlowBoxCreateWidgetFunc
;;;     gtk_flow_box_bind_model
;;;     gtk_flow_box_child_new
;;;     gtk_flow_box_child_get_index
;;;     gtk_flow_box_child_is_selected
;;;     gtk_flow_box_child_changed
;;;
;;; Properties
;;;
;;;     activate-on-single-click
;;;     column-spacing
;;;     homogeneous
;;;     max-children-per-line
;;;     min-children-per-line
;;;     row-spacing
;;;     selection-mode
;;;
;;; Signals
;;;
;;;     activate-cursor-child
;;;     child-activated
;;;     move-cursor
;;;     select-all
;;;     selected-children-changed
;;;     toggle-cursor-child
;;;     unselect-all
;;;     activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ├── GtkBin
;;;                 │   ╰── GtkFlowBoxChild
;;;                 ╰── GtkFlowBox
;;;
;;; Implemented Interfaces
;;;
;;; GtkFlowBox implements AtkImplementorIface, GtkBuildable and GtkOrientable.
;;;
;;; GtkFlowBoxChild implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxChild
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFlowBoxChild" flow-box-child
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_flow_box_child_get_type")
  nil)

#+liber-documentation
(setf (documentation 'flow-box-child 'type)
 "@version{2025-06-06}
  @begin{short}
    The @class{gtk:flow-box-child} widget is the kind of widget that can be
    added to a @class{gtk:flow-box} widget.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (child)    :action
      @end{pre}
      @begin[code]{table}
        @entry[child]{The @class{gtk:flow-box-child} widget on which the signal
          is emitted.}
      @end{table}
      The signal is emitted when the user activates a child widget in a
      @class{gtk:flow-box} widget, either by clicking or double-clicking, or by
      using the @kbd{Space} or @kbd{Enter} key. While this signal is used as a
      keybinding signal, it can be used by applications for their own purposes.
  @end{dictionary}
  @see-constructor{gtk:flow-box-child-new}
  @see-class{gtk:flow-box}")

;;; ----------------------------------------------------------------------------
;;; GtkFlowBox
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFlowBox" flow-box
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_flow_box_get_type")
  ((activate-on-single-click
    flow-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (column-spacing
    flow-box-column-spacing
    "column-spacing" "guint" t t)
   (homogeneous
    flow-box-homogeneous
    "homogeneous" "gboolean" t t)
   (max-children-per-line
    flow-box-max-children-per-line
    "max-children-per-line" "guint" t t)
   (min-children-per-line
    flow-box-min-children-per-line
    "min-children-per-line" "guint" t t)
   (row-spacing
    flow-box-row-spacing
    "row-spacing" "guint" t t)
   (selection-mode
    flow-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)))

#+liber-documentation
(setf (documentation 'flow-box 'type)
 "@version{2025-06-06}
  @begin{short}
    The @class{gtk:flow-box} widget positions child widgets in sequence
    according to its orientation.
  @end{short}

  @image[flow-box]{Figure: GtkFlowBox}

  For instance, with the horizontal orientation, the widgets will be arranged
  from left to right, starting a new row under the previous row when necessary.
  Reducing the width in this case will require more rows, so a larger height
  will be requested.

  Likewise, with the vertical orientation, the widgets will be arranged from
  top to bottom, starting a new column to the right when necessary. Reducing
  the height will require more columns, so a larger width will be requested.

  The size request of a @class{gtk:flow-box} widget alone may not be what you
  expect. If you need to be able to shrink it along both axes and dynamically
  reflow its children, you may have to wrap it in a @class{gtk:scrolled-window}
  widget to enable that.

  The children of a @class{gtk:flow-box} widget can be dynamically sorted and
  filtered.

  Although a @class{gtk:flow-box} widget must have only
  @class{gtk:flow-box-child} children, you can add any kind of widget to it via
  the @fun{gtk:container-add} function, and a @class{gtk:flow-box-child} widget
  will automatically be inserted between the box and the widget.

  Also see the @class{gtk:list-box} widget.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
flowbox
├── flowboxchild
│   ╰── <child>
├── flowboxchild
│   ╰── <child>
│
╰── [rubberband]
    @end{pre}
    The @class{gtk:flow-box} implementation uses a single CSS node with name
    @code{flowbox}. The @class{gtk:flow-box-child} implementation uses a single
    CSS node with name @code{flowboxchild}. For rubberband selection, a subnode
    with name @code{rubberband} is used.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-cursor-child\" signal}
      @begin{pre}
lambda (flowbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[flowbox]{The @class{gtk:flow-box} widget on which the signal is
          emitted.}
      @end{table}
      The signal is a keybinding signal which gets emitted when the user
      activates the flow box.
    @subheading{The \"child-activated\" signal}
      @begin{pre}
lambda (flowbox child)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[flowbox]{The @class{gtk:flow-box} widget on which the signal is
          emitted.}
        @entry[child]{The @class{gtk:flow-box-child} widget that is activated.}
      @end{table}
      The signal is emitted when a child has been activated by the user.
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (flowbox step count)    :action
      @end{pre}
      @begin[code]{table}
        @entry[flowbox]{The @class{gtk:flow-box} widget on which the signal is
          emitted.}
        @entry[step]{The granularity to the move, as a value of the
          @symbol{gtk:movement-step} enumeration.}
        @entry[count]{The integer with the number of step units to move.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event. @em{False} to propagate the event further.}
      @end{table}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. Applications should not connect to it, but
      may emit it with the @fun{g:signal-emit} function if they need to control
      the cursor programmatically.

      The default bindings for this signal come in two variants, the variant
      with the @kbd{Shift} modifier extends the selection, the variant without
      the @kbd{Shift} modifier does not. There are too many key combinations to
      list them all here. Arrow keys move by individual children.
      @kbd{Home}/@kbd{End} keys move to the ends of the box.
      @kbd{PageUp}/@kbd{PageDown} keys move vertically by pages.
    @subheading{The \"select-all\" signal}
      @begin{pre}
lambda (flowbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[flowbox]{The @class{gtk:flow-box} widget on which the signal is
          emitted.}
      @end{table}
      The signal is a keybinding signal which gets emitted to select all
      children of the box, if the selection mode permits it. The default
      bindings for this signal is the @kbd{Ctrl-a} key.
    @subheading{The \"selected-children-changed\" signal}
      @begin{pre}
lambda (flowbox)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[box]{The @class{gtk:flow-box} on which the signal is emitted.}
      @end{table}
      The signal is emitted when the set of selected children changes. Use the
      @fun{gtk:flow-box-selected-foreach} or
      @fun{gtk:flow-box-selected-children} functions to obtain the selected
      children.
    @subheading{The \"toggle-cursor-child\" signal}
      @begin{pre}
lambda (flowbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[flowbox]{The @class{gtk:flow-box} widget on which the signal is
          emitted.}
      @end{table}
      The signal is a keybinding signal which toggles the selection of the
      child that has the focus. The default binding for this signal is the
      @kbd{Ctrl-Space} key.
    @subheading{The \"unselect-all\" signal}
      @begin{pre}
lambda (flowbox)    :action
      @end{pre}
      @begin[code]{table}
        @entry[flowbox]{The @class{gtk:flow-box} widget on which the signal is
          emitted.}
      @end{table}
      The signal is a keybinding signal which gets emitted to unselect all
      children of the box, if the selection mode permits it. The default
      bindings for this signal is the @kbd{Ctrl-Shift-a} key.
  @end{dictionary}
  @see-constructor{gtk:flow-box-new}
  @see-slot{gtk:flow-box-activate-on-single-click}
  @see-slot{gtk:flow-box-column-spacing}
  @see-slot{gtk:flow-box-homogeneous}
  @see-slot{gtk:flow-box-max-children-per-line}
  @see-slot{gtk:flow-box-min-children-per-line}
  @see-slot{gtk:flow-box-row-spacing}
  @see-slot{gtk:flow-box-selection-mode}
  @see-class{gtk:flow-box-child}
  @see-class{gtk:list-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:flow-box-activate-on-single-click ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activate-on-single-click"
                                               'flow-box) t)
 "The @code{activate-on-single-click} property of type @code{:boolean}
  (Read / Write) @br{}
  Determines whether children can be activated with a single click, or require
  a double click. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-activate-on-single-click)
      "Accessor"
      (documentation 'flow-box-activate-on-single-click 'function)
 "@version{2023-03-05}
  @syntax{(gtk:flow-box-activate-on-single-click object) => setting}
  @syntax{(setf (gtk:flow-box-activate-on-single-click object) setting)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[setting]{@em{false} to emit the @code{\"child-activated\"} signal
    on a single click}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{activate-on-single-click} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-activate-on-single-click} function returns whether
  children activate on single clicks. If the @arg{setting} argument is
  @em{true}, children will be activated when you click on them, otherwise you
  need to double click.
  @see-class{gtk:flow-box}")

;;; --- gtk:flow-box-column-spacing --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-spacing" 'flow-box) t)
 "The @code{column-spacing} property of type @code{:uint} (Read / Write) @br{}
  The amount of horizontal space between two children. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-column-spacing)
      "Accessor"
      (documentation 'flow-box-column-spacing 'function)
 "@version{2025-06-06}
  @syntax{(gtk:flow-box-column-spacing object) => spacing}
  @syntax{(setf (gtk:flow-box-column-spacing object) spacing)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[spacing]{an unsigned integer for the spacing to use}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{column-spacing} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-column-spacing} function gets the horizontal space to
  add between children. The @setf{gtk:flow-box-column-spacing} function sets the
  horizontal spacing.
  @see-class{gtk:flow-box}
  @see-function{gtk:flow-box-row-spacing}")

;;; --- gtk:flow-box-homogeneous -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "homogeneous" 'flow-box) t)
 "The @code{homogeneous} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether all children should be allocated the same size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-homogeneous)
      "Accessor"
      (documentation 'flow-box-homogeneous 'function)
 "@version{2023-03-05}
  @syntax{(gtk:flow-box-homogeneous object) => homogeneous}
  @syntax{(setf (gtk:flow-box-homogeneous object) homogeneous)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[homogeneous]{@em{true} to create equal allotments, @em{false} for
    variable allotments}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{homogeneous} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-homogeneous} function returns whether the flow box is
  homogeneous - all children are the same size. The
  @setf{gtk:flow-box-homogeneous} function sets the property.
  @see-class{gtk:flow-box}")

;;; --- gtk:flow-box-max-children-per-line -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-children-per-line"
                                               'flow-box) t)
 "The @code{max-children-per-line} property of type @code{:uint}
  (Read / Write) @br{}
  The maximum amount of children to request space for consecutively in the
  given orientation. @br{}
  Default value: 7")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-max-children-per-line)
      "Accessor"
      (documentation 'flow-box-max-children-per-line 'function)
 "@version{2025-06-06}
  @syntax{(gtk:flow-box-max-children-per-line object) => n-children}
  @syntax{(setf (gtk:flow-box-max-children-per-line object) n-children)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[n-children]{an unsigned integer for the maximum number of children
    per line}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{max-children-per-line} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-max-children-per-line} function gets the maximum number
  of children per line to request and allocate space for in the orientation of
  the flow box. The @setf{gtk:flow-box-max-children-per-line} function sets the
  maximum number of children.

  Setting the maximum number of children per line limits the overall natural
  size request to be no more than @arg{n-children} children long in the given
  orientation.
  @see-class{gtk:flow-box}
  @see-function{gtk:flow-box-min-children-per-line}")

;;; --- gtk:flow-box-min-children-per-line -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-children-per-line"
                                               'flow-box) t)
 "The @code{min-children-per-line} property of type @code{:uint} (Read / Write)
  @br{}
  The minimum number of children to allocate consecutively in the given
  orientation. Setting the minimum children per line ensures that a reasonably
  small height will be requested for the overall minimum width of the box. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-min-children-per-line)
      "Accessor"
      (documentation 'flow-box-min-children-per-line 'function)
 "@version{2025-06-06}
  @syntax{(gtk:flow-box-min-children-per-line object) => n-children}
  @syntax{(setf (gtk:flow-box-min-children-per-line object) n-children)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[n-children]{an unsigned integer for the minimum number of children
    per line}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{min-children-per-line} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-min-children-per-line} function gets the minimum number
  of children per line in the orientation of the flow box before flowing. The
  @setf{gtk:flow-box-min-children-per-line} function sets the minimum number of
  children per line.
  @see-class{gtk:flow-box}
  @see-function{gtk:flow-box-max-children-per-line}")

;;; --- gtk:flow-box-row-spacing -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-spacing" 'flow-box) t)
 "The @code{row-spacing} property of type @code{:uint} (Read / Write) @br{}
  The amount of vertical space between two children. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-row-spacing)
      "Accessor"
      (documentation 'flow-box-row-spacing 'function)
 "@version{2025-06-06}
  @syntax{(gtk:flow-box-row-spacing object) => spacing}
  @syntax{(setf (gtk:flow-box-row-spacing object) spacing)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[spacing]{an unsigned integer for the spacing to use}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{row-spacing} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-row-spacing} function gets the vertical space to add
  between children. The @setf{gtk:flow-box-row-spacing} function sets the
  vertical spacing.
  @see-class{gtk:flow-box}
  @see-function{gtk:flow-box-column-spacing}")

;;; --- gtk:flow-box-selection-mode --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selection-mode" 'flow-box) t)
 "The @code{selection-mode} property of type @symbol{gtk:selection-mode}
  (Read / Write) @br{}
  The selection mode used by the flow box. @br{}
  Default value: @code{:single}")

#+liber-documentation
(setf (liber:alias-for-function 'flow-box-selection-mode)
      "Accessor"
      (documentation 'flow-box-selection-mode 'function)
 "@version{2024-04-09}
  @syntax{(gtk:flow-box-selection-mode object) => mode}
  @syntax{(setf (gtk:flow-box-selection-mode object) mode)}
  @argument[object]{a @class{gtk:flow-box} widget}
  @argument[mode]{a value of the @symbol{gtk:selection-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:flow-box]{selection-mode} slot of the
    @class{gtk:flow-box} class.
  @end{short}
  The @fun{gtk:flow-box-selection-mode} function gets the selection mode of the
  flow box. The @setf{gtk:flow-box-selection-mode} function sets how selection
  works in the flow box.
  @see-class{gtk:flow-box}
  @see-symbol{gtk:selection-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_new
;;; ----------------------------------------------------------------------------

(declaim (inline flow-box-new))

(defun flow-box-new ()
 #+liber-documentation
 "@version{#2023-03-05}
  @return{The new @class{gtk:flow-box} widget.}
  @begin{short}
    Creates a new flow box.
  @end{short}
  @see-class{gtk:flow-box}"
  (make-instance 'flow-box))

(export 'flow-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_insert" flow-box-insert) :void
 #+liber-documentation
 "@version{#2025-06-06}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[position]{an integer for the position to insert the child widget
    in}
  @begin{short}
    Inserts the child widget into the flow box at a given position.
  @end{short}
  If a sort function is set, the widget will actually be inserted at the
  calculated position and this function has the same effect as the
  @fun{gtk:container-add} function.

  If the @arg{position} argument is -1, or larger than the total number of
  children in the flow box, then the child widget will be appended to the end.
  @see-class{gtk:flow-box}
  @see-class{gtk:widget}
  @see-function{gtk:container-add}"
  (flowbox (g:object flow-box))
  (child (g:object widget))
  (position :int))

(export 'flow-box-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_get_child_at_index" flow-box-child-at-index)
    (g:object flow-box-child)
 #+liber-documentation
 "@version{#2025-06-06}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[index]{an integer for the position of the child widget}
  @begin{return}
    The child widget, which will always be a @class{gtk:flow-box-child} widget
    or @code{nil} in case no child widget with the given index exists.
  @end{return}
  @begin{short}
    Gets the nth child widget in the flow box.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{gtk:flow-box-child}"
  (flowbox (g:object flow-box))
  (index :int))

(export 'flow-box-child-at-index)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_child_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_get_child_at_pos" flow-box-child-at-pos)
    (g:object flow-box-child)
 #+liber-documentation
 "@version{#2025-06-06}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[x]{an integer for the x coordinate of the child widget}
  @argument[y]{an integer for the y coordinate of the child widget}
  @begin{return}
    The child widget, which will always be a @class{gtk:flow-box-child} widget
    or @code{nil} in case no child widget exists for the given coordinates.
  @end{return}
  @begin{short}
    Gets the child widget in the flow box at the given coordinates.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{gtk:flow-box-child}"
  (flowbox (g:object flow-box))
  (x :int)
  (y :int))

(export 'flow-box-child-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_hadjustment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_set_hadjustment" flow-box-set-hadjustment) :void
 #+liber-documentation
 "@version{#2024-4-9}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object which should be adjusted
    when the focus is moved among the descendents of @arg{flowbox}}
  @begin{short}
    Hooks up an adjustment to focus handling in the flow box.
  @end{short}
  The adjustment is also used for autoscrolling during rubberband selection.
  See the @fun{gtk:scrolled-window-hadjustment} function for a typical way of
  obtaining the adjustment, and the @fun{gtk:flow-box-set-vadjustment} function
  for setting the vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the flow box.
  @see-class{gtk:flow-box}
  @see-class{gtk:adjustment}
  @see-function{gtk:flow-box-set-vadjustment}
  @see-function{gtk:scrolled-window-hadjustment}"
  (flowbox (g:object flow-box))
  (adjustment (g:object adjustment)))

(export 'flow-box-set-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_vadjustment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_set_vadjustment" flow-box-set-vadjustment) :void
 #+liber-documentation
 "@version{#2024-04-09}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object which should be adjusted
    when the focus is moved among the descendents of @arg{flowbox}}
  @begin{short}
    Hooks up an adjustment to focus handling in the flow box.
  @end{short}
  The adjustment is also used for autoscrolling during rubberband selection.
  See the @fun{gtk:scrolled-window-vadjustment} function for a typical way of
  obtaining the adjustment, and the @fun{gtk:flow-box-set-hadjustment} function
  for setting the vertical adjustment.

  The adjustments have to be in pixel units and in the same coordinate system
  as the allocation for immediate children of the box.
  @see-class{gtk:flow-box}
  @see-class{gtk:adjustment}
  @see-function{gtk:flow-box-set-hadjustment}
  @see-function{gtk:scrolled-window-vadjustment}"
  (flowbox (g:object flow-box))
  (adjustment (g:object adjustment)))

(export 'flow-box-set-vadjustment)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxForeachFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback flow-box-foreach-func :void
    ((flowbox (g:object flow-box))
     (child (g:object flow-box-child))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func flowbox child)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'flow-box-foreach-func)
      "Callback"
      (liber:symbol-documentation 'flow-box-foreach-func)
 "@version{#2024-03-23}
  @syntax{lambda (flowbox child)}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[child]{a @class{gtk:flow-box-child} child wiget}
  @begin{short}
    A callback function used by the @fun{gtk:flow-box-selected-foreach}
    function.
  @end{short}
  It will be called on every selected child widget of the flow box.
  @see-class{gtk:flow-box}
  @see-class{gtk:flow-box-child}
  @see-function{gtk:flow-box-selected-foreach}")

(export 'flow-box-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_selected_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_selected_foreach" %flow-box-selected-foreach) :void
  (flowbox (g:object flow-box))
  (func :pointer)
  (data :pointer))

(defun flow-box-selected-foreach (flowbox func)
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[func]{a @symbol{gtk:flow-box-foreach-func} callback function}
  @begin{short}
    Calls a function for each selected child widget in the flow box.
  @end{short}
  Note that the selection cannot be modified from within this function.
  @see-class{gtk:flow-box}
  @see-symbol{gtk:flow-box-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%flow-box-selected-foreach flowbox
                                (cffi:callback flow-box-foreach-func)
                                ptr)))

(export 'flow-box-selected-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_get_selected_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_get_selected_children" flow-box-selected-children)
    (g:list-t (g:object flow-box-child))
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @begin{return}
    The list containing the @class{gtk:flow-box-child} child widget for each
    selected child widget.
  @end{return}
  @begin{short}
    Creates a list of all selected children.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{gtk:flow-box-child}"
  (flowbox (g:object flow-box)))

(export 'flow-box-selected-children)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_select_child" flow-box-select-child) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[child]{a @class{gtk:widget} child widget of the flow box}
  @begin{short}
    Selects a single child widget of the flow box, if the selection mode allows
    it.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{gtk:widget}"
  (flowbox (g:object flow-box))
  (child (g:object flow-box-child)))

(export 'flow-box-select-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_unselect_child" flow-box-unselect-child) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[child]{a @class{gtk:widget} child widget of the flow box}
  @begin{short}
    Unselects a single child widget of the flow box, if the selection mode
    allows it.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{gtk:widget}"
  (flowbox (g:object flow-box))
  (child (g:object flow-box-child)))

(export 'flow-box-unselect-child)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_select_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_select_all" flow-box-select-all) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @begin{short}
    Select all children of the flow box, if the selection mode allows it.
  @end{short}
  @see-class{gtk:flow-box}"
  (flowbox (g:object flow-box)))

(export 'flow-box-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_unselect_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_unselect_all" flow-box-unselect-all) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @begin{short}
    Unselect all children of the flow box, if the selection mode allows it.
  @end{short}
  @see-class{gtk:flow-box}"
  (flowbox (g:object flow-box)))

(export 'flow-box-unselect-all)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxFilterFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback flow-box-filter-func :boolean
    ((child (g:object flow-box-child))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func child)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'flow-box-filter-func)
      "Callback"
      (liber:symbol-documentation 'flow-box-filter-func)
 "@version{2024-03-23}
  @syntax{lambda (child) => result}
  @argument[child]{a @class{gtk:flow-box-child} widget that may be filtered}
  @argument[result]{@em{true} if the row should be visible,
    @em{false} otherwise}
  @begin{short}
    A function that will be called whenever a child widget changes or is added.
  @end{short}
  It lets you control if the child widget should be visible or not.
  @see-class{gtk:flow-box}
  @see-class{gtk:flow-box-child}
  @see-function{gtk:flow-box-set-filter-func}")

(export 'flow-box-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_filter_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_set_filter_func" %flow-box-set-filter-func) :void
  (flowbox (g:object flow-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun flow-box-set-filter-func (flowbox func)
 #+liber-documentation
 "@version{2024-01-02}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[func]{a @symbol{gtk:flow-box-filter-func} callback function that
    lets you filter which children to show}
  @begin{short}
    By setting a filter function on the flow box one can decide dynamically
    which of the children to show.
  @end{short}
  For instance, to implement a search function that only shows the children
  matching the search terms.

  The @arg{func} function will be called for each child widget after the call,
  and it will continue to be called each time a child changes, via the
  @fun{gtk:flow-box-child-changed} function or when the
  @fun{gtk:flow-box-invalidate-filter} function is called.

  Note that using a filter function is incompatible with using a model. See
  the @fun{gtk:flow-box-bind-model} function.
  @see-class{gtk:flow-box}
  @see-symbol{gtk:flow-box-filter-func}
  @see-function{gtk:flow-box-child-changed}
  @see-function{gtk:flow-box-invalidate-filter}
  @see-function{gtk:flow-box-bind-model}"
  (%flow-box-set-filter-func
          flowbox
          (cffi:callback flow-box-filter-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'flow-box-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_invalidate_filter" flow-box-invalidate-filter)
    :void
 #+liber-documentation
 "@version{2024-01-02}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @begin{short}
    Updates the filtering for all children in the flow box.
  @end{short}
  Call this function when the result of the filter function on the flow box is
  changed due ot an external factor. For instance, this would be used if the
  filter function just looked for a specific search term, and the entry with
  the string has changed.
  @see-class{gtk:flow-box}"
  (flowbox (g:object flow-box)))

(export 'flow-box-invalidate-filter)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxSortFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback flow-box-sort-func :int
    ((child1 (g:object flow-box-child))
     (child2 (g:object flow-box-child))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func child1 child2)
      (return<0 () :report "Return -1" -1)
      (return=0 () :report "Return  0" 0)
      (return>0 () :report "Return  1" 1))))

#+liber-documentation
(setf (liber:alias-for-symbol 'flow-box-sort-func)
      "Callback"
      (liber:symbol-documentation 'flow-box-sort-func)
 "@version{2024-03-23}
  @syntax{lambda (child1 child2) => result}
  @argument[child1]{a first @class{gtk:flow-box-child} widget}
  @argument[child2]{a second @class{gtk:flow-box-child} widget}
  @argument[result]{< 0 if @arg{child1} should be before @arg{child2}, 0 if the
    are equal, and > 0 otherwise}
  @begin{short}
    A function to compare two children to determine which should come first.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{gtk:flow-box-child}
  @see-function{gtk:flow-box-set-sort-func}")

(export 'flow-box-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_set_sort_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_set_sort_func" %flow-box-set-sort-func) :void
  (flowbox (g:object flow-box))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun flow-box-set-sort-func (flowbox func)
 #+liber-documentation
 "@version{2024-01-02}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[func]{a @symbol{gtk:flow-box-sort-func} callback function for the
    sort function}
  @begin{short}
    By setting a sort function on the flow box, one can dynamically reorder the
    children of the flow box, based on the contents of the children.
  @end{short}

  The @arg{func} function will be called for each child after the call,
  and will continue to be called each time a child changes, via the
  @fun{gtk:flow-box-child-changed} function, and when the
  @fun{gtk:flow-box-invalidate-sort} function is called.

  Note that using a sort function is incompatible with using a model. See
  the @fun{gtk:flow-box-bind-model} function.
  @see-class{gtk:flow-box}
  @see-symbol{gtk:flow-box-sort-func}
  @see-function{gtk:flow-box-child-changed}
  @see-function{gtk:flow-box-invalidate-sort}
  @see-function{gtk:flow-box-bind-model}"
  (%flow-box-set-sort-func flowbox
                           (cffi:callback flow-box-sort-func)
                           (glib:allocate-stable-pointer func)
                           (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'flow-box-set-sort-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_invalidate_sort
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_invalidate_sort" flow-box-invalidate-sort) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @begin{short}
    Updates the sorting for all children in the flow box.
  @end{short}
  Call this when the result of the sort function on the flow box is changed due
  to an external factor.
  @see-class{gtk:flow-box}"
  (flowbox (g:object flow-box)))

(export 'flow-box-invalidate-sort)

;;; ----------------------------------------------------------------------------
;;; GtkFlowBoxCreateWidgetFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback flow-box-create-widget-func (g:object widget)
    ((item :pointer)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func item)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'flow-box-create-widget-func)
      "Callback"
      (liber:symbol-documentation 'flow-box-create-widget-func)
 "@version{#2024-03-23}
  @syntax{lambda (item) => result}
  @argument[item]{a pointer to the item from the model for which to create a
    widget for}
  @argument[result]{a @class{gtk:widget} object that represents @arg{item}}
  @begin{short}
    Called for flow boxes that are bound to a @class{g:list-model} object with
    the @fun{gtk:flow-box-bind-model} function for each item that gets added to
    the model.
  @end{short}
  @see-class{gtk:flow-box}
  @see-class{g:list-model}
  @see-class{gtk:widget}
  @see-function{gtk:flow-box-bind-model}")

(export 'flow-box-create-widget-func)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_bind_model
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_bind_model" %flow-box-bind-model) :void
  (flowbox (g:object flow-box))
  (model (g:object g-list-model))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun flow-box-bind-model (flowbox model func)
 #+liber-documentation
 "@version{#2023-03-13}
  @argument[flowbox]{a @class{gtk:flow-box} widget}
  @argument[model]{a @class{g:list-model} object to be bound to @arg{flowbox}}
  @argument[func]{a @symbol{gtk:flow-box-create-widget-func} callback function
    that creates widgets for items}
  @begin{short}
    Binds a model to the flow box.
  @end{short}
  If the flow box was already bound to a model, that previous binding is
  destroyed.

  The contents of the flow box are cleared and then filled with widgets that
  represent items from the model. The flow box is updated whenever the model
  changes. If the @arg{model} argument is @code{nil}, the flow box is left
  empty.

  It is undefined to add or remove widgets directly, for example, with the
  @fun{gtk:flow-box-insert} or @fun{gtk:container-add} functions, while the
  flow box is bound to a model.

  Note that using a model is incompatible with the filtering and sorting
  functionality in the flow box. When using a model, filtering and sorting
  should be implemented by the model.
  @see-class{gtk:flow-box}
  @see-class{g:list-model}
  @see-symbol{gtk:flow-box-create-widget-func}
  @see-function{gtk:flow-box-insert}
  @see-function{gtk:container-add}"
  (%flow-box-bind-model flowbox
          model
          (cffi:callback flow-box-create-widget-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'flow-box-bind-model)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_new
;;; ----------------------------------------------------------------------------

(declaim (inline flow-box-child-new))

(defun flow-box-child-new ()
 #+liber-documentation
 "@version{#2023-03-05}
  @return{The new @class{gtk:flow-box-child} widget.}
  @begin{short}
    Creates a new @class{gtk:flow-box-child} widget, to be used as a child
    widget of a @class{gtk:flow-box} widget.
  @end{short}
  @see-class{gtk:flow-box-child}
  @see-class{gtk:flow-box}"
  (make-instance 'flow-box-child))

(export 'flow-box-child-new)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_get_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_child_get_index" flow-box-child-index) :int
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[child]{a @class{gtk:flow-box-child} widget}
  @begin{return}
    The integer with the index of the child, or -1 if the child is not in a
    flow box.
  @end{return}
  @begin{short}
    Gets the current index of the child widget in its flow box.
  @end{short}
  @see-class{gtk:flow-box-child}
  @see-class{gtk:flow-box}"
  (child (g:object flow-box-child)))

(export 'flow-box-child-index)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_child_is_selected" flow-box-child-is-selected)
    :boolean
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[child]{a @class{gtk:flow-box-child} widget}
  @return{@em{True} if @arg{child} is selected.}
  @begin{short}
    Returns whether the child widget is currently selected in its flow box.
  @end{short}
  @see-class{gtk:flow-box-child}
  @see-class{gtk:flow-box}"
  (child (g:object flow-box-child)))

(export 'flow-box-child-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_flow_box_child_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_flow_box_child_changed" flow-box-child-changed) :void
 #+liber-documentation
 "@version{#2023-03-05}
  @argument[child]{a @class{gtk:flow-box-child} widget}
  @begin{short}
    Marks the child widget as changed, causing any state that depends on this
    to be updated.
  @end{short}
  This affects sorting and filtering.

  Note that calls to this method must be in sync with the data used for the
  sorting and filtering functions. For instance, if the list is mirroring some
  external data set, and *two* children changed in the external data set when
  you call the @fun{gtk:flow-box-child-changed} function on the first child
  widget, the sort function must only read the new data for the first of the
  two changed children, otherwise the resorting of the children will be wrong.

  This generally means that if you do not fully control the data model, you
  have to duplicate the data that affects the sorting and filtering functions
  into the widgets themselves. Another alternative is to call the
  @fun{gtk:flow-box-invalidate-sort} function on any model change, but that is
  more expensive.
  @see-class{gtk:flow-box-child}
  @see-class{gtk:flow-box}
  @see-function{gtk:flow-box-invalidate-sort}"
  (child (g:object flow-box-child)))

(export 'flow-box-child-changed)

;;; --- End of file gtk3.flow-box.lisp -----------------------------------------
