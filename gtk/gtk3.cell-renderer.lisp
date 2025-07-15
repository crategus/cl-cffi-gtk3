;;; ----------------------------------------------------------------------------
;;; gtk3.cell-renderer.lisp
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
;;; GtkCellRenderer
;;;
;;;     An object for rendering a single cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererState
;;;     GtkCellRendererMode
;;;     GtkCellRenderer
;;;
;;; Accessors
;;;
;;;     gtk_cell_renderer_get_sensitive
;;;     gtk_cell_renderer_set_sensitive
;;;     gtk_cell_renderer_get_visible
;;;     gtk_cell_renderer_set_visible
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_class_set_accessible_type         not implemented
;;;     gtk_cell_renderer_get_aligned_area
;;;     gtk_cell_renderer_get_size
;;;     gtk_cell_renderer_render
;;;     gtk_cell_renderer_activate
;;;     gtk_cell_renderer_start_editing
;;;     gtk_cell_renderer_stop_editing
;;;     gtk_cell_renderer_get_fixed_size
;;;     gtk_cell_renderer_set_fixed_size
;;;     gtk_cell_renderer_get_alignment
;;;     gtk_cell_renderer_set_alignment
;;;     gtk_cell_renderer_get_padding
;;;     gtk_cell_renderer_set_padding
;;;     gtk_cell_renderer_get_state
;;;     gtk_cell_renderer_is_activatable
;;;     gtk_cell_renderer_get_preferred_height
;;;     gtk_cell_renderer_get_preferred_height_for_width
;;;     gtk_cell_renderer_get_preferred_size
;;;     gtk_cell_renderer_get_preferred_width
;;;     gtk_cell_renderer_get_preferred_width_for_height
;;;     gtk_cell_renderer_get_request_mode
;;;
;;; Properties
;;;
;;;     cell-background
;;;     cell-background-gdk
;;;     cell-background-rgba
;;;     cell-background-set
;;;     editing
;;;     height
;;;     is-expanded
;;;     is-expander
;;;     mode
;;;     sensitive
;;;     visible
;;;     width
;;;     xalign
;;;     xpad
;;;     yalign
;;;     ypad
;;;
;;; Signals
;;;
;;;     editing-canceled
;;;     editing-started
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ├── GtkCellRendererText
;;;             ├── GtkCellRendererPixbuf
;;;             ├── GtkCellRendererProgress
;;;             ├── GtkCellRendererSpinner
;;;             ╰── GtkCellRendererToggle
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellRendererState
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkCellRendererState" cell-renderer-state
  (:export t
   :type-initializer "gtk_cell_renderer_state_get_type")
  (:selected    #.(ash 1 0))
  (:prelit      #.(ash 1 1))
  (:insensitive #.(ash 1 2))
  (:sorted      #.(ash 1 3))
  (:focused     #.(ash 1 4))
  (:expandable  #.(ash 1 5))
  (:expanded    #.(ash 1 6)))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-renderer-state)
      "GFlags"
      (liber:symbol-documentation 'cell-renderer-state)
 "@version{2024-03-15}
  @begin{declaration}
(gobject:define-gflags \"GtkCellRendererState\" cell-renderer-state
  (:export t
   :type-initializer \"gtk_cell_renderer_state_get_type\")
  (:selected    #.(ash 1 0))
  (:prelit      #.(ash 1 1))
  (:insensitive #.(ash 1 2))
  (:sorted      #.(ash 1 3))
  (:focused     #.(ash 1 4))
  (:expandable  #.(ash 1 5))
  (:expanded    #.(ash 1 6)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:selected]{The cell is currently selected, and probably has a
        selection colored background to render to.}
      @entry[:prelit]{The mouse is hovering over the cell.}
      @entry[:insensitive]{The cell is drawn in an insensitive manner.}
      @entry[:sorted]{The cell is in a sorted row.}
      @entry[:focused]{The cell is in the focus row.}
      @entry[:expandable]{The cell is in a row that can be expanded.}
      @entry[:expanded]{The cell is in a row that is expanded.}
    @end{simple-table}
  @end{values}
  @short{Tells how a cell is to be rendererd.}
  @see-class{gtk:cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; GtkCellRendererMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkCellRendererMode" cell-renderer-mode
  (:export t
   :type-initializer "gtk_cell_renderer_mode_get_type")
  (:inert 0)
  (:activatable 1)
  (:editable 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-renderer-mode)
      "GEnum"
      (liber:symbol-documentation 'cell-renderer-mode)
 "@version{2025-07-05}
  @begin{declaration}
(gobject:define-genum \"GtkCellRendererMode\" cell-renderer-mode
  (:export t
   :type-initializer \"gtk_cell_renderer_mode_get_type\")
  (:inert 0)
  (:activatable 1)
  (:editable 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:inert]{The cell is just for display and cannot be interacted with.
        Note that this does not mean that, for example, the row being drawn
        cannot be selected - just that a particular element of it cannot be
        individually modified.}
      @entry[:activatable]{The cell can be clicked.}
      @entry[:editable]{The cell can be edited or otherwise modified.}
    @end{simple-table}
  @end{values}
  @short{Identifies how the user can interact with a particular cell.}
  @see-class{gtk:cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; GtkCellRenderer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCellRenderer" cell-renderer
  (:superclass g:initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_get_type")
  ((cell-background
    cell-renderer-cell-background
    "cell-background" "gchararray" nil t)
   (cell-background-gdk
    cell-renderer-cell-background-gdk
    "cell-background-gdk" "GdkColor" t t)
   (cell-background-rgba
    cell-renderer-cell-background-rgba
    "cell-background-rgba" "GdkRGBA" t t)
   (cell-background-set
    cell-renderer-cell-background-set
    "cell-background-set" "gboolean" t t)
   (editing
    cell-renderer-editing
    "editing" "gboolean" t nil)
   (height
    cell-renderer-height
    "height" "gint" t t)
   (is-expanded
    cell-renderer-is-expanded
    "is-expanded" "gboolean" t t)
   (is-expander
    cell-renderer-is-expander
    "is-expander" "gboolean" t t)
   (mode
    cell-renderer-mode
    "mode" "GtkCellRendererMode" t t)
   (sensitive
    cell-renderer-sensitive
    "sensitive" "gboolean" t t)
   (visible
    cell-renderer-visible
    "visible" "gboolean" t t)
   (width
    cell-renderer-width
    "width" "gint" t t)
   (xalign
    cell-renderer-xalign
    "xalign" "gfloat" t t)
   (xpad
    cell-renderer-xpad
    "xpad" "guint" t t)
   (yalign
    cell-renderer-yalign
    "yalign" "gfloat" t t)
   (ypad
    cell-renderer-ypad
    "ypad" "guint" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer 'type)
 "@version{2025-07-05}
  @begin{short}
    The @class{gtk:cell-renderer} class is a base class of a set of objects used
    for rendering a cell to a @sym{cairo:context-t} context.
  @end{short}
  These objects are used primarily by the @class{gtk:tree-view} widget, though
  they are not tied to them in any specific way. It is worth noting that the
  @class{gtk:cell-renderer} object is not a @class{gtk:widget} object and cannot
  be treated as such.

  The primary use of a @class{gtk:cell-renderer} object is for drawing a certain
  graphical elements on a Cairo context. Typically, one cell renderer is used
  to draw many cells on the screen. To this extent, it is not expected that a
  @class{gtk:cell-renderer} object keep any permanent state around. Instead, any
  state is set just prior to use using GObjects property system. Then, the cell
  is measured using the @fun{gtk:cell-renderer-preferred-size} function.
  Finally, the cell is rendered in the correct location using the
  @fun{gtk:cell-renderer-render} function.

  There are a number of rules that must be followed when writing a new
  @class{gtk:cell-renderer} class. First and formost, its important that a
  certain set of properties will always yield a cell renderer of the same size,
  barring a @code{GtkStyle} change. The @class{gtk:cell-renderer} class also has
  a number of generic properties that are expected to be honored by all
  children.

  Beyond merely rendering a cell, cell renderers can optionally provide active
  user interface elements. A cell renderer can be \"activatable\" like the
  @class{gtk:cell-renderer-toggle} object, which toggles when it gets activated
  by a mouse click, or it can be \"editable\" like the
  @class{gtk:cell-renderer-text} object, which allows the user to edit the text
  using a @class{gtk:entry} widget. To make a cell renderer activatable or
  editable, you have to implement the @code{GtkCellRendererClass.activate} or
  @code{GtkCellRendererClass.start_editing} virtual functions, respectively.

  Many properties of the @class{gtk:cell-renderer} class and its subclasses have
  a corresponding @code{set} property, for example, the
  @code{cell-background-set} property corresponds to the @code{cell-background}
  property. These @code{set} properties reflect whether a property has been set
  or not. You should not set them independently.
  @begin[Signal Details]{dictionary}
    @begin[cell-renderer::editing-canceled]{signal}
      @begin{pre}
lambda (renderer)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[renderer]{The @class{gtk:cell-renderer} object which received
        the signal.}
      @end{simple-table}
      The signal gets emitted when the user cancels the process of editing a
      cell. For example, an editable cell renderer could be written to cancel
      editing when the user presses the @kbd{Escape} key. See also the
      @fun{gtk:cell-renderer-stop-editing} function.
    @end{signal}
    @begin[cell-renderer::editing-started]{signal}
      @begin{pre}
lambda (renderer editable path)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[renderer]{The @class{gtk:cell-renderer} object which received
          the signal.}
        @entry[editable]{The @class{gtk:cell-editable} widget.}
        @entry[path]{The string with the path identifying the edited cell.}
      @end{simple-table}
      The signal gets emitted when a cell starts to be edited. The intended
      use of this signal is to do special setup on editable, for example, adding
      a @class{gtk:entry-completion} object or setting up additional columns in
      a @class{gtk:combo-box} widget. Note that GTK does not guarantee that cell
      renderers will continue to use the same kind of widget for editing in
      future releases, therefore you should check the type of the @arg{editable}
      argument before doing any specific setup.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:cell-renderer-cell-background}
  @see-slot{gtk:cell-renderer-cell-background-gdk}
  @see-slot{gtk:cell-renderer-cell-background-rgba}
  @see-slot{gtk:cell-renderer-cell-background-set}
  @see-slot{gtk:cell-renderer-editing}
  @see-slot{gtk:cell-renderer-height}
  @see-slot{gtk:cell-renderer-is-expanded}
  @see-slot{gtk:cell-renderer-is-expander}
  @see-slot{gtk:cell-renderer-mode}
  @see-slot{gtk:cell-renderer-sensitive}
  @see-slot{gtk:cell-renderer-visible}
  @see-slot{gtk:cell-renderer-width}
  @see-slot{gtk:cell-renderer-xalign}
  @see-slot{gtk:cell-renderer-xpad}
  @see-slot{gtk:cell-renderer-yalign}
  @see-slot{gtk:cell-renderer-ypad}
  @see-class{gtk:cell-editable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-renderer-cell-background --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-background"
                                               'cell-renderer) t)
 "The @code{cell-background} property of type @code{:string} (Write) @br{}
  Cell background color as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-cell-background)
      "Accessor"
      (documentation 'cell-renderer-cell-background 'function)
 "@version{2025-07-06}
  @syntax{(setf (gtk:cell-renderer-cell-background object) background)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[background]{a string for the cell background color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{cell-background} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Cell background color as a string. This property is not readable. After
  setting the background color is readable with the
  @fun{gtk:cell-renderer-cell-background-rgba} function.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-cell-background-rgba}")

;;; --- gtk:cell-renderer-cell-background-gdk ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-background-gdk"
                                               'cell-renderer) t)
 "The @code{cell-background-gdk} property of type @class{gdk:color}
  (Read / Write) @br{}
  Cell background color. @br{}
  @em{Warning:} The @code{cell-background-gdk} property has been deprecated
  since version 3.4 and should not be used in newly written code. Use the
  @code{cell-background-rgba} property instead.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-cell-background-gdk)
      "Accessor"
      (documentation 'cell-renderer-cell-background-gdk 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-cell-background-gdk object) => background}
  @syntax{(setf (gtk:cell-renderer-cell-background-gdk object) background)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[background]{a @class{gdk:color} color for the cell background
    color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{cell-background-gdk} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Cell background color.
  @begin[Warning]{dictionary}
    The @fun{gtk:cell-renderer-cell-background-gdk} function has been deprecated
    since version 3.4 and should not be used in newly written code. Use the
    @fun{gtk:cell-renderer-cell-background-rgba} function instead.
  @end{dictionary}
  @see-class{gtk:cell-renderer}
  @see-class{gdk:color}
  @see-function{gtk:cell-renderer-cell-background-rgba}")

;;; --- gtk:cell-renderer-cell-background-rgba ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-background-rgba"
                                               'cell-renderer) t)
 "The @code{cell-background-rgba} property of type @class{gdk:rgba}
  (Read / Write) @br{}
  Cell background RGBA color.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-cell-background-rgba)
      "Accessor"
      (documentation 'cell-renderer-cell-background-rgba 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-cell-background-rgba object) => background}
  @syntax{(setf (gtk:cell-renderer-cell-background-rgba object) background)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[background]{a @class{gdk:rgba} color for the cell background color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{cell-background-rgba} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Cell background RGBA color.
  @see-class{gtk:cell-renderer}
  @see-class{gdk:rgba}
  @see-function{gtk:cell-renderer-cell-background-set}")

;;; --- gtk:cell-renderer-cell-background-set ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-background-set"
                                               'cell-renderer) t)
 "The @code{cell-background-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the cell background color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-cell-background-set)
      "Accessor"
      (documentation 'cell-renderer-cell-background-set 'function)
 "@version{2024-03-15}
  @syntax{(gtk:cell-renderer-cell-background-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-cell-background-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[setting]{a boolean whether this tag affects the cell background
    color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{cell-background-set} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Whether this tag affects the cell background color.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-cell-background}
  @see-function{gtk:cell-renderer-cell-background-rgba}")

;;; --- gtk:cell-renderer-editing ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editing" 'cell-renderer) t)
 "The @code{editing} property of type @code{:boolean} (Read) @br{}
  Whether the cell renderer is currently in editing mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-editing)
      "Accessor"
      (documentation 'cell-renderer-editing 'function)
 "@version{2024-03-15}
  @syntax{(gtk:cell-renderer-editing object) => setting}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[setting]{a boolean whether the cell renderer is in editing mode}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{editing} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Whether the cell renderer is currently in editing mode.
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-renderer-height -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'cell-renderer) t)
 "The @code{height} property of type @code{:int} (Read / Write) @br{}
  The fixed height. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-height)
      "Accessor"
      (documentation 'cell-renderer-height 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-height object) => height}
  @syntax{(setf (gtk:cell-renderer-height object) height)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[height]{an integer for the fixed height}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{height} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The fixed height.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-width}")

;;; --- gtk:cell-renderer-is-expanded ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-expanded" 'cell-renderer) t)
 "The @code{is-expanded} property of type @code{:boolean} (Read / Write) @br{}
  Row is an expander row, and is expanded. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-is-expanded)
      "Accessor"
      (documentation 'cell-renderer-is-expanded 'function)
 "@version{2024-03-15}
  @syntax{(gtk:cell-renderer-is-expanded object) => setting}
  @syntax{(setf (gtk:cell-renderer-is-expanded object) setting)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[setting]{a boolean whether the row is expanded}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{is-expanded} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Row is an expander row, and is expanded.
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-renderer-is-expander ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-expander" 'cell-renderer) t)
 "The @code{is-expander} property of type @code{:boolean} (Read / Write) @br{}
  Row has children. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-is-expander)
      "Accessor"
      (documentation 'cell-renderer-is-expander 'function)
 "@version{2024-03-15}
  @syntax{(gtk:cell-renderer-is-expander object) => setting}
  @syntax{(setf (gtk:cell-renderer-is-expander object) setting)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[setting]{a boolean whether the row has children}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{is-expander} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Row has children.
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-renderer-mode -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'cell-renderer) t)
 "The @code{mode} property of type @sym{gtk:cell-renderer-mode} (Read / Write)
  @br{}
  Editable mode of the cell renderer. @br{}
  Default value: @val[gtk:cell-renderer-mode]{:inert}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-mode)
      "Accessor"
      (documentation 'cell-renderer-mode 'function)
 "@version{2025-07-07}
  @syntax{(gtk:cell-renderer-mode object) => mode}
  @syntax{(setf (gtk:cell-renderer-mode object) mode)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[mode]{a value of the @sym{gtk:cell-renderer-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{mode} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  Editable mode of the cell renderer.
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:cell-renderer-mode}")

;;; --- gtk:cell-renderer-sensitive --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sensitive" 'cell-renderer) t)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Display the cell sensitive. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-sensitive)
      "Accessor"
      (documentation 'cell-renderer-sensitive 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-sensitive object) => sensitive}
  @syntax{(setf (gtk:cell-renderer-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[sensitive]{a boolean for the sensitivity of the cell}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{sensitive} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The @fun{gtk:cell-renderer-sensitive} function returns the cell renderer's
  sensitivity. The @setf{gtk:cell-renderer-sensitive} function sets the
  sensitivity.
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-renderer-visible ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'cell-renderer) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Display the cell. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-visible)
      "Accessor"
      (documentation 'cell-renderer-visible 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-visible object) => visible}
  @syntax{(setf (gtk:cell-renderer-visible object) visible)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[visible]{a boolean for the visibility of the cell}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{visible} of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The @fun{gtk:cell-renderer-sensitive} function returns the cell renderer's
  visibility. The @setf{gtk:cell-renderer-sensitive} function sets the
  visibility.
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-renderer-width ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width" 'cell-renderer) t)
 "The @code{width} property of type @code{:int} (Read / Write) @br{}
  The fixed width. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-width)
      "Accessor"
      (documentation 'cell-renderer-width 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-width object) => width}
  @syntax{(setf (gtk:cell-renderer-width object) width)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[width]{an integer for the fixed width}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{width} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The fixed width.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-height}")

;;; --- gtk:cell-renderer-xalign -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'cell-renderer) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for
  RTL layouts. @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-xalign)
      "Accessor"
      (documentation 'cell-renderer-xalign 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-xalign object) => align}
  @syntax{(setf (gtk:cell-renderer-xalign object) align)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[align]{a number coerced to a single float for the x alignment}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{xalign} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for RTL
  layouts.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-yalign}")

;;; --- gtk:cell-renderer-xpad -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xpad" 'cell-renderer) t)
 "The @code{xpad} property of type @code{:uint} (Read / Write) @br{}
  The amount of space to add on the left and right, in pixels. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-xpad)
      "Accessor"
      (documentation 'cell-renderer-xpad 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-xpad object) => padding}
  @syntax{(setf (gtk:cell-renderer-xpad object) padding)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[padding]{a unsigned integer for the padding}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{xpad} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The amount of space to add on the left and right, in pixels.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-ypad}")

;;; --- gtk:cell-renderer-yalign -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'cell-renderer) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The vertical alignment, from 0.0 (top) to 1.0 (bottom). @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-yalign)
      "Accessor"
      (documentation 'cell-renderer-yalign 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-yalign object) => align}
  @syntax{(setf (gtk:cell-renderer-yalign object) align)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[align]{a number coerced to a single float for the y alignment}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{yalign} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The vertical alignment, from 0.0 (top) to 1.0 (bottom).
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-xalign}")

;;; --- gtk:cell-renderer-ypad -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ypad" 'cell-renderer) t)
 "The @code{ypad} property of tpye @code{:uint} (Read / Write) @br{}
  The amount of space to add on the top and bottom, in pixels. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-ypad)
      "Accessor"
      (documentation 'cell-renderer-ypad 'function)
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-ypad object) => padding}
  @syntax{(setf (gtk:cell-renderer-ypad object) padding)}
  @argument[object]{a @class{gtk:cell-renderer} object}
  @argument[padding]{an unsigned integer for the padding}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{ypad} slot of the
    @class{gtk:cell-renderer} class.
  @end{short}
  The amount of space to add on the top and bottom, in pixels.
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-renderer-xpad}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_class_set_accessible_type ()
;;;
;;; void
;;; gtk_cell_renderer_class_set_accessible_type
;;;                                (GtkCellRendererClass *renderer_class,
;;;                                 GType type);
;;;
;;; Sets the type to be used for creating accessibles for cells rendered by cell
;;; renderers of renderer_class . Note that multiple accessibles will be
;;; created.
;;;
;;; This function should only be called from class init functions of cell
;;; renderers.
;;;
;;; renderer_class :
;;;     class to set the accessible type for
;;;
;;; type :
;;;     The object type that implements the accessible for widget_class . The
;;;     type must be a subtype of GtkRendererCellAccessible
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_aligned_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_aligned_area" %cell-renderer-aligned-area)
    :void
  (cell (g:object cell-renderer))
  (widget (g:object widget))
  (flags cell-renderer-state)
  (area (g:boxed gdk:rectangle))
  (aligned (g:boxed gdk:rectangle)))

(defun cell-renderer-aligned-area (renderer widget flags area)
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object this cell renderer will be
    rendering to}
  @argument[flags]{a @sym{gtk:cell-renderer-state} value for the render flags}
  @argument[area]{a @class{gdk:rectangle} instance  for the cell area which
    would be passed to the @fun{gtk:cell-renderer-render} function}
  @begin{return}
    The @class{gdk:rectangle} area for the space inside @arg{area} that would
    acually be used to render.
  @end{return}
  @begin{short}
    Gets the aligned area used by the cell renderer inside @arg{area}.
  @end{short}
  Used for finding the appropriate edit and focus rectangle.
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:cell-renderer-state}
  @see-function{gtk:cell-renderer-render}"
  (let ((aligned (gdk:rectangle-new)))
    (%cell-renderer-aligned-area renderer widget flags area aligned)
    aligned))

(export 'cell-renderer-aligned-area)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_size" %cell-renderer-size) :void
  (cell (g:object cell-renderer))
  (widget (g:object widget))
  (area (g:boxed gdk:rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun cell-renderer-size (cell widget area)
 #+liber-documentation
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-size cell widget area) => xoffset, yoffset,
    width, height}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object the cell renderer is rendering
    to}
  @argument[area]{a @class{gdk:rectangle} instance for the area a cell will be
    allocated, or @code{nil}}
  @argument[xoffset]{an integer for the x offset of cell relative to @arg{area},
    or @code{nil}}
  @argument[yoffset]{an integer for the y offset of cell relative to @arg{area},
    or @code{nil}}
  @argument[width]{an integer for the width needed to render a cell, or
    @code{nil}}
  @argument[height]{an integer for the height needed to render a cell, or
    @code{nil}}
  @begin{short}
    Obtains the width and height needed to render the cell.
  @end{short}
  Used by tree view widgets to determine the appropriate size for the
  cell area passed to the @fun{gtk:cell-renderer-render} function. If the
  @arg{area} argument is not @code{nil}, fills in the x and y offsets (if set)
  of the cell relative to this location.

  Please note that the values set in @arg{width} and @arg{height}, as well as
  those in @arg{xoffset} and @arg{yoffset} are inclusive of the
  @slot[gtk:cell-renderer]{xpad} and @slot[gtk:cell-renderer]{ypad} properties.
  @begin[Warning]{dictionary}
    The @fun{gtk:cell-renderer-size} function has been deprecated since version
    3.0 and should not be used in newly written code. Use the
    @fun{gtk:cell-renderer-preferred-size} function instead.
  @end{dictionary}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-function{gtk:cell-renderer-render}
  @see-function{gtk:cell-renderer-preferred-size}"
  (cffi:with-foreign-objects ((xoffset :int)
                              (yoffset :int)
                              (width :int)
                              (height :int))
    (%cell-renderer-size cell
                         widget
                         area
                         xoffset
                         yoffset
                         width
                         height)
    (values (cffi:mem-ref xoffset :int)
            (cffi:mem-ref yoffset :int)
            (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'cell-renderer-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_render
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_render" cell-renderer-render) :void
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[cr]{a @sym{cairo:context-t} instance for the context to draw to}
  @argument[widget]{a @class{gtk:widget} object owning the window}
  @argument[background]{a @class{gdk:rectangle} instance for entire cell area
    including tree expanders and maybe padding on the sides}
  @argument[area]{a @class{gdk:rectangle} instance for the area normally
    rendered by a cell renderer}
  @argument[flags]{a @sym{gtk:cell-renderer-state} value that affect
    rendering}
  @begin{short}
    Invokes the virtual render function of the cell renderer.
  @end{short}
  The passed-in rectangles are areas in @arg{cr}. Most renderers will draw
  within @arg{area}. The @slot[gtk:cell-renderer]{xalign},
  @slot[gtk:cell-renderer]{yalign}, @slot[gtk:cell-renderer]{xpad}, and
  @slot[gtk:cell-renderer]{ypad} properties of the cell renderer should be
  honored with respect to @arg{area}. The @arg{background} argument includes
  the blank space around the cell, and also the area containing the tree
  expander. So the @arg{background} rectangles for all cells tile to cover the
  entire window.
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:cell-renderer-state}"
  (renderer (g:object cell-renderer))
  (cr (:pointer (:struct cairo:context-t)))
  (widget (g:object widget))
  (background (g:boxed gdk:rectangle))
  (area (g:boxed gdk:rectangle))
  (flags cell-renderer-state))

(export 'cell-renderer-render)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_activate" cell-renderer-activate) :boolean
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[event]{a @class{gdk:event} event}
  @argument[widget]{a @class{gtk:widget} object that received the event}
  @argument[path]{a string for the widget dependent representation of the event
    location, for example for the a @class{gtk:tree-view} widget, a string
    representation of a @class{gtk:tree-path} instance}
  @argument[background]{a @class{gdk:rectangle} instance for the background
    area as passed to the @fun{gtk:cell-renderer-render} function}
  @argument[area]{a @class{gdk:rectangle} instance for the cell area as passed
    to the @fun{gtk:cell-renderer-render} function}
  @argument[flags]{a @sym{gtk:cell-renderer-state} value}
  @return{@em{True} if the event was consumed/handled.}
  @begin{short}
    Passes an activate event to the cell renderer for possible processing.
  @end{short}
  Some cell renderers may use events. For example, the
  @class{gtk:cell-renderer-toggle} object toggles when it gets a mouse click.
  @see-class{gtk:cell-renderer}
  @see-class{gtk:cell-renderer-toggle}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-class{gdk:event}
  @see-symbol{gtk:cell-renderer-state}
  @see-function{gtk:cell-renderer-render}"
  (renderer (g:object cell-renderer))
  (event (g:boxed gdk:event))
  (widget (g:object widget))
  (path :string)
  (background (g:boxed gdk:rectangle))
  (area (g:boxed gdk:rectangle))
  (flags cell-renderer-state))

(export 'cell-renderer-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_start_editing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_start_editing" cell-renderer-start-editing)
    (g:object cell-editable)
 #+liber-documentation
 "@version{#2025-07-05}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[event]{a @class{gdk:event} event}
  @argument[widget]{a @class{gtk:widget} object that received the event}
  @argument[path]{a string for the widget dependent representation of the event
    location, for example for the @class{gtk:tree-view} widget, a string
    representation of a @class{gtk:tree-path} instance}
  @argument[background]{a @class{gdk:rectangle} instance for the background
    area as passed to the @fun{gtk:cell-renderer-render} function}
  @argument[area]{a @class{gdk:rectangle} instance for the cell area as passed
    to the @fun{gtk:cell-renderer-render} function}
  @argument[flags]{a @sym{gtk:cell-renderer-state} render flags}
  @return{The new @class{gtk:cell-editable} widget, or @code{nil}.}
  @begin{short}
    Passes an activate event to the cell renderer for possible processing.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-class{gdk:event}
  @see-class{gtk:cell-editable}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-path}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:cell-renderer-state}
  @see-function{gtk:cell-renderer-render}"
  (renderer (g:object cell-renderer))
  (event (g:boxed gdk:event))
  (widget (g:object widget))
  (path :string)
  (background (g:boxed gdk:rectangle))
  (area (g:boxed gdk:rectangle))
  (flags cell-renderer-state))

(export 'cell-renderer-start-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_stop_editing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_stop_editing" cell-renderer-stop-editing)
    :void
 #+liber-documentation
 "@version{#2024-03-15}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[canceled]{@em{true} if the editing has been canceled}
  @begin{short}
    Informs the cell renderer that the editing is stopped.
  @end{short}
  If the @arg{canceled} argument is @em{true}, the cell renderer will emit the
  @sig[gtk:cell-renderer]{editing-canceled} signal. This function should be
  called by cell renderer implementations in response to the
  @sig[gtk:cell-editable]{editing-done} signal of the @class{gtk:cell-editable}
  widget.
  @see-class{gtk:cell-renderer}
  @see-class{gtk:cell-editable}
  @see-function{gtk:cell-renderer-start-editing}"
  (renderer (g:object cell-renderer))
  (canceled :boolean))

(export 'cell-renderer-stop-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_fixed_size
;;; gtk_cell_renderer_set_fixed_size
;;; ----------------------------------------------------------------------------

(defun (setf cell-renderer-fixed-size) (value renderer)
  (destructuring-bind (width height) value
    (cffi:foreign-funcall "gtk_cell_renderer_set_fixed_size"
                          (g:object cell-renderer) renderer
                          :int width
                          :int height
                          :void)
    (values width height)))

(cffi:defcfun ("gtk_cell_renderer_get_fixed_size" %cell-renderer-fixed-size)
    :void
  (renderer (g:object cell-renderer))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun cell-renderer-fixed-size (renderer)
 #+liber-documentation
 "@version{#2025-07-06}
  @syntax{(gtk:cell-renderer-fixed-size renderer) => width, height}
  @syntax{(setf (gtk:cell-renderer-fixe-size renderer) (list width height))}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[width]{an integer for the width of the cell renderer, or -1}
  @argument[height]{an integer for the height of the cell renderer, or -1}
  @begin{short}
    The @fun{gtk:cell-renderer-fixed-size} function returns @arg{width} and
    @arg{height} with the appropriate size of @arg{renderer}.
  @end{short}
  The @setf{gtk:cell-renderer-fixed-size} function sets the renderer size to be
  explicit, independent of the properties set.
  @see-class{gtk:cell-renderer}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%cell-renderer-fixed-size renderer width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'cell-renderer-fixed-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_alignment
;;; gtk_cell_renderer_set_alignment
;;; ----------------------------------------------------------------------------

(defun (setf cell-renderer-alignment) (value renderer)
  (destructuring-bind (xalign yalign) value
    (let ((xalign (coerce xalign 'single-float))
          (yalign (coerce yalign 'single-float)))
      (cffi:foreign-funcall "gtk_cell_renderer_set_alignment"
                            (g:object cell-renderer) renderer
                            :float xalign
                            :float yalign
                            :void)
       (values xalign yalign))))

(cffi:defcfun ("gtk_cell_renderer_get_alignment" %cell-renderer-alignment) :void
  (renderer (g:object cell-renderer))
  (xalign (:pointer :float))
  (yalign (:pointer :float)))

(defun cell-renderer-alignment (renderer)
 #+liber-documentation
 "@version{2025-07-06}
  @syntax{(gtk:cell-renderer-alignment renderer) => xalign, yalign}
  @syntax{(setf (gtk:cell-renderer-alignment renderer) (list xalign yalign))}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[xalign]{a single float for the x alignment of the cell renderer}
  @argument[yalign]{a single float for the y alignment of the cell renderer}
  @begin{short}
    The @fun{gtk:cell-renderer-alignment} function returns the appropriate
    @arg{xalign} and @arg{yalign} values of the cell renderer.
  @end{short}
  The @setf{gtk:cell-renderer-alignment} function sets the alignment of the
  cell renderer within its available space.The @arg{xalign} and @arg{yalign}
  values are coerced to single floats before assignment.
  @see-class{gtk:cell-renderer}"
  (cffi:with-foreign-objects ((xalign :float) (yalign :float))
    (%cell-renderer-alignment renderer xalign yalign)
    (values (cffi:mem-ref xalign :float)
            (cffi:mem-ref yalign :float))))

(export 'cell-renderer-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_padding
;;; gtk_cell_renderer_set_padding
;;; ----------------------------------------------------------------------------

(defun (setf cell-renderer-padding) (value renderer)
  (destructuring-bind (xpad ypad) value
    (cffi:foreign-funcall "gtk_cell_renderer_set_padding"
                          (g:object cell-renderer) renderer
                          :int xpad
                          :int ypad
                          :void)
     (values xpad ypad)))

(cffi:defcfun ("gtk_cell_renderer_get_padding" %cell-renderer-padding) :void
  (renderer (g:object cell-renderer))
  (xpad (:pointer :int))
  (ypad (:pointer :int)))

(defun cell-renderer-padding (renderer)
 #+liber-documentation
 "@version{#2025-07-06}
  @syntax{(gtk:cell-renderer-padding renderer) => xpad, ypad}
  @syntax{(setf gtk:cell-renderer-padding renderer) (list xpad ypad))}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[xpad]{an integer for the x padding of the cell renderer}
  @argument[ypad]{an integer for the y padding of the cell renderer}
  @begin{short}
    The @fun{gtk:cell-renderer-padding} function returns the appropriate
    @arg{xpad} and @arg{ypad} of the cell renderer.
  @end{short}
  The @setf{gtk:cell-renderer-padding} function sets the padding of the cell
  renderer.
  @see-class{gtk:cell-renderer}"
  (cffi:with-foreign-objects ((xpad :int) (ypad :int))
    (%cell-renderer-padding renderer xpad ypad)
    (values (cffi:mem-ref xpad :int)
            (cffi:mem-ref ypad :int))))

(export 'cell-renderer-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_state" cell-renderer-state) state-flags
 #+liber-documentation
 "@version{#2025-07-06}
  @argument[renderer]{a @class{gtk:cell-renderer}, or @code{nil}}
  @argument[widget]{a @class{gtk:widget} object, or @code{nil}}
  @argument[state]{a @sym{gtk:cell-renderer-state} value for the cell renderer
    state}
  @begin{return}
    The @sym{gtk:state-flags} value for the state flags applying to the cell
    renderer.
  @end{return}
  @begin{short}
  Translates the cell renderer state to @sym{gtk:state-flags} flags, based
  on the cell renderer and widget sensitivity, and the given
  @sym{gtk:cell-renderer-state} flags.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-symbol{gtk:state-flags}
  @see-symbol{gtk:cell-renderer-state}"
  (renderer (g:object cell-renderer))
  (widget (g:object widget))
  (state cell-renderer-state))

(export 'cell-renderer-state)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_is_activatable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_is_activatable" cell-renderer-is-activatable)
    :boolean
 #+liber-documentation
 "@version{#2024-03-15}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @return{@em{True} if the cell renderer can do anything when activated.}
  @begin{short}
    Checks whether the cell renderer can do something when activated.
  @end{short}
  @see-class{gtk:cell-renderer}"
  (renderer (g:object cell-renderer)))

(export 'cell-renderer-is-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_preferred_height"
          %cell-renderer-preferred-height) :void
  (renderer (g:object cell-renderer))
  (widget (g:object widget))
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-renderer-preferred-height (renderer widget)
 #+liber-documentation
 "@version{#2025-07-14}
  @syntax{(gtk:cell-renderer-preferred-height renderer widget) => minimum,
    natural}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object this cell renderer will be
    rendering to}
  @argument[minimum]{an integer for the minimum size}
  @argument[natural]{an integer for the natural size}
  @begin{short}
    Retreives the minimum and natural size of a cell renderer when rendered to
    @arg{widget}.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-renderer-preferred-height renderer widget minimum natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-renderer-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_height_for_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_preferred_height_for_width"
               %cell-renderer-preferred-height-for-width) :void
  (renderer (g:object cell-renderer))
  (widget (g:object widget))
  (width :int)
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-renderer-preferred-height-for-width (renderer widget width)
 #+liber-documentation
 "@version{#2025-07-06}
  @syntax{(gtk:cell-renderer-preferred-height-for-width renderer widget width)
    => minimum, natural}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object this cell renderer will be
    rendering to}
  @argument[width]{an integer for the size which is available for allocation}
  @argument[minimum]{an integer for the minimum size}
  @argument[natural]{an integer for the preferred size}
  @begin{short}
    Retreives the minimum and natural height of a cell renderer if it were
    rendered to @arg{widget} with the specified width.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-renderer-preferred-height-for-width renderer
                                               widget
                                               width
                                               minimum
                                               natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-renderer-preferred-height-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_preferred_size"
               %cell-renderer-preferred-size) :void
  (renderer (g:object cell-renderer))
  (widget (g:object widget))
  (minimum (g:boxed requisition))
  (natural (g:boxed requisition)))

(defun cell-renderer-preferred-size (renderer widget)
 #+liber-documentation
 "@version{#2025-07-14}
  @syntax{(gtk:cell-renderer-preferred-size renderer widget) => minimum,
    natural}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object this cell renderer will be
    rendering to}
  @argument[minimum]{a @class{gtk:requisition} instance for the minimum size}
  @argument[natural]{a @class{gtk:requisition} instance for the natural size}
  @begin{short}
    Retrieves the minimum and natural size of a cell renderer taking into
    account the preference for height-for-width management of the widget.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-class{gtk:requisition}"
  (let ((minimum (make-requisition))
        (natural (make-requisition)))
    (%cell-renderer-preferred-size renderer widget minimum natural)
    (values minimum natural)))

(export 'cell-renderer-preferred-size)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_preferred_width"
               %cell-renderer-preferred-width) :void
  (renderer (g:object cell-renderer))
  (widget (g:object widget))
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-renderer-preferred-width (renderer widget)
 #+liber-documentation
 "@version{#2025-07-14}
  @syntax{(gtk:cell-renderer-preferred-width renderer widget) => minimum,
    natural}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object this cell renderer will be
    rendering to}
  @argument[minimum]{an integer for the minimum size}
  @argument[natural]{an integer for natural size}
  @begin{short}
    Retreives the minimum and natural size of a cell renderer when rendered to
    @arg{widget}.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-renderer-preferred-width renderer widget minimum natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-renderer-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_preferred_width_for_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_preferred_width_for_height"
               %cell-renderer-preferred-width-for-height) :void
  (renderer (g:object cell-renderer))
  (widget (g:object widget))
  (height :int)
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-renderer-preferred-width-for-height (renderer widget height)
 #+liber-documentation
 "@version{#2025-07-06}
  @syntax{(gtk:cell-renderer-preferred-width-for-height renderer widget height)
    => minimum, natural}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[widget]{a @class{gtk:widget} object this cell renderer will be
    rendering to}
  @argument[height]{an integer for the size which is available for allocation}
  @argument[minimum]{an integer for the minimum size}
  @argument[natural]{an integer for the preferred size}
  @begin{short}
    Retreives the minimum and natural of a cell renderer if it were rendered
    to @arg{widget} with the specified height.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-renderer-preferred-width-for-height renderer
                                               widget
                                               height
                                               minimum
                                               natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-renderer-preferred-width-for-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_get_request_mode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_get_request_mode" cell-renderer-request-mode)
    size-request-mode
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @begin{return}
    The @sym{gtk:size-request-mode} mode preferred by this cell renderer.
  @end{return}
  @begin{short}
    Gets whether the cell renderer prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:size-request-mode}"
  (renderer (g:object cell-renderer)))

(export 'cell-renderer-request-mode)

;;; --- End of file gtk3.cell-renderer.lisp ------------------------------------
