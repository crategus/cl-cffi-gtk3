;;; ----------------------------------------------------------------------------
;;; gtk3.paned.lisp
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
;;; GtkPaned
;;;
;;;     A widget with two adjustable panes
;;;
;;; Types and Values
;;;
;;;     GtkPaned
;;;
;;; Accessors
;;;
;;;     gtk_paned_set_position
;;;     gtk_paned_get_position
;;;     gtk_paned_set_wide_handle
;;;     gtk_paned_get_wide_handle
;;;
;;; Functions
;;;
;;;     gtk_paned_new
;;;     gtk_paned_add1
;;;     gtk_paned_add2
;;;     gtk_paned_pack1
;;;     gtk_paned_pack2
;;;     gtk_paned_get_child1
;;;     gtk_paned_get_child2
;;;     gtk_paned_get_handle_window
;;;
;;; Properties
;;;
;;;     max-position
;;;     min-position
;;;     position
;;;     position-set
;;;     wide-handle
;;;
;;; Child Properties
;;;
;;;     resize
;;;     shrink
;;;
;;; Style Properties
;;;
;;;     handle-size
;;;
;;; Signals
;;;
;;;     accept-position
;;;     cancel-position
;;;     cycle-child-focus
;;;     cycle-handle-focus
;;;     move-handle
;;;     toggle-handle-focus
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkPaned
;;;                     ├── GtkHPaned
;;;                     ╰── GtkVPaned
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPaned implements AtkImplementorIface, GtkBuildable and GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPaned
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPaned" paned
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable")
   :type-initializer "gtk_paned_get_type")
  ((max-position
    paned-max-position
    "max-position" "gint" t nil)
   (min-position
    paned-min-position
    "min-position" "gint" t nil)
   (position
    paned-position
    "position" "gint" t t)
   (position-set
    paned-position-set
    "position-set" "gboolean" t t)
   (wide-handle
    paned-wide-handle
    "wide-handle" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'paned 'type)
 "@version{2025-06-27}
  @begin{short}
    The @class{gtk:paned} widget has two panes, arranged either horizontally or
    vertically.
  @end{short}
  The division between the two panes is adjustable by the user by dragging a
  handle.

  @image[panes]{Figure: GtkPaned}

  Child widgets are added to the panes of the paned widget with the
  @fun{gtk:paned-pack1} and @fun{gtk:paned-pack2} functions. The division
  between the two children is set by default from the size requests of the
  children, but it can be adjusted by the user.

  A paned widget draws a separator between the two child widgets and a small
  handle that the user can drag to adjust the division. It does not draw any
  relief around the children or around the separator. The space in which the
  separator is called the gutter. Often, it is useful to put each child widget
  inside a @class{gtk:frame} widget with the shadow type set to the
  @val[gtk:shadow-type]{:in} value so that the gutter appears as a ridge. No
  separator is drawn if one of the children is missing.

  Each child widget has two child properties that can be set, the
  @prop[gtk:paned]{resize} and @prop[gtk:paned]{shrink} child properties. If the
  @prop[gtk:paned]{resize} child property is @em{true}, then when the
  @class{gtk:paned} widget is resized, that child widget will expand or shrink
  along with the paned widget. If the @prop[gtk:paned]{shrink} property is
  @em{true}, then that child widget can be made smaller than its requisition by
  the user. Setting the @prop[gtk:paned]{shrink} child property to @em{false}
  allows the application to set a minimum size. If the @prop[gtk:paned]{resize}
  child property is @em{false} for both children, then this is treated as if
  the @prop[gtk:paned]{resize} child property is @em{true} for both children.

  The application can set the position of the slider as if it were set by the
  user, by calling the @fun{gtk:paned-position} function.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
paned
├── <child>
├── separator\[.wide\]
╰── <child>
    @end{pre}
    The @class{gtk:paned} implementation has a main CSS node with name
    @code{paned}, and a subnode for the separator with name @code{separator}.
    The subnode gets a @code{.wide} style class when the paned is supposed to
    be wide. In horizontal orientation, the nodes of the children are always
    arranged from left to right. So @code{:first-child} will always select the
    leftmost child widget, regardless of text direction.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Creating a paned widget with minimum sizes.
    @begin{pre}
(let ((paned (make-instance 'paned
                            :orientation :horizontal
                            :width-request 250
                            :height-request 150))
      (frame1 (make-instance 'frame :shadow-type :in
                                    :width-request 100))
      (frame2 (make-instance 'frame :shadow-type :in
                                    :width-request 50)))
    (gtk:paned-pack1 paned frame1 :resize t :shrink nil)
    (gtk:paned-pack2 paned frame2 :resize nil :shrink nil)
    ... )
    @end{pre}
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[progress-bar:resize]{property}
      The @code{resize} child property of type @code{:boolean} (Read / Write)
      @br{}
      Determines whether the child widget expands and shrinks along with the
      paned widget. @br{}
      Default value: @em{true} @br{}
    @end{property}
    @begin[progress-bar:shrink]{property}
      The @code{shrink} child property of type @code{:boolean} (Read / Write)
      @br{}
      Determines whether the child widget can be made smaller than its
      requisition. @br{}
      Default value: @em{true} @br{}
    @end{property}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[paned:handle-size]{property}
      The @code{handle-size} style property of type @code{:int} (Read) @br{}
      The width of the handle. @br{}
      Allowed values: >= 0 @br{}
      Default value: 5
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[paned::accept-position]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:paned} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to accept the current
      position of the handle when moving it using key bindings. The default
      binding for this signal is the @kbd{Return} or @kbd{Space} key.
    @end{signal}
    @begin[panded::cancel-position]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:paned} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to cancel moving the
      position of the handle using key bindings. The position of the handle will
      be reset to the value prior to moving it. The default binding for this
      signal is the @kbd{Escape} key.
    @end{signal}
    @begin[paned::cycle-child-focus]{signal}
      @begin{pre}
lambda (widget reversed)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:paned} widget that received the signal.}
        @entry[reversed]{The boolean whether cycling backward or forward.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to cycle the focus
      between the children of the paned widget. The default binding is the
      @kbd{F6} key.
    @end{signal}
    @begin[paned::cycle-handle-focus]{signal}
      @begin{pre}
lambda (widget reversed)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:paned} widget that received the signal.}
        @entry[reversed]{The boolean whether cycling backward or forward.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to cycle whether the
      paned widget should grab focus to allow the user to change position of the
      handle by using key bindings. The default binding for this signal is the
      @kbd{F8} key.
    @end{signal}
    @begin[paned::move-handle]{signal}
      @begin{pre}
lambda (widget scrolltype)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:paned} widget that received the signal.}
        @entry[scrolltype]{The value of the @sym{gtk:scroll-type} enumeration.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to move the handle
      when the user is using key bindings to move it.
    @end{signal}
    @begin[paned::toggle-handle-focus]{signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:paned} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to accept the current
      position of the handle and then move focus to the next widget in the focus
      chain. The default binding is the @kbd{Tab} key.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:paned-new}
  @see-slot{gtk:paned-max-position}
  @see-slot{gtk:paned-min-position}
  @see-slot{gtk:paned-position}
  @see-slot{gtk:paned-position-set}
  @see-slot{gtk:paned-wide-handle}
  @see-symbol{gtk:scroll-type}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:paned-max-position -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-position" 'paned) t)
 "The @code{max-position} property of type @code{:int} (Read) @br{}
  The largest possible value for the position property. This property is
  derived from the size and shrinkability of the children of the paned widget.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 2147483647")

#+liber-documentation
(setf (liber:alias-for-function 'paned-max-position)
      "Accessor"
      (documentation 'paned-max-position 'function)
 "@version{2025-06-13}
  @syntax{(gtk:paned-max-position object) => position}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[position]{an integer for the largest possible position}
  @begin{short}
    Accessor of the @slot[gtk:paned]{max-position} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @fun{gtk:paned-max-position} function gets the largest possible value for
  the position property.
  @see-class{gtk:paned}
  @see-function{gtk:paned-position}
  @see-function{gtk:paned-min-position}")

;;; --- gtk:paned-min-position -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-position" 'paned) t)
 "The @code{min-position} property of type @code{:int} (Read) @br{}
  The smallest possible value for the position property. This property is
  derived from the size and shrinkability of the children of the paned widget.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'paned-min-position)
      "Accessor"
      (documentation 'paned-min-position 'function)
 "@version{2025-06-13}
  @syntax{(gtk:paned-min-position object) => position}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[position]{an integer for the smallest possible position}
  @begin{short}
    Accessor of the @slot[gtk:paned]{min-position} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @fun{gtk:paned-min-position} function gets the smallest possible value
  for the position property.
  @see-class{gtk:paned}
  @see-function{gtk:paned-position}
  @see-function{gtk:paned-max-position}")

;;; --- gtk:paned-position -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'paned) t)
 "The @code{position} property of type @code{:int} (Read / Write) @br{}
  The position of the paned separator in pixels, 0 means all the way to the
  left/top. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'paned-position)
      "Accessor"
      (documentation 'paned-position 'function)
 "@version{2025-06-13}
  @syntax{(gtk:paned-position object) => position}
  @syntax{(setf (gtk:paned-position position) position)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[position]{an integer for the pixel position of divider, a negative
    value means that the position is unset}
  @begin{short}
    Accessor of the @slot[gtk:paned]{position} slot of the @class{gtk:paned}
    class.
  @end{short}
  The @fun{gtk:paned-position} function obtains the position of the divider
  between the two panes. The @setf{gtk:paned-position} function sets the
  position.
  @see-class{gtk:paned}
  @see-function{gtk:paned-max-position}
  @see-function{gtk:paned-min-position}")

;;; --- gtk:paned-position-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position-set" 'paned) t)
 "The @code{position-set} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the @slot[gtk:paned]{position} property should be used. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-position-set)
      "Accessor"
      (documentation 'paned-position-set 'function)
 "@version{2023-03-05}
  @syntax{(gtk:paned-position-set object) => setting}
  @syntax{(setf (gtk:paned-position-set position) setting)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[setting]{a boolean whether the @slot[gtk:paned]{position} property
    should be used}
  @begin{short}
    Accessor of the @slot[gtk:paned]{position-set} slot of the
    @class{gtk:paned} class.
  @end{short}
  The @fun{gtk:paned-position-set} function gets whether the
  @slot[gtk:paned]{position} property should be used. The
  @setf{gtk:paned-position-set} function sets whether the
  @slot[gtk:paned]{position} property should be used.
  @see-class{gtk:paned}
  @see-function{gtk:paned-position}")

;;; --- gtk:paned-wide-handled -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wide-handle" 'paned) t)
 "The @code{wide-handled} property of type @code{:boolean} (Read / Write) @br{}
  Setting this property to @em{true} indicates that the paned widget needs to
  provide stronger visual separation, for example because it separates between
  two notebooks, whose tab rows would otherwise merge visually. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'paned-wide-handle)
      "Accessor"
      (documentation 'paned-wide-handle 'function)
 "@version{2025-06-13}
  @syntax{(gtk:paned-wide-handle object) => wide}
  @syntax{(setf (gtk:paned-wide-handle object) wide)}
  @argument[object]{a @class{gtk:paned} widget}
  @argument[wide]{a boolean for the value of the @slot[gtk:paned]{wide-handle}
    property}
  @begin{short}
    Accessor of the @slot[gtk:paned]{wide-handle} slot of the @class{gtk:paned}
    class.
  @end{short}
  The @fun{gtk:paned-wide-handle} function gets the
  @slot[gtk:paned]{wide-handle} property. The
  @setf{gtk:paned-wide-handle} function sets the property.
  @see-class{gtk:paned}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:paned-child-resize -------------------------------------------------

(define-child-property paned-child-resize
                       "resize" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'paned-child-resize)
      "Accessor"
      (documentation 'paned-child-resize 'function)
 "@version{2027-06-27}
  @syntax{(gtk:paned-child-resize container child) => resize)}
  @syntax{(setf (gtk:paned-child-resize container child) resize)}
  @argument[container]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[resize]{a boolean whether the child widget expands and shrinks
    along with the paned widget}
  @begin{short}
    Accessor of the @prop[gtk:paned]{resize} child property of the
    @class{gtk:paned} class.
  @end{short}
  The @fun{gtk:paned-child-resize} function gets whether the child widget
  expands and shrinks along with the paned widget. The
  @setf{gtk:paned-child-resize} function sets the child property.
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-child-shrink}")

;;; --- gtk:paned-child-shrink -------------------------------------------------

(define-child-property paned-child-shrink
                       "shrink" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'paned-child-shrink)
      "Accessor"
      (documentation 'paned-child-shrink 'function)
 "@version{2027-06-27}
  @syntax{(gtk:paned-child-shrink container child) => shrink)}
  @syntax{(setf (gtk:paned-child-shrink container child) shrink)}
  @argument[container]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[shrink]{a boolean whether the child widget can be made smaller than
    its requisition}
  @begin{short}
    Accessor of the @prop[gtk:paned]{shrink} child property of the
    @class{gtk:paned} class.
  @end{short}
  The @fun{gtk:paned-child-shrink} function determines whether the child widget
  can be made smaller than its requisition. The
  @setf{gtk:paned-child-shrink} function gets the value of the child property.
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-child-resize}")

;;; ----------------------------------------------------------------------------
;;; gtk_paned_new
;;; ----------------------------------------------------------------------------

(declaim (inline paned-new))

(defun paned-new (orientation)
 #+liber-documentation
 "@version{2025-06-27}
  @argument[orientation]{a @sym{gtk:orientation} value for the orientation
    of the paned widget}
  @return{The new @class{gtk:paned} widget.}
  @short{Creates a new paned widget.}
  @see-class{gtk:paned}
  @see-symbol{gtk:orientation}"
  (make-instance 'paned
                 :orientation orientation))

(export 'paned-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add1
;;; ----------------------------------------------------------------------------

(defun paned-add1 (paned child)
 #+liber-documentation
 "@version{2023-03-05}
  @argument[paned]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @begin{short}
    Adds a child widget to the top or left pane with default parameters.
  @end{short}
  This is equivalent to:
  @begin{pre}
(gtk:paned-pack1 paned child nil t)
  @end{pre}
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-add2}
  @see-function{gtk:paned-pack1}
  @see-function{gtk:paned-pack2}"
  (paned-pack1 paned child :resize nil :shrink t))

(export 'paned-add1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_add2
;;; ----------------------------------------------------------------------------

(defun paned-add2 (paned child)
 #+liber-documentation
 "@version{2023-03-05}
  @argument[paned]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @begin{short}
    Adds a child widget to the bottom or right pane with default parameters.
  @end{short}
  This is equivalent to:
  @begin{pre}
(gtk:paned-pack2 paned child t t)
  @end{pre}
  @see-class{gtk:paned}
  @see-function{gtk:paned-add1}
  @see-function{gtk:paned-pack1}
  @see-function{gtk:paned-pack2}"
  (paned-pack2 paned child :resize t :shrink t))

(export 'paned-add2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack1
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paned_pack1" %paned-pack1) :void
  (paned (g:object paned))
  (child (g:object widget))
  (resize :boolean)
  (shrink :boolean))

(defun paned-pack1 (paned child &key (resize nil) (shrink t))
 #+liber-documentation
 "@version{2023-03-05}
  @argument[paned]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[resize]{a boolean whether the child widget should expand when the
  paned widget is resized}
  @argument[shrink]{a boolean whether the child widget can be made smaller than
    its requisition}
  @begin{short}
    Adds a child widget to the top or left pane.
  @end{short}
  The @arg{resize} and @arg{shrink} arguments are keyword arguments.
  The @arg{resize} argument has the @em{false} default value and the
  @arg{shrink} argument the @em{true} default value.
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-pack2}"
  (%paned-pack1 paned child resize shrink))

(export 'paned-pack1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_pack2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paned_pack2" %paned-pack2) :void
  (paned (g:object paned))
  (child (g:object widget))
  (resize :boolean)
  (shrink :boolean))

(defun paned-pack2 (paned child &key (resize t) (shrink t))
 #+liber-documentation
 "@version{2023-03-05}
  @argument[paned]{a @class{gtk:paned} widget}
  @argument[child]{a @class{gtk:widget} child widget to add}
  @argument[resize]{a boolean whether the child widget should expand when the
    paned widget is resized}
  @argument[shrink]{a boolean whether the child widget can be made smaller than
    its requisition}
  @begin{short}
    Adds a child widget to the bottom or right pane.
  @end{short}
  The @arg{resize} and @arg{shrink} arguments are keyword arguments with the
  @em{true} default value.
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-pack1}"
  (%paned-pack2 paned child resize shrink))

(export 'paned-pack2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child1
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paned_get_child1" paned-child1) (g:object widget)
 #+liber-documentation
 "@version{2024-04-10}
  @argument[paned]{a @class{gtk:paned} widget}
  @begin{return}
    The first @class{gtk:widget} child widget, or @code{nil} if it is not set.
  @end{return}
  @short{Obtains the first child widget of the paned widget.}
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-child2}"
  (paned (g:object paned)))

(export 'paned-child1)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_child2
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paned_get_child2" paned-child2) (g:object widget)
 #+liber-documentation
 "@version{2024-04-10}
  @argument[paned]{a @class{gtk:paned} widget}
  @begin{return}
    The second @class{gtk:widget} child widget, or @code{nil} if it is not set.
  @end{return}
  @short{Obtains the second child widget of the paned widget.}
  @see-class{gtk:paned}
  @see-class{gtk:widget}
  @see-function{gtk:paned-child1}"
  (paned (g:object paned)))

(export 'paned-child2)

;;; ----------------------------------------------------------------------------
;;; gtk_paned_get_handle_window
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paned_get_handle_window" paned-handle-window)
    (g:object gdk:window)
 #+liber-documentation
 "@version{2025-07-17}
  @argument[paned]{a @class{gtk:paned} widget}
  @return{The @class{gdk:window} handle window for the paned widget.}
  @begin{short}
    Returns the GDK window of the handle.
  @end{short}
  This function is useful when handling button or motion events because it
  enables the callback to distinguish between the window of the paned widget,
  a child widget and the handle.
  @see-class{gtk:paned}
  @see-class{gdk:window}"
  (paned (g:object paned)))

(export 'paned-handle-window)

;;; --- End of file gtk3.paned.lisp --------------------------------------------
