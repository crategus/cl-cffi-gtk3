;;; ----------------------------------------------------------------------------
;;; gtk3.tool-item.lisp
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
;;; GtkToolItem
;;;
;;;     The base class of widgets that can be added to GtkToolShell
;;;
;;; Types and Values
;;;
;;;     GtkToolItem
;;;
;;; Functions
;;;
;;;     gtk_tool_item_new
;;;     gtk_tool_item_set_homogeneous
;;;     gtk_tool_item_get_homogeneous
;;;     gtk_tool_item_set_expand
;;;     gtk_tool_item_get_expand
;;;     gtk_tool_item_set_tooltip_text
;;;     gtk_tool_item_set_tooltip_markup
;;;     gtk_tool_item_set_use_drag_window
;;;     gtk_tool_item_get_use_drag_window
;;;     gtk_tool_item_set_visible_horizontal
;;;     gtk_tool_item_get_visible_horizontal
;;;     gtk_tool_item_set_visible_vertical
;;;     gtk_tool_item_get_visible_vertical
;;;     gtk_tool_item_set_is_important
;;;     gtk_tool_item_get_is_important
;;;     gtk_tool_item_get_ellipsize_mode
;;;     gtk_tool_item_get_icon_size
;;;     gtk_tool_item_get_orientation
;;;     gtk_tool_item_get_toolbar_style
;;;     gtk_tool_item_get_relief_style
;;;     gtk_tool_item_get_text_alignment
;;;     gtk_tool_item_get_text_orientation
;;;     gtk_tool_item_retrieve_proxy_menu_item
;;;     gtk_tool_item_get_proxy_menu_item
;;;     gtk_tool_item_set_proxy_menu_item
;;;     gtk_tool_item_rebuild_menu
;;;     gtk_tool_item_toolbar_reconfigured
;;;     gtk_tool_item_get_text_size_group
;;;
;;; Properties
;;;
;;;     is-important
;;;     visible-horizontal
;;;     visible-vertical
;;;
;;; Signals
;;;
;;;     create-menu-proxy
;;;     toolbar-reconfigured
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ├── GtkToolButton
;;;                         ╰── GtkSeparatorToolItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToolItem implements AtkImplementorIface, GtkBuildable and
;;;     GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToolItem
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkToolItem" tool-item
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_tool_item_get_type")
  ((is-important
    tool-item-is-important
    "is-important" "gboolean" t t)
   (visible-horizontal
    tool-item-visible-horizontal
    "visible-horizontal" "gboolean" t t)
   (visible-vertical
    tool-item-visible-vertical
    "visible-vertical" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'tool-item 'type)
 "@version{#2023-3-28}
  @begin{short}
    The @sym{gtk:tool-item} widget is a widget that can appear on a toolbar.
  @end{short}
  To create a toolbar item that contain something else than a button, use the
  @fun{gtk:tool-item-new} function. Use the @fun{gtk:container-add} function to
  add a child widget to the tool item. For toolbar items that contain buttons,
  see the @class{gtk:tool-button}, @class{gtk:toggle-tool-button} and
  @class{gtk:radio-tool-button} widgets.

  See the @class{gtk:toolbar} documentation for a description of the toolbar
  widget, and the @class{gtk:tool-shell} documentation for a description of the
  tool shell interface.
  @begin[Signal Details]{dictionary}
    @subheading{The \"create-menu-proxy\" signal}
      @begin{pre}
lambda (item)    :run-last
      @end{pre}
      The signal is emitted when the toolbar needs information from @arg{item}
      about whether the item should appear in the toolbar overflow menu. In
      response the tool item should either.
      @begin{itemize}
        @begin{item}
          Call the @fun{gtk:tool-item-proxy-menu-item} function with @code{nil}
          and return @em{true} to indicate that the tool item should not appear
          in the overflow menu,
        @end{item}
        @begin{item}
          call the @fun{gtk:tool-item-proxy-menu-item} function with a new
          menu item and return @em{true}, or
        @end{item}
        @begin{item}
          return @em{false} to indicate that the signal was not handled by the
          tool item. This means that the tool item will not appear in the
          overflow menu unless a later handler installs a menu item.
        @end{item}
      @end{itemize}
      The toolbar may cache the result of this signal. When the tool item
      changes how it will respond to this signal it must call the
      @fun{gtk:tool-item-rebuild-menu} function to invalidate the cache and
      ensure that the toolbar rebuilds its overflow menu.
      @begin[code]{table}
        @entry[item]{The @sym{gtk:tool-item} widget the signal was emitted on.}
        @entry[Returns]{@em{True} if the signal was handled, @em{false} if not.}
      @end{table}
    @subheading{The \"toolbar-reconfigured\" signal}
      @begin{pre}
lambda (item)    :run-last
      @end{pre}
      The signal is emitted when some property of the toolbar that the item is
      a child of changes. For custom subclasses of the @sym{gtk:tool-item}
      class, the default handler of this signal use the
      @fun{gtk:tool-shell-orientation}, @fun{gtk:tool-shell-style},
      @fun{gtk:tool-shell-icon-size}, or @fun{gtk:tool-shell-relief-style}
      functions to find out what the toolbar should look like and change
      themselves accordingly.
      @begin[code]{table}
        @entry[item]{The @sym{gtk:tool-item} widget the signal was emitted
          on.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:tool-item-new}
  @see-slot{gtk:tool-item-is-important}
  @see-slot{gtk:tool-item-visible-horizontal}
  @see-slot{gtk:tool-item-visible-vertical}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- tool-item-is-important -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-important" 'tool-item) t)
 "The @code{is-important} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toolbar item is considered important. When @em{true}, toolbar
  buttons show text in @code{:both-horiz} mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-is-important)
      "Accessor"
      (documentation 'tool-item-is-important 'function)
 "@version{#2023-3-28}
  @syntax[]{gtk:tool-item-is-important object) => setting}
  @syntax[]{(setf (gtk:tool-item-is-important object) setting)}
  @argument[object]{a @class{gtk:tool-item} widget}
  @argument[setting]{a boolean whether the tool item should be considered
    important}
  @begin{short}
    Accessor of the @slot[gtk:tool-item]{is-important} slot of the
    @class{gtk:tool-item} class.
  @end{short}
  The @sym{gtk:tool-item-is-important} function returns whether the tool item
  is considered important. The @sym{(setf gtk:tool-item-is-important)} function
  sets whether the tool item should be considered important.

  The @class{gtk:tool-button} class uses this property to determine whether to
  show or hide its label when the toolbar style is @code{:both-horiz}. The
  result is that only tool buttons with the @code{is-important} property set
  have labels, an effect known as \"priority text\".
  @see-class{gtk:tool-item}
  @see-class{gtk:tool-button}
  @see-symbol{gtk:toolbar-style}")

;;; --- tool-item-visible-horizontal -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-horizontal"
                                               'tool-item) t)
 "The @code{visible-horizontal} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a horizontal
  orientation. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-visible-horizontal)
      "Accessor"
      (documentation 'tool-item-visible-horizontal 'function)
 "@version{#2023-3-28}
  @syntax[]{gtk:tool-item-visible-horizontal object) => visible}
  @syntax[]{(setf (gtk:tool-item-visible-horizontal object) visible)}
  @argument[object]{a @class{gtk:tool-item} widget}
  @argument[visible]{a boolean whether @arg{object} is visible when in
    horizontal mode}
  @begin{short}
    Accessor of the @slot[gtk:tool-item]{visible-horizontal} slot of the
    @class{gtk:tool-item} class.
  @end{short}
  The @sym{gtk:tool-item-visible-horizontal} function returns whether the tool
  item is visible on toolbars that are docked horizontally. The
  @sym{(setf gtk:tool-item-visible-horizontal)} function sets whether the tool
  item is visible when the toolbar is docked horizontally.
  @see-class{gtk:tool-item}")

;;; --- tool-item-visible-vertical ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-vertical"
                                               'tool-item) t)
 "The @code{visible-vertical} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a vertical
  orientation. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'tool-item-visible-vertical)
      "Accessor"
      (documentation 'tool-item-visible-vertical 'function)
 "@version{#2023-3-28}
  @syntax[]{gtk:tool-item-visible-vertical object) => visible}
  @syntax[]{(setf (gtk:tool-item-visible-vertical object) visible)}
  @argument[object]{a @class{gtk:tool-item} widget}
  @argument[visible]{a boolean whether @arg{object} is visible when the toolbar
    is in vertical mode}
  @begin{short}
    Accessor of the @slot[gtk:tool-item]{visible-vertical} slot of the
    @class{gtk:tool-item} class.
  @end{short}
  The @sym{gtk:tool-item-visible-vertical} function returns whether the tool
  item is visible when the toolbar is docked vertically. The
  @sym{(setf gtk:tool-item-visible-vertical)} function sets whether the tool
  item is visible when the toolbar is docked vertically.

  Some tool items, such as text entries, are too wide to be useful on a
  vertically docked toolbar. If the @arg{visible} argument is @em{false} the
  tool item will not appear on toolbars that are docked vertically.
  @see-class{gtk:tool-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline tool-item-new))

(defun tool-item-new ()
 #+liber-documentation
 "@version{#2023-3-28}
  @return{The new @class{gtk:tool-item} widget.}
  @short{Creates a new tool item.}
  @see-class{gtk:tool-item}"
  (make-instance 'tool-item))

(export 'tool-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_homogeneous ()
;;; gtk_tool_item_set_homogeneous () -> tool-item-homogeneous
;;; ----------------------------------------------------------------------------

(defun (setf tool-item-homogeneous) (homogeneous item)
  (cffi:foreign-funcall "gtk_tool_item_set_homogeneous"
                        (g:object tool-item) item
                        :boolean homogeneous
                        :void)
  homogeneous)

(cffi:defcfun ("gtk_tool_item_get_homogeneous" tool-item-homogeneous) :boolean
 #+liber-documentation
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-item-homogeneous tool-item) => homogeneous}
  @syntax[]{(setf (gtk:tool-item-homogeneous tool-item) homogeneous)}
  @argument[item]{a @class{gtk:tool-item} widget}
  @argument[homogeneous]{a boolean whether @arg{item} is the same size as other
    homogeneous items}
  @begin{short}
    Accessor of the homogeneous property of the tool item.
  @end{short}
  The @sym{gtk:tool-item-homogeneous} function returns whether @arg{item} is
  the same size as other homogeneous items. The
  @sym{(setf gtk:tool-item-homogeneous)} function sets whether @arg{item} is to
  be allocated the same size as other homogeneous items.

  The effect is that all homogeneous items will have the same width as the
  widest of the items.
  @see-class{gtk:tool-item}"
  (item (g:object tool-item)))

(export 'tool-item-homogeneous)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_expand ()
;;; gtk_tool_item_set_expand () -> tool-item-expand
;;; ----------------------------------------------------------------------------

(defun (setf tool-item-expand) (expand item)
  (cffi:foreign-funcall "gtk_tool_item_set_expand"
                        (g:object tool-item) item
                        :boolean expand
                        :void)
  expand)

(cffi:defcfun ("gtk_tool_item_get_expand" tool-item-expand) :boolean
 #+liber-documentation
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-item-expand tool-item) => expand}
  @syntax[]{(setf (gtk:tool-item-expand tool-item) expand)}
  @argument[item]{a @class{gtk:tool-item} widget}
  @argument[expand]{a boolean whether @arg{item} is allocated extra space}
  @begin{short}
    Accessor of the expand property of the tool item.
  @end{short}
  The @sym{gtk:tool-item-expand} function returns whether @arg{item} is
  allocated extra space. The @sym{(setf gtk:tool-item-expand)} function sets
  whether @arg{item} is allocated extra space when there is more room on
  the toolbar then needed for the tool items. The effect is that the tool item
  gets bigger when the toolbar gets bigger and smaller when the toolbar gets
  smaller.
  @see-class{gtk:tool-item}"
  (item (g:object tool-item)))

(export 'tool-item-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_tooltip_text ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_set_tooltip_text" tool-item-set-tooltip-text)
    :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @argument[text]{a string with the text to be used as tooltip for @arg{item}}
  @begin{short}
    Sets the text to be displayed as tooltip on the tool item.
  @end{short}
  See the @fun{gtk:widget-tooltip-text} function.
  @see-class{gtk:tool-item}
  @see-function{gtk:widget-tooltip-text}
  @see-function{gtk:tool-item-set-tooltip-markup}"
  (item (g:object tool-item))
  (text :string))

(export 'tool-item-set-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_set_tooltip_markup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_set_tooltip_markup" tool-item-set-tooltip-markup)
    :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @argument[markup]{a string with the markup text to be used as tooltip for
    @arg{item}}
  @begin{short}
    Sets the markup text to be displayed as tooltip on the tool item.
  @end{short}
  See the @fun{gtk:widget-tooltip-markup} function.
  @see-class{gtk:tool-item}
  @see-function{gtk:widget-tooltip-markup}
  @see-function{gtk:tool-item-set-tooltip-text}"
  (item (g:object tool-item))
  (markup :string))

(export 'tool-item-set-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_use_drag_window ()
;;; gtk_tool_item_set_use_drag_window () -> tool-item-use-drag-window
;;; ----------------------------------------------------------------------------

(defun (setf tool-item-use-drag-window) (setting item)
  (cffi:foreign-funcall "gtk_tool_item_set_use_drag_window"
                        (g:object tool-item) item
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("gtk_tool_item_get_use_drag_window" tool-item-use-drag-window)
    :boolean
 #+liber-documentation
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-item-use-drag-window tool-item) => setting}
  @syntax[]{(setf (gtk:tool-item-use-drag-window tool-item) setting)}
  @argument[item]{a @class{gtk:tool-item} widget}
  @argument[setting]{a boolean whether @arg{item} has a drag window}
  @begin{short}
    Accessor of the use drag window property of the tool item.
  @end{short}
  The @sym{gtk:tool-item-use-drag-window} function returns whether @arg{item}
  has a drag window. The @sym{(setf gtk:tool-item-use-drag-window)} function
  sets whether @arg{item} has a drag window.

  When @em{true} the tool item can be used as a drag source through
  the @fun{gtk:drag-source-set} function. When @arg{item} has a drag window it
  will intercept all events, even those that would otherwise be sent to a child
  of @arg{item}.
  @see-class{gtk:tool-item}
  @see-function{gtk:drag-source-set}"
  (item (g:object tool-item)))

(export 'tool-item-use-drag-window)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_ellipsize_mode () -> tool-item-ellipsize-mode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_ellipsize_mode" tool-item-ellipsize-mode)
    pango:ellipsize-mode
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A @symbol{pango:ellipsize-mode} value indicating how text in @arg{item}
    should be ellipsized.
  @end{return}
  @begin{short}
    Returns the ellipsize mode used for the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function to find out how text should be ellipsized.
  @see-class{gtk:tool-item}
  @see-symbol{pango:ellipsize-mode}"
  (item (g:object tool-item)))

(export 'tool-item-ellipsize-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_icon_size () -> tool-item-icon-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_icon_size" tool-item-icon-size) icon-size
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A @symbol{gtk:icon-size} value indicating the icon size used for @arg{item}.
  @end{return}
  @begin{short}
    Returns the icon size used for the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function to find out what size icons they should use.
  @see-class{gtk:tool-item}
  @see-symbol{gtk:icon-size}"
  (item (g:object tool-item)))

(export 'tool-item-icon-size)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_orientation () -> tool-item-orientation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_orientation" tool-item-orientation)
    orientation
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A @symbol{gtk:orientation} value indicating the orientation used for
    @arg{item}.
  @end{return}
  @begin{short}
    Returns the orientation used for the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function to find out what size icons they should use.
  @see-class{gtk:tool-item}
  @see-symbol{gtk:orientation}"
  (item (g:object tool-item)))

(export 'tool-item-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_toolbar_style () -> tool-item-toolbar-style
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_toolbar_style" tool-item-toolbar-style)
    toolbar-style
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A @symbol{gtk:toolbar-style} value indicating the toolbar style used for
    @arg{item}.
  @end{return}
  @begin{short}
    Returns the toolbar style used for the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function in the handler of the \"toolbar-reconfigured\" signal to find out in
  what style the toolbar is displayed and change themselves accordingly.

  Possibilities are:
  @begin{itemize}
    @entry[:both]{Meaning the tool item should show both an icon and a label,
      stacked vertically.}
    @entry[:icons]{Meaning the toolbar shows only icons.}
    @entry[:text]{Meaning the tool item should only show text.}
    @entry[:both-horiz]{Meaning the tool item should show both an icon and a
      label, arranged horizontally. However, note the \"has-text-horizontally\"
      property that makes tool buttons not show labels when the toolbar style
      is @code{:both-horiz}.}
  @end{itemize}
  @see-class{gtk:tool-item}
  @see-symbol{gtk:toolbar-style}"
  (item (g:object tool-item)))

(export 'tool-item-toolbar-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_relief_style () -> tool-item-relief-style
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_relief_style" tool-item-relief-style)
    relief-style
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A value of the @symbol{gtk:relief-style} enumeration indicating the relief
    style used for @arg{item}.
  @end{return}
  @begin{short}
    Returns the relief style of the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function in the handler of the \"toolbar-reconfigured\" signal to find out
  the relief style of buttons. See the @fun{gtk:button-relief-style} function.
  @see-class{gtk:tool-item}
  @see-function{gtk:button-relief-style}"
  (item (g:object tool-item)))

(export 'tool-item-relief-style)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_alignment () -> tool-item-text-alignment
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_text_alignment" tool-item-text-alignment)
    :float
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A float indicating the horizontal text alignment used for @arg{item}.
  @end{return}
  @begin{short}
    Returns the text alignment used for the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function to find out how text should be aligned.
  @see-class{gtk:tool-item}"
  (item (g:object tool-item)))

(export 'tool-item-text-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_orientation () -> tool-item-text-orientation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_text_orientation" tool-item-text-orientation)
    orientation
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    A value of the @symbol{gtk:orientation} enumeration indicating the text
    orientation used for @arg{item}.
  @end{return}
  @begin{short}
    Returns the text orientation used for the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function to find out how text should be orientated.
  @see-class{gtk:tool-item}
  @see-symbol{gtk:orientation}"
  (item (g:object tool-item)))

(export 'tool-item-text-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_retrieve_proxy_menu_item ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_retrieve_proxy_menu_item"
               tool-item-retrieve-proxy-menu-item) g:object
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{return}
    The @class{gtk:menu-item} widget that is going to appear in the overflow
    menu for @arg{item}.
  @end{return}
  @begin{short}
    Returns the @class{gtk:menu-item} widget that was last set by the
    @fun{gtk:tool-item-proxy-menu-item} function, i.e. the @class{gtk:menu-item}
    widget that is going to appear in the overflow menu.
  @end{short}
  @see-class{gtk:tool-item}
  @see-class{gtk:menu-item}
  @see-function{gtk:tool-item-proxy-menu-item}"
  (item (g:object tool-item)))

(export 'tool-item-retrieve-proxy-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_proxy_menu_item ()
;;; gtk_tool_item_set_proxy_menu_item () -> tool-item-proxy-menu-item
;;; ----------------------------------------------------------------------------

(defun (setf tool-item-proxy-menu-item) (menuitem item id)
  (cffi:foreign-funcall "gtk_tool_item_set_proxy_menu_item"
                        (g:object tool-item) item
                        :string id
                        (g:object widget) menuitem
                        :void)
  menuitem)

(cffi:defcfun ("gtk_tool_item_get_proxy_menu_item" tool-item-proxy-menu-item)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-3-28}
  @syntax[]{(gtk:tool-item-proxy-menu-item item id) => menuitem}
  @syntax[]{(setf (gtk:tool-item-proxy-menu-item item id) menuitem)}
  @argument[item]{a @class{gtk:tool-item} widget}
  @argument[id]{a string used to identify the menu item}
  @argument[menuitem]{a @class{gtk:menu-item} to be used in the overflow menu}
  @begin{short}
    Accessor of the proxy menu item of the tool item.
  @end{short}
  The @sym{gtk:tool-item-proxy-menu-item} function gets the menu item used in
  the toolbar overflow menu. The @sym{(setf gtk:tool-item-proxy-menu-item)}
  function sets the menu item used in the toolbar overflow menu.

  Custom subclasses of the @class{gtk:tool-item} class should use this function
  to update their menu item when the @class{gtk:tool-item} changes. That the
  @arg{id} argument must match ensures that a @class{gtk:tool-item} widget will
  not inadvertently change a menu item that they did not create.
  @see-class{gtk:tool-item}
  @see-class{gtk:menu-item}"
  (item (g:object tool-item))
  (id :string))

(export 'tool-item-proxy-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_rebuild_menu ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_rebuild_menu" tool-item-rebuild-menu) :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{short}
    Calling this function signals to the toolbar that the overflow menu item
    for the tool item has changed.
  @end{short}
  If the overflow menu is visible when this function it called, the menu will
  be rebuilt.

  The function must be called when the tool item changes what it will do in
  response to the \"create-menu-proxy\" signal.
  @see-class{gtk:tool-item}"
  (item (g:object tool-item)))

(export 'tool-item-rebuild-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_toolbar_reconfigured ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_toolbar_reconfigured"
               tool-item-toolbar-reconfigured) :void
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @begin{short}
    Emits the signal \"toolbar-reconfigured\" on the tool item.
  @end{short}
  The @class{gtk:toolbar} widget and other @class{gtk:tool-shell}
  implementations use this function to notify children, when some aspect of
  their configuration changes.
  @see-class{gtk:tool-item}
  @see-class{gtk:toolbar}
  @see-class{gtk:tool-shell}"
  (item (g:object widget)))

(export 'tool-item-toolbar-reconfigured)

;;; ----------------------------------------------------------------------------
;;; gtk_tool_item_get_text_size_group () -> tool-item-text-size-group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tool_item_get_text_size_group" tool-item-text-size-group)
    (g:object size-group)
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[item]{a @class{gtk:tool-item} widget}
  @return{A @class{gtk:size-group} widget.}
  @begin{short}
    Returns the size group used for labels in the tool item.
  @end{short}
  Custom subclasses of the @class{gtk:tool-item} class should call this
  function and use the size group for labels.
  @see-class{gtk:tool-item}
  @see-class{gtk:size-group}"
  (item (g:object tool-item)))

(export 'tool-item-text-size-group)

;;; --- End of file gtk3.tool-item.lisp ----------------------------------------
