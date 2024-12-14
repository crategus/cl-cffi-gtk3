;;; ----------------------------------------------------------------------------
;;; gtk3.action.lisp
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
;;; GtkAction
;;;
;;;     A deprecated action which can be triggered by a menu or toolbar item.
;;;
;;; Types and Values
;;;
;;;     GtkAction
;;;
;;; Accessors
;;;
;;;     gtk_action_get_always_show_image
;;;     gtk_action_set_always_show_image
;;;     gtk_action_set_gicon
;;;     gtk_action_get_gicon
;;;     gtk_action_set_icon_name
;;;     gtk_action_get_icon_name
;;;     gtk_action_set_is_important
;;;     gtk_action_get_is_important
;;;     gtk_action_set_label
;;;     gtk_action_get_label
;;;     gtk_action_get_name
;;;     gtk_action_get_sensitive
;;;     gtk_action_set_sensitive
;;;     gtk_action_set_short_label
;;;     gtk_action_get_short_label
;;;     gtk_action_set_stock_id
;;;     gtk_action_get_stock_id
;;;     gtk_action_set_tooltip
;;;     gtk_action_get_tooltip
;;;     gtk_action_get_visible
;;;     gtk_action_set_visible
;;;     gtk_action_set_visible_horizontal
;;;     gtk_action_get_visible_horizontal
;;;     gtk_action_set_visible_vertical
;;;     gtk_action_get_visible_vertical
;;;
;;; Functions
;;;
;;;     gtk_action_new
;;;     gtk_action_is_sensitive
;;;     gtk_action_is_visible
;;;     gtk_action_activate
;;;     gtk_action_create_icon
;;;     gtk_action_create_menu_item
;;;     gtk_action_create_tool_item
;;;     gtk_action_create_menu
;;;     gtk_action_get_proxies
;;;     gtk_action_connect_accelerator
;;;     gtk_action_disconnect_accelerator
;;;     gtk_action_block_activate
;;;     gtk_action_unblock_activate
;;;     gtk_action_get_accel_path
;;;     gtk_action_set_accel_path
;;;     gtk_action_get_accel_closure
;;;     gtk_action_set_accel_group
;;;
;;; Properties
;;;
;;;     action-group
;;;     always-show-image
;;;     gicon
;;;     hide-if-empty
;;;     icon-name
;;;     is-important
;;;     label
;;;     name
;;;     sensitive
;;;     short-label
;;;     stock-id
;;;     tooltip
;;;     visible
;;;     visible-horizontal
;;;     visible-overflown
;;;     visible-vertical
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ├── GtkToggleAction
;;;         ╰── GtkRecentAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAction implements GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAction" action
  (:superclass g:object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_action_get_type")
  ((action-group
    action-action-group
    "action-group" "GtkActionGroup" t t)
   (always-show-image
    action-always-show-image
    "always-show-image" "gboolean" t t)
   (gicon
    action-gicon
    "gicon" "GIcon" t t)
   (hide-if-empty
    action-hide-if-empty
    "hide-if-empty" "gboolean" t t)
   (icon-name
    action-icon-name
    "icon-name" "gchararray" t t)
   (is-important
    action-is-important
    "is-important" "gboolean" t t)
   (label
    action-label
    "label" "gchararray" t t)
   (name
    action-name
    "name" "gchararray" t nil)
   (sensitive
    action-sensitive
    "sensitive" "gboolean" t t)
   (short-label
    action-short-label
    "short-label" "gchararray" t t)
   (stock-id
    action-stock-id
    "stock-id" "gchararray" t t)
   (tooltip
    action-tooltip
    "tooltip" "gchararray" t t)
   (visible
    action-visible
    "visible" "gboolean" t t)
   (visible-horizontal
    action-visible-horizontal
    "visible-horizontal" "gboolean" t t)
   (visible-overflown
    action-visible-overflown
    "visible-overflown" "gboolean" t t)
   (visible-vertical
    action-visible-vertical
    "visible-vertical" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'action 'type)
 "@version{2024-9-26}
  @begin{short}
    Actions represent operations that the user can be perform, along with some
    information how it should be presented in the interface.
  @end{short}
  Each action provides methods to create icons, menu items and toolbar items
  representing itself.

  As well as the callback function that is called when the action gets
  activated, the following also gets associated with the action:
  @begin{itemize}
    @item{a name, not translated, for path lookup}
    @item{a label, translated, for display}
    @item{an accelerator}
    @item{whether label indicates a stock ID}
    @item{a tooltip, optional, translated}
    @item{a toolbar label, optional, shorter than label}
  @end{itemize}
  The action will also have some state information:
  @begin{itemize}
    @item{visible, shown/hidden}
    @item{sensitive, enabled/disabled}
  @end{itemize}
  Apart from regular actions, there are toggle actions, which can be toggled
  between two states and radio actions, of which only one in a group can be in
  the \"active\" state. Other actions can be implemented as @class{gtk:action}
  subclasses.

  Each action can have one or more proxy widgets. To act as an action proxy,
  the widget needs to implement the @class{gtk:activatable} interface. Proxies
  mirror the state of the action and should change when the state of the action
  changes. Properties that are always mirrored by proxies are the
  @slot[gtk:action]{sensitive} and @slot[gtk:action]{visible} properties. The
  @slot[gtk:action]{gicon}, @slot[gtk:action]{icon-name},
  @slot[gtk:action]{label}, @slot[gtk:action]{short-label} and
  @slot[gtk:action]{stock-id} properties are only mirorred if the proxy widget
  has the @slot[gtk:activatable]{use-action-appearance} property set to
  @em{true}.

  When the proxy is activated, it should activate its action.
  @begin[Warning]{dictionary}
    The @class{gtk:action} class has been deprecated since GTK 3.10. Use
    the @class{g:action} interface instead, and associate actions with
    @class{gtk:actionable} widgets. Use the @class{g:menu-model} class for
    creating menus with the @fun{gtk:menu-new-from-model} function.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (action)    :no-recurse
      @end{pre}
      The signal is emitted when the action is activated.
      @begin[code]{table}
        @entry[action]{The @class{gtk:action} object which received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:action-new}
  @see-slot{gtk:action-action-group}
  @see-slot{gtk:action-always-show-image}
  @see-slot{gtk:action-gicon}
  @see-slot{gtk:action-hide-if-empty}
  @see-slot{gtk:action-icon-name}
  @see-slot{gtk:action-is-important}
  @see-slot{gtk:action-label}
  @see-slot{gtk:action-name}
  @see-slot{gtk:action-sensitive}
  @see-slot{gtk:action-short-label}
  @see-slot{gtk:action-stock-id}
  @see-slot{gtk:action-tooltip}
  @see-slot{gtk:action-visible}
  @see-slot{gtk:action-visible-horizontal}
  @see-slot{gtk:action-visible-overflown}
  @see-slot{gtk:action-visible-vertical}
  @see-class{gtk:action-group}
  @see-class{gtk:activatable}
  @see-class{g:action}
  @see-class{gtk:actionable}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:action-action-group ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action-group" 'action) t)
 "The @code{action-group} property of type @class{gtk:action-group}
  (Read / Write) @br{}
  The action group the action is associated with, or @code{nil} for internal
  use.")

#+liber-documentation
(setf (liber:alias-for-function 'action-action-group)
      "Accessor"
      (documentation 'action-action-group 'function)
 "@version{2024-9-26}
  @syntax{gtk:action-action-group object) => group}
  @syntax{(setf (gtk:action-action-group object) group)}
  @argument[object]{a @class{gtk:action} object}
  @argument[group]{a @class{gtk:action-group} object}
  @begin{short}
    Accessor of the @slot[gtk:action]{action-group} slot of the
    @class{gtk:action} class.
  @end{short}
  The action group the action is associated with, or @code{nil} for internal
  use.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-action-group} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:action-group}")

;;; --- gtk:action-always-show-image -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "always-show-image" 'action) t)
 "The @code{always-show-image} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If @em{true}, the menu item proxies of the action will ignore the
  @slot[gtk:settings]{gtk-menu-images} setting and always show their image, if
  available. Use this property if the menu item would be useless or hard to use
  without their image. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'action-always-show-image)
      "Accessor"
      (documentation 'action-always-show-image 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-always-show-image object) => always-show}
  @syntax{(setf (gtk:action-always-show-image object) always-show)}
  @argument[object]{a @class{gtk:action} object}
  @argument[always-show]{@em{true} if menu item proxies should always show
    their image}
  @begin{short}
    Accessor of the @slot[gtk:action]{always-show-image} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-always-show-image} function returns whether the action
  menu item proxies will ignore the @slot[gtk:settings]{gtk-menu-images}
  setting and always show their image, if available. The
  @setf{gtk:action-always-show-image} function sets the property. Use this if
  the menu item would be useless or hard to use without their image.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-always-show-image} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:settings-gtk-menu-images}")

;;; --- gtk:action-gicon -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'action) t)
 "The @code{gicon} property of type @class{g:icon} (Read / Write) @br{}
  The icon displayed in the action. Note that the stock icon is preferred, if
  the @slot[gtk:action]{stock-id} property holds the ID of an existing stock
  icon. This is an appearance property and thus only applies if the
  @slot[gtk:activatable]{use-action-appearance} property is @em{true}.")

#+liber-documentation
(setf (liber:alias-for-function 'action-gicon)
      "Accessor"
      (documentation 'action-gicon 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-gicon object) => icon}
  @syntax{(setf (gtk:action-gicon object) icon)}
  @argument[object]{a @class{gtk:action} object}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:action]{gicon} slot of the @class{gtk:action}
    class.
  @end{short}
  The @fun{gtk:action-gicon} function gets the icon of the action. The
  @setf{gtk:action-gicon} function sets the icon.

  The icon displayed in the action. Note that the stock icon is preferred, if
  the @slot[gtk:action]{stock-id} property holds the ID of an existing stock
  icon. This is an appearance property and thus only applies if the
  @slot[gtk:activatable]{use-action-appearance} property is @em{true}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-gicon} function has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{g:icon}
  @see-function{gtk:action-stock-id}
  @see-function{gtk:activatable-use-action-appearance}")

;;; --- gtk:action-hide-if-empty -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hide-if-empty" 'action) t)
 "The @code{hide-if-empty} property of type @code{:boolean} (Read / Write) @br{}
  When @em{true}, empty menu proxies for this action are hidden. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-hide-if-empty)
      "Accessor"
      (documentation 'action-hide-if-empty 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-hide-if-empty object) => hide-if-empty}
  @syntax{(setf (gtk:action-hide-if-empty object) hide-if-empty)}
  @argument[object]{a @class{gtk:action} object}
  @argument[hide-if-empty]{a boolean whether empty menu proxies are hidden}
  @begin{short}
    Accessor of the @slot[gtk:action]{hide-if-empty} slot of the
    @class{gtk:action} class.
  @end{short}
  When @em{true}, empty menu proxies for this action are hidden.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-hide-if-empty} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}")

;;; --- gtk:action-icon-name ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'action) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon from the icon theme. Note that the stock icon is
  preferred, if the @slot[gtk:action]{stock-id} property holds the ID of an
  existing stock icon, and the @class{g:icon} object is preferred if the
  @slot[gtk:action]{gicon} property is set. This is an appearance property and
  thus only applies if the @slot[gtk:activatable]{use-action-appearance}
  property is @em{true}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-icon-name)
      "Accessor"
      (documentation 'action-icon-name 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-icon-name object) => name}
  @syntax{(setf (gtk:action-icon-name object) name)}
  @argument[object]{a @class{gtk:action} object}
  @argument[name]{a string with the icon name to set}
  @begin{short}
    Accessor of the @slot[gtk:action]{icon-name} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-icon-name} function gets the icon name of the action. The
  @setf{gtk:action-icon-name} function sets the icon name.

  The name of the icon from the icon theme. Note that the stock icon is
  preferred, if the @slot[gtk:action]{stock-id} property holds the ID of an
  existing stock icon, and the @class{g:icon} object is preferred if the
  @slot[gtk:action]{gicon} property is set. This is an appearance property and
  thus only applies if the @slot[gtk:activatable]{use-action-appearance}
  property is @em{true}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-icon-name} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:action-gicon}
  @see-function{gtk:action-stock-id}
  @see-function{gtk:activatable-use-action-appearance}")

;;; --- gtk:action-is-important ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-important" 'action) t)
 "The @code{is-important} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is considered important. When @em{true}, toolitem proxies
  for this action show text in @code{:both-horiz} mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'action-is-important)
      "Accessor"
      (documentation 'action-is-important 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-is-important object) => is-important}
  @syntax{(setf (gtk:action-is-important object) is-important)}
  @argument[object]{a @class{gtk:action} object}
  @argument[is-important]{@em{true} to make the action important}
  @begin{short}
    Accessor of the @slot[gtk:action]{is-important} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-is-important} function checks whether the action is
  important or not. The @setf{gtk:action-is-important} function sets whether
  the action is important. This attribute is used primarily by toolbar items to
  decide whether to show a label or not. When @em{true}, toolitem proxies for
  this action show text in @code{:both-horiz} mode.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-is-important} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-symbol{gtk:toolbar-style}")

;;; --- gtk:action-label -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'action) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The label used for menu items and buttons that activate this action. If the
  label is @code{nil}, GTK uses the stock label specified via the
  @slot[gtk:action]{stock-id} property. This is an appearance property and thus
  only applies if the @slot[gtk:activatable]{use-action-appearance} property is
  @em{true}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-label)
      "Accessor"
      (documentation 'action-label 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-label object) => label}
  @syntax{(setf (gtk:action-label object) label)}
  @argument[object]{a @class{gtk:action} object}
  @argument[label]{a string with the label text to set}
  @begin{short}
    Accessor of the @slot[gtk:action]{label} slot of the @class{gtk:action}
    class.
  @end{short}
  The @fun{gtk:action-label} function gets the label text of the action. The
  @setf{gtk:action-label} function sets the label.

  The label used for menu items and buttons that activate this action. If the
  label is @code{nil}, GTK uses the stock label specified via the
  @slot[gtk:action]{stock-id} property. This is an appearance property and thus
  only applies if the @slot[gtk:activatable]{use-action-appearance} property is
  @em{true}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-label} function has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:action-stock-id}
  @see-function{gtk:activatable-use-action-appearance}")

;;; --- gtk:action-name --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'action) t)
 "The @code{name} property of type @code{:string} (Read / Write / Construct)
  @br{}
  The unique name for the action. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-name)
      "Accessor"
      (documentation 'action-name 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-name object) => name}
  @argument[object]{a @class{gtk:action} object}
  @argument[name]{a string with the name of the action}
  @begin{short}
    Accessor of the @slot[gtk:action]{name} slot of the @class{gtk:action}
    class.
  @end{short}
  The @fun{gtk:action-name} function returns the unique name of the action.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-name} function has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}")

;;; --- gtk:action-sensitive ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sensitive" 'action) t)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action is enabled. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-sensitive)
      "Accessor"
      (documentation 'action-sensitive 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-sensitive object) => sensitive}
  @syntax{(setf (gtk:action-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:action} object}
  @argument[sensitive]{@em{true} to make the action sensitive}
  @begin{short}
    Accessor of the @slot[gtk:action]{sensitive} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-label} function returns whether the action itself is
  sensitive. The @setf{gtk:action-label} function sets the sensitivity.

  Note that this does not necessarily mean effective sensitivity. See the
  @fun{gtk:action-is-sensitive} function for that.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-sensitive} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:action-is-sensitive}")

;;; --- gtk:action-short-label -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "short-label" 'action) t)
 "The @code{short-label} property of type @code{:string} (Read / Write) @br{}
  The shorter label that may be used on toolbar buttons. This is an appearance
  property and thus only applies if the
  @slot[gtk:activatable]{use-action-appearance} property is @em{true}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-short-label)
      "Accessor"
      (documentation 'action-short-label 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-short-label object) => label}
  @syntax{(setf (gtk:action-short-label object) label)}
  @argument[object]{a @class{gtk:action} object}
  @argument[label]{a string with the label text to set}
  @begin{short}
    Accessor of the @slot[gtk:action]{short-label} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-short-label} function gets the short label text of the
  action. The @setf{gtk:action-short-label} function sets a shorter label text.

  A shorter label that may be used on toolbar buttons. This is an appearance
  property and thus only applies if the
  @slot[gtk:activatable]{use-action-appearance} property is @em{true}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-short-label} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:activatable-use-action-appearance}")

;;; --- gtk:action-stock-id ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stock-id" 'action) t)
 "The @code{stock-id} property of type @code{:string} (Read / Write) @br{}
  The stock icon displayed in widgets representing this action. This is an
  appearance property and thus only applies if the
  @slot[gtk:activatable]{use-action-appearance} property is @em{true}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-stock-id)
      "Accessor"
      (documentation 'action-stock-id 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-stock-id object) => stock-id}
  @syntax{(setf (gtk:action-stock-id object) stock-id)}
  @argument[object]{a @class{gtk:action} object}
  @argument[stock-id]{a string with the stock ID}
  @begin{short}
    Accessor of the @slot[gtk:action]{stock-id} slot of the @class{gtk:action}
    class.
  @end{short}
  The @fun{gtk:action-stock-id} function gets the stock ID of the action. The
  @setf{gtk:action-stock-id} function sets the stock ID.

  The stock icon displayed in widgets representing the action. This is an
  appearance property and thus only applies if the
  @slot[gtk:activatable]{use-action-appearance} property is @em{true}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-stock-id} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:activatable-use-action-appearance}")

;;; --- gtk:action-tooltip -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip" 'action) t)
 "The @code{tooltip} property of type @code{:string} (Read / Write) @br{}
  The tooltip for this action. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-tooltip)
      "Accessor"
      (documentation 'action-tooltip 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-tooltip object) => tooltip}
  @syntax{(setf (gtk:action-tooltip object) tooltip)}
  @argument[object]{a @class{gtk:action} object}
  @argument[tooltip]{a string with the tooltip text}
  @begin{short}
    Accessor of the @slot[gtk:action]{tooltip} slot of the @class{gtk:action}
    class.
  @end{short}
  The @fun{gtk:action-tooltip} function gets the tooltip text of the action.
  The @setf{gtk:action-tooltip} function sets the tooltip text.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-tooltip} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}")

;;; --- gtk:action-visible -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'action) t)
 "The @code{visible} property of type  @code{:boolean} (Read / Write) @br{}
  Whether the action is visible. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-visible)
      "Accessor"
      (documentation 'action-visible 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-visible object) => visible}
  @syntax{(setf (gtk:action-visible object) visible)}
  @argument[object]{a @class{gtk:action} object}
  @argument[visible]{@em{true} to make the action visible}
  @begin{short}
    Accessor of the @slot[gtk:action]{visible} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-visible} function returns whether the action itself is
  visible. The @setf{gtk:action-visible} function sets the visibility.

  Note that this does not necessarily mean effective visibility. See the
  @fun{gtk:action-is-visible} function for that.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-visible} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-function{gtk:action-is-visible}")

;;; --- gtk:action-visible-horizontal ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-horizontal" 'action) t)
 "The @code{visible-horizontal} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a horizontal
  orientation. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-visible-horizontal)
      "Accessor"
      (documentation 'action-visible-horizontal 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-visible-horizontal object) => visible}
  @syntax{(setf (gtk:action-visible-horizontal object) visible)}
  @argument[object]{a @class{gtk:action} object}
  @argument[visible]{@em{true} to make the action visible horizontally}
  @begin{short}
    Accessor of the @slot[gtk:action]{visible-horizontal} slot of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-visible-horizontal} function checks whether the action is
  visible when horizontal. The @setf{gtk:action-visible-horizontal} function
  sets the visibility.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-visible-horizontal} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}")

;;; --- gtk:action-visible-overflow --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-overflown" 'action) t)
 "The @code{visible-overflown} property of type @code{:boolean} (Read / Write)
  @br{}
  When @em{true}, toolitem proxies for this action are represented in the
  toolbar overflow menu. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-visible-overflown)
      "Accessor"
      (documentation 'action-visible-overflown 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-visible-overflow object) => visible}
  @syntax{(setf (gtk:action-visible-overflow object) visible)}
  @argument[object]{a @class{gtk:action} object}
  @argument[visible]{a boolean whether a toolbar overflow menu is shown}
  @begin{short}
    Accessor of the @slot[gtk:action]{visible-overflown} slot of the
    @class{gtk:action} class.
  @end{short}
  When @em{true}, toolitem proxies for this action are represented in the
  toolbar overflow menu.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-visible-overflown} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}")

;;; --- gtk:action-visible-vertical --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-vertical" 'action) t)
 "The @code{visible-vertical} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toolbar item is visible when the toolbar is in a vertical
  orientation. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-visible-vertical)
      "Accessor"
      (documentation 'action-visible-vertical 'function)
 "@version{2024-9-26}
  @syntax{(gtk:action-visible-vertical object) => visible}
  @syntax{(setf (gtk:action-visible-vertical object) visible)}
  @argument[object]{a @class{gtk:action} object}
  @argument[visible]{@em{true} to make the action visible vertically}
  @begin{short}
    Accessor of the @slot[gtk:action]{visible-vertical} of the
    @class{gtk:action} class.
  @end{short}
  The @fun{gtk:action-visible-vertical} function checks whether the action is
  visible when vertical. The @setf{gtk:action-visible-vertical} function sets
  the visibility.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-visible-vertical} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_new
;;; ----------------------------------------------------------------------------

(declaim (inline action-new))

(defun action-new (name &optional label tooltip stock-id)
 #+liber-documentation
 "@version{2024-9-26}
  @argument[name]{a string with a unique name for the action}
  @argument[label]{an optional string with the label displayed in menu items
    and on buttons, or @code{nil}}
  @argument[tooltip]{an optional string with a tooltip for the action, or
    @code{nil}}
  @argument[stock-id]{an optional string with the stock icon to display in
    widgets representing the action, or @code{nil}}
  @return{The new @class{gtk:action} object.}
  @begin{short}
    Creates a new action.
  @end{short}
  To add the action to a @class{gtk:action-group} object and set the accelerator
  for the action, call the @fun{gtk:action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-new} function has been deprecated since version 3.10
    and should not be used in newly written code. Use the @class{g:action}
    interface instead, associating it to a widget with the
    @class{gtk:actionable} interface or creating a @class{gtk:menu} widget with
    the @fun{gtk:menu-new-from-model} function.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:action-group}
  @see-class{gtk:menu}
  @see-class{g:action}
  @see-class{g:menu-model}
  @see-function{gtk:action-group-add-action}
  @see-function{gtk:actionable}
  @see-function{gtk:menu-new-from-model}
  @see-function{g:action-enabled}"
  (make-instance 'action
                 :name name
                 :label (or label (cffi:null-pointer))
                 :tooltip (or tooltip (cffi:null-pointer))
                 :stock-id (or stock-id (cffi:null-pointer))))

(export 'action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_sensitive
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_is_sensitive" action-is-sensitive) :boolean
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @return{@em{True} if the action and its associated action group are both
    sensitive.}
  @short{Returns whether the action is effectively sensitive.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-is-sensitive} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{g:action-enabled} on a @class{g:action} object instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{g:action}
  @see-function{gtk:action-sensitive}
  @see-function{g:action-enabled}"
  (action (g:object action)))

(export 'action-is-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_action_is_visible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_is_visible" action-is-visible) :boolean
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @return{@em{True} if the action and its associated action group are
    both visible.}
  @short{Returns whether the action is effectively visible.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-is-visible} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the @class{g:action}
    interface instead, and control and monitor the state of
    @class{gtk:actionable} widgets directly.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{g:action}
  @see-class{gtk:actionable}"
  (action (g:object action)))

(export 'action-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_action_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_activate" action-activate) :void
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @begin{short}
    Emits the @code{\"activate\"} signal on the specified action, if it is not
    insensitive.
  @end{short}
  This gets called by the proxy widgets when they get activated. It can also be
  used to manually activate an action.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-activate} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{g:action-group-activate-action} function on a @class{g:action} object
    instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{g:action}
  @see-function{g:action-group-activate-action}"
  (action (g:object action)))

(export 'action-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_create_icon" action-create-icon) (g:object widget)
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @argument[size]{a value of the @symbol{gtk:icon-size} enumeration for the
    size of the icon that should be created}
  @return{The @class{gtk:widget} object that displays the icon for this action.}
  @begin{short}
    This function is intended for use by action implementations to create icons
    displayed in the proxy widgets.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-create-icon} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{g:menu-item-set-icon} function to set an icon on a @class{g:menu-item}
    object, or the @fun{gtk:container-add} function to add a @class{gtk:image}
    widget to a @class{gtk:button} widget.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:widget}
  @see-class{g:menu-item}
  @see-class{gtk:image}
  @see-class{gtk:button}
  @see-symbol{gtk:icon-size}
  @see-function{g:menu-item-set-icon}
  @see-function{gtk:container-add}"
  (action (g:object action))
  (size icon-size))

(export 'action-create-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_create_menu_item" action-create-menu-item)
    (g:object image-menu-item)
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @return{The @class{gtk:image-menu-item} widget connected to the action.}
  @short{Creates a menu item widget that proxies for the given action.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-create-menu-item} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{g:menu-item-new} function and associate it with a @class{g:action}
    object instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:image-menu-item}
  @see-class{g:action}
  @see-function{g:menu-item-new}"
  (action (g:object action)))

(export 'action-create-menu-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_tool_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_create_tool_item" action-create-tool-item)
    (g:object tool-button)
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @return{The @class{gtk:tool-button} widget connected to the action.}
  @short{Creates a toolbar item that proxies for the given action.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-create-tool-item} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use a
    @class{gtk:tool-item} widget and associate it with a @class{g:action}
    object using the @fun{gtk:actionable-action-name} function instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:tool-button}
  @see-class{gtk:tool-item}
  @see-class{g:action}
  @see-function{gtk:actionable-action-name}"
  (action (g:object action)))

(export 'action-create-tool-item)

;;; ----------------------------------------------------------------------------
;;; gtk_action_create_menu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_create_menu" action-create-menu) g:object
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @return{The @class{gtk:menu} widget provided by the action, or @code{nil}.}
  @begin{short}
    If the action provides a @class{gtk:menu} widget as a submenu for the menu
    item or the toolbar item it creates, this function returns an instance of
    that menu.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-create-menu} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the @class{g:action}
    interface and the @class{g:menu-model} object instead, and create a
    @class{gtk:menu} widget with the @fun{gtk:menu-new-from-model} function.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:menu}
  @see-class{g:action}
  @see-class{g:menu-model}
  @see-function{gtk:menu-new-from-model}"
  (action (g:object action)))

(export 'action-create-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_proxies
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_get_proxies" action-proxies)
    (g:slist-t g:object :free-from-foreign nil)
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @return{The list of @class{gtk:widget} proxy widgets.}
  @short{Returns the proxy widgets for the action.}
  See also the @fun{gtk:activatable-related-action} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-proxies} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:widget}
  @see-function{gtk:activatable-related-action}"
  (action (g:object action)))

(export 'action-proxies)

;;; ----------------------------------------------------------------------------
;;; gtk_action_connect_accelerator
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_connect_accelerator" action-connect-accelerator)
    :void
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @begin{short}
    Installs the accelerator for the action if the action has an accel path and
    group.
  @end{short}
  See the @fun{gtk:action-accel-path} and @fun{gtk:action-set-accel-group}
  functions.

  Since multiple proxies may independently trigger the installation of the
  accelerator, the action counts the number of times this function has been
  called and does not remove the accelerator until the
  @fun{gtk:action-disconnect-accelerator} function has been called as many
  times.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-connect-accelerator} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{g:action} interface and the accelerator group on an associated
    @class{gtk:menu} widget instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:menu}
  @see-class{g:action}
  @see-function{gtk:action-accel-path}
  @see-function{gtk:action-set-accel-group}
  @see-function{gtk:action-disconnect-accelerator}"
  (action (g:object action)))

(export 'action-connect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_disconnect_accelerator
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_disconnect_accelerator"
               action-disconnect-accelerator) :void
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @begin{short}
    Undoes the effect of one call to the @fun{gtk:action-connect-accelerator}
    function.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-disconnect-accelerator} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @class{g:action} interface and the accelerator group on an associated
    @class{gtk:menu} widget instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:menu}
  @see-class{g:action}
  @see-function{gtk:action-connect-accelerator}"
  (action (g:object action)))

(export 'action-disconnect-accelerator)

;;; ----------------------------------------------------------------------------
;;; gtk_action_block_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_block_activate" action-block-activate) :void
 #+liber-documentation
 "@version{#2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @short{Disable activation signals from the action.}
  This is needed when updating the state of your @class{gtk:activatable} proxy
  widget could result in calling the @fun{gtk:action-activate} function, this
  is a convenience function to avoid recursing in those cases, updating toggle
  state for instance.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-block-activate} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{g:simple-action-enabled} function to disable the
    @class{g:simple-action} object instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:activatable}
  @see-class{g:simple-action}
  @see-function{gtk:action-activate}
  @see-function{g:simple-action-enabled}"
  (action (g:object action)))

(export 'action-block-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_unblock_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_unblock_activate" action-unblock-activate) :void
 #+liber-documentation
 "@version{#2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @short{Reenable activation signals from the action.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-unblock-activate} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{g:simple-action-enabled} function to enable the @class{g:simple-action}
    object instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{g:simple-action}
  @see-function{gtk:action-block-activate}
  @see-function{g:simple-action-enabled}"
  (action (g:object action)))

(export 'action-unblock-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_path
;;; gtk_action_set_accel_path
;;; ----------------------------------------------------------------------------

(defun (setf action-accel-path) (path action)
  (cffi:foreign-funcall "gtk_action_set_accel_path"
                        (g:object action) action
                        :string path
                        :void)
  path)

(cffi:defcfun ("gtk_action_get_accel_path" action-accel-path) :string
 #+liber-documentation
 "@version{2024-9-26}
  @syntax{(gtk:action-accel-path action) => path}
  @syntax{(setf (gtk:action-accel-path action) path)}
  @argument[action]{a @class{gtk:action} object}
  @argument[path]{a string with the accelerator path}
  @begin{short}
    The @fun{gtk:action-accel-path} function returns the accel path for this
    action, or @code{nil} if none is set.
  @end{short}
  The @setf{gtk:action-accel-path} function sets the accel path. All proxy
  widgets associated with the action will have this accel path, so that their
  accelerators are consistent.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-accel-path} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @class{g:action} interface and the accelerator path on an associated
    @class{gtk:menu} widget instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:menu}
  @see-class{g:action}"
  (action (g:object action)))

(export 'action-accel-path)

;;; ----------------------------------------------------------------------------
;;; gtk_action_get_accel_closure                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_get_accel_closure" action-accel-closure) :pointer
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[action]{a @class{gtk:action} object}
  @return{The accel closure for this action. The returned closure is owned by
    GTK and must not be unreffed or modified.}
  @short{Returns the accel closure for this action.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-get-accel-closure} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{g:action} inferface and @class{gtk:menu} widget instead, which have
    no equivalent for getting the accel closure.
  @end{dictionary}
  @see-class{gtk:action}"
  (action (g:object action)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_set_accel_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_set_accel_group" action-set-accel-group) :void
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:action} object}
  @argument[group]{a @class{gtk:accel-group} object or @code{nil}}
  @begin{short}
    Sets the accelerator group in which the accelerator for this action will be
    installed.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-set-accel-group} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @class{g:action} interface and the accelerator group on an associated
    @class{gtk:menu} widget instead.
  @end{dictionary}
  @see-class{gtk:action}
  @see-class{gtk:accel-group}
  @see-class{gtk:menu}
  @see-class{g:action}"
  (action (g:object action))
  (group (g:object accel-group)))

(export 'action-set-accel-group)

;;; --- End of file gtk3.action.lisp -------------------------------------------
