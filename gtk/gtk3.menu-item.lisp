;;; ----------------------------------------------------------------------------
;;; gtk3.menu-item.lisp
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
;;; GtkMenuItem
;;;
;;;     The widget used for item in menus.
;;;
;;; Types and Values
;;;
;;;     GtkMenuItem
;;;
;;; Functions
;;;
;;;     gtk_menu_item_new
;;;     gtk_menu_item_new_with_label
;;;     gtk_menu_item_new_with_mnemonic
;;;     gtk_menu_item_set_right_justified
;;;     gtk_menu_item_get_right_justified
;;;     gtk_menu_item_get_label
;;;     gtk_menu_item_set_label
;;;     gtk_menu_item_get_use_underline
;;;     gtk_menu_item_set_use_underline
;;;     gtk_menu_item_set_submenu
;;;     gtk_menu_item_get_submenu
;;;     gtk_menu_item_set_accel_path
;;;     gtk_menu_item_get_accel_path
;;;     gtk_menu_item_select
;;;     gtk_menu_item_deselect
;;;     gtk_menu_item_activate
;;;     gtk_menu_item_toggle_size_request
;;;     gtk_menu_item_toggle_size_allocate
;;;     gtk_menu_item_get_reserve_indicator
;;;     gtk_menu_item_set_reserve_indicator
;;;
;;; Properties
;;;
;;;     accel-path
;;;     label
;;;     right-justified
;;;     submenu
;;;     use-underline
;;;
;;; Style Properties
;;;
;;;     arrow-scaling
;;;     arrow-spacing
;;;     horizontal-padding
;;;     selected-shadow-type
;;;     toggle-spacing
;;;     width-chars
;;;
;;; Signals
;;;
;;;     activate
;;;     activate-item
;;;     deselect
;;;     select
;;;     toggle-size-allocate
;;;     toggle-size-request
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ├── GtkCheckMenuItem
;;;                         ├── GtkImageMenuItem
;;;                         ├── GtkSeparatorMenuItem
;;;                         ╰── GtkTearoffMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuItem implements AtkImplementorIface, GtkBuildable, GtkActivatable
;;;     and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkMenuItem
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkMenuItem" menu-item
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_menu_item_get_type")
  ((accel-path
    menu-item-accel-path
    "accel-path" "gchararray" t t)
   (label
    menu-item-label "label" "gchararray" t t)
   (right-justified
    menu-item-right-justified
    "right-justified" "gboolean" t t)
   (submenu
    menu-item-submenu
    "submenu" "GtkMenu" t t)
   (use-underline
    menu-item-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'menu-item 'type)
 "@version{#2023-3-21}
  @begin{short}
    The @sym{gtk:menu-item} widget and the derived widgets are the only valid
    childs for menus. Their function is to correctly handle highlighting,
    alignment, events and submenus.
  @end{short}

  As it derives from the @class{gtk:bin} class it can hold any valid child
  widget, although only a few are really useful.
  @begin[GtkMenuItem as GtkBuildable]{dictionary}
    The @sym{gtk:menu-item} implementation of the @class{gtk:buildable}
    interface supports adding a submenu by specifying @code{\"submenu\"} as the
    @code{\"type\"} attribute of a @code{<child>} element.

    @b{Example:} A UI definition fragment with submenus.
    @begin{pre}
<object class=\"GtkMenuItem\">
  <child type=\"submenu\">
    <object class=\"GtkMenu\"/>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
menuitem
├── <child>
╰── [arrow.right]
    @end{pre}
    The @sym{gtk:menu-item} class has a single CSS node with name
    @code{menuitem}. If the menu item has a submenu, it gets another CSS node
    with name @code{arrow}, which has the @code{.left} or @code{.right} style
    class.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of type @code{:float} (Read)
        @br{}
        Amount of space used up by arrow, relative to the font size of the menu
        item. @br{}
        @em{Warning:} The @code{arrow-scaling} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard min-width/min-height CSS properties on the arrow
        node. The value of this style property is ignored. @br{}
        Allowed values: [0,2] @br{}
        Default value: 0.8
      @end{entry}
      @begin[arrow-spacing]{entry}
        The @code{arrow-spacing} style property of type @code{:int} (Read) @br{}
        Space between label and arrow. @br{}
        @em{Warning:} The @code{arrow-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard margin CSS property on the arrow node. The value
        of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 10
      @end{entry}
      @begin[horizontal-padding]{entry}
        The @code{horizontal-padding} style property of type @code{:int} (Read)
        @br{}
        Padding to left and right of the menu item. @br{}
        @em{Warning:} The @code{horizontal-padding} style property has been
        deprecated since version 3.8 and should not be used in newly written
        code. Use the standard padding CSS property, through objects like
        @class{gtk:style-context} and @class{gtk:css-provider}. The value of
        this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 3
      @end{entry}
      @begin[selected-shadow-type]{entry}
        The @code{selected-shadow-type} style property of type
        @symbol{gtk:shadow-type} (Read) @br{}
        Shadow type when the menu item is selected. @br{}
        @em{Warning:} The @code{selected-shadow-type} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS to determine the shadow. The value of this style property
        is ignored. @br{}
        Default value: @code{:none}
      @end{entry}
      @begin[toggle-spacing]{entry}
        The @code{toggle-spacing} style property of type @code{:int} (Read)@br{}
        Space between icon and label. @br{}
        @em{Warning:} The @code{toggle-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard margin CSS property on the check or radio nodes.
        The value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 5
      @end{entry}
      @begin[width-chars]{entry}
        The @code{width-chars} style property of type @code{:int} (Read) @br{}
        The minimum desired width of the menu item in characters. @br{}
        @em{Warning:} The @code{width-chars} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use the
        standard CSS property min-width. The value of this style property is
        ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 12
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (item)    :action
      @end{pre}
      Emitted when the menu item is activated.
      @begin[code]{table}
        @entry[item]{The @sym{gtk:menu-item} widget which received the signal.}
      @end{table}
    @subheading{The \"activate-item\" signal}
      @begin{pre}
lambda (item)    :run-first
      @end{pre}
      Emitted when the menu item is activated, but also if the menu item has a
      submenu. For normal applications, the relevant signal is \"activate\".
      @begin[code]{table}
        @entry[item]{The @sym{gtk:menu-item} widget which received the signal.}
      @end{table}
    @subheading{The \"deselect\" signal}
      @begin{pre}
lambda (item)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[item]{The @sym{gtk:menu-item} widget which received the signal.}
      @end{table}
    @subheading{The \"select\" signal}
      @begin{pre}
lambda (item)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[item]{The @sym{gtk:menu-item} widget which received the signal.}
      @end{table}
    @subheading{The \"toggle-size-allocate\" signal}
      @begin{pre}
lambda (item arg)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[item]{The @sym{gtk:menu-item} widget which received the signal.}
        @entry[arg]{An integer which is not documented.}
      @end{table}
    @subheading{The \"toggle-size-request\" signal}
      @begin{pre}
lambda (item arg)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[item]{The @sym{gtk:menu-item} widget which received the signal.}
        @entry[arg]{A pointer which is not documented.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:menu-item-new}
  @see-constructor{gtk:menu-item-new-with-label}
  @see-constructor{gtk:menu-item-new-with-mnemonic}
  @see-slot{gtk:menu-item-accel-path}
  @see-slot{gtk:menu-item-label}
  @see-slot{gtk:menu-item-right-justified}
  @see-slot{gtk:menu-item-submenu}
  @see-slot{gtk:menu-item-use-underline}
  @see-class{gtk:bin}
  @see-class{gtk:menu-shell}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- menu-item-accel-path ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-path" 'menu-item) t)
 "The @code{accel-path} property of type @code{:string} (Read / Write) @br{}
  Sets the accelerator path of the menu item, through which runtime changes of
  the accelerator of the menu item caused by the user can be identified and
  saved to persistant storage. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-item-accel-path)
      "Accessor"
      (documentation 'menu-item-accel-path 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-item-accel-path object) => path}
  @syntax[]{(setf (gtk:menu-item-accel-path object) path)}
  @argument[item]{a valid @class{gtk:menu-item} widget}
  @argument[path]{a string with the accelerator path, corresponding to
    this functionality of the menu item, or @code{nil} to unset the current
    path}
  @begin{short}
    Accessor of the @slot[gtk:menu-item]{accel-path} slot of the
    @class{gtk:menu-item} class.
  @end{short}
  The @sym{gtk:menu-item-accel-path} function retrieve the accelerator path that
  was previously set on the menu item. The @sym{(setf gtk:menu-item-accel-path)}
  function sets the accelerator path on the menu item, through which runtime
  changes of the accelerator of the menu item caused by the user can be
  identified and saved to persistent storage, see the @fun{gtk:accel-map-save}
  function on this.

  To set up a default accelerator for this menu item, call the
  @fun{gtk:accel-map-add-entry} function with the same @arg{path}. See
  also the @fun{gtk:accel-map-add-entry} function on the specifics of
  accelerator paths, and the @fun{gtk:menu-accel-path} function for a more
  convenient variant of this function.

  This function is basically a convenience wrapper that handles calling
  the @fun{gtk:widget-set-accel-path} function with the appropriate accelerator
  group for the menu item.

  Note that you do need to set an accelerator on the parent menu with the
  @fun{gtk:menu-accel-group} function for this to work.
  @see-class{gtk:menu-item}
  @see-function{gtk:accel-map-save}
  @see-function{gtk:accel-map-add-entry}
  @see-function{gtk:menu-accel-path}
  @see-function{gtk:menu-accel-group}
  @see-function{gtk:widget-set-accel-path}")

;;; --- menu-item-label --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'menu-item) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text for the child label. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'menu-item-label)
      "Accessor"
      (documentation 'menu-item-label 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-item-label object) => label}
  @syntax[]{(setf (gtk:menu-item-label object) label)}
  @argument[object]{a @class{gtk:menu-item} widget}
  @argument[label]{a string with the text you want to set}
  @begin{short}
    Accessor of the @slot[gtk:menu-item]{label} slot of the
    @class{gtk:menu-item} class.
  @end{short}
  The @sym{gtk:menu-item-label} function gets the text on the menu item label.
  The @sym{(setf gtk:menu-item-label)} function sets the text.
  @see-class{gtk:menu-item}")

;;; --- menu-item-right-justified ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "right-justified" 'menu-item) t)
 "The @code{right-justified} property of type @code{:boolean} (Read / Write)
  @br{}
  Sets whether the menu item appears justified at the right side of a menu
  bar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-item-right-justified)
      "Accessor"
      (documentation 'menu-item-right-justified 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-item-right-justified object) => justified}
  @syntax[]{(setf (gtk:menu-item-right-justified object) justified)}
  @argument[item]{a @class{gtk:menu-item} widget}
  @argument[justified]{if @em{true} the menu item will appear at the far right
    if added to a menu bar}
  @begin{short}
    Accessor of the @slot[gtk:menu-item]{right-justified} slot of the
    @class{gtk:menu-item} class.
  @end{short}
  The @sym{gtk:menu-item-right-justified} function gets whether the menu item
  appears justified at the right side of the menu bar. The
  @sym{(setf gtk:menu-item-right-justified)} function sets whether the menu
  item appears justified at the right side.

  This was traditionally done for \"Help\" menu items, but is now considered a
  bad idea. If the widget layout is reversed for a right-to-left language like
  Hebrew or Arabic, right-justified menu items appear at the left.
  @begin[Warning]{dictionary}
    The @sym{gtk:menu-item-right-justified} function has been deprecated since
    version 3.2 and should not be used in newly written code. If you insist on
    using it, use the @fun{gtk:widget-hexpand} and @fun{gtk:widget-halign}
    functions.
  @end{dictionary}
  @see-class{gtk:menu-item}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-hexpand}")

;;; --- menu-item-submenu ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "submenu" 'menu-item) t)
 "The @code{submenu} property of type @class{gtk:menu} (Read / Write) @br{}
  The submenu attached to the menu item, or @code{nil} if it has none.")

#+liber-documentation
(setf (liber:alias-for-function 'menu-item-submenu)
      "Accessor"
      (documentation 'menu-item-submenu 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-item-submenu object) => submenu}
  @syntax[]{(setf (gtk:menu-item-submenu object) submenu)}
  @argument[object]{a @class{gtk:menu-item} widget}
  @argument[submenu]{a @class{gtk:menu} submenu, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:menu-item]{submenu} slot of the
    @class{gtk:menu-item} class.
  @end{short}
  The @sym{gtk:menu-item-submenu} function gets the submenu underneath this
  menu item, if any. The @sym{(setf gtk:menu-item-submenu)} function sets or
  replaces the submenu of the menu item, or removes it when a @code{nil}
  submenu is passed.
  @see-class{gtk:menu-item}
  @see-class{gtk:menu}")

;;; --- menu-item-use-underline ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'menu-item) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if underlines in the text indicate mnemonics. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-item-use-underline)
      "Accessor"
      (documentation 'menu-item-use-underline 'function)
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-item-use-underline object) => setting}
  @syntax[]{(setf (gtk:menu-item-use-underline object) setting)}
  @argument[item]{a @class{gtk:menu-item} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:menu-item]{use-underline} slot of the
    @class{gtk:menu-item} class.
  @end{short}
  The @sym{gtk:menu-item-use-underline} function checks if an underline in the
  text indicates the next character should be used for the mnemonic accelerator
  key. If @em{true}, an underline in the text indicates the next character
  should be used for the mnemonic accelerator key.
  @see-class{gtk:menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline menu-item-new))

(defun menu-item-new ()
 #+liber-documentation
 "@version{#2023-3-21}
  @return{The newly created @class{gtk:menu-item} widget.}
  @short{Creates a new menu item.}
  @see-class{gtk:menu-item}"
  (make-instance 'menu-item))

(export 'menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline menu-item-new-with-label))

(defun menu-item-new-with-label (label)
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[label]{a string with the text for the label}
  @return{The newly created @class{gtk:menu-item} widget.}
  @short{Creates a new menu item whose child is a @class{gtk:label} widget.}
  @see-class{gtk:menu-item}
  @see-class{gtk:label}
  @see-function{gtk:menu-item-new-with-mnemonic}"
  (make-instance 'menu-item
                 :label label))

(export 'menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_new_with_mnemonic" menu-item-new-with-mnemonic)
   (g:object menu-item)
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk:menu-item} widget.}
  @begin{short}
    Creates a new menu item containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the menu item.
  @see-class{gtk:menu-item}
  @see-function{gtk:menu-item-new-with-label}
  @see-function{gtk:label-new-with-mnemonic}"
  (label :string))

(export 'menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_select ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_select" menu-item-select) :void
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[item]{a @class{gtk:menu-item} widget}
  @short{Emits the \"select\" signal on the given menu item.}
  @see-class{gtk:menu-item}
  @see-function{gtk:menu-item-deselect}"
  (item (g:object menu-item)))

(export 'menu-item-select)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_deselect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_deselect" menu-item-deselect) :void
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[item]{a @class{gtk:menu-item} widget}
  @begin{short}
    Emits the \"deselect\" signal on the given menu item.
  @end{short}
  @see-class{gtk:menu-item}
  @see-function{gtk:menu-item-select}"
  (item (g:object menu-item)))

(export 'menu-item-deselect)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_activate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_activate" menu-item-activate) :void
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[item]{a @class{gtk:menu-item} widget}
  @begin{short}
    Emits the \"activate\" signal on the given menu item.
  @end{short}
  @see-class{gtk:menu-item}"
  (item (g:object menu-item)))

(export 'menu-item-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_request ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_toggle_size_request" menu-item-toggle-size-request)
    :void
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[item]{a @class{gtk:menu-item} widget}
  @argument[requisition]{an integer with the requisition to use as signal data}
  @begin{short}
    Emits the \"toggle-size-request\" signal on the given menu item.
  @end{short}
  @see-class{gtk:menu-item}
  @see-function{gtk:menu-item-toggle-size-allocate}"
  (item (g:object menu-item))
  (requisition :int))

(export 'menu-item-toggle-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_toggle_size_allocate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_menu_item_toggle_size_allocate" menu-item-toggle-size-allocate)
    :void
 #+liber-documentation
 "@version{#2023-3-21}
  @argument[item]{a @class{gtk:menu-item} widget}
  @argument[allocation]{an integer with the allocation to use as signal data}
  @begin{short}
    Emits the \"toggle-size-allocate\" signal on the given item.
  @end{short}
  @see-class{gtk:menu-item}
  @see-function{gtk:menu-item-toggle-size-request}"
  (item (g:object menu-item))
  (allocation :int))

(export 'menu-item-toggle-size-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_item_get_reserve_indicator
;;; gtk_menu_item_set_reserve_indicator -> menu-item-reserve-indicator
;;; ----------------------------------------------------------------------------

(defun (setf menu-item-reserve-indicator) (reserve item)
  (cffi:foreign-funcall "gtk_menu_item_set_reserve_indicator"
                        (g:object menu-item) item
                        :boolean reserve
                        :void)
  reserve)

(defcfun ("gtk_menu_item_get_reserve_indicator" menu-item-reserve-indicator)
    :boolean
 #+liber-documentation
 "@version{#2023-3-21}
  @syntax[]{(gtk:menu-item-reserve-indicator item) => reserve}
  @syntax[]{(setf (gtk:menu-item-reserve-indicator item) reserve)}
  @argument[item]{a @class{gtk:menu-item} widget}
  @argument[reserve]{a boolean whether the menu item always reserves space for
    the submenu indicator}
  @begin{short}
    Accessor of the reserve indicator of the menu item.
  @end{short}
  The @sym{gtk:menu-item-reserve-indicator} function returns whether the menu
  item reserves space for the submenu indicator, regardless if it has a submenu
  or not. The @sym{(setf gtk:menu-item-reserve-indicator)} function sets whether
  the menu item should reserve space. There should be little need for
  applications to call this functions.
  @see-class{gtk:menu-item}"
  (item (g:object menu-item)))

(export 'menu-item-reserve-indicator)

;;; --- End of file gtk3.menu-item.lisp ----------------------------------------
