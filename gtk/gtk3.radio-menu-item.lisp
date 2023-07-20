;;; ----------------------------------------------------------------------------
;;; gtk3.radio-menu.item.lisp
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
;;; GtkRadioMenuItem
;;;
;;;     A choice from multiple check menu items
;;;
;;; Types and Values
;;;
;;;     GtkRadioMenuItem
;;;
;;; Functions
;;;
;;;     gtk_radio_menu_item_new
;;;     gtk_radio_menu_item_new_with_label
;;;     gtk_radio_menu_item_new_with_mnemonic
;;;     gtk_radio_menu_item_new_from_widget
;;;     gtk_radio_menu_item_new_with_label_from_widget
;;;     gtk_radio_menu_item_new_with_mnemonic_from_widget
;;;     gtk_radio_menu_item_set_group
;;;     gtk_radio_menu_item_get_group
;;;     gtk_radio_menu_item_join_group
;;;
;;; Properties
;;;
;;;     group
;;;
;;; Signals
;;;
;;;     group-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkCheckMenuItem
;;;                             ╰── GtkRadioMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRadioMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRadioMenuItem
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkRadioMenuItem" radio-menu-item
  (:superclass check-menu-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_radio_menu_item_get_type")
  ((group
    radio-menu-item-group
    "group" "GtkRadioMenuItem" nil t)))

#+liber-documentation
(setf (documentation 'radio-menu-item 'type)
 "@version{2023-2-24}
  @begin{short}
    A radio menu item is a check menu item that belongs to a group. At each
    instant exactly one of the radio menu items from a group is selected.
  @end{short}

  The correct way to create a group of radio menu items is approximatively
  this:

  @b{Example:} How to create a group of radio menu items.
  @begin{pre}
(let (menu-item last-menu-item)
  ;; Add three menu items to a group
  (dolist (label '(\"First Menu Item\" \"Second Menu Item\" \"Third Menu Item\"))
    (setf menu-item (gtk:radio-menu-item-new-with-label nil label))
    (gtk:radio-menu-item-join-group menu-item last-menu-item)
    (setf last-menu-item menu-item)))
  @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"group-changed\" signal}
      @begin{pre}
lambda (radiomenuitem)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[radiomenuitem]{The @sym{gtk:radio-menu-item} widget which
          received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:radio-menu-item-new}
  @see-constructor{gtk:radio-menu-item-new-with-label}
  @see-constructor{gtk:radio-menu-item-new-with-mnemonic}
  @see-constructor{gtk:radio-menu-item-new-from-widget}
  @see-constructor{gtk:radio-menu-item-new-with-label-from-widget}
  @see-constructor{gtk:radio-menu-item-new-mnemonic-from-widget}
  @see-slot{gtk:radio-menu-item-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group" 'radio-menu-item) t)
 "The @code{group} property of type @class{gtk:radio-menu-item} (Write) @br{}
  The radio menu item whose group this widget belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'radio-menu-item-group)
      "Accessor"
      (documentation 'radio-menu-item-group 'function)
 "@version{2023-2-24}
  @begin{short}
    Accessor of the @slot[gtk:radio-menu-item]{group} slot of the
    @class{gtk:radio-menu-item} class.
  @end{short}
  @see-class{gtk:radio-menu-item}
  @see-function{gtk:radio-menu-item-get-group}
  @see-function{gtk:radio-menu-item-set-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_new" radio-menu-item-new)
    (g:object radio-menu-item)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets with the radio
    group to which the radio menu item is to be attached or @code{nil}}
  @return{A new @class{gtk:radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item.
  @end{short}
  @see-class{gtk:radio-menu-item}"
  (group (g:slist-t (g:object radio-menu-item))))

(export 'radio-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_new_with_label"
               radio-menu-item-new-with-label) (g:object radio-menu-item)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets with the radio
    group to which the radio menu item is to be attached or @code{nil}}
  @argument[label]{a string with the text for the label}
  @return{A new @class{gtk:radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item whose child is a simple @class{gtk:label}
    widget.
  @end{short}
  @see-class{gtk:radio-menu-item}"
  (group (g:slist-t (g:object radio-menu-item)))
  (label :string))

(export 'radio-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_new_with_mnemonic"
               radio-menu-item-new-with-mnemonic) (g:object radio-menu-item)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets with the radio
    group to which the radio menu item is to be attached or @code{nil}}
  @argument[label]{a string with the text of the radio menu item, with an
    underscore in front of the mnemonic character}
  @return{A new @class{gtk:radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in the label indicate the mnemonic for the menu item.
  @see-class{gtk:radio-menu-item}"
  (group (g:slist-t (g:object radio-menu-item)))
  (label :string))

(export 'radio-menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_from_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_new_from_widget"
               radio-menu-item-new-from-widget) (g:object radio-menu-item)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets with the radio
    group to which the radio menu item is to be attached or @code{nil}}
  @return{A new @class{gtk:radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item adding it to the same group as @arg{group}.
  @end{short}
  @see-class{gtk:radio-menu-item}
  @see-function{gtk:radio-menu-item-new-with-label-from-widget}"
  (group (g:object radio-menu-item)))

(export 'radio-menu-item-new-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_label_from_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_new_with_label_from_widget"
               radio-menu-item-new-with-label-from-widget)
    (g:object radio-menu-item)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets with the radio
    group to which the radio menu item is to be attached or @code{nil}}
  @argument[label]{a string with the text for the label}
  @return{A new @class{gtk:radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item whose child is a simple @class{gtk:label}
    widget.
  @end{short}
  The new radio menu item is added to the same group as @arg{group}.
  @see-class{gtk:radio-menu-item}"
  (group (g:object radio-menu-item))
  (label :string))

(export 'radio-menu-item-new-with-label-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_new_with_mnemonic_from_widget ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_new_with_mnemonic_from_widget"
               radio-menu-item-new-with-mnemonic-from-widget)
    (g:object radio-menu-item)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets with the radio
    group to which the radio menu item is to be attached or @code{nil}}
  @argument[label]{a string with the text of the radio menu item, with an
    underscore in front of the mnemonic character}
  @return{A new @class{gtk:radio-menu-item} widget.}
  @begin{short}
    Creates a new radio menu item containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the menu item.

  The new radio menu item is added to the same group as @arg{group}.
  @see-class{gtk:radio-menu-item}"
  (group (g:object radio-menu-item))
  (label :string))

(export 'radio-menu-item-new-with-mnemonic-from-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_set_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_set_group" radio-menu-item-set-group) :void
 #+liber-documentation
 "@version{2023-2-24}
  @argument[item]{a @class{gtk:radio-menu-item} widget}
  @argument[group]{a list of @class{gtk:radio-menu-item} widgets for the group}
  @begin{short}
    Sets the group of a radio menu item, or changes it.
  @end{short}
  @see-class{gtk:radio-menu-item}"
  (item (g:object radio-menu-item))
  (group (g:slist-t (g:object radio-menu-item) :free-from-foreign nil)))

(export 'radio-menu-item-set-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_get_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_get_group" radio-menu-item-get-group)
    (g:slist-t (g:object radio-menu-item) :free-from-foreign nil)
 #+liber-documentation
 "@version{2023-2-24}
  @argument[item]{a @class{gtk:radio-menu-item} widget}
  @return{The list @class{gtk:radio-menu-item} widgets in the group of
    @arg{item}.}
  @begin{short}
    Returns the group to which the radio menu item belongs, as a list of
    @class{gtk:radio-menu-item} widgets.
  @end{short}
  @see-class{gtk:radio-menu-item}"
  (item (g:object radio-menu-item)))

(export 'radio-menu-item-get-group)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_menu_item_join_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_menu_item_join_group" radio-menu-item-join-group)
    :void
 #+liber-documentation
 "@version{2023-3-13}
  @argument[item]{a @class{gtk:radio-menu-item} widget}
  @argument[group]{a @class{gtk:radio-menu-item} widget whose group we
    are joining, or @code{nil} to remove @arg{item} from its current group}
  @begin{short}
    Joins a radio menu item to the group of another radio menu item.
  @end{short}
  This function should be used by language bindings to avoid the memory
  manangement of the opaque @code{g:slist-t} structure of the
  @fun{gtk:radio-menu-item-get-group} and @fun{gtk:radio-menu-item-set-group}
  functions.
  @begin[Example]{dictionary}
  A common way to set up a group of radio menu item instances is:
    @begin{pre}
(let (item lastitem)
  ;; Add three menu items to a group
  (dolist (label '(\"First Menu Item\" \"Second Menu Item\" \"Third Menu Item\"))
    (setf item (gtk:radio-menu-item-new-with-label nil label))
    (gtk:radio-menu-item-join-group item lastitem)
    (setf lastitem item)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:radio-menu-item}"
  (item (g:object radio-menu-item))
  (group (g:object radio-menu-item)))

(export 'radio-menu-item-join-group)

;;; --- End of file gtk3.radio-menu-item.lisp ----------------------------------
