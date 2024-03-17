;;; ----------------------------------------------------------------------------
;;; gtk3.recent-action.lisp
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
;;; GtkRecentAction
;;;
;;;     An action of which represents a list of recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentAction
;;;
;;; Functions
;;;
;;;     gtk_recent_action_new
;;;     gtk_recent_action_new_for_manager
;;;     gtk_recent_action_get_show_numbers                 Accessor
;;;     gtk_recent_action_set_show_numbers                 Accessor
;;;
;;; Properties
;;;
;;;     show-numbers
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ╰── GtkRecentAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentAction implements GtkBuildable and GtkRecentChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentAction
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkRecentAction" recent-action
  (:superclass action
   :export t
   :interfaces ("GtkBuildable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_action_get_type")
  ((show-numbers
    recent-action-show-numbers
    "show-numbers" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'recent-action 'type)
 "@version{#2023-3-24}
  @begin{short}
    A @sym{gtk:recent-action} object represents a list of recently used files,
    which can be shown by widgets such as a @class{gtk:recent-chooser-dialog}
    widget or a @class{gtk:recent-chooser-menu} widget.
  @end{short}

  To construct a submenu showing recently used files, use a
  @sym{gtk:recent-action} object as the action for a menu item. To construct a
  menu toolbutton showing the recently used files in the popup menu, use a
  @sym{gtk:recent-action} object as the action for a tool item.
  @begin[Warning]{dictionary}
    The @sym{gtk:recent-action} class has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-constructor{gtk:recent-acton-new}
  @see-constructor{gtk:recent-action-new-for-manager}
  @see-slot{gtk:recent-action-show-numbers}
  @see-class{gtk:action}
  @see-class{gtk:recent-chooser-dialog}
  @see-class{gtk:recent-chooser-menu}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-numbers" 'recent-action) t)
 "The @code{show-numbers} property of type @code{:boolean} (Read / Write) @br{}
  Whether the items should be displayed with a number. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-action-show-numbers)
      "Accessor"
      (documentation 'recent-action-show-numbers 'function)
 "@version{#2023-3-24}
  @syntax{(gtk:recent-action-show-numbers object) => show-numbers}
  @syntax{(setf (gtk:recent-action-show-numbers object) show-numbers)}
  @argument[object]{a @class{gtk:recent-action} object}
  @argument[show-numbers]{@em{true} if the shown items should be numbered}
  @begin{short}
    Accessor of the @slot[gtk:recent-action]{show-numbers} slot of the
    @class{gtk:recent-action} class.
  @end{short}
  The @sym{gtk:recent-action-show-numbers} function sets whether a number
  should be added to the items shown by the widgets representing the action.
  The numbers are shown to provide a unique character for a mnemonic to be used
  inside the menu label of the item. Only the first ten items get a number to
  avoid clashes.
  @begin[Warning]{dictionary}
    The @sym{gtk:recent-action-show-numbers} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:recent-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_action_new" recent-action-new) (g:object action)
 "@version{#2023-3-24}
  @argument[name]{a string with a unique name for the action}
  @argument[label]{a string with the label displayed in menu items and on
    buttons, or @code{nil}}
  @argument[tooltip]{a string with a tooltip for the action, or @code{nil}}
  @argument[stock-id]{a string with the stock icon to display in widgets
    representing the action, or @code{nil}}
  @return{The newly created @class{gtk:recent-action} object.}
  @begin{short}
    Creates a new recent action.
  @end{short}
  To add the action to a @class{gtk:action-group} object and set the accelerator
  for the action, call the @fun{gtk:action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @sym{gtk:recent-action-new} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:recent-action}
  @see-class{gtk:action-group}
  @see-function{gtk:action-group-add-action}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string))

(export 'recent-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_action_new_for_manager ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_action_new_for_manager"
               recent-action-new-for-manager) (g:object action)
 "@version{#2023-3-24}
  @argument[name]{a string with a unique name for the action}
  @argument[label]{a string with the label displayed in menu items and on
    buttons, or @code{nil}}
  @argument[tooltip]{a string with a tooltip for the action, or @code{nil}}
  @argument[stock-id]{a string with the stock icon to display in widgets
    representing the action, or @code{nil}}
  @argument[manager]{a @class{gtk:recent-manager} object, or @code{nil} for
    using the default recent manager}
  @return{The newly created @class{gtk:recent-action} object.}
  @begin{short}
    Creates a new recent action.
  @end{short}
  To add the action to a @class{gtk:action-group} object and set the accelerator
  for the action, call the @fun{gtk:action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @sym{gtk:recent-action-new-manager} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:recent-action}
  @see-class{gtk:action-group}
  @see-class{gtk:recent-manager}
  @see-function{gtk:action-group-add-action}"
  (name :string)
  (label :string)
  (tooltip :string)
  (stock-id :string)
  (manager (g:object recent-manager)))

(export 'recent-action-new-for-manager)

;;; --- End of file gtk3.recent-action.lisp ------------------------------------
