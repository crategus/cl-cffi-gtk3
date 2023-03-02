;;; ----------------------------------------------------------------------------
;;; gtk3.activatable.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkActivatable
;;;
;;;     An interface for activatable widgets
;;;
;;; Types and Values
;;;
;;;     GtkActivatable
;;;
;;; Functions
;;;
;;;     gtk_activatable_do_set_related_action
;;;     gtk_activatable_get_related_action
;;;     gtk_activatable_get_use_action_appearance
;;;     gtk_activatable_sync_action_properties
;;;     gtk_activatable_set_related_action
;;;     gtk_activatable_set_use_action_appearance
;;;
;;; Properties
;;;
;;;     related-action
;;;     use-action-appearance
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkActivatable
;;;
;;; Prerequisites
;;;
;;;     GtkActivatable requires GObject.
;;;
;;; Known Implementations
;;;
;;;     GtkActivatable is implemented by GtkButton, GtkCheckButton,
;;;     GtkCheckMenuItem, GtkColorButton, GtkFontButton, GtkImageMenuItem,
;;;     GtkLinkButton, GtkLockButton, GtkMenuButton, GtkMenuItem,
;;;     GtkMenuToolButton, GtkModelButton, GtkRadioButton, GtkRadioMenuItem,
;;;     GtkRadioToolButton, GtkRecentChooserMenu, GtkScaleButton,
;;;     GtkSeparatorMenuItem, GtkSeparatorToolItem, GtkSwitch,
;;;     GtkTearoffMenuItem, GtkToggleButton, GtkToggleToolButton,
;;;     GtkToolButton, GtkToolItem and GtkVolumeButton.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActivatable
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkActivatable" activatable
  (:export t
   :type-initializer "gtk_activatable_get_type")
  ((related-action
    activatable-related-action
    "related-action" "GtkAction" t t)
   (use-action-appearance
    activatable-use-action-appearance
    "use-action-appearance" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'activatable)
      "Interface"
      (documentation 'activatable 'type)
 "@version{2023-2-27}
  @begin{short}
    Activatable widgets can be connected to a @class{gtk:action} object and
    reflects the state of its action.
  @end{short}
  An activatable widget can also provide feedback through its action, as they
  are responsible for activating their related actions.
  @begin[Warning]{dictionary}
    The @sym{gtk:activatable} interface has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-slot{gtk:activatable-related-action}
  @see-slot{gtk:activatable-use-action-appearance}
  @see-class{gtk:action}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- activatable-related-action ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "related-action" 'activatable) t)
 "The @code{related-action} property of type @class{gtk:action} (Read / Write)
  @br{}
  The action that the activatable will activate and receive updates from for
  various states and possibly appearance.")

#+liber-documentation
(setf (liber:alias-for-function 'activatable-related-action)
      "Accessor"
      (documentation 'activatable-related-action 'function)
 "@version{2023-2-27}
  @syntax[]{(gtk:activatable-related-action object) => action}
  @syntax[]{(setf (gtk:activatable-related-action object) action)}
  @argument[activatable]{a @class{gtk:activatable} widget}
  @argument[action]{a @class{gtk:action} object to set}
  @begin{short}
    Accessor of the @slot[gtk:activatable]{related-action} slot of the
    @class{gtk:activatable} class.
  @end{short}
  The @sym{gtk:activatable-related-action} function gets the related action.
  The @sym{(setf gtk:activatable-related-action)} sets the related action.
  @begin[Warning]{dictionary}
    The @sym{gtk:activatable-related-action} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}
  @see-class{gtk:action}")

;;; --- activatable-use-action-appearance --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-action-appearance"
                                               'activatable) t)
 "The @code{use-action-appearance} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the activatable should reset its layout and appearance when setting
  the related action or when the action changes appearance. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'activatable-use-action-appearance)
      "Accessor"
      (documentation 'activatable-use-action-appearance 'function)
 "@version{2023-2-27}
  @syntax[]{(gtk:activatable-use-action-appearance object) => action}
  @syntax[]{(setf (gtk:activatable-use-action-appearance object) action)}
  @argument[activatable]{a @class{gtk:activatable} widget}
  @argument[use-appearance]{a boolean whether to use the actions appearance}
  @begin{short}
    Accessor of the @slot[gtk:activatable]{use-action-appearance} slot of the
    @class{gtk:activatable} class.
  @end{short}
  The @sym{gtk:activatable-use-action-appearance} function gets whether the
  activatable should reset its layout and appearance when setting the related
  action or when the action changes appearance. The
  @sym{(setf gtk:activatable-use-action-appearance)} function sets whether this
  activatable should reset its layout and appearance.
  @begin[Warning]{dictionary}
    The @sym{gtk:activatable-use-action-appearance} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}")

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_do_set_related_action ()               not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_activatable_do_set_related_action"
           activatable-do-set-related-action) :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[activatable]{a @class{gtk:activatable} object}
  @argument[action]{the @class{gtk:action} object to set}
  @begin{short}
    This is a utility function for @class{gtk:activatable} interface
    implementors.
  @end{short}
  When implementing the @class{gtk:activatable} interface you must call this
  when handling changes of the @slot[gtk:activatable]{related-action} property,
  and you must also use this to break references in @code{GObject->dispose()}.

  This function adds a reference to the currently set related action for you,
  it also makes sure the @code{GtkActivatable->update()} method is called when
  the related @class{gtk:action} object properties change and registers to the
  action's proxy list.
  @begin[Note]{dictionary}
    Be careful to call this before setting the local copy of the
    @class{gtk:action} object property, since this function uses
    @fun{gtk:activatable-get-related-action} to retrieve the previous action.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @sym{gtk:activatable-do-set-related-action} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}
  @see-class{gtk:action}
  @see-function{gtk:activatable-get-related-action}"
  (activatable (g:object activatable))
  (action (g:object action)))

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_sync_action_properties ()              not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_activatable_sync_action_properties"
           activatable-sync-action-properties) :void
 #+liber-documentation
 "@version{#2023-1-21}
  @argument[activatable]{a @class{gtk:activatable} object}
  @argument[action]{a related @class{gtk:action} object or @code{nil}}
  @begin{short}
    This is called to update the @arg{activatable} object completely, this is
    called internally when the @slot[gtk:activatable]{related-action} property
    is set or unset and by the implementing class when
    @slot[gtk:activatable]{use-action-appearance} changes.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gtk:activatable-sync-action-properties} function has been
    deprecated since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}
  @see-class{gtk:action}"
  (activatable (g:object activatable))
  (action (g:object action)))

;;; --- End of file gtk3.activatable.lisp --------------------------------------
