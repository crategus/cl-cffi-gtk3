;;; ----------------------------------------------------------------------------
;;; gtk3.activatable.lisp
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

(gobject:define-ginterface "GtkActivatable" activatable
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
 "@version{2024-09-24}
  @begin{short}
    Activatable widgets can be connected to a @class{gtk:action} object and
    reflects the state of its action.
  @end{short}
  An activatable widget can also provide feedback through its action, as they
  are responsible for activating their related actions.
  @begin[Warning]{dictionary}
    The @class{gtk:activatable} interface has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-slot{gtk:activatable-related-action}
  @see-slot{gtk:activatable-use-action-appearance}
  @see-class{gtk:action}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:activatable-related-action -----------------------------------------

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
 "@version{2024-09-24}
  @syntax{(gtk:activatable-related-action object) => action}
  @syntax{(setf (gtk:activatable-related-action object) action)}
  @argument[activatable]{a @class{gtk:activatable} widget}
  @argument[action]{a @class{gtk:action} object to set}
  @begin{short}
    Accessor of the @slot[gtk:activatable]{related-action} slot of the
    @class{gtk:activatable} class.
  @end{short}
  The @fun{gtk:activatable-related-action} function gets the related action.
  The @setf{gtk:activatable-related-action} sets the related action.
  @begin[Warning]{dictionary}
    The @fun{gtk:activatable-related-action} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}
  @see-class{gtk:action}")

;;; --- gtk:activatable-use-action-appearance ----------------------------------

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
 "@version{2024-09-24}
  @syntax{(gtk:activatable-use-action-appearance object) => use-appearance}
  @syntax{(setf (gtk:activatable-use-action-appearance object) use-appearance)}
  @argument[activatable]{a @class{gtk:activatable} widget}
  @argument[use-appearance]{a boolean whether to use the actions appearance}
  @begin{short}
    Accessor of the @slot[gtk:activatable]{use-action-appearance} slot of the
    @class{gtk:activatable} class.
  @end{short}
  The @fun{gtk:activatable-use-action-appearance} function gets whether the
  activatable should reset its layout and appearance when setting the related
  action or when the action changes appearance. The
  @setf{gtk:activatable-use-action-appearance} function sets whether this
  activatable should reset its layout and appearance.
  @begin[Warning]{dictionary}
    The @fun{gtk:activatable-use-action-appearance} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}")

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_do_set_related_action                   not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_activatable_do_set_related_action"
               activatable-do-set-related-action) :void
 #+liber-documentation
 "@version{#2023-01-21}
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
  @begin[Notes]{dictionary}
    Be careful to call this before setting the local copy of the
    @class{gtk:action} object property, since this function uses
    @fun{gtk:activatable-get-related-action} to retrieve the previous action.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:activatable-do-set-related-action} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}
  @see-class{gtk:action}
  @see-function{gtk:activatable-get-related-action}"
  (activatable (g:object activatable))
  (action (g:object action)))

;;; ----------------------------------------------------------------------------
;;; gtk_activatable_sync_action_properties ()               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_activatable_sync_action_properties"
               activatable-sync-action-properties) :void
 #+liber-documentation
 "@version{#2023-01-21}
  @argument[activatable]{a @class{gtk:activatable} object}
  @argument[action]{a related @class{gtk:action} object or @code{nil}}
  @begin{short}
    This is called to update the @arg{activatable} object completely, this is
    called internally when the @slot[gtk:activatable]{related-action} property
    is set or unset and by the implementing class when
    @slot[gtk:activatable]{use-action-appearance} changes.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:activatable-sync-action-properties} function has been
    deprecated since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:activatable}
  @see-class{gtk:action}"
  (activatable (g:object activatable))
  (action (g:object action)))

;;; --- End of file gtk3.activatable.lisp --------------------------------------
