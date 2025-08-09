;;; ----------------------------------------------------------------------------
;;; gtk3.radio-action.lisp
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
;;; GtkRadioAction
;;;
;;;     An action of which only one in a group can be active
;;;
;;; Types and Values
;;;
;;;     GtkRadioAction
;;;
;;; Accessors
;;;
;;;     gtk_radio_action_get_current_value
;;;     gtk_radio_action_set_current_value
;;;     gtk_radio_action_get_group
;;;     gtk_radio_action_set_group
;;;
;;; Functions
;;;
;;;     gtk_radio_action_new
;;;     gtk_radio_action_join_group
;;;
;;; Properties
;;;
;;;     current-value
;;;     group
;;;     value
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkAction
;;;         ╰── GtkToggleAction
;;;             ╰── GtkRadioAction
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRadioAction implements GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRadioAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkRadioAction" radio-action
  (:superclass toggle-action
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_radio_action_get_type")
  ((current-value
    radio-action-current-value
    "current-value" "gint" t t)
   (group
    radio-action-group
    "group" "GtkRadioAction" nil t)
   (value
    radio-action-value
    "value" "gint" t t)))

#+liber-documentation
(setf (documentation 'radio-action 'type)
 "@version{2024-9-26}
  @begin{short}
    The @class{gtk:radio-action} object is similar to the
    @class{gtk:radio-menu-item} widget.
  @end{short}
  A number of radio actions can be linked together so that only one may be
  active at any one time.
  @begin[Warning]{dictionary}
    The @class{gtk:radio-action} class has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[radio-action::changed]{signal}
      @begin{pre}
lambda (action current)    :no-recurse
      @end{pre}
      @begin[code]{simple-table}
        @entry[action]{The @class{gtk:radio-action} object on which the signal
          is emitted.}
        @entry[current]{The @class{gtk:radio-action} member of the action group
          which has just been activated.}
      @end{simple-table}
      The signal is emitted on every member of a radio group when the active
      member is changed. The signal gets emitted after the @code{\"activate\"}
      signals for the previous and current active members.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:radio-action-new}
  @see-slot{gtk:radio-action-current-value}
  @see-slot{gtk:radio-action-group}
  @see-slot{gtk:radio-action-value}
  @see-class{gtk-action}
  @see-class{gtk:radio-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:radio-action-current-value -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-value" 'radio-action) t)
 "The @code{current-value} property of type @code{:int} (Read / Write) @br{}
  The value property of the currently active member of the group to which this
  action belongs. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'radio-action-current-value)
      "Accessor"
      (documentation 'radio-action-current-value 'function)
 "@version{2025-07-06}
  @syntax{(gtk:radio-action-current-value object) => current-value}
  @syntax{(setf (gtk:radio-action-current-value object) current-value)}
  @argument[object]{a @class{gtk:radio-action} object}
  @argument[current-value]{an integer for the value}
  @begin{short}
    Accessor of the @slot[gtk:radio-action]{current-value} slot of the
    @class{gtk:radio-action} class.
  @end{short}
  The @fun{gtk:radio-action-current-value} function obtains the value property
  of the currently active member of the group to which the radio action belongs.
  The @setf{gtk:radio-action-current-value} function sets the currently active
  group member.
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-action-current-value} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:radio-action}")

;;; --- gtk:radio-action-group -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group" 'radio-action) t)
 "The @code{group} property of type @class{gtk:radio-action} (Write) @br{}
  Sets a new group for a radio action. @em{Note:} This property is not
  readable und cannot be set from the Lisp side.")

#+liber-documentation
(setf (liber:alias-for-function 'radio-action-group)
      "Accessor"
      (documentation 'radio-action-group 'function)
 "@version{2024-9-26}
  @syntax{(setf (gtk:radio-action-group object) group)}
  @argument[object]{a @class{gtk:radio-action} object}
  @argument[group]{a list of @class{gtk:radion-action} objects representing a
    radio group}
  @begin{short}
    Accessor of the @slot[gtk:radio-action]{group} slot of the
    @class{gtk:radio-action} class.
  @end{short}
  The @setf{gtk:radio-action-group} function sets the radio group.
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-action-group} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:radio-action}
  @see-function{gtk:radio-action-join-group}")

;;; --- gtk:radio-action-value -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value" 'radio-action) t)
 "The @code{value} property of type @code{:int} (Read / Write) @br{}
  The value is an integer which can be used as a convenient way to determine
  which action in the group is currently active in an @code{\"activate\"} or
  @code{\"changed\"} signal handler. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'radio-action-value)
      "Accessor"
      (documentation 'radio-action-value 'function)
 "@version{2025-07-06}
  @syntax{(gtk:radio-action-value object) => value}
  @syntax{(setf (gtk:radio-action-value object) value)}
  @argument[object]{a @class{gtk:radio-action} object}
  @argument[value]{an integer for the value which can be used to determine
    which action is active}
  @begin{short}
    Accessor of the @slot[gtk:radio-action]{value} slot of the
    @class{gtk:radio-action} class.
  @end{short}
  The value is an integer which can be used as a convenient way to determine
  which action in the group is currently active in an @code{\"activate\"} or
  @code{\"changed\"} signal handler.
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-action-value} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:radio-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_new
;;; ----------------------------------------------------------------------------

(defun radio-action-new (name &optional label tooltip stock-id value)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[name]{a string for the unique name for the action}
  @argument[label]{a string for the label displayed in menu items and on
    buttons, or @code{nil}}
  @argument[tooltip]{a string for the tooltip for this action, or @code{nil}}
  @argument[stock-id]{a string for the stock icon to display in widgets
    representing this action, or @code{nil}}
  @argument[value]{an integer for the value which the
    @fun{gtk:radio-action-current-value} function should return if this action
    is selected}
  @return{The new @class{gtk:radio-action} object.}
  @begin{short}
    Creates a new radio action.
  @end{short}
  To add the action to a @class{gtk-action-group} object and set the accelerator
  for the action, call the @fun{gtk-action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-action-new} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:radio-action}
  @see-class{gtk-action-group}
  @see-function{gtk:radio-action-current-value}
  @see-function{gtk-action-group-add-action}"
  (make-instance 'radio-action
                 :name name
                 :label (or label (cffi:null-pointer))
                 :tooltip (or tooltip (cffi:null-pointer))
                 :stock-id (or stock-id (cffi:null-pointer))
                 :value (or value 0)))

(export 'radio-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_radio_action_join_group ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_radio_action_join_group" radio-action-join-group) :void
 #+liber-documentation
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:radio-action} object}
  @argument[source]{a @class{gtk:radio-action} object whose group we are
    joining, or @code{nil} to remove the radio action from its group}
  @begin{short}
    Joins a radio action object to the group of another radio action object.
  @end{short}
  Use this in language bindings instead of the the @fun{gtk:radio-action-group}
  function.
  @begin[Examples]{dictionary}
    A common way to set up a group of radio actions is the following:
    @begin{pre}
(let ((actions '((\"action0\" \"label\" \"tooltip\" \"stock-id\" 0)
                 ...
                )
      (lastaction nil))
   (dolist (action actions)
     (setf action (apply #'gtk:radio-action-new action))
     (gtk:radio-action-join-group action lastaction)
     (setf lastaction action))
     ... ))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:radio-action-join-group} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:radio-action}
  @see-function{gtk:radio-action-group}"
  (action (g:object radio-action))
  (source (g:object radio-action)))

(export 'radio-action-join-group)

;;; --- End of file gtk3.radio-action.lisp -------------------------------------
