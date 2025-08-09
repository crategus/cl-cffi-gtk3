;;; ----------------------------------------------------------------------------
;;; gtk3.toggle-action.lisp
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
;;; GtkToggleAction
;;;
;;;     An action which can be toggled between two states.
;;;
;;; Types and Values
;;;
;;;     GtkToggleAction
;;;
;;; Accessors
;;;
;;;     gtk_toggle_action_set_active
;;;     gtk_toggle_action_get_active
;;;     gtk_toggle_action_set_draw_as_radio
;;;     gtk_toggle_action_get_draw_as_radio
;;;
;;; Functions
;;;
;;;     gtk_toggle_action_new
;;;     gtk_toggle_action_toggled
;;;
;;; Properties
;;;
;;;     active
;;;     draw-as-radio
;;;
;;; Signals
;;;
;;;     toggled
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
;;;     GtkToggleAction implements GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkToggleAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkToggleAction" toggle-action
  (:superclass action
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_toggle_action_get_type")
  ((active
    toggle-action-active
    "active" "gboolean" t t)
   (draw-as-radio
    toggle-action-draw-as-radio
    "draw-as-radio" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'toggle-action 'type)
 "@version{2025-06-27}
  @begin{short}
    The @class{gtk:toggle-action} object corresponds roughly to a
    @class{gtk:check-menu-item} widget.
  @end{short}
  It has an \"active\" state specifying whether the action has been checked or
  not.
  @begin[Warning]{dictionary}
    The @class{gtk:toggle-action} class has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[toggle-action::toggled]{signal}
      @begin{pre}
lambda (action)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[action]{The @class{gtk:toggle-action} object that received the
          signal.}
      @end{simple-table}
      Should be connected if you wish to perform an action whenever the toggle
      action state is changed.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:toggle-action-new}
  @see-slot{gtk:toggle-action-active}
  @see-slot{gtk:toggle-action-draw-as-radio}
  @see-class{gtk:action}
  @see-class{gtk:check-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:toggle-action-active -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'toggle-action) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle action should be active. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-action-active)
      "Accessor"
      (documentation 'toggle-action-active 'function)
 "@version{2024-9-26}
  @syntax{(gtk:toggle-action-active object) => is-active}
  @syntax{(setf (gtk:toggle-action-active object) is-active)}
  @argument[object]{a @class{gtk:toggle-action} object}
  @argument[is-active]{a boolean whether the action should be checked or not}
  @begin{short}
    Accessor of the @slot[gtk:toggle-action]{active} slot of the
    @class{gtk:toggle-action} class.
  @end{short}
  The @fun{gtk:toggle-action-active} function returns the checked state of the
  toggle action. The @setf{gtk:toggle-action-active} function sets the checked
  state.
  @begin[Warning]{dictionary}
    The @fun{gtk:toggle-action-active} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:toggle-action}")

;;; --- gtk:toggle-action-draw-as-radio ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw-as-radio"
                                               'toggle-action) t)
 "The @code{draw-as-radio} property of type @code{:boolean} (Read / Write) @br{}
  Whether the proxies for this action look like radio action proxies. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-action-draw-as-radio)
      "Accessor"
      (documentation 'toggle-action-draw-as-radio 'function)
 "@version{2024-9-26}
  @syntax{(gtk:toggle-action-draw-as-radio object) => draw-as-radio}
  @syntax{(setf (gtk:toggle-action-draw-as-radio object) draw-as-radio)}
  @argument[object]{a @class{gtk:toggle-action} object}
  @argument[draw-as-radio]{a boolean whether the action should have proxies
    like a radio action}
  @begin{short}
    Accessor of the @slot[gtk:toggle-action]{draw-as-radio} slot of the
    @class{gtk:toggle-action} class.
  @end{short}
  The @fun{gtk:toggle-action-draw-as-radio} function returns whether the action
  should have proxies like a radio action. The
  @setf{gtk:toggle-action-draw-as-radio} function sets whether the action should
  have proxies.
  @begin[Warning]{dictionary}
    The @fun{gtk:toggle-action-draw-as-radio} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:toggle-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_new
;;; ----------------------------------------------------------------------------

(defun toggle-action-new (name &optional label tooltip stock-id)
 "@version{2025-07-07}
  @argument[name]{a string for a unique name for the action}
  @argument[label]{an optional string for the label displayed in menu items
    and on buttons, or @code{nil}}
  @argument[tooltip]{an optional string for a tooltip for the action, or
    @code{nil}}
  @argument[stock-id]{an optional string for the stock icon to display in
    widgets representing the action, or @code{nil}}
  @return{The new @class{gtk:toggle-action} object.}
  @begin{short}
    Creates a new toggle action.
  @end{short}
  To add the action to a @class{gtk:action-group} object and set the accelerator
  for the action, call the @fun{gtk:action-group-add-action} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:toggle-action-new} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:toggle-action}
  @see-class{gtk:action-group}
  @see-function{gtk:action-group-add-action}"
  (make-instance 'toggle-action
                 :name name
                 :label (or label (cffi:null-pointer))
                 :tooltip (or tooltip (cffi:null-pointer))
                 :stock-id (or stock-id (cffi:null-pointer))))

(export 'toggle-action-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_action_toggled
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_toggle_action_toggled" toggle-action-toggled) :void
 "@version{2024-9-26}
  @argument[action]{a @class{gtk:toggle-action} object}
  @begin{short}
    Emits the @code{\"toggled\"} signal on the toggle action.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:toggle-action-toggled} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:toggle-action}"
  (action (g:object toggle-action)))

(export 'toggle-action-toggled)

;;; --- End of file gtk3.toggle-action.lisp ------------------------------------
