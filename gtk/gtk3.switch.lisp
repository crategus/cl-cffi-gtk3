;;; ----------------------------------------------------------------------------
;;; gtk3.switch.lisp
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
;;; GtkSwitch
;;;
;;;     A "light switch" style toggle
;;;
;;; Types and Values
;;;
;;;     GtkSwitch
;;;
;;; Functions
;;;
;;;     gtk_switch_new
;;;     gtk_switch_set_active
;;;     gtk_switch_get_active
;;;     gtk_switch_set_state
;;;     gtk_switch_get_state
;;;
;;; Properties
;;;
;;;     active
;;;     state
;;;
;;; Style Properties
;;;
;;;     slider-height
;;;     slider-width
;;;
;;; Signals
;;;
;;;     activate
;;;     state-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSwitch
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSwitch implements AtkImplementorIface, GtkBuildable, GtkActionable
;;;     and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSwitch
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSwitch" switch
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_switch_get_type")
  ((active
    switch-active
    "active" "gboolean" t t)
   (state
    switch-state
    "state" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'switch 'type)
 "@version{#2025-06-18}
  @begin{short}
    The @class{gtk:switch} widget is a widget that has two states: on or off.
  @end{short}

  @image[switch]{Figure: GtkSwitch}

  The user can control which state should be active by clicking the switch,
  or by dragging the handle.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 switch
 ╰── slider
    @end{pre}
    The @class{gtk:switch} implementation has two CSS nodes, the main node with
    the name @code{switch} and a subnode named @code{slider}. Neither of them
    is using any style classes.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[slider-height]{entry}
        The @code{slider-height} style property of type @code{:int} (Read) @br{}
        The minimum height of the switch handle, in pixels. @br{}
        @em{Warning:} The @code{slider-height} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the CSS @code{min-height} property instead. @br{}
        Allowed values: >= 22 @br{}
        Default value: 22
      @end{entry}
      @begin[slider-width]{entry}
        The @code{slider-width} style property of type @code{:int} (Read) @br{}
        The minimum width of the switch handle, in pixels. @br{}
        @em{Warning:} The @code{slider-width} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use the
        CSS @code{min-height} property instead. @br{}
        Allowed values: >= 36 @br{}
        Default value: 36
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:switch} widget which received the signal.}
      @end{table}
      The signal on the switch is an action signal and emitting it causes the
      switch to animate. Applications should never connect to this signal, but
      use the @code{\"notify::active\"} signal.
    @subheading{The \"state-set\" signal}
      @begin{pre}
lambda (widget state)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:switch} widget which received the signal.}
        @entry[state]{The boolean with the state of the switch.}
        @entry[Returns]{@em{True} to stop the signal emission.}
      @end{table}
      The signal on the switch is emitted to change the underlying state. It is
      emitted when the user changes the switch position. The default handler
      keeps the state in sync with the @code{active} property.

      To implement delayed state change, applications can connect to this
      signal, initiate the change of the underlying state, and call the
      @fun{gtk:switch-state} function when the underlying state change is
      complete. The signal handler should return @em{true} to prevent the
      default handler from running.

      Visually, the underlying state is represented by the trough color of the
      switch, while the @code{active} property is represented by the position
      of the switch.
  @end{dictionary}
  @see-constructor{gtk:switch-new}
  @see-slot{gtk:switch-active}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:switch-active ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'switch) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the switch is in its on or off state. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'switch-active)
      "Accessor"
      (documentation 'switch-active 'function)
 "@version{#2023-03-27}
  @syntax{(gtk:switch-active object) => is-active)}
  @syntax{(setf (gtk:switch-active object) is-active)}
  @argument[object]{a @class{gtk:switch} widget}
  @argument[is-active]{@em{true} if the switch should be active,
    and @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gtk:switch]{active} slot of the @class{gtk:switch}
    class.
  @end{short}
  The @fun{gtk:switch-active} function gets whether the switch is in its \"on\"
  or \"off\" state. The @setf{gtk:switch-active} function changes the state of
  the switch to the desired one.
  @see-class{gtk:switch}")

;;; --- gtk:switch-state -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state" 'switch) t)
 "The @code{state} property of type @code{:boolean} (Read / Write) @br{}
  The backend state that is controlled by the switch. See the
  @code{\"state-set\"} signal for details. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'switch-state)
      "Accessor"
      (documentation 'switch-state 'function)
 "@version{#2025-06-18}
  @syntax{(gtk:switch-state object) => state)}
  @syntax{(setf (gtk:switch-state object) state)}
  @argument[object]{a @class{gtk:switch} widget}
  @argument[state]{a boolean for the state}
  @begin{short}
    Accessor of the @slot[gtk:switch]{state} slot of the @class{gtk:switch}
    class.
  @end{short}
  The @fun{gtk:switch-active} function gets the underlying state of the switch.
  The @setf{gtk:switch-active} function sets the underlying state of the switch.

  Normally, this is the same as the @slot[gtk:switch]{active} property, unless
  the switch is set up for delayed state changes. This function is typically
  called from a @code{\"state-set\"} signal handler. See the
  @code{\"state-set\"} signal for details.
  @see-class{gtk:switch}
  @see-function{gtk:switch-active}")

;;; ----------------------------------------------------------------------------
;;; gtk_switch_new
;;; ----------------------------------------------------------------------------

(declaim (inline switch-new))

(defun switch-new ()
 #+liber-documentation
 "@version{#2023-03-27}
  @return{The newly created @class{gtk:switch} widget.}
  @short{Creates a new switch.}
  @see-class{gtk:switch}"
  (make-instance 'switch))

(export 'switch-new)

;;; --- End of file gtk3.switch.lisp -------------------------------------------
