;;; ----------------------------------------------------------------------------
;;; gtk3.toggle-tool-button.lisp
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
;;; GtkToggleToolButton
;;;
;;;     A GtkToolItem containing a toggle button
;;;
;;; Types and Values
;;;
;;;     GtkToggleToolButton
;;;
;;; Functions
;;;
;;;     gtk_toggle_tool_button_new
;;;     gtk_toggle_tool_button_new_from_stock
;;;     gtk_toggle_tool_button_set_active                  Accessor
;;;     gtk_toggle_tool_button_get_active                  Accessor
;;;
;;; Properties
;;;
;;;     active
;;;
;;; Signals
;;;
;;;     toggled
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ╰── GtkToolButton
;;;                             ╰── GtkToggleToolButton
;;;                                 ╰── GtkRadioToolButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToggleToolButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkToggleToolButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkToggleToolButton" toggle-tool-button
  (:superclass tool-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable"
                "GtkActionable")
   :type-initializer "gtk_toggle_tool_button_get_type")
  ((active
    toggle-tool-button-active
    "active" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'toggle-tool-button 'type)
 "@version{#2023-3-28}
  @begin{short}
    A @class{gtk:toggle-tool-button} widget is a @class{gtk:tool-item} widget
    that contains a toggle button.
  @end{short}
  Use the @fun{gtk:toggle-tool-button-new} function to create a new toggle tool
  button.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:toggle-tool-button} implementation has a single CSS node with
    name @code{togglebutton}.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      Emitted whenever the toggle tool button changes state.
      @begin[code]{table}
        @entry[button]{The @class{gtk:toggle-tool-button} widget that emitted
          the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:toggle-tool-button-active}
  @see-class{gtk:tool-item}
  @see-class{gtk:radio-tool-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'toggle-tool-button) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  If the toggle tool button should be pressed in. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-tool-button-active)
      "Accessor"
      (documentation 'toggle-tool-button-active 'function)
 "@version{2024-1-2}
  @syntax{(gtk:toggle-tool-button-active object) => setting}
  @syntax{(setf (gtk:toggle-tool-button-active object) setting)}
  @argument[object]{a @class{gtk:toggle-tool-button} widget}
  @argument[setting]{a boolean whether the toggle tool button should be
    active}
  @begin{short}
    Accessor of the @slot[gtk:toggle-tool-button]{active} slot of the
    @class{gtk:toggle-tool-button} class.
  @end{short}
  The @fun{gtk:toggle-tool-button-active} function queries a toggle tool button
  and returns its current state. The @setf{gtk:toggle-tool-button-active}
  function sets the status.

  Set to @em{true} if you want the toggle tool button to be 'pressed in',
  and @em{false} to raise it. This action causes the @code{\"toggled\"} signal
  to be emitted.
  @see-class{gtk:toggle-tool-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-tool-button-new))

(defun toggle-tool-button-new ()
 #+liber-documentation
 "@version{#2023-3-28}
  @return{The newly created @class{gtk:toggle-tool-button} widget.}
  @begin{short}
    Returns a new toggle tool button.
  @end{short}
  @see-class{gtk:toggle-tool-button}"
  (make-instance 'toggle-tool-button))

(export 'toggle-tool-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_tool_button_new_from_stock ()
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-tool-button-new-from-stock))

(defun toggle-tool-button-new-from-stock (stock)
 #+liber-documentation
 "@version{#2023-3-28}
  @argument[stock]{a string with the name of the stock item}
  @return{The new @class{gtk:toggle-tool-button} widget.}
  @begin{short}
    Creates a new toggle tool button containing the image and text from a stock
    item.
  @end{short}
  It is an error if @arg{stock} is not a name of a stock item.
  @begin[Warning]{dictionary}
    The @fun{gtk:toggle-tool-button-new-from-stock} function has been deprecated
    since version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:toggle-tool-button-new} function instead.
  @end{dictionary}
  @see-class{gtk:toggle-tool-button}
  @see-function{gtk:toggle-tool-button-new}"
  (make-instance 'toggle-tool-button
                 :stock-id stock))

(export 'toggle-tool-button-new-from-stock)

;;; --- End of file gtk3.toggle-tool-button.lisp -------------------------------
