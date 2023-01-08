;;; ----------------------------------------------------------------------------
;;; gtk.toggle-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;; GtkToggleButton
;;;
;;;     Create buttons which retain their state
;;;
;;; Types and Values
;;;
;;;     GtkToggleButton
;;;
;;; Functions
;;;
;;;     gtk_toggle_button_new
;;;     gtk_toggle_button_new_with_label
;;;     gtk_toggle_button_new_with_mnemonic
;;;     gtk_toggle_button_set_mode
;;;     gtk_toggle_button_get_mode
;;;     gtk_toggle_button_toggled
;;;     gtk_toggle_button_get_active                       Accessor
;;;     gtk_toggle_button_set_active                       Accessor
;;;     gtk_toggle_button_get_inconsistent                 Accessor
;;;     gtk_toggle_button_set_inconsistent                 Accessor
;;;
;;; Properties
;;;
;;;     gboolean    active            Read / Write
;;;     gboolean    draw-indicator    Read / Write
;;;     gboolean    inconsistent      Read / Write
;;;
;;; Signals
;;;
;;;         void    toggled           Run First
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkButton
;;;                         ╰── GtkToggleButton
;;;                             ├── GtkCheckButton
;;;                             ╰── GtkMenuButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkToggleButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkToggleButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkToggleButton" toggle-button
  (:superclass button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_toggle_button_get_type")
  ((active
    toggle-button-active
    "active" "gboolean" t t)
   (draw-indicator
    toggle-button-draw-indicator
    "draw-indicator" "gboolean" t t)
   (inconsistent
    toggle-button-inconsistent
    "inconsistent" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'toggle-button 'type)
 "@version{#2021-10-11}
  @begin{short}
    A @sym{gtk:toggle-button} widget is a @class{gtk:button} widget which will
    remain \"pressed-in\" when clicked.
  @end{short}
  Clicking again will cause the toggle button to return to its normal state.

  @image[toggle-button]{}

  A toggle button is created by calling either the @fun{gtk:toggle-button-new}
  or @fun{gtk:toggle-button-new-with-label} functions. If using the former, it
  is advisable to pack a widget, such as a @class{gtk:label} or a
  @class{gtk:image} widget, into the container of the toggle button. See the
  @class{gtk:button} widget for more information.

  The state of a @sym{gtk:toggle-button} widget can be set and retrieved using
  the @fun{gtk:toggle-button-active} function.

  To simply switch the state of a toggle button, use the
  @fun{gtk:toggle-button-toggled} function.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:toggle-button} implementation has a single CSS node with name
    @code{button}. To differentiate it from a plain button, it gets the
    @code{.toggle} style class.
  @end{dictionary}
  @begin[Example]{dictionary}
    This example from the GTK tutorial has two toggle buttons. The toggle
    buttons are used to switch the column and row spacing of a grid.
    @begin{pre}
(defun example-grid-spacing ()
  (within-main-loop
    (let ((window (make-instance 'gtk:window
                                 :type :toplevel
                                 :title \"Example Grid Spacing\"
                                 :border-width 12
                                 :default-width 320))
          (grid (make-instance 'gtk:grid
                               :column-homogeneous t
                               :column-spacing 6
                               :row-homogeneous t
                               :row-spacing 6))
          (button1 (make-instance 'gtk:toggle-button
                                  :label \"More Row Spacing\"))
          (button2 (make-instance 'gtk:toggle-button
                                  :label \"More Col Spacing\"))
          (button3 (make-instance 'gtk:button
                                  :label \"Button 3\")))

      (g:signal-connect window \"destroy\"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g:signal-connect button1 \"toggled\"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (progn
                 (setf (gtk:grid-row-spacing grid) 24)
                 (setf (gtk:button-label widget) \"Less Row Spacing\"))
               (progn
                 (setf (gtk:grid-row-spacing grid) 6)
                 (setf (gtk:button-label widget) \"More Row Spacing\")))))
      (g:signal-connect button2 \"toggled\"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (progn
                 (setf (gtk:grid-column-spacing grid) 24)
                 (setf (gtk:button-label widget) \"Less Col Spacing\"))
               (progn
                 (setf (gtk:grid-column-spacing grid) 6)
                 (setf (gtk:button-label widget) \"More Col Spacing\")))))

      (gtk:grid-attach grid button1 0 0 1 1)
      (gtk:grid-attach grid button2 1 0 1 1)
      (gtk:grid-attach grid button3 0 1 2 1)

      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
 lambda (togglebutton)    :run-first
      @end{pre}
      Should be connected if you wish to perform an action whenever the
      state of the toggle button is changed.
      @begin[code]{table}
        @entry[togglebutton]{The @sym{gtk:toggle-button} widget which received
          the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:toggle-button-active}
  @see-slot{gtk:toggle-button-draw-indicator}
  @see-slot{gtk:toggle-button-inconsistent}
  @see-class{gtk:button}
  @see-class{gtk:check-button}
  @see-class{gtk:check-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- toggle-button-active -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'toggle-button) t)
"The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  If the toggle button should be pressed in. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-active)
      "Accessor"
      (documentation 'toggle-button-active 'function)
 "@version{#2021-10-11}
  @syntax[]{(gtk:toggle-button-active object) => is-active}
  @syntax[]{(setf (gtk:toggle-button-active object) is-active)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[is-active]{@em{true} if the toggle button should be pressed in}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{active} slot of the
    @class{gtk:toggle-button} class.
  @end{short}

  The @sym{gtk:toggle-button-active} slot access function queries a toggle
  button and returns its current state. Returns @em{true} if the toggle button
  is pressed in and @em{false} if it is raised. The
  @sym{(setf gtk:toggle-button-active)} slot access function sets the status of
  the toggle button.

  This action causes the \"toggled\" signal to be emitted.
  @see-class{gtk:toggle-button}")

;;; --- toggle-button-draw-indicator ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw-indicator"
                                               'toggle-button) t)
 "The @code{draw-indicator} property of type @code{:boolean} (Read / Write)
  @br{}
  If the toggle part of the button is displayed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-draw-indicator)
      "Accessor"
      (documentation 'toggle-button-draw-indicator 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:toggle-button-draw-indicator object) => indicator}
  @syntax[]{(setf (gtk:toggle-button-draw-indicator object) indicator)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[indicator]{a boolean whether the toggle part of the button is
    displayed}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{draw-indicator} slot of the
    @class{gtk:toggle-button} class.
  @end{short}

  If the toggle part of the button is displayed.
  @see-class{gtk:toggle-button}")

;;; --- toggle-button-inconsistent -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inconsistent"
                                               'toggle-button) t)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  If the toggle button is in an \"in between\" state. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-inconsistent)
      "Accessor"
      (documentation 'toggle-button-inconsistent 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:toggle-button-inconsistent object) => setting}
  @syntax[]{(setf (gtk:toggle-button-inconsistent object) setting)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[setting]{@em{true} if state is inconsistent}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{inconsistent} slot of the
    @class{gtk:toggle-button} class.
  @end{short}

  If the user has selected a range of elements, such as some text or
  spreadsheet cells, that are affected by a toggle button, and the current
  values in that range are inconsistent, you may want to display the toggle in
  an \"in between\" state. This function turns on \"in between\" display.
  Normally you would turn off the inconsistent state again if the user toggles
  the toggle button. This has to be done manually, the
  @sym{(setf gtk:toggle-button-inconsistent)} function only affects visual
  appearance, it does not affect the semantics of the button.
  @see-class{gtk:toggle-button}")

;;; ----------------------------------------------------------------------------
;;; toggle-button-new
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new))

(defun toggle-button-new ()
 #+liber-documentation
 "@version{#2021-12-23}
  @return{A new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button.
  @end{short}
  A widget should be packed into the button, as in the @fun{gtk:button-new}
  function.
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new-with-label}
  @see-function{gtk:toggle-button-new-with-mnemonic}
  @see-function{gtk:button-new}"
  (make-instance 'toggle-button))

(export 'toggle-button-new)

;;; ----------------------------------------------------------------------------
;;; toggle-button-new-with-label
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new-with-label))

(defun toggle-button-new-with-label (label)
 #+liber-documentation
 "@version{#2021-12-23}
  @argument[label]{a string containing the message to be placed in the toggle
    button}
  @return{A new @class{gtk:toggle-button} widget.}
  @short{Creates a new toggle button with a text label.}
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new}
  @see-function{gtk:toggle-button-new-with-mnemonic}"
  (make-instance 'toggle-button
                 :label label))

(export 'toggle-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; toggle-button-new-with-mnemonic
;;; ----------------------------------------------------------------------------

;; TODO: Rewrite the implementation in terms of the function make-instance

(defcfun ("gtk_toggle_button_new_with_mnemonic"
           toggle-button-new-with-mnemonic)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-12-23}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button containing a label.
  @end{short}
  The label will be created using the
  @fun{gtk:label-new-with-mnemonic} function, so underscores in label indicate
  the mnemonic for the button.
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new}
  @see-function{gtk:toggle-button-new-with-label}
  @see-function{gtk:label-new-with-mnemonic}"
  (label :string))

(export 'toggle-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_get_mode ()
;;; gtk_toggle_button_set_mode () -> toggle-button-mode
;;; ----------------------------------------------------------------------------

(defun (setf toggle-button-mode) (draw-indicator button)
  (setf (toggle-button-draw-indicator button) draw-indicator))

(defun toggle-button-mode (button)
 #+liber-documentation
 "@version{#2021-12-23}
  @syntax[]{(gtk:toggle-button-mode button) => draw-indicator}
  @syntax[]{(setf (gtk:toggle-button-mode button) draw-indicator)}
  @argument[button]{a @class{gtk:toggle-button} widget}
  @argument[draw-indicator]{if @em{true}, draw the button as a separate
    indicator and label, if @em{false}, draw the button like a normal button}
  @begin{short}
    Accessor of the mode of the toggle button.
  @end{short}

  The @sym{gtk:toggle-button-mode} function retrieves whether the button is
  displayed as a separate indicator and label. The
  @sym{(setf gtk:toggle-button-mode)} function sets whether the button is
  displayed as a separate indicator and label.

  You can call this function on a check button or a radio button with the
  @em{false} value for @arg{draw-indicator} to make the button look like a
  normal button.

  This function only affects instances of classes like the
  @class{gtk:check-button} and @class{gtk:radio-button} classes that derive from
  the @class{gtk:toggle-button} class, not instances of the
  @class{gtk:toggle-button} class itself.
  @begin[Note]{dictionary}
    The function @sym{gtk:toggle-button-mode} is equivalent to the slot access
    function @fun{gtk:toggle-button-draw-indicator}.
  @end{dictionary}
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-draw-indicator}"
  (toggle-button-draw-indicator button))

(export 'toggle-button-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_toggled ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_toggle_button_toggled" toggle-button-toggled) :void
 #+liber-documentation
 "@version{#2021-12-23}
  @argument[button]{a @class{gtk:toggle-button} widget}
  @begin{short}
    Emits the \"toggled\" signal on the toggle button.
  @end{short}
  There is no good reason for an application ever to call this function.
  @see-class{gtk:toggle-button}"
  (button (g:object toggle-button)))

(export 'toggle-button-toggled)

;;; --- End of file gtk.toggle-button.lisp -------------------------------------
