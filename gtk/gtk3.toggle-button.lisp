;;; ----------------------------------------------------------------------------
;;; gtk3.toggle-button.lisp
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
;;; GtkToggleButton
;;;
;;;     Create buttons which retain their state
;;;
;;; Types and Values
;;;
;;;     GtkToggleButton
;;;
;;; Accessors
;;;
;;;     gtk_toggle_button_get_active
;;;     gtk_toggle_button_set_active
;;;     gtk_toggle_button_get_inconsistent
;;;     gtk_toggle_button_set_inconsistent
;;;
;;; Functions
;;;
;;;     gtk_toggle_button_new
;;;     gtk_toggle_button_new_with_label
;;;     gtk_toggle_button_new_with_mnemonic
;;;     gtk_toggle_button_set_mode
;;;     gtk_toggle_button_get_mode
;;;     gtk_toggle_button_toggled
;;;
;;; Properties
;;;
;;;     active
;;;     draw-indicator
;;;     inconsistent
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

(gobject:define-gobject "GtkToggleButton" toggle-button
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
 "@version{2025-07-17}
  @begin{short}
    The @class{gtk:toggle-button} widget is a @class{gtk:button} widget that
    will remain \"pressed-in\" when clicked.
  @end{short}
  Clicking again will cause the toggle button to return to its normal state.

  @image[toggle-button]{Figure: GtkToggleButton}

  A toggle button is created by calling either the @fun{gtk:toggle-button-new}
  or @fun{gtk:toggle-button-new-with-label} functions. If using the former, it
  is advisable to pack a widget, such as a @class{gtk:label} or a
  @class{gtk:image} widget, into the container of the toggle button. See the
  @class{gtk:button} widget for more information.

  The state of a @class{gtk:toggle-button} widget can be set and retrieved using
  the @fun{gtk:toggle-button-active} function.

  To simply switch the state of a toggle button, use the
  @fun{gtk:toggle-button-toggled} function.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:toggle-button} implementation has a single CSS node with
    name @code{button}. To differentiate it from a plain button, it gets the
    @code{.toggle} style class.
  @end{dictionary}
  @begin[Examples]{dictionary}
    This example from the GTK tutorial has two toggle buttons. The toggle
    buttons are used to switch the column and row spacing of a grid.
    @begin{pre}
(defun example-grid-spacing ()
  (gtk:within-main-loop
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
                          (gtk:leave-gtk-main)))
      (g:signal-connect button1 \"toggled\"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (setf (gtk:grid-row-spacing grid) 24
                     (gtk:button-label widget) \"Less Row Spacing\")
               (setf (gtk:grid-row-spacing grid) 6
                     (gtk:button-label widget) \"More Row Spacing\"))))
      (g:signal-connect button2 \"toggled\"
         (lambda (widget)
           (if (gtk:toggle-button-active widget)
               (setf (gtk:grid-column-spacing grid) 24
                     (gtk:button-label widget) \"Less Col Spacing\")
               (setf (gtk:grid-column-spacing grid) 6
                     (gtk:button-label widget) \"More Col Spacing\"))))
      (gtk:grid-attach grid button1 0 0 1 1)
      (gtk:grid-attach grid button2 1 0 1 1)
      (gtk:grid-attach grid button3 0 1 2 1)
      (gtk:container-add window grid)
      (gtk:widget-show-all window))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[toggle-button::toggled]{signal}
      @begin{pre}
lambda (togglebutton)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[togglebutton]{The @class{gtk:toggle-button} widget that received
          the signal.}
      @end{simple-table}
      Should be connected if you wish to perform an action whenever the
      state of the toggle button is changed.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:toggle-button-new}
  @see-constructor{gtk:toggle-button-new-with-label}
  @see-constructor{gtk:toggle-button-new-with-mnemonic}
  @see-slot{gtk:toggle-button-active}
  @see-slot{gtk:toggle-button-draw-indicator}
  @see-slot{gtk:toggle-button-inconsistent}
  @see-class{gtk:button}
  @see-class{gtk:check-button}
  @see-class{gtk:check-menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:toggle-button-active -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'toggle-button) t)
"The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle button should be pressed in. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-active)
      "Accessor"
      (documentation 'toggle-button-active 'function)
 "@version{2025-06-28}
  @syntax{(gtk:toggle-button-active object) => is-active}
  @syntax{(setf (gtk:toggle-button-active object) is-active)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[is-active]{@em{true} if the toggle button should be pressed in}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{active} slot of the
    @class{gtk:toggle-button} class.
  @end{short}
  The @fun{gtk:toggle-button-active} function queries a toggle button and
  returns its current state. Returns @em{true} if the toggle button is pressed
  in and @em{false} if it is raised. The @setf{gtk:toggle-button-active}
  function sets the status of the toggle button.

  This action causes the @sig[gtk:toggle-button]{toggled} signal to be emitted.
  @see-class{gtk:toggle-button}")

;;; --- gtk:toggle-button-draw-indicator ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw-indicator"
                                               'toggle-button) t)
 "The @code{draw-indicator} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the toggle part of the button is displayed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-draw-indicator)
      "Accessor"
      (documentation 'toggle-button-draw-indicator 'function)
 "@version{2023-12-29}
  @syntax{(gtk:toggle-button-draw-indicator object) => indicator}
  @syntax{(setf (gtk:toggle-button-draw-indicator object) indicator)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[indicator]{a boolean whether the toggle part of the button is
    displayed}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{draw-indicator} slot of the
    @class{gtk:toggle-button} class.
  @end{short}
  If the toggle part of the button is displayed.
  @see-class{gtk:toggle-button}")

;;; --- gtk:toggle-button-inconsistent -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inconsistent" 'toggle-button) t)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle button is in an \"in between\" state. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-inconsistent)
      "Accessor"
      (documentation 'toggle-button-inconsistent 'function)
 "@version{2023-12-29}
  @syntax{(gtk:toggle-button-inconsistent object) => setting}
  @syntax{(setf (gtk:toggle-button-inconsistent object) setting)}
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
  @setf{gtk:toggle-button-inconsistent} function only affects visual appearance,
  it does not affect the semantics of the button.
  @see-class{gtk:toggle-button}")

;;; ----------------------------------------------------------------------------
;;; toggle-button-new
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new))

(defun toggle-button-new ()
 #+liber-documentation
 "@version{2023-12-29}
  @return{The new @class{gtk:toggle-button} widget.}
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
 "@version{2023-12-29}
  @argument[label]{a string containing the message to be placed in the toggle
    button}
  @return{The new @class{gtk:toggle-button} widget.}
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

(cffi:defcfun ("gtk_toggle_button_new_with_mnemonic"
               toggle-button-new-with-mnemonic) (g:object widget)
 #+liber-documentation
 "@version{2025-06-18}
  @argument[label]{a string for the text of the button, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the button.
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new}
  @see-function{gtk:toggle-button-new-with-label}
  @see-function{gtk:label-new-with-mnemonic}"
  (label :string))

(export 'toggle-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_get_mode
;;; gtk_toggle_button_set_mode
;;; ----------------------------------------------------------------------------

(defun (setf toggle-button-mode) (draw-indicator button)
  (setf (toggle-button-draw-indicator button) draw-indicator))

(defun toggle-button-mode (button)
 #+liber-documentation
 "@version{#2025-06-18}
  @syntax{(gtk:toggle-button-mode button) => draw-indicator}
  @syntax{(setf (gtk:toggle-button-mode button) draw-indicator)}
  @argument[button]{a @class{gtk:toggle-button} widget}
  @argument[draw-indicator]{if @em{true}, draw the button as a separate
    indicator and label, if @em{false}, draw the button like a normal button}
  @begin{short}
    The @fun{gtk:toggle-button-mode} function retrieves whether the button is
    displayed as a separate indicator and label.
  @end{short}
  The @setf{gtk:toggle-button-mode} function sets whether the button is
  displayed as a separate indicator and label.

  You can call this function on a check button or a radio button with the
  @em{false} value for @arg{draw-indicator} to make the button look like a
  normal button.

  This function only affects instances of classes like the
  @class{gtk:check-button} and @class{gtk:radio-button} classes that derive
  from the @class{gtk:toggle-button} class, not instances of the
  @class{gtk:toggle-button} class itself.
  @begin[Notes]{dictionary}
    The @fun{gtk:toggle-button-mode} function is equivalent to the
    @fun{gtk:toggle-button-draw-indicator} function.
  @end{dictionary}
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-draw-indicator}"
  (toggle-button-draw-indicator button))

(export 'toggle-button-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_toggled
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_toggle_button_toggled" toggle-button-toggled) :void
 #+liber-documentation
 "@version{#2025-06-28}
  @argument[button]{a @class{gtk:toggle-button} widget}
  @begin{short}
    Emits the @sig[gtk:toggle-button]{toggled} signal on the toggle button.
  @end{short}
  There is no good reason for an application ever to call this function.
  @see-class{gtk:toggle-button}"
  (button (g:object toggle-button)))

(export 'toggle-button-toggled)

;;; --- End of file gtk3.toggle-button.lisp ------------------------------------
