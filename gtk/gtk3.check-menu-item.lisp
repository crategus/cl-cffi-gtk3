;;; ----------------------------------------------------------------------------
;;; gtk3.check-menu-item.lisp
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
;;; GtkCheckMenuItem
;;;
;;;     A menu item with a check box
;;;
;;; Types and Values
;;;
;;;     GtkCheckMenuItem
;;;
;;; Functions
;;;
;;;     gtk_check_menu_item_new
;;;     gtk_check_menu_item_new_with_label
;;;     gtk_check_menu_item_new_with_mnemonic
;;;     gtk_check_menu_item_get_active                     Accessor
;;;     gtk_check_menu_item_set_active                     Accessor
;;;     gtk_check_menu_item_toggled
;;;     gtk_check_menu_item_get_inconsistent               Accessor
;;;     gtk_check_menu_item_set_inconsistent               Accessor
;;;     gtk_check_menu_item_set_draw_as_radio              Accessor
;;;     gtk_check_menu_item_get_draw_as_radio              Accessor
;;;
;;; Properties
;;;
;;;     active
;;;     draw-as-radio
;;;     inconsistent
;;;
;;; Style Properties
;;;
;;;     indicator-size
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
;;;                     ╰── GtkMenuItem
;;;                         ╰── GtkCheckMenuItem
;;;                             ╰── GtkRadioMenuItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCheckMenuItem implements AtkImplementorIface, GtkBuildable,
;;;     GtkActivatable and GtkActionable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCheckMenuItem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCheckMenuItem" check-menu-item
  (:superclass menu-item
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable"
                 "GtkActivatable")
    :type-initializer "gtk_check_menu_item_get_type")
  ((active
    check-menu-item-active
    "active" "gboolean" t t)
   (draw-as-radio
    check-menu-item-draw-as-radio
    "draw-as-radio" "gboolean" t t)
   (inconsistent
    check-menu-item-inconsistent
    "inconsistent" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'check-menu-item 'type)
 "@version{2023-2-27}
  @begin{short}
    A @class{gtk:check-menu-item} widget is a menu item that maintains the state
    of a boolean value in addition to a @class{gtk:menu-item} usual role in
    activating application code.
  @end{short}

  A check box indicating the state of the boolean value is displayed at the
  left side of the @class{gtk:menu-item} widget. Activating the
  @class{gtk:menu-item} widget toggles the value.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
menuitem
├── check.left
╰── <child>
    @end{pre}
    The @class{gtk:check-menu-item} implementation has a main CSS node with
    name @code{menuitem}, and a subnode with name @code{check}, which gets the
    @code{.left} or @code{.right} style class.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[indicator-size]{entry}
        The @code{indicator-size} style property of type @code{:int} (Read)
        @br{}
        Size of check or radio indicator. @br{}
        @em{Warning:} The @code{indicator-size} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard @code{min-width} CSS property on the check or
        radio nodes. The value of this style property is ignored. @br{}
       Allowed values: >= 0 @br{}
       Default value: 16
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
lambda (item)    :run-first
      @end{pre}
      The signal is emitted when the state of the check box is changed. A signal
      handler can use the @fun{gtk:check-menu-item-active} function to discover
      the new state.
      @begin[code]{table}
        @entry[item]{The @class{sym:check-menu-item} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:check-menu-item-new}
  @see-constructor{gtk:check-menu-item-new-with-label}
  @see-constructor{gtk:check-menu-item-new-with-mnemonic}
  @see-slot{gtk:check-menu-item-active}
  @see-slot{gtk:check-menu-item-draw-as-radio}
  @see-slot{gtk:check-menu-item-inconsistent}
  @see-class{gtk:menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:check-menu-item-active ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'check-menu-item) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the menu item is checked. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'check-menu-item-active)
      "Accessor"
      (documentation 'check-menu-item-active 'function)
 "@version{2023-2-27}
  @syntax{(gtk:check-menu-item-active object) => is-active}
  @syntax{(setf (gtk:checkk-menu-item-active object) is-active)}
  @argument[object]{a @class{gtk:check-menu-item} widget}
  @argument[is-active]{a boolean value indicating whether the check box
    is active}
  @begin{short}
    Accessor of the @slot[gtk:check-menu-item]{active} slot of the
    @class{gtk:check-menu-item} class.
  @end{short}
  The @fun{gtk:check-menu-item-active} returns whether the check menu item is
  active. The @setf{gtk:check-menu-item-active} function sets the active state
  of the menu item's check box.
  @see-class{gtk:check-menu-item}")

;;; --- gtk:check-menu-item-draw-as-radio --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw-as-radio"
                                               'check-menu-item) t)
 "The @code{draw-as-radio} property of type @code{:boolean} (Read / Write) @br{}
  Whether the menu item looks like a radio menu item. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'check-menu-item-draw-as-radio)
      "Accessor"
      (documentation 'check-menu-item-draw-as-radio 'function)
 "@version{2023-2-27}
  @syntax{(gtk:check-menu-item-draw-as-radio object) => setting}
  @syntax{(setf (gtk:checkk-menu-item-draw-as-radio object) setting)}
  @argument[object]{a @class{gtk:check-menu-item} widget}
  @argument[setting]{a boolean whether @arg{objct} is drawn like a
    @class{gtk:radio-menu-item} widget}
  @begin{short}
    Accessor of the @slot[gtk:check-menu-item]{draw-as-radio} slot of the
    @class{gtk:check-menu-item} class.
  @end{short}
  The @fun{gtk:check-menu-item-draw-as-radio} function returns whether
  @arg{object} looks like a @class{gtk:radio-menu-item} widget. The
  @setf{gtk:check-menu-item-draw-as-radio} function sets whether @arg{object}
  is drawn like a @class{gtk:radio-menu-item} widget.
  @see-class{gtk:check-menu-item}
  @see-class{gtk:radio-menu-item}")

;;; --- gtk:check-menu-item-inconsistent ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inconsistent"
                                               'check-menu-item) t)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  Whether to display an \"inconsistent\" state. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'check-menu-item-inconsistent)
      "Accessor"
      (documentation 'check-menu-item-inconsistent 'function)
 "@version{2023-2-27}
  @syntax{(gtk:check-menu-item-inconsistent object) => setting}
  @syntax{(setf (gtk:checkk-menu-item-inconsistent object) setting)}
  @argument[object]{a @class{gtk:check-menu-item} widget}
  @argument[setting]{@em{true} to display an \"inconsistent\" third state check}
  @begin{short}
    Accessor of the @slot[gtk:check-menu-item]{inconsistent} slot of the
    @class{gtk:check-menu-item} class.
  @end{short}
  If the user has selected a range of elements, such as some text or spreadsheet
  cells, that are affected by a boolean setting, and the current values in that
  range are inconsistent, you may want to display the check in an \"in between\"
  state.

  This function turns on \"in between\" display. Normally you would turn off the
  inconsistent state again if the user explicitly selects a setting. This has to
  be done manually, the @fun{gtk:check-menu-item-inconsistent} function only
  affects visual appearance, it does not affect the semantics of the widget.
  @see-class{gtk:check-menu-item}")

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline check-menu-item-new))

(defun check-menu-item-new ()
 #+liber-documentation
 "@version{#2023-2-27}
  @return{A new @class{gtk:check-menu-item} widget.}
  @begin{short}
    Creates a new check menu item.
  @end{short}
  @see-class{gtk:check-menu-item}"
  (make-instance 'check-menu-item))

(export 'check-menu-item-new)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new_with_label ()
;;; ----------------------------------------------------------------------------

(declaim (inline check-menu-item-new-with-label))

(defun check-menu-item-new-with-label (label)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[label]{a string to use for the label}
  @return{A new @class{gtk:check-menu-item} widget.}
  @begin{short}
    Creates a new check menu item with a label.
  @end{short}
  @see-class{gtk:check-menu-item}"
  (make-instance 'check-menu-item
                 :label label))

(export 'check-menu-item-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_new_with_mnemonic ()
;;; ----------------------------------------------------------------------------

(declaim (inline check-menu-item-new-with-mnemonic))

(defun check-menu-item-new-with-mnemonic (label)
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the character}
  @return{A new @class{gtk:check-menu-item} widget.}
  @begin{short}
    Creates a new check menu item containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the menu item.
  @see-class{gtk:check-menu-item}"
  (make-instance 'check-menu-item
                 :label label
                 :use-underline t))

(export 'check-menu-item-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_check_menu_item_toggled ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_check_menu_item_toggled" check-menu-item-toggled) :void
 #+liber-documentation
 "@version{#2023-2-27}
  @argument[item]{a @class{gtk:check-menu-item} widget}
  @begin{short}
    Emits the @code{\"toggled\"} signal.
  @end{short}
  @see-class{gtk:check-menu-item}"
  (item (g:object check-menu-item)))

(export 'check-menu-item-toggled)

;;; --- End of file gtk3.check-menu-item.lisp ----------------------------------
