;;; ----------------------------------------------------------------------------
;;; gtk3.check-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
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
;;; GtkCheckButton
;;;
;;;     Create widgets with a discrete toggle button
;;;
;;; Types and Values
;;;
;;;     GtkCheckButton
;;;
;;; Functions
;;;
;;;     gtk_check_button_new
;;;     gtk_check_button_new_with_label
;;;     gtk_check_button_new_with_mnemonic
;;;
;;; Style Properties
;;;
;;;     indicator-size
;;;     indicator-spacing
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                    ╰── GtkButton
;;;                        ╰── GtkToggleButton
;;;                            ╰── GtkCheckButton
;;;                                ╰── GtkRadioButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCheckButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkActionable and GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCheckButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCheckButton" check-button
  (:superclass toggle-button
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActionable"
                "GtkActivatable")
   :type-initializer "gtk_check_button_get_type")
  nil)

#+liber-documentation
(setf (documentation 'check-button 'type)
 "@version{2025-3-9}
  @begin{short}
    The @class{gtk:check-button} widget places a discrete
    @class{gtk:toggle-button} widget next to a widget, usually a
    @class{gtk:label} widget.
  @end{short}
  See the @class{gtk:toggle-button} documentation for more information about
  toggle and check buttons. The important @code{\"toggled\"} signal is also
  inherited from the @class{gtk:toggle-button} class.

  @image[check-button]{Figure: GtkCheckButton}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
checkbutton
├── check
╰── <child>
    @end{pre}
    A @class{gtk:check-button} widget with indicator, see the
    @fun{gtk:toggle-button-mode} function, has a main CSS node with name
    @code{checkbutton} and a subnode with name @code{check}.
    @begin{pre}
button.check
├── check
╰── <child>
    @end{pre}
    A @class{gtk:check-button} widget without indicator changes the name of its
    main node to @code{button} and adds a @code{.check} style class to it. The
    subnode is invisible in this case.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[indicator-size]{entry}
        The @code{indicator-size} style property of type @code{:int} (Read)
        @br{}
        Size of check or radio indicator. @br{}
        @em{Warning:} The @code{indicator-size} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS @code{min-width} and @code{min-height} on the indicator
        node. @br{}
        Allowed values: >= 0 @br{}
        Default value: 16
      @end{entry}
      @begin[indicator-spacing]{entry}
        The @code{indicator-spacing} style property of type @code{:int}
        (Read) @br{}
        Spacing around check or radio indicator. @br{}
        @em{Warning:} The @code{indicator-spacing} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS @code{min-width} and @code{min-height} on the indicator
        node. @br{}
        Allowed values: >= 0 @br{}
        Default value: 2
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:check-button-new}
  @see-constructor{gtk:check-button-new-with-label}
  @see-constructor{gtk:check-button-new-with-mnemonic}
  @see-class{gtk:button}
  @see-class{gtk:toggle-button}
  @see-class{gtk:radio-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline check-button-new))

(defun check-button-new ()
 #+liber-documentation
 "@version{2025-3-9}
  @return{The new @class{gtk:check-button} widget.}
  @short{Creates a new check button.}
  @see-class{gtk:check-button}
  @see-function{gtk:check-button-new-with-label}
  @see-function{gtk:check-button-new-with-mnemonic}"
  (make-instance 'check-button))

(export 'check-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new_with_label
;;; ----------------------------------------------------------------------------

(declaim (inline check-button-new-with-label))

(defun check-button-new-with-label (label)
 #+liber-documentation
 "@version{2025-3-9}
  @argument[label]{a string for the text of the check button}
  @return{The new @class{gtk:check-button} widget.}
  @begin{short}
    Creates a new check button with a @class{gtk:label} widget to the right
    of it.
  @end{short}
  @see-class{gtk:check-button}
  @see-class{gtk:label}
  @see-function{gtk:check-button-new}
  @see-function{gtk:check-button-new-with-mnemonic}"
  (make-instance 'check-button
                 :label label))

(export 'check-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(declaim (inline check-button-new-with-mnemonic))

(defun check-button-new-with-mnemonic (label)
#+liber-documentation
 "@version{2025-3-9}
  @argument[label]{a string for the text of the button, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:check-button} widget.}
  @begin{short}
    Creates a new check button widget containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the check button.
  @see-class{gtk:check-button}
  @see-function{gtk:check-button-new}
  @see-function{gtk:check-button-new-with-label}
  @see-function{gtk:label-new-with-mnemonic}"
  (make-instance 'check-button
                 :label label
                 :use-underline t))

(export 'check-button-new-with-mnemonic)

;;; --- End of file gtk3.check-button.lisp -------------------------------------
