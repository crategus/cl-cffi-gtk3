;;; ----------------------------------------------------------------------------
;;; gtk3.file-chooser-button.lisp
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
;;; GtkFileChooserButton
;;;
;;;     A button to launch a file selection dialog
;;;
;;; Types and Values
;;;
;;;     GtkFileChooserButton
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_button_new
;;;     gtk_file_chooser_button_new_with_dialog
;;;     gtk_file_chooser_button_get_title                  Accessor
;;;     gtk_file_chooser_button_set_title                  Accessor
;;;     gtk_file_chooser_button_get_width_chars            Accessor
;;;     gtk_file_chooser_button_set_width_chars            Accessor
;;;     gtk_file_chooser_button_get_focus_on_click         Accessor
;;;     gtk_file_chooser_button_set_focus_on_click         Accessor
;;;
;;; Properties
;;;
;;;     dialog
;;;     focus-on-click
;;;     title
;;;     width-chars
;;;
;;; Signals
;;;
;;;     file-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkFileChooserButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFileChooserButton implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkFileChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFileChooserButton" file-chooser-button
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFileChooser")
   :type-initializer "gtk_file_chooser_button_get_type")
  ((dialog
    file-chooser-button-dialog
    "dialog" "GtkFileChooser" nil nil)
   (focus-on-click
    file-chooser-button-focus-on-click
    "focus-on-click" "gboolean" t t)
   (title
    file-chooser-button-title
    "title" "gchararray" t t)
   (width-chars
    file-chooser-button-width-chars
    "width-chars" "gint" t t)))

#+liber-documentation
(setf (documentation 'file-chooser-button 'type)
 "@version{#2025-07-14}
  @begin{short}
    The @class{gtk:file-chooser-button} widget is a widget that lets the user
    select a file.
  @end{short}

  @image[file-chooser-button]{GtkFileChooserButton}

  It implements the @class{gtk:file-chooser} interface. Visually, it is a file
  name with a button to bring up a @class{gtk:file-chooser-dialog} widget. The
  user can then use that dialog to change the file associated with that button.
  This widget does not support setting the
  @slot[gtk:file-chooser]{select-multiple} property to @em{true}.

  The @class{gtk:file-chooser-button} widget supports the
  @val[gtk:file-chooser-action]{:open} and
  @val[gtk:file-chooser-action]{:select-folder} values of the
  @sym{gtk:file-chooser-action} enumeration.
  @begin[Examples]{dictionary}
    Create a button to let the user select a file.
    @begin{pre}
(let ((button (gtk:file-chooser-button-new \"Select a file\" :open)))
  (setf (gtk:file-chooser-current-folder button) \"/etc\")
  ... )
    @end{pre}
  @end{dictionary}
  @begin[Notes]{dictionary}
    The @class{gtk:file-chooser-button} widget will ellipsize the label, and
    thus will request little horizontal space. To give the button more space,
    you should call the @fun{gtk:widget-preferred-size},
    @fun{gtk:file-chooser-button-width-chars} functions, or pack the button in
    such a way that other interface elements give space to the widget.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:file-chooser-button} implementation has a CSS node with name
    @code{filechooserbutton}, containing a subnode for the internal button with
    name @code{button} and @code{.file} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[file-chooser-button::file-set]{signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:file-chooser-button} widget that received
          the signal.}
      @end{simple-table}
      The signal is emitted when the user selects a file. Note that this signal
      is only emitted when the user changes the file.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:file-chooser-button-new}
  @see-constructor{gtk:file-chooser-button-new-with-dialog}
  @see-slot{gtk:file-chooser-button-focus-on-click}
  @see-slot{gtk:file-chooser-button-title}
  @see-slot{gtk:file-chooser-button-width-chars}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:file-chooser-button-dialog -----------------------------------------

;; This property is not readable or writable. We do not export the accessor.

#+liber-documentation
(setf (documentation (liber:slot-documentation "dialog" 'file-chooser-button) t)
 "The @code{dialog} property of type @class{gtk:file-chooser}
  (Write / Construct Only) @br{}
  Instance of the file chooser dialog associated with the button. This
  property is not readable or writable from the Lisp side. The accessor
  is not exported.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-button-dialog)
      "Accessor"
      (documentation 'file-chooser-button-dialog 'function)
 "@version{2023-06-11}
  @syntax{(gtk:file-chooser-button-dialog object) => dialog}
  @syntax{(setf (gtk:file-chooser-button-dialog object) dialog)}
  @argument[object]{a @class{gtk:file-chooser-button} widget}
  @argument[dialog]{a @class{gtk:file-chooser-dialog} widget}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-button]{dialog} slot of the
    @class{gtk:file-chooser-button} class.
  @end{short}
  Instance of the file chooser dialog associated with the button.
  @see-class{gtk:file-chooser-button}
  @see-class{gtk:file-chooser-dialog}")

(unexport 'file-chooser-button-dialog)

;;; --- gtk:file-chooser-button-focus-on-click ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-on-click"
                                               'file-chooser-button) t)
 "The @code{focus-on-click} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the file chooser button grabs focus when it is clicked with the
  mouse. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-button-focus-on-click)
      "Accessor"
      (documentation 'file-chooser-button-focus-on-click 'function)
 "@version{2023-06-11}
  @syntax{(gtk:file-chooser-button-focus-on-click object) => focus-on-click}
  @syntax{(setf (gtk:file-chooser-button-focus-on-click object) focus-on-click)}
  @argument[object]{a @class{gtk:file-chooser-button} widget to modify}
  @argument[focus-on-click]{a boolean whether the button grabs focus when
    clicked with the mouse}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-button]{focus-on-click} slot of the
    @class{gtk:file-chooser-button} class.
  @end{short}
  The @fun{gtk:file-chooser-button-focus-on-click} function returns whether the
  button grabs focus when it is clicked with the mouse. The
  @setf{gtk:file-chooser-button-focus-on-click} function sets whether the button
  will grab focus.

  Making mouse clicks not grab focus is useful in places like toolbars where
  you do not want the keyboard focus removed from the main area of the
  application.
  @begin[Warning]{dictionary}
    The @fun{gtk:file-chooser-button-focus-on-click} function has been
    deprecated since version 3.20 and should not be used in newly written code.
    Use the @fun{gtk:widget-focus-on-click} function instead.
  @end{dictionary}
  @see-class{gtk:file-chooser-button}
  @see-function{gtk:widget-focus-on-click}")

;;; --- gtk:file-chooser-button-title ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'file-chooser-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  Title to put on the file chooser dialog associated with the button. @br{}
  Default value: \"Select a File\"")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-button-title)
      "Accessor"
      (documentation 'file-chooser-button-title 'function)
 "@version{2025-07-06}
  @syntax{(gtk:file-chooser-button-title object) => title}
  @syntax{(setf (gtk:file-chooser-button-title object) title)}
  @argument[object]{a @class{gtk:file-chooser-button} widget to modify}
  @argument[title]{a string for the browse dialog title}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-button]{title} slot of the
    @class{gtk:file-chooser-button} class.
  @end{short}
  The @fun{gtk:file-chooser-button-title} function retrieves the title of the
  browse dialog used by the the file chooser button. The
  @setf{gtk:file-chooser-button-title} function sets the title.
  @see-class{gtk:file-chooser-button}")

;;; --- gtk:file-chooser-button-width-chars ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars"
                                               'file-chooser-button) t)
 "The @code{width-chars} property of type @code{:int} (Read / Write) @br{}
  The width of the entry and label inside the button, in characters. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-button-width-chars)
      "Accessor"
      (documentation 'file-chooser-button-width-chars 'function)
 "@version{2025-07-06}
  @syntax{(gtk:file-chooser-button-width-chars object) => n-chars}
  @syntax{(setf (gtk:file-chooser-button-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:file-chooser-button} widget to modify}
  @argument[n-chars]{an integer for the width, in characters}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser-button]{width-chars} slot of the
    @class{gtk:file-chooser-button} class.
  @end{short}
  The @fun{gtk:file-chooser-button-width-chars} retrieves the width in
  characters of the file chooser button's entry and/or label. The
  @setf{gtk:file-chooser-button-width-chars} function sets the width, in
  characters, that the file chooser button will use.
  @see-class{gtk:file-chooser-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline file-chooser-button-new))

(defun file-chooser-button-new (title action)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[title]{a string for the title of the browse dialog}
  @argument[action]{a @sym{gtk:file-chooser-action} value for the open mode
    of the widget}
  @return{The new @class{gtk:file-chooser-button} widget.}
  @begin{short}
    Creates a new file selecting file chooser button widget.
  @end{short}
  @see-class{gtk:file-chooser-button}
  @see-symbol{gtk:file-chooser-action}"
  (make-instance 'file-chooser-button
                 :title title
                 :action action))

(export 'file-chooser-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_button_new_with_dialog
;;; ----------------------------------------------------------------------------

(declaim (inline file-chooser-button-new-with-dialog))

(defun file-chooser-button-new-with-dialog (dialog)
 #+liber-documentation
 "@version{2025-07-06}
  @argument[dialog]{a @class{gtk:dialog} widget to use as dialog}
  @return{The new @class{gtk:file-chooser} widget.}
  @begin{short}
    Creates a file chooser button which uses @arg{dialog} as its file picking
    window.
  @end{short}

  Note that the dialog must be a @class{gtk:dialog} widget, or subclass, which
  implements the @class{gtk:file-chooser} interface and must not have
  @slot[gtk:window]{destroy-with-parent} property set.

  Also note that the dialog needs to have its confirmative button added with
  response @code{:accept} or @code{:ok} in order for the button to take over
  the file selected in the dialog.
  @see-class{gtk:file-chooser-button}
  @see-class{gtk:file-chooser}
  @see-class{gtk:dialog}"
  (make-instance 'file-chooser-button
                 :dialog dialog))

(export 'file-chooser-button-new-with-dialog)

;;; --- End of file gtk3.file-chooser-button.lisp ------------------------------
