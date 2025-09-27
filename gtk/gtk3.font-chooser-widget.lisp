;;; ----------------------------------------------------------------------------
;;; gtk3.font-chooser-widget.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkFontChooserWidget
;;;
;;;     A widget for selecting fonts
;;;
;;; Types and Values
;;;
;;;     GtkFontChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_widget_new
;;;
;;; Properties
;;;
;;;     tweak-action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkFontChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkFontChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkFontChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontChooserWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFontChooserWidget" font-chooser-widget
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_widget_get_type")
  ((tweak-action
    font-chooser-widget-tweak-action
    "tweak-action" "GAction" t nil)))

#+liber-documentation
(setf (documentation 'font-chooser-widget 'type)
 "@version{2023-06-16}
  @begin{short}
    The @class{gtk:font-chooser-widget} widget lists the available fonts,
    styles and sizes, allowing the user to select a font.
  @end{short}
  It is used in the @class{gtk:font-chooser-dialog} widget to provide a dialog
  box for selecting fonts.

  To set or to get the font which is initially selected, use the
  @fun{gtk:font-chooser-font} or @fun{gtk:font-chooser-font-desc} functions.

  To change the text which is shown in the preview area, use the
  @fun{gtk:font-chooser-preview-text} function.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:font-chooser-widget} implementation has a single CSS node
    with name @code{fontchooser}.
  @end{dictionary}
  @see-constructor{gtk:font-chooser-widget-new}
  @see-slot{gtk:font-chooser-widget-tweak-action}
  @see-class{gtk:font-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-chooser-widget-tweak-action -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tweak-action"
                                               'font-chooser-widget) t)
 "The @code{tweak-action} property of type @class{g:action} (Read) @br{}
  A toggle action that can be used to switch to the tweak page of the font
  chooser widget, which lets the user tweak the OpenType features and variation
  axes of the selected font. The action will be enabled or disabled depending
  on whether the selected font has any features or axes.")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-widget-tweak-action)
      "Accessor"
      (documentation 'font-chooser-widget-tweak-action 'function)
 "@version{2023-06-16}
  @syntax{(gtk:font-chooser-widget-tweak-action object) => tweak-action}
  @syntax{(setf (gtk:font-chooser-widget-tweak-action object) tweak-action)}
  @argument[object]{a @class{gtk:font-chooser-widget} widget}
  @argument[tweak-action]{a @class{g:action} toggle action}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser-widget]{tweak-action} slot of the
    @class{gtk:font-chooser-widget} class.
  @end{short}
  A toggle action that can be used to switch to the tweak page of the font
  chooser widget, which lets the user tweak the OpenType features and variation
  axes of the selected font. The action will be enabled or disabled depending
  on whether the selected font has any features or axes.
  @see-class{gtk:font-chooser-widget}
  @see-function{g:action}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_widget_new
;;; ----------------------------------------------------------------------------

(declaim (inline font-chooser-widget-new))

(defun font-chooser-widget-new ()
 #+liber-documentation
 "@version{2025-07-07}
  @return{The new @class{gtk:font-chooser-widget} widget.}
  @short{Creates a new font chooser widget.}
  @see-class{gtk:font-chooser-widget}"
  (make-instance 'font-chooser-widget))

(export 'font-chooser-widget-new)

;;; --- End of file gtk3.font-chooser-widget.lisp ------------------------------
