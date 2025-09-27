;;; ----------------------------------------------------------------------------
;;; gtk3.separator-tool-item.lisp
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
;;; GtkSeparatorToolItem
;;;
;;;     A toolbar item that separates groups of other toolbar items
;;;
;;; Types and Values
;;;
;;;     GtkSeparatorToolItem
;;;
;;; Functions
;;;
;;;     gtk_separator_tool_item_new
;;;     gtk_separator_tool_item_set_draw                   Accessor
;;;     gtk_separator_tool_item_get_draw                   Accessor
;;;
;;; Properties
;;;
;;;     draw
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkToolItem
;;;                         ╰── GtkSeparatorToolItem
;;;
;;; Implemented Interfaces
;;;
;;;     GtkSeparatorToolItem implements AtkImplementorIface, GtkBuildable and
;;;     GtkActivatable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSeparatorToolItem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSeparatorToolItem" separator-tool-item
  (:superclass tool-item
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkActivatable")
   :type-initializer "gtk_separator_tool_item_get_type")
  ((draw
    separator-tool-item-draw
    "draw" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'separator-tool-item 'type)
 "@version{#2023-03-24}
  @begin{short}
    The @class{gtk:separator-tool-item} widget is a @class{gtk:tool-item} widget
    that separates groups of other @class{gtk:tool-item} widgets.
  @end{short}
  Depending on the theme, a separator tool item will often look like a vertical
  line on horizontally docked toolbars.

  If the @code{expand} child property of the @class{gtk:toolbar} widget is
  @em{true} and the @code{draw} property is @em{false}, a separator tool item
  will act as a \"spring\" that forces other items to the ends of the toolbar.

  Use the @fun{gtk:separator-tool-item-new} function to create a new
  @class{gtk:separator-tool-item} widget.
  @see-constructor{gtk:separator-tool-item-new}
  @see-slot{gtk:separator-tool-item-draw}
  @see-class{gtk:tool-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw" 'separator-tool-item) t)
 "The @code{draw} property of type @code{:boolean} (Read / Write) @br{}
  Whether the separator is drawn, or just blank. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'separator-tool-item-draw)
      "Accessor"
      (documentation 'separator-tool-item-draw 'function)
 "@version{#2023-03-24}
  @syntax{(gtk:separator-tool-item-draw object) => draw)}
  @syntax{(setf (gtk:separator-tool-item-draw object) draw)}
  @argument[object]{a @class{gtk:separator-tool-item} widget}
  @argument[draw]{a boolean whether @arg{object} is drawn as a vertical line}
  @begin{short}
    Accessor of the @slot[gtk:separator-tool-item]{draw} slot of the
    @class{gtk:separator-tool-item} class.
  @end{short}
  The @fun{gtk:separator-tool-item-draw} function returns whether the separator
  tool item is drawn as a line, or just blank. The
  @setf{gtk:separator-tool-item-draw} function returns whether item is drawn as
  a vertical line, or just blank.

  Setting this to @em{false} along with the @fun{gtk:tool-item-expand} function
  is useful to create an item that forces following items to the end of the
  toolbar.
  @see-class{gtk:separator-tool-item}
  @see-function{gtk:tool-item-expand}")

;;; ----------------------------------------------------------------------------
;;; gtk_separator_tool_item_new
;;; ----------------------------------------------------------------------------

(declaim (inline separator-tool-item-new))

(defun separator-tool-item-new ()
 #+liber-documentation
 "@version{#2023-03-24}
  @return{The new @class{gtk:separator-tool-item} widget.}
  @begin{short}
    Create a new separator tool item.
  @end{short}
  @see-class{gtk:separator-tool-item}"
  (make-instance 'separator-tool-item-new))

(export 'separator-tool-item-new)

;;; --- End of file gtk3.separator-tool-item.lisp ------------------------------
