;;; ----------------------------------------------------------------------------
;;; gtk3.recent-chooser-widget.lisp
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
;;; GtkRecentChooserWidget
;;;
;;;     Displays recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_recent_chooser_widget_new
;;;     gtk_recent_chooser_widget_new_for_manager
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBox
;;;                     ╰── GtkRecentChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkRecentChooserWidget implements AtkImplementorIface, GtkBuildable,
;;;     GtkOrientable and GtkRecentChooser.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentChooserWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkRecentChooserWidget" recent-chooser-widget
  (:superclass box
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkOrientable"
                "GtkRecentChooser")
   :type-initializer "gtk_recent_chooser_widget_get_type")
  nil)

#+liber-documentation
(setf (documentation 'recent-chooser-widget 'type)
 "@version{#2023-3-24}
  @begin{short}
    The @class{gtk:recent-chooser-widget} widget is a widget suitable for
    selecting recently used files.
  @end{short}
  It is the main building block of a @class{gtk:recent-chooser-dialog} widget.
  Most applications will only need to use the latter. You can use
  @class{gtk:recent-chooser-widget} widget as part of a larger window if you
  have special needs.

  Note that the @class{gtk:recent-chooser-widget} widget does not have any
  methods of its own. Instead, you should use the functions that work on a
  @class{gtk:recent-chooser} widget.
  @see-constructor{gtk:recent-chooser-widget-new}
  @see-constructor{gtk:recent-chooser-widget-for-manager}
  @see-class{gtk:recent-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_widget_new
;;; ----------------------------------------------------------------------------

(declaim (inline recent-chooser-widget-new))

(defun recent-chooser-widget-new ()
 #+liber-documentation
 "@version{#2025-07-07}
  @return{The new @class{gtk:recent-chooser-widget} widget.}
  @begin{short}
    Creates a new @class{gtk:recent-chooser-widget} widget.
  @end{short}
  This is an embeddable widget used to access the recently used resources list.
  @see-class{gtk:recent-chooser-widget}
  @see-function{gtk:recent-chooser-widget-new-for-manager}"
  (make-instance 'recent-chooser))

(export 'recent-chooser-widget-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_chooser_widget_new_for_manager
;;; ----------------------------------------------------------------------------

(declaim (inline recent-chooser-widget-new-for-manager))

(defun recent-chooser-widget-new-for-manager (manager)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @return{The new @class{gtk:recent-chooser-widget} widget.}
  @begin{short}
    Creates a new @class{gtk:recent-chooser-widget} widget with a specified
    recent manager.
  @end{short}

  This is useful if you have implemented your own recent manager, or if you
  have a customized instance of a @class{gtk:recent-manager} object.
  @see-class{gtk:recent-chooser-widget}
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-chooser-widget-new}"
  (make-instance 'recent-chooser-widget
                 :recent-manager manager))

(export 'recent-chooser-widget-new-for-manager)

;;; --- End of file gtk3.recent-chooser-widget.lisp ----------------------------
