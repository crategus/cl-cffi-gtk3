;;; ----------------------------------------------------------------------------
;;; gtk3.menu-bar.lisp
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
;;; GtkMenuBar
;;;
;;;     A subclass of GtkMenuShell which holds GtkMenuItem widgets
;;;
;;; Types and Values
;;;
;;;     GtkMenuBar
;;;     GtkPackDirection
;;;
;;; Functions
;;;
;;;     gtk_menu_bar_new
;;;     gtk_menu_bar_new_from_model
;;;     gtk_menu_bar_set_pack_direction
;;;     gtk_menu_bar_get_pack_direction
;;;     gtk_menu_bar_set_child_pack_direction
;;;     gtk_menu_bar_get_child_pack_direction
;;;
;;; Properties
;;;
;;;     child-pack-direction
;;;     pack-direction
;;;
;;; Style Properties
;;;
;;;     internal-padding
;;;     shadow-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkMenuShell
;;;                     ╰── GtkMenuBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkMenuBar implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPackDirection
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPackDirection" pack-direction
  (:export t
   :type-initializer "gtk_pack_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb 2)
  (:btt 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'pack-direction)
      "GEnum"
      (liber:symbol-documentation 'pack-direction)
 "@version{#2025-06-27}
  @begin{declaration}
(gobject:define-genum \"GtkPackDirection\" pack-direction
  (:export t
   :type-initializer \"gtk_pack_direction_get_type\")
  (:ltr 0)
  (:rtl 1)
  (:ttb 2)
  (:btt 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:ltr]{Widgets are packed left-to-right.}
      @entry[:rtl]{Widgets are packed right-to-left.}
      @entry[:ttb]{Widgets are packed top-to-bottom.}
      @entry[:btt]{Widgets are packed bottom-to-top.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Determines how widgets should be packed insided menubars and menuitems
    contained in menubars.
  @end{short}
  @see-class{gtk:menu-bar}")

;;; ----------------------------------------------------------------------------
;;; GtkMenuBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMenuBar" menu-bar
  (:superclass menu-shell
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_menu_bar_get_type")
  ((child-pack-direction
    menu-bar-child-pack-direction
    "child-pack-direction" "GtkPackDirection" t t)
   (pack-direction
    menu-bar-pack-direction
    "pack-direction" "GtkPackDirection" t t)))

#+liber-documentation
(setf (documentation 'menu-bar 'type)
 "@version{#2023-3-21}
  @begin{short}
    The @class{gtk:menu-bar} class is a subclass of the @class{gtk:menu-shell}
    class which contains one or more @class{gtk:menu-item} widgets.
  @end{short}
  The result is a standard menu bar which can hold many menu items.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:menu-bar} implementation has a single CSS node with name
    @code{menubar}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[menu-bar:internal-padding]{property}
      The @code{internal-padding} style property of type @code{:int} (Read)
      @br{}
      Amount of border space between the menu bar shadow and the menu items.
      @br{}
      @em{Warning:} The @code{internal-padding} style property has been
      deprecated since version 3.8 and should not be used in newly written
      code. Use the standard padding CSS property, through objects like
      @class{gtk:style-context} and @class{gtk:css-provider}. The value of
      this style property is ignored. @br{}
      Allowed values: >= 0 @br{}
      Default value: 1
    @end{property}
    @begin[menu-bar:shadow-type]{property}
      The @code{shadow-type} style property of type @sym{gtk:shadow-type} (Read)
      @br{}
      The style of bevel around the menu bar. @br{}
      @em{Warning:} The @code{shadow-type} style property has been deprecated
      since version 3.20 and should not be used in newly written code. Use CSS
      to determine the shadow. The value of this style property is ignored.
      @br{}
      Default value: @val[gtk:shadow-type]{:out}
    @end{property}
  @end{dictionary}
  @see-constructor{gtk:menu-bar-new}
  @see-constructor{gtk:menu-bar-new-from-model}
  @see-slot{gtk:menu-bar-child-pack-direction}
  @see-slot{gtk:menu-bar-pack-direction}
  @see-class{gtk:menu-shell}
  @see-class{gtk:menu-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:menu-bar-child-pack-direction --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child-pack-direction"
                                               'menu-bar) t)
 "The @code{child-pack-direction} property of type @sym{gtk:pack-direction}
  (Read / Write) @br{}
  The child pack direction of the menu bar. It determines how the widgets
  contained in child menuitems are arranged. @br{}
  Default value: @val[gt:pack-direction]{:ltr}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-bar-child-pack-direction)
      "Accessor"
      (documentation 'menu-bar-child-pack-direction 'function)
 "@version{#2025-07-11}
  @syntax{(gtk:menu-bar-child-pack-direction object) => direction}
  @syntax{(setf (gtk:menu-bar-child-pack-direction object) direction)}
  @argument[object]{a @class{gtk:menu-bar} widget}
  @argument[direction]{a value of the @sym{gtk:pack-direction} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:menu-bar]{child-pack-direction} slot of the
    @class{gtk:menu-bar} class.
  @end{short}
  The @fun{gtk:menu-bar-child-pack-direction} function retrieves the current
  child pack direction of the menu bar. The
  @setf{gtk:menu-bar-child-pack-direction} function sets how widgets should be
  packed inside the children of a menu bar.
  @see-class{gtk:menu-bar}
  @see-symbol{gtk:pack-direction}")

;;; --- gtk:menu-bar-pack-direction --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pack-direction" 'menu-bar) t)
 "The @code{pack-direction} property of type @sym{gtk:pack-direction}
  (Read / Write) @br{}
  The pack direction of the menu bar. It determines how menuitems are arranged
  in the menu bar. @br{}
  Default value: @val[gtk:pack-direction]{:ltr}")

#+liber-documentation
(setf (liber:alias-for-function 'menu-bar-pack-direction)
      "Accessor"
      (documentation 'menu-bar-pack-direction 'function)
 "@version{#2025-07-11}
  @syntax{(gtk:menu-bar-pack-direction object) => direction}
  @syntax{(setf (gtk:menu-bar-pack-direction object) direction)}
  @argument[menubar]{a @class{gtk:menu-bar} widget}
  @argument[direction]{a value of the @sym{gtk:pack-direction} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:menu-bar]{pack-direction} slot of the
    @class{gtk:menu-bar} class.
  @end{short}
  The @fun{gtk:menu-bar-child-pack-direction} function retrieves the current
  pack direction of the menu bar. The @setf{gtk:menu-bar-child-pack-direction}
  function sets how items should be packed inside a menu bar.
  @see-class{gtk:menu-bar}
  @see-symbol{gtk:pack-direction}")

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline menu-bar-new))

(defun menu-bar-new ()
 #+liber-documentation
 "@version{#2023-3-21}
  @return{The new @class{gtk:menu-bar} widget.}
  @begin{short}
    Creates a new menu bar.
  @end{short}
  @see-class{gtk:menu-bar}
  @see-function{gtk:menu-new-from-model}"
  (make-instance 'menu-bar))

(export 'menu-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_menu_bar_new_from_model
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_menu_bar_new_from_model" menu-bar-new-from-model)
    (g:object menu-bar)
 #+liber-documentation
 "@version{#2025-07-07}
  @argument[model]{a @class{g:menu-model} object}
  @return{The new @class{gtk:menu-bar} widget.}
  @begin{short}
    Creates a new menu bar and populates it with menu items and
    submenus according to the menu model.
  @end{short}
  The created menu items are connected to actions found in the
  @class{gtk:application-window} widget to which the menu bar belongs -
  typically by means of being contained within the
  @class{gtk:application-window} widgets hierarchy.
  @see-class{gtk:menu-bar}
  @see-class{g:menu-model}
  @see-class{gtk:application-window}"
  (model (g:object g:menu-model)))

(export 'menu-bar-new-from-model)

;;; --- End of file gtk3.menu-bar.lisp -----------------------------------------
