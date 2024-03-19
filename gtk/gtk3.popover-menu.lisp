;;; ----------------------------------------------------------------------------
;;; gtk3.popover-menu.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkPopoverMenu
;;;
;;;     Popovers to use as menus
;;;
;;; Types and Values
;;;
;;;     GtkPopoverMenu
;;;
;;; Functions
;;;
;;;     gtk_popover_menu_new
;;;     gtk_popover_menu_open_submenu
;;;
;;; Properties
;;;
;;;     visible-submenu
;;;
;;; Child Properties
;;;
;;;     position
;;;     submenu
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkPopover
;;;                         ╰── GtkPopoverMenu
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPopoverMenu implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkPopoverMenu
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkPopoverMenu" popover-menu
  (:superclass popover
    :export t
    :interfaces ("AtkImplementorIface"
                 "GtkBuildable")
    :type-initializer "gtk_popover_menu_get_type")
  ((visible-submenu
    popover-menu-visible-submenu
    "visible-submenu" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'popover-menu 'type)
 "@version{#2023-3-12}
  @begin{short}
    The @class{gtk:popover-menu} class is a subclass of the @class{gtk:popover}
    class that treats its children like menus and allows switching between them.
  @end{short}
  It is meant to be used primarily together with @class{gtk:model-button}
  widgets, but any widget can be used, such as @class{gtk:spin-button} or
  @class{gtk:scale} widgets. In this respect, the @class{gtk:popover-menu}
  widget is more flexible than popovers that are created from a
  @class{g:menu-model} object with the @fun{gtk:popover-new-from-model}
  function.

  To add a child as a submenu, set the @code{submenu} child property to the
  name of the submenu. To let the user open this submenu, add a
  @class{gtk:model-button} widget whose @slot[gtk:model-button]{menu-name}
  property is set to the name you have given to the submenu.

  By convention, the first child of a submenu should be a
  @class{gtk:model-button} widget to switch back to the parent menu. Such a
  button should use the @slot[gtk:model-button]{inverted} and
  @slot[gtk:model-button]{centered} properties to achieve a title-like
  appearance and place the submenu indicator at the opposite side. To switch
  back to the main menu, use @code{main} as the menu name.
  @begin{examples}
    @begin{pre}
<object class=\"GtkPopoverMenu\">
  <child>
    <object class=\"GtkBox\">
      <property name=\"visible\">True</property>
      <property name=\"margin\">10</property>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">win.frob</property>
          <property name=\"text\" translatable=\"yes\">Frob</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"menu-name\">more</property>
          <property name=\"text\" translatable=\"yes\">More</property>
        </object>
      </child>
    </object>
  </child>
  <child>
    <object class=\"GtkBox\">
      <property name=\"visible\">True</property>
      <property name=\"margin\">10</property>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">win.foo</property>
          <property name=\"text\" translatable=\"yes\">Foo</property>
        </object>
      </child>
      <child>
        <object class=\"GtkModelButton\">
          <property name=\"visible\">True</property>
          <property name=\"action-name\">win.bar</property>
          <property name=\"text\" translatable=\"yes\">Bar</property>
        </object>
      </child>
    </object>
    <packing>
      <property name=\"submenu\">more</property>
    </packing>
  </child>
</object>
    @end{pre}
  @end{examples}
  @begin[CSS nodes]{dictionary}
    Just like normal popovers created using the @fun{gtk:popover-new-from-model}
    function, the @class{gtk:popover-menu} instances have a single CSS node
    called @code{popover} and get the @code{.menu} style class.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[code]{table}
      @begin[position]{entry}
        The @code{position} child property of type @code{:int} (Read / Write)
        @br{}
        The index of the child in the parent. @br{}
        Allowed values: >= -1 @br{}
        Default value: 0
      @end{entry}
      @begin[submenu]{entry}
        The @code{submenu} child property of type @code{:string} (Read / Write)
        @br{}
        Specifies the name of the submenu. If it is @code{nil} or @code{main},
        the child is used as the main menu, which is shown initially when the
        popover is mapped. @br{}
        Default value: @code{nil}
      @end{entry}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:popover-menu-new}
  @see-slot{gtk:popover-menu-visible-submenu}
  @see-class{gtk:popover}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:popover-menu-visible-submenu ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible-submenu"
                                               'popover-menu) t)
 "The @code{visible-submenu} property of type @code{:string} (Read / Write)
  @br{}
  The name of the visible submenu. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'popover-menu-visible-submenu)
      "Accessor"
      (documentation 'popover-menu-visible-submenu 'function)
 "@version{#2023-3-12}
  @syntax{(gtk:popover-menu-visible-submenu object) => submenu}
  @syntax{(setf (gtk:popover-menu-visible-submenu object) submenu)}
  @argument[object]{a @class{gtk:popover-menu} widget}
  @argument[submenu]{a string with the name of the submenu}
  @begin{short}
    Accessor of the @slot[gtk:popover-menu]{visible-submenu} slot of the
    @class{gtk:popover-menu} class.
  @end{short}
  The name of the visible submenu.
  @see-class{gtk:popover-menu}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:popover-menu-child-position ----------------------------------------

(define-child-property popover-menu-child-position "position" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'popover-menu-child-position)
      "Accessor"
      (documentation 'popover-menu-child-position 'function)
 "@version{#2023-3-12}
  @syntax{(gtk:popover-menu-child-position container child) => position}
  @syntax{(setf (gtk:popover-menu-child-position container child) position)}
  @argument[container]{a @class{gtk:popover-menu} widget}
  @argument[child]{a @class{gtk:widget} object}
  @argument[position]{an integer with the index of the child in the parent}
  @begin{short}
    Accessor of the @code{position} child property of the
    @class{gtk:popover-menu} class.
  @end{short}
  The index of the child in the parent.
  @see-class{gtk:popover-menu}
  @see-class{gtk:widget}")

;;; --- gtk:popover-menu-child-submenu -----------------------------------------

(define-child-property popover-menu-child-submenu "submenu" "gchararray" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'popover-menu-child-submenu)
      "Accessor"
      (documentation 'popover-menu-child-submenu 'function)
 "@version{#2023-3-12}
  @syntax{(gtk:popover-menu-child-submenu container child) => submenu}
  @syntax{(setf (gtk:popover-menu-child-submenu container child) submenu)}
  @argument[container]{a @class{gtk:popover-menu} widget}
  @argument[child]{a @class{gtk:widget} object}
  @argument[submenu]{a string with the name of the submenu}
  @begin{short}
    Accessor of the @code{submenu} child property of the
    @class{gtk:popover-menu} class.
  @end{short}
  The @code{submenu} child property specifies the name of the submenu. If it is
  @code{nil} or @code{main}, the child is used as the main menu, which is shown
  initially when the popover is mapped.
  @see-class{gtk:popover-menu}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline popover-menu-new))

(defun popover-menu-new ()
 "@version{#2023-3-12}
  @return{A new @class{gtk:popover-menu} widget.}
  @short{Creates a new popover menu.}
  @see-class{gtk:popover-menu}"
  (make-instance 'popover-menu))

(export 'popover-menu-new)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_open_submenu ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_menu_open_submenu" popover-menu-open-submenu) :void
 "@version{#2023-3-12}
  @argument[popover]{a @class{gtk:popover-menu} widget}
  @argument[name]{a string with the name of the menu to switch to}
  @begin{short}
    Opens a submenu of the popover.
  @end{short}
  The name must be one of the names given to the submenus of the popover with
  \"submenu\", or \"main\" to switch back to the main menu.

  The @class{gtk:model-button} widget will open submenus automatically when the
  @slot[gtk:model-button]{menu-name} property is set, so this function is only
  needed when you are using other kinds of widgets to initiate menu changes.
  @see-class{gtk:popover-menu}
  @see-class{gtk:model-button}
  @see-function{gtk:model-button-menu-name}"
  (popover (g:object popover-menu))
  (name :string))

(export 'popover-menu-open-submenu)

;;; --- End of file gtk3.popover-menu.lisp -------------------------------------
