;;; ----------------------------------------------------------------------------
;;; gtk3.notebook.lisp
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
;;; GtkNotebook
;;;
;;;     A tabbed notebook container
;;;
;;; Types and Values
;;;
;;;     GtkNotebook
;;;
;;;     gtk_notebook_set_group_name
;;;     gtk_notebook_get_group_name
;;;     gtk_notebook_get_scrollable
;;;     gtk_notebook_set_scrollable
;;;     gtk_notebook_get_show_border
;;;     gtk_notebook_set_show_border
;;;     gtk_notebook_get_show_tabs
;;;     gtk_notebook_set_show_tabs
;;;     gtk_notebook_get_tab_pos
;;;     gtk_notebook_set_tab_pos
;;;
;;; Functions
;;;
;;;     gtk_notebook_new
;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page
;;;     gtk_notebook_detach_tab
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child
;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_get_nth_page
;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_menu_label
;;;     gtk_notebook_set_menu_label_text
;;;     gtk_notebook_set_tab_label
;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_set_tab_reorderable
;;;     gtk_notebook_set_tab_detachable
;;;     gtk_notebook_get_menu_label_text
;;;     gtk_notebook_get_tab_label_text
;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_get_tab_hborder                        deprecated
;;;     gtk_notebook_get_tab_vborder                        deprecated
;;;     gtk_notebook_set_current_page
;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget
;;;
;;; Properties
;;;
;;;     enable-popup
;;;     group-name
;;;     page
;;;     scrollable
;;;     show-border
;;;     show-tabs
;;;     tab-pos
;;;
;;; Child Properties
;;;
;;;     detachable
;;;     menu-label
;;;     position
;;;     reorderable
;;;     tab-expand
;;;     tab-fill
;;;     tab-label
;;;
;;; Style Properties
;;;
;;;     arrow-spacing
;;;     has-backward-stepper
;;;     has-forward-stepper
;;;     has-secondary-backward-stepper
;;;     has-secondary-forward-stepper
;;;     has-tab-gap
;;;     initial-gap
;;;     tab-curvature
;;;     tab-overlap
;;;
;;; Signals
;;;
;;;     change-current-page
;;;     create-window
;;;     focus-tab
;;;     move-focus-out
;;;     page-added
;;;     page-removed
;;;     page-reordered
;;;     reorder-tab
;;;     select-page
;;;     switch-page
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkNotebook
;;;
;;; Implemented Interfaces
;;;
;;;     GtkNotebook implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNotebookTab
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkNotebookTab" notebook-tab
  (:export t
   :type-initializer "gtk_notebook_tab_get_type")
  (:tab-first 0)
  (:tab-last 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'notebook-tab)
      "GEnum"
      (liber:symbol-documentation 'notebook-tab)
 "@version{#2025-06-27}
  @begin{declaration}
(gobject:define-genum \"GtkNotebookTab\" notebook-tab
  (:export t
   :type-initializer \"gtk_notebook_tab_get_type\")
  (:tab-first 0)
  (:tab-last 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:tab-first]{}
      @entry[:tab-last]{}
    @end{simple-table}
  @end{values}
  @begin{short}
    The values of this enumeration are used as arguments of the
    @sig[gtk:notebook]{focus-tab} signal.
  @end{short}
  @see-class{gtk:notebook}")

;;; ----------------------------------------------------------------------------
;;; GtkNotebook
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkNotebook" notebook
  (:superclass container
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_notebook_get_type")
  ((enable-popup
    notebook-enable-popup
    "enable-popup" "gboolean" t t)
   (group-name
    notebook-group-name
    "group-name" "gchararray" t t)
   (page
    notebook-page
    "page" "gint" t t)
   (scrollable
    notebook-scrollable
    "scrollable" "gboolean" t t)
   (show-border
    notebook-show-border
    "show-border" "gboolean" t t)
   (show-tabs
    notebook-show-tabs
    "show-tabs" "gboolean" t t)
   (tab-pos
    notebook-tab-pos
    "tab-pos" "GtkPositionType" t t)))

#+liber-documentation
(setf (documentation 'notebook 'type)
 "@version{#2025-07-15}
  @begin{short}
    The @class{gtk:notebook} widget is a @class{gtk:container} widget whose
    children are pages that can be switched between using tab labels along one
    edge.
  @end{short}

  @image[notebook]{Figure: GtkNotebook}

  There are many configuration options for @class{gtk:notebook} widgets. Among
  other things, you can choose on which edge the tabs appear, see the
  @fun{gtk:notebook-tab-pos} function, whether, if there are too many tabs to
  fit the notebook should be made bigger or scrolling arrows added, see the
  @fun{gtk:notebook-scrollable} function, and whether there will be a popup menu
  allowing the users to switch pages, see the @fun{gtk:notebook-popup-enable}
  and @fun{gtk:notebook-popup-disable} functions.
  @begin[GtkNotebook as GtkBuildable]{dictionary}
    The @class{gtk:notebook} implementation of the @class{gtk:buildable}
    interface supports placing children into tabs by specifying @code{\"tab\"}
    as the @code{\"type\"} attribute of a @code{<child>} element. Note that the
    content of the tab must be created before the tab can be filled. A tab child
    can be specified without specifying a @code{<child>} type attribute. To add
    a child widget in the notebooks action area, specify @code{\"action-start\"}
    or @code{\"action-end\"} as the @code{\"type\"} attribute of the
    @code{<child>} element.

    @b{Example:} A UI definition fragment with the @class{gtk:notebook} widget
    @begin{pre}
<object class=\"GtkNotebook\">
  <child>
    <object class=\"GtkLabel\" id=\"notebook-content\">
      <property name=\"label\">Content</property>
    </object>
  </child>
  <child type=\"tab\">
    <object class=\"GtkLabel\" id=\"notebook-tab\">
      <property name=\"label\">Tab</property>
    </object>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
notebook
├── header.top
│   ├── [<action widget>]
│   ├── tabs
│   │   ├── [arrow]
│   │   ├── tab
│   │   │   ╰── <tab label>
│   │   │
│   │   ├── tab[.reorderable-page]
│   │   │   ╰── <tab label>
│   │   ╰── [arrow]
│   ╰── [<action widget>]
│
╰── stack
    ├── <child>
    │
    ╰── <child>
    @end{pre}
    The @class{gtk:notebook} implementation has a main CSS node with name
    @code{notebook}, a subnode with name @code{header} and below that a subnode
    with name @code{tabs} which contains one subnode per tab with name
    @code{tab}.

    If action widgets are present, their CSS nodes are placed next to the tabs
    node. If the notebook is scrollable, CSS nodes with name @code{arrow} are
    placed as first and last child of the tabs node.

    The main node gets the @code{.frame} style class when the notebook has a
    border, see the @fun{gtk:notebook-show-border} function.

    The header node gets one of the @code{.top}, @code{.bottom}, @code{.left} or
    @code{.right} style classes, depending on where the tabs are placed. For
    reorderable pages, the tab node gets the @code{.reorderable-page} style
    class.

    A tab node gets the @code{.dnd} style class while it is moved with
    drag and drop.

    The nodes are always arranged from left-to-right, regardless of text
    direction.
  @end{dictionary}
  @begin[Child Property Details]{dictionary}
    @begin[notebook:detachable]{property}
      The @code{detachable} child property of type @code{:boolean}
      (Read / Write) @br{}
      Whether the tab is detachable. @br{}
      Default value: @em{false}
    @end{property}
    @begin[notebook:menu-label]{property}
      The @code{menu-label} child property of type @code{:string}
      (Read / Write) @br{}
      The string displayed in the menu entry of the child page. @br{}
      Default value: @code{nil}
    @end{property}
    @begin[notebook:position]{property}
      The @code{position} child property of type @code{:int} (Read / Write)
      @br{}
      The index of the child page in the parent. @br{}
      Allowed values: >= 0 @br{}
      Default value: 0
    @end{property}
    @begin[notebook:reorderable]{property}
      The @code{reorderable} child property of type @code{:boolean}
      (Read / Write) @br{}
      Whether the tab is reorderable by user action. @br{}
      Default value: @em{false}
    @end{property}
    @begin[notebook:tab-expand]{property}
      The @code{tab-expand} child property of type @code{:boolean}
      (Read / Write) @br{}
      Whether to expand the tab of the child page. @br{}
      Default value: @em{false}
    @end{property}
    @begin[notebook:tab-fill]{property}
      The @code{tab-fill} child property of type @code{:boolean}
      (Read / Write) @br{}
      Whether the tab of the child page should fill the allocated area. @br{}
      Default value: @em{true}
    @end{property}
    @begin[notebook:tab-label]{property}
      The @code{tab-label} child property of type @code{:string}
      (Read / Write) @br{}
      The string displayed on the tab label of the child page. @br{}
      Default value: @code{nil}
    @end{property}
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[notebook:arrow-spacing]{property}
      The @code{arrow-spacing} style property of type @code{:int} (Read) @br{}
      Defines the spacing between the scroll arrows and the tabs. @br{}
      @em{Warning:} The @code{arrow-spacing} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. This property is ignored. Use margins on arrows or the @code{tabs}
      node to achieve the same effect. @br{}
      Allowed values: >= 0 @br{}
      Default value: 0
    @end{property}
    @begin[notebook:has-backward-stepper]{property}
      The @code{has-backward-stepper} style property of type @code{:boolean}
      (Read) @br{}
      Determines whether the standard backward arrow button is displayed.
      @br{}
      Default value: @em{true}
    @end{property}
    @begin[notebook:has-forward-stepper]{property}
      The @code{has-forward-stepper} style property of type @code{:boolean}
      (Read) @br{}
      Determines whether the standard forward arrow button is displayed. @br{}
      Default value: @em{true}
    @end{property}
    @begin[notebook:has-secondary-backward-stepper]{property}
      The @code{has-secondary-backward-stepper} style property of type
      @code{:boolean} (Read) @br{}
      Determines whether a second backward arrow button is displayed on the
      opposite end of the tab area. @br{}
      Default value: @em{false}
    @end{property}
    @begin[notebook:has-secondary-forward-stepper]{property}
      The @code{has-secondary-forward-stepper} style property of type
      @code{:boolean} (Read) @br{}
      Determines whether a second forward arrow button is displayed on the
      opposite end of the tab area. @br{}
      Default value: @em{false}
    @end{property}
    @begin[notebook:has-tab-gap]{property}
      The @code{has-tab-gap} style property of type @code{:boolean} (Read)
      @br{}
      Defines whether the active tab is draw with a gap at the bottom. @br{}
      @em{Warning:} The @code{has-tab-gap} style property has been deprecated
      since version 3.20 and should not be used in newly written code. This
      function always behaves as if it was set to @em{false}. @br{}
      Default value: @em{true}
    @end{property}
    @begin[notebook:initial-gap]{property}
      The @code{initial-gap} style property of type @code{:int} (Read) @br{}
      Defines the minimum size for the initial gap between the first tab.@br{}
      @em{Warning:} The @code{initial-gap} style property has been deprecated
      since version 3.20 and should not be used in newly written code. The
      intial gap is ignored. Use margins on the header node to achieve the
      same effect. @br{}
      Allowed values: >= 0 @br{}
      Default value: 0
    @end{property}
    @begin[notebook:tab-curvature]{property}
      The @code{tab-curvature} style property of type @code{:int} (Read) @br{}
      Defines size of tab curvature. @br{}
      @em{Warning:} The @code{tab-curvature} style property has been
      deprecated since version 3.20 and should not be used in newly written
      code. This property is ignored. Use margins on tab nodes to achieve the
      same effect. @br{}
      Allowed values: >= 0 @br{}
      Default value: 1
    @end{property}
    @begin[notebook:tab-overlap]{property}
      The @code{tab-overlap} style property of type @code{:int} (Read) @br{}
      Defines size of tab overlap area. @br{}
      @em{Warning;} The @code{tab-overlap} style property has been deprecated
      since version 3.20 and should not be used in newly written code. This
      property is ignored. Use margins on tab nodes to achieve the same
      effect. @br{}
      Default value: 2
    @end{property}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[notebook::change-current-page]{signal}
      @begin{pre}
lambda (notebook offset)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[offset]{The integer for the offset to step forward or backward
          for a negative integer.}
      @end{simple-table}
    @end{signal}
    @begin[notebook::create-window]{signal}
      @begin{pre}
lambda (notebook page x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[page]{The @class{gtk:widget} tab of @arg{notebook} that is being
          detached.}
        @entry[x]{The integer for the x coordinate where the drop happens.}
        @entry[y]{The integer for the y coordinate where the drop happens.}
        @entry[Returns]{The @class{gtk:notebook} widget that @arg{page} should
          be added to, or @code{nil}.}
      @end{simple-table}
      The signal is emitted when a detachable tab is dropped on the root window.
      A handler for this signal can create a window containing a notebook where
      the tab will be attached. It is also responsible for moving/resizing the
      window and adding the necessary properties to the notebook, for example
      the @slot[gtk:notebook]{group-name} property.
    @end{signal}
    @begin[notebook::focus-tab]{signal}
      @begin{pre}
lambda (notebook tab)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[tab]{The value of the @sym{gtk:notebook-tab} enumeration.}
      @end{simple-table}
    @end{signal}
    @begin[notebook::move-focus-out]{signal}
      @begin{pre}
lambda (notebook direction)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[direction]{The value of the @sym{gtk:direction-type}
          enumeration.}
      @end{simple-table}
    @end{signal}
    @begin[notebook::page-added]{signal}
      @begin{pre}
lambda (notebook child num)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk:widget} child page affected.}
        @entry[num]{The unsigned integer with the child page number.}
      @end{simple-table}
      The signal is emitted in the notebook right after a page is added to the
      notebook.
    @end{signal}
    @begin[notebook::page-removed]{signal}
      @begin{pre}
lambda (notebook child num)   :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk:widget} child page affected.}
        @entry[num]{The unsigned integer with the child page number.}
      @end{simple-table}
      The signal is emitted in the notebook right after a page is removed from
      the notebook.
    @end{signal}
    @begin[notebook::page-reordered]{signal}
      @begin{pre}
lambda (notebook child num)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk:widget} child page affected.}
        @entry[num]{The unsigned integer with the child page number.}
      @end{simple-table}
      The signal is emitted in the notebook right after a page has been
      reordered.
    @end{signal}
    @begin[notebook::reorder-tab]{signal}
      @begin{pre}
lambda (notebook direction move-to-last)   :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[direction]{The value of the @sym{gtk:direction-type}
          enumeration.}
        @entry[move-to-last]{The boolean.}
      @end{simple-table}
    @end{signal}
    @begin[notebook::select-page]{signal}
      @begin{pre}
lambda (notebook move-focus)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[move-focus]{The boolean.}
      @end{simple-table}
    @end{signal}
    @begin[notebook::switch-page]{signal}
      @begin{pre}
lambda (notebook page num)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[page]{The @class{gtk:widget} current page.}
        @entry[num]{The unsigned integer with the index of the page.}
      @end{simple-table}
      Emitted when the user or a function changes the current page.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:notebook-new}
  @see-slot{gtk:notebook-enable-popup}
  @see-slot{gtk:notebook-group-name}
  @see-slot{gtk:notebook-page}
  @see-slot{gtk:notebook-scrollable}
  @see-slot{gtk:notebook-show-border}
  @see-slot{gtk:notebook-show-tabs}
  @see-slot{gtk:notebook-tab-pos}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:notebook-enable-popup ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-popup" 'notebook) t)
 "The @code{enable-popup} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-enable-popup)
      "Accessor"
      (documentation 'notebook-enable-popup 'function)
 "@version{#2023-03-21}
  @syntax{(gtk:notebook-enable-popup object) => enable}
  @syntax{(setf (gtk:notebook-enable-popup object) enable)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[enable]{if @em{true}, pops up a menu}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{enable-popup} slot of the
    @class{gtk:notebook} class.
  @end{short}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-popup-enable}
  @see-function{gtk:notebook-popup-disable}")

;;; --- gtk:notebook-group-name ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group-name" 'notebook) t)
 "The @code{group-name} property of type @code{:string} (Read / Write) @br{}
  The group name for tab drag and drop. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-group-name)
      "Accessor"
      (documentation 'notebook-group-name 'function)
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-group-name object) => name}
  @syntax{(setf (gtk:notebook-group-name object) name)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[name]{a string for the name of the notebook group, or @code{nil}
    to unset it}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{group-name} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-group-name} function gets the current group name for the
  notebook. The @setf{gtk:notebook-group-name} function sets a group name.

  Notebooks with the same name will be able to exchange tabs via drag and
  drop. A notebook with a @code{nil} group name will not be able to exchange
  tabs with any other notebook.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-page ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page" 'notebook) t)
 "The @code{page} property of type @code{:int} (Read / Write) @br{}
  The index of the current page. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page)
      "Accessor"
      (documentation 'notebook-page 'function)
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-page object) => page}
  @syntax{(setf (gtk:notebook-page object) page)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[page]{an integer for the index of the current page}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{page} slot of the @class{gtk:notebook}
    class.
  @end{short}
  The index of the current page.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-scrollable ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scrollable" 'notebook) t)
 "The @code{scrollable} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, scroll arrows are added if there are too many tabs to fit. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-scrollable)
      "Accessor"
      (documentation 'notebook-scrollable 'function)
 "@version{#2023-03-21}
  @syntax{(gtk:notebook-scrollable object) => scrollable}
  @syntax{(setf (gtk:notebook-scrollable object) scrollable)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[scrollable]{@em{true} if scroll arrows should be added}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{scrollable} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-scrollable} function returns whether the tab label area
  has arrows for scrolling if there are too many tabs to fit in the area. The
  @setf{gtk:notebook-scrollable} function sets whether the tab label area will
  have arrows for scrolling.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-show-border -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-border" 'notebook) t)
 "The @code{show-border} property of type @code{:boolean} (Read / Write) @br{}
  Whether the border should be shown. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-show-border)
      "Accessor"
      (documentation 'notebook-show-border 'function)
 "@version{#2023-03-21}
  @syntax{(gtk:notebook-show-border object) => show-border}
  @syntax{(setf (gtk:notebook-show-border object) show-border)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[show-border]{@em{true} if a bevel should be drawn around the
    notebook}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{show-border} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-show-border} function returns whether a bevel will be
  drawn around the notebook pages. The @setf{gtk:notebook-show-border} function
  sets whether a bevel will be drawn.

  This only has a visual effect when the tabs are not shown. See the
  @fun{gtk:notebook-show-tabs} function.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-show-tabs}")

;;; --- gtk:notebook-show-tabs -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-tabs" 'notebook) t)
 "The @code{show-tabs} property of type @code{:boolean} (Read / Write) @br{}
  Whether tabs should be shown. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-show-tabs)
      "Accessor"
      (documentation 'notebook-show-tabs 'function)
 "@version{#2023-03-21}
  @syntax{(gtk:notebook-show-tabs object) => show-tabs}
  @syntax{(setf (gtk:notebook-show-tabs object) show-tabs)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[show-tabs]{@em{true} if the tabs should be shown}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{show-tabs} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-show-tabs} function returns whether the tabs of the
  notebook are shown. The @setf{gtk:notebook-show-tabs} function sets whether
  to show the tabs.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-tab-pos ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tab-pos" 'notebook) t)
 "The @code{tab-pos} property of type @sym{gtk:position-type} (Read / Write)
  @br{}
  Which side of the notebook holds the tabs. @br{}
  Default value: @val[gtk:position-type]{:top}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-tab-pos)
      "Accessor"
      (documentation 'notebook-tab-pos 'function)
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-tab-pos object) => pos}
  @syntax{(setf (gtk:notebook-tab-pos object) pos)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[pos]{a value of the @sym{gtk:position-type} enumeration for the
    edge to draw the tabs at}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{tab-pos} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-tab-pos} function gets the edge at which the tabs for
  switching pages in the notebook are drawn. The
  @setf{gtk:notebook-tab-pos} function sets the edge.
  @see-class{gtk:notebook}
  @see-symbol{gtk:position-type}")

;;; ----------------------------------------------------------------------------
;;; Child Property and Child Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:notebook-child-detachable ------------------------------------------

(define-child-property notebook-child-detachable "detachable" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-detachable)
      "Accessor"
      (documentation 'notebook-child-detachable 'function)
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-child-detachable container child) => detachable}
  @syntax{(setf (gtk:notebook-child-detachable container child) detachable)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[detachable]{a boolean whether the tab is detachable}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{detachable} child property of the
    @class{gtk:notebook} class.
  @end{short}
  Whether the tab is detachable.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-child-menu-label ------------------------------------------

(define-child-property notebook-child-menu-label
                       "menu-label" "gchararray" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-menu-label)
      "Accessor"
      (documentation 'notebook-child-menu-label 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:notebook-child-menu-label container child) => label}
  @syntax{(setf (gtk:notebook-child-menu-label container child) label)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[label]{a string displayed in the menu entry of the child widget}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{menu-label} child property of the
    @class{gtk:notebook} class.
  @end{short}
  The string displayed in the menu entry of the child widget.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-child-position --------------------------------------------

(define-child-property notebook-child-position "position" "gint" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-position)
      "Accessor"
      (documentation 'notebook-child-position 'function)
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-child-position container child) => position}
  @syntax{(setf (gtk:notebook-child-position container child) position)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[position]{an integer for the index of the child widget in the
    notebook}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{position} child property of the
    @class{gtk:notebook} class.
  @end{short}
  The index of the child widget in the notebook.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-child-reorderable -----------------------------------------

(define-child-property notebook-child-reorderable
                       "reorderable" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-reorderable)
      "Accessor"
      (documentation 'notebook-child-reorderable 'function)
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-child-reorderable container child) => reorderable}
  @syntax{(setf (gtk:notebook-child-reorderable container child) reorderable)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[reorderable]{a boolean whether the tab is reorderable}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{reorderable} child property of the
    @class{gtk:notebook} class.
  @end{short}
  Whether the tab is reorderable by user action.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-child-tab-expand ------------------------------------------

(define-child-property notebook-child-tab-expand "tab-expand" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-tab-expand)
      "Accessor"
      (documentation 'notebook-child-tab-expand 'function)
 "@version{#2025-06-28}
  @syntax{(gtk:notebook-child-tab-expand container child) => expand}
  @syntax{(setf (gtk:notebook-child-tab-expand container child) expand)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[expand]{a boolean whether to expand the tab of the child widget}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{tab-expand} child property of the
    @class{gtk:notebook} class.
  @end{short}
  Whether to expand the tab of the child widget.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-child-tab-fill --------------------------------------------

(define-child-property notebook-child-tab-fill "tab-fill" "gboolean" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-tab-fill)
      "Accessor"
      (documentation 'notebook-child-tab-fill 'function)
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-child-tab-fill container child) => fill}
  @syntax{(setf (gtk:notebook-child-tab-fill container child) fill)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[fill]{a boolean whether the tab of the child widget should fill
    the allocated area}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{tab-fill} child property of the
    @class{gtk:notebook} class.
  @end{short}
  Whether the tab of the child widget should fill the allocated area.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-child-tab-level -------------------------------------------

(define-child-property notebook-child-tab-label "tab-label" "gchararray" t t t)

#+liber-documentation
(setf (liber:alias-for-function 'notebook-child-tab-label)
      "Accessor"
      (documentation 'notebook-child-tab-label 'function)
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-child-tab-label container child) => label}
  @syntax{(setf (gtk:notebook-child-tab-label container child) label)}
  @argument[container]{a @class{notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[label]{a string displayed on the tab label of the child widget}
  @begin{short}
    Accessor of the @prop[gtk:notebook]{tab-label} child property of the
    @class{gtk:notebook} class.
  @end{short}
  The string displayed on the tab label of the child page.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_new
;;; ----------------------------------------------------------------------------

(declaim (inline notebook-new))

(defun notebook-new ()
 #+liber-documentation
 "@version{#2023-03-21}
  @return{The newly created @class{gtk:notebook} widget.}
  @begin{short}
    Creates a new notebook with no pages.
  @end{short}
  @see-class{gtk:notebook}"
  (make-instance 'notebook))

(export 'notebook-new)

;;; ----------------------------------------------------------------------------
;;; gtk:notebook-add-page
;;; ----------------------------------------------------------------------------

(defun notebook-add-page (notebook child tab &key (position :end) menu)
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget to use as the content of
    the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, 'page N'}
  @argument[position]{an integer for the index starting at 0 at which to insert
    the page, or -1 to append the page after all other pages, or @code{:end} to
    append the page, @code{:start} to prepend the page, the default value is
    @code{:end}}
  @argument[menu]{a @class{gtk:widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{return}
    The integer for the index starting from 0 of the added page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Inserts a page into the notebook depending on the value of the
    @arg{position} keyword argument with the @code{:end} default value:
  @end{short}
  @begin[code]{table}
    @begin[:start]{entry}
      Prepends a page to the notebook. This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk:notebook-prepend-page}}
        @item{@fun{gtk:notebook-prepend-page-menu}}
      @end{itemize}
    @end{entry}
    @begin[:end]{entry}
      Appends a page to the notebook. This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk:notebook-append-page}}
        @item{@fun{gtk:notebook-append-page-menu}}
      @end{itemize}
    @end{entry}
    @begin[otherwise]{entry}
      Insert a page into the notebook at the given @arg{position}, which is an
      integer with the index starting from 0. This replaces the functions:
      @begin{itemize}
        @item{@fun{gtk:notebook-insert-page}}
        @item{@fun{gtk:notebook-insert-page-menu}}
      @end{itemize}
    @end{entry}
  @end{table}
  If the @arg{menu} optinal argument is @code{nil}, that is the default value,
  and the @arg{tab} argument is a @class{gtk:label} widget or @code{nil}, then
  the menu label will be a newly created label with the same text as @arg{tab}.
  If the @arg{tab} argument is not a @class{gtk:label} widget, the @arg{menu}
  argument must be specified if the page-switch menu is to be used.
  @see-class{gtk:notebook}
  @see-class{gtk:label}
  @see-function{gtk:notebook-append-page}
  @see-function{gtk:notebook-append-page-menu}
  @see-function{gtk:notebook-prepend-page}
  @see-function{gtk:notebook-prepend-page-menu}
  @see-function{gtk:notebook-insert-page}
  @see-function{gtk:notebook-insert-page-menu}"
  (assert (typep position '(or integer (member :start :end))))
  (assert (typep menu '(or null g:object (member :default))))
  (case position
    (:end
     (if menu
         (notebook-append-page-menu notebook
                                    child
                                    tab
                                    (if (eq menu :default)
                                        (cffi:null-pointer)
                                        menu))
         (notebook-append-page notebook child tab)))
    (:start
     (if menu
         (notebook-prepend-page-menu notebook
                                     child
                                     tab
                                     (if (eq menu :default)
                                         (cffi:null-pointer)
                                         menu))
         (notebook-prepend-page notebook child tab)))
    (otherwise
     (if menu
         (notebook-insert-page-menu notebook
                                    child
                                    tab
                                    (if (eq menu :default)
                                        (cffi:null-pointer)
                                        menu)
                                    position)
         (notebook-insert-page notebook
                               child
                               tab
                               position)))))

(export 'notebook-add-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_append_page" notebook-append-page) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{the @class{gtk:widget} child to use as the content of the
    page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, \"page N\"}
  @begin{return}
    The integer for the index starting from 0 of the appended page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Appends a page to the notebook.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-add-page}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget)))

(export 'notebook-append-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page_menu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_append_page_menu" notebook-append-page-menu) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child to use as the content of the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, \"page N\"}
  @argument[menu]{a @class{gtk:widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{return}
    The integer for the index starting from 0 of the appended page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Appends a page to the notebook, specifying the widget to use as the label
    in the popup menu.
  @end{short}

  If the @arg{menu} optional argument is @code{nil}, and @arg{tab} is a
  @class{gtk:label} widget or @code{nil}, then the menu label will be a newly
  created label with the same text as @arg{tab}. If the @arg{tab} argument is
  not a @class{gtk:label} widget, the @arg{menu} argument must be specified if
  the page-switch menu is to be used.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-class{gtk:label}
  @see-function{gtk:notebook-add-page}"
  (notebook (g:object widget))
  (child (g:object widget))
  (tab (g:object widget))
  (menu (g:object widget)))

(export 'notebook-append-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_prepend_page" notebook-prepend-page) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child to use as the content of the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, 'page N'}
  @begin{return}
    The integer for the index starting from 0 of the prepended page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Prepends a page to the notebook.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-add-page}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget)))

(export 'notebook-prepend-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page_menu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_prepend_page_menu" notebook-prepend-page-menu) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child to use as the content of the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, 'page N'}
  @argument[menu]{a @class{gtk:widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{return}
    The integer for the index starting from 0 of the prepended page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Prepends a page to the notebook, specifying the widget to use as the label
    in the popup menu.
  @end{short}

  If the @arg{menu} optional argument is @code{nil}, and @arg{tab} is a
  @class{gtk:label} widget or @code{nil}, then the menu label will be a newly
  created label with the same text as @arg{tab}. If the @arg{tab} argument is
  not a @class{gtk:label} widget, the @arg{menu} argument must be specified if
  the page-switch menu is to be used.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-class{gtk:label}
  @see-function{gtk:notebook-add-page}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget))
  (menu (g:object widget)))

(export 'notebook-prepend-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_insert_page" notebook-insert-page) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child to use as the content of the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, 'page N'}
  @argument[position]{an integer for the index starting at 0 at which to
    insert the page, or -1 to append the page after all other pages}
  @begin{return}
    The integer for the index starting from 0 of the inserted page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Insert a page into the notebook at the given position.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-add-page}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget))
  (position :int))

(export 'notebook-insert-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page_menu
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_insert_page_menu" notebook-insert-page-menu) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child to use as the content of the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, 'page N'}
  @argument[menu]{a @class{gtk:widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @argument[position]{an integer for the index starting at 0 at which to
    insert the page, or -1 to append the page after all other pages}
  @begin{return}
    The integer for the index starting from 0 of the inserted page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Insert a page into the notebook at the given position, specifying the
    widget to use as the label in the popup menu.
  @end{short}

  If the @arg{menu} optional argument is @code{nil}, and @arg{tab} is a
  @class{gtk:label} widget or @code{nil}, then the menu label will be a newly
  created label with the same text as @arg{tab}. If the @arg{tab} argument is
  not a @class{gtk:label} widget, the @arg{menu} argument must be specified if
  the page-switch menu is to be used.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-class{gtk:label}
  @see-function{gtk:notebook-add-page}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget))
  (menu (g:object widget))
  (position :int))

(export 'notebook-insert-page-menu)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_remove_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_remove_page" %notebook-remove-page) :void
  (notebook (g:object notebook))
  (num :int))

(defun notebook-remove-page (notebook page-or-number)
 #+liber-documentation
 "@version{#2025-06-16}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[page-or-number]{an integer for the index of a notebook page,
    starting from 0, if -1, the last page will be removed, or the
    @class{gtk:widget} child page}
  @begin{short}
    Removes a page from the notebook given the page widget or its index in the
    notebook.
  @end{short}
  @begin[Notes]{dictionary}
    In the Lisp implementation the argument can be an integer for the index or
    the page widget. The index of the page widget is got with the
    @fun{gtk:notebook-page-num} function and passed to the C function.
  @end{dictionary}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar notebook (make-instance 'gtk:notebook))
=> NOTEBOOK
(defvar page (make-instance 'gtk:frame))
=> PAGE
(gtk:notebook-append-page notebook page nil)
=> 0
(gtk:notebook-remove-page notebook page)
(gtk:notebook-append-page notebook page nil)
=> 0
(gtk:notebook-remove-page notebook 0)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-page-num}"
  (%notebook-remove-page notebook
                         (etypecase page-or-number
                           (integer page-or-number)
                           (widget
                             (notebook-page-num notebook
                                                page-or-number)))))

(export 'notebook-remove-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_detach_tab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_detach_tab" notebook-detach-tab) :void
 #+liber-documentation
 "@version{#2023-03-21}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child}
  @begin{short}
    Removes the child page from the notebook.
  @end{short}
  This function is very similar to the @fun{gtk:container-remove} function, but
  additionally informs the notebook that the removal is happening as part of a
  tab drag and drop operation, which should not be cancelled.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:container-remove}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-detach-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_page_num
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_page_num" notebook-page-num) :int
 #+liber-documentation
 "@version{#2023-03-21}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child}
  @begin{return}
    The index of the page containing child, or -1 if child is not in the
    notebook.
  @end{return}
  @begin{short}
    Finds the index of the page which contains the given child widget.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-page-num)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_next_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_next_page" notebook-next-page) :void
 #+liber-documentation
 "@version{#2023-03-21}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Switches to the next page.
  @end{short}
  Nothing happens if the current page is the last page.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-prev-page}"
  (notebook (g:object notebook)))

(export 'notebook-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prev_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_prev_page" notebook-prev-page) :void
 #+liber-documentation
 "@version{#2023-03-21}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Switches to the previous page.
  @end{short}
  Nothing happens if the current page is the first page.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-next-page}"
  (notebook (g:object notebook)))

(export 'notebook-prev-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_reorder_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_reorder_child" notebook-reorder-child) :void
 #+liber-documentation
 "@version{#2025-06-16}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page to move}
  @argument[position]{an integer for the position, or -1 to move to the end}
  @begin{short}
    Reorders the page containing the child, so that it appears in the given
    position.
  @end{short}
  If the position is greater than or equal to the number of children in the
  list or negative, the child will be moved to the end of the list.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (position :int))

(export 'notebook-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_enable
;;; ----------------------------------------------------------------------------

(declaim (inline notebook-popup-enable))

(defun notebook-popup-enable (notebook)
 #+liber-documentation
 "@version{#2023-03-21}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Enables the popup menu.
  @end{short}
  If the user clicks with the right mouse button on the tab labels, a menu with
  all the pages will be popped up.
  @begin[Notes]{dictionary}
    This function calls the @fun{gtk:notebook-enable-popup} function with the
    @em{true} value.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-popup-disable}
  @see-function{gtk:notebook-enable-popup}"
  (setf (notebook-enable-popup notebook) t))

(export 'notebook-popup-enable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_disable ()
;;; ----------------------------------------------------------------------------

(declaim (inline notebook-popup-disable))

(defun notebook-popup-disable (notebook)
 #+liber-documentation
 "@version{#2023-03-21}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Disables the popup menu.
  @end{short}
  See the @fun{gtk:notebook-popup-enable} function.
  @begin[Notes]{dictionary}
    This function calls the @fun{gtk:notebook-enable-popup} function with the
    @em{false} value.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-popup-enable}
  @see-function{gtk:notebook-enable-popup}"
  (setf (notebook-enable-popup notebook) nil))

(export 'notebook-popup-disable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_current_page
;;; gtk_notebook_set_current_page
;;; ----------------------------------------------------------------------------

(defun (setf notebook-current-page) (num notebook)
  (cffi:foreign-funcall "gtk_notebook_set_current_page"
                        (g:object notebook) notebook
                        :int num
                        :void)
  num)

(cffi:defcfun ("gtk_notebook_get_current_page" notebook-current-page) :int
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-current-page notebook) => num}
  @syntax{(setf (gtk:notebook-current-page notebook) num)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[num]{an integer for the index of the page to switch to, starting
    from 0, if negative, the last page will be used, if greater than the number
    of pages in the notebook, nothing will be done}
  @begin{short}
    The @fun{gtk:notebook-current-page} function returns an integer with the
    index starting from 0 of the page number of the current page.
  @end{short}
  The @setf{gtk:notebook-current-page} function switches to the given page
  number.

  Note that due to historical reasons, the @class{gtk:notebook} widget refuses
  to switch to a page unless the child widget is visible. Therefore, it is
  recommended to show child widgets before adding them to a notebook.
  @see-class{gtk:notebook}"
  (notebook (g:object notebook)))

(export 'notebook-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label
;;; gtk_notebook_set_menu_label
;;; ----------------------------------------------------------------------------

(defun (setf notebook-menu-label) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_menu_label"
                        (g:object notebook) notebook
                        (g:object widget) child
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_menu_label" notebook-menu-label)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-menu-label notebook child) => menu}
  @syntax{(setf (gtk:notebook-menu-label notebook child) menu)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child contained in a page of the
    notebook}
  @argument[menu]{a @class{gtk:widget} menu label, or @code{nil} for default}
  @begin{short}
    The @fun{gtk:notebook-menu-label} function returns the menu label, or
    @code{nil} if the notebook page does not have a menu label other than the
    default tab label.
  @end{short}
  The @setf{gtk:notebook-menu-label} function changes the menu label for the
  page containing the child.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-menu-label-text}
  @see-function{gtk:notebook-child-menu-label}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_nth_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook" notebook-nth-page) (g:object widget)
 #+liber-documentation
 "@version{#2025-06-16}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[num]{an integer for the index of a page in the notebook, or -1 to
    get the last page}
  @begin{return}
    The @class{gtk:widget} child page, or @code{nil} if @arg{num} is out of
    bounds.
  @end{return}
  @begin{short}
    Returns the child widget contained in page number @arg{num}.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (num :int))

(export 'notebook-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_n_pages
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_get_n_pages" notebook-n-pages) :int
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @return{The integer for the number of pages in the notebook.}
  @short{Gets the number of pages in a notebook.}
  @see-class{gtk:notebook}"
  (notebook (g:object notebook)))

(export 'notebook-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label
;;; gtk_notebook_set_tab_label
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-label) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_tab_label"
                        (g:object notebook) notebook
                        (g:object widget) child
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_tab_label" notebook-tab-label)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-tab-label notebook child) => tab}
  @syntax{(setf (gtk:notebook-tab-label notebook child) tab)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @argument[tab]{a @class{gtk:widget} tab label to use, or @code{nil} for
    default tab label}
  @begin{short}
    The @fun{gtk:notebook-tab-label} function returns the tab label widget for
    the page child.
  @end{short}
  The @code{nil} value is returned if the child is not in the notebook or if no
  tab label has been set for the child. The @setf{gtk:notebook-tab-label}
  function changes the tab label for the child page. If the @code{nil} value is
  specified for @arg{tab}, then the page will have the label 'page N'.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-tab-label-text}
  @see-function{gtk:notebook-child-tab-label}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label_text
;;; gtk_notebook_set_menu_label_text
;;; ----------------------------------------------------------------------------

(defun (setf notebook-menu-label-text) (value notebook child)
  (setf (notebook-child-menu-label notebook child) value))

(defun notebook-menu-label-text (notebook child)
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-menu-label-text notebook child) => text}
  @syntax{(setf (gtk:notebook-menu-label-text notebook child) text)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child of a page of the notebook}
  @argument[text]{a string for the label text}
  @begin{short}
    The @fun{gtk:notebook-menu-label-text} function retrieves the text of the
    menu label for the page containing child.
  @end{short}
  The @setf{gtk:notebook-menu-label-text} function creates a new label and sets
  it as the menu label of the child page.
  @begin[Notes]{dictionary}
    This function is implemented with the @fun{gtk:notebook-child-menu-label}
    function.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-menu-label}
  @see-function{gtk:notebook-child-menu-label}"
  (notebook-child-menu-label notebook child))

(export 'notebook-menu-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label_text
;;; gtk_notebook_set_tab_label_text
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-label-text) (value notebook child)
  (setf (notebook-child-tab-label notebook child) value))

(defun notebook-tab-label-text (notebook child)
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-tab-label-text notebook child) => text}
  @syntax{(setf (gtk:notebook-tab-label-text notebook child) text)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child contained in a page of the
    notebook}
  @argument[text]{a string for the label text}
  @begin{short}
    The @fun{gtk:notebook-tab-label-text} function retrieves the text of the
    tab label for the page containing child.
  @end{short}
  The @setf{gtk:notebook-tab-label-text} function creates a new label and sets
  it as the tab label for the page containing child.
  @begin[Notes]{dictionary}
    This function is implemented with the @fun{gtk:notebook-child-tab-label}
    function.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-tab-label}
  @see-function{gtk:notebook-child-tab-label}"
  (notebook-child-tab-label notebook child))

(export 'notebook-tab-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_reorderable
;;; gtk_notebook_get_tab_reorderable
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-reorderable) (reorderable notebook child)
  (setf (notebook-child-reorderable notebook child) reorderable))

(defun notebook-tab-reorderable (notebook child)
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-tab-reorderable notebook child) => reorderable}
  @syntax{(setf (gtk:notebook-tab-reorderable notebook child) reorderable)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @argument[reorderable]{a boolean whether the tab is reorderable or not}
  @begin{short}
    The @fun{gtk:notebook-tab-reorderable} function gets whether the tab can be
    reordered via drag and drop or not.
  @end{short}
  The @setf{gtk:notebook-tab-reorderable} function sets whether the notebook
  tab can be reordered.
  @begin[Notes]{dictionary}
    This function duplicates the implementation of the
    @fun{gtk:notebook-child-reorderable} function.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-child-reorderable}"
  (notebook-child-reorderable notebook child))

(export 'notebook-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_detachable
;;; gtk_notebook_set_tab_detachable
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-detachable) (value notebook child)
  (setf (notebook-child-detachable notebook child) value))

(defun notebook-tab-detachable (notebook child)
 #+liber-documentation
 "@version{#2025-06-16}
  @syntax{(gtk:notebook-tab-detachable notebook child) => detachable}
  @syntax{(setf (gtk:notebook-tab-detachable notebook child) detachable)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @argument[detachable]{a boolean whether the tab is detachable or not}
  @begin{short}
    The @fun{gtk:notebook-tab-detachable} function returns whether the tab
    content can be detached from the notebook to another notebook or widget.
  @end{short}
  The @setf{gtk:notebook-tab-detachable} function sets whether the tab can be
  detached.

  Note that two notebooks must share a common group identificator, see the
  @fun{gtk:notebook-group-name} function, to allow automatic tabs interchange
  between them.
  @begin[Examples]{dictionary}
    If you want a widget to interact with a notebook through DnD, that is accept
    dragged tabs from it, it must be set as a drop destination and accept the
    \"GTK_NOTEBOOK_TAB\" target. The notebook will fill the selection with a
    GtkWidget** pointing to the child widget that corresponds to the dropped
    tab.
    @begin{pre}
static void
on_drop_zone_drag_data_received (GtkWidget        *widget,
                                 GdkDragContext   *context,
                                 gint              x,
                                 gint              y,
                                 GtkSelectionData *selection_data,
                                 guint             info,
                                 guint             time,
                                 gpointer          user_data)
{
  GtkWidget *notebook;
  GtkWidget **child;

  notebook = gtk_drag_get_source_widget (context);
  child = (void*) gtk_selection_data_get_data (selection_data);

  process_widget (*child);
  gtk_container_remove (GTK_CONTAINER (notebook), *child);
@}
    @end{pre}
    If you want a notebook to accept drags from other widgets, you will have to
    set your own DnD code to do it.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-child-detachable}
  @see-function{gtk:notebook-group-name}"
  (notebook-child-detachable notebook child))

(export 'notebook-tab-detachable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_hborder                            deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_vborder                            deprecated
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_action_widget
;;; gtk_notebook_set_action_widget
;;; ----------------------------------------------------------------------------

(defun (setf notebook-action-widget) (value notebook packtype)
  (cffi:foreign-funcall "gtk_notebook_set_action_widget"
                        (g:object notebook) notebook
                        pack-type packtype
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_action_widget" notebook-action-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-06-27}
  @syntax{(gtk:notebook-action-widget notebook pack-type) => widget}
  @syntax{(setf (gtk:notebook-action-widget notebook pack-type) widget)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[packtype]{a value of the @sym{gtk:pack-type} enumeration for
    the action}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    The @fun{gtk:notebook-action-widget} function gets one of the action
    widgets.
  @end{short}
  The @setf{gtk:notebook-action-widget} function sets the widget as one of the
  action widgets. Depending on the pack type the widget will be placed before
  or after the tabs. You can use a @class{gtk:box} widget if you need to pack
  more than one widget on the same side.

  Note that action widgets are \"internal\" children of the notebook and thus
  not included in the list returned from the @fun{gtk:container-foreach}
  function.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-class{gtk:box}
  @see-symbol{gtk:pack-type}
  @see-function{gtk:container-foreach}"
  (notebook (g:object notebook))
  (packtype pack-type))

(export 'notebook-action-widget)

;;; --- End of file gtk3.notebook.lisp -----------------------------------------
