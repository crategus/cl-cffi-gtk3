;;; ----------------------------------------------------------------------------
;;; gtk3.combo-box.lisp
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
;;; GtkComboBox
;;;
;;;     A widget used to choose from a list of items
;;;
;;; Types and Values
;;;
;;;     GtkComboBox
;;;
;;; Functions
;;;
;;;     gtk_combo_box_new
;;;     gtk_combo_box_new_with_entry
;;;     gtk_combo_box_new_with_model
;;;     gtk_combo_box_new_with_model_and_entry
;;;     gtk_combo_box_new_with_area
;;;     gtk_combo_box_new_with_area_and_entry
;;;     gtk_combo_box_get_wrap_width                       Accessor
;;;     gtk_combo_box_set_wrap_width                       Accessor
;;;     gtk_combo_box_get_row_span_column                  Accessor
;;;     gtk_combo_box_set_row_span_column                  Accessor
;;;     gtk_combo_box_get_column_span_column               Accessor
;;;     gtk_combo_box_set_column_span_column               Accessor
;;;     gtk_combo_box_get_active                           Accessor
;;;     gtk_combo_box_set_active                           Accessor
;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_get_id_column                        Accessor
;;;     gtk_combo_box_set_id_column                        Accessor
;;;     gtk_combo_box_get_active_id                        Accessor
;;;     gtk_combo_box_set_active_id                        Accessor
;;;     gtk_combo_box_get_model                            Accessor
;;;     gtk_combo_box_set_model                            Accessor
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_popup_accessible
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func
;;;     gtk_combo_box_set_add_tearoffs                     Accessor
;;;     gtk_combo_box_get_add_tearoffs                     Accessor
;;;     gtk_combo_box_set_title                            deprecated
;;;     gtk_combo_box_get_title                            deprecated
;;;     gtk_combo_box_set_focus_on_click                   Accessor
;;;     gtk_combo_box_get_focus_on_click                   Accessor
;;;     gtk_combo_box_set_button_sensitivity               Accessor
;;;     gtk_combo_box_get_button_sensitivity               Accessor
;;;     gtk_combo_box_get_has_entry                        Accessor
;;;     gtk_combo_box_set_entry_text_column                Accessor
;;;     gtk_combo_box_get_entry_text_column                Accessor
;;;     gtk_combo_box_set_popup_fixed_width                Accessor
;;;     gtk_combo_box_get_popup_fixed_width                Accessor
;;;
;;; Properties
;;;
;;;     active
;;;     active-id
;;;     add-tearoffs
;;;     button-sensitivity
;;;     cell-area
;;;     column-span-column
;;;     entry-text-column
;;;     focus-on-click
;;;     has-entry
;;;     has-frame
;;;     id-column
;;;     model
;;;     popup-fixed-width
;;;     popup-shown
;;;     row-span-column
;;;     tearoff-title
;;;     wrap-width
;;;
;;; Style Properties
;;;
;;;     appears-as-list
;;;     arrow-scaling
;;;     arrow-size
;;;     shadow-type
;;;
;;; Signals
;;;
;;;     changed
;;;     format-entry-text
;;;     move-active
;;;     popdown
;;;     popup
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkBin
;;;                     ╰── GtkComboBox
;;;                         ├── GtkAppChooserButton
;;;                         ╰── GtkComboBoxText
;;;
;;; Implemented Interfaces
;;;
;;;     GtkComboBox implements AtkImplementorIface, GtkBuildable, GtkCellLayout
;;;     and GtkCellEditable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBox
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkComboBox" combo-box
  (:superclass bin
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkCellEditable"
                "GtkCellLayout")
   :type-initializer "gtk_combo_box_get_type")
  ((active
    combo-box-active
    "active" "gint" t t)
   (active-id
    combo-box-active-id
    "active-id" "gchararray" t t)
   (add-tearoffs
    combo-box-add-tearoffs
    "add-tearoffs" "gboolean" t t)
   (button-sensitivity
    combo-box-button-sensitivity
    "button-sensitivity" "GtkSensitivityType" t t)
   (cell-area
    combo-box-cell-area
    "cell-area" "GtkCellArea" t t)
   (column-span-column
    combo-box-column-span-column
    "column-span-column" "gint" t t)
   (entry-text-column
    combo-box-entry-text-column
    "entry-text-column" "gint" t t)
   (focus-on-click
    combo-box-focus-on-click
    "focus-on-click" "gboolean" t t)
   (has-entry
    combo-box-has-entry
    "has-entry" "gboolean" t nil)
   (has-frame
    combo-box-has-frame
    "has-frame" "gboolean" t t)
   (id-column
    combo-box-id-column
    "id-column" "gint" t t)
   (model
    combo-box-model
    "model" "GtkTreeModel" t t)
   (popup-fixed-width
    combo-box-popup-fixed-width
    "popup-fixed-width" "gboolean" t t)
   (popup-shown
    combo-box-popup-shown
    "popup-shown" "gboolean" t nil)
   (row-span-column
    combo-box-row-span-column
    "row-span-column" "gint" t t)
   (tearoff-title
    combo-box-tearoff-title
    "tearoff-title" "gchararray" t t)
   (wrap-width
    combo-box-wrap-width
    "wrap-width" "gint" t t)))

#+liber-documentation
(setf (documentation 'combo-box 'type)
 "@version{#2023-3-17}
  @begin{short}
    The @class{gtk:combo-box} widget allows the user to choose from a list of
    valid choices.
  @end{short}
  The @class{gtk:combo-box} widget displays the selected choice. When activated,
  the @class{gtk:combo-box} widget displays a popup which allows the user to
  make a new choice. The style in which the selected value is displayed, and the
  style of the popup is determined by the current theme. It may be similar to a
  Windows style combo box.

  The @class{gtk:combo-box} widget uses the model-view pattern. The list of
  valid choices is specified in the form of a tree model, and the display of the
  choices can be adapted to the data in the model by using cell renderers, as
  you would in a tree view. This is possible since the @class{gtk:combo-box}
  class implements the @class{gtk:cell-layout} interface. The tree model holding
  the valid choices is not restricted to a flat list, it can be a real tree, and
  the popup will reflect the tree structure.

  To allow the user to enter values not in the model, the @code{has-entry}
  property allows the @class{gtk:combo-box} widget to contain a
  @class{gtk:entry} widget. This entry can be accessed by calling the
  @fun{gtk:bin-child} function on the combo box.

  For a simple list of textual choices, the model-view API of the
  @class{gtk:combo-box} widget can be a bit overwhelming. In this case, the
  @class{gtk:combo-box-text} widget offers a simple alternative. Both the
  @class{gtk:combo-box} widget and the @class{gtk:combo-box-text} widget can
  contain an entry.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
combobox
├── box.linked
│   ╰── button.combo
│       ╰── box
│           ├── cellview
│           ╰── arrow
╰── window.popup
    @end{pre}
    A normal @code{combobox} contains a @code{box} with the @code{.linked}
    class, a @code{button} with the @code{.combo} class and inside those
    buttons, there are a @code{cellview} and an @code{arrow}.
    @begin{pre}
combobox
├── box.linked
│   ├── entry.combo
│   ╰── button.combo
│       ╰── box
│           ╰── arrow
╰── window.popup
    @end{pre}
    A @class{gtk:combo-box} widget with an entry has a single CSS node with name
    @code{combobox}. It contains a @code{box} with the @code{.linked} class.
    That box contains an @code{entry} and a @code{button}, both with the
    @code{.combo} class added. The button also contains another node with name
    @code{arrow}.
  @end{dictionary}
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[appears-as-list]{entry}
        The @code{appears-as-list} style property of type @code{:boolean}
        (Read) @br{}
        Whether dropdowns should look like lists rather than menus. @br{}
        Default value: @em{false}
      @end{entry}
      @begin[arrow-scaling]{entry}
        The @code{arrow-scaling} style property of @code{:float} (Read) @br{}
        Sets the amount of space used up by the combo box arrow, proportional
        to the font size. @br{}
        @em{Warning:} The @code{arrow-scaling} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use the standard min-width/min-height CSS properties on the arrow
        node. The value of this style property is ignored. @br{}
        Allowed values: [0.0,2.0] @br{}
        Default value: 1.0
      @end{entry}
      @begin[arrow-size]{entry}
        The @code{arrow-size} style property of type @code{:int} (Read) @br{}
        Sets the minimum size of the arrow in the combo box. Note that the arrow
        size is coupled to the font size, so in case a larger font is used, the
        arrow will be larger than set by arrow size. @br{}
        @em{Warning:} The @code{arrow-size} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use
        the standard min-width/min-height CSS properties on the arrow node. The
        value of this style property is ignored. @br{}
        Allowed values: >= 0 @br{}
        Default value: 15
      @end{entry}
      @begin[shadow-type]{entry}
        The @code{shadow-type} style property of type @symbol{gtk:shadow-type}
        (Read) @br{}
        Which kind of shadow to draw around the combo box. @br{}
        @em{Warning:} The @code{shadow-type} style property has been deprecated
        since version 3.20 and should not be used in newly written code. Use
        CSS styling to change the appearance of the combobox frame. The value
        of this style property is ignored. @br{}
        Default value: @code{:none}
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (combo)    :run-last
      @end{pre}
      The signal is emitted when the active item is changed. The can be due to
      the user selecting a different item from the list, or due to a call to
      the @fun{gtk:combo-box-active-iter} function. It will also be emitted
      while typing into the entry of a combo box with an entry.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
    @subheading{The \"format-entry-text\" signal}
      @begin{pre}
lambda (combo pathstr)    :run-last
      @end{pre}
      A signal which allows you to change how the text displayed in a combo
      box's entry is displayed. Connect a signal handler which returns an
      allocated string representing @arg{path}. That string will then be used
      to set the text in the combo box's entry. The default signal handler uses
      the text from the @code{entry-text-column} property model column. Here is
      an example signal handler which fetches data from the model and displays
      it in the entry. For combo boxes that are created with an entry. See the
      @code{has-entry} property.
      @begin{pre}
(defun format-entry-text-callback (combo pathstr)
  (let* ((model (gtk:combo-box-model combo))
         (iter (gtk:tree-model-iter-from-string model pathstr))
         (value (gtk:tree-model-value model iter col-value)))
    (format nil \"~a\" value)))
      @end{pre}
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
        @entry[pathstr]{A string representing the @class{gtk:tree-path} instance
          from the combo box's current model to format text for.}
        @entry[Returns]{A string representing the value at @argp{pathstr} for
          the current @class{gtk:combo-box} model.}
      @end{table}
    @subheading{The \"move-active\" signal}
      @begin{pre}
lambda (combo scroll)    :action
      @end{pre}
      A keybinding signal which gets emitted to move the active selection.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
        @entry[scroll]{A value of the @symbol{gtk:scroll-type} enumeration.}
      @end{table}
    @subheading{The \"popdown\" signal}
      @begin{pre}
lambda (combo)    :action
      @end{pre}
      A keybinding signal which gets emitted to popdown the combo box list. The
      default bindings for this signal are the @kbd{Alt+Up} and @kbd{Escape}
      keys.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
    @subheading{The \"popup\" signal}
      @begin{pre}
lambda (combo)    :action
      @end{pre}
      A keybinding signal which gets emitted to popup the combo box list. The
      default binding for this signal is the @kbd{Alt+Down} key.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:combo-box-new}
  @see-constructor{gtk:combo-box-new-with-entry}
  @see-constructor{gtk:combo-box-new-with-model}
  @see-constructor{gtk:combo-box-new-with-model-and-entry}
  @see-constructor{gtk:combo-box-new-with-area}
  @see-constructor{gtk:combo-box-new-with-area-and-entry}
  @see-slot{gtk:combo-box-active}
  @see-slot{gtk:combo-box-active-id}
  @see-slot{gtk:combo-box-add-tearoffs}
  @see-slot{gtk:combo-box-button-sensitivity}
  @see-slot{gtk:combo-box-cell-area}
  @see-slot{gtk:combo-box-column-span-column}
  @see-slot{gtk:combo-box-entry-text-column}
  @see-slot{gtk:combo-box-focus-on-click}
  @see-slot{gtk:combo-box-has-entry}
  @see-slot{gtk:combo-box-has-frame}
  @see-slot{gtk:combo-box-id-column}
  @see-slot{gtk:combo-box-model}
  @see-slot{gtk:combo-box-popup-fixed-width}
  @see-slot{gtk:combo-box-popup-shown}
  @see-slot{gtk:combo-box-row-span-column}
  @see-slot{gtk:combo-box-tearoff-title}
  @see-slot{gtk:combo-box-wrap-width}
  @see-class{gtk:tree-model}
  @see-class{gtk:combo-box-text}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:combo-box-active ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'combo-box) t)
 "The @code{active} property of type @code{:int} (Read / Write) @br{}
  The item which is currently active. If the model is a non-flat treemodel,
  and the active item is not an immediate child of the root of the tree, this
  property has the value @code{(first (gtk:tree-path-indices path))}, where
  @arg{path} is the @class{gtk:tree-path} instance of the active item. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-active)
      "Accessor"
      (documentation 'combo-box-active 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-active object) => index}
  @syntax{(setf (gtk:combo-box-active object) index)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[index]{an integer with the index in the model passed during
    construction, or -1 to have no active item}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{active} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-active} function returns the index of the currently
  active item, or -1 if there is no active item. The
  @setf{gtk:combo-box-active} function sets the active item.

  If the model is a non-flat tree model, and the active item is not an
  immediate child of the root of the tree, this function returns
  @code{(first (gtk:tree-path-indices path))}, where @code{path} is the
  @class{gtk:tree-path} instance of the active item.
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-path}")

;;; --- gtk:combo-box-active-id ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active-id" 'combo-box) t)
 "The @code{active-id} property of type @code{:string} (Read / Write) @br{}
  The value of the ID column of the active row. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-active-id)
      "Accessor"
      (documentation 'combo-box-active-id 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-active-id object) => active-id}
  @syntax{(setf (gtk:combo-box-active-id object) active-id)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[active-id]{a string with the ID of the row to select, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{active-id} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-active-id} function returns the ID of the active row
  of the combo box. This value is taken from the active row and the column
  specified by the @slot[gtk:combo-box]{id-column} property of the
  combo box. The @setf{gtk:combo-box-active-id} function changes the active row
  of the combo box to the one that has an ID equal to @arg{active-id}, or unsets
  the active row if @arg{active-id} is @code{nil}. Rows having a @code{nil} ID
  string cannot be made active by this function.

  If the @code{id-column} property of @arg{combo-box} is unset or if no row
  has the given ID then the function does nothing and returns @code{nil}.
  @see-class{gtk:combo-box}
  @see-function{gtk:combo-box-id-column}")

;;; --- gtk:combo-box-add-tearoffs ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "add-tearoffs" 'combo-box) t)
 "The @code{add-tearoffs} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether generated menus have tearoff menu items. Note that this only
  affects menu style combo boxes. @br{}
  @em{Warning:} The @code{add-tearoffs} property has been deprecated since
  version 3.10 and should not be used in newly written code. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-add-tearoffs)
      "Accessor"
      (documentation 'combo-box-add-tearoffs 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-add-tearoffs object) => add-tearoffs}
  @syntax{(setf (gtk:combo-box-add-tearoffs object) add-tearoffs)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[add-tearoffs]{@em{true} to add tearoff menu items}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{add-tearoffs} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-add-tearoffs} function gets whether the popup menu
  should have a tearoff menu item. The @setf{gtk:combo-box-add-tearoffs}
  function sets the property.
  @begin[Warning]{dictionary}
    The @fun{gtk:combo-box-add-tearoffs} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-button-sensitivity ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "button-sensitivity"
                                               'combo-box) t)
 "The @code{button-sensitivity} property of type @symbol{gtk:sensitivity-type}
  (Read / Write) @br{}
  Whether the dropdown button is sensitive when the model is empty. @br{}
  Default value: @code{:auto}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-button-sensitivity)
      "Accessor"
      (documentation 'combo-box-button-sensitivity 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-button-sensitivity object) => sensitivity}
  @syntax{(setf (gtk:combo-box-button-sensitivity object) sensitivity)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[sensitivity]{a value of the @symbol{gtk:sensitivity-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{button-sensitivity} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-button-sensitivity} function returns whether the combo
  box sets the dropdown button sensitive or not when there are no items in the
  model. The @setf{gtk:combo-box-button-sensitivity} function sets the
  sensitivity.

  @code{:on} if the dropdown button is sensitive when the model is empty,
  @code{:off} if the button is always insensitive or @code{:auto} if it
  is only sensitive as long as the model has one item to be selected.
  @see-class{gtk:combo-box}
  @see-symbol{gtk:sensitivity-type}")

;;; --- gtk:combo-box-cell-area ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-area" 'combo-box) t)
 "The @code{cell-area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers for this combo box. If no cell
  area is specified when creating the combo box with the
  @fun{gtk:combo-box-new-with-area} function a horizontally oriented
  @class{gtk:cell-area-box} object will be used.")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-cell-area)
      "Accessor"
      (documentation 'combo-box-cell-area 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-button-cell-area object) => area}
  @syntax{(setf (gtk:combo-box-cell-area object) area)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[area]{a @class{gtk:cell-area} object used to layout cell renderes}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{cell-area} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The cell area used to layout cell renderers for this combo box. If no area is
  specified when creating the combo box with the
  @fun{gtk:combo-box-new-with-area} function a horizontally oriented
  @class{gtk:cell-area-box} object will be used.
  @see-class{gtk:combo-box}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-box}
  @see-function{gtk:combo-box-new-with-area}")

;;; --- gtk:combo-box-column-span-column ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-span-column"
                                               'combo-box) t)
 "The @code{column-span-column} property of type @code{:int} (Read / Write)
  @br{}
  If this is set to a non-negative value, it must be the index of a column of
  type \"gint\" in the model. The value in that column for each item will
  determine how many columns that item will span in the popup. Therefore, values
  in this column must be greater than zero, and the sum of an item’s column
  position + span should not exceed @code{wrap-width}. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-column-span-column)
      "Accessor"
      (documentation 'combo-box-column-span-column 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-column-span-column object) => column-span}
  @syntax{(setf (gtk:combo-box-column-span-column object) column-span)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[column-span]{an integer with a column in the model passed during
    construction}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{column-span-column} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-column-span-column} function returns the column with
  column span information. The @setf{gtk:combo-box-column-span-column} function
  sets the column with column span information for the combo box to be
  @arg{column-span}. The column span column contains integers which indicate how
  many columns an item should span.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-entry-text-column ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "entry-text-column"
                                               'combo-box) t)
 "The @code{entry-text-column} property of type @code{:int} (Read / Write) @br{}
  The column in the combo box's model to associate with strings from the entry
  if the combo was created with the value @em{true} for the @code{has-entry}
  property. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-entry-text-column)
      "Accessor"
      (documentation 'combo-box-entry-text-column 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-entry-text-column object) => text-column}
  @syntax{(setf (gtk:combo-box-entry-text-column object) text-column)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[text-column]{an integer with a column in model to get the strings
    from for the internal entry}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{entry-text-column} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-entry-text-column} function returns the column which
  the combo box is using to get the strings from to display in the internal
  entry. The @setf{gtk:combo-box-entry-text-column} function sets the model
  column which the combo box should use to get strings.

  The column @arg{text-column} in the model of the combo box must be of type
  @code{gchararray}. This is only relevant if the combo box has been created
  with the @code{has-entry} property as @em{true}.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-focus-on-click -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-on-click" 'combo-box) t)
 "The @code{focus-on-click} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the combo box grabs focus when it is clicked with the mouse. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-focus-on-click)
      "Accessor"
      (documentation 'combo-box-focus-on-click 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-focus-on-click object) => focus-on-click}
  @syntax{(setf (gtk:combo-box-focus-on-click object) focus-on-click)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[focus-on-click]{a boolean whether the combo box grabs focus when
    clicked with the mouse}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{focus-on-click} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-focus-on-click} function returns whether the combo box
  grabs focus when it is clicked with the mouse. The
  @setf{gtk:combo-box-focus-on-click} function sets whether the combo box will
  grab focus.

  Making mouse clicks not grab focus is useful in places like toolbars
  where you do not want the keyboard focus removed from the main area of the
  application.
  @begin[Warning]{dictionary}
    The @fun{gtk:combo-box-focus-on-click} function has been deprecated since
    version 3.20 and should not be used in newly written code. Use the
    @fun{gtk:widget-focus-on-click} function instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-function{gtk:widget-focus-on-click}")

;;; --- gtk:combo-box-has-entry ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-entry" 'combo-box) t)
 "The @code{has-entry} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the combo box has an entry. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-has-entry)
      "Accessor"
      (documentation 'combo-box-has-entry 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-has-entry object) => has-entry}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[has-entry]{a boolean whether the combo box has an entry}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{has-entry} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-has-entry} function returns whether the combo box has
  an entry.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-has-frame ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'combo-box) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether a frame is drawn around the entry. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-has-frame)
      "Accessor"
      (documentation 'combo-box-has-frame 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-has-frame object) => has-frame}
  @syntax{(setf (gtk:combo-box-has-frame object) has-frame)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[has-frame]{a boolean whether a frame is drawn around the entry}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{has-frame} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  Controls whether a frame is drawn around the entry.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-id-column ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "id-column" 'combo-box) t)
 "The @code{id-column} property of type @code{:int} (Read / Write) @br{}
  The column in the combo box's model that provides string IDs for the values
  in the model, if not equal to -1. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-id-column)
      "Accessor"
      (documentation 'combo-box-id-column 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-id-column object) => id-column}
  @syntax{(setf (gtk:combo-box-id-column object) id-column)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[id-column]{an integer with a column in model to get string IDs for
    values from}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{id-column} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-id-column} function returns the model column which the
  combo box is using to get string IDs for values from. The
  @setf{gtk:combo-box-id-column} function sets the model column.

  The column @arg{id-column} in the model of the combo box must be of type
  @code{gchararray}.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-model ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'combo-box) t)
 "The @code{model} property of type @class{gtk:tree-model} (Read / Write) @br{}
  The model from which the combo box takes the values shown in the list.")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-model)
      "Accessor"
      (documentation 'combo-box-model 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-model object) => model}
  @syntax{(setf (gtk:combo-box-model object) model)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[model]{a @class{gtk:tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{model} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-model} function returns the model which is acting as
  data source for the combo box. The @setf{gtk:combo-box-model} function sets
  the model. Will unset a previously set model, if applicable. If @arg{model} is
  @code{nil}, then it will unset the model.

  Note that this function does not clear the cell renderers, you have to call
  the @fun{gtk:cell-layout-clear} function yourself if you need to set up
  different cell renderers for the new model.
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-layout-clear}")

;;; --- gtk:combo-box-popup-fixed-width ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-fixed-width"
                                               'combo-box) t)
 "The @code{popup-fixed-width} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the popup's width should be a fixed width matching the allocated
  width of the combo box. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-popup-fixed-width)
      "Accessor"
      (documentation 'combo-box-popup-fixed-width 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-popup-fixed-width object) => fixed}
  @syntax{(setf (gtk:combo-box-popup-fixed-width object) fixed)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[fixed]{a boolean whether to use a fixed popup width}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{popup-fixed-width} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-popup-fixed-width} function gets whether the popup
  uses a fixed width matching the allocated width of the combo box. The
  @setf{gtk:combo-box-popup-fixed-width} function specifies whether the popup's
  width should be a fixed.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-popup-shown ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-shown" 'combo-box) t)
 "The @code{popup-shown} property of type @code{:boolean} (Read) @br{}
  Whether the combo boxes dropdown is popped up. Note that this property is
  mainly useful, because it allows you to connect to the
  @code{\"notify::popup-shown\"} signal. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-popup-shown)
      "Accessor"
      (documentation 'combo-box-popup-shown 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-popup-shown object) => popup-shown}
  @syntax{(setf (gtk:combo-box-popup-shown object) popup-shown)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[popup-shown]{a boolean whether the combo boxes dropdown is popped
    up}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{popup-shown} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  Whether the combo boxes dropdown is popped up. Note that this property is
  mainly useful, because it allows you to connect to the
  @code{\"notify::popup-shown\"} signal.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-row-span-column ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "row-span-column" 'combo-box) t)
 "The @code{row-span-column} property of type @code{:int} (Read / Write) @br{}
  If this is set to a non-negative value, it must be the index of a column of
  type \"gint\" in the model. The values of that column are used to determine
  how many rows a value in the list will span. Therefore, the values in the
  model column pointed to by this property must be greater than zero and not
  larger than the @code{wrap-width} property. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-row-span-column)
      "Accessor"
      (documentation 'combo-box-row-span-column 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-row-span-column object) => row-span}
  @syntax{(setf (gtk:combo-box-row-span-column object) row-span)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[row-span]{an integer with a column in the model passed during
    construction}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{row-span-column} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-row-span-column} function returns the column with row
  span information for the combo box. The @setf{gtk:combo-box-row-span-column}
  function sets the column with row span information. The row span column
  contains integers which indicate how many rows an item should span.
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-tearoff-title --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tearoff-title" 'combo-box) t)
 "The @code{tearoff-title} property of type @code{:string} (Read / Write) @br{}
  A title that may be displayed by the window manager when the popup is
  torn-off. @br{}
  @em{Warning:} The @code{tearoff-title} property has been deprecated since
  version 3.10 and should not be used in newly written code. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-tearoff-title)
      "Accessor"
      (documentation 'combo-box-tearoff-title 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-tearoff-title object) => title}
  @syntax{(setf (gtk:combo-box-tearoff-title object) title)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[title]{a string with a title}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{tearoff-title} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  A title that may be displayed by the window manager when the popup is
  torn-off.
  @begin[Warning]{dictionary}
    The @code{tearoff-title} property has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:combo-box}")

;;; --- gtk:combo-box-wrap-width -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-width" 'combo-box) t)
 "The @code{wrap-width} property of type @code{:int} (Read / Write) @br{}
  If @code{wrap-width} is set to a positive value, the list will be displayed
  in multiple columns, the number of columns is determined by @code{wrap-width}.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-wrap-width)
      "Accessor"
      (documentation 'combo-box-wrap-width 'function)
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-wrap-width object) => width}
  @syntax{(setf (gtk:combo-box-wrap-width object) width)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[width]{an integer with the preferred number of columns}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{wrap-width} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-row-wrap-width} function returns the wrap width which
  is used to determine the number of columns for the popup menu. The
  @setf{gtk:combo-box-row-wrap-width} function sets the wrap width. If the wrap
  width is larger than 1, the combo box is in table mode. The wrap width is
  basically the preferred number of columns when you want the popup to be layed
  out in a table.
  @see-class{gtk:combo-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new))

(defun combo-box-new ()
 #+liber-documentation
 "@version{#2023-3-17}
  @return{The new @class{gtk:combo-box} widget.}
  @short{Creates a new empty combo box.}
  @see-class{gtk:combo-box}
  @see-function{gtk:combo-box-new-with-entry}
  @see-function{gtk:combo-box-new-with-model}
  @see-function{gtk:combo-box-new-with-model-and-entry}"
  (make-instance 'combo-box))

(export 'combo-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-entry))

(defun combo-box-new-with-entry ()
 #+liber-documentation
 "@version{#2023-3-17}
  @return{The new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new empty combo box with an entry.
  @end{short}
  @see-class{gtk:combo-box}
  @see-function{gtk:combo-box-new}
  @see-function{gtk:combo-box-new-with-model}
  @see-function{gtk:combo-box-new-with-model-and-entry}"
  (make-instance 'combo-box
                 :has-entry t))

(export 'combo-box-new-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-model))

(defun combo-box-new-with-model (model)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{The new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new combo box with the model initialized to @arg{model}.
  @end{short}
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-model}
  @see-function{gtk:combo-box-new}
  @see-function{gtk:combo-box-new-with-entry}
  @see-function{gtk:combo-box-new-with-model-and-entry}"
  (make-instance 'combo-box
                 :model model))

(export 'combo-box-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model_and_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-model-and-entry))

(defun combo-box-new-with-model-and-entry (model)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{The new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new empty combo box with an entry and with the model initialized
    to @arg{model}.
  @end{short}
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-model}
  @see-function{gtk:combo-box-new}
  @see-function{gtk:combo-box-new-with-entry}
  @see-function{gtk:combo-box-new-with-model}"
  (make-instance 'combo-box
                 :model model
                 :has-entry t))

(export 'combo-box-new-with-model-and-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-area))

(defun combo-box-new-with-area (area)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[area]{a @class{gtk:cell-area} object to use to layout cell
    renderers}
  @return{The new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new empty combo box using @arg{area} to layout cells.
  @end{short}
  @see-class{gtk:combo-box}
  @see-class{gtk:cell-area}
  @see-function{gtk:combo-box-new-with-area-and-entry}"
  (make-instance 'combo-box
                 :cell-area area))

(export 'combo-box-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_area_and_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-area-and-entry))

(defun combo-box-new-with-area-and-entry (area)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[area]{a @class{gtk:cell-area} object to use to layout cell
    renderers}
  @return{The new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new empty combo box with an entry.
  @end{short}
  The new combo box will use @arg{area} to layout cells.
  @see-class{gtk:combo-box}
  @see-class{gtk:cell-area}
  @see-function{gtk:combo-box-new-with-area}"
  (make-instance 'combo-box
                 :cell-area area
                 :has-entry t))

(export 'combo-box-new-with-area-and-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active_iter ()
;;; gtk_combo_box_set_active_iter () -> combo-box-active-iter
;;; ----------------------------------------------------------------------------

(defun (setf combo-box-active-iter) (iter combo)
  (cffi:foreign-funcall "gtk_combo_box_set_active_iter"
                        (g:object combo-box) combo
                        (g:boxed tree-iter) iter
                        :void)
  iter)

(cffi:defcfun ("gtk_combo_box_get_active_iter" %combo-box-active-iter) :boolean
  (combo (g:object combo-box))
  (iter (g:boxed tree-iter)))

(defun combo-box-active-iter (combo)
 #+liber-documentation
 "@version{#2023-3-17}
  @syntax{(gtk:combo-box-active-iter combo-box) => iter}
  @syntax{(setf (gtk:combo-box-active-iter combo-box) iter)}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[iter]{the @class{gtk:tree-iter}, or @code{nil}}
  @begin{short}
    Accessor of the active iterator of the combo box.
  @end{short}
  The @fun{gtk:combo-box-active-iter} function returns @arg{iter} to point to
  the current active item, if it exists. The
  @setf{gtk:combo-box-active-iter} function sets the current active item to be
  the one referenced by @arg{iter}, or unsets the active item if @arg{iter} is
  @code{nil}.
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-iter}"
  (let ((iter (make-instance 'tree-iter)))
    (when (%combo-box-active-iter combo iter)
      iter)))

(export 'combo-box-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup_for_device ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_combo_box_popup_for_device" combo-box-popup-for-device) :void
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[device]{a @class{gdk:device} object}
  @begin{short}
    Pops up the menu or dropdown list of the combo box.
  @end{short}
  The popup window will be grabbed so only @arg{device} and its associated
  pointer/keyboard are the only @class{gdk:device} objects able to send events
  to it.
  @see-class{gtk:combo-box}
  @see-class{gdk:device}"
  (combo (g:object combo-box))
  (device (g:object gdk.device)))

(export 'combo-box-popup-for-device)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_combo_box_popup" combo-box-popup) :void
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @begin{short}
    Pops up the menu or dropdown list of the combo box.
  @end{short}
  This function is mostly intended for use by accessibility technologies.
  Applications should have little use for it.
  @see-class{gtk:combo-box}"
  (combo (g:object combo-box)))

(export 'combo-box-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popdown ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_combo_box_popdown" combo-box-popdown) :void
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @begin{short}
    Hides the menu or dropdown list of the combo box.
  @end{short}
  This function is mostly intended for use by accessibility technologies.
  Applications should have little use for it.
  @see-class{gtk:combo-box}"
  (combo (g:object combo-box)))

(export 'combo-box-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_popup_accessible () -> combo-box-popup-accessible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_combo_box_get_popup_accessible" combo-box-popup-accessible)
    g:object
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @return{The accessible object corresponding to the combo box's popup.}
  @begin{short}
    Gets the accessible object corresponding to the combo box's popup.
  @end{short}
  This function is mostly intended for use by accessibility technologies.
  Applications should have little use for it.
  @see-class{gtk:combo-box}"
  (combo (g:object combo-box)))

(export 'combo-box-popup-accessible)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_separator_func ()                not exported
;;; ----------------------------------------------------------------------------

;; TODO: Is this function useful in the Lisp implementation?

(cffi:defcfun ("gtk_combo_box_get_row_separator_func"
               combo-box-get-row-separator-func) :pointer
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo-box]{a @class{gtk:combo-box} widget}
  @return{The current row separator function.}
  @short{Returns the current row separator function.}
  @see-class{gtk:combo-box}"
  (combo-box (g:object combo-box)))

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_row_separator_func ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_combo_box_set_row_separator_func"
               %combo-box-set-row-separator-func) :void
  (combo (g:object combo-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun combo-box-set-row-separator-func (combo func)
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[func]{a @symbol{gtk:tree-view-row-separator-func} callback function}
  @begin{short}
    Sets the row separator function, which is used to determine whether a row
    should be drawn as a separator.
  @end{short}
  If the row separator function is @code{nil}, no separators are drawn. This is
  the default value.
  @see-class{gtk:combo-box}
  @see-symbol{gtk:tree-view-row-separator-func}"
  (%combo-box-set-row-separator-func
          combo
          (cffi:callback tree-view-row-separator-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'combo-box-set-row-separator-func)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_title ()                             not exported
;;; ----------------------------------------------------------------------------

;; Implemented as the combo-box-tearoff-title function.
;; This function is deprecated and not exported.

(cffi:defcfun ("gtk_combo_box_set_title" combo-box-set-title) :void
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo-box]{a @class{gtk:combo-box} widget}
  @argument[title]{a title for the menu in tearoff mode}
  @begin{short}
    Sets the menu's title in tearoff mode.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:combo-box-set-title} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-function{gtk:combo-box-get-title}"
  (combo (g:object combo-box))
  (title :string))

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_title ()                             not exported
;;; ----------------------------------------------------------------------------

;; Implemented as the combo-box-tearoff-title function.
;; This function is deprecated and not exported.

(cffi:defcfun ("gtk_combo_box_get_title" combo-box-get-title) :string
 #+liber-documentation
 "@version{#2023-3-17}
  @argument[combo-box]{a @class{gtk:combo-box} widget}
  @return{The menu's title in tearoff mode.}
  @begin{short}
    Gets the current title of the menu in tearoff mode.
  @end{short}
  See the @fun{gtk:combo-box-set-add-tearoffs} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:combo-box-get-title} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-function{gtk:combo-box-set-title}
  @see-function{gtk:combo-box-set-add-tearoffs}"
  (combo (g:object combo-box)))

;;; --- End of file gtk3.combo-box.lisp ----------------------------------------
