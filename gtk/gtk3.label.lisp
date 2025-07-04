;;; ----------------------------------------------------------------------------
;;; gtk3.label.lisp
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
;;; GtkLabel
;;;
;;;     A widget that displays a small to medium amount of text.
;;;
;;; Types and Values
;;;
;;;     GtkLabel
;;;
;;; Accessors
;;;
;;;     gtk_label_get_angle
;;;     gtk_label_set_angle
;;;     gtk_label_get_attributes
;;;     gtk_label_set_attributes
;;;     gtk_label_get_ellipsize
;;;     gtk_label_set_ellipsize
;;;     gtk_label_get_justify
;;;     gtk_label_set_justify
;;;     gtk_label_get_label
;;;     gtk_label_set_label
;;;     gtk_label_get_lines
;;;     gtk_label_set_lines
;;;     gtk_label_get_max_width_chars
;;;     gtk_label_set_max_width_chars
;;;     gtk_label_get_mnemonic_keyval
;;;     gtk_label_get_mnemonic_widget
;;;     gtk_label_set_mnemonic_widget
;;;     gtk_label_set_pattern
;;;     gtk_label_get_selectable
;;;     gtk_label_set_selectable
;;;     gtk_label_get_single_line_mode
;;;     gtk_label_set_single_line_mode
;;;     gtk_label_get_track_visited_links
;;;     gtk_label_set_track_visited_links
;;;     gtk_label_get_use_markup
;;;     gtk_label_set_use_markup
;;;     gtk_label_get_use_underline
;;;     gtk_label_set_use_underline
;;;     gtk_label_get_width_chars
;;;     gtk_label_set_width_chars
;;;     gtk_label_get_xalign
;;;     gtk_label_set_xalign
;;;     gtk_label_get_yalign
;;;     gtk_label_set_yalign
;;;
;;; Functions
;;;
;;;     gtk_label_new
;;;     gtk_label_set_text
;;;     gtk_label_set_markup
;;;     gtk_label_set_markup_with_mnemonic
;;;     gtk_label_set_line_wrap
;;;     gtk_label_set_line_wrap_mode
;;;     gtk_label_get_layout_offsets
;;;     gtk_label_get_text
;;;     gtk_label_new_with_mnemonic
;;;     gtk_label_select_region
;;;     gtk_label_set_text_with_mnemonic
;;;     gtk_label_get_layout
;;;     gtk_label_get_line_wrap
;;;     gtk_label_get_line_wrap_mode
;;;     gtk_label_get_selection_bounds
;;;     gtk_label_get_current_uri
;;;
;;; Properties
;;;
;;;     angle
;;;     attributes
;;;     cursor-position
;;;     ellipsize
;;;     justify
;;;     label
;;;     lines
;;;     max-width-chars
;;;     mnemonic-keyval
;;;     mnemonic-widget
;;;     pattern
;;;     selectable
;;;     selection-bound
;;;     single-line-mode
;;;     track-visited-links
;;;     use-markup
;;;     use-underline
;;;     width-chars
;;;     wrap
;;;     wrap-mode
;;;     xalign
;;;     yalign
;;;
;;; Signals
;;;
;;;     activate-current-link
;;;     activate-link
;;;     copy-clipboard
;;;     move-cursor
;;;     populate-popup
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkMisc
;;;                 ╰── GtkLabel
;;;                     ╰── GtkAccelLabel
;;;
;;; Implemented Interfaces
;;;
;;;     GtkLabel implements AtkImplementorIface and GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkLabel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkLabel" label
  (:superclass misc
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable")
   :type-initializer "gtk_label_get_type")
  ((angle
    label-angle
    "angle" "gdouble" t t)
   (attributes
    label-attributes
    "attributes" "PangoAttrList" t t)
   (cursor-position
    label-cursor-position
    "cursor-position" "gint" t nil)
   (ellipsize
    label-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (justify
    label-justify
    "justify" "GtkJustification" t t)
   (label
    label-label
    "label" "gchararray" t t)
   (lines
    label-lines
    "lines" "gint" t t)
   (max-width-chars
    label-max-width-chars
    "max-width-chars" "gint" t t)
   (mnemonic-keyval
    label-mnemonic-keyval
    "mnemonic-keyval" "guint" t nil)
   (mnemonic-widget
    label-mnemonic-widget
    "mnemonic-widget" "GtkWidget" t t)
   (pattern
    label-pattern
    "pattern" "gchararray" nil t)
   (selectable
    label-selectable
    "selectable" "gboolean" t t)
   (selection-bound
    label-selection-bound
    "selection-bound" "gint" t nil)
   (single-line-mode
    label-single-line-mode
    "single-line-mode" "gboolean" t t)
   (track-visited-links
    label-track-visited-links
    "track-visited-links" "gboolean" t t)
   (use-markup
    label-use-markup
    "use-markup" "gboolean" t t)
   (use-underline
    label-use-underline
    "use-underline" "gboolean" t t)
   (width-chars
    label-width-chars
    "width-chars" "gint" t t)
   (wrap
    label-wrap
    "wrap" "gboolean" t t)
   (wrap-mode
    label-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (xalign
    label-xalign
    "xalign" "gfloat" t t)
   (yalign
    label-yalign
    "yalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'label 'type)
 "@version{2025-06-28}
  @begin{short}
    The @class{gtk:label} widget displays a small amount of text.
  @end{short}
  As the name implies, most labels are used to label another widget such as
  a @class{gtk:button}, a @class{gtk:menu-item}, or a @class{gtk:combo-box}
  widget.

  @subheading{Mnemonics}
  Labels may contain mnemonics. Mnemonics are underlined characters in the
  label, used for keyboard navigation. Mnemonics are created by providing a
  string with an underscore before the mnemonic character, such as \"_File\",
  to the @fun{gtk:label-new-with-mnemonic} or
  @fun{gtk:label-set-text-with-mnemonic} functions.

  Mnemonics automatically activate any activatable widget the label is inside,
  such as a @class{gtk:button} widget. If the label is not inside the
  target widget of the mnemonic, you have to tell the label about the target
  using the @fun{gtk:label-mnemonic-widget} function. Here is a simple example
  where the label is inside a button:
  @begin{pre}
;; Pressing Alt+H will activate this button
(let ((button (make-instance 'gtk:button)))
   (gtk:container-add button
                      (gtk:label-new-with-mneonic \"_Hello\"))
   ... )
  @end{pre}
  There is a convenience function to create buttons with a mnemonic label
  already inside:
  @begin{pre}
;; Pressing Alt+H will activate this button
(gtk:button-new-with-mnemonic \"_Hello\")
  @end{pre}
  To create a mnemonic for a widget alongside the label, such as a
  @class{gtk:entry} widget, you have to point the label at the entry with the
  @fun{gtk:label-mnemonic-widget} function:
  @begin{pre}
;; Pressing Alt+H will focus the entry
(let ((entry (make-instance 'gtk:entry))
      (label (gtk:label-new-with-mnemonic \"_Hello\")))
   (gtk:label-mnemonic-widget label entry)
   ... )
  @end{pre}
  @subheading{Markup (styled text)}
  To make it easy to format text in a label, changing colors, fonts, and so on,
  label text can be provided in a simple markup format. Here is how to create
  a label with a small font:
  @begin{pre}
(let ((label (make-instance 'gtk:label)))
  (gtk:label-set-markup label
                        \"<span style=\"color: red\">
                         <small>Small text</small></span>\")
  ... )
  @end{pre}
  See complete documentation of available tags in the Pango manual.

  The markup passed to the @fun{gtk:label-set-markup} function must be valid.
  For example, literal <, > and & characters must be escaped as \<, \gt;, and
  \&. If you pass text obtained from the user, file, or a network to the
  @fun{gtk:label-set-markup} function, you will want to escape it with the
  @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()} functions.

  Markup strings are just a convenient way to set the @class{pango:attr-list}
  instance on a label. The @fun{gtk:label-attributes} function may be a simpler
  way to set attributes in some cases. Be careful though, a
  @class{pango:attr-list} instance tends to cause internationalization problems,
  unless you are applying attributes to the entire string, that is, unless you
  set the range of each attribute to @code{[0, G_MAXINT]}). The reason is that
  specifying the @code{start_index} and @code{end_index} for a
  @class{pango:attribute} structure requires knowledge of the exact string
  being displayed, so translations will cause problems.

  @subheading{Selectable labels}
  Labels can be made selectable with the @fun{gtk:label-selectable} function.
  Selectable labels allow the user to copy the label contents to the clipboard.
  Only labels that contain useful-to-copy information - such as error messages -
  should be made selectable.

  @subheading{Text layout}
  A label can contain any number of paragraphs, but will have performance
  problems if it contains more than a small number. Paragraphs are separated
  by newlines or other paragraph separators understood by Pango.

  Labels can automatically wrap text if you call the @fun{gtk:label-line-wrap}
  function.

  The @fun{gtk:label-justify} function sets how the lines in a label align with
  one another. If you want to set how the label as a whole aligns in its
  available space, see the @slot[gtk:widget]{halign} and
  @slot[gtk:widget]{valign} properties.

  The @slot[gtk:label]{width-chars} and @slot[gtk:label]{max-width-chars}
  properties can be used to control the size allocation of ellipsized or
  wrapped labels. For ellipsizing labels, if either is specified and less than
  the actual text size, it is used as the minimum width, and the actual text
  size is used as the natural width of the label. For wrapping labels,
  @slot[gtk:label]{width-chars} is used as the minimum width, if specified, and
  @slot[gtk:label]{max-width-chars} is used as the natural width. Even if
  @slot[gtk:label]{max-width-chars} specified, wrapping labels will be
  rewrapped to use all of the available width.

  Note that the interpretation of the @slot[gtk:label]{width-chars} and
  @slot[gtk:label]{max-width-chars} properties has changed a bit with the
  introduction of \"width-for-height\" geometry management.

  @subheading{Links}
  GTK supports markup for clickable hyperlinks in addition to regular Pango
  markup. The markup for links is borrowed from HTML, using the @code{a} with
  href and title attributes. GTK renders links similar to the way they appear
  in web browsers, with colored, underlined text. The title attribute is
  displayed as a tooltip on the link. An example looks like this:
  @begin{pre}
(gtk:label-set-markup label
                      \"Go to the <span style=\"color: red\">
                                   <a>GTK website</a></span> for more...\")
  @end{pre}
  It is possible to implement custom handling for links and their tooltips
  with the @sig[gtk:label]{activate-link} signal and the
  @fun{gtk:label-current-uri} function.
  @begin[GtkLabel as GtkBuildable]{dictionary}
    The @class{gtk:label} implementation of the @class{gtk:buildable} interface
    supports a custom @code{<attributes>} element, which supports any number of
    @code{<attribute>} elements. The @code{<attribute>} element has attributes
    named @code{name}, @code{value}, @code{start} and @code{end} and allows you
    to specify @code{PangoAttribute} values for this label.

    @b{Example:} A UI definition fragment specifying Pango attributes
  @begin{pre}
<object class=\"GtkLabel\">
 <attributes>
    <attribute name=\"weight\" value=\"PANGO_WEIGHT_BOLD\"/>
    <attribute name=\"background\" value=\"red\" start=\"5\" end=\"10\"/>\"
  </attributes>
</object>
    @end{pre}
    The @code{start} and @code{end} attributes specify the range of characters
    to which the Pango attribute applies. If @code{start} and @code{end} are not
    specified, the attribute is applied to the whole text. Note that specifying
    ranges does not make much sense with translatable attributes. Use markup
    embedded in the translatable content instead.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
label
├── [selection]
├── [link]
┊
╰── [link]
    @end{pre}
    The @class{gtk:label} implementation has a single CSS node with the name
    @code{label}. A wide variety of style classes may be applied to labels, such
    as the @code{.title}, @code{.subtitle}, @code{.dim-label} style classes. In
    the @class{gtk:shortcuts-window} widget, labels are used wth the
    @code{.keycap} style class.

    If the label has a selection, it gets a subnode with name @code{selection}.

    If the label has links, there is one subnode per link. These subnodes carry
    the link or visited state depending on whether they have been visited.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[label::activate-current-link]{signal}
      @begin{pre}
lambda (label)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget on which the signal was
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user activates a link in
      the label. Applications may also emit the signal with the
      @fun{g:signal-emit} function if they need to control activation of URIs
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
    @end{signal}
    @begin[label::activate-link]{signal}
      @begin{pre}
lambda (label uri)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget on which the signal was
          emitted.}
        @entry[uri]{The string with the URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{simple-table}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call the
      @fun{gtk:show-uri} function.
    @end{signal}
    @begin[label::copy-clipboard]{signal}
      @begin{pre}
lambda (label)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget which received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to copy the selection
      to the clipboard. The default binding for this signal is the @kbd{Ctrl-c}
      key.
    @end{signal}
    @begin[label::move-cursor]{signal}
      @begin{pre}
lambda (label step count extend)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
          @sym{gtk:movement-step} enumeration.}
        @entry[count]{The integer with the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. If the cursor is not visible in the label,
      this signal causes the viewport to be moved instead. Applications should
      not connect to it, but may emit it with the @fun{g:signal-emit} function
      if they need to control the cursor programmatically. The default bindings
      for this signal come in two variants, the variant with the @kbd{Shift}
      modifier extends the selection, the variant without the @kbd{Shift}
      modifier does not. There are too many key combinations to list them all
      here.
      @begin{itemize}
        @item{Arrow keys move by individual characters/lines.}
        @item{@kbd{Ctrl}-arrow key combinations move by words/paragraphs.}
        @item{@kbd{Home}/@kbd{End} keys move to the ends of the buffer.}
      @end{itemize}
    @end{signal}
    @begin[label::populate-popup]{signal}
      @begin{pre}
lambda (label menu)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget on which the signal is
          emitted.}
        @entry[menu]{The @class{gtk:menu} widget that is being populated.}
      @end{simple-table}
      The signal gets emitted before showing the context menu of the label. Note
      that only selectable labels have context menus. If you need to add items
      to the context menu, connect to this signal and append your menuitems to
      the menu.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:label-new}
  @see-constructor{gtk:label-new-with-mnemonic}
  @see-slot{gtk:label-angle}
  @see-slot{gtk:label-attributes}
  @see-slot{gtk:label-cursor-position}
  @see-slot{gtk:label-ellipsize}
  @see-slot{gtk:label-justify}
  @see-slot{gtk:label-label}
  @see-slot{gtk:label-lines}
  @see-slot{gtk:label-max-width-chars}
  @see-slot{gtk:label-mnemonic-keyval}
  @see-slot{gtk:label-mnemonic-widget}
  @see-slot{gtk:label-pattern}
  @see-slot{gtk:label-selectable}
  @see-slot{gtk:label-selection-bound}
  @see-slot{gtk:label-single-line-mode}
  @see-slot{gtk:label-track-visited-links}
  @see-slot{gtk:label-use-markup}
  @see-slot{gtk:label-use-underline}
  @see-slot{gtk:label-width-chars}
  @see-slot{gtk:label-wrap}
  @see-slot{gtk:label-wrap-mode}
  @see-slot{gtk:label-xalign}
  @see-slot{gtk:label-yalign}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:label-angle --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "angle" 'label) t)
 "The @code{angle} property of type @code{:double} (Read / Write) @br{}
  The angle that the baseline of the label makes with the horizontal, in
  degrees, measured counterclockwise. An angle of 90 degrees reads from bottom
  to top, an angle of 270 degrees, from top to bottom. Ignored if the label is
  selectable, wrapped, or ellipsized. @br{}
  Allowed values: [0.0, 360.0] @br{}
  Default value: 0.0 @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'label-angle)
      "Accessor"
      (documentation 'label-angle 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-angle object) => angle}
  @syntax{(setf (gtk:label-angle object) angle)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[angle]{a number coerced to a double float for the angle that the
    baseline of the label makes with the horizontal, in degrees, measured
    counterclockwise}
  @begin{short}
    Accessor of the @slot[gtk:label]{angle} slot of the @class{gtk:label} class.
  @end{short}
  The @fun{gtk:label-angle} function sets the angle of rotation for the label.
  The @setf{gtk:label-angle} function sets the angle of rotation for the label.

  An angle of 90 degrees reads from from bottom to top, an angle of 270 degrees,
  from top to bottom. The angle setting for the label is ignored if the label is
  selectable, wrapped, or ellipsized.
  @see-class{gtk:label}")

;;; --- gtk:label-attributes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'label) t)
 "The @code{attributes} property of type @class{pango:attr-list}
  (Read / Write) @br{}
  The list of style attributes to apply to the text of the label.")

#+liber-documentation
(setf (liber:alias-for-function 'label-attributes)
      "Accessor"
      (documentation 'label-attributes 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-attributes object) => attrs}
  @syntax{(setf (gtk:label-attributes object) attrs)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[attrs]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:label]{attributes} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-attributes} function gets the attribute list that was set
  on the label, if any. The @setf{gtk:label-attributes} function sets a
  attribute list. The attributes in the list are applied to the label text.

  This function does not reflect attributes that come from the labels markup,
  see the @fun{gtk:label-set-markup} function. If you want to get the
  effective attributes for the label, use
  @begin{pre}
(pango:layout-attributes (gtk:label-layout label))
  @end{pre}
  @begin[Notes]{dictionary}
    The attributes set with this function will be applied and merged with any
    other attributes previously effected by way of the
    @slot[gtk:label]{use-underline} or @slot[gtk:label]{use-markup} properties.
    While it is not recommended to mix markup strings with manually set
    attributes, if you must, know that the attributes will be applied to the
    label after the markup string is parsed.
  @end{dictionary}
  @see-class{gtk:label}
  @see-class{pango:attr-list}
  @see-function{gtk:label-set-markup}
  @see-function{gtk:label-layout}
  @see-function{gtk:label-use-markup}
  @see-function{gtk:label-use-underline}")

;;; --- gtk:label-cursor-position ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-position" 'label) t)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'label-cursor-position)
      "Accessor"
      (documentation 'label-cursor-position 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-cursor-position object) => position}
  @syntax{(setf (gtk:label-cursor-position object) position)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[position]{an integer for the position of the insertion cursor}
  @begin{short}
    Accessor of the @slot[gtk:label]{cursor-position} slot of the
    @class{gtk:label} class.
  @end{short}
  The current position of the insertion cursor in chars.
  @see-class{gtk:label}")

;;; --- gtk:label-ellipsize ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize" 'label) t)
 "The @code{ellipsize} property of type @sym{pango:ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the label does not have
  enough room to display the entire string, specified as a value of the
  @sym{pango:ellipsize-mode} enumeration. Note that setting this property to
  a value other than @val[pango:ellipsize-mode]{:none} has the side-effect that
  the label requests only enough space to display the ellipsis \"...\". In
  particular, this means that ellipsizing labels do not work well in notebook
  tabs, unless the @prop[gtk:notebook]{tab-expand} child property of the tab is
  set to @em{true}. Other ways to set a width of the label are the
  @fun{gtk:widget-size-request} and @fun{gtk:label-width-chars} functions. @br{}
  Default value: @val[pango:ellipsize-mode]{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'label-ellipsize)
      "Accessor"
      (documentation 'label-ellipsize 'function)
 "@version{2025-06-28}
  @syntax{(gtk:label-ellipsize object) => mode}
  @syntax{(setf (gtk:label-ellipsize object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{a value of the @sym{pango:ellipsize-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:label]{ellipsize} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-ellipsize} function returns the ellipsizing position of the
  label. The @setf{gtk:label-ellipsize} function sets the mode used to ellipsize,
  add an ellipsis: \"...\", to the text if there is not enough space
  to render the entire string.
  @see-class{gtk:label}
  @see-symbol{pango:ellipsize-mode}")

;;; --- gtk:label-justify ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "justify" 'label) t)
 "The @code{justify} property of type @sym{gtk:justification} (Read / Write)
  @br{}
  The alignment of the lines in the text of the label relative to each other.
  This does not affect the alignment of the label within its allocation. See
  the @slot[gtk:label]{xalign} property for that. @br{}
  Default value: @val[gtk:justification]{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'label-justify)
      "Accessor"
      (documentation 'label-justify 'function)
 "@version{2025-06-28}
  @syntax{(gtk:label-justify object) => justify}
  @syntax{(setf (gtk:label-justify object) justify)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[justify]{a value of the @sym{gtk:justification} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:label]{justify} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-justify} function returns the justification of the label.
  The @setf{gtk:label-justify} function sets the alignment of the lines in the
  text of the label relative to each other.

  The @val[gtk:justification]{:left} value is the default value when the widget
  is first created with the @fun{gtk:label-new} function. If you instead want
  to set the alignment of the label as a whole, use the @fun{gtk:widget-halign}
  function instead. The @setf{gtk:label-justify} function has no effect on
  labels containing only a single line.
  @see-class{gtk:label}
  @see-symbol{gtk:justification}
  @see-function{gtk:label-new}
  @see-function{gtk:widget-halign}")

;;; --- gtk:label-label --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'label) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text of the label. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'label-label)
      "Accessor"
      (documentation 'label-label 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-label object) => text}
  @syntax{(setf (gtk:label-label object) text)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[text]{a string for the text for the label}
  @begin{short}
    Accessor of the @slot[gtk:label]{label} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-label} function returns the text of the label widget
  including any embedded underlines indicating mnemonics and Pango markup. See
  the @fun{gtk:label-text} function.

  The @setf{gtk:label-label} function sets the text of the label. The label is
  interpreted as including embedded underlines and/or Pango markup depending on
  the values of the @slot[gtk:label]{use-underline} and
  @slot[gtk:label]{use-markup} properties.
  @see-class{gtk:label}
  @see-function{gtk:label-text}
  @see-function{gtk:label-use-markup}
  @see-function{gtk:label-use-underline}")

;;; --- gtk:label-lines --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "lines" 'label) t)
 "The @code{lines} property of type @code{:int} (Read / Write) @br{}
  The number of lines to which an ellipsized, wrapping label should be limited.
  This property has no effect if the label is not wrapping or ellipsized. Set
  this property to -1 if you do not want to limit the number of lines. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-lines)
      "Accessor"
      (documentation 'label-lines 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-lines object) => lines}
  @syntax{(setf (gtk:label-lines object) lines)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[lines]{an integer for the desired number of lines, or -1}
  @begin{short}
    Accessor of the @slot[gtk:label]{lines} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-lines} function gets the number of lines to which an
  ellipsized, wrapping label should be limited. The @setf{gtk:label-lines}
  function sets the number of lines to which an ellipsized, wrapping label
  should be limited. This has no effect if the label is not wrapping or
  ellipsized. Set this to -1 if you do not want to limit the number of lines.
  @see-class{gtk:label}")

;;; --- gtk:label-max-width-chars ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width-chars" 'label) t)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the label, in characters. If this property is
  set to -1, the width will be calculated automatically. See the section on text
  layout for details of how the @slot[gtk:label]{width-chars} and
  @slot[gtk:label]{max-width-chars} properties determine the width of ellipsized
  and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-max-width-chars)
      "Accessor"
      (documentation 'label-max-width-chars 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-max-width-chars object) => n-chars}
  @syntax{(setf (gtk:label-max-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[n-chars]{an integer for the desired maximum width, in characters}
  @begin{short}
    Accessor of the @slot[gtk:label]{max-width-chars} slot of the
    @class{gtk:label} class.
  @end{short}
  The @fun{gtk:label-max-width-chars} function returns the maximum width of the
  label in characters. The @setf{gtk:label-max-width-chars} function sets the
  desired maximum width in characters of the label to @arg{n-chars}.
  @see-class{gtk:label}")

;;; --- gtk:label-mnemonic-keyval ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-keyval" 'label) t)
 "The @code{mnemonic-keyval} property of @code{:uint} (Read) @br{}
  The mnemonic accelerator key for this label. @br{}
  Default value: @code{#xffffff}")

#+liber-documentation
(setf (liber:alias-for-function 'label-mnemonic-keyval)
      "Accessor"
      (documentation 'label-mnemonic-keyval 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-mnemonic-keyval object) => keyval}
  @argument[object]{a @class{gtk:label} widget}
  @argument[keyval]{an unsigned integer for the keyval}
  @begin{short}
    Accessor of the @slot[gtk:label]{mnemonic-keyval} slot of the
    @class{gtk:label} class.
  @end{short}
  If the label has been set so that it has a mnemonic key the
  @fun{gtk:label-mnemonic-keyval} function returns the keyval used for the
  mnemonic accelerator. If there is no mnemonic set up it returns
  @code{#xffffff}.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq label (gtk:label-new-with-mnemonic \"_Print\"))
=> #<gtk:label {1001A051E3@}>
(gtk:label-mnemonic-keyval label) => 112
    @end{pre}
  @end{dictionary}
  @see-class{gtk:label}")

;;; --- gtk:label-mnemonic-widget ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-widget" 'label) t)
 "The @code{mnemonic-widget} property of type @class{gtk:widget}
  (Read / Write) @br{}
  The widget to be activated when the mnemonic key of the label is pressed.")

#+liber-documentation
(setf (liber:alias-for-function 'label-mnemonic-widget)
      "Accessor"
      (documentation 'label-mnemonic-widget 'function)
 "@version{2025-06-28}
  @syntax{(gtk:label-mnemonic-widget object) => widget}
  @syntax{(setf (gtk:label-mnemonic-widget object) widget)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[widget]{a @class{gtk:widget} widget target}
  @begin{short}
    Accessor of the @slot[gtk:label]{mnemonic-widget} slot of the
    @class{gtk:label} class.
  @end{short}
  The @fun{gtk:label-mnemonic-widget} function returns the target of the
  mnemonic of the label, or @code{nil} if none has been set and the default
  algorithm will be used.

  If the label has been set so that it has an mnemonic key, using, that is
  the @fun{gtk:label-set-markup-with-mnemonic},
  @fun{gtk:label-set-text-with-mnemonic}, @fun{gtk:label-new-with-mnemonic}
  functions or the @slot[gtk:label]{use-underline} property, the label can be
  associated with a widget that is the target of the mnemonic.

  When the label is inside a widget, like a @class{gtk:button} widget or a
  @class{gtk:notebook} tab, it is automatically associated with the correct
  widget, but sometimes, that is, when the target is a @class{gtk:entry} widget
  next to the label, you need to set it explicitly using this function.

  The target widget will be accelerated by emitting the
  @sig[gtk:widget]{mnemonic-activate} signal on it. The default handler for this
  signal will activate the widget if there are no mnemonic collisions and toggle
  focus between the colliding widgets otherwise.
  @see-class{gtk:label}
  @see-class{gtk:widget}
  @see-function{gtk:label-set-markup-with-mnemonic}
  @see-function{gtk:label-set-text-with-mnemonic}
  @see-function{gtk:label-new-with-mnemonic}
  @see-function{gtk:label-use-underline}")

;;; --- gtk:label-pattern ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pattern" 'label) t)
 "The @code{pattern} property of type @code{:string} (Write) @br{}
  The string with \"_\" characters in positions correspond to characters in the
  text to underline. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'label-pattern)
      "Accessor"
      (documentation 'label-pattern 'function)
 "@version{2025-06-17}
  @syntax{(setf (gtk:label-pattern object) pattern)}
  @argument[object]{a @class{gtk:label} widget you want to set the pattern to}
  @argument[pattern]{a string for the pattern as described below}
  @begin{short}
    Accessor of the @slot[gtk:label]{pattern} slot of the @class{gtk:label}
    class.
  @end{short}
  The @setf{gtk:label-pattern} function sets the pattern of underlines you want
  under the existing text within the label. For example, if the current text of
  the label says \"FooBarBaz\" passing a pattern of @code{\"___   ___\"} will
  underline \"Foo\" and \"Baz\" but not \"Bar\".
  @see-class{gtk:label}")

;;; --- gtk:label-selectable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable" 'label) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the label text can be selected with the mouse. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-selectable)
      "Accessor"
      (documentation 'label-selectable 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-selectable object) => selectable}
  @syntax{(setf (gtk:label-selectable object) selectable)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[selectable]{@em{true} to allow selecting text in the label}
  @begin{short}
    Accessor of the @slot[gtk:label]{selectable} slot of the @class{gtk:label}
    class.
  @end{short}
  Selectable labels allow the user to select text from the label, for copy and
  paste.
  @see-class{gtk:label}")

;;; --- gtk:label-selection-bound ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selection-bound" 'label) t)
 "The @code{selection-bound} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in chars.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'label-selection-bound)
      "Accessor"
      (documentation 'label-selection-bound 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-selection-bound object) => bound}
  @argument[object]{a @class{gtk:label} widget}
  @argument[bound]{an integer for a position}
  @begin{short}
    Accessor of the @slot[gtk:label]{selection-bound} slot of the
    @class{gtk:label} class.
  @end{short}
  The position of the opposite end of the selection from the cursor in chars.
  @see-class{gtk:label}")

;;; --- gtk:label-single-line-mode ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-line-mode" 'label) t)
 "The @code{single-line-mode} property of type  @code{:boolean} (Read / Write)
  @br{}
  Whether the label is in single line mode. In single line mode, the height
  of the label does not depend on the actual text, it is always set to the
  @code{(ascent + descent)} value of the font. This can be an advantage in
  situations where resizing the label because of text changes would be
  distracting, for example in a statusbar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-single-line-mode)
      "Accessor"
      (documentation 'label-single-line-mode 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-single-line-mode object) => mode}
  @syntax{(setf (gtk:label-single-line-mode object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{@em{true} if the label should be in single line mode}
  @begin{short}
    Accessor of the @slot[gtk:label]{single-line-mode} slot of the
    @class{gtk:label} class.
  @end{short}
  The @fun{gtk:label-single-line-mode} function returns whether the label is in
  single line mode. The @setf{gtk:label-single-line-mode} function sets whether
  the label is in single line mode.
  @see-class{gtk:label}")

;;; --- gtk:label-track-visited-links ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "track-visited-links" 'label) t)
 "The @code{track-visited-links} property of type @code{:boolean} (Read / Write)
  @br{}
  Set this property to @em{true} to make the label track which links have been
  clicked. It will then apply the color of the
  @prop[gtk:widget]{visited-link-color} style property of a @class{gtk:widget}
  widget, instead of the color of the @prop[gtk:widget]{link-color} style
  property. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'label-track-visited-links)
      "Accessor"
      (documentation 'label-track-visited-links 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-track-visited-links object) => setting}
  @syntax{(setf (gtk:label-track-visited-links object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} to track visited links}
  @begin{short}
    Accessor of the @slot[gtk:label]{track-visited-links} slot of the
    @class{gtk:label} class.
  @end{short}
  The @fun{gtk:label-track-visited-links} function returns whether the label is
  currently keeping track of clicked links. The
  @setf{gtk:label-track-visited-links} function sets whether the label should
  keep track of clicked links and use a different color for them.
  @see-class{gtk:label}")

;;; --- gtk:label-use-markup ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup" 'label) t)
 "The @code{use-markup} property of type @code{:boolean} (Read / Write) @br{}
  The text of the label includes XML Pango markup. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-use-markup)
      "Accessor"
      (documentation 'label-use-markup 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-use-markup object) => setting}
  @syntax{(setf (gtk:label-use-markup object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} if the text of the label should be parsed for
    markup}
  @begin{short}
    Accessor of the @slot[gtk:label]{use-markup} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-use-markup} function returns whether the text of the label
  is interpreted as marked up with the Pango text markup language. The
  @setf{gtk:label-use-markup} function sets whether the text of the label
  contains markup. See the @fun{gtk:label-set-markup} function.
  @see-class{gtk:label}
  @see-function{gtk:label-set-markup}")

;;; --- gtk:label-use-underline ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'label) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-use-underline)
      "Accessor"
      (documentation 'label-use-underline 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-use-underline object) => setting}
  @syntax{(setf (gtk:label-use-underline object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:label]{use-underline} slot of the
    @class{gtk:label} class.
  @end{short}
  The @fun{gtk:label-use-underline} function returns whether an embedded
  underline in the label indicates a mnemonic. If @em{true}, an underline in the
  text indicates the next character should be used for the mnemonic accelerator
  key.
  @see-class{gtk:label}")

;;; --- gtk:label-width-chars --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars" 'label) t)
 "The @code{width-chars} property of @code{:int} (Read / Write) @br{}
  The desired width of the label, in characters. If this property is set to
  -1, the width will be calculated automatically. See the section on text
  layout for details of how the @slot[gtk:label]{width-chars} and
  @slot[gtk:label]{max-width-chars} properties determine the width of ellipsized
  and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-width-chars)
      "Accessor"
      (documentation 'label-width-chars 'function)
 "@version{2025-06-17}
  @syntax{(gtk:label-width-chars object) => n-chars}
  @syntax{(setf (gtk:label-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[n-chars]{an integer for the new desired width, in characters}
  @begin{short}
    Accessor of the @slot[gtk:label]{width-chars} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-width-chars} function retrieves the desired width of the
  label, in characters. The @setf{gtk:label-width-chars} function sets the
  desired width in characters of the label to @arg{n-chars}.
  @see-class{gtk:label}")

;;; --- gtk:label-wrap ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap" 'label) t)
 "The @code{wrap} property of type @code{:boolean} (Read / Write) @br{}
  If set, wrap lines if the text becomes too wide. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-wrap)
      "Accessor"
      (documentation 'label-wrap 'function)
 "@version{2023-03-05}
  @syntax{(gtk:label-wrap object) => wrap}
  @syntax{(setf (gtk:label-wrap object) wrap)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[wrap]{a boolean whether lines are wrapped}
  @begin{short}
    Accessor of the @slot[gtk:label]{wrap} slot of the @class{gtk:label} class.
  @end{short}
  If set, wrap lines if the text becomes too wide.
  @see-class{gtk:label}")

;;; --- gtk:label-wrap-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode" 'label) t)
 "The @code{wrap-mode} property of type @sym{pango:wrap-mode} (Read / Write)
  @br{}
  If line wrapping is on, see the @slot[gtk:label]{wrap} property, this controls
  how the line wrapping is done. The default is @val[pango:wrap-mode]{:word},
  which means wrap on word boundaries. @br{}
  Default value: @val[pango:wrap-mode]{:word}")

#+liber-documentation
(setf (liber:alias-for-function 'label-wrap-mode)
      "Accessor"
      (documentation 'label-wrap-mode 'function)
 "@version{2025-06-28}
  @syntax{(gtk:label-wrap-mode object) => setting}
  @syntax{(setf (gtk:label-wrap-mode object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{a value of the @sym{pango:wrap-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:label]{wrap-mode} slot of the @class{gtk:label}
    class.
  @end{short}
  If line wrapping is on, see the @slot[gtk:label]{wrap} property, this controls
  how the line wrapping is done. The default is @val[pango:wrap-mode]{:word},
  which means wrap on word boundaries.
  @see-class{gtk:label}
  @see-symbol{pango:wrap-mode}
  @see-function{gtk:label-wrap}")

;;; --- gtk:label-xalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'label) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  Determines the horizontal aligment of the label text inside the labels size
  allocation. Compare this to the @slot[gtk:widget]{halign} property, which
  determines how the labels size allocation is positioned in the space available
  for the label. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'label-xalign)
      "Accessor"
      (documentation 'label-xalign 'function)
 "@version{2025-06-28}
  @syntax{(gtk:label-xalign object) => xalign}
  @syntax{(setf (gtk:label-xalign object) xalign)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[xalign]{a number coerced to a single float for the xalign value,
    between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk:label]{xalign} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-xalign} function sets the @slot[gtk:label]{xalign} property
  for the label. The @setf{gtk:label-xalign} function sets the property.
  @see-class{gtk:label}
  @see-function{gtk:label-yalign}")

;;; --- gtk:label-yalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'label) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  Determines the vertical aligment of the label text inside the labels size
  allocation. Compare this to @slot[gtk:widget]{valign}, which determines how
  the labels size allocation is positioned in the space available for the label.
  @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'label-yalign)
      "Accessor"
      (documentation 'label-yalign 'function)
 "@version{2025-06-28}
  @syntax{(gtk:label-yalign object) => yalign}
  @syntax{(setf (gtk:label-yalign object) yalign)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[yalign]{a number coerced to a single float for the yalign value,
    between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk:label]{yalign} slot of the @class{gtk:label}
    class.
  @end{short}
  The @fun{gtk:label-yalign} function sets the @slot[gtk:label]{yalign} property
  for the label. The @setf{gtk:label-yalign} function sets the property.
  @see-class{gtk:label}
  @see-function{gtk:label-xalign}")

;;; ----------------------------------------------------------------------------
;;; gtk_label_new
;;; ----------------------------------------------------------------------------

(defun label-new (text)
 #+liber-documentation
 "@version{2025-06-17}
  @argument[text]{a string for the text of the label}
  @return{The new @class{gtk:label} widget.}
  @begin{short}
    Creates a new label with the given text inside it.
  @end{short}
  You can pass @code{nil} to get an empty label widget.
  @see-class{gtk:label}"
  (let ((label (make-instance 'label)))
    (when text
      (setf (label-label label) text))
    label))

(export 'label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_text
;;; gtk_label_set_text
;;; ----------------------------------------------------------------------------

(defun (setf label-text) (text label)
  (cffi:foreign-funcall "gtk_label_set_text"
                        (g:object label) label
                        :string text
                        :void)
  text)

(cffi:defcfun ("gtk_label_get_text" label-text) :string
 #+liber-documentation
 "@version{2025-06-17}
  @syntax{(gtk:label-text label) => text}
  @syntax{(setf (gtk:label-text-label) text)}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a string for the text}
  @begin{short}
    The @fun{gtk:label-text} function fetches the text from a label, as
    displayed on the screen.
  @end{short}
  The @setf{gtk:label-text} function sets the text.

  It overwrites any text that was there before. This will also clear any
  previously set mnemonic accelerators. This does not include any embedded
  underlines indicating mnemonics or Pango markup. See the @fun{gtk:label-label}
  function.
  @see-class{gtk:label}
  @see-function{gtk:label-label}"
  (label (g:object label)))

(export 'label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_set_markup" label-set-markup) :void
 #+liber-documentation
 "@version{2023-03-05}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language,
    setting the text of the label and attribute list based on the parse results.
  @end{short}
  @see-class{gtk:label}
  @see-function{gtk:label-text}
  @see-function{gtk:label-set-markup-with-mnemonic}"
  (label (g:object label))
  (text :string))

(export 'label-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup_with_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_set_markup_with_mnemonic"
               label-set-markup-with-mnemonic) :void
 #+liber-documentation
 "@version{2023-03-05}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a Pango markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language.
  @end{short}
  This sets the text and attribute list of the label based on the parse results.

  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic.
  The mnemonic key can be used to activate another widget, chosen automatically,
  or explicitly using the @fun{gtk:label-mnemonic-widget} function.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (label (g:object label))
  (text :string))

(export 'label-set-markup-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap
;;; gtk_label_get_line_wrap
;;; ----------------------------------------------------------------------------

(defun (setf label-line-wrap) (wrap label)
  (cffi:foreign-funcall "gtk_label_set_line_wrap"
                        (g:object label) label
                        :boolean wrap
                        :void)
  wrap)

(cffi:defcfun ("gtk_label_get_line_wrap" label-line-wrap) :boolean
 #+liber-documentation
 "@version{2025-06-17}
  @syntax{(gtk:label-line-wrap label) => wrap}
  @syntax{(setf (gtk:label-line-wrap label) wrap)}
  @argument[label]{a @class{gtk:label} widget}
  @argument[wrap]{a boolean whether the lines of the label are automatically
    wrapped}
  @begin{short}
    The @fun{gtk:label-line-wrap} function returns whether lines in the label
    are automatically wrapped.
  @end{short}
  The @setf{gtk:label-line-wrap} function toggles line wrapping of the label.

  @em{True} makes it break lines if text exceeds the size of the widget.
  @em{False} lets the text get cut off by the edge of the widget if it exceeds
  the widget size.

  Note that setting line wrapping to @em{true} does not make the label wrap at
  the width of the parent container, because GTK widgets conceptually cannot
  make their requisition depend on the size of the parent container. For a label
  that wraps at a specific position, set the width of the label using the
  @fun{gtk:widget-size-request} function.
  @see-class{gtk:label}
  @see-function{gtk:widget-size-request}"
  (label (g:object label)))

(export 'label-line-wrap)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_line_wrap_mode
;;; gtk_label_get_line_wrap_mode
;;; ----------------------------------------------------------------------------

(defun (setf label-line-wrap-mode) (wrap-mode label)
  (cffi:foreign-funcall "gtk_label_set_line_wrap_mode"
                        (g:object label) label
                        pango:wrap-mode wrap-mode
                        :void)
  wrap-mode)

(cffi:defcfun ("gtk_label_get_line_wrap_mode" label-line-wrap-mode)
    pango:wrap-mode
 #+liber-documentation
 "@version{2025-06-28}
  @syntax{(gtk:label-line-wrap-mode label) => mode}
  @syntax{(setf (gtk:label-line-wrap-mode label) mode)}
  @argument[label]{a @class{gtk:label} widget}
  @argument[mode]{a @sym{pango:wrap-mode} value for the line wrapping mode}
  @begin{short}
    The @fun{gtk:label-line-wrap-mode} function returns the line wrap mode used
    by the label.
  @end{short}
  The @setf{gtk:label-line-wrap-mode} function sets the line wrap mode of the
  label.

  If line wrapping is on this controls how the line wrapping is done. The
  default is @val[pango:wrap-mode]{:word} which means wrap on word boundaries.
  @see-class{gtk:label}
  @see-symbol{pango:wrap-mode}
  @see-function{gtk:label-line-wrap}"
  (label (g:object label)))

(export 'label-line-wrap-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout_offsets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_layout_offsets" %label-get-layout-offsets) :void
  (label (g:object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun label-layout-offsets (label)
 #+liber-documentation
 "@version{2025-06-28}
  @syntax{(gtk:label-layout-offsets label) => x, y}
  @argument[label]{a @class{gtk:label} widget}
  @argument[x]{an integer for the x offset}
  @argument[y]{an integer for the y offset}
  @begin{short}
    Obtains the coordinates where the label will draw the @class{pango:layout}
    object representing the text in the label.
  @end{short}
  This is useful to convert mouse events into coordinates inside the
  @class{pango:layout} object, for example, to take some action if some part of
  the label is clicked.

  Of course you will need to create a @class{gtk:event-box} widget to receive
  the events, and pack the label inside it, since labels are windowless. Labels
  return @em{false} from the @fun{gtk:widget-has-window} function. Remember
  when using the @class{pango:layout} functions you need to convert to and from
  pixels using the @fun{pango:pixels} function or the @var{pango:+scale+}
  constant.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:label-layout-offsets (make-instance 'gtk:label))
=> 0
=> -9
(gtk:label-layout-offsets (make-instance 'gtk:label :label \"text\"))
=> -14
=> -9
    @end{pre}
  @end{dictionary}
  @see-class{gtk:label}
  @see-class{gtk:event-box}
  @see-class{pango:layout}
  @see-variable{pango:+scale+}
  @see-function{pango:pixels}
  @see-function{gtk:widget-has-window}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%label-get-layout-offsets label x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'label-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_label_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_new_with_mnemonic" label-new-with-mnemonic)
    (g:object widget)
 #+liber-documentation
 "@version{2025-06-17}
  @argument[text]{a string for the text of the label, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:label} widget.}
  @begin{short}
    Creates a new @class{gtk:label} widget, containing the given.
  @end{short}
  If characters in @arg{text} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use '__'
  (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. The mnemonic key can be used to activate
  another widget, chosen automatically, or explicitly using the
  @fun{gtk:label-mnemonic-widget} function.

  If the @fun{gtk:label-mnemonic-widget} function is not called, then the first
  activatable ancestor of the @class{gtk:label} widget will be chosen as the
  mnemonic widget. For instance, if the label is inside a button or menu item,
  the button or menu item will automatically become the mnemonic widget and be
  activated by the mnemonic.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (text :string))

(export 'label-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_select_region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_select_region" label-select-region) :void
 #+liber-documentation
 "@version{2025-06-17}
  @argument[label]{a @class{gtk:label} widget}
  @argument[start]{an integer for the start offset, in characters not bytes}
  @argument[end]{an integer for the end offset, in characters not bytes}
  @begin{short}
    Selects a range of characters in the label, if the label is selectable.
  @end{short}
  See the @fun{gtk:label-selectable} function. If the label is not
  selectable, this function has no effect. If @arg{start} or @arg{end} are -1,
  then the end of the label will be substituted.
  @see-class{gtk:label}
  @see-function{gtk:label-selectable}"
  (label (g:object label))
  (start :int)
  (end :int))

(export 'label-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text_with_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_set_text_with_mnemonic" label-set-text-with-mnemonic)
    :void
 #+liber-documentation
 "@version{2023-03-05}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a string for the label}
  @begin{short}
    Sets the text of the label from the string @arg{text}.
  @end{short}
  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic. The
  mnemonic key can be used to activate another widget, chosen automatically, or
  explicitly using the @fun{gtk:label-mnemonic-widget} function.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (label (g:object label))
  (text :string))

(export 'label-set-text-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_layout" label-layout) (g:object pango:layout)
 #+liber-documentation
 "@version{2025-06-28}
  @argument[label]{a @class{gtk:label} widget}
  @return{The @class{pango:layout} object for this label.}
  @begin{short}
    Gets the Pango layout used to display the label.
  @end{short}
  The layout is useful to, for example, convert text positions to pixel
  positions, in combination with the @fun{gtk:label-layout-offsets} function.
  The label is free to recreate its layout at any time, so it should be
  considered read-only.
  @see-class{gtk:label}
  @see-class{pango:layout}
  @see-function{gtk:label-layout-offsets}"
  (label (g:object label)))

(export 'label-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selection_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_selection_bounds" %label-get-selection-bounds)
    :boolean
  (label (g:object label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun label-selection-bounds (label)
 #+liber-documentation
 "@version{2025-06-17}
  @syntax{(gtk:label-selection-bounds label) => start, end}
  @argument[label]{a @class{gtk:label} widget}
  @argument[start]{an integer for the start of selection, as a character offset}
  @argument[end]{an integer for the end of selection, as a character offset}
  @begin{short}
    Gets the selected range of characters in the label.
  @end{short}
  @see-class{gtk:label}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (when (%label-get-selection-bounds label start end)
      (values (cffi:mem-ref start :int)
              (cffi:mem-ref end :int)))))

(export 'label-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_current_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_current_uri" label-current-uri) :string
 #+liber-documentation
 "@version{2025-07-02}
  @argument[label]{a @class{gtk:label} widget}
  @begin{return}
    The string with the currently active URI.
  @end{return}
  @begin{short}
    Returns the URI for the currently active link in the label.
  @end{short}
  The active link is the one under the mouse pointer or, in a selectable label,
  the link in which the text cursor is currently positioned.

  This function is intended for use in a @sig[gtk:label]{activate-link} signal
  handler or for use in a @sig[gtk:widget]{query-tooltip} signal handler.
  @see-class{gtk:label}"
  (label (g:object label)))

(export 'label-current-uri)

;;; --- End of file gtk3.label.lisp --------------------------------------------
