;;; ----------------------------------------------------------------------------
;;; gtk3.entry.lisp
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
;;; GtkEntry
;;;
;;;     A single line text entry field
;;;
;;; Types and Values
;;;
;;;     GtkEntry
;;;     GtkEntryIconPosition
;;;     GtkInputPurpose
;;;     GtkInputHints
;;;
;;; Functions
;;;
;;;     gtk_entry_new
;;;     gtk_entry_new_with_buffer
;;;     gtk_entry_get_buffer                               Accessor
;;;     gtk_entry_set_buffer                               Accessor
;;;     gtk_entry_set_text                                 Accessor
;;;     gtk_entry_get_text                                 Accessor
;;;     gtk_entry_get_text_length                          Accessor
;;;     gtk_entry_get_text_area
;;;     gtk_entry_set_visibility                           Accessor
;;;     gtk_entry_set_invisible_char                       Accessor
;;;     gtk_entry_unset_invisible_char
;;;     gtk_entry_set_max_length                           Accessor
;;;     gtk_entry_get_activates_default                    Accessor
;;;     gtk_entry_get_has_frame                            Accessor
;;;     gtk_entry_get_inner_border                         Accessor
;;;     gtk_entry_get_width_chars                          Accessor
;;;     gtk_entry_get_max_width_chars                      Accessor
;;;     gtk_entry_set_activates_default                    Accessor
;;;     gtk_entry_set_has_frame                            Accessor
;;;     gtk_entry_set_inner_border                         Accessor
;;;     gtk_entry_set_width_chars                          Accessor
;;;     gtk_entry_set_max_width_chars                      Accessor
;;;     gtk_entry_get_invisible_char                       Accessor
;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment
;;;     gtk_entry_set_placeholder_text                     Accessor
;;;     gtk_entry_get_placeholder_text                     Accessor
;;;     gtk_entry_set_overwrite_mode                       Accessor
;;;     gtk_entry_get_overwrite_mode                       Accessor
;;;     gtk_entry_get_layout
;;;     gtk_entry_get_layout_offsets
;;;     gtk_entry_layout_index_to_text_index
;;;     gtk_entry_text_index_to_layout_index
;;;     gtk_entry_set_attributes                           Accessor
;;;     gtk_entry_get_attributes                           Accessor
;;;     gtk_entry_get_max_length                           Accessor
;;;     gtk_entry_get_visibility                           Accessor
;;;     gtk_entry_set_completion                           Accessor
;;;     gtk_entry_get_completion                           Accessor
;;;     gtk_entry_set_cursor_hadjustment
;;;     gtk_entry_get_cursor_hadjustment
;;;     gtk_entry_set_progress_fraction                    Accessor
;;;     gtk_entry_get_progress_fraction                    Accessor
;;;     gtk_entry_set_progress_pulse_step                  Accessor
;;;     gtk_entry_get_progress_pulse_step                  Accessor
;;;     gtk_entry_progress_pulse
;;;     gtk_entry_im_context_filter_keypress
;;;     gtk_entry_reset_im_context
;;;     gtk_entry_get_tabs                                 Accessor
;;;     gtk_entry_set_tabs                                 Accessor
;;;     gtk_entry_set_icon_from_pixbuf
;;;     gtk_entry_set_icon_from_stock
;;;     gtk_entry_set_icon_from_icon_name
;;;     gtk_entry_set_icon_from_gicon
;;;     gtk_entry_get_icon_storage_type
;;;     gtk_entry_get_icon_pixbuf
;;;     gtk_entry_get_icon_stock
;;;     gtk_entry_get_icon_name
;;;     gtk_entry_get_icon_gicon
;;;     gtk_entry_set_icon_activatable
;;;     gtk_entry_get_icon_activatable
;;;     gtk_entry_set_icon_sensitive
;;;     gtk_entry_get_icon_sensitive
;;;     gtk_entry_get_icon_at_pos
;;;     gtk_entry_set_icon_tooltip_text
;;;     gtk_entry_get_icon_tooltip_text
;;;     gtk_entry_set_icon_tooltip_markup
;;;     gtk_entry_get_icon_tooltip_markup
;;;     gtk_entry_set_icon_drag_source
;;;     gtk_entry_get_current_icon_drag_source
;;;     gtk_entry_get_icon_area
;;;     gtk_entry_set_input_purpose                        Accessor
;;;     gtk_entry_get_input_purpose                        Accessor
;;;     gtk_entry_set_input_hints                          Accessor
;;;     gtk_entry_get_input_hints                          Accessor
;;;     gtk_entry_grab_focus_without_selecting
;;;
;;; Properties
;;;
;;;     activates-default
;;;     attributes
;;;     buffer
;;;     caps-lock-warning
;;;     completion
;;;     cursor-position
;;;     editable
;;;     enable-emoji-completion
;;;     has-frame
;;;     im-module
;;;     inner-border
;;;     input-hints
;;;     input-purpose
;;;     invisible-char
;;;     invisible-char-set
;;;     max-length
;;;     max-width-chars
;;;     overwrite-mode
;;;     placeholder-text
;;;     populate-all
;;;     primary-icon-activatable
;;;     primary-icon-gicon
;;;     primary-icon-name
;;;     primary-icon-pixbuf
;;;     primary-icon-sensitive
;;;     primary-icon-stock
;;;     primary-icon-storage-type
;;;     primary-icon-tooltip-markup
;;;     primary-icon-tooltip-text
;;;     progress-fraction
;;;     progress-pulse-step
;;;     scroll-offset
;;;     secondary-icon-activatable
;;;     secondary-icon-gicon
;;;     secondary-icon-name
;;;     secondary-icon-pixbuf
;;;     secondary-icon-sensitive
;;;     secondary-icon-stock
;;;     secondary-icon-storage-type
;;;     secondary-icon-tooltip-markup
;;;     secondary-icon-tooltip-text
;;;     selection-bound
;;;     shadow-type
;;;     show-emoji-icon
;;;     tabs
;;;     text
;;;     text-length
;;;     truncate-multiline
;;;     visibility
;;;     width-chars
;;;     xalign
;;;
;;; Style Properties
;;;
;;;     icon-prelight
;;;     inner-border
;;;     invisible-char
;;;     progress-border
;;;
;;; Signals
;;;
;;;     activate
;;;     backspace
;;;     copy-clipboard
;;;     cut-clipboard
;;;     delete-from-cursor
;;;     icon-press
;;;     icon-release
;;;     insert-at-cursor
;;;     insert-emoji
;;;     move-cursor
;;;     paste-clipboard
;;;     populate-popup
;;;     preedit-changed
;;;     toggle-overwrite
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkEntry
;;;                 ├── GtkSearchEntry
;;;                 ╰── GtkSpinButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkEntry implements AtkImplementorIface, GtkBuildable, GtkEditable and
;;;     GtkCellEditable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEntryIconPosition
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkEntryIconPosition" entry-icon-position
  (:export t
   :type-initializer "gtk_entry_icon_position_get_type")
  (:primary 0)
  (:secondary 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'entry-icon-position)
      "GEnum"
      (liber:symbol-documentation 'entry-icon-position)
 "@version{2024-3-21}
  @begin{declaration}
(gobject:define-g-enum \"GtkEntryIconPosition\" entry-icon-position
  (:export t
   :type-initializer \"gtk_entry_icon_position_get_type\")
  (:primary 0)
  (:secondary 1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:primary]{At the beginning of the entry, depending on the text
        direction.}
      @entry[:secondary]{At the end of the entry, depending on the text
        direction.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies the side of the entry at which an icon is placed.
  @end{short}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; GtkInputPurpose
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkInputPurpose" input-purpose
  (:export t
   :type-initializer "gtk_input_purpose_get_type")
  (:free-form 0)
  (:alpha 1)
  (:digits 2)
  (:number 3)
  (:phone 4)
  (:url 5)
  (:email 6)
  (:name 7)
  (:password 8)
  (:pin 9))

#+liber-documentation
(setf (liber:alias-for-symbol 'input-purpose)
      "GEnum"
      (liber:symbol-documentation 'input-purpose)
 "@version{2024-3-21}
  @begin{declaration}
(gobject:define-g-enum \"GtkInputPurpose\" input-purpose
  (:export t
   :type-initializer \"gtk_input_purpose_get_type\")
  (:free-form 0)
  (:alpha 1)
  (:digits 2)
  (:number 3)
  (:phone 4)
  (:url 5)
  (:email 6)
  (:name 7)
  (:password 8)
  (:pin 9))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:free-form]{Allow any character.}
      @entry[:alpha]{Allow only alphabetic characters.}
      @entry[:digits]{Allow only digits.}
      @entry[:number]{Edited field expects numbers.}
      @entry[:phone]{Edited field expects phone number.}
      @entry[:url]{Edited field expects URL.}
      @entry[:email]{Edited field expects email address.}
      @entry[:name]{Edited field expects the name of a person.}
      @entry[:password]{Like @code{:free-form}, but characters are hidden.}
      @entry[:pin]{Like @code{:digits}, but characters are hidden.}
    @end{table}
  @end{values}
  @begin{short}
    Describes primary purpose of the input widget.
  @end{short}
  This information is useful for on-screen keyboards and similar input methods
  to decide which keys should be presented to the user.

  Note that the purpose is not meant to impose a totally strict rule about
  allowed characters, and does not replace input validation. It is fine for an
  on-screen keyboard to let the user override the character set restriction
  that is expressed by the purpose. The application is expected to validate
  the entry contents, even if it specified a purpose.

  The difference between the @code{:digits} and @code{:number} values is that
  the former accepts only digits while the latter also some punctuation, like
  commas or points, plus, minus, and 'e' or 'E' as in 3.14E+000.

  This enumeration may be extended in the future. Input methods should
  interpret unknown values as 'free form'.
  @see-class{gtk:entry}
  @see-symbol{gtk:input-hints}")

;;; ----------------------------------------------------------------------------
;;; enum GtkInputHints
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GtkInputHints" input-hints
  (:export t
   :type-initializer "gtk_input_hints_get_type")
  (:none 0)
  (:spellcheck          #.(ash 1 0))
  (:no-spellcheck       #.(ash 1 1))
  (:word-completion     #.(ash 1 2))
  (:lowercase           #.(ash 1 3))
  (:uppercase-chars     #.(ash 1 4))
  (:uppercase-words     #.(ash 1 5))
  (:uppercase-sentences #.(ash 1 6))
  (:inhibit-osk         #.(ash 1 7))
  (:vertical-writing    #.(ash 1 8))
  (:emoji               #.(ash 1 9))
  (:no-emoji            #.(ash 1 10)))

#+liber-documentation
(setf (liber:alias-for-symbol 'input-hints)
      "GFlags"
      (liber:symbol-documentation 'input-hints)
 "@version{2024-3-21}
  @begin{declaration}
(gobject:define-g-flags \"GtkInputHints\" input-hints
  (:export t
   :type-initializer \"gtk_input_hints_get_type\")
  (:none 0)
  (:spellcheck          #.(ash 1 0))
  (:no-spellcheck       #.(ash 1 1))
  (:word-completion     #.(ash 1 2))
  (:lowercase           #.(ash 1 3))
  (:uppercase-chars     #.(ash 1 4))
  (:uppercase-words     #.(ash 1 5))
  (:uppercase-sentences #.(ash 1 6))
  (:inhibit-osk         #.(ash 1 7))
  (:vertical-writing    #.(ash 1 8))
  (:emoji               #.(ash 1 9))
  (:no-emoji            #.(ash 1 10)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No special behaviour suggested.}
      @entry[:spellcheck]{Suggest checking for typos.}
      @entry[:no-spellcheck]{Suggest not checking for typos.}
      @entry[:word-completion]{Suggest word completion.}
      @entry[:lowercase]{Suggest to convert all text to lowercase.}
      @entry[:uppercase-chars]{Suggest to capitalize all text.}
      @entry[:uppercase-words]{Suggest to capitalize the first character of each
        word.}
      @entry[:uppercase-sentences]{Suggest to capitalize the first word of each
        sentence.}
      @entry[:inhibit-osk]{Suggest to not show an onscreen keyboard, e.g. for a
        calculator that already has all the keys.}
      @entry[:vertical-writing]{The text is vertical.}
      @entry[:emoji]{Suggest offering Emoji support.}
      @entry[:no-emoji]{Suggest not offering Emoji support.}
    @end{table}
  @end{values}
  @begin{short}
    Describes hints that might be taken into account by input methods or
    applications.
  @end{short}
  Note that input methods may already tailor their behaviour according to the
  @symbol{gtk:input-purpose} value of the entry.

  Some common sense is expected when using these flags - mixing
  @code{:lowercase} with any of the uppercase hints makes no sense.

  This flags may be extended in the future. Input methods should ignore
  unknown values.
  @see-class{gtk:entry}
  @see-symbol{gtk:input-purpose}")

;;; ----------------------------------------------------------------------------
;;; struct GtkEntry
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEntry" entry
  (:superclass widget
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkEditable"
                "GtkCellEditable")
   :type-initializer "gtk_entry_get_type")
  ((activates-default
    entry-activates-default
    "activates-default" "gboolean" t t)
   (attributes
    entry-attributes
    "attributes" "PangoAttrlist" t t)
   (buffer
    entry-buffer
    "buffer" "GtkEntryBuffer" t t)
   (caps-lock-warning
    entry-caps-lock-warning
    "caps-lock-warning" "gboolean" t t)
   (completion
    entry-completion
    "completion" "GtkEntryCompletion" t t)
   (cursor-position
    entry-cursor-position
    "cursor-position" "gint" t nil)
   (editable
    entry-editable
    "editable" "gboolean" t t)
   (enable-emoji-completion
    entry-enable-emoji-completion
    "enable-emoji-completion" "gboolean" t t)
   (has-frame
    entry-has-frame
    "has-frame" "gboolean" t t)
   (im-module
    entry-im-module
    "im-module" "gchararray" t t)
   (inner-border
    entry-inner-border
    "inner-border" "GtkBorder" t t)
   (input-hints
    entry-input-hints
    "input-hints" "GtkInputHints" t t)
   (input-purpose
    entry-input-purpose
    "input-purpose" "GtkInputPurpose" t t)
   (invisible-char
    entry-invisible-char
    "invisible-char" "guint" t t)
   (invisible-char-set
    entry-invisible-char-set
    "invisible-char-set" "gboolean" t t)
   (max-length
    entry-max-length
    "max-length" "gint" t t)
   (max-width-chars
    entry-max-width-chars
    "max-width-chars" "gint" t t)
   (overwrite-mode
    entry-overwrite-mode
    "overwrite-mode" "gboolean" t t)
   (placeholder-text
    entry-placeholder-text
    "placeholder-text" "gchararray" t t)
   (populate-all
    entry-populate-all
    "populate-all" "gboolean" t t)
   (primary-icon-activatable
    entry-primary-icon-activatable
    "primary-icon-activatable" "gboolean" t t)
   (primary-icon-gicon
    entry-primary-icon-gicon
    "primary-icon-gicon" "GIcon" t t)
   (primary-icon-name
    entry-primary-icon-name
    "primary-icon-name" "gchararray" t t)
   (primary-icon-pixbuf
    entry-primary-icon-pixbuf
    "primary-icon-pixbuf" "GdkPixbuf" t t)
   (primary-icon-sensitive
    entry-primary-icon-sensitive
    "primary-icon-sensitive" "gboolean" t t)
   (primary-icon-stock
    entry-primary-icon-stock
    "primary-icon-stock" "gchararray" t t)
   (primary-icon-storage-type
    entry-primary-icon-storage-type
    "primary-icon-storage-type" "GtkImageType" t nil)
   (primary-icon-tooltip-markup
    entry-primary-icon-tooltip-markup
    "primary-icon-tooltip-markup" "gchararray" t t)
   (primary-icon-tooltip-text
    entry-primary-icon-tooltip-text
    "primary-icon-tooltip-text" "gchararray" t t)
   (progress-fraction
    entry-progress-fraction
    "progress-fraction" "gdouble" t t)
   (progress-pulse-step
    entry-progress-pulse-step
    "progress-pulse-step" "gdouble" t t)
   (scroll-offset
    entry-scroll-offset
    "scroll-offset" "gint" t nil)
   (secondary-icon-activatable
    entry-secondary-icon-activatable
    "secondary-icon-activatable" "gboolean" t t)
   (secondary-icon-gicon
    entry-secondary-icon-gicon
    "secondary-icon-gicon" "GIcon" t t)
   (secondary-icon-name
    entry-secondary-icon-name
    "secondary-icon-name" "gchararray" t t)
   (secondary-icon-pixbuf
    entry-secondary-icon-pixbuf
    "secondary-icon-pixbuf" "GdkPixbuf" t t)
   (secondary-icon-sensitive
    entry-secondary-icon-sensitive
    "secondary-icon-sensitive" "gboolean" t t)
   (secondary-icon-stock
    entry-secondary-icon-stock
    "secondary-icon-stock" "gchararray" t t)
   (secondary-icon-storage-type
    entry-secondary-icon-storage-type
    "secondary-icon-storage-type" "GtkImageType" t nil)
   (secondary-icon-tooltip-markup
    entry-secondary-icon-tooltip-markup
    "secondary-icon-tooltip-markup" "gchararray" t t)
   (secondary-icon-tooltip-text
    entry-secondary-icon-tooltip-text
    "secondary-icon-tooltip-text" "gchararray" t t)
   (selection-bound
    entry-selection-bound
    "selection-bound" "gint" t nil)
   (shadow-type
    entry-shadow-type
    "shadow-type" "GtkShadowType" t t)
   (show-emoji-icon
    entry-show-emoji-icon
    "show-emoji-icon" "gboolean" t t)
   (tabs
    entry-tabs
    "tabs" "PangoTabArray" t t)
   (text
    entry-text
    "text" "gchararray" t t)
   (text-length
    entry-text-length
    "text-length" "guint" t nil)
   (truncate-multiline
    entry-truncate-multiline
    "truncate-multiline" "gboolean" t t)
   (visibility
    entry-visibility
    "visibility" "gboolean" t t)
   (width-chars
    entry-width-chars
    "width-chars" "gint" t t)
   (xalign
    entry-xalign
    "xalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'entry 'type)
 "@version{2023-3-4}
  @begin{short}
    The @class{gtk:entry} widget is a single line text entry widget.
  @end{short}
  A fairly large set of key bindings are supported by default. If the entered
  text is longer than the allocation of the widget, the widget will scroll so
  that the cursor position is visible.

  @image[entry]{GtkEntry}

  When using an entry for passwords and other sensitive information, it can be
  put into \"password mode\" using the @fun{gtk:entry-visibility} function. In
  this mode, entered text is displayed using a 'invisible' character. By
  default, GTK picks the best invisible character that is available in the
  current font, but it can be changed with the @fun{gtk:entry-invisible-char}
  function. GTK displays a warning when Caps Lock or input methods might
  interfere with entering text in a password entry. The warning can be turned
  off with the @slot[gtk:entry]{caps-lock-warning} property.

  The @class{gtk:entry} widget has the ability to display progress or activity
  information behind the text. To make an entry display such information, use
  the @fun{gtk:entry-progress-fraction} or @fun{gtk:entry-progress-pulse-step}
  functions.

  Additionally, the @class{gtk:entry} widget can show icons at either side of
  the entry. These icons can be activatable by clicking, can be set up as drag
  source and can have tooltips. To add an icon, use the
  @fun{gtk:entry-set-icon-from-gicon} function or one of the various other
  functions that set an icon from a stock ID, an icon name or a pixbuf. To
  trigger an action when the user clicks an icon, connect to the
  @code{\"icon-press\"} signal. To allow DND operations from an icon, use the
  @fun{gtk:entry-set-icon-drag-source} function. To set a tooltip on an icon,
  use the @fun{gtk:entry-icon-tooltip-text} function or the corresponding
  function for markup.

  Note that functionality or information that is only available by clicking on
  an icon in an entry may not be accessible at all to users which are not able
  to use a mouse or other pointing device. It is therefore recommended that
  any such functionality should also be available by other means, e.g. via the
  context menu of the entry.
  @begin[Style Property Details]{dictionary}
    @begin[code]{table}
      @begin[icon-prelight]{entry}
        The @code{icon-prelight} style property of type @code{:boolean} (Read)
        @br{}
        The prelight style property determines whether activatable icons
        prelight on mouseover. @br{}
        @em{Warning:} The @code{icon-prelight} style property has been
        deprecated since version 3.20 and should not be used in newly written
        code. Use CSS to control the appearance of prelighted icons. The value
        of this style property is ignored.
      @end{entry}
      @begin[inner-border]{entry}
        The @code{inner-border} style property of type @class{gtk:border} (Read)
        @br{}
        Sets the border of the text area between the text and the frame. @br{}
        @em{Warning:} The @code{inner-border} style property has been deprecated
        since version 3.4 and should not be used in newly written code. Use the
        standard border and padding CSS properties through objects like
        the @class{gtk:style-context} and @class{gtk:css-provider} objects. The
        value of this style property is ignored.
      @end{entry}
      @begin[invisible-char]{entry}
        The @code{invisible-char} style property of type @code{:uint} (Read)
        @br{}
        The invisible character is used when masking entry contents in
        \"password mode\". When it is not explicitly set with the
        @code{invisible-char} property, GTK determines the character to use
        from a list of possible candidates, depending on availability in the
        current font. This style property allows the theme to prepend a
        character to the list of candidates. @br{}
        Default value: 0
      @end{entry}
      @begin[progress-border]{entry}
        The @code{progress-border} style property of type @class{gtk:border}
        (Read) @br{}
        The border around the progress bar in the entry. @br{}
        @em{Warning:} The @code{progress-border} style property has been
        deprecated since version 3.4 and should not be used in newly written
        code. Use the standard margin CSS property through objects like the
        @class{gtk:style-context} and @class{gtk:css-provider} objects. The
        value of this style property is ignored.
      @end{entry}
    @end{table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user activates the entry.
      Applications should not connect to it, but may emit it with the
      @fun{g:signal-emit} function if they need to control activation
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"backspace\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user asks for it. The
      default bindings for this signal are the @kbd{Backspace} and
      @kbd{Shift-Backspace} keys.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted to copy the selection to the
      clipboard. The default bindings for this signal are the @kbd{Ctrl-c} and
      @kbd{Ctrl-Insert} keys.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
      @end{table}
    @subheading{The \"cut-clipboard\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted to cut the selection to the
      clipboard. The default bindings for this signal are the @kbd{Ctrl-x} and
      @kbd{Shift-Delete} keys.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
      @end{table}
    @subheading{The \"delete-from-cursor\" signal}
      @begin{pre}
lambda (entry type count)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user initiates a text
      deletion. If the type is @code{:chars} of the @symbol{gtk:delete-type}
      enumeration, GTK deletes the selection if there is one, otherwise it
      deletes the requested number of characters. The default bindings for this
      signal are the @kbd{Delete} key for deleting a character and the
      @kbd{Ctrl-Delete} key for deleting a word.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
        @entry[type]{The granularity of the deletion, as a value of the
          @symbol{gtk:delete-type} enumeration.}
        @entry[count]{An integer with the number of type units to delete.}
      @end{table}
    @subheading{The \"icon-press\" signal}
      @begin{pre}
lambda (entry pos event)    :run-last
      @end{pre}
      The signal is emitted when an activatable icon is clicked.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
        @entry[pos]{The position of the clicked icon as a value of the
          @symbol{gtk:entry-icon-position} enumeration.}
        @entry[event]{The @class{gdk:event} button press event.}
      @end{table}
    @subheading{The \"icon-release\" signal}
      @begin{pre}
lambda (entry pos event)    :run-last
      @end{pre}
      The signal is emitted on the button release from a mouse click over an
      activatable icon.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
        @entry[pos]{The position of the clicked icon as a value of the
          @symbol{gtk:entry-icon-position} enumeration.}
        @entry[event]{The @class{gdk:event} button release event.}
      @end{table}
    @subheading{The \"insert-at-cursor\" signal}
      @begin{pre}
lambda (entry string)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user initiates the
      insertion of a fixed string at the cursor. The signal has no default
      bindings.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
        @entry[string]{The string to insert.}
      @end{table}
    @subheading{The \"insert-emoji\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted to present the Emoji chooser for
      the entry. The default bindings for this signal are the @kbd{Ctrl-.} and
      @kbd{Ctrl-;} keys.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (entry step count extend)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user initiates a cursor
      movement. If the cursor is not visible in the entry, this signal causes
      the viewport to be moved instead. Applications should not connect to it,
      but may emit it with the @fun{g:signal-emit} function if they need to
      control the cursor programmatically. The default bindings for this signal
      come in two variants, the variant with the @kbd{Shift} modifier extends
      the selection, the variant without the @kbd{Shift} modifier does not.
      There are too many key combinations to list them all here.
      @begin{itemize}
        @item{Arrow keys move by individual characters/lines.}
        @item{@kbd{Ctrl}-arrow key combinations move by words/paragraphs.}
        @item{@kbd{Home}/@kbd{End} keys move to the ends of the buffer.}
      @end{itemize}
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk:movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{table}
    @subheading{The \"paste-clipboard\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted to paste the contents of the
      clipboard into the text view. The default bindings for this signal are
      the @kbd{Ctrl-v} and @kbd{Shift-Insert} keys.
      @begin[code]{table}
        @entry[entry]{The @ckass{gtk:entry} widget which received the signal.}
      @end{table}
    @subheading{The \"populate-popup\" signal}
      @begin{pre}
lambda (entry widget)    :run-last
      @end{pre}
      The signal gets emitted before showing the context menu of the entry. If
      you need to add items to the context menu, connect to this signal and
      append your items to the widget, which will be a @class{gtk:menu} widget
      in this case. If the @code{populate-all} property is @em{true}, this
      signal will also be emitted to populate touch popups. In this case, widget
      will be a different container, e.g. a @class{gtk:toolbar} widget. The
      signal handler should not make assumptions about the type of the widget.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
        @entry[widget]{The @class{gtk:widget} container that is being
          populated.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
lambda (entry preedit)    :action
      @end{pre}
      If an input method is used, the typed text will not immediately be
      committed to the buffer. So if you are interested in the text, connect to
      this signal.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
        @entry[preedit]{The current preedit string.}
      @end{table}
    @subheading{The \"toggle-overwrite\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      A keybinding signal which gets emitted to toggle the overwrite mode of
      the entry. The default bindings for this signal is the @kbd{Insert} key.
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:entry-new}
  @see-constructor{gtk:entry-new-with-buffer}
  @see-slot{gtk:entry-activates-default}
  @see-slot{gtk:entry-attributes}
  @see-slot{gtk:entry-buffer}
  @see-slot{gtk:entry-caps-lock-warning}
  @see-slot{gtk:entry-completion}
  @see-slot{gtk:entry-cursor-position}
  @see-slot{gtk:entry-editable}
  @see-slot{gtk:entry-has-frame}
  @see-slot{gtk:entry-im-module}
  @see-slot{gtk:entry-inner-border}
  @see-slot{gtk:entry-input-hints}
  @see-slot{gtk:entry-input-purpose}
  @see-slot{gtk:entry-invisible-char}
  @see-slot{gtk:entry-invisible-char-set}
  @see-slot{gtk:entry-max-length}
  @see-slot{gtk:entry-overwrite-mode}
  @see-slot{gtk:entry-placeholder-text}
  @see-slot{gtk:entry-populate-all}
  @see-slot{gtk:entry-primary-icon-activatable}
  @see-slot{gtk:entry-primary-icon-gicon}
  @see-slot{gtk:entry-primary-icon-name}
  @see-slot{gtk:entry-primary-icon-pixbuf}
  @see-slot{gtk:entry-primary-icon-sensitive}
  @see-slot{gtk:entry-primary-icon-stock}
  @see-slot{gtk:entry-primary-icon-storage-type}
  @see-slot{gtk:entry-primary-icon-tooltip-markup}
  @see-slot{gtk:entry-primary-icon-tooltip-text}
  @see-slot{gtk:entry-progress-fraction}
  @see-slot{gtk:entry-progress-pulse-step}
  @see-slot{gtk:entry-scroll-offset}
  @see-slot{gtk:entry-secondary-icon-activatable}
  @see-slot{gtk:entry-secondary-icon-gicon}
  @see-slot{gtk:entry-secondary-icon-name}
  @see-slot{gtk:entry-secondary-icon-pixbuf}
  @see-slot{gtk:entry-secondary-icon-sensitive}
  @see-slot{gtk:entry-secondary-icon-stock}
  @see-slot{gtk:entry-secondary-icon-storage-type}
  @see-slot{gtk:entry-secondary-icon-tooltip-markup}
  @see-slot{gtk:entry-secondary-icon-tooltip-text}
  @see-slot{gtk:entry-selection-bound}
  @see-slot{gtk:entry-shadow-type}
  @see-slot{gtk:entry-tabs}
  @see-slot{gtk:entry-text}
  @see-slot{gtk:entry-text-length}
  @see-slot{gtk:entry-truncate-multiline}
  @see-slot{gtk:entry-visibility}
  @see-slot{gtk:entry-width-chars}
  @see-slot{gtk:entry-xalign}
  @see-class{gtk:text-view}
  @see-class{gtk:entry-completion}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:entry-activates-default --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activates-default" 'entry) t)
 "The @code{activates-default} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to activate the default widget, such as the default button in a
  dialog, when the @kbd{Enter} key is pressed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-activates-default)
      "Accessor"
      (documentation 'entry-activates-default 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-activates-default object) => setting}
  @syntax{(setf (gtk:entry-activates-default object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{@em{true} to activate the default widget of the window on
    @kbd{Enter} keypress}
  @begin{short}
    Accessor of the @slot[gtk:entry]{activates-default} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-activates-default} function retrieves whether to activate
  the default widget, when the @kbd{Enter} key is pressed.

  If the @arg{setting} argument is @em{true}, pressing the @kbd{Enter} key in
  the entry will activate the default widget for the window containing the
  entry. This usually means that the dialog box containing the entry will be
  closed, since the default widget is usually one of the dialog buttons.

  If the @arg{setting} argument is @em{true}, the entry calls the
  @fun{gtk:window-activate-default} function on the window containing the entry,
  in the default handler for the @code{\"activate\"} signal.
  @see-class{gtk:entry}
  @see-function{gtk:window-activate-default}")

;;; --- gtk:entry-attributes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'entry) t)
 "The @code{attributes} property of type @class{pango:attr-list} (Read / Write)
  @br{}
  A list of Pango attributes to apply to the text of the entry. This is mainly
  useful to change the size or weight of the text.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-attributes)
      "Accessor"
      (documentation 'entry-attributes 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-attributes object) => attrs}
  @syntax{(setf (gtk:entry-attributes object) attrs)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[attrs]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:entry]{attributes} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-attributes} function gets the attribute list, if any. The
  @setf{gtk:entry-attributes} function sets a attributes list. The attributes
  in the list are applied to the entry text.
  @see-class{gtk:entry}
  @see-class{pango:attr-list}")

;;; --- gtk:entry-buffer -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buffer" 'entry) t)
 "The @code{buffer} property of type @class{gtk:entry-buffer}
  (Read / Write / Construct) @br{}
  Text buffer object which actually stores entry text.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-buffer)
      "Accessor"
      (documentation 'entry-buffer 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-buffer object) => buffer}
  @syntax{(setf (gtk:entry-buffer object) buffer)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{buffer} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-buffer} function gets the entry buffer which holds the
  text for the entry. The @setf{gtk:entry-buffer} function sets the entry
  buffer.
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}")

;;; --- gtk:entry-caps-lock-warning --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "caps-lock-warning" 'entry) t)
 "The @code{caps-lock-warning} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether password entries will show a warning when the @kbd{Caps Lock} key is
  on. Note that the warning is shown using a secondary icon, and thus does not
  work if you are using the secondary icon position for some other purpose.@br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-caps-lock-warning)
      "Accessor"
      (documentation 'entry-caps-lock-warning 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-caps-lock-warning object) => setting}
  @syntax{(setf (gtk:entry-caps-lock-warning object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether password entries will show a warning when
    the @kbd{Caps Lock} key is on}
  @begin{short}
    Accessor of the @slot[gtk:entry]{caps-lock-warning} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether password entries will show a warning when the @kbd{Caps Lock} key is
  on. Note that the warning is shown using a secondary icon, and thus does not
  work if you are using the secondary icon position for some other purpose.
  @see-class{gtk:entry}")

;;; --- gtk:entry-completion ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "completion" 'entry) t)
 "The @code{completion} property of type @class{gtk:entry-completion}
  (Read / Write) @br{}
  The auxiliary completion object to use with the entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion)
      "Accessor"
      (documentation 'entry-completion 'function)
 "@version{2024-3-14}
  @syntax{(gtk:entry-completion object) => completion}
  @syntax{(setf (gtk:entry-completion object) completion)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[completion]{a @class{gtk:entry-completion} object or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:entry]{completion} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-completion} function returns the auxiliary completion
  object currently in use by the entry. The @setf{gtk:entry-completion} function
  sets the auxiliary completion object.

  All further configuration of the completion mechanism is done on
  @arg{completion} using the @class{gtk:entry-completion} API. Completion is
  disabled if the @arg{completion} argument is set to @code{nil}.
  @see-class{gtk:entry}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-cursor-position ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-position" 'entry) t)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-cursor-position)
      "Accessor"
      (documentation 'entry-cursor-position 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-cursor-position object) => position}
  @syntax{(setf (gtk:entry-cursor-position object) position)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[position]{an integer with the current position of the insertion
    cursor in chars}
  @begin{short}
    Accessor of the @slot[gtk:entry]{cursor-position} slot of the
    @class{gtk:entry} class.
  @end{short}
  The current position of the insertion cursor in chars.
  @see-class{gtk:entry}")

;;; --- gtk:entry-editable -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable" 'entry) t)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the entry contents can be edited. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-editable)
      "Accessor"
      (documentation 'entry-editable 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-editable object) => editable}
  @syntax{(setf (gtk:entry-editable object) editable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[editable]{a boolean whether the entry contents can be edited}
  @begin{short}
    Accessor of the @slot[gtk:entry]{editable} slot of the @class{gtk:entry}
    class.
  @end{short}
  Whether the entry contents can be edited.
  @see-class{gtk:entry}")

;;; --- gtk:entry-enable-emoji-completion --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-emoji-completion"
                                               'entry) t)
 "The @code{enable-emoji-completion} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to suggest Emoji replacements. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-enable-emoji-completion)
      "Accessor"
      (documentation 'entry-enable-emoji-completion 'function)
 "@version{2023-3-13}
  @syntax{(gtk:entry-enable-emoji-completion object) => enable}
  @syntax{(setf (gtk:entry-enable-emoji-completion object) enable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[enable]{a boolean whether to suggest Emoji replacements}
  @begin{short}
    Accessor of the @slot[gtk:entry]{enable-emoji-completion} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether to suggest Emoji replacements.
  @see-class{gtk:entry}")

;;; --- gtk:entry-has-frame ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'entry) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  @em{False} removes outside bevel from the entry. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-has-frame)
      "Accessor"
      (documentation 'entry-has-frame 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-has-frame object) => setting}
  @syntax{(setf (gtk:entry-has-frame object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether to remove bevel from the entry}
  @begin{short}
    Accessor of the @slot[gtk:entry]{has-frame} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-has-frame} function returns whether the entry has a beveled
  frame. The @setf{gtk:entry-has-frame} function sets whether the entry has a
  beveled frame around it.
  @see-class{gtk:entry}")

;;; --- gtk:entry-im-module ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "im-module" 'entry) t)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM input method module should be used for this entry. See the
  @class{gtk:im-context} documentation. Setting this to a non-@code{nil} value
  overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} setting. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-im-module)
      "Accessor"
      (documentation 'entry-im-module 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-im-module object) => setting}
  @syntax{(setf (gtk:entry-im-module object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a string which IM input method module should be used for
    the entry}
  @begin{short}
    Accessor of the @slot[gtk:entry]{im-module} slot of the @class{gtk:entry}
    class.
  @end{short}
  Which IM input method module should be used for this entry. See the
  @class{gtk:im-context} documentation. Setting this to a non-@code{nil} value
  overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} setting.
  @see-class{gtk:entry}
  @see-class{gtk:im-context}
  @see-function{gtk:settings-gtk-im-module}")

;;; --- gtk:entry-inner-border -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inner-border" 'entry) t)
 "The @code{inner-border} property of type @class{gtk:border} (Read / Write)
  @br{}
  Sets the border of the text area between the text and the frame. @br{}
  @em{Warning:} The @code{inner-border} property has been deprecated since
  version 3.4 and should not be used in newly written code. Use the standard
  border and padding CSS properties through objects like the
  @class{gtk:style-context} and @class{gtk:css-provider} objects. The value of
  this style property is ignored.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-inner-border)
      "Accessor"
      (documentation 'entry-inner-border 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-inner-border object) => border}
  @syntax{(setf (gtk:entry-inner-border object) border)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[border]{a @class{gtk:border} instance, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:entry]{inner-border} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-inner-border} function returns the
  @slot[gtk:entry]{inner-border} property. The @setf{gtk:entry-inner-border}
  function sets the @slot[gtk:entry]{inner-border} property to @arg{border}, or
  clears it if @code{nil} is passed. The inner border is the area around the
  text of the entry, but inside its frame.

  If set, this property overrides the @code{inner-border} style property.
  Overriding the style-provided border is useful when you want to do in-place
  editing of some text in a canvas or list widget, where pixel-exact positioning
  of the entry is important.
  @begin[Warning]{dictionary}
    The @fun{gtk:entry-inner-border} function has been deprecated since version
    3.4 and should not be used in newly written code. Use the standard border
    and padding CSS properties through objects like the
    @class{gtk:style-context} and @class{gtk:css-provider} objects. The value
    returned by this function is ignored by the entry.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-class{gtk:border}
  @see-class{gtk:style-context}
  @see-class{gtk:css-provider}")

;;; --- gtk:entry-input-hints --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-hints" 'entry) t)
 "The @code{input-hints} property of type @symbol{gtk:input-hints}
  (Read / Write) @br{}
  Additional hints, beyond the @code{input-purpose} property, that allow
  input methods to fine-tune their behaviour.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-input-hints)
      "Accessor"
      (documentation 'entry-input-hints 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-input-hints object) => hints}
  @syntax{(setf (gtk:entry-input-hints object) hints)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[hints]{a value of the @symbol{gtk:input-hints} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{input-hints} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-input-hints} function gets the value of the
  @slot[gtk:entry]{input-hints} property. The @setf{gtk:entry-input-hints}
  function sets the @slot[gtk:entry]{input-hints} property, which allows input
  methods to fine-tune their behaviour.
  @see-class{gtk:entry}
  @see-symbol{gtk:input-hints}")

;;; --- gtk:entry-input-purpose ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-purpose" 'entry) t)
 "The @code{input-purpose} property of type @symbol{gtk:input-purpose}
  (Read / Write) @br{}
  The purpose of this text field. This property can be used by on-screen
  keyboards and other input methods to adjust their behaviour. Note that setting
  the purpose to the @code{:password} or @code{:pin} values is independent from
  setting the @code{visibility} property. @br{}
  Default value: @code{:free-form}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-input-purpose)
      "Accessor"
      (documentation 'entry-input-purpose 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-input-purpose object) => purpose}
  @syntax{(setf (gtk:entry-input-purpose object) purpose)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[purpose]{a value of the @symbol{gtk:input-purpose} enumeration}
  @begin{short}
    Accessor of the slot @slot[gtk:entry]{input-purpose} of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-input-purpose} function gets the value of the
  @slot[gtk:entry]{input-purpose} property. The
  @setf{gtk:entry-input-purpose} function sets the
  @slot[gtk:entry]{input-purpose} property which can be used by on-screen
  keyboards and other input methods to adjust their behaviour.
  @see-class{gtk:entry}
  @see-symbol{gtk:input-purpose}")

;;; --- gtk:entry-invisible-char -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-char" 'entry) t)
 "The @code{invisible-char} property of type @code{:uint} (Read / Write) @br{}
  The invisible character is used when masking entry contents in \"password
  mode\". When it is not explicitly set with the @code{invisible-char} property,
  GTK determines the character to use from a list of possible candidates,
  depending on availability in the current font. This style property allows the
  theme to prepend a character to the list of candidates. @br{}
  Default value: \"*\"")

#+liber-documentation
(setf (liber:alias-for-function 'entry-invisible-char)
      "Accessor"
      (documentation 'entry-invisible-char 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-invisible-char object) => char}
  @syntax{(setf (gtk:entry-invisble-char object) char)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[char]{a Unicode character}
  @begin{short}
    Accessor of the @slot[gtk:entry]{invisible-char} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-invisible-char} function retrieves the character displayed
  in place of the real characters for entries with visibility set to @em{false}.
  The @setf{gtk:entry-invisible-char} function sets the character to use in
  place of the actual text when the @fun{gtk:entry-visibility} function has
  been called to set text visibility to @em{false}.

  I.e. this is the character used in \"password mode\" to show the user how
  many characters have been typed. By default, GTK picks the best invisible
  char available in the current font. If you set the invisible char to 0, then
  the user will get no feedback at all. There will be no text on the screen as
  they type.
  @see-class{gtk:entry}
  @see-function{gtk:entry-visibility}
  @see-function{gtk:entry-unset-invisible-char}")

;;; --- gtk:entry-invisible-char-set -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-char-set" 'entry) t)
 "The @code{invisible-char-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the invisible char has been set for the entry. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-invisible-char-set)
      "Accessor"
      (documentation 'entry-invisible-char-set 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-invisible-char-set object) => setting}
  @syntax{(setf (gtk:entry-invisible-char-set object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether the invisible char has been set for the
    entry}
  @begin{short}
    Accessor of the @slot[gtk:entry]{invisible-char-set} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the invisible char has been set for the entry.
  @see-class{gtk:entry}")

;;; --- gtk:entry-max-length ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-length" 'entry) t)
 "The @code{max-length} property of type @code{:int} (Read / Write) @br{}
  Maximum number of characters for this entry. Zero if no maximum. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-max-length)
      "Accessor"
      (documentation 'entry-max-length 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-max-length object) => max}
  @syntax{(setf (gtk:entry-max-length object) max)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[max]{an integer with the maximum length of the entry, or 0 for no
    maximum, the value passed in will be clamped to the range [0, 65536]}
  @begin{short}
    Accessor of the @slot[gtk:entry]{max-length} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-max-length} function retrieves the maximum allowed length
  of the text in the entry, or 0 if there is no maximum. This is equivalent to:
  @begin{pre}
(gtk:entry-buffer-max-length (gtk:entry-buffer object))
  @end{pre}
  The @setf{gtk:entry-max-length} function sets the maximum allowed length of
  the contents of the widget. If the current contents are longer than the given
  length, then they will be truncated to fit. This is equivalent to
  @begin{pre}
(setf (gtk:entry-buffer-max-length (gtk:entry-buffer object)) max)
  @end{pre}
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-max-length}")

;;; --- gtk:entry-max-width-chars ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width-chars" 'entry) t)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the entry, in characters. If this property is
  set to -1, the width will be calculated automatically. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'entry-max-width-chars)
      "Accessor"
      (documentation 'entry-max-width-chars 'function)
 "@version{2023-3-13}
  @syntax{(gtk:entry-max-width-chars object) => n-chars}
  @syntax{(setf (gtk:entry-max-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:entry} object}
  @argument[n-chars]{an integer with the maximum width, in characters}
  @begin{short}
    Accessor of the @slot[gtk:entry]{max-width-chars} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-max-width-chars} function retrieves the desired maximum
  width of the entry, in characters. The @setf{gtk:entry-max-width-chars}
  function sets the desired maximum width.
  @see-class{gtk:entry}")

;;; --- gtk:entry-overwrite-mode -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overwrite-mode" 'entry) t)
 "The @code{overwrite-mode} property of type @code{:boolean} (Read / Write)
  @br{}
  If text is overwritten when typing in the entry. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-overwrite-mode)
      "Accessor"
      (documentation 'entry-overwrite-mode 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-overwrite-mode object) => overwrite}
  @syntax{(setf (gtk:entry-overwrite-mode object) overwrite)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[overwrite]{a boolean whether the text is overwritten when typing}
  @begin{short}
    Accessor of the @slot[gtk:entry]{overwrite-mode} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-overwrite-mode} function returns whether the text is
  overwritten when typing in the text entry. The
  @setf{gtk:entry-overwrite-mode} function sets whether the text is overwritten
  when typing.
  @see-class{gtk:entry}")

;;; --- gtk:entry-placeholder-text ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "placeholder-text" 'entry) t)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the entry when it is empty and unfocused.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-placeholder-text)
      "Accessor"
      (documentation 'entry-placeholder-text 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-placeholder-text object) => text}
  @syntax{(setf (gtk:entry-placeholder-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string to be displayed when @arg{entry} is empty and
    unfocused, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:entry]{placeholder-text} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-placeholder-text} function retrieves the text that will be
  displayed when the entry is empty and unfocused. The
  @setf{gtk:entry-placeholder-text} function sets the text. This can be used to
  give a visual hint of the expected contents of the entry.

  Note that since the placeholder text gets removed when the entry received
  focus, using this feature is a bit problematic if the entry is given the
  initial focus in a window. Sometimes this can be worked around by delaying
  the initial focus setting until the first key event arrives.
  @see-class{gtk:entry}")

;;; --- gtk:entry-populate-all -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "populate-all" 'entry) t)
 "The @code{populate-all} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, the @code{\"populate-popup\"} signal is also emitted for touch
  popups. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-populate-all)
      "Accessor"
      (documentation 'entry-populate-all 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-populate-all object) => setting}
  @syntax{(setf (gtk:entry-populate-all object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether the @code{\"populate\"} signal is also
    emitted for touch popups}
  @begin{short}
    Accessor of the @slot[gtk:entry]{populate-all} slot of the @class{gtk:entry}
    class.
  @end{short}
  If the @code{setting} argument is @em{true}, the @code{\"populate-popup\"}
  signal is also emitted for touch popups.
  @see-class{gtk:entry}")

;;; --- gtk:entry-primary-icon-activatable -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-activatable"
                                                'entry) t)
 "The @code{primary-icon-activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-activatable)
      "Accessor"
      (documentation 'entry-primary-icon-activatable 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-activatable object) => activatable}
  @syntax{(setf (gtk:entry-primary-icon-activatable object) activatable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[activatable]{a boolean whether the primary icon is activatable}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-activatable} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the primary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-activatable}")

;;; --- gtk:entry-primary-icon-gicon -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-gicon" 'entry) t)
 "The @code{primary-icon-gicon} property of type @class{g:icon} (Read / Write)
  @br{}
  The icon to use for the primary icon for the entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-gicon)
      "Accessor"
      (documentation 'entry-primary-icon-gicon 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-gicon object) => icon}
  @syntax{(setf (gtk:entry-primary-icon-gicon object) icon)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-gicon} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon to use for the primary icon for the entry.
  @see-class{gtk:entry}
  @see-class{g:icon}")

;;; --- gtk:entry-primary-icon-name --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-name" 'entry) t)
 "The @code{primary-icon-name} property of type @code{:string} (Read / Write)
  @br{}
  The icon name to use for the primary icon for the entry. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-name)
      "Accessor"
      (documentation 'entry-primary-icon-name 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-name object) => name}
  @syntax{(setf (gtk:entry-primary-icon-name object) name)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[name]{a string with the icon name}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-name} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon name to use for the primary icon for the entry.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-name}
  @see-function{gtk:entry-secondary-icon-name}")

;;; --- gtk:entry-primary-icon-pixbuf ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-pixbuf" 'entry) t)
 "The @code{primary-icon-pixbuf} property of type @class{gdk-pixbuf:pixbuf}
  (Read / Write) @br{}
  A pixbuf to use as the primary icon for the entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-pixbuf)
      "Accessor"
      (documentation 'entry-primary-icon-pixbuf 'function)
 "@version{2023-3-12}
  @syntax{(gtk:entry-primary-icon-pixbuf object) => pixbuf}
  @syntax{(setf (gtk:entry-primary-icon-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-pixbuf} slot of the
    @class{gtk:entry} class.
  @end{short}
  A pixbuf to use as the primary icon for the entry.
  @see-class{gtk:entry}
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gtk:entry-primary-icon-sensitive ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-sensitive"
                                               'entry) t)
 "The @code{primary-icon-sensitive} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is sensitive. An insensitive icon appears grayed out.
  GTK does not emit the \"icon-press\" and @code{\"icon-release\"} signals and
  does not allow drag and drop from insensitive icons. An icon should be set
  insensitive if the action that would trigger when clicked is currently not
  available. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-sensitive)
      "Accessor"
      (documentation 'entry-primary-icon-sensitive 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-sensitive object) => sensitive}
  @syntax{(setf (gtk:entry-primary-icon-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[sensitive]{a boolean whether the primary icon is sensitive}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-sensitive} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the primary icon is sensitive. An insensitive icon appears grayed out.
  GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-sensitive}
  @see-function{gtk:entry-secondary-icon-sensitive}")

;;; --- gtk:entry-primay-icon-stock --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-stock" 'entry) t)
 "The @code{primary-icon-stock} property of type @code{:string} (Read / Write)
  @br{}
  The stock ID to use for the primary icon for the entry. @br{}
  @em{Warning:} The @code{primary-icon-stock} property has been deprecated
  since version 3.10 and should not be used in newly written code. Use the
  @code{primary-icon-name} property instead. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-stock)
      "Accessor"
      (documentation 'entry-primary-icon-stock 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-stock object) => stock}
  @syntax{(setf (gtk:entry-primary-icon-stock object) stock)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[stock]{a string with the stock ID to use for the primary icon}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-stock} slot of the
    @class{gtk:entry} class.
  @end{short}
  The stock ID to use for the primary icon for the entry.
  @begin[Warning]{dictionary}
    The @code{primary-icon-stock} property has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @code{primary-icon-name} property instead.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-function{gtk:entry-primary-icon-name}")

;;; --- gtk:entry-primary-icon-storage-type ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-storage-type"
                                               'entry) t)
 "The @code{primary-icon-storage-type} property of type @symbol{gtk:image-type}
  (Read) @br{}
  The representation which is used for the primary icon of the entry. @br{}
  Default value: @code{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-storage-type)
      "Accessor"
      (documentation 'entry-primary-icon-storage-type 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-storage-type object) => type}
  @syntax{(setf (gtk:entry-primary-icon-storage-type object) type)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[type]{a value of the @symbol{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-storage-type} slot of the
    @class{gtk:entry} class.
  @end{short}
  The representation which is used for the primary icon of the entry.
  @see-class{gtk:entry}
  @see-symbol{gtk:image-type}")

;;; --- gtk:entry-primary-icon-tooltip-markup ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-tooltip-markup"
                                               'entry) t)
 "The @code{primary-icon-tooltip-markup} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-tooltip-markup)
      "Accessor"
      (documentation 'entry-primary-icon-tooltip-markup 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-tooltip-markup object) => markup}
  @syntax{(setf (gtk:entry-primary-icon-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[markup]{a string with the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-tooltip-markup} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the primary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-markup}")

;;; --- gtk:entry-primary-icon-tooltip-text ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-tooltip-text"
                                               'entry) t)
 "The @code{primary-icon-tooltip-text} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-tooltip-text)
      "Accessor"
      (documentation 'entry-primary-icon-tooltip-text 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-primary-icon-tooltip-text object) => text}
  @syntax{(setf (gtk:entry-primary-icon-tooltip-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string with the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-tooltip-text} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the primary icon. Also see the
  @fun{gtk:entry-icon-tooltip-text} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-text}
  @see-function{gtk:entry-secondary-icon-tooltip-text}")

;;; --- gtk:entry-progress-fraction --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "progress-fraction" 'entry) t)
 "The @code{progress-fraction} property of type @code{:double} (Read / Write)
  @br{}
  The current fraction of the task that is been completed. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-progress-fraction)
      "Accessor"
      (documentation 'entry-progress-fraction 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-progress-fraction object) => fraction}
  @syntax{(setf (gtk:entry-progress-fraction object) fraction)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[fraction]{a double float with the fraction of the task that is
    been completed}
  @begin{short}
    Accessor of the @slot[gtk:entry]{progress-fraction} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-progress-fraction} function returns the current fraction
  that is been completed. The @setf{gtk:entry-progress-fraction} function
  causes the entry progress indicator to \"fill in\" the given fraction of the
  progress bar. The fraction should be between 0.0 and 1.0, inclusive.
  @see-class{gtk:entry}")

;;; --- gtk:entry-progress-pulse-step ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "progress-pulse-step" 'entry) t)
 "The @code{progress-pulse-step} property of type @code{:double} (Read / Write)
  @br{}
  The fraction of total entry width to move the progress bouncing block for
  each call to the @fun{gtk:entry-progress-pulse} function. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.1")

#+liber-documentation
(setf (liber:alias-for-function 'entry-progress-pulse-step)
      "Accessor"
      (documentation 'entry-progress-pulse-step 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-progress-pulse-step object) => step}
  @syntax{(setf (gtk:entry-progress-pulse-step object) step)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[step]{a double float with the fraction between 0.0 and 1.0}
  @begin{short}
    Accessor of the @slot[gtk:entry]{progress-pulse-step} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-progress-pulse-step} function retrieves the pulse step as
  a fraction from 0.0 to 1.0. The @setf{gtk:entry-progress-pulse-step} function
  sets the fraction of total entry width to move the progress bouncing block for
  each call to the @fun{gtk:entry-progress-pulse} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-progress-pulse}")

;;; --- gtk:entry-scroll-offset ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scroll-offset" 'entry) t)
 "The @code{scroll-offset} property of type @code{:int} (Read) @br{}
  Number of pixels of the entry scrolled off the screen to the left. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-scroll-offset)
      "Accessor"
      (documentation 'entry-scroll-offset 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-scroll-offset object) => offset}
  @syntax{(setf (gtk:entry-scroll-offset object) offset)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[offset]{an integer with the number of pixels of the entry scrolled
    off}
  @begin{short}
    Accessor of the @slot[gtk:entry]{scroll-offset} slot of the
    @class{gtk:entry} class.
  @end{short}
  Number of pixels of the entry scrolled off the screen to the left.
  @see-class{gtk:entry}")

;;; --- gtk:entry-secondary-icon-activatable -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-activatable"
                                               'entry) t)
 "The @code{secondary-icon-activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-activatable)
      "Accessor"
      (documentation 'entry-secondary-icon-activatable 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-activatable object) => activatable}
  @syntax{(setf (gtk:entry-secondary-icon-activatable object) activatable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[activatable]{a boolean whether the secondary icon is activatable}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-activatable} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the secondary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-activatable}")

;;; --- gtk:entry-secondary-icon-gicon -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-gicon" 'entry) t)
 "The @code{secondary-icon-gicon} property of type @class{g:icon} (Read / Write)
  @br{}
  The icon to use for the secondary icon for the entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-gicon)
      "Accessor"
      (documentation 'entry-secondary-icon-gicon 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-gicon object) => icon}
  @syntax{(setf (gtk:entry-secondary-icon-gicon object) icon)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-gicon} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon to use for the secondary icon for the entry.
  @see-class{gtk:entry}
  @see-class{g:icon}
  @see-function{gtk:entry-icon-gicon}")

;;; --- gtk:entry-secondary-icon-name ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-name" 'entry) t)
 "The @code{secondary-icon-name} property of type @code{:string} (Read / Write)
  @br{}
  The icon name to use for the secondary icon for the entry. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-name)
      "Accessor"
      (documentation 'entry-secondary-icon-name 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-name object) => name}
  @syntax{(setf (gtk:entry-secondary-icon-name object) name)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[name]{a string with the icon name}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-name} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon name to use for the secondary icon for the entry.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-name}
  @see-function{gtk:entry-primary-icon-name}")

;;; --- gtk:entry-secondary-icon-pixbuf ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-pixbuf"
                                               'entry) t)
 "The @code{secondary-icon-pixbuf} property of type @class{gdk-pixbuf:pixbuf}
  (Read / Write) @br{}
  A pixbuf to use as the secondary icon for the entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-pixbuf)
      "Accessor"
      (documentation 'entry-secondary-icon-pixbuf 'function)
 "@version{2023-3-12}
  @syntax{(gtk:entry-secondary-icon-pixbuf object) => pixbuf}
  @syntax{(setf (gtk:entry-secondary-icon-pixbuf object) pixbuf)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-pixbuf} slot of the
    @class{gtk:entry} class.
  @end{short}
  A pixbuf to use as the secondary icon for the entry.
  @see-class{gtk:entry}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:entry-icon-pixbuf}")

;;; --- gtk:entry-secondary-icon-sensitive -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-sensitive"
                                               'entry) t)
 "The @code{secondary-icon-sensitive} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is sensitive. An insensitive icon appears grayed
  out. GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-sensitive)
      "Accessor"
      (documentation 'entry-secondary-icon-sensitive 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-sensitive object) => sensitive}
  @syntax{(setf (gtk:entry-secondary-icon-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[sensitive]{a boolean whether the icon is sensitive}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-sensitive} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the secondary icon is sensitive. An insensitive icon appears grayed
  out. GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-sensitive}")

;;; --- gtk:entry-secondary-icon-stock -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-stock" 'entry) t)
 "The @code{secondary-icon-stock} property of type @code{:string} (Read / Write)
  @br{}
  The stock ID to use for the secondary icon for the entry. @br{}
  @em{Warning:} The @code{secondary-icon-stock} property has been deprecated
  since version 3.10 and should not be used in newly written code. Use the
  @code{secondary-icon-name} property instead. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-stock)
      "Accessor"
      (documentation 'entry-secondary-icon-stock 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-stock object) => stock}
  @syntax{(setf (gtk:entry-secondary-icon-stock object) stock)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[stock]{a string with the stock ID to use for the icon}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-stock} slot of the
    @class{gtk:entry} class.
  @end{short}
  The stock ID to use for the secondary icon for the entry.
  @begin[Warning]{dictionary}
    The @code{secondary-icon-stock} property has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @slot[gtk:entry]{secondary-icon-name} property instead.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-function{gtk:entry-secondary-icon-name}")

;;; --- gtk:entry-secondary-icon-storage-type ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-storage-type"
                                               'entry) t)
 "The @code{secondary-icon-storage-type} property of type
  @symbol{gtk:image-type} (Read) @br{}
  The representation which is used for the secondary icon of the entry. @br{}
  Default value: @code{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-storage-type)
      "Accessor"
      (documentation 'entry-secondary-icon-storage-type 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-storage-type object) => type}
  @syntax{(setf (gtk:entry-secondary-icon-storage-type object) type)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[type]{a value of the @symbol{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-storage-type} slot of the
    @class{gtk:entry} class.
  @end{short}
  The representation which is used for the secondary icon of the entry.
  @see-class{gtk:entry}
  @see-symbol{gtk:image-type}
  @see-function{gtk:entry-icon-storage-type}")

;;; --- gtk:entry-secondary-icon-tooltip-markup --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-tooltip-markup"
                                               'entry) t)
 "The @code{secondary-icon-tooltip-markup} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-tooltip-markup)
      "Accessor"
      (documentation 'entry-secondary-icon-tooltip-markup 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-tooltip-markup object) => markup}
  @syntax{(setf (gtk:entry-secondary-icon-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[markup]{a string with the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-tooltip-markup} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the secondary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-markup}")

;;; --- gtk:entry-secondary-icon-tooltip-text ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-tooltip-text"
                                               'entry) t)
 "The @code{secondary-icon-tooltip-text} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-tooltip-text)
      "Accessor"
      (documentation 'entry-secondary-icon-tooltip-text 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-secondary-icon-tooltip-text object) => text}
  @syntax{(setf (gtk:entry-secondary-icon-tooltip-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string with the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-tooltip-text} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the secondary icon. Also see the
  @fun{gtk:entry-icon-tooltip-text} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-text}
  @see-function{gtk:entry-primary-icon-tooltip-text}")

;;; --- gtk:entry-selection-bound ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selection-bound" 'entry) t)
 "The @code{selection-bound} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in chars.
  @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-selection-bound)
      "Accessor"
      (documentation 'entry-selection-bound 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-selection-bound object) => bound}
  @syntax{(setf (gtk:entry-selection-bound object) bound)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[bound]{an integer with the position of the opposite end of the
    selection from the cursor in chars}
  @begin{short}
    Accessor of the @slot[gtk:entry]{selection-bound} slot of the
    @class{gtk:entry} class.
  @end{short}
  The position of the opposite end of the selection from the cursor in chars.
  @see-class{gtk:entry}")

;;; --- gtk:entry-shadow-type --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shadow-type" 'entry) t)
 "The @code{shadow-type} property of type @symbol{gtk:shadow-type}
  (Read / Write) @br{}
  Which kind of shadow to draw around the entry when \"has-frame\" is set to
  @em{true}. @br{}
  @em{Warning:} The @code{shadow-type} property has been deprecated since
  version 3.20 and should not be used in newly written code. Use CSS to
  determine the style of the border. The value of this style property is
  ignored. @br{}
  Default value: @code{:in}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-shadow-type)
      "Accessor"
      (documentation 'entry-shadow-type 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-shadow-type object) => type}
  @syntax{(setf (gtk:entry-shadow-type object) type)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[type]{a value of the @symbol{gtk:shadow-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{shadow-type} slot of the @class{gtk:entry}
    class.
  @end{short}
  Which kind of shadow to draw around the entry when \"has-frame\" is set to
  @em{true}.
  @begin[Warning]{dictionary}
    The @code{shadow-type} property has been deprecated since version 3.20 and
    should not be used in newly written code. Use CSS to determine the style of
    the border. The value of this style property is ignored.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-symbol{gtk:shadow-type}")

;;; --- gtk:entry-show-emoji-icon ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-emoji-icon" 'entry) t)
 "The @code{show-emoji-icon} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show an icon for Emoji. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-show-emoji-icon)
      "Accessor"
      (documentation 'entry-show-emoji-icon 'function)
 "@version{2023-3-13}
  @syntax{(gtk:entry-show-emoji-icon object) => setting}
  @syntax{(setf (gtk:entry-show-emoji-icon object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether to show an icon for Emoji}
  @begin{short}
    Accessor of the @slot[gtk:entry]{show-emoji-icon} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether to show an icon for Emoji.
  @see-class{gtk:entry}")

;;; --- gtk:entry-tabs ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tabs" 'entry) t)
 "The @code{tabs} property of type @class{pango:tab-array} (Read / Write) @br{}
  A list of tabstop locations to apply to the text of the entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-tabs)
      "Accessor"
      (documentation 'entry-tabs 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-tabs object) => tabs}
  @syntax{(setf (gtk:entry-tabs object) tabs)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @begin{short}
    Accessor of the @slot[gtk:entry]{tabs} slot of the @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-tabs} function gets the tabstops that were set on the
  entry using the @setf{gtk:entry-tabs} function, if any.

  The tabstops in the array are applied to the entry text.
  @see-class{gtk:entry}
  @see-class{pango:tab-array}")

;;; --- gtk:entry-text ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'entry) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The contents of the entry. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'entry-text)
      "Accessor"
      (documentation 'entry-text 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-text object) => text}
  @syntax{(setf (gtk:entry-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string with the contents of the entry}
  @begin{short}
    Accessor of the @slot[gtk:entry]{text} slot of the @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-text} function retrieves the contents of the entry widget
  as a string. The @setf{gtk:entry-text} function sets the text, replacing the
  current contents.

  See also the @fun{gtk:editable-chars} and @fun{gtk:entry-buffer-text}
  functions. This is equivalent to:
  @begin{pre}
(gtk:entry-buffer-text (gtk:entry-buffer object))
  @end{pre}
  @see-class{gtk:entry}
  @see-function{gtk:entry-buffer-text}
  @see-function{gtk:editable-chars}")

;;; --- gtk:entry-text-length --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-length" 'entry) t)
 "The @code{text-length} property of type @code{:uint} (Read) @br{}
  The length of the text in the entry. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-text-length)
      "Accessor"
      (documentation 'entry-text-length 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-text-length object) => length}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[length]{an unsigned integer with the length of the text}
  @begin{short}
    Accessor of the @slot[gtk:entry]{text-length} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-text-length} function retrieves the current length of the
  text in the entry, or 0 if there are none. This is equivalent to:
  @begin{pre}
(gtk:entry-buffer-length (gtk:entry-buffer object))
  @end{pre}
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-length}")

;;; --- gtk:entry-truncate-multiline -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "truncate-multiline" 'entry) t)
 "The @code{truncate-multiline} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, pasted multi-line text is truncated to the first line. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-truncate-multiline)
      "Accessor"
      (documentation 'entry-truncate-multiline 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-truncate-multiline object) => truncate}
  @syntax{(setf (gtk:entry-truncate-multiline object) truncate)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[truncate]{a boolean whether multi-line text is truncated}
  @begin{short}
    Accessor of the @slot[gtk:entry]{truncate-multiline} slot of the
    @class{gtk:entry} class.
  @end{short}
  If @em{true}, pasted multi-line text is truncated to the first line.
  @see-class{gtk:entry}")

;;; --- gtk:entry-visibility ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visibility" 'entry) t)
 "The @code{visibility} property of type @code{:boolean} (Read / Write) @br{}
  @em{False} displays the \"invisible char\" instead of the actual text
  (password mode). @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-visibility)
      "Accessor"
      (documentation 'entry-visibility 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-visibility object) => visible}
  @syntax{(setf (gtk:entry-visibility object) visible)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[visible]{@em{true} if the contents of the entry are displayed as
    plaintext}
  @begin{short}
    Accessor of the @slot[gtk:entry]{visibility} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-visible} function retrieves whether the text in the entry
  is visible. The @setf{gtk:entry-visible} function sets whether the contents
  of the entry are visible or not.

  When visibility is set to @em{false}, characters are displayed as the
  invisible char, and will also appear that way when the text in the entry
  is copied elsewhere.

  By default, GTK picks the best invisible character available in the current
  font, but it can be changed with the @fun{gtk:entry-invisible-char} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-invisible-char}")

;;; --- gtk:entry-width-chars --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars" 'entry) t)
 "The @code{width-chars} property of tpye @code{:int} (Read / Write) @br{}
  Number of characters to leave space for in the entry. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'entry-width-chars)
      "Accessor"
      (documentation 'entry-width-chars 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-width-chars object) => n-chars}
  @syntax{(setf (gtk:entry-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[n-chars]{an integer with the width in chars}
  @begin{short}
    Accessor of the @slot[gtk:entry]{width-chars} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-width-chars} function returns the number of chars to
  request space for, or negative if unset. The
  @setf{gtk:entry-width-chars} function changes the size request of the entry
  to be about the right size for @arg{n-chars} characters.

  Note that it changes the size request, the size can still be affected by how
  you pack the widget into containers. If the @arg{n-chars} argument is -1, the
  size reverts to the default entry size.
  @see-class{gtk:entry}")

;;; --- gtk:entry-xalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'entry) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for RTL
  layouts. @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-xalign)
      "Accessor"
      (documentation 'entry-xalign 'function)
 "@version{2023-3-4}
  @syntax{(gtk:entry-xalign object) => xalign}
  @syntax{(setf (gtk:entry-xalign object) xalign)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[xalign]{a float with the horizontal alignment}
  @begin{short}
    Accessor of the @slot[gtk:entry]{xalign} slot of the @class{gtk:entry}
    class.
  @end{short}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for RTL
  layouts.
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline entry-new))

(defun entry-new ()
 #+liber-documentation
 "@version{#2023-3-4}
  @return{The new @class{gtk:entry} widget.}
  @begin{short}
    Creates a new entry.
  @end{short}
  @see-class{gtk:entry}"
  (make-instance 'entry))

(export 'entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new_with_buffer ()
;;; ----------------------------------------------------------------------------

(declaim (inline entry-new-with-buffer))

(defun entry-new-with-buffer (buffer)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[buffer]{a @class{gtk:entry-buffer} object to use for the entry}
  @return{The new @class{gtk:entry} widget.}
  @begin{short}
    Creates a new entry with the specified text buffer.
  @end{short}
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}"
  (make-instance 'entry
                 :buffer buffer))

(export 'entry-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_text_area () -> entry-text-area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_text_area" %entry-get-text-area) :void
  (entry (g:object entry))
  (area (g:boxed gdk:rectangle)))

(defun entry-text-area (entry)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @return{The @class{gdk:rectangle} instance with the text area.}
  @begin{short}
    Gets the area where the text of the entry is drawn.
  @end{short}
  This function is useful when drawing something to the entry in a draw
  callback function.

  If the entry is not realized, the returned @class{gdk:rectangle} instance is
  filled with zeros. See also the @fun{gtk:entry-icon-area} function.
  @see-class{gtk:entry}
  @see-class{gdk:rectangle}
  @see-function{gtk:entry-icon-area}"
  (let ((area (gdk:rectangle-new)))
    (%entry-get-text-area entry area)
    area))

(export 'entry-text-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_unset_invisible_char ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_unset_invisible_char" entry-unset-invisible-char)
    :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Unsets the invisible char previously set with the
    @fun{gtk:entry-invisible-char} function.
  @end{short}
  So that the default invisible char is used again.
  @see-class{gtk:entry}
  @see-function{gtk:entry-invisible-char}"
  (entry (g:object entry)))

(export 'entry-unset-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_alignment ()
;;; gtk_entry_get_alignment () -> entry-alignment
;;; ----------------------------------------------------------------------------

(defun (setf entry-alignment) (align entry)
  (setf (entry-xalign entry) align))

(defun entry-alignment (entry)
 #+liber-documentation
 "@version{#2023-3-4}
  @syntax{(gtk:entry-alignment entry) => align}
  @syntax{(setf (gtk:entry-alignment entry) align)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[align]{a float with the horizontal alignment, from 0.0 (left) to
    1.0 (right), reversed for RTL layouts}
  @begin{short}
    Accessor of the horizontal positioning of the entry.
  @end{short}
  This controls the horizontal positioning of the contents when the displayed
  text is shorter than the width of the entry.
  @begin[Lisp implementation]{dictionary}
    This function is implemented with the @fun{gtk:entry-xalign} function.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-function{gtk:entry-xalign}"
  (entry-xalign entry))

(export 'entry-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout () -> entry-layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_layout" entry-layout) (g:object pango-layout)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @return{The @class{pango:layout} object for the entry.}
  @begin{short}
    Gets the Pango layout used to display the entry.
  @end{short}
  The Pano layout is useful to e.g. convert text positions to pixel positions,
  in combination with the @fun{gtk:entry-layout-offsets} function. The returned
  Pango layout is owned by the entry and must not be modified or freed by the
  caller.

  Keep in mind that the Pango layout text may contain a preedit string, so the
  @fun{gtk:entry-layout-index-to-text-index} and
  @fun{gtk:entry-text-index-to-layout-index} functions are needed to convert
  byte indices in the Pango layout to byte indices in the entry contents.
  @see-class{gtk:entry}
  @see-class{pango:layout}
  @see-function{gtk:entry-layout-offsets}
  @see-function{gtk:entry-layout-index-to-text-index}
  @see-function{gtk:entry-text-index-to-layout-index}"
  (entry (g:object entry)))

(export 'entry-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_layout_offsets () -> entry-layout-offsets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_layout_offsets" %entry-get-layout-offsets) :void
  (entry (g:object entry))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun entry-layout-offsets (entry)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{return}
    @code{x} -- an integer with the x offset of the Pango layout, or @code{nil}
    @br{}
    @code{y} -- an integer with the y offset of the Pango layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the position of the Pango layout used to render text in the entry,
    in widget coordinates.
  @end{short}
  Useful if you want to line up the text in an entry with some other text, e.g.
  when using the entry to implement editable cells in a sheet widget. Also
  useful to convert mouse events into coordinates inside the Pango layout, e.g.
  to take some action if some part of the entry text is clicked.

  Note that as the user scrolls around in the entry the offsets will change.
  You will need to connect to the @code{\"notify::scroll-offset\"} signal to
  track this. Remember when using the @class{pango:layout} functions you need
  to convert to and from pixels using the @fun{pango:pixels} function or
  the @var{pango:+scale+} constant.

  Keep in mind that the layout text may contain a preedit string, so the
  @fun{gtk:entry-layout-index-to-text-index} and
  @fun{gtk:entry-text-index-to-layout-index} functions are needed to convert
  byte indices in the layout to byte indices in the entry contents.
  @see-class{gtk:entry}
  @see-class{pango:layout}
  @see-function{gtk:entry-layout-index-to-text-index}
  @see-function{gtk:entry-text-index-to-layout-index}
  @see-function{pango:pixels}
  @see-variable{pango:+scale+}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%entry-get-layout-offsets entry x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'entry-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_layout_index_to_text_index ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_layout_index_to_text_index"
               entry-layout-index-to-text-index) :int
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[index]{an integer with the byte index into the entry layout
    text}
  @return{Byte index into the entry contents.}
  @begin{short}
    Converts from a position in the entry contents, returned by the
    @fun{gtk:entry-text} function, to a position in the Pango layout of the
    entry, returned by the @fun{gtk:entry-layout} function, with text retrieved
    via the @fun{pango:layout-text} function.
  @end{short}
  @see-class{gtk:entry}
  @see-function{gtk:entry-text}
  @see-function{gtk:entry-layout}
  @see-function{pango:layout-text}"
  (entry (g:object entry))
  (index :int))

(export 'entry-layout-index-to-text-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_text_index_to_layout_index ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_text_index_to_layout_index"
               entry-text-index-to-layout-index) :int
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[index]{an integer with the byte index into the entry contents}
  @return{The integer with the byte index into the entry layout text.}
  @begin{short}
    Converts from a position in the Pango layout of the entry, returned by the
    @fun{gtk:entry-layout} function, to a position in the entry contents,
    returned by the @fun{gtk:entry-text} function.
  @end{short}
  @see-class{gtk:entry}
  @see-function{gtk:entry-layout}
  @see-function{gtk:entry-text}"
  (entry (g:object entry))
  (index :int))

(export 'entry-text-index-to-layout-index)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_cursor_hadjustment ()
;;; gtk_entry_get_cursor_hadjustment () -> entry-cursor-hadjustment
;;; ----------------------------------------------------------------------------

(defun (setf entry-cursor-hadjustment) (adjustment entry)
  (cffi:foreign-funcall "gtk_entry_set_cursor_hadjustment"
                        (g:object entry) entry
                        (g:object adjustment) adjustment
                        :void)
  adjustment)

(cffi:defcfun ("gtk_entry_get_cursor_hadjustment" entry-cursor-hadjustment)
    (g:object adjustment)
 #+liber-documentation
 "@version{#2023-3-4}
  @syntax{(gtk:entry-cursor-hadjustment entry) => adjustment}
  @syntax{(setf (gtk:entry-cursor-hadjustment entry) adjustment)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object which should be
    adjusted when the cursor is moved, or @code{nil}}
  @begin{short}
    Accessor of the horizontal cursor adjustment for the entry.
  @end{short}
  Hooks up an adjustment to the cursor position in an entry, so that when the
  cursor is moved, the adjustment is scrolled to show that position.

  See the @fun{gtk:scrolled-window-hadjustment} function for a typical way
  of obtaining the adjustment. The adjustment has to be in pixel units and in
  the same coordinate system as the entry.
  @see-class{gtk:entry}
  @see-class{gtk:adjustment}
  @see-function{gtk:scrolled-window-hadjustment}"
  (entry (g:object entry)))

(export 'entry-cursor-hadjustment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_progress_pulse ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_progress_pulse" entry-progress-pulse) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Indicates that some progress is made, but you do not know how much.
  @end{short}
  Causes the progress indicator of the entry to enter \"activity mode\", where
  a block bounces back and forth. Each call to the
  @fun{gtk:entry-progress-pulse} function causes the block to move by a little
  bit. The amount of movement per pulse is determined by the
  @fun{gtk:entry-progress-pulse-step} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-progress-pulse-step}"
  (entry (g:object entry)))

(export 'entry-progress-pulse)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_im_context_filter_keypress ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_im_context_filter_keypress"
               entry-im-context-filter-keypress) :boolean
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[event]{a @class{gdk:event} key event}
  @return{@em{True} if the input method handled the key event.}
  @begin{short}
    Allow the entry input method to internally handle key press and release
    events.
  @end{short}
  If this function returns @em{true}, then no further processing should be
  done for this key event. See the @fun{gtk:im-context-filter-keypress}
  function.

  Note that you are expected to call this function from your handler when
  overriding key event handling. This is needed in the case when you need to
  insert your own key handling between the input method and the default key
  event handling of the entry. See the @fun{gtk:text-view-reset-im-context}
  function for an example of use.
  @see-class{gtk:entry}
  @see-class{gdk:event-key}
  @see-function{gtk:im-context-filter-keypress}
  @see-function{gtk:text-view-reset-im-context}"
  (entry (g:object entry))
  (event (g:boxed gdk:event)))

(export 'entry-im-context-filter-keypress)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_reset_im_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_reset_im_context" entry-reset-im-context) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Reset the input method context of the entry if needed.
  @end{short}
  This can be necessary in the case where modifying the buffer would confuse
  on-going input method behavior.
  @see-class{gtk:entry}"
  (entry (g:object entry)))

(export 'entry-reset-im-context)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_pixbuf ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_from_pixbuf" entry-set-icon-from-pixbuf)
    :void
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object, or @code{nil}}
  @begin{short}
    Sets the icon shown in the specified position using a pixbuf.
  @end{short}
  If the @arg{pixbuf} argument is @code{nil}, no icon will be shown in the
  specified position.
  @see-class{gtk:entry}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'entry-set-icon-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_stock ()
;;; ----------------------------------------------------------------------------

(defun entry-set-icon-from-stock (entry pos stock)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[stock]{a string with the name of the stock item, or @code{nil}}
  @begin{short}
    Sets the icon shown in the entry at the specified position from a stock
    image.
  @end{short}
  If the @arg{stock} argument is @code{nil}, no icon will be shown in the
  specified position.
  @begin[Warning]{dictionary}
    The @fun{gtk:entry-set-icon-from-stock} function has been deprecated since
    version 3.10 and should not be used in newly written code. Use the
    @fun{gtk:entry-set-icon-from-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:entry-icon-stock}
  @see-function{gtk:entry-set-icon-from-icon-name}"
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-stock entry) stock))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-stock entry) stock))
        (t
         (error "Unexpected icon position in GTK:ENTRY-SET-ICON-FROM-STOCK"))))

(export 'entry-set-icon-from-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_icon_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_from_icon_name"
               entry-set-icon-from-icon-name) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[name]{a string with the icon name, or @code{nil}}
  @begin{short}
    Sets the icon shown in the entry at the specified position from the current
    icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the @arg{icon-name} argument is @code{nil}, no icon will be shown
  in the specified position.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (name :string))

(export 'entry-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_from_gicon" entry-set-icon-from-gicon) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[icon]{a @class{g:icon} object with the icon to set, or @code{nil}}
  @begin{short}
    Sets the icon shown in the entry at the specified position from the current
    icon theme.
  @end{short}
  If the icon is not known, a \"broken image\" icon will be displayed instead.
  If the @arg{icon} argument is @code{nil}, no icon will be shown in the
  specified position.
  @see-class{gtk:entry}
  @see-class{g:icon}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (icon (g:object g:icon)))

(export 'entry-set-icon-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_storage_type () -> entry-icon-storage-type
;;; ----------------------------------------------------------------------------

(defun entry-icon-storage-type (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @return{The @symbol{gtk:image-type} value with the image representation being
    used.}
  @begin{short}
    Gets the type of representation being used by the icon to store image data.
  @end{short}
  If the icon has no image data, the return value will be @code{:empty}.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-symbol{gtk:image-type}"
  (cond ((eq pos :primary)
         (entry-primary-icon-storage-type entry))
        ((eq pos :secondary)
         (entry-secondary-icon-storage-type entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-STORAGE-TYPE"))))

(export 'entry-icon-storage-type)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_pixbuf () -> entry-icon-pixbuf
;;; ----------------------------------------------------------------------------

(defun entry-icon-pixbuf (entry pos)
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @begin{return}
    A @class{gdk-pixbuf:pixbuf} object, or @code{nil} if no icon is set for
    this position.
  @end{return}
  @begin{short}
    Retrieves the image used for the icon.
  @end{short}
  Unlike the other methods of setting and getting icon data, this method will
  work regardless of whether the icon was set using a @class{gdk-pixbuf:pixbuf}
  object, a @class{g:icon} object, a stock item, or an icon name.
  @see-class{gtk:entry}
  @see-class{g:icon}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-pixbuf entry))
        ((eq pos :secondary)
         (entry-secondary-icon-pixbuf entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-PIXBUF"))))

(export 'entry-icon-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_stock () -> entry-icon-stock
;;; ----------------------------------------------------------------------------

(defun entry-icon-stock (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @begin{return}
    A stock ID, or @code{nil} if no icon is set or if the icon was not set from
    a stock ID.
  @end{return}
  @begin{short}
    Retrieves the stock ID used for the icon, or @code{nil} if there is no icon
    or if the icon was set by some other method, e.g., by Pixbuf, icon name or
    GIcon.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:entry-icon-stock} function has been deprecated since version
    3.10 and should not be used in newly written code. Use the
    @fun{gtk:entry-icon-name} function instead.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:entry-icon-name}"
  (cond ((eq pos :primary)
         (entry-primary-icon-stock entry))
        ((eq pos :secondary)
         (entry-secondary-icon-stock entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-STOCK"))))

(export 'entry-icon-stock)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_name () -> entry-icon-name
;;; ----------------------------------------------------------------------------

(defun entry-icon-name (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @begin{return}
    An icon name, or @code{nil} if no icon is set or if the icon was not set
    from an icon name.
  @end{return}
  @begin{short}
    Retrieves the icon name used for the icon, or @code{nil} if there is no icon
    or if the icon was set by some other method, e.g., by Pixbuf, stock or
    GIcon.
  @end{short}
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-name entry))
        ((eq pos :secondary)
         (entry-secondary-icon-name entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-NAME"))))

(export 'entry-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_gicon () -> entry-icon-gicon
;;; ----------------------------------------------------------------------------

(defun entry-icon-gicon (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @return{The @class{g:icon} object, or @code{nil} if no icon is set or if the
    icon is not a @class{g:icon} object.}
  @begin{short}
    Retrieves the @class{g:icon} object used for the icon, or @code{nil} if
    there is no icon or if the icon was set by some other method, e.g., by
    stock, Pixbuf, or icon name.
  @end{short}
  @see-class{gtk:entry}
  @see-class{g:icon}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-gicon entry))
        ((eq pos :secondary)
         (entry-secondary-icon-gicon entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-GICON"))))

(export 'entry-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_activatable ()
;;; gtk_entry_get_icon_activatable () -> entry-icon-activatable
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-activatable) (activatable entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-activatable entry) activatable))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-activatable entry) activatable))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-ACTIVATABLE"))))

(defun entry-icon-activatable (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @syntax{(gtk:entry-icon-activatable entry pos) => activatable}
  @syntax{(setf (gtk:entry-icon-activatable entry pos) activatable)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[activatable]{@em{true} if the icon should be activatable}
  @begin{short}
    Accessor of the activatable property of the icon in the entry.
  @end{short}
  The @fun{gtk:entry-icon-activatable} function returns whether the icon is
  activatable. The @setf{gtk:entry-icon-activatable} function sets whether the
  icon is activatable.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-activatable entry))
        ((eq pos :secondary)
         (entry-secondary-icon-activatable entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-ACTIVATABLE"))))

(export 'entry-icon-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_sensitive ()
;;; gtk_entry_get_icon_sensitive () -> entry-icon-sensitive
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-sensitive) (sensitive entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-sensitive entry) sensitive))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-sensitive entry) sensitive))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-SENSITIVE"))))

(defun entry-icon-sensitive (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @syntax{(gtk:entry-icon-sensitive entry pos) => sensitive}
  @syntax{(setf (gtk:entry-icon-sensitive entry pos) sensitive)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[sensitive]{specifies whether the icon should appear sensitive or
    insensitive}
  @begin{short}
    Accessor of the sensitive property of the icon in the entry.
  @end{short}
  The @fun{gtk:entry-icon-sensitive} function returns whether the icon appears
  sensitive or insensitive. The @setf{gtk:entry-icon-sensitive} function sets
  the sensitivity for the specified icon.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-sensitive entry))
        ((eq pos :secondary)
         (entry-secondary-icon-sensitive entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-SENSITIVE"))))

(export 'entry-icon-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_at_pos () -> entry-icon-at-pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_icon_at_pos" entry-icon-at-pos) :int
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[x]{an integer with the x coordinate of the position to find}
  @argument[y]{an integer with the y coordinate of the position to find}
  @return{The integer with the index of the icon at the given position, or -1.}
  @begin{short}
    Finds the icon at the given position and return its index.
  @end{short}
  The coordinates of the posistion are relative to the top left corner of the
  entry. If x, y does not lie inside an icon, -1 is returned. This function is
  intended for use in a @code{\"query-tooltip\"} signal handler.
  @see-class{gtk:entry}"
  (entry (g:object entry))
  (x :int)
  (y :int))

(export 'entry-icon-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_text ()
;;; gtk_entry_get_icon_tooltip_text () -> entry-icon-tooltip-text
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-tooltip-text) (tooltip entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-tooltip-text entry) tooltip))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-tooltip-text entry) tooltip))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-TOOLTIP-TEXT"))))

(defun entry-icon-tooltip-text (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @syntax{(gtk:entry-icon-tooltip-text entry pos) => tooltip}
  @syntax{(setf (gtk:entry-icon-tooltip-text entry pos) tooltip)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[tooltip]{a string with the contents of the tooltip for the icon, or
    @code{nil}}
  @begin{short}
    Accessor of the tooltip text on the icon in the entry.
  @end{short}
  The @fun{gtk:entry-icon-tooltip-text} function gets the contents of the
  tooltip on the icon at the specified position in the entry. The
  @setf{gtk:entry-icon-tooltip-text} function sets a tooltip. Use @code{nil}
  for @arg{tooltip} to remove an existing tooltip.

  See also the @fun{gtk:widget-tooltip-text} and
  @fun{gtk:entry-icon-tooltip-markup} functions.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:widget-tooltip-text}
  @see-function{gtk:entry-icon-tooltip-markup}"
  (cond ((eq pos :primary)
         (entry-primary-icon-tooltip-text entry))
        ((eq pos :secondary)
         (entry-secondary-icon-tooltip-text entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-TOOLTIP-TEXT"))))

(export 'entry-icon-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_markup ()
;;; gtk_entry_get_icon_tooltip_markup () -> entry-icon-tooltip-markup
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-tooltip-markup) (tooltip entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-tooltip-markup entry) tooltip))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-tooltip-markup entry) tooltip))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-TOOLTIP-MARKUP"))))

(defun entry-icon-tooltip-markup (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @syntax{(gtk:entry-icon-tooltip-markup entry pos) => tooltip}
  @syntax{(setf (gtk:entry-icon-tooltip-markup entry pos) tooltip)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[tooltip]{a string with the contents of the tooltip for the icon, or
    @code{nil}}
  @begin{short}
    Accessor of the tooltip markup on the icon in the entry.
  @end{short}
  The @fun{gtk:entry-icon-tooltip-markup} function gets the contents of the
  tooltip on the icon at the specified position in the entry. The
  @setf{gtk:entry-icon-tooltip-markup} function sets the tooltip for the icon
  at the specified position. The @arg{tooltip} argument is assumed to be marked
  up with the Pango text markup language. Use @code{nil} for @arg{tooltip} to
  remove an existing tooltip.

  See also the @fun{gtk:widget-tooltip-markup} and
  @fun{gtk:entry-icon-tooltip-text} functions.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:widget-tooltip-markup}
  @see-function{gtk:entry-icon-tooltip-text}"
  (cond ((eq pos :primary)
         (entry-primary-icon-tooltip-markup entry))
        ((eq pos :secondary)
         (entry-secondary-icon-tooltip-markup entry))
        (t
         (error "Unexpected icon position in GTK:ENTRY-ICON-TOOLTIP-MARKUP"))))

(export 'entry-icon-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_drag_source ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_drag_source" entry-set-icon-drag-source)
    :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @argument[tlist]{a @class{gtk:target-list} instance with the targets (data
    formats) in which the data can be provided}
  @argument[actions]{a @symbol{gdk:drag-action} bitmask with the allowed drag
    actions}
  @begin{short}
    Sets up the icon at the given position so that GTK will start a drag
    operation when the user clicks and drags the icon.
  @end{short}
  To handle the drag operation, you need to connect to the usual
  @code{\"drag-data-get\"} or possibly @code{\"drag-data-delete\"} signal, and
  use the @fun{gtk:entry-current-icon-drag-source} function in your signal
  handler to find out if the drag was started from an icon.

  By default, GTK uses the icon as the drag icon. You can use the
  @code{\"drag-begin\"} signal to set a different icon. Note that you have to
  use the @fun{g:signal-connect-after} function to ensure that your signal
  handler gets executed after the default handler.
  @see-class{gtk:entry}
  @see-class{gtk:target-list}
  @see-symbol{gdk:drag-action}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:entry-current-icon-drag-source}
  @see-function{g:signal-connect-after}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (tlist (g:boxed target-list))
  (actions gdk:drag-action))

(export 'entry-set-icon-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_current_icon_drag_source ()
;;; -> entry-current-icon-drag-source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_current_icon_drag_source"
               entry-current-icon-drag-source) :int
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{return}
    An integer with the index of the icon which is the source of the current
    DND operation, or -1.
  @end{return}
  @begin{short}
    Returns the index of the icon which is the source of the current DND
    operation, or -1.
  @end{short}
  This function is meant to be used in a \"drag-data-get\" callback function.
  @see-class{gtk:entry}"
  (entry (g:object entry)))

(export 'entry-current-icon-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_area () -> entry-icon-area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_icon_area" %entry-get-icon-area) :void
  (entry (g:object entry))
  (pos entry-icon-position)
  (area (g:boxed gdk:rectangle)))

(defun entry-icon-area (entry pos)
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value}
  @return{@code{area} -- a @class{gdk:rectangle} instance with the area of the
    icon}
  @begin{short}
    Gets the area where the icon of the entry at @arg{pos} is drawn.
  @end{short}
  This function is useful when drawing something to the entry in a draw
  callback function. If the entry is not realized or has no icon at the given
  position, @arg{area} is filled with zeros. See also the
  @fun{gtk:entry-text-area} function.
  @see-class{gtk:entry}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:entry-text-area}"
  (let ((area (gdk:rectangle-new)))
    (%entry-get-icon-area entry pos area)
    area))

(export 'entry-icon-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_grab_focus_without_selecting ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_grab_focus_without_selecting"
               entry-grab-focus-without-selecting) :void
 #+liber-documentation
 "@version{#2023-3-4}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Causes entry to have keyboard focus.
  @end{short}
  It behaves like the @fun{gtk:widget-grab-focus} function, except that it
  does not select the contents of the entry. You only want to call this on some
  special entries which the user usually does not want to replace all text in,
  such as search-as-you-type entries.
  @see-class{gtk:entry}"
  (entry (g:object entry)))

(export 'entry-grab-focus-without-selecting)

;;; --- End of file gtk3.entry.lisp --------------------------------------------
